$PBExportHeader$w_individual_search.srw
forward
global type w_individual_search from w_ancestor
end type
type uo_claim_search from u_claim_search within w_individual_search
end type
type cb_ok from commandbutton within w_individual_search
end type
type cb_cancel from commandbutton within w_individual_search
end type
end forward

global type w_individual_search from w_ancestor
integer x = 910
integer y = 468
integer width = 3223
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
global w_individual_search w_individual_search

type variables
w_ancestor	iwi_window_parent
s_window_message		istr_window_message

long il_individual_no
end variables

forward prototypes
public subroutine wf_set_parent (window window_parent)
public function integer wf_set_claim (long claim_no)
end prototypes

public subroutine wf_set_parent (window window_parent);/*------------------------------------------------------------------------*/
/*  This function sets a pointer to the parent window.                    */
/*------------------------------------------------------------------------*/

iwi_window_parent = window_parent
end subroutine

public function integer wf_set_claim (long claim_no);/*---------------------------------------------------------------------*/
/*  This function triggers the parent windows 'wf_set_claim', passing  */
/*  the selected claim number.                                         */
/*---------------------------------------------------------------------*/

Integer	li_ReturnCode
li_ReturnCode = iwi_window_parent.wf_set_claim(claim_no)
Return li_ReturnCode
end function

event open;call super::open;/*------------------------------------------------------------------*/
/*  Set the default to an 'or' type of search for indexing.         */
/*------------------------------------------------------------------*/

istr_window_message = Message.PowerObjectParm

wf_set_parent(istr_window_message.awi_parent_window)

il_individual_no = istr_window_message.al_doubleparm[1]

uo_claim_search.uf_set_parent(This)
uo_claim_search.uf_set_search_type(istr_window_message.as_StringParm[1])
uo_claim_search.uf_protect_searchtype("DISABLE")
end event

on w_individual_search.create
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

on w_individual_search.destroy
call super::destroy
destroy(this.uo_claim_search)
destroy(this.cb_ok)
destroy(this.cb_cancel)
end on

type uo_claim_search from u_claim_search within w_individual_search
integer x = 18
integer y = 8
integer width = 3186
integer height = 1828
integer taborder = 10
end type

on ue_doubleclicked;call u_claim_search::ue_doubleclicked;cb_ok.TriggerEvent(Clicked!)
end on

on uo_claim_search.destroy
call u_claim_search::destroy
end on

type cb_ok from commandbutton within w_individual_search
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
Long 				ll_count
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
		
		IF istr_window_message.as_stringparm[2] = 'MOD' THEN
			li_ReturnCode = MessageBox("Individual Search","Do you wish to change individual to " + ls_first_name + " " + ls_last_name + "?",Question!,YesNoCancel!)
		ELSEIF istr_window_message.as_stringparm[2] = 'ADD' THEN
			li_ReturnCode = MessageBox("Individual Search","Do you wish to add individual " + ls_first_name + " " + ls_last_name + "?",Question!,YesNoCancel!)
		END IF 
		
	   IF li_ReturnCode = 3 THEN			
    		Return
   	/*--------------------------------------------------------------*/
	   /*  Chose the 'No' button, so you must create      individual   */
	   /*--------------------------------------------------------------*/
   	ELSEIF li_ReturnCode = 2 THEN
	   	lstr_window_message.awi_parent_window = iwi_window_parent
		   lstr_window_message.al_DoubleParm[1] = 0
	   	CloseWithReturn(Parent,lstr_window_message)
			Return
   	/*--------------------------------------------------------------*/
	   /*  Chose the 'Yes' button     */
   	/*--------------------------------------------------------------*/
	   ELSE	
   		lstr_window_message.awi_parent_window = iwi_window_parent
		   lstr_window_message.al_DoubleParm[1] = uo_claim_search.dw_search_list_individual.GetItemNumber(li_Row,"individual_no")
	   	CloseWithReturn(Parent,lstr_window_message)
			Return
	   END IF
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

   	IF istr_window_message.as_stringparm[2] = 'MOD' THEN
			li_ReturnCode = MessageBox("Individual Search","Do you wish to change individual to " + ls_first_name + " " + ls_last_name + "?",Question!,YesNoCancel!)
		ELSEIF istr_window_message.as_stringparm[2] = 'ADD' THEN
			li_ReturnCode = MessageBox("Individual Search","Do you wish to add individual " + ls_first_name + " " + ls_last_name + "?",Question!,YesNoCancel!)
		END IF
		
	   IF li_ReturnCode = 3 THEN			
    		Return
   	/*--------------------------------------------------------------*/
	/*  Chose the 'No' button, so you must send back 0 for          */
   	/*  individual no                                               */
	/*--------------------------------------------------------------*/
   	ELSEIF li_ReturnCode = 2 THEN
	   	lstr_window_message.awi_parent_window = iwi_window_parent
		   lstr_window_message.al_DoubleParm[1] = 0
	   	CloseWithReturn(Parent,lstr_window_message)
			Return
   	/*--------------------------------------------------------------*/
	   /*  Chose the 'Yes' button, so go and create the claim.         */
   	/*--------------------------------------------------------------*/
	   ELSE	
			
			//Check for active annuity eligiblity and don't let the switch if there is.
			
			Select Count(*)
			Into    :ll_count
			From  ANNUITY_ACCOUNT a, ANNUITY_ELIGIBILITY b
			Where a.annuity_account_no = b.annuity_account_no
			And   annuity_eligibility_status_code = 'A'
			And   individual_no = :il_individual_no
			Using SQLCA;
			
			SQLCA.nf_handle_error('w_indidivual_search','cb_ok.clicked','ANNUITY_ACCOUNT')
			
			IF ll_count > 0 THEN
				MessageBox('Error','Active annuity eligibility exists for the individual. The individual can not be changed until their account has been resolved.',Exclamation!)
				Return
			END IF
				
			lstr_window_message.awi_parent_window = iwi_window_parent
			lstr_window_message.al_DoubleParm[1] = uo_claim_search.dw_search_list.GetItemNumber(li_Row,"individual_no")
			CloseWithReturn(Parent,lstr_window_message)
			Return
	   END IF
   ELSE
   	lstr_window_message.awi_parent_window = iwi_window_parent
	   lstr_window_message.al_DoubleParm[1] = 0
   	CloseWithReturn(Parent,lstr_window_message)
		Return
   END IF
END IF
end event

type cb_cancel from commandbutton within w_individual_search
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

