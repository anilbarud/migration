$PBExportHeader$w_claim_participant_list.srw
forward
global type w_claim_participant_list from window
end type
type st_2 from statictext within w_claim_participant_list
end type
type dw_claim_participant_list from u_datawindow within w_claim_participant_list
end type
type cb_close from commandbutton within w_claim_participant_list
end type
type cb_ok from commandbutton within w_claim_participant_list
end type
type st_participant from statictext within w_claim_participant_list
end type
end forward

global type w_claim_participant_list from window
integer width = 3822
integer height = 1376
boolean titlebar = true
string title = "Claim Participant List"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
st_2 st_2
dw_claim_participant_list dw_claim_participant_list
cb_close cb_close
cb_ok cb_ok
st_participant st_participant
end type
global w_claim_participant_list w_claim_participant_list

type variables

end variables

on w_claim_participant_list.create
this.st_2=create st_2
this.dw_claim_participant_list=create dw_claim_participant_list
this.cb_close=create cb_close
this.cb_ok=create cb_ok
this.st_participant=create st_participant
this.Control[]={this.st_2,&
this.dw_claim_participant_list,&
this.cb_close,&
this.cb_ok,&
this.st_participant}
end on

on w_claim_participant_list.destroy
destroy(this.st_2)
destroy(this.dw_claim_participant_list)
destroy(this.cb_close)
destroy(this.cb_ok)
destroy(this.st_participant)
end on

event open;long ll_individual_no, ll_rows, ll_annuity_account_no, ll_annuity_payout_no
STRING ls_individual_name, ls_sex, ls_claim_role_code
INT li_cntr
s_window_message lstr_message

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


lstr_message = Message.PowerObjectParm 

ll_individual_no = lstr_message.al_doubleparm[1]  // this is the annuity benefit holder
ll_annuity_account_no = lstr_message.al_doubleparm[2]
ll_annuity_payout_no = lstr_message.al_doubleparm[3]

dw_claim_participant_list.settransObject(SQLCA)
ll_rows = dw_claim_participant_list.retrieve(ll_individual_no, ll_annuity_account_no, ll_annuity_payout_no)
SQLCA.nf_handle_error('w_claim_participant_list','dw_claim_participant_list.retrieve','open event')

IF ll_rows < 1 THEN
	MESSAGEBOX("No Participants", "There are no additional claim participants for any claims belonging to this individual.")
	CLOSE (THIS)
ELSE
	ls_individual_name = lstr_message.as_stringparm[1]
	this.title = "Claim Participants For All Claims Belonging To " + UPPER(ls_individual_name)
	st_participant.text = "Claim participant list for " + ls_individual_name
	
	FOR li_cntr = 1 to ll_rows		
		ls_claim_role_code = dw_claim_participant_list.getItemString(li_cntr,'claim_role_code')
		ls_sex = dw_claim_participant_list.getItemString(li_cntr,'sex')
		
		dw_claim_participant_list.setItem(li_cntr, 'recipient_type_code','I')  // default to 'I'ndividual
		
		CHOOSE CASE ls_claim_role_code
								
			CASE 'C'
				dw_claim_participant_list.setItem(li_cntr, 'annuity_role_code', 'C')
				
			CASE 'SS'
				// Clai participants with claim role code of Surviving Spouses that are are added as dependant spouses for Annuity purposes
				dw_claim_participant_list.setItem(li_cntr, 'annuity_role_code', '01')
				
			CASE 'DC'
				IF ls_sex = 'M' THEN
					dw_claim_participant_list.setItem(li_cntr, 'annuity_role_code', '08')
				ELSEIF ls_sex = 'F' THEN
					dw_claim_participant_list.setItem(li_cntr, 'annuity_role_code', '09')
				END IF
				
			CASE  'GU'
				dw_claim_participant_list.setItem(li_cntr, 'annuity_role_code', 'GU')
				
			CASE 'TR'
				dw_claim_participant_list.setItem(li_cntr, 'annuity_role_code', 'TR')
				dw_claim_participant_list.setItem(li_cntr, 'recipient_type_code','O')
				
			CASE 'OD'
				// at this point, no way to determine what an 'Other Dependant' might be, as far as 
				// an annuity role code goes, so user will have to select from the dropdown list
				
		END CHOOSE
		
	NEXT	

END IF











end event

type st_2 from statictext within w_claim_participant_list
integer x = 1577
integer y = 1020
integer width = 2226
integer height = 76
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16711680
long backcolor = 67108864
string text = "Select participants, and VERIFY the correct Annuity Role and Recipient Type"
boolean focusrectangle = false
end type

type dw_claim_participant_list from u_datawindow within w_claim_participant_list
integer x = 91
integer y = 16
integer width = 3607
integer height = 968
integer taborder = 10
string dataobject = "d_claim_participant_list"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event constructor;call super::constructor;
datawindowchild ldw_child, ldw_child2

this.getChild('annuity_role_code', ldw_child)
this.getChild('recipient_type_code', ldw_child2)

ldw_child.setFilter("active_flag = 'Y'")
ldw_child2.setFilter("active_flag = 'Y'")

ldw_child.filter()
ldw_child2.filter()
end event

type cb_close from commandbutton within w_claim_participant_list
integer x = 1819
integer y = 1140
integer width = 315
integer height = 104
integer taborder = 30
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Close"
end type

event clicked;Close (Parent)
end event

type cb_ok from commandbutton within w_claim_participant_list
integer x = 1399
integer y = 1140
integer width = 315
integer height = 104
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "OK"
end type

event clicked;long ll_rows, ll_find_duplicate_individual_row
INT li_cntr, li_cntr2
STRING ls_annuity_role_code
boolean lb_error
s_window_message lstr_message

dw_claim_participant_list.setredraw(false)
dw_claim_participant_list.setFilter("selected = 'Y'")
dw_claim_participant_list.filter()

ll_rows = dw_claim_participant_list.rowcount()


FOR li_cntr = 1 to ll_rows
	lstr_message.al_doubleparm[li_cntr] = dw_claim_participant_list.getItemNumber(li_cntr, 'individual_no')
	// check for the same individual, selected more than once
	IF li_cntr < ll_rows THEN
		ll_find_duplicate_individual_row = dw_claim_participant_list.find("individual_no = " + STRING(lstr_message.al_doubleparm[li_cntr]), li_cntr +1, ll_rows)
		IF ll_find_duplicate_individual_row > 0 THEN 
			MESSAGEBOX("Duplicate individual found","You have selected the same individual more than once. Please deselect one (or more) and try again.", INFORMATION!)
			dw_claim_participant_list.setredraw(true)
			dw_claim_participant_list.setFilter("")
			dw_claim_participant_list.filter()
			Return 0
		END IF
	END IF
	ls_annuity_role_code = dw_claim_participant_list.getItemString(li_cntr, 'annuity_role_code')
	
	IF isNull(ls_annuity_role_code) OR ls_annuity_role_code = "" THEN
		MESSAGEBOX("Invalid Annuity Role Code", "Please select a valid annuity role code for each selected participant." ,INFORMATION!)
		lb_error = true
		EXIT
	END IF
	
	li_cntr2++
	lstr_message.as_stringparm[li_cntr2] = ls_annuity_role_code
	
	li_cntr2++
	lstr_message.as_stringparm[li_cntr2] = dw_claim_participant_list.getItemString(li_cntr, 'recipient_type_code')
NEXT 

dw_claim_participant_list.setredraw(true)

IF NOT lb_error THEN
	CloseWithReturn(Parent, lstr_message)
ELSE
	dw_claim_participant_list.setFilter("")
	dw_claim_participant_list.filter()
	dw_claim_participant_list.setredraw(true)
END IF
end event

type st_participant from statictext within w_claim_participant_list
integer x = 18
integer y = 1020
integer width = 1536
integer height = 144
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16711680
long backcolor = 67108864
string text = "Claim participant list for"
boolean focusrectangle = false
end type

