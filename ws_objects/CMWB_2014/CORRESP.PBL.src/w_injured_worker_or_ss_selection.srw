$PBExportHeader$w_injured_worker_or_ss_selection.srw
$PBExportComments$select injured worker or surviving spouse from main menu based on DD criteria
forward
global type w_injured_worker_or_ss_selection from window
end type
type dw_details from u_dw_online within w_injured_worker_or_ss_selection
end type
type cb_cancel from commandbutton within w_injured_worker_or_ss_selection
end type
type cb_ok from commandbutton within w_injured_worker_or_ss_selection
end type
end forward

global type w_injured_worker_or_ss_selection from window
integer x = 1335
integer y = 688
integer width = 3090
integer height = 688
boolean titlebar = true
string title = "Select Correct Recipient"
windowtype windowtype = response!
long backcolor = 67108864
dw_details dw_details
cb_cancel cb_cancel
cb_ok cb_ok
end type
global w_injured_worker_or_ss_selection w_injured_worker_or_ss_selection

type variables
LONG	il_claim_no
end variables

event open;INTEGER              li_record_count

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


/*
If accessing the module from the menu item, a search for a claim must first be performed.  
The menu item is only enabled if a claim is retrieved into the tombstone.  If the identified 
claim does not have a surviving spouse then all claims where the identified individual is the 
injured worker are loaded into the module.  If the selected claim has a surviving spouse as a 
claim participant then the user must select the individual (injured worker or surviving spouse) for 
which to open the module.  If the injured worker is selected then the data for all claims for the 
injured worker is displayed.  If the surviving spouse is selected then only the data for that individual
for the selected claim is displayed.
*/

/* grab the claim_no from the calling menu */
il_claim_no = Message.DoubleParm	

/* make sure we have a valid claim no */
IF isnull(il_claim_no) OR il_claim_no <= 0 THEN
	//problem - return out
	CLOSE(THIS)
	RETURN 
END IF 

/*	Set up the DW */
dw_details.SetTransObject(SQLCA)
li_record_count = dw_details.retrieve(il_claim_no)
SQLCA.nf_handle_error("w_injured_worker_or_ss","open","li_record_count = dw_details.retrieve(il_claim_no)")

/* check the record count */
IF li_record_count < 1 THEN
	CLOSE(THIS)
	RETURN 
END IF 


end event

on w_injured_worker_or_ss_selection.create
this.dw_details=create dw_details
this.cb_cancel=create cb_cancel
this.cb_ok=create cb_ok
this.Control[]={this.dw_details,&
this.cb_cancel,&
this.cb_ok}
end on

on w_injured_worker_or_ss_selection.destroy
destroy(this.dw_details)
destroy(this.cb_cancel)
destroy(this.cb_ok)
end on

type dw_details from u_dw_online within w_injured_worker_or_ss_selection
integer x = 5
integer y = 8
integer width = 3067
integer height = 440
integer taborder = 10
string dataobject = "d_claims_individuals_for_annuities"
boolean controlmenu = true
boolean vscrollbar = true
boolean righttoleft = true
end type

event constructor;call super::constructor;THIS.uf_setselect(1)
end event

type cb_cancel from commandbutton within w_injured_worker_or_ss_selection
integer x = 1499
integer y = 496
integer width = 379
integer height = 96
integer taborder = 70
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cance&l"
end type

event clicked;s_window_message  lstr_message

//setup the rest of the structure
lstr_message.al_doubleparm[1] 	= 0
lstr_message.as_stringparm[1] 	= ''
		
//close this window
CloseWithReturn(Parent, lstr_message)


end event

type cb_ok from commandbutton within w_injured_worker_or_ss_selection
integer x = 1079
integer y = 496
integer width = 379
integer height = 96
integer taborder = 60
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
end type

event clicked;LONG			ll_individual_no
INTEGER 	li_row
STRING      	ls_claim_role
s_window_message  lstr_message
		
//grab the selected row and see if the user has selected the surviving spouse or not
li_row = dw_details.getrow()

IF ISNULL(li_row) THEN li_row = 0

IF li_row <= 0 THEN 
	messagebox('Invalid Selection', 'Please select a valid Annuity Account or cancel action.')
	RETURN 
END IF 

//grab the claim role code
ls_claim_role 		= dw_details.getitemstring(li_row,'claim_role_code')
ll_individual_no 	= dw_details.getitemnumber(li_row,'individual_no')
	
//make sure we have a valid value
IF ISNULL(ls_claim_role) OR TRIM(ls_claim_role) = "" THEN
	messagebox('Claim Role Code', 'The Claim Role Code could not be determined. Please try again.')
	RETURN 
END IF 
	
//setup the rest of the structure
lstr_message.al_doubleparm[1] 	= ll_individual_no
lstr_message.as_stringparm[1] 	= ls_claim_role
		
//close this window
CloseWithReturn(Parent, lstr_message)
		

end event

