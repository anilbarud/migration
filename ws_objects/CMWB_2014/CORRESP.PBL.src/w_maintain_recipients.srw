$PBExportHeader$w_maintain_recipients.srw
$PBExportComments$Maintains recipient list for a claim. Excluding card file entries
forward
global type w_maintain_recipients from window
end type
type rb_wadv from radiobutton within w_maintain_recipients
end type
type dw_get_address from u_dw_online within w_maintain_recipients
end type
type st_1 from statictext within w_maintain_recipients
end type
type sle_addr_code from singlelineedit within w_maintain_recipients
end type
type st_hidden from statictext within w_maintain_recipients
end type
type st_addressee_id from statictext within w_maintain_recipients
end type
type sle_addressee_id from singlelineedit within w_maintain_recipients
end type
type dw_individuals from u_dw_online within w_maintain_recipients
end type
type cb_close from commandbutton within w_maintain_recipients
end type
type cb_delete from commandbutton within w_maintain_recipients
end type
type cb_edit from commandbutton within w_maintain_recipients
end type
type cb_new from commandbutton within w_maintain_recipients
end type
type rb_other from radiobutton within w_maintain_recipients
end type
type rb_representative from radiobutton within w_maintain_recipients
end type
type rb_provider from radiobutton within w_maintain_recipients
end type
type rb_employer from radiobutton within w_maintain_recipients
end type
type rb_claimant from radiobutton within w_maintain_recipients
end type
type gb_individuals from groupbox within w_maintain_recipients
end type
type gb_groups from groupbox within w_maintain_recipients
end type
end forward

global type w_maintain_recipients from window
integer x = 32
integer y = 716
integer width = 2071
integer height = 1168
boolean titlebar = true
string title = "Maintain Recipients"
boolean controlmenu = true
windowtype windowtype = child!
long backcolor = 67108864
rb_wadv rb_wadv
dw_get_address dw_get_address
st_1 st_1
sle_addr_code sle_addr_code
st_hidden st_hidden
st_addressee_id st_addressee_id
sle_addressee_id sle_addressee_id
dw_individuals dw_individuals
cb_close cb_close
cb_delete cb_delete
cb_edit cb_edit
cb_new cb_new
rb_other rb_other
rb_representative rb_representative
rb_provider rb_provider
rb_employer rb_employer
rb_claimant rb_claimant
gb_individuals gb_individuals
gb_groups gb_groups
end type
global w_maintain_recipients w_maintain_recipients

type variables
long                        vil_individuals_rowno, vil_search_rowno
string                      vis_group_type, vis_group_desc
string                      vis_search_list_title
s_correspond_claim  vistr_correspond_claim
int		  vii_return_code
datawindow	  d_employer_list
datawindow	  d_service_provider
w_sheet		  viw_sheet
w_correspond	  viw_correspond


end variables

forward prototypes
public function integer wf_redraw_individuals_dw ()
public function long wf_establish_individual (string group, string action)
public function integer wf_refresh_indiv_lst ()
public function boolean wf_update ()
public function string wf_vldte_label (string p_label)
public function integer wf_vldte_card_file (long al_rcpnt_id)
end prototypes

public function integer wf_redraw_individuals_dw ();// ************************************************************************************************
// AUTHOR:	MW George
// Modified:	94.09.28

// ************************************************************************************************
// THIS FUNCTION WILL REDISPLAY THE LIST OF INDIVIDUALS

int	vli_nbr_indiv

dw_individuals.SetRedraw(FALSE)
dw_individuals.Sort()
dw_individuals.SelectRow(0,FALSE)
dw_individuals.SelectRow(1,TRUE)
dw_individuals.SetRedraw(TRUE)

vli_nbr_indiv = dw_individuals.RowCount()
IF vli_nbr_indiv > 0 THEN
	cb_edit.enabled = True
	cb_delete.enabled = True
ELSE
	cb_delete.enabled = False
	cb_edit.enabled = False
END IF

//dw_individuals.SetFocus()

RETURN 1
end function

public function long wf_establish_individual (string group, string action);// ************************************************************************************************
// AUTHOR:	MW George
// Modified:	94.10.05

// ************************************************************************************************
// THIS FUNCTION WILL ALLOWS THE USER TO ESTABLISH A NEW CLAIM RECIPIENT
// AND SETS THE CLAIM_RECIPIENT IDENTIFIER and its description

// *********************************************************************************************
// SET CLAIM_RECIPIENT IDENTIFIER

vistr_correspond_claim.rcpnt.rcpnt_type_code	=group

vistr_correspond_claim.rcpnt.claim_rcpnt_id	= 0
vistr_correspond_claim.rcpnt.claim_rcpnt_label	= ""
vistr_correspond_claim.rcpnt.action			= action
vistr_correspond_claim.rcpnt.default_status	= "N"

Return vistr_correspond_claim.rcpnt.claim_rcpnt_id
end function

public function integer wf_refresh_indiv_lst ();// **************************************************************************************************
// RETRIEVE ALL EXISTING CLAIM RECIPIENTS OF TYPE "CLAIMANT" ASSOCIATED WITH
// THE CURRENT CLAIM AND DISPLAY THE RESULTANT LIST IN THE "INDIVIDUALS"
// DATAWINDOW

int	vli_nbr_rows

vli_nbr_rows = dw_individuals.Retrieve(vistr_correspond_claim.claim_no, vis_group_type)
vii_return_code = SQLCA.nf_handle_error("dw_individuals","w_maintain_recipients","in wf_refresh_indiv_lst")
IF vli_nbr_rows < 0 THEN
	// *********************************************************************************************
	// THE DATABASE OF CLAIM RECIPIENTS COULD NOT BE PROPERLY ACCESSED.
	MessageBox("UNRECOVERABLE DATABASE ERROR: 513-CLAIMANT","THE LIST OF" &
				+ " CLAIM RECIPIENTS COULD NOT BE FOUND." &
				+ "~n~r~n~rPLEASE CONTACT YOUR SYSTEM ADMINISTRATOR.",StopSign!)

	Return -1
End IF

wf_redraw_individuals_dw()

Return 0
end function

public function boolean wf_update ();// process update, return false if update failed

dw_individuals.Update()
vii_return_code = SQLCA.nf_handle_error("dw_individuals","w_maintain_recipients","on wf_update")
IF vii_return_code <> 0 THEN
	Return False
END IF

Return True

end function

public function string wf_vldte_label (string p_label);//**********************************************************************************************************
///
// This function will keep checking for duplicate labels
//	every time a new label is entered by the user until
// the label is unique.
//
// Parms - Return new label
//			 		   none
//						label passed
//

long				vll_nbr_rows, vll_return_row
string			vls_label,vls_find_expression
boolean			vlb_found = True


vls_label = p_label
/* Ensure that the label begins with a valid character else reject value
*/

IF   vls_label = " " OR ISNULL(vls_label)  THEN
	vls_label = 'none'
	vlb_found = False
END IF

Do While vlb_found
// Added the tilde to signify a nested string, since the string can contain a quote

	
	vls_label = UPPER(vls_label)
//	"WHERE ( PROVIDER.name like ~~~""+is_name+"~~~") OR "+&
	vls_find_expression =  "UPPER(claim_recipient_label) = ~""+vls_label+"~""
	vll_return_row = dw_individuals.Find(vls_find_expression,1,dw_individuals.Rowcount())			
	IF vll_return_row > 0 THEN
		MessageBox(title,"An individual with this NAME already exists for this claim." &
				+"~n~r~n~rPlease change.")
		OpenWithParm(w_label,vls_label)
		vls_label = message.StringParm	// returns new label or 'none', check new label for duplicates
		IF vls_label = "none" THEN exit
	ELSE
		vlb_found = False
	END IF
LOOP

	
Return vls_label
end function

public function integer wf_vldte_card_file (long al_rcpnt_id);//********************************************************************************
///
// This function will check for any duplicate entries within a group 
//	(SVPV, EMPL, etc..). 
// 
// Parms - Return 0 Successfull
//			 		  -1 Id already selected
//

long				vll_nbr_rows, vll_rcpnt_id
int				vli_row_cntr

vll_nbr_rows = dw_individuals.RowCount()
	
FOR vli_row_cntr = 1 TO vll_nbr_rows
	vll_rcpnt_id = dw_individuals.GetItemNumber(vli_row_cntr, "correspond_recipient_id")
	IF vll_rcpnt_id = al_rcpnt_id  then
		MessageBox(title,"You cannot select an individual from the card file twice for the same group." &
				+"~n~rPlease select another or cancel.")
		Return -1
	END IF
NEXT	

Return 0
end function

event open;// ******************************************************************************************************************************
// AUTHOR:	MW George
// Modified:	94.10.20

// **************************************************************************************************
// DECLARATIONS

int		 claimant_types
long	corr_recipient_sqlcode, claim_recipient_sqlcode,addressee_id
string	label

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


SetPointer(HourGlass!)

vistr_correspond_claim  = Message.PowerObjectParm

vis_group_type = "CLMT"



viw_sheet = vistr_correspond_claim.sheet_window
viw_correspond = vistr_correspond_claim.parent_window

// Set parent window to current for use in w_define
vistr_correspond_claim.parent_window = this


// **************************************************************************************************
// SET THE WINDOW HEIGHT TO HIDE THE SEARCH LIST DATA WINDOW
//, AND SET THE "CLAIMANT" RADIO BUTTON TO
// CHECKED and t to set instance variable value to proper description.

rb_claimant.checked	= TRUE
viw_sheet.SetMicroHelp("Ready")
SELECT  correspond_recipient_type_desc into :vis_group_desc
From Correspond_Recipient_Type where correspond_recipient_type_cd = :vis_group_type
Using SQLCA;
 
IF SQLCA.nf_handle_error("SQL Select","w_maintain_recipients","in clicked of rb_claimant") < 0 THEN RETURN -1
// **************************************************************************************************
// RETRIEVE ALL EXISTING CLAIM RECIPIENTS OF TYPE "CLAIMANT" ASSOCIATED WITH
// THE CURRENT CLAIM AND DISPLAY THE RESULTANT LIST IN THE "INDIVIDUALS"
// DATAWINDOW

vil_search_rowno = 0
dw_individuals.SetTransObject(SQLCA)
claimant_types = dw_individuals.Retrieve(vistr_correspond_claim.claim_no,vis_group_type)
vii_return_code = SQLCA.nf_handle_error("dw_individuals","w_maintain_recipients","on open event")

IF claimant_types < 0 THEN
	MessageBox("UNRECOVERABLE DATABASE ERROR: 513-OPEN","THE LIST OF" &
				+ " CLAIM RECIPIENTS COULD NOT BE FOUND." &
				+ "~n~r~n~rPLEASE CONTACT YOUR SYSTEM ADMINISTRATOR.",StopSign!)

	close (this)
	Return
END IF

// *********************************************************************************************
// AT LEAST ONE CLAIMANT TYPE HAS BEEN ESTABLISHED SO DISPLAY THE LIST
// IN THE DATAWINDOW

wf_redraw_individuals_dw()
vil_individuals_rowno = 1

end event

on closequery;If isValid(viw_correspond) then 
	viw_correspond.wf_reset_buttons(True, "")
	viw_correspond.cb_recipients.SetFocus()
END IF
end on

event activate;// ************************************************************************************************
// AUTHOR:	MW George
// Modified:	94.10.05

wf_redraw_individuals_dw()
end event

on w_maintain_recipients.create
this.rb_wadv=create rb_wadv
this.dw_get_address=create dw_get_address
this.st_1=create st_1
this.sle_addr_code=create sle_addr_code
this.st_hidden=create st_hidden
this.st_addressee_id=create st_addressee_id
this.sle_addressee_id=create sle_addressee_id
this.dw_individuals=create dw_individuals
this.cb_close=create cb_close
this.cb_delete=create cb_delete
this.cb_edit=create cb_edit
this.cb_new=create cb_new
this.rb_other=create rb_other
this.rb_representative=create rb_representative
this.rb_provider=create rb_provider
this.rb_employer=create rb_employer
this.rb_claimant=create rb_claimant
this.gb_individuals=create gb_individuals
this.gb_groups=create gb_groups
this.Control[]={this.rb_wadv,&
this.dw_get_address,&
this.st_1,&
this.sle_addr_code,&
this.st_hidden,&
this.st_addressee_id,&
this.sle_addressee_id,&
this.dw_individuals,&
this.cb_close,&
this.cb_delete,&
this.cb_edit,&
this.cb_new,&
this.rb_other,&
this.rb_representative,&
this.rb_provider,&
this.rb_employer,&
this.rb_claimant,&
this.gb_individuals,&
this.gb_groups}
end on

on w_maintain_recipients.destroy
destroy(this.rb_wadv)
destroy(this.dw_get_address)
destroy(this.st_1)
destroy(this.sle_addr_code)
destroy(this.st_hidden)
destroy(this.st_addressee_id)
destroy(this.sle_addressee_id)
destroy(this.dw_individuals)
destroy(this.cb_close)
destroy(this.cb_delete)
destroy(this.cb_edit)
destroy(this.cb_new)
destroy(this.rb_other)
destroy(this.rb_representative)
destroy(this.rb_provider)
destroy(this.rb_employer)
destroy(this.rb_claimant)
destroy(this.gb_individuals)
destroy(this.gb_groups)
end on

type rb_wadv from radiobutton within w_maintain_recipients
integer x = 55
integer y = 804
integer width = 462
integer height = 80
integer taborder = 60
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Worker~'s Adv."
end type

event clicked;// ******************************************************************************************************************************
// AUTHOR:	MW George
// Modified:	94.10.20
//

int	vli_rc

	SetPointer(HourGlass!)

	vis_group_type = "WADV"
	
	SELECT  correspond_recipient_type_desc into :vis_group_desc
	  From Correspond_Recipient_Type where correspond_recipient_type_cd = :vis_group_type
 	 Using SQLCA;
 
	IF SQLCA.nf_handle_error("SQL Select","w_maintain_recipients","in clicked of worker's advocate") < 0 THEN RETURN -1
 
	vli_rc = wf_refresh_indiv_lst()
	IF vli_rc < 0 THEN Close(parent)

end event

type dw_get_address from u_dw_online within w_maintain_recipients
boolean visible = false
integer x = 334
integer y = 836
integer height = 360
integer taborder = 120
string dataobject = "d_get_address"
end type

event dberror;SQLCA.SQLDBCode = sqldbcode
SQLCA.SQLErrText = sqlerrtext
RETURN 1
end event

type st_1 from statictext within w_maintain_recipients
integer x = 2743
integer y = 208
integer width = 736
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Addressee RcpntTypeCode"
boolean focusrectangle = false
end type

type sle_addr_code from singlelineedit within w_maintain_recipients
integer x = 2761
integer y = 288
integer width = 695
integer height = 80
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean autohscroll = false
borderstyle borderstyle = stylelowered!
end type

type st_hidden from statictext within w_maintain_recipients
integer x = 2853
integer y = 416
integer width = 558
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "THIS AREA IS HIDDEN"
boolean focusrectangle = false
end type

type st_addressee_id from statictext within w_maintain_recipients
integer x = 2907
integer y = 32
integer width = 361
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Addressee ID"
boolean focusrectangle = false
end type

type sle_addressee_id from singlelineedit within w_maintain_recipients
integer x = 2761
integer y = 112
integer width = 695
integer height = 80
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean autohscroll = false
borderstyle borderstyle = stylelowered!
end type

type dw_individuals from u_dw_online within w_maintain_recipients
integer x = 699
integer y = 136
integer width = 928
integer height = 732
integer taborder = 70
string dataobject = "d_individuals"
boolean hscrollbar = true
boolean vscrollbar = true
end type

on rowfocuschanged;vil_individuals_rowno = this.GetRow()

IF vil_individuals_rowno > 0 THEN

	// Deselect previous selection made in DataWindow
	this.SelectRow(0,False)
	this.SelectRow(vil_individuals_rowno,true)
END IF
end on

event dberror;SQLCA.SQLDBCode = sqldbcode
SQLCA.SQLErrText = sqlerrtext
RETURN 1
end event

type cb_close from commandbutton within w_maintain_recipients
integer x = 1691
integer y = 816
integer width = 311
integer height = 96
integer taborder = 110
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Close"
boolean default = true
end type

on clicked;// ************************************************************************************************
// AUTHOR:	MW George
// Modified:	94.09.08
viw_sheet.SetMicroHelp("Ready")
close(parent)
end on

type cb_delete from commandbutton within w_maintain_recipients
integer x = 1696
integer y = 656
integer width = 311
integer height = 96
integer taborder = 100
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Delete"
end type

event clicked;// ******************************************************************************************************************************
// AUTHOR:	MW George
// Modified:	94.10.10

// ******************************************************************************************************************************
// DECLARATIONS

int				urc, total_indv, answer
long				vll_row_nbr, claim_rcpnt_id, recipient_no, total_recipients
string			label, vls_addr_location_code
string			recipient_type_code, vls_card_file

// ******************************************************************************************************************************
// DELETION RULES:
//	DISALLOW THE DELETION IF THE INDIVIDUAL IS A RECIPIENT ON ANY CORRESPONDENCE, SENT OR
//		UNSENT (Message 005);
//	ISSUE A WARNING IF THE INDIVIDUAL IS NOT A RECIPIENT ON ANY CORRESPONDENCE (Message 002)

// ******************************************************************************************************************************
// ENSURE THE USER SELECTED AN INDIVIDUAL FROM THE LIST

dw_get_address.SetTransObject(SQLCA)
dw_individuals.SetTransObject(SQLCA)

vil_individuals_rowno = dw_individuals.GetSelectedRow(0)

IF vil_individuals_rowno = 0 THEN
	MessageBox("DELETE MESSAGE: 513-004","To DELETE an Individual, an item must be selected" &
				+" ~n~rfrom the Individuals List.")
END IF

// Gather recipient data needed
claim_rcpnt_id	= dw_individuals.GetitemNumber(vil_individuals_rowno,"correspond_recipient_id")
recipient_no	= dw_individuals.GetitemNumber(vil_individuals_rowno,"recipient_no")
label = UPPER(dw_individuals.GetItemString(vil_individuals_rowno,"claim_recipient_label"))
vls_addr_location_code = dw_individuals.GetItemString(vil_individuals_rowno, "address_location_code")
recipient_type_code = dw_individuals.GetItemString(vil_individuals_rowno, "correspond_recipient_type_cd")
vls_card_file = dw_individuals.GetItemString(vil_individuals_rowno, "card_file_flag")

// Don't allow the default recipients to be deleted
IF label = "EMPLOYER" THEN
	MessageBox(parent.title,"Cannot delete the Employer from the Individuals List.")
	Return 
END IF		
IF label = "CLAIMANT" THEN
	MessageBox(parent.title,"Cannot delete the Claimant from the Individuals List.")
	Return 
END IF

// *****************************************************************************************
// DETERMINE IF ANY CORRESPONDENCE CONTAINS THE SELECTED INDIVIDUAL AS A RECIPIENT 
//	(look in correspondence_recipient)
//	NOTE - If recipient from card file table check the entire table  
//			 to see if it exists, somebody else might be using the id - reason is card file
//			 is a table used by everybody.
//			 IF recipient not from card file, only check the table using the id and 
//			 claim number. (Default for datawindow) 
///
IF UPPER(Trim(vls_card_file)) = 'Y' THEN		// Card file entry
 	SELECT Count(*) 
	INTO	 :total_recipients
	FROM	 CORRESPONDENCE_RECIPIENT
   WHERE  (correspond_recipient_id = :claim_rcpnt_id ) AND  
          (correspond_recipient_type_cd = :recipient_type_code )  
	USING  SQLCA;  
ELSE
 	SELECT Count(*) 
	INTO	 :total_recipients
	FROM	 CORRESPONDENCE_RECIPIENT
   WHERE  (claim_no = :vistr_correspond_claim.claim_no ) AND  
          (correspond_recipient_id = :claim_rcpnt_id )  
	USING SQLCA;  
END IF
vii_return_code = SQLCA.nf_handle_error("","w_maintain_recipients","on cb_delete clicked")

 
IF total_recipients > 0 THEN
		
	// ********************************************************************************************************************
	// THERE IS AT LEAST ONE PIECE OF CORRESPONDENCE CONTAINING THE SELECTED INDIVIDUAL SO
	// DISALLOW THE DELETION AND DISPLAY MESSAGE
	MessageBox("DELETION MESSAGE: 513-005","You cannot delete the '"+label+"' since there is "&
				+"~n~r at least one piece of Correspondence addressed to this Individual.")
	Return

END IF

// ***************************************************************************************************************
// NO CORRESPONDENCE CONTAINS THE SELECTED INDIVIDUAL SO SIMPLY CONFIRM THE
// DELETION BY DISPLAYING MESSAGE 002

answer = MessageBox("DELETE CONFIRMATION MESSAGE: 513-002","Are you sure you want to DELETE" &
			+"~n~rthe Individual labelled '"+label+"'?",Question!,YesNo!)

if answer = 1 then
	
	SQLCA.nf_begin_transaction()
	
	parent.SetRedraw(False)
	IF vls_addr_location_code = "C" AND &
		UPPER(Trim(vls_card_file)) = 'N' THEN

		// Delete claim's custom address - claim_recipient_address is child of claim_correspond_recipient
		vll_row_nbr = dw_get_address.Retrieve(claim_rcpnt_id)
		vii_return_code = SQLCA.nf_handle_error("dw_get_address","w_maintain_recipients","on cb_delete clicked")
		
		IF vll_row_nbr = 0 THEN
			MessageBox(title,"THE INDIVIDUAL'S ADDRESS COULD NOT BE FOUND." &
					+ "~r~n~r~nPLEASE CONTACT YOUR SYSTEM ADMINISTRATOR",StopSign!)
			dw_individuals.SetFocus()
			Return
		END IF
		
		vls_card_file = dw_get_address.GetItemString(vll_row_nbr, "card_file_flag")
		IF UPPER(Trim(vls_card_file)) = 'N' THEN	// Check to make sure address is not a card file address
			dw_get_address.DeleteRow(vll_row_nbr)
			urc = dw_get_address.Update()
			vii_return_code = SQLCA.nf_handle_error("dw_get_address","w_maintain_recipients","on cb_delete clicked")
		END IF
	end if

	// Delete label from selection list
	dw_individuals.DeleteRow(vil_individuals_rowno)
	urc = dw_individuals.Update()
	vii_return_code = SQLCA.nf_handle_error("dw_individuals","w_maintain_recipients","on cb_delete clicked")


	SQLCA.nf_commit_transaction()
	
	wf_redraw_individuals_dw()
	parent.SetRedraw(True)
end if
end event

type cb_edit from commandbutton within w_maintain_recipients
integer x = 1691
integer y = 540
integer width = 311
integer height = 96
integer taborder = 90
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Edit..."
end type

event clicked;// ******************************************************************************************************************************
// AUTHOR:	MW George
// Modified:	94.10.10


vil_individuals_rowno = dw_individuals.GetSelectedRow(0)

IF vil_individuals_rowno = 0 THEN
	MessageBox("EDIT MESSAGE: 513-004","To EDIT an Individual, an item must be selected" &
				+" ~n~rfrom the Individuals List.")
END IF

// ********************************************************************************************************************
// SET CLAIM_RECIPIENT IDENTIFIER USING THE DATA FROM THE SELECTED INDIVIDUAL

vistr_correspond_claim.rcpnt.rcpnt_type_code				= dw_individuals.GetItemString(vil_individuals_rowno,"correspond_recipient_type_cd")
vistr_correspond_claim.rcpnt.claim_rcpnt_id				= dw_individuals.GetitemNumber(vil_individuals_rowno,"correspond_recipient_id")
vistr_correspond_claim.rcpnt.claim_rcpnt_label			= dw_individuals.GetItemString(vil_individuals_rowno,"claim_recipient_label")
vistr_correspond_claim.ext_addr.address_card_file		= dw_individuals.GetItemString(vil_individuals_rowno,"card_file_flag")
vistr_correspond_claim.rcpnt.action							= "Edit"
vistr_correspond_claim.rcpnt.default_status				= dw_individuals.GetItemString(vil_individuals_rowno,"default_address_flag")
vistr_correspond_claim.ext_addr.address_id				= dw_individuals.GetItemNumber(vil_individuals_rowno, "recipient_no")
vistr_correspond_claim.ext_addr.address_location_code	= dw_individuals.GetItemString(vil_individuals_rowno, "address_location_code")
vistr_correspond_claim.ext_addr.address_subtype_code	= dw_individuals.GetItemString(vil_individuals_rowno, "correspond_recipient_subtyp_cd")

// if service provider or employer don't allow editing of fields, same
// for card files and Claimant. Card files are maintained in another module
// and claimant is updated on the way into the correspondence driver. It
// can be changed from the individual module
if vistr_correspond_claim.ext_addr.address_location_code = "P" OR &
	vistr_correspond_claim.ext_addr.address_location_code = "E"	OR &
	vistr_correspond_claim.ext_addr.address_card_file = "Y" OR &
	vistr_correspond_claim.rcpnt.claim_rcpnt_label = "Claimant" then
	vistr_correspond_claim.rcpnt.action = "VIEW"
end if
/* Change this to invisible, hide is too expensive  */
parent.Hide()

OpenWithParm(w_define,vistr_correspond_claim, viw_sheet)


end event

type cb_new from commandbutton within w_maintain_recipients
integer x = 1691
integer y = 432
integer width = 311
integer height = 96
integer taborder = 80
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&New..."
end type

event clicked;// ******************************************************************************************************************************
// AUTHOR:	MW George
// Modified:	94.10.04

// ******************************************************************************************************************************
// DECLARATIONS

Long	rc

// ******************************************************************************************************************************
// ESTABLISH A NEW INDIVIDUAL
viw_sheet.SetMicroHelp("Ready")
vistr_correspond_claim.ext_addr.address_location_code	= "C"
vistr_correspond_claim.ext_addr.address_id				= -1
vistr_correspond_claim.ext_addr.address_card_file		= "N"
/* this function returns the value assigned to Correspondence Recipient Id which should be a long */
rc = wf_establish_individual(vis_group_type,"New")

if rc <> -1 then
	/*
	 OPEN "NEW INDIVIDUAL" WINDOW
	 Change this to visible= False, since I beleive Hide may cause null references if an
	 event is called for a hidden object - May 24,97 By Earl
	*/
	parent.Visible=False
	 rc = OpenWithParm(w_define,vistr_correspond_claim, viw_sheet)
	 
else
	close(parent)
end if

end event

type rb_other from radiobutton within w_maintain_recipients
integer x = 55
integer y = 672
integer width = 366
integer height = 80
integer taborder = 50
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Other"
end type

event clicked;// ******************************************************************************************************************************
// AUTHOR:	MW George
// Modified:	94.10.20
//

int	vli_rc

SetPointer(HourGlass!)

vis_group_type = "OTHR"
SELECT  correspond_recipient_type_desc into :vis_group_desc
From Correspond_Recipient_Type where correspond_recipient_type_cd = :vis_group_type
Using SQLCA;
 
IF SQLCA.nf_handle_error("SQL Select","w_maintain_recipients","in clicked of other") < 0 THEN RETURN -1
 

vli_rc = wf_refresh_indiv_lst()
IF vli_rc < 0 THEN Close(parent)

end event

type rb_representative from radiobutton within w_maintain_recipients
integer x = 55
integer y = 544
integer width = 384
integer height = 80
integer taborder = 40
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Represntve"
end type

event clicked;// ******************************************************************************************************************************
// AUTHOR:	MW George
// Modified:	94.10.20
//

int	vli_rc

SetPointer(HourGlass!)

vis_group_type = "LAWY"
SELECT  correspond_recipient_type_desc into :vis_group_desc
From Correspond_Recipient_Type where correspond_recipient_type_cd = :vis_group_type
Using SQLCA;
 
IF SQLCA.nf_handle_error("SQL Select","w_maintain_recipients","in clicked of rb_claimant") < 0 THEN RETURN -1
 


vli_rc = wf_refresh_indiv_lst()
IF vli_rc < 0 THEN Close(parent)


end event

type rb_provider from radiobutton within w_maintain_recipients
integer x = 55
integer y = 416
integer width = 366
integer height = 80
integer taborder = 30
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Provider"
end type

event clicked;// ******************************************************************************************************************************
// AUTHOR:	MW George
// Modified:	94.10.20
//

int	vli_rc

SetPointer(HourGlass!)

vis_group_type = "SVPV"
SELECT  correspond_recipient_type_desc into :vis_group_desc
From Correspond_Recipient_Type where correspond_recipient_type_cd = :vis_group_type
Using SQLCA;
 
IF SQLCA.nf_handle_error("SQL Select","w_maintain_recipients","in clicked of rb_provider") < 0 THEN RETURN -1
 


vli_rc = wf_refresh_indiv_lst()
IF vli_rc < 0 THEN Close(parent)


end event

type rb_employer from radiobutton within w_maintain_recipients
integer x = 55
integer y = 288
integer width = 384
integer height = 80
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Employer"
end type

event clicked;// ******************************************************************************************************************************
// AUTHOR:	MW George
// Modified:	94.10.20
//

int	vli_rc

SetPointer(HourGlass!)

vis_group_type = "EMPL"

SELECT  correspond_recipient_type_desc into :vis_group_desc
From Correspond_Recipient_Type where correspond_recipient_type_cd = :vis_group_type
Using SQLCA;
 
IF SQLCA.nf_handle_error("SQL Select","w_maintain_recipients","in clicked of rb_employer") < 0 THEN RETURN -1
 

vli_rc = wf_refresh_indiv_lst()
IF vli_rc < 0 THEN Close(parent)

end event

type rb_claimant from radiobutton within w_maintain_recipients
integer x = 55
integer y = 168
integer width = 366
integer height = 80
integer taborder = 10
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Claimant"
end type

event clicked;// ******************************************************************************************************************************
// AUTHOR:	MW George
// Modified:	94.10.20
// **************************************************************************************************
// 

int	vli_rc

SetPointer(HourGlass!)

vis_group_type = "CLMT"

SELECT  correspond_recipient_type_desc into :vis_group_desc
From Correspond_Recipient_Type where correspond_recipient_type_cd = :vis_group_type
Using SQLCA;
 
IF SQLCA.nf_handle_error("SQL Select","w_maintain_recipients","in clicked of rb_claimant") < 0 THEN RETURN -1
 
vli_rc = wf_refresh_indiv_lst()
IF vli_rc < 0 THEN Close(parent)





end event

type gb_individuals from groupbox within w_maintain_recipients
integer x = 663
integer y = 48
integer width = 1006
integer height = 868
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "&Individuals"
end type

type gb_groups from groupbox within w_maintain_recipients
integer x = 18
integer y = 48
integer width = 613
integer height = 876
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "&Groups"
end type

