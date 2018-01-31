$PBExportHeader$w_recipients.srw
$PBExportComments$Allows selection of recipients for a specific piece of correspondence.
forward
global type w_recipients from window
end type
type cb_cancel from commandbutton within w_recipients
end type
type st_1 from statictext within w_recipients
end type
type sle_addr_code from singlelineedit within w_recipients
end type
type st_hidden from statictext within w_recipients
end type
type st_addressee_id from statictext within w_recipients
end type
type sle_addressee_id from singlelineedit within w_recipients
end type
type dw_ccs from u_dw_online within w_recipients
end type
type dw_individuals from u_dw_online within w_recipients
end type
type cb_remove from commandbutton within w_recipients
end type
type cb_add from commandbutton within w_recipients
end type
type cb_replace from commandbutton within w_recipients
end type
type cb_ok from commandbutton within w_recipients
end type
type sle_addressee from singlelineedit within w_recipients
end type
type gb_cc from groupbox within w_recipients
end type
type gb_addressee from groupbox within w_recipients
end type
type gb_individuals from groupbox within w_recipients
end type
end forward

global type w_recipients from window
integer x = 9
integer y = 716
integer width = 2519
integer height = 1108
boolean titlebar = true
string title = "Recipients"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 67108864
cb_cancel cb_cancel
st_1 st_1
sle_addr_code sle_addr_code
st_hidden st_hidden
st_addressee_id st_addressee_id
sle_addressee_id sle_addressee_id
dw_ccs dw_ccs
dw_individuals dw_individuals
cb_remove cb_remove
cb_add cb_add
cb_replace cb_replace
cb_ok cb_ok
sle_addressee sle_addressee
gb_cc gb_cc
gb_addressee gb_addressee
gb_individuals gb_individuals
end type
global w_recipients w_recipients

type variables
long                        vil_individuals_rowno
string                      vis_group_type
string                      vis_search_list_title
s_correspond_claim  vistr_correspond_claim
int		  vii_return_code
w_sheet		  viw_sheet
w_maintain	  viw_maintain
boolean		  vib_set_default_addressee


end variables

forward prototypes
public function int wf_vldte_addressee (string label)
public function long wf_add_cc (string label, long id, string type_cd)
end prototypes

public function int wf_vldte_addressee (string label);long	total_ccs, cc_cntr
string	cc

total_ccs = dw_ccs.RowCount()
FOR cc_cntr = 1 TO total_ccs
	cc = dw_ccs.GetItemString(cc_cntr,"claim_recipient_label")
	if label = cc then
		return(-1)
	end if
NEXT

return 1
end function

public function long wf_add_cc (string label, long id, string type_cd);// ************************************************************************************************
// AUTHOR:	MW George
// Modified:	94.10.06

// ************************************************************************************************
// THIS FUNCTION WILL ADD AN "INDIVIDUAL" TO THE CARBON COPY DATAWINDOW

// ************************************************************************************************
// DECLARATIONS
// Validate paramters
Trim(label)
If  ISNULL(label) or label = ' ' then goto err_fnd
Trim (type_cd)
If ISNULL(type_cd) or type_cd = ' ' then goto err_fnd
If ISNULL(id) or id < 0 then goto err_fnd

long rc

rc = dw_ccs.InsertRow(0)
if rc <> -1 then

	dw_ccs.SetItem(rc,1,label)
	dw_ccs.SetItem(rc,2,id)
	dw_ccs.SetItem(rc,3,type_cd)
	dw_ccs.Sort()

else
	MessageBox("APPLICATION ERROR: 513-ADD","THE INDIVIDUAL COULD NOT BE MADE A CARBON-COPY RECIPIENT." &
				+ "~r~n~r~nPLEASE CONTACT YOUR SYSTEM ADMINISTRATOR",StopSign!)
end if
return rc // No errors

err_fnd:
MessageBox("APPLICATION ERROR: 513-ADD","INVALID PARAMTERS FOR ADDING CC's" &
  +"~n~r ID = "+string (id)+" LABEL = "+label +" TYPE_CODE= " +type_cd &
	+ "~n~r~nPLEASE CONTACT YOUR SYSTEM ADMINISTRATOR",StopSign!)
rc = -1
Return rc
end function

event closequery;If ISVALID (viw_maintain) then
	viw_maintain.SetRedraw(False)
	viw_maintain.Visible= True
	viw_maintain.SetRedraw(True)
	viw_maintain.cb_recipients.SetFocus()
END IF
end event

event open;// ******************************************************************************************************************************
// AUTHOR:	MW George
// Modified:	94.10.20

// **************************************************************************************************
// DECLARATIONS
long		recipient_id,ll_rc
int		claimant_types,  i=1, vli_max_rcpnt
string	recipient_type_cd, individual
int		vli_width, vli_height
window lw_frame
boolean lb_set_claimant

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


this.visible = false
SetPointer(HourGlass!)

vistr_correspond_claim  = Message.PowerObjectParm
this.title = this.title+" (#"+String(vistr_correspond_claim.claim_no)+")"
vis_group_type = "CLMT"

viw_sheet = vistr_correspond_claim.sheet_window
viw_maintain = vistr_correspond_claim.parent_window
vib_set_default_addressee = vistr_correspond_claim.set_default_addressee

// **************************************************************************************************
// RETRIEVE ALL EXISTING CLAIM RECIPIENTS OF TYPE "CLAIMANT" ASSOCIATED WITH
// THE CURRENT CLAIM AND DISPLAY THE RESULTANT LIST IN THE "INDIVIDUALS"
// DATAWINDOW
//
//dw_individuals.SetRedraw(False)
dw_individuals.SetTransObject(SQLCA)
claimant_types = dw_individuals.Retrieve(vistr_correspond_claim.claim_no)
vii_return_code = SQLCA.nf_handle_error("dw_individuals","w_recipients","on open event")
//vli_height = dw_individuals.height
//vli_width = dw_individuals.width
//dw_individuals.Resize(vli_width - 200, vli_height - 200)
//dw_individuals.Resize(vli_width, vli_height)
//dw_individuals.SetRedraw(True)

Choose Case claimant_types

Case 0
	MessageBox(this.title, "No Individuals found for the claimant",INFORMATION!)
	close(this)
	Return

Case Is > 0
	// *********************************************************************************************
	// AT LEAST ONE CLAIMANT TYPE HAS BEEN ESTABLISHED SO DISPLAY THE LIST
	// IN THE DATAWINDOW
	vil_individuals_rowno = 1

Case Else
	// *********************************************************************************************
	// THE DATABASE OF CLAIM RECIPIENTS COULD NOT BE PROPERLY ACCESSED.
	MessageBox("UNRECOVERABLE DATABASE ERROR: 513-OPEN","THE LIST OF" &
				+ " CLAIM RECIPIENTS COULD NOT BE FOUND." &
				+ "~n~r~n~rPLEASE CONTACT YOUR SYSTEM ADMINISTRATOR.",StopSign!)
	close(this)
	Return

End Choose

IF vib_set_default_addressee THEN
	
	// Setup claimant as default
	claimant_types = dw_individuals.Find("UPPER(claim_recipient_label) = 'CLAIMANT'", 1, dw_individuals.RowCount())
	IF claimant_types = 0 THEN
			MessageBox(title,"Error locating claimant's address." &
			+ "~n~r~n~rPLEASE CONTACT YOUR SYSTEM ADMINISTRATOR.",StopSign!)
			Close(this)
			Return
	END IF

	sle_addressee.text		= dw_individuals.GetItemString(claimant_types, "claim_recipient_label")
	sle_addressee_id.text	= String(dw_individuals.GetItemNumber(claimant_types, "correspond_recipient_id"))
	sle_addr_code.text		= dw_individuals.GetItemString(claimant_types, "correspond_recipient_type_cd")

	this.visible = False
	cb_ok.TriggerEvent(Clicked!)	
	lb_set_claimant = true
ELSE

	// **************************************************************************************************
	// Populate recipient list (addressee and CC's)
	//
	//sle_addressee.text		= viw_maintain.gs_rcpnt_lst[i].ws_rcpnt_label
	
	sle_addressee.text		= gs_rcpnt_lst[i].ws_rcpnt_label
	sle_addressee_id.text	= String(gs_rcpnt_lst[i].ws_rcpnt_id)
	sle_addr_code.text		= gs_rcpnt_lst[i].ws_rcpnt_type_cd

	vli_max_rcpnt = UPPERBOUND(gs_rcpnt_lst)
	FOR i = 2 to vli_max_rcpnt
		recipient_id = gs_rcpnt_lst[i].ws_rcpnt_id
		recipient_type_cd = gs_rcpnt_lst[i].ws_rcpnt_type_cd
		individual =	gs_rcpnt_lst[i].ws_rcpnt_label
		ll_rc = wf_add_cc(individual, recipient_id, recipient_type_cd)
		If ll_rc > 0 then postevent("close")
	NEXT
	
	dw_individuals.SetFocus()
	
END IF

IF lb_set_claimant then
	lb_set_claimant = false
else
	// center the window over w_correspond to imitate child / parent window behaviour
	IF IsValid(viw_maintain) THEN
		lw_frame = viw_maintain.ParentWindow()  // w_frame
		THIS.x = lw_frame.x + (lw_frame.width - THIS.Width)/2
		THIS.y = lw_frame.y + (lw_frame.height - THIS.Height)/2
	END IF
	This.Visible = TRUE
end if


end event

on w_recipients.create
this.cb_cancel=create cb_cancel
this.st_1=create st_1
this.sle_addr_code=create sle_addr_code
this.st_hidden=create st_hidden
this.st_addressee_id=create st_addressee_id
this.sle_addressee_id=create sle_addressee_id
this.dw_ccs=create dw_ccs
this.dw_individuals=create dw_individuals
this.cb_remove=create cb_remove
this.cb_add=create cb_add
this.cb_replace=create cb_replace
this.cb_ok=create cb_ok
this.sle_addressee=create sle_addressee
this.gb_cc=create gb_cc
this.gb_addressee=create gb_addressee
this.gb_individuals=create gb_individuals
this.Control[]={this.cb_cancel,&
this.st_1,&
this.sle_addr_code,&
this.st_hidden,&
this.st_addressee_id,&
this.sle_addressee_id,&
this.dw_ccs,&
this.dw_individuals,&
this.cb_remove,&
this.cb_add,&
this.cb_replace,&
this.cb_ok,&
this.sle_addressee,&
this.gb_cc,&
this.gb_addressee,&
this.gb_individuals}
end on

on w_recipients.destroy
destroy(this.cb_cancel)
destroy(this.st_1)
destroy(this.sle_addr_code)
destroy(this.st_hidden)
destroy(this.st_addressee_id)
destroy(this.sle_addressee_id)
destroy(this.dw_ccs)
destroy(this.dw_individuals)
destroy(this.cb_remove)
destroy(this.cb_add)
destroy(this.cb_replace)
destroy(this.cb_ok)
destroy(this.sle_addressee)
destroy(this.gb_cc)
destroy(this.gb_addressee)
destroy(this.gb_individuals)
end on

type cb_cancel from commandbutton within w_recipients
integer x = 2002
integer y = 872
integer width = 311
integer height = 96
integer taborder = 80
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cance&l"
boolean cancel = true
end type

on clicked;Close(parent)
end on

type st_1 from statictext within w_recipients
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

type sle_addr_code from singlelineedit within w_recipients
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

type st_hidden from statictext within w_recipients
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

type st_addressee_id from statictext within w_recipients
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

type sle_addressee_id from singlelineedit within w_recipients
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

type dw_ccs from u_dw_online within w_recipients
integer x = 1481
integer y = 396
integer width = 933
integer height = 408
integer taborder = 60
string dataobject = "d_ccs"
boolean vscrollbar = true
boolean border = false
end type

event clicked;dw_ccs.SelectRow(row, True)
end event

event rowfocuschanged;IF currentrow > 0 THEN
	dw_individuals.SelectRow(0,False)
	this.SelectRow(0,False)
	this.SelectRow(currentrow,true)
END IF

end event

event dberror;SQLCA.SQLDBCode = sqldbcode
SQLCA.SQLErrText = sqlerrtext
RETURN 1
end event

type dw_individuals from u_dw_online within w_recipients
integer x = 55
integer y = 132
integer width = 969
integer height = 676
integer taborder = 10
string dataobject = "d_individuals_list"
boolean hscrollbar = true
boolean vscrollbar = true
boolean border = false
end type

event rowfocuschanged;vil_individuals_rowno = currentrow

IF vil_individuals_rowno > 0 THEN
	// Deselect previous selection made in DataWindow
	dw_ccs.SelectRow(0,False)
	this.SelectRow(0,False)
	this.SelectRow(vil_individuals_rowno,true)
END IF
end event

event scrollvertical;int vli_thisrow

vli_thisrow = Integer(dw_individuals.Describe("DataWindow.Firstrowonpage"))
dw_individuals.SetRow(vli_thisrow)
end event

event dberror;SQLCA.SQLDBCode = sqldbcode
SQLCA.SQLErrText = sqlerrtext
RETURN 1
end event

type cb_remove from commandbutton within w_recipients
integer x = 1083
integer y = 560
integer width = 311
integer height = 96
integer taborder = 40
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "< &Remove"
end type

on clicked;// ******************************************************************************************************************************
// AUTHOR:	MW George
// Modified:	94.10.20

long		delete_rc, selected_rowno

// ******************************************************************************************************************************
// ENSURE THAT THE USER SELECTED A ROW IN THE CCs DATAWINDOW

selected_rowno = dw_ccs.GetSelectedRow(0)
if selected_rowno > 0 then
	delete_rc = dw_ccs.DeleteRow(selected_rowno)
	IF dw_ccs.RowCount() = 0 THEN
		dw_individuals.TriggerEvent(RowFocusChanged!)
		dw_individuals.SetFocus()
	END IF	
else
	// *************************************************************************************************************************
	// USER TRIED TO REMOVE WITHOUT SELECTING ANYTHING

	MessageBox("REMOVE MESSAGE: 513-006","To remove a carbon-copy recipient, an item must" &
				+"~n~rbe selected from the CCs List.")
end if
end on

type cb_add from commandbutton within w_recipients
integer x = 1083
integer y = 432
integer width = 311
integer height = 96
integer taborder = 30
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Add >"
end type

on clicked;// ******************************************************************************************************************************
// AUTHOR:	MW George
// Modified:	94.10.06

// ******************************************************************************************************************************
// DECLARATIONS

boolean	found
int		i 
long		selected_rowno, total_ccs, insert_rc,recipient_id
string	cc, individual, recipient_type_cd

// ******************************************************************************************************************************
// ENSURE THAT THE TEMPLATE ALLOWS CARBON-COPIES. IF NOT, DON'T ALLOW USER TO CONTINUE
IF UPPER(Trim(vistr_correspond_claim.corr.cc_allowed_yn)) = "N" THEN
	MessageBox(parent.title,"This template type - " + vistr_correspond_claim.corr.template_type + " does not allow carbon-copies." &
					+"~n~rYou can only change the Addressee")
	Return
END IF
 
// ******************************************************************************************************************************
// ENSURE THAT THE USER SELECTED A ROW IN THE INDIVIDUALS DATAWINDOW

insert_rc = 0
selected_rowno = dw_individuals.GetSelectedRow(0)
if selected_rowno > 0 then
	
	individual = dw_individuals.GetItemString(selected_rowno,"claim_recipient_label")
	if individual = sle_addressee.text then
		// *************************************************************************************************************************
		// USER TRIED TO ADD THE SAME CC AS ADDRESSEE

		MessageBox("ADD MESSAGE: 513-027","Cannot make this individual a carbon-copy," &
					+"~n~rsince he/she is already the Addressee")
	else	
		individual	= dw_individuals.GetItemString(selected_rowno,"claim_recipient_label")
		recipient_id	= dw_individuals.GetItemNumber(selected_rowno, "correspond_recipient_id")
		recipient_type_cd = dw_individuals.GetItemString(selected_rowno,"correspond_recipient_type_cd")

		total_ccs = dw_ccs.RowCount()
		Choose Case total_ccs
		
		Case 0
			wf_add_cc(individual, recipient_id, recipient_type_cd)

		Case Is > 0
			// ********************************************************************************************************************		
			// ENSURE THAT THE INDIVIDUAL DOES NOT ALREADY EXIST IN THE CCs DATAWINDOW

			found = FALSE
			i = 1
			do while not(found) AND i <= total_ccs
				cc = dw_ccs.GetItemString(i,"claim_recipient_label")
				if cc = individual then
					found = TRUE
					MessageBox("ADD MESSAGE: 513-021","This Individual is already a carbon-copy recipient.")
				end if
				i++
			loop		

			if not(found) then
				insert_rc = wf_add_cc(individual, recipient_id, recipient_type_cd)

				IF insert_rc < 0 then
					MessageBox("APPLICATION ERROR: 513-ADD","THE GENERATOR COULD NOT DETERMINE THE NUMBER" &
						+"~n~rOF CARBON-COPY RECIPIENTS.~n~r~n~rPLEASE CONTACT YOUR SYSTEM" &
						+" ADMINISTRATOR",StopSign!)
					cb_add.enabled = FALSE
					found = TRUE     // GET OUT
					total_ccs = 999  // OF TOWN
				end if
			end if

		Case Is < 0
			// ********************************************************************************************************************
			// APPLICATION ERROR.  UNABLE TO DETERMINE HOW MANY CCs EXIST.  DISPLAY A MESSAGE AND
			// DISABLE THE ADD BUTTON.

			MessageBox("APPLICATION ERROR: 513-ADD","THE GENERATOR COULD NOT DETERMINE THE NUMBER" &
						+"~n~rOF CARBON-COPY RECIPIENTS.~n~r~n~rPLEASE CONTACT YOUR SYSTEM" &
						+" ADMINISTRATOR",StopSign!)
			cb_add.enabled = FALSE
		
		End Choose
	end if

	if insert_rc < 0 then
		MessageBox("APPLICATION ERROR: 513-ADD","THE GENERATOR COULD NOT INSERT A NEW" &
					+ "~n~rCARBON-COPY RECIPIENT.~n~r~n~rPLEASE CONTACT YOUR SYSTEM" &
					+ " ADMINISTRATOR",StopSign!)
		cb_add.enabled = FALSE
	end if
else
	// *************************************************************************************************************************
	// USER TRIED TO ADD WITHOUT SELECTING ANYTHING

	MessageBox("ADD MESSAGE: 513-001","To add a carbon-copy recipient, an item must" &
				+"~n~rbe selected from the Individuals List.")
end if
end on

type cb_replace from commandbutton within w_recipients
integer x = 1083
integer y = 104
integer width = 311
integer height = 96
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Re&place >"
end type

on clicked;// ******************************************************************************************************************************
// AUTHOR:	MW George
// Modified:	94.10.07

// ******************************************************************************************************************************
// DECLARATIONS

long		selected_rowno
string	individual
int		rc

// ******************************************************************************************************************************
// ENSURE THAT THE USER SELECTED A ROW IN THE INDIVIDUALS DATAWINDOW

selected_rowno = dw_individuals.GetSelectedRow(0)
if selected_rowno > 0 then
	// *************************************************************************************************************************
	// DETERMINE THE NAME OF THE INDIVIDUAL SELECTED AND CHECK TO SEE IF IT IS DIFFERENT FROM THE
	// CURRENT ADDRESSEE

	individual = dw_individuals.GetItemString(selected_rowno,"claim_recipient_label")
	if individual = sle_addressee.text then
		MessageBox("REPLACE MESSAGE: 513-008","The Replace action would not change the" &
					+"~n~rvalue of the Addressee.")

	else
		// ********************************************************************************************************************
		// CHECK IF ADDRESSEE ALREADY A CC

		rc = wf_vldte_addressee(individual)		
		if rc = -1  then
			MessageBox("ADD MESSAGE: 513-028","Cannot make this individual an Addressee," &
					+"~n~rsince he/she is already a carbon-copy")
		else
			// ********************************************************************************************************************
			// REPLACE THE ADDRESSEE

			sle_addressee.text		= individual
			sle_addressee_id.text	= String(dw_individuals.GetItemNumber(selected_rowno,"correspond_recipient_id"))
			sle_addr_code.text		= dw_individuals.GetItemString(selected_rowno,"correspond_recipient_type_cd")
		end if
	end if

else
	// *************************************************************************************************************************
	// USER TRIED TO REPLACE WITHOUT SELECTING ANYTHING

	MessageBox("REPLACE MESSAGE: 513-007","To replace the Addressee, an item must" &
				+"~n~rbe selected from the Individuals List.")
end if
end on

type cb_ok from commandbutton within w_recipients
integer x = 1618
integer y = 872
integer width = 311
integer height = 96
integer taborder = 70
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
boolean default = true
end type

event clicked;// ************************************************************************************************
// AUTHOR:	MW George
// Modified:	94.10.14

// ************************************************************************************************
// DECLARATIONS

string	vls_ccs = ""
long		i, vll_nbr_rows	

// Set parent window detail data
viw_maintain.st_to.text = sle_addressee.text

vll_nbr_rows = dw_ccs.RowCount()

// Setup the "To:" and "CCs:" in w_maintain
FOR i = 1 TO vll_nbr_rows
	vls_ccs = vls_ccs + dw_ccs.GetItemString(i, "claim_recipient_label")
	IF i <  dw_ccs.RowCount() THEN
		vls_ccs = vls_ccs + ','
	END IF
NEXT
viw_maintain.mle_ccs.text = vls_ccs

// Clear recipient list
gs_rcpnt_lst[] = gs_empty_lst[]

// Set parent window's recipient list structure - addressee
gs_rcpnt_lst[1].ws_rcpnt_id = Long(sle_addressee_id.text)
gs_rcpnt_lst[1].ws_rcpnt_type_cd = sle_addr_code.text
gs_rcpnt_lst[1].ws_rcpnt_label = sle_addressee.text
gs_rcpnt_lst[1].ws_addressee_role = "A"

// Set parent window's recipient list structure - CC's
FOR i = 1 TO vll_nbr_rows
	gs_rcpnt_lst[i + 1].ws_rcpnt_id = dw_ccs.GetItemNumber(i, "correspond_recipient_id")
	gs_rcpnt_lst[i + 1].ws_rcpnt_type_cd = dw_ccs.GetItemString(i, "correspond_recipient_type_cd")
	gs_rcpnt_lst[i + 1].ws_rcpnt_label = dw_ccs.GetItemString(i, "claim_recipient_label")
	gs_rcpnt_lst[i + 1].ws_addressee_role = "C"
NEXT

Close(parent)
end event

type sle_addressee from singlelineedit within w_recipients
integer x = 1481
integer y = 112
integer width = 933
integer height = 80
integer taborder = 50
integer textsize = -8
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean border = false
boolean autohscroll = false
boolean displayonly = true
end type

type gb_cc from groupbox within w_recipients
integer x = 1445
integer y = 316
integer width = 1006
integer height = 520
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "&CCs"
end type

type gb_addressee from groupbox within w_recipients
integer x = 1445
integer y = 48
integer width = 1006
integer height = 176
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Addressee"
end type

type gb_individuals from groupbox within w_recipients
integer x = 37
integer y = 48
integer width = 1006
integer height = 788
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "&Individuals"
end type

