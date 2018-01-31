$PBExportHeader$w_define.srw
$PBExportComments$Maitains a custom recipient's name and address (i.e. create, edit, and display)
forward
global type w_define from window
end type
type cb_ok from commandbutton within w_define
end type
type cb_search from commandbutton within w_define
end type
type cb_close from commandbutton within w_define
end type
type cb_cancel from commandbutton within w_define
end type
type dw_recipient_address from u_dw_online within w_define
end type
end forward

global type w_define from window
integer y = 680
integer width = 2656
integer height = 1308
boolean titlebar = true
string title = "Define"
boolean controlmenu = true
windowtype windowtype = child!
long backcolor = 67108864
event populate_addr_info pbm_custom02
event ue_filesave pbm_custom01
cb_ok cb_ok
cb_search cb_search
cb_close cb_close
cb_cancel cb_cancel
dw_recipient_address dw_recipient_address
end type
global w_define w_define

type variables
s_correspond_claim  vistr_correspond_claim
string 		  group
int		  vii_return_code

string		  vis_action, vis_label
boolean		  vib_data_ok, vib_label_changed
boolean		  vib_show_parent
DataWindowChild	  vidwc_city
DataWindowChild	  vidwc_province
DataWindowChild	  vidwc_country
Private:
w_maintain_recipients viw_maintain_recipients
end variables

forward prototypes
public function integer wf_modified ()
public function boolean wf_update ()
public subroutine wf_disable_screen (string p_color, boolean p_protect)
public function string wf_check_item ()
end prototypes

event populate_addr_info;// Retrieve the service provider or employer address and populate fields on screen
Long    ll_nbr_rows, ll_accident_employer_operation_no
String  ls_other, ls_contact_name
String  ls_addr1, ls_addr2, ls_addr3, ls_city, ls_prov, ls_pc
String  ls_country, ls_fax_no, ls_email, ls_employer_type_code

CHOOSE CASE vistr_correspond_claim.ext_addr.address_location_code
	CASE "C"
		ll_nbr_rows = dw_recipient_address.Retrieve(vistr_correspond_claim.rcpnt.claim_rcpnt_id)
		vii_return_code = SQLCA.nf_handle_error("dw_recipient_address","w_define","on populate_addr_info event")
		IF ll_nbr_rows = 0 THEN
			MessageBox(title,"COULD NOT FIND THE ADDRESS INFORMATION" &
							+"~n~rASSOCIATED WITH THIS INDIVIDUAL.~n~r~n~rPLEASE CONTACT YOUR SYSTEM" &
							+" ADMINISTRATOR.",StopSign!)
			vib_show_parent = True
			Close(this)
		END IF
		Return
	CASE "P"
		SELECT		name, address_line1, address_line2,  city, prov_state_code,
					postal_code, country_code, fax_no, contact_name
		INTO		:ls_other, :ls_addr1, :ls_addr2, 
					:ls_city, :ls_prov, :ls_pc, :ls_country, :ls_fax_no, :ls_contact_name
		FROM		PROVIDER
		WHERE		provider_no = :vistr_correspond_claim.ext_addr.address_id AND
					provider_type_code	= :vistr_correspond_claim.ext_addr.address_subtype_code;

		vii_return_code = SQLCA.nf_handle_error(" ","w_define","on populate_addr_info event")
		IF vii_return_code = 100 THEN
			MessageBox("DATABASE INTEGRITY ERROR: 513-DEFINE","COULD NOT FIND THE ADDRESS INFORMATION" &
							+"~n~rASSOCIATED WITH THIS INDIVIDUAL.~n~r~n~rPLEASE CONTACT YOUR SYSTEM" &
							+" ADMINISTRATOR.",StopSign!)
			vib_show_parent = True
			Close(this)
			Return
		END IF
	CASE "E"
		SELECT C.accident_employer_operation_no, E.employer_type_code  
		  INTO :ll_accident_employer_operation_no, :ls_employer_type_code 
		  FROM CLAIM C, EMPLOYER E   
		 WHERE C.claim_no = :vistr_correspond_claim.claim_no 
			AND C.accident_employer_no = E.employer_no ;

		vii_return_code = SQLCA.nf_handle_error(" ","w_maintain","on wf_determine_field_values")
		
		IF ls_employer_type_code = "S" THEN
			SELECT E.employer_legal_name, O.operation_name, EA.address_line1, EA.address_line2, EA.address_line3,   
					 EA.city, EA.prov_state_code, EA.postal_code, EA.country_code, EA.fax_no 
			  INTO :ls_other, :ls_contact_name, :ls_addr1, :ls_addr2, :ls_addr3, 
			       :ls_city, :ls_prov, :ls_pc, :ls_country, :ls_fax_no 
			  FROM EMPLOYER_ADDRESS EA, EMPLOYER E, OPERATION O  
			 WHERE EA.employer_no = :vistr_correspond_claim.ext_addr.address_id
				AND EA.address_type_code = "BA" 
				AND O.operation_no = :ll_accident_employer_operation_no 
				AND EA.employer_no = E.employer_no 
				AND EA.employer_no = O.employer_no 
			 USING SQLCA ; 
		ELSE
			SELECT E.employer_legal_name, EA.address_line1, EA.address_line2, EA.address_line3, EA.city,   
					 EA.prov_state_code, EA.postal_code, EA.country_code, EA.fax_no, ""
	//				 EA.contact_name    edythe - revised 99/09/22 (PR 817 fix)
			  INTO :ls_other, :ls_addr1, :ls_addr2, :ls_addr3, :ls_city, 
					 :ls_prov, :ls_pc, :ls_country, :ls_fax_no, :ls_contact_name
			  FROM EMPLOYER_ADDRESS EA, EMPLOYER E  
			 WHERE EA.employer_no = E.employer_no 
				AND EA.employer_no = :vistr_correspond_claim.ext_addr.address_id 
				AND EA.address_type_code = "BA" ;
		END IF

		vii_return_code = SQLCA.nf_handle_error(" ","w_define","on populate_addr_info event")
		IF vii_return_code = 100 THEN
			MessageBox("DATABASE INTEGRITY ERROR: 513-DEFINE","COULD NOT FIND THE ADDRESS INFORMATION" &
							+"~n~rASSOCIATED WITH THIS INDIVIDUAL.~n~r~n~rPLEASE CONTACT YOUR SYSTEM" &
							+" ADMINISTRATOR.",StopSign!)
			vib_show_parent = True
			Close(this)
			Return
		END IF
END CHOOSE	

LeftTrim(ls_other)
leftTrim(ls_contact_name)
LeftTrim(ls_addr1)
LeftTrim(ls_addr2)
LeftTrim(ls_addr3)
LeftTrim(ls_city)
LeftTrim(ls_prov)

// Move address data to datawindow for display
dw_recipient_address.Insertrow(0)
dw_recipient_address.SetItem(1, "name1", ls_other)
dw_recipient_address.SetItem(1, "name2", ls_contact_name)
dw_recipient_address.SetItem(1, "address_line1", ls_addr1)
dw_recipient_address.SetItem(1, "address_line2", ls_addr2)
dw_recipient_address.SetItem(1, "address_line3", ls_addr3)
dw_recipient_address.SetItem(1, "city", ls_city)
dw_recipient_address.SetItem(1, "province", ls_prov)
dw_recipient_address.SetItem(1, "postal_code", ls_pc)
dw_recipient_address.SetItem(1, "country", ls_country)
dw_recipient_address.SetItem(1, "fax_no", ls_fax_no)
dw_recipient_address.SetItem(1, "email_id", ls_email)

vis_label = ls_other
end event

event ue_filesave;// ******************************************************************************************************************************
// AUTHOR:	MW George
// Modified:	94.09.15

// ***********************************************************************************
///
// This function will only be triggered when creating or modifying user defined
// addresses. (does not include card file entries)
// Apr. 8, 1995 a checkbox was in the old version, June '95 to set the default

long				rc, dbrc, total_indv, vli_row, vll_rcpnt_id
dwItemStatus 	l_status
string			vls_return_value, vls_name,ls_country,ls_pc,ls_string
singlelineedit vis_field_error
INTEGER        li_trancount

	vib_data_ok = FALSE


	IF dw_recipient_address.uf_accept_dw() = -1 THEN RETURN
	IF dw_recipient_address.uf_datachanged() < 1 THEN
		MessageBox( "SAVE ERROR", "There are no changes to save," &
					+"~n~r Try modifing some data before using" &
					+"~n~r the Save button or Cancel to quit!",INFORMATION!)
		Return
	END IF		


	IF vis_action = "NEW" THEN
			
	/* INSERT REOCRD - Determine and Validate label entered
	*/
		vls_name = Trim(dw_recipient_address.GetItemString(1, "name1")) 
		vis_label = viw_maintain_recipients.wf_vldte_label(vls_name)
		
		IF UPPER(vis_label) = "NONE" THEN 
			MessageBox("SAVE ERROR", "The label chosen for this entry is not in the correct format.")
			dw_recipient_address.Setfocus()
			RETURN
		END IF

		vll_rcpnt_id = f_get_next_rcpnt_id()

		IF vll_rcpnt_id < 0 THEN Return 
		vistr_correspond_claim.rcpnt.claim_rcpnt_id = vll_rcpnt_id

		// Make an entry in both the recipient list and recipient address
		vli_row = viw_maintain_recipients.dw_individuals.InsertRow(0) //Changed from a direct reference

		// Card file flag value wil be determined by the search. A user cannot enter
		// a card file from this module
		viw_maintain_recipients.dw_individuals.SetItem(vli_row,"default_address_flag","N")
		viw_maintain_recipients.dw_individuals.SetItem(vli_row,"card_file_flag","N")
		viw_maintain_recipients.dw_individuals.SetItem(vli_row,"claim_no",vistr_correspond_claim.claim_no)
		viw_maintain_recipients.dw_individuals.SetItem(vli_row,"correspond_recipient_type_cd",vistr_correspond_claim.rcpnt.rcpnt_type_code)
		viw_maintain_recipients.dw_individuals.SetItem(vli_row,"correspond_recipient_id",vistr_correspond_claim.rcpnt.claim_rcpnt_id)
		viw_maintain_recipients.dw_individuals.SetItem(vli_row,"claim_recipient_label",vis_label)
		viw_maintain_recipients.dw_individuals.SetItem(vli_row,"address_location_code", vistr_correspond_claim.ext_addr.address_location_code)
		viw_maintain_recipients.dw_individuals.SetItem(vli_row,"recipient_no",vistr_correspond_claim.ext_addr.address_id)
		viw_maintain_recipients.dw_individuals.SetItem(vli_row,"correspond_recipient_subtyp_cd",vistr_correspond_claim.ext_addr.address_subtype_code)
		
		/* Add code to check the type of SQL about to be sent. Since 1995, records have been
		   updated instead of inserted.
		*/	
		l_status = viw_maintain_recipients.dw_individuals.GetItemStatus(vli_row,0, Primary!)
		Choose Case l_status
			Case NotModified!
				ls_string = "NotModified"
			Case DataModified!
				ls_string ="DataModified"
			Case New!
				ls_string ="New"
			Case NewModified!
				ls_string = "NewModified"
			Case Else
				ls_string = "Unknown"
		End Choose
		
		IF l_status <> NewModified! then
			MessageBox("SAVE ERROR", "An error occurred while attempting to add this individual"&
			+"~n~r to your recipient list. The status indicates " +ls_string + " instead of NewModified"&
			+"~n~rPlease report this error to the helpdesk!",Stopsign!)
			Return
		End if
		
		SQLCA.nf_begin_transaction()
		
		If	NOT viw_maintain_recipients.wf_update() THEN
			vii_return_code = viw_maintain_recipients.vii_return_code 
			Return
		END IF

		dw_recipient_address.SetItem(1,"correspond_recipient_id",vistr_correspond_claim.rcpnt.claim_rcpnt_id)
		dw_recipient_address.SetItem(1,"correspond_recipient_type_cd",vistr_correspond_claim.rcpnt.rcpnt_type_code)
		dw_recipient_address.SetItem(1,"correspond_recipient_subtyp_cd","")   // No subtype code for user defined addresses
		dw_recipient_address.SetItem(1,"card_file_flag","N")
		dw_recipient_address.SetItem(1,"active_flag","Y")
		
		/* Add code to check the type of SQL about to be sent. Since 1995, records have been
		   updated instead of inserted.
		*/	
		l_status = dw_recipient_address.GetItemStatus(1,0, Primary!)
		Choose Case l_status
			Case NotModified!
				ls_string = "NotModified"
			Case DataModified!
				ls_string ="DataModified"
			Case New!
				ls_string ="New"
			Case NewModified!
				ls_string = "NewModified"
			Case Else
				ls_string = "Unknown"
		End Choose
		
		IF l_status <> NewModified! then
			// signal error if status is not NewModified
			Error.Text        = SQLCA.sqlerrtext
			IF Error.Text = '' THEN
				Error.Text        = 'An error occurred while attempting to add the address for this individual.' + &
			                       '~r~nThe status indicates ' +ls_string + ' instead of NewModified.' + &
										  '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.' 
			ELSE
				Error.Text        = Error.Text + '.~r~nAn error occurred while attempting to add the address for this individual.' + &
			                       '~r~nThe status indicates ' +ls_string + ' instead of NewModified.' + &
										  '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.' 
			END IF
			Error.Object      = 'w_define'
			Error.is_database = 'CLAIM'
			Error.il_dbcode   = SQLCA.SQLDBCode
			Error.ObjectEvent = 'ue_filesave'
			SignalError()			
		End if
				
		IF NOT wf_update() THEN
			SQLCA.nf_rollback_transaction()
			Return
		END IF
		
		SQLCA.nf_commit_transaction()
		IF SQLCA.nf_handle_error('w_define', 'EXECUTE IMMEDIATE COMMIT TXN USING SQLCA', 'ue_filesave (1)') <> 0 THEN
			Return
		ELSE
			viw_maintain_recipients.wf_redraw_individuals_dw()
		END IF
	
	ELSE
	
		/* ACTION + EDIT
		Should be only UPDATING HERE
		*/
		rc = viw_maintain_recipients.dw_individuals.GetSelectedRow(0)

		
		IF vib_label_changed THEN
			vls_name = Trim(dw_recipient_address.GetItemString(1, "name1")) 
			vis_label = viw_maintain_recipients.wf_vldte_label(vls_name)
		
			IF UPPER(vis_label) = "NONE" THEN Return
			viw_maintain_recipients.dw_individuals.SetItem(rc,"claim_recipient_label",vis_label)
			/* Add code to check the type of SQL about to be sent. Since 1995, records have been
				  incorrectly updated. 
			*/	
			l_status = viw_maintain_recipients.dw_individuals.GetItemStatus(rc,0, Primary!)
			
			Choose Case l_status
				Case NotModified!
					ls_string = "NotModified"
				Case DataModified!
					ls_string ="DataModified"
				Case New!
					ls_string ="New"
				Case NewModified!
					ls_string = "NewModified"
				Case Else
					ls_string = "Unknown"
			End Choose
		
			IF l_status <> DataModified! then
				// signal error if status is not DataModified
				Error.Text        = SQLCA.sqlerrtext
				IF Error.Text = '' THEN
					Error.Text        = 'An error occurred while attempting to edit/update your recipient list.' + &
											  'The status indicates ' +ls_string + ' instead of DataModified.' + &
											  '.~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.' 
				ELSE
					Error.Text        = Error.Text + '.~r~nAn error occurred while attempting to edit/update your recipient list.' + &
											  'The status indicates ' +ls_string + ' instead of DataModified.' + &
											  '.~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.' 
				END IF
				Error.Object      = 'w_define'
				Error.is_database = 'CLAIM'
				Error.il_dbcode   = SQLCA.SQLDBCode
				Error.ObjectEvent = 'ue_filesave'
				SignalError()
			End if
			
			SQLCA.nf_begin_transaction()
			
			IF NOT viw_maintain_recipients.wf_update() THEN
				vii_return_code = viw_maintain_recipients.vii_return_code 
				Return
			END IF	
		END IF
		
		l_status = dw_recipient_address.GetItemStatus(1,0, Primary!) 
		Choose Case l_status
			Case NotModified!
				ls_string = "NotModified"
			Case DataModified!
				ls_string ="DataModified"
			Case New!
				ls_string ="New"
			Case NewModified!
				ls_string = "NewModified"
			Case Else
				ls_string = "Unknown"
		End Choose
		
		IF l_status <> DataModified! then
			// signal error if status is not DataModified
			Error.Text        = SQLCA.sqlerrtext
			IF Error.Text = '' THEN
				Error.Text        = 'An error occurred while attempting to edit/update the address for this individual.' + &
			                       '~r~nThe status indicates ' +ls_string + ' instead of DataModified.' + &
										  '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.' 
			ELSE
				Error.Text        = Error.Text + '.~r~nAn error occurred while attempting to edit/update the address for this individual.' + &
			                       '~r~nThe status indicates ' +ls_string + ' instead of DataModified.' + &
										  '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.' 
			END IF
			Error.Object      = 'w_define'
			Error.is_database = 'CLAIM'
			Error.il_dbcode   = SQLCA.SQLDBCode
			Error.ObjectEvent = 'ue_filesave'
			SignalError()
		End if
		
	// ******************************************************************************************************************************
	// UPDATE THE PROPER ADDRESS RECORD IF NECESSARY


		// if the txn count = 0, then begin txn here
		SQLCA.nf_transaction_count(li_trancount,1,'','','',FALSE)
		IF li_trancount = 0 THEN
			SQLCA.nf_begin_transaction()
		END IF		
		
		IF NOT wf_update() THEN
			SQLCA.nf_rollback_transaction()
			Return
		END IF
		
		vii_return_code = SQLCA.nf_commit_transaction()
		
		IF vii_return_code <> 0 THEN Return
		viw_maintain_recipients.wf_redraw_individuals_dw()

		vii_return_code = viw_maintain_recipients.wf_refresh_indiv_lst()
		viw_maintain_recipients.dw_individuals.Sort()
	END IF

	vib_data_ok = True

end event

public function integer wf_modified ();// If the Datawindow fails the accepttext() function,
//	return 0
// If  rows have been modified or deleted, return 0
// If everything ok, return 1

//dw_recipient_address.ib_supress_pbmessage = true
dw_recipient_address.uf_Accept_dw()

IF dw_recipient_address.ModifiedCount() + &
	dw_recipient_address.DeletedCount() > 0 THEN Return 0

Return 1 


end function

public function boolean wf_update ();// process update, return false if update failed

dw_recipient_address.Update()
vii_return_code = SQLCA.nf_handle_error(" ","w_define","on wf_update")
IF vii_return_code <> 0 THEN
	Return False
END IF

Return True


		
end function

public subroutine wf_disable_screen (string p_color, boolean p_protect);//******************************************************************************
///
// This routine will change the background color of every
// column, as well as protect them.
//
//Parms: string	p_color		Bacground color
//			boolean	p_protect	True to protect column
//										or False no to protect column
///

int		vli_colcount, vli_col_cntr, vli_protect
string	vls_mod_string

IF p_protect THEN
	vli_protect = 1
ELSE
	vli_protect = 0
END IF

vli_colcount = Integer(dw_recipient_address.Describe("Datawindow.Column.Count"))

FOR vli_col_cntr = 1 TO vli_colcount
	vls_mod_string = "#" + String(vli_col_cntr) + ".Background.Color=" + p_color +&
	" #" + String(vli_col_cntr) + ".Protect=" + String(vli_protect)
	dw_recipient_address.Modify(vls_mod_string)
NEXT

end subroutine

public function string wf_check_item ();dwitemstatus  l_status
integer i,ll_row_num,il_colcount
string ls_colvarible,ls_colname,ls_colvalue
// Get rowcount and value 

il_colcount = integer(dw_recipient_address.Describe("datawindow.column.count"))

/* Set up all the initial values for string items that can be updated in the datawindow
	Find the column that's been flagged as not being modified and determine that it's not null or blank
*/
For i = 1 to il_colcount
	ls_colvarible ='#'+string(i)+'.name'
	ls_colname =dw_recipient_address.Describe(ls_colvarible)
	
	CHOOSE Case ls_colname
			case  "city","province","country","name1","address_line1","recipient_type_cd","postal_code"
			
					l_status = dw_recipient_address.GETITEMSTATUS(1,ls_colname,Primary!)

/* END OF DEBUG  */
					IF l_status = NotModified!  then
						ls_colvalue = dw_recipient_address.GetItemString(1,ls_colname)
						If ISNULL(ls_colvalue) or ls_colvalue = ' ' then
							Return(ls_colname)
						END IF
					END IF
		case Else

	 END CHOOSE
next
SETNULL(ls_colname)
Return(ls_colname)

end function

event closequery;IF vib_data_ok THEN

	CHOOSE CASE wf_modified()
		CASE 0
			CHOOSE CASE MessageBox(title, &
					"Do you want to save changes!", &
					Question!, YesNoCancel!)
			CASE 1		// Save the data
				This.TriggerEvent( "ue_filesave" )
				IF NOT vib_data_ok THEN Message.ReturnValue = 1
				Return
			CASE 2		// Close without saving (do nothing)
				// May have to set the focus on the previous window, else the user has to click in it
				IF isValid (viw_maintain_recipients) then viw_maintain_recipients.Setfocus()
			CASE 3		// Cancel and return to window
				Message.ReturnValue = 1
				Return
		END CHOOSE
		// No changes made, nothing to do
	END CHOOSE
END IF
//******************************************************
// Don't want to show parent "w_maintain_recipients"
// when search is triggered
IF vib_show_parent THEN
	IF ISVALID(viw_maintain_recipients) THEN	
		viw_maintain_recipients.SetRedraw(False)
		viw_maintain_recipients.Visible = True
		viw_maintain_recipients.Setfocus()
		viw_maintain_recipients.SetRedraw(True)
	END IF
END IF	
end event

event open;// ******************************************************************************************************************************
// AUTHOR:	MW George
// Modified:	94.10.05

// ******************************************************************************************************************************
// DECLARATIONS
unsignedinteger win_handle
string	vls_color
boolean	vlb_protect

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


// Get the Data Window Child for Municipality
dw_recipient_address.GetChild("city",vidwc_city)

// Get the Data Window Child for Province/State
dw_recipient_address.GetChild("province",vidwc_province)

// Get the Data Window Child for Country
dw_recipient_address.GetChild("country",vidwc_country)

vistr_correspond_claim = Message.PowerObjectParm

	
vis_action = UPPER(vistr_correspond_claim.rcpnt.action)
win_handle = Handle(viw_maintain_recipients) // Check to see if viw_maintain is already in use
If win_handle > 0 then
	MessageBox(title,"A system error has occurred ~n~rwhile attempting to assoicate this window with"&
							+"~n~ra window to maintain recipients.~n~r~n~rPLEASE CONTACT YOUR SYSTEM" &
							+" ADMINISTRATOR.",StopSign!)
	Return
END IF

viw_maintain_recipients = vistr_correspond_claim.parent_window

vis_label = vistr_correspond_claim.rcpnt.claim_rcpnt_label

dw_recipient_address.SetTransObject(SQLCA)

IF vis_action <> "NEW" then
	this.title = "Definition of "+viw_maintain_recipients.vis_group_desc+" -  "+vistr_correspond_claim.rcpnt.claim_rcpnt_label
	dw_recipient_address.uf_set_pbmessage(True)
	cb_search.visible = False

	If vis_action = "EDIT" then
		dw_recipient_address.SetFocus()

	elseif (vis_action = "VIEW") then
		cb_ok.visible = False
		cb_cancel.visible = False
		cb_ok.default = False
		cb_close.visible = True
		cb_close.default = True

		/* disallow modification of service providers 
			and employers
		*/	
		vls_color = "67108864"
		vlb_protect = True
		wf_disable_screen(vls_color, vlb_protect)

		cb_close.SetFocus()
	end if

	// RETRIEVE THE ADDRESS INFO FOR THE INDIVIDUAL
	this.TriggerEvent("populate_addr_info")
ELSE
	this.title = "Definition for "+viw_maintain_recipients.vis_group_desc
	dw_recipient_address.InsertRow(0)
END IF


end event

on w_define.create
this.cb_ok=create cb_ok
this.cb_search=create cb_search
this.cb_close=create cb_close
this.cb_cancel=create cb_cancel
this.dw_recipient_address=create dw_recipient_address
this.Control[]={this.cb_ok,&
this.cb_search,&
this.cb_close,&
this.cb_cancel,&
this.dw_recipient_address}
end on

on w_define.destroy
destroy(this.cb_ok)
destroy(this.cb_search)
destroy(this.cb_close)
destroy(this.cb_cancel)
destroy(this.dw_recipient_address)
end on

type cb_ok from commandbutton within w_define
integer x = 2245
integer y = 956
integer width = 311
integer height = 96
integer taborder = 30
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Save"
boolean default = true
end type

event clicked;string ls_name,ls_column_text_name,ls_country,ls_province,ls_city,vls_find_expression
long	vll_rownum
/*  ANY CHANGE to name1 or recipient type must be treated as a new record not an update
*/

IF dw_recipient_address.accepttext() = -1 Then  // only if everthing is validated else use validation error messages
	dw_recipient_address.SETFOCUS()
	Return
END IF

IF dw_recipient_address.uf_datachanged() < 1 THEN
	MessageBox(parent.title,"You haven't changed anything, please re-enter.")
	dw_recipient_address.SETFOCUS()
	Return
END IF

ls_name = wf_check_item()

If  NOT ISNULL(ls_name) THEN
	ls_column_text_name = ls_name + "_t.Text"
	ls_column_text_name = dw_recipient_address.Describe(ls_column_text_name)
	
	If ls_column_text_name = "!" or ls_column_text_name = "" Then 
		ls_column_text_name = ls_name
	Else
		If right(ls_column_text_name,1) = ":" Then ls_column_text_name = left(ls_column_text_name,len(ls_column_text_name)-1)
	End IF

	vib_data_ok= False
	MESSAGEBOX(parent.title,"Missing a value for " +ls_column_text_name+".  Please enter!",Exclamation!)
	dw_recipient_address.Setfocus()
	Return
End If

/* ADD LOGIC TO VALIDATE CITY IF PROVINCE AND COUNTRY IS IN NEW BRUNSWICK  */


/* vii_return code is set from two different window functions called wf_update(parent) and wf_update(this)BAD BAD */
ls_country 	= 	dw_recipient_address.GetItemString(1,'country')
ls_province =	dw_recipient_address.GetItemString(1,'province')
ls_city		=	dw_recipient_address.GetItemString(1,'city')
If ISNULL(ls_country) or ls_country = "" then ls_country = ' '
If ISNULL(ls_province) or ls_province = "" then ls_province = ' '
If ISNULL(ls_city) or ls_city = "" then  ls_city = ' ' 
	

	
/* If Canada must be a valid country    */
		/* If New Brunswick must be a valid city */
	IF  ls_province = "NB" and ls_country = "CAN" then 
		vls_find_expression = "location_desc1 = '" + ls_city + "' AND prov_state_code = 'NB' AND country_code = 'CAN'" 
		vll_rownum 		=	vidwc_city.Find(vls_find_expression,1,vidwc_city.Rowcount())	
		IF vll_rownum = 0 THEN
			MessageBox("Invalid City", "The city entered is not currently valid in NB.  Please correct.", Exclamation!)
			vib_data_ok = False
			dw_recipient_address.setcolumn("city")
			ReTurn
		END IF

	ELSEIF	ls_country = "CAN" then			
		vls_find_expression	= 	"location_code = '" + ls_province + "' AND country_code = 'CAN'"
		vll_rownum 		=	vidwc_province.Find(vls_find_expression,1,vidwc_province.Rowcount())	
		IF vll_rownum = 0 THEN
			dw_recipient_address.uf_set_pbmessage ( True )
			vib_data_ok = False
			MessageBox("Invalid Province", "The province entered is not currently valid in Canada.  Please correct.", Exclamation!)
			dw_recipient_address.setcolumn("province")
			Return			
		END IF

//* IF US must be a valid state */		

	ELSEIF	ls_country = 'USA' THEN
			vls_find_expression = "location_code = '" + ls_province + "' AND country_code = 'USA'" 
			vll_rownum = vidwc_province.Find(vls_find_expression, 1, vidwc_province.RowCount())      	
			IF vll_rownum = 0 THEN
				vib_data_ok = False
				MessageBox("Invalid State", "The state entered is not currently valid in the USA.  Please correct.", Exclamation!)
				Return
			END IF

	ELSEIF ls_province <> ' ' then
		vib_data_ok = False
		MessageBox("Invalid State/Province", "State/Province must be blank for all contries, except USA or Canada.", Exclamation!)
		dw_recipient_address.setcolumn("province")
		Return	 
	
END IF
parent.TriggerEvent( "ue_filesave" )
IF NOT vib_data_ok THEN
			
	CHoose Case vii_return_code
		Case -3	
			MessageBox("DATABASE ERROR: 532-SAVE","ERROR SAVING RECIPIENT." &
			+"~n~r~n~rA DEADLOCK HAS OCCURRED. PLEASE RETRY LATER ON!",StopSign!)
		Case -2
			MessageBox("DATABASE ERROR: 532-SAVE","ERROR SAVING RECIPIENT." &
			+"~n~r~n~rTHIS RECORD WAS UPDATED MOMENTS BEFORE YOU. PLEASE RETRIEVE THE RECORD AND TRY AGAIN!",Information!)
		Case Else
			MessageBox("DATABASE ERROR: 532-SAVE","ERROR SAVING RECIPIENT." &
			+"~n~r~n~rUNABLE TO SAVE RECIPIENT. PLEASE RETRY LATER ON!",StopSign!)
	END CHOOSE
	
		
End if

vib_show_parent = True
Close(parent)
end event

type cb_search from commandbutton within w_define
integer x = 2249
integer y = 816
integer width = 311
integer height = 96
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "S&earch..."
end type

on clicked;// ************************************************************************************************
// AUTHOR:	MW George
// Modified:	94.10.04

w_sheet		vlw_sheet
vlw_sheet = vistr_correspond_claim.sheet_window

OpenWithParm(w_search, vistr_correspond_claim, vlw_sheet)

dw_recipient_address.Reset()  // To avoid closequery
vib_show_parent = False
Close(parent)
end on

type cb_close from commandbutton within w_define
boolean visible = false
integer x = 2245
integer y = 1068
integer width = 311
integer height = 96
integer taborder = 50
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Close"
end type

on clicked;// Do reset to avoid closequery prompting you to save, because
// we insert and setitems for service providers and employers
// in populate_address_info event of window
dw_recipient_address.Reset()

vib_show_parent = True
close(parent)
end on

type cb_cancel from commandbutton within w_define
integer x = 2245
integer y = 1068
integer width = 311
integer height = 96
integer taborder = 40
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cance&l"
boolean cancel = true
end type

on clicked;// ************************************************************************************************
// AUTHOR:	MW George
// Modified:	94.09.30
if dw_recipient_address.uf_datachanged() = 0 then dw_recipient_address.uf_set_pbmessage(False)
vib_show_parent = True
close(parent)
end on

type dw_recipient_address from u_dw_online within w_define
integer x = 55
integer y = 12
integer width = 2592
integer height = 1172
integer taborder = 10
string dataobject = "d_recipient_address"
boolean border = false
end type

event itemchanged;call super::itemchanged;int		vli_rc, vli_nbr_rows
long		vll_rownum
string	vls_gettext, vls_into_string,ls_city,ls_province,ls_country,ls_pc
string	vls_mod_str, vls_country, vls_find_expression 

vll_rownum					=	GetRow()
vls_gettext					=	GetText()
vls_gettext					= 	LeftTrim(vls_gettext)

this.uf_set_pbmessage ( True )
//// Check if any double quotes exists in the string
IF Match(vls_gettext, "~"+") THEN	
	MessageBox("Validation Error","Text cannot contain double quotes, please re-enter.")
	vib_data_ok = False
	uf_set_pbmessage(True)
	return 1
END IF

// Get any values already set
ls_city = this.GetItemString(1, "city")			
ls_province =this.GetItemString(1,"province")
ls_country = this.GetItemString(1,"country")
ls_pc			= this.GetItemString(1,"postal_code")

IF (IsNull(ls_city) or ls_city = "" or   TRIM(ls_city) = ' ') = TRUE THEN 		ls_city = ' '
IF (IsNull(ls_province) or  ls_province ="" or TRIM(ls_province) = ' ') = TRUE THEN	ls_province = ' '
IF (IsNull(ls_country) or ls_country=""  or Trim(ls_country) = ' ') = TRUE THEN		ls_country = ' '

CHOOSE Case GetColumnName()

Case	'postal_code'
	ls_pc			= vls_gettext

	IF ls_country = "CAN" THEN
   	IF NOT (IsNulL(ls_pc) OR Trim(ls_pc) = '') THEN
      	IF Len(ls_pc) <> 7 OR NOT (IsNumber(Mid(ls_pc,2,1)) AND IsNumber(Mid(ls_pc,5,1)) AND IsNumber(Mid(ls_pc,7,1)))&
         	OR IsNumber(Mid(ls_pc,1,1)) OR IsNumber(Mid(ls_pc,3,1)) OR IsNumber(Mid(ls_pc,6,1)) OR NOT (Mid(ls_pc,4,1) = ' ') THEN
      	// invalid postal code
         	MessageBox("Invalid Postal Code", "The postal code entered is not in the correct format.")
				this.uf_set_pbmessage ( True )
				vib_data_ok = False
				RETURN 1
			END IF
   	END IF
	END IF

/* City should be entered since its used for a search criteria */
Case 'city'
	ls_city = vls_gettext
	IF(ls_city = ' ' or ISNULL(ls_city) or ls_city="") = TRUE then
			this.uf_set_pbmessage (True)	
			vib_data_ok = False			
  			MessageBox("Invalid City", "Missing value for City.  Please correct.", Exclamation!)
			RETURN 1	
	END IF
	vll_rownum = vidwc_city.Find("location_desc1 ='" + ls_city+"'",1,vidwc_city.Rowcount())
	
	IF vll_rownum > 0 THEN
			/* Set the Province and City  */
			this.SetItem(1,"province",vidwc_city.GetItemString(vll_rownum,'prov_state_code'))
			ls_province = vidwc_city.GetItemString(vll_rownum,'prov_state_code') 
   		this.SetItem(1,"country",vidwc_city.GetItemString(vll_rownum,'country_code'))	
			ls_country = vidwc_city.GetItemString(vll_rownum,'country_code') 
	END IF
	
Case 'province'
	ls_province = vls_gettext

	If NOT(ls_province = " " or ISNULL(ls_province)) then
		vll_rownum = vidwc_province.Find("location_code = '" + ls_province+"'",1,vidwc_province.Rowcount())
		IF vll_rownum > 0 THEN
			/* Set the country code  */
				This.SetItem(1,"country",vidwc_province.GetItemString(vll_rownum,"country_code"))
				ls_country = vidwc_province.GetItemString(vll_rownum,"country_code")
		END IF
	END IF

Case 'country'
 /* validate country outside North America there should be no province or state */
    	ls_country = vls_gettext
		vll_rownum	=		vidwc_country.Find("location_code = '" + ls_country + "'" ,1,vidwc_country.Rowcount())
		IF vll_rownum = 0 THEN
			this.uf_set_pbmessage(TRUE)
			MessageBox("Invalid Country", "Unrecognizable country.  Please correct!", Exclamation!)
			vib_data_ok = False
			Return 1
  		END IF

		If ls_country <>  'USA' or ls_country <> 'CAN' then 	This.SetItem(1,"province"," ")

Case 'name1'
	vib_label_changed = True
	leftTrim(vls_gettext)
	IF IsNull( vls_gettext ) OR match(vls_gettext,"[%+]")  THEN 
		MessageBOX("Invalid Name","Name cannot be blank or begin with a %!")
		this.uf_set_pbmessage ( True )
		RETURN 1
	END IF
	
Case 'address_line1'
	IF (Isnull(vls_gettext)  or vls_gettext ="" or vls_gettext  = ' ') = True Then
		MessageBOX("Invalid Address","Address line 1 cannot be blank !")
		RETURN 1
	END IF
		
END CHOOSE

end event

