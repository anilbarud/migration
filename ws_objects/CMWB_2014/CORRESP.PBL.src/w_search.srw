$PBExportHeader$w_search.srw
$PBExportComments$Enter search criteria to produce a provider/employer/card file list
forward
global type w_search from window
end type
type cb_cancel from commandbutton within w_search
end type
type cb_ok from commandbutton within w_search
end type
type dw_srch_criteria from u_dw_online within w_search
end type
type cb_srch_ok from commandbutton within w_search
end type
type gb_criteria from groupbox within w_search
end type
type dw_search_list from u_dw_online within w_search
end type
end forward

global type w_search from window
integer x = 46
integer y = 716
integer width = 1472
integer height = 736
boolean titlebar = true
string title = "Search"
boolean controlmenu = true
windowtype windowtype = child!
long backcolor = 67108864
event ue_filesave pbm_custom01
event ue_open pbm_custom02
cb_cancel cb_cancel
cb_ok cb_ok
dw_srch_criteria dw_srch_criteria
cb_srch_ok cb_srch_ok
gb_criteria gb_criteria
dw_search_list dw_search_list
end type
global w_search w_search

type variables
Protected:

string   		  vis_title
s_correspond_claim  vistr_correspond_claim
long		  vil_row_nbr,vli_total_rows
w_maintain_recipients viw_maintain_recipients
boolean		  vib_clr_rslt_set
boolean		  vib_data_ok, vib_keyshift
end variables

event ue_filesave;//**********************************************************************************
///
// This user event will process the information selected and add an entry
// in recipient_list.
// 
dwitemstatus lb_status
long		vll_rcpnt_id, vll_recipient_no
int		vli_rc, vll_row_nbr
string	vls_rcpnt_type_cd, vls_rcpnt_subtyp_cd, vls_addr_location_cd
string	vls_card_file, vls_label, vls_return_value, vls_name

vib_data_ok  = False

// Gather the information necessary to make the entry in reipient_list.
// NOTE - The card file information will map to different variables than
//	service providers and employers
Choose Case vistr_correspond_claim.rcpnt.rcpnt_type_code
	Case "SVPV"
		vls_name = dw_search_list.GetItemString(vil_row_nbr,"name")
		vls_card_file = dw_search_list.GetItemString(vil_row_nbr,"cardfile")	// Returns 'N' or NULL
		IF UPPER(Trim(vls_card_file)) = 'N' THEN
			vls_addr_location_cd	= "P"
			vll_recipient_no = dw_search_list.GetItemNumber(vil_row_nbr,"provider_no")
			vls_rcpnt_subtyp_cd = dw_search_list.GetItemString(vil_row_nbr,"provider_type_code")
			vll_rcpnt_id = f_get_next_rcpnt_id()
			
			IF vll_rcpnt_id < 0 THEN
				MessageBOX(this.title,"UNABLE TO ASSIGN ADDRESS ID " +String(vll_rcpnt_id)&
				+"~n~r PLEASE CONTACT THE HELPDESK WITH THIS MESSAGE",STOPSIGN!)
				Return 
			END IF

		ELSE
			
			// The card file fields are mapped to the service provider datawindow - Refer to code in parent.cb_ok
			vll_rcpnt_id = dw_search_list.GetItemNumber(vil_row_nbr,"provider_no")
			vls_rcpnt_type_cd = dw_search_list.GetItemString(vil_row_nbr,"provider_type_code")
			vls_rcpnt_subtyp_cd = dw_search_list.GetItemString(vil_row_nbr,"provider_sub_type_code")
			vll_recipient_no = vistr_correspond_claim.ext_addr.address_id
			vls_addr_location_cd	= vistr_correspond_claim.ext_addr.address_location_code
			vll_recipient_no = vistr_correspond_claim.ext_addr.address_id
			vls_card_file = 'Y'
		END IF

	Case "EMPL"
		vls_name = dw_search_list.GetItemString(vil_row_nbr,"employer_name")
		vls_card_file = dw_search_list.GetItemString(vil_row_nbr,"ccardfile")
		IF UPPER(Trim(vls_card_file)) = 'N' THEN
			vls_addr_location_cd	= "E"
			vll_recipient_no = dw_search_list.GetItemNumber(vil_row_nbr,"employer_address_employer_no")
//			vls_rcpnt_subtyp_cd = dw_search_list.GetItemString(vil_row_nbr,"employer_name_type_code")
			vls_rcpnt_subtyp_cd = ""
//			vls_rcpnt_subtyp_cd = dw_search_list.GetItemString(vil_row_nbr,"employer_name_type_code")
			vll_rcpnt_id = f_get_next_rcpnt_id()
			IF vll_rcpnt_id < 0 THEN
				MessageBOX(this.title,"UNABLE TO ASSIGN ADDRESS ID " +String(vll_rcpnt_id)&
				+"~n~r PLEASE CONTACT THE HELPDESK WITH THIS MESSAGE",STOPSIGN!)
				Return 
			END IF 
		ELSE

			// The card file fields are mapped to the employer datawindow - Refer to code in parent.cb_ok
			vll_rcpnt_id = dw_search_list.GetItemNumber(vil_row_nbr,"employer_address_employer_no")
			vls_rcpnt_type_cd =""
//			vls_rcpnt_type_cd = dw_search_list.GetItemString(vil_row_nbr,"employer_name_type_code")
//			vls_rcpnt_subtyp_cd = dw_search_list.GetItemString(vil_row_nbr,"cemployer_name_subtyp_cd")
			vls_rcpnt_subtyp_cd = ""
			vls_addr_location_cd	= vistr_correspond_claim.ext_addr.address_location_code
			vll_recipient_no = vistr_correspond_claim.ext_addr.address_id
			vls_card_file = 'Y'
		END IF

	// All other type codes have search capability on card file table only. 
	//	Service_table is the the default for the datawindow control. Therefore,
	// we obtain the information through it's columns.

	Case ELSE	
		vls_name = dw_search_list.GetItemString(vil_row_nbr,"name")
		vls_card_file = dw_search_list.GetItemString(vil_row_nbr,"cardfile")	// Returns 'N' or NULL

		// The card file fields are mapped to the service provider datawindow - Refer to code in parent.cb_ok
		vll_rcpnt_id = dw_search_list.GetItemNumber(vil_row_nbr,"provider_no")
		vls_rcpnt_type_cd = dw_search_list.GetItemString(vil_row_nbr,"provider_type_code")
		/* Should be null for CardFIle Entries Anyhow - It doesn't matter if selected  */
		vls_rcpnt_subtyp_cd = dw_search_list.GetItemString(vil_row_nbr,"provider_sub_type_code")
//		vls_rcpnt_subtyp_cd = dw_search_list.GetItemString(vil_row_nbr,"provider_type_code")
		vll_recipient_no = vistr_correspond_claim.ext_addr.address_id
		vls_addr_location_cd	= vistr_correspond_claim.ext_addr.address_location_code
		vls_card_file = 'Y'
End Choose

// Check if card file already selected for this type code.
// If no check is done we can get a duplicate key error.
IF UPPER(vls_card_file) = 'Y' THEN 	vli_rc = viw_maintain_recipients.wf_vldte_card_file(vll_rcpnt_id)
IF vli_rc < 0 THEN Return

// Validate label, to make sure they're no duplicates 	
vls_label = viw_maintain_recipients.wf_vldte_label(vls_name)
IF UPPER(vls_label) = "NONE" THEN Return

// Insert the individual selected into the recipient_list	
vll_row_nbr = w_maintain_recipients.dw_individuals.InsertRow(0)

viw_maintain_recipients.dw_individuals.SetItem(vll_row_nbr,"default_address_flag","N")
viw_maintain_recipients.dw_individuals.SetItem(vll_row_nbr,"claim_no",vistr_correspond_claim.claim_no)
viw_maintain_recipients.dw_individuals.SetItem(vll_row_nbr,"correspond_recipient_type_cd",vistr_correspond_claim.rcpnt.rcpnt_type_code)
viw_maintain_recipients.dw_individuals.SetItem(vll_row_nbr,"correspond_recipient_id",vll_rcpnt_id)
viw_maintain_recipients.dw_individuals.SetItem(vll_row_nbr,"claim_recipient_label",vls_label)
viw_maintain_recipients.dw_individuals.SetItem(vll_row_nbr,"address_location_code", vls_addr_location_cd)
viw_maintain_recipients.dw_individuals.SetItem(vll_row_nbr,"recipient_no",vll_recipient_no)
viw_maintain_recipients.dw_individuals.SetItem(vll_row_nbr,"correspond_recipient_subtyp_cd",vls_rcpnt_subtyp_cd)
viw_maintain_recipients.dw_individuals.SetItem(vll_row_nbr,"card_file_flag",vls_card_file)
lb_status = viw_maintain_recipients.dw_individuals.GetItemStatus(vll_row_nbr,0,Primary!)
If lb_status <> NewModified! then
					MessageBox("SAVE ERROR", "An error occurred while attempting recipient " +vls_label&
					+"~n~r to your recipient list. The status indicates something other than NewModified"&
					+"~n~rPlease report this error to the helpdesk!",Stopsign!)
					Return(-1)
End if

SQLCA.nf_begin_transaction()

IF NOT viw_maintain_recipients.wf_update() THEN
	Error.Text        = SQLCA.sqlerrtext
	IF Error.Text = '' THEN
		Error.Text        = 'Error saving the recipient due to a database error.' + &
								  '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.' 
	ELSE
		Error.Text        = Error.Text + '.~r~nError saving the recipient due to a database error.' + &
								  '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.' 
	END IF
	Error.Object      = 'w_search'
	Error.is_database = 'CLAIM'
	Error.il_dbcode   = SQLCA.SQLDBCode
	Error.ObjectEvent = 'ue_filesave'
	SignalError()
END IF

vli_rc = SQLCA.nf_commit_transaction()

IF vli_rc <> 0 THEN
	if vli_rc = -3 then
			MessageBox("DATABSE ERROR: 532-SAVE","ERROR SAVING RECIPIENT." &
					+"~n~r~n~rA DEADLOCK HAS OCCURED. PLEASE RETRY AGAIN!",StopSign!)
			close(this)
			RETURN
	else
		MessageBox("DATABSE ERROR: 532-SAVE","UNABLE TO SAVE RECIPIENT." &
					+"~n~r~n~rPLEASE CONTACT THE HELP DESK WITH THIS MESSAGE!",StopSign!)
		close(this)
		RETURN
	End if
END IF
 
viw_maintain_recipients.wf_redraw_individuals_dw()

vib_data_ok = True
end event

on ue_open;this.BringToTop = True
/* Need to find out why the window doesn't display
*/
dw_srch_criteria.InsertRow(0)
end on

event open;// ************************************************************************************************
// AUTHOR:	MW George
// Modified:	94.09.29

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


vistr_correspond_claim = Message.PowerObjectParm

viw_maintain_recipients = vistr_correspond_claim.parent_window

this.title = "Search for "+viw_maintain_recipients.vis_group_desc

this.PostEvent( "ue_open" )	// Make sure ue_open last thing done





end event

on w_search.create
this.cb_cancel=create cb_cancel
this.cb_ok=create cb_ok
this.dw_srch_criteria=create dw_srch_criteria
this.cb_srch_ok=create cb_srch_ok
this.gb_criteria=create gb_criteria
this.dw_search_list=create dw_search_list
this.Control[]={this.cb_cancel,&
this.cb_ok,&
this.dw_srch_criteria,&
this.cb_srch_ok,&
this.gb_criteria,&
this.dw_search_list}
end on

on w_search.destroy
destroy(this.cb_cancel)
destroy(this.cb_ok)
destroy(this.dw_srch_criteria)
destroy(this.cb_srch_ok)
destroy(this.gb_criteria)
destroy(this.dw_search_list)
end on

type cb_cancel from commandbutton within w_search
integer x = 1042
integer y = 536
integer width = 329
integer height = 96
integer taborder = 50
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
// Modified:	94.09.29
/* Cancel should return back to the calling window which is w_define */
If isValid(viw_maintain_recipients) then
	viw_maintain_recipients.cb_new.TriggerEvent(Clicked!)
	viw_maintain_recipients.parentwindow().SetMicroHelp("Ready")
END IF
close (parent)
end on

type cb_ok from commandbutton within w_search
integer x = 571
integer y = 536
integer width = 329
integer height = 96
integer taborder = 40
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
// Modified:	94.10.05
// ***********************************************************************************************
// DECLARATIONS
long		total_names,combined_total_names,appended_names
string	s_name, s_city, vls_mod_string, vls_type_cd, vls_result,s_where_clause,s_original_sql,mod_string,&
			s_sngle_quote="'"
int		vli_return_code, vli_len_name, vli_len_city
boolean	vlb_card_file_only

// New code for  external data window Search, where I removed single line edits
IF dw_srch_criteria.uf_Accept_dw() = -1  Then 
		dw_srch_criteria.SETFOCUS()
		Return
ELSEIF dw_srch_criteria.uf_datachanged() < 1 THEN
		MessageBox("SEARCH MESSAGE: 513-024","You haven't changed your search criteria, please re-enter.")
		dw_srch_criteria.SETFOCUS()
		Return	
END IF

// Get the latest type description after the accept text event
s_name = UPPER(dw_srch_criteria.GetItemString(1,"name"))
s_city = Upper(dw_srch_criteria.GetItemString(1, "city"))

// Get name entered, if any
IF (IsNull(s_name) or s_name = ' ' or s_name="") = True THEN 	s_name	= "%%"

IF (TRIM(s_name) = "%%"  )  THEN
	MessageBox("SEARCH MESSAGE: 513-024","Name or City must not be blank, please re-enter.")
		dw_srch_criteria.SETFOCUS()
		Return
	ELSE
		s_name = "%"+s_name+"%"	
END IF

// Get city entered, if any
IF (IsNull(s_city) OR  s_city="" or s_city = ' ') = True THEN	
	s_city	= "%%"
ELSE 
	s_city = "%"+s_city+"%"
END IF
// Look for names with a single quote and replace with a two consective single quote, tilde is
// needed for any string variable containing a single quote 
vls_type_cd = UPPER(Trim(vistr_correspond_claim.rcpnt.rcpnt_type_code))
vis_title = viw_maintain_recipients.vis_group_desc + " List"

// Set default list to Custom Recipients and to search on Cardfile entries
vlb_card_file_only = TRUE

CHOOSE CASE vls_type_cd
	CASE "SVPV" 
		dw_search_list.DataObject  =  "d_service_provider_list"
		vlb_card_file_only = FALSE
	CASE "EMPL"
		dw_search_list.DataObject =  "d_employer_list"
		vlb_card_file_only = FALSE
END CHOOSE

vli_return_code = dw_search_list.SetTransObject(SQLCA)

If vls_type_cd = "SVPV" then
	/* If no city entered then remove city criteria from original select     */
	If Trim(s_city) = '%%' Then
			s_original_sql = dw_search_list.Describe("DataWindow.Table.Select")

			s_original_sql = left(s_original_sql,pos(s_original_sql,"WHERE",1)-1)
	
			s_where_clause =  "WHERE ( ( PROVIDER.name like ~~~""+s_name+"~~~") OR "+& 
         						"(PROVIDER.contact_name like ~~~""+s_name+"~~~") ) AND"+&  
        							" ( PROVIDER.active_flag = 'Y' ) " 
			mod_string	=	"DataWindow.Table.Select=~"" + s_original_sql + s_where_clause+ "~"" 
			vls_result = dw_search_list.Modify(mod_string)
		END IF
END IF

If vls_type_cd = "EMPL" then
	/* If no city entered then remove city criteria from original select     */
	If TRIM(s_city) = '%%'  Then
			s_original_sql = dw_search_list.Describe("DataWindow.Table.Select")
			s_original_sql = left(s_original_sql,pos(s_original_sql,"WHERE",1)-1)
			s_where_clause =  "WHERE ( EMPLOYER_ADDRESS.employer_no = EMPLOYER_NAME.employer_no ) and "+&  
         						"( EMPLOYER_NAME.employer_name like ~~~""+s_name+"~~~" ) AND "+&  
          						"( EMPLOYER_ADDRESS.employer_no >= 50000 ) AND "+&
									"( EMPLOYER_ADDRESS.address_type_code = 'BA' ) AND " +&
         						"( EMPLOYER_NAME.employer_name_type_code = 'L' )"    
 			mod_string	=	"DataWindow.Table.Select=~"" + s_original_sql + s_where_clause+ "~"" 
			vls_result = dw_search_list.Modify(mod_string)
		END IF
END IF

SetPointer(Hourglass!)
parent.SetRedraw(False)
vib_clr_rslt_set = False
   
viw_maintain_recipients.parentwindow().SetMicroHelp("Starting Search...")

IF NOT vlb_card_file_only THEN
	vli_return_code = dw_search_list.SetTransObject(SQLCA) // Rob Head 98/09/17.  After modify.
	total_names = dw_search_list.Retrieve(s_name,s_city)
	vli_return_code = SQLCA.nf_handle_error("dw_search_list","w_search","on cb_ok")

	// Extend search to include any results from card file list by
	// using this variable to control the Datawindow buffer in 
	// retrievestart event.
	vib_clr_rslt_set = True

END IF

//************************************************************************************
// Need to modify select statment for the datawindow to retrieve
// the card file entries that match the specified criteria. 
// NOTE - Need to map columns from recipient_address table to 
//	employer or service_provider table. The service provider and employer
//	table also have a computed column called cardfile which will have a value of 'N'.
// This will be used to identify which table the selected row is from.
//	The list from the card file table will have a "Y" for the computed column.

CHOOSE CASE vls_type_cd
	CASE "SVPV"
	vls_mod_string = "DataWindow.Table.Select=~"SELECT substring(name1,1,30),substring(name2,1,30), address_line1, " +&
						"address_line2,  substring(city,1,25), province,postal_code, correspond_recipient_id,correspond_recipient_subtyp_cd,  " +&
						"card_file_flag,correspond_recipient_subtyp_cd FROM RECIPIENT_ADDRESS WHERE (active_flag " +&
						"= 'Y' AND card_file_flag = 'Y' AND  correspond_recipient_type_cd =~~~""+vls_type_cd+"~~~") AND "+&
						"((UPPER(name1) LIKE ~~~""+s_name+"~~~" OR "+&
						"UPPER(name2) LIKE ~~~""+s_name+"~~~") AND "+&
						"UPPER(city) LIKE ~~~"" +s_city+"~~~" )"
  
	s_original_sql = left(vls_mod_string,pos(vls_mod_string,"WHERE",1)-1)

	s_where_clause = 	"WHERE (active_flag = 'Y'  and card_file_flag = 'Y' AND"+&
						" correspond_recipient_type_cd = ~~~"" +vls_type_cd+"~~~") AND "+&
						"(UPPER(name1) LIKE ~~~""+s_name+"~~~" OR "+&
						"UPPER(name2) LIKE ~~~""+s_name+"~~~")"


/* END OF ABOVE COMMENTS WITH EXTRA TILDE  */
/* Employer data object has an extra column address_line3 */
	CASE "EMPL"
	vls_mod_string = "DataWindow.Table.Select=~"SELECT Substring(name1,1,40), substring(name2,1,30), address_line1, " +&
						"address_line2, address_line3, city, province, postal_code, correspond_recipient_id, " +&
						" card_file_flag,correspond_recipient_type_cd" +&
						" FROM RECIPIENT_ADDRESS WHERE (active_flag " +&
						"= 'Y' AND card_file_flag = 'Y' AND  correspond_recipient_type_cd = ~~~""+vls_type_cd+"~~~") AND "+&
						"((UPPER(name1) LIKE ~~~""+s_name+"~~~" OR "+&
						"UPPER(name2) LIKE ~~~""+s_name+"~~~") AND "+&
						"UPPER(city) LIKE ~~~""+s_city+"~~~")"

	s_original_sql = left(vls_mod_string,pos(vls_mod_string,"WHERE",1)-1)

	s_where_clause = 	"WHERE (active_flag = 'Y'  and card_file_flag = 'Y' AND"+&
						" correspond_recipient_type_cd = " + s_sngle_quote+vls_type_cd+s_sngle_quote+ ") AND "+&
						"(UPPER(name1) LIKE ~~~""+s_name+"~~~" OR "+&
						"UPPER(name2) LIKE ~~~""+s_name+"~~~")"
	CASE ELSE

	/* All other cases set up select list, same as columns from RECIPIENT ADDRESS and datawindow should be bound to 
		Service Provider
	*/
	vls_mod_string = "DataWindow.Table.Select=~"SELECT substring(name1,1,30),substring(name2,1,30), address_line1, " +&
						"address_line2,  substring(city,1,25), province,postal_code, correspond_recipient_id,correspond_recipient_subtyp_cd,  " +&
						"card_file_flag,correspond_recipient_subtyp_cd FROM RECIPIENT_ADDRESS WHERE (active_flag " +&
						"= 'Y' AND card_file_flag = 'Y' AND  correspond_recipient_type_cd =~~~""+vls_type_cd+"~~~") AND "+&
						"((UPPER(name1) LIKE ~~~""+s_name+"~~~" OR "+&
						"UPPER(name2) LIKE ~~~""+s_name+"~~~") AND "+&
						"UPPER(city) LIKE ~~~"" +s_city+"~~~" )"
	

	s_original_sql = left(vls_mod_string,pos(vls_mod_string,"WHERE",1)-1)

	s_where_clause = 	"WHERE (active_flag = 'Y'  and card_file_flag = 'Y' AND"+&
						" correspond_recipient_type_cd = " + s_sngle_quote+vls_type_cd+s_sngle_quote+ ") AND "+&
						"(UPPER(name1) LIKE	~~~""+s_name+"~~~" OR "+&
						"UPPER(name2) LIKE	~~~""+s_name+"~~~")"
 
END CHOOSE

/* If no city entered then search without a city */
If TRIM(s_city) ='%%' Then
	mod_string	=	 s_original_sql + s_where_clause+ "~"" 
	vls_result = dw_search_list.Modify(mod_string)
	
ELSE
	vls_result = dw_search_list.Modify(vls_mod_string+ "~"")
	
END IF
	//**********************************************************************************
	// Retrieves new list of names from card file table that meet the criteria, and
	// appends them to the end of the previous list that was selected from either
	// the service_provider or employer table. This will eliminate giving the option 
	// to the user of which table to search from. It will produce one list from
	//	both tables
	///
vli_return_code = dw_search_list.SetTransObject(SQLCA) // Rob Head 98/09/17.  After modify.
dw_search_list.Retrieve(s_name,s_city)

SQLCA.nf_handle_error("dw_search_list","w_search","on cb_ok")
/* To increase performance set the order by on the client 
	It's specified in the datawindow but may be lost due to the modify  */
vli_return_code = dw_search_list.SetSort("name, A, city A")
vli_return_code 	= dw_search_list.Sort()	
parent.SetRedraw(True)
viw_maintain_recipients.parentwindow().SetMicroHelp("Search Completed...")
vli_total_rows = dw_search_list.rowcount()

if vli_total_rows < 1 then
	MessageBox("SEARCH MESSAGE: 513-023","No records were found that match your criteria.")
	Return
else
	if vli_total_rows > 500 then
		MessageBox("SEARCH MESSAGE: 513-024","Your search criteria should be more specific" &
					+"~n~rsince at least 500 matches occurred.")
	end if

	// Setup search result datawindow

	parent.title = vis_title+" ("+String(vli_total_rows)+" items)"
	IF vli_total_rows = 1 THEN
		parent.title = vis_title+" ("+String(vli_total_rows)+" item)"
	END IF
	dw_search_list.visible = True
	cb_ok.default = False
	cb_srch_ok.visible = True
	cb_srch_ok.default = True
	cb_cancel.X = 1756
	cb_cancel.Y = 937
	parent.height = 1157
	parent.width = 2177
	
	// This will trigger the event when the retrieve is done a second time and rowfocus does not change
	dw_search_list.TriggerEvent(RowFocusChanged!)	
	dw_search_list.SetFocus()
end if
end event

type dw_srch_criteria from u_dw_online within w_search
integer x = 55
integer y = 144
integer width = 1312
integer height = 288
integer taborder = 10
string dataobject = "d_srch_criteria"
boolean border = false
end type

event itemchanged;call super::itemchanged;// Declare local variables and 
string	ls_col_value,ls_col_selected
int li_row_nbr	

this.uf_set_pbmessage ( True )

// Get the row number and values
li_row_nbr = GetRow()
ls_col_value = Gettext()
ls_col_selected = GetcolumnName()

Choose Case ls_col_selected
	Case "name"
	// Check if any double quotes exists in the string
	IF Match(ls_col_value, "~"+") THEN	
		MessageBOX("Validation Error","Name  must contain any double quotes!")
		Return 1
	END IF
	
	IF (IsNull( ls_col_value ) OR Len( ls_col_value)  <= 2 &
		OR match(ls_col_value,"[%+]") OR ls_col_value="" )= TRUE   THEN 
		MessageBOX("Validation Error","Name  must contain at least 3 characters and"&
						+ "~n~r cannot begin with a % or blank !")
		RETURN 1
	END IF
	
	Case "city"
	// Check if any double quotes exists in the string
	IF Match(ls_col_value, "~"+") THEN	
		MessageBOX("Validation Error","Name  must contain any double quotes!")
		Return 1
	END IF
End Choose

end event

type cb_srch_ok from commandbutton within w_search
boolean visible = false
integer x = 1371
integer y = 936
integer width = 329
integer height = 96
integer taborder = 60
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Save"
end type

event clicked;// ******************************************************************************************************************************
// AUTHOR:	MW George
// Modified:	94.10.05

// ******************************************************************************************************************************
// DECLARATIONS

boolean		found
int			i,  total_indv
long			rc
string		vls_card_file

// ******************************************************************************************************************************
// ENSURE THE USER SELECTED AN INDIVIDUAL FROM THE LIST

total_indv = dw_search_list.RowCount()
vil_row_nbr = dw_search_list.GetSelectedRow(0)

if total_indv > 0 then
	found = FALSE
	i = 1
	do while not(found) AND i <= total_indv
		if dw_search_list.IsSelected(i) then
			found = TRUE
		else
			i ++
		end if
	loop
	if not(found) then
		MessageBox("LINK MESSAGE: 513-025","An item must be selected from the Search List.")
		return
	end if
	parent.TriggerEvent( "ue_filesave" )
	IF NOT vib_data_ok THEN Return	// Error occured
else
	MessageBox("APPLICATION ERROR: 513-SEARCH","THE GENERATOR COULD NOT DETERMINE THE NUMBER" &
					+"~n~rOF INDIVIDUALS IN THE LIST.~n~r~n~rPLEASE CONTACT YOUR SYSTEM" &
					+" ADMINISTRATOR",StopSign!)
end if

viw_maintain_recipients.Show()

close(parent)
end event

type gb_criteria from groupbox within w_search
integer x = 41
integer y = 80
integer width = 1362
integer height = 408
integer taborder = 30
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Criteria"
end type

type dw_search_list from u_dw_online within w_search
boolean visible = false
integer x = 14
integer width = 2057
integer height = 816
integer taborder = 20
string dataobject = "d_service_provider_list"
boolean hscrollbar = true
boolean vscrollbar = true
end type

event retrievestart;IF vib_clr_rslt_set THEN
	RETURN 2
END IF
end event

event rowfocuschanged;
IF THIS.GetRow() > 0 THEN
	vil_row_nbr = THIS.GetRow()
	
	IF vil_row_nbr <> 0 THEN
		// Deselect previous selection made in DataWindow
		THIS.SelectRow(0, FALSE)
		THIS.SelectRow(vil_row_nbr, TRUE)
	END IF
END IF
end event

event dberror;SQLCA.SQLDBCode = sqldbcode
SQLCA.SQLErrText = sqlerrtext
RETURN 1
end event

on doubleclicked;cb_srch_ok.TriggerEvent(Clicked!)
end on

