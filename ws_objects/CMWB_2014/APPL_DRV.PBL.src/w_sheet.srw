$PBExportHeader$w_sheet.srw
$PBExportComments$Application Driver - Main window for displaying worksheet
forward
global type w_sheet from w_ancestor
end type
type pb_dashboard from picturebutton within w_sheet
end type
type p_maximize from picture within w_sheet
end type
type p_maximize2 from picture within w_sheet
end type
type dw_basic_claim from u_dw_online within w_sheet
end type
type cb_refresh_document_list from commandbutton within w_sheet
end type
type dw_document_path from u_dw_document_path within w_sheet
end type
type uo_claim_search from u_claim_search within w_sheet
end type
type uo_image_append from u_image_append within w_sheet
end type
type r_dw_document_normal from rectangle within w_sheet
end type
type r_dw_document_maximize from rectangle within w_sheet
end type
type dw_documents from u_dw_online within w_sheet
end type
type st_document_splitter_bar from u_splitbar_horizontal within w_sheet
end type
end forward

global type w_sheet from w_ancestor
integer x = 1893
integer y = 48
integer width = 3250
integer height = 2792
string title = ""
string menuname = "m_cmwb"
windowtype windowtype = main!
long backcolor = 67108864
windowdockstate windowdockstate = windowdockstatefloating!
long il_design_time_height = 2780
long il_design_time_width = 2757
pb_dashboard pb_dashboard
p_maximize p_maximize
p_maximize2 p_maximize2
dw_basic_claim dw_basic_claim
cb_refresh_document_list cb_refresh_document_list
dw_document_path dw_document_path
uo_claim_search uo_claim_search
uo_image_append uo_image_append
r_dw_document_normal r_dw_document_normal
r_dw_document_maximize r_dw_document_maximize
dw_documents dw_documents
st_document_splitter_bar st_document_splitter_bar
end type
global w_sheet w_sheet

type prototypes
function long get_trans_count() RPCFUNC ALIAS FOR "dbo.p_get_trans_count"

end prototypes

type variables
s_window_message 	istr_window_message
long			il_document_row_number
string                    		is_image_status
u_dw_document_path 	iu_dw_document_path


// The following are instance variables for each of the
// child windows that can be open on the sheet

w_correspond 		iw_correspond
w_event_log 		iw_event_log
w_calculation_details 	iw_calculation_details
w_cost_analysis		iw_cost_analysis
w_benefits		iw_benefits
w_periodic_award   	iw_awards
w_payments 		iw_payments
w_claim 			iw_claim
w_individual	   	iw_individual
w_payment_inquiry    	iw_payment_inquiry
w_InBasket		iw_InBasket
w_InBasket_old		iw_InBasket_old
w_Account_Payment	iw_Account_Payment
w_Accidents              	iw_Accidents
w_document_import    	iw_document_import
w_document_indexing 	iw_document_indexing
w_document_archive 	iw_document_archive
w_document_details   	iw_document_details
w_reference_search             iw_document_search
w_non_imaged_correspondence	iw_ni_correspondence
w_non_imaged_files_logged_out	iw_ni_files_logged_out
w_overpayments                                iw_overpayments
w_maintain_reporting_fees iw_reporting_fees
w_issue_reporting_fee  iw_issuereportingfee
w_action_list				iw_action_list
w_rx_coverage				iw_rx_coverage
w_excluded_flag         iw_excluded_flag
w_maintain_formulary		iw_maintain_formulary
w_income_requests            iw_income_requests
w_calc_annuity				iw_annuity_inquiry
w_maintain_bank_information iw_maintain_bank_information
w_treatment				iw_treatment
w_prepare_annuity_account iw_annuity_payout_inquiry
w_reissue       iw_reissue
w_management_reports   iw_management_reports
w_maintain_provider_therapist   iw_maintain_provider_therapist
w_form67_web_access iw_form67_web_access



// Declare the menu instance of this sheet

m_cmwb 			im_menu

//Window resize variables
window iw_child_window[]
CONSTANT	LONG				il_child_height_diff = -750
CONSTANT	LONG				il_child_width_diff = -25
CONSTANT	LONG				il_child_min_width = 2679
CONSTANT	LONG				il_child_min_height = 1840

BOOLEAN                 ib_basic_claim_RowFocusChanged
end variables

forward prototypes
public subroutine wf_clear_identifier ()
public function integer wf_unselect ()
public subroutine wf_initialize_basic_claim ()
public subroutine wf_clear_worksheet ()
public function integer wf_list_documents ()
public function integer wf_set_claim (long claim_no)
public subroutine wf_disable_menu ()
public subroutine wf_enable_menu (string as_option)
public function integer wf_resize_child (long il_window_index, long il_newwidth, long il_newheight)
public subroutine wf_register_child (window al_window)
public subroutine wf_menu_toolbar (menu am_menu, boolean ab_enable)
public function boolean wf_get_basic_claim_rfc ()
public subroutine wf_set_basic_claim_rfc (boolean ab_basic_claim_rowfocuschanged)
public subroutine wf_find_related_rehab (ref long al_rehab_sheet_handle, ref w_rehab_sheet aw_rehab_sheet)
public function boolean wf_find_related_individual_dashboard ()
end prototypes

public subroutine wf_clear_identifier ();	wf_clear_worksheet()

Return
end subroutine

public function integer wf_unselect ();/*	This function will unselect any highlighted rows in the list
	or clear the claim number if a basic search was originally performed.

	This is because when you come back to the search screen after opening another module
	that retrieves into the basic claim dw, it get confusing because the currently selected
	row may not match the top dw
*/

	uo_claim_search.dw_search_list.SelectRow(0,FALSE)
	uo_claim_search.dw_search_list_individual.SelectRow(0,FALSE)

	Return 0
end function

public subroutine wf_initialize_basic_claim ();/*MA000711 sr_17 Had to insert a row because all fields were moved from the header
	into the detail band in order to include phone in the tab order*/
	datawindowchild		ldwc_phone
	long						ll_rtn	
	
	dw_basic_claim.getchild("phone",ldwc_phone)
	ldwc_phone.settransobject(sqlca)
	ldwc_phone.insertrow(0)		
	ll_rtn = dw_basic_claim.insertrow(0)		
/********************/
end subroutine

private subroutine wf_clear_worksheet ();/*	This function Clears the worksheet and disables all the buttons. It is called
	from the open of w_sheet and the wf_set_claim window function.
	This function DOES NOT CLEAR the user object uo_claim_search, which is where
	the search list resides
*/
	dw_basic_claim.Reset()
	dw_documents.Reset()

/*	Disable buttons and menu functions
*/
	dw_documents.Enabled					= False
	cb_refresh_document_list.Enabled	= False
	
	wf_disable_menu()

/*	Ensure that the title bar for the shhet does not still have the last guy's name
*/
	This.Title = "Work Sheet"

end subroutine

public function integer wf_list_documents ();//
//	Purpose:
//
//	Determine whether the claim is imaged/non-imaged
//	Retrieve the documents for the claim from the Imaging database and sort by default sort order of document type sort
//
//	Return Codes:	 1	Successful		(determination of whether claim imaged/non-imaged)
//						-1	Unsuccessful	(determination of whether claim imaged/non-imaged)
//
 
Int		li_error_status
Long		ll_claim, ll_cnt
String	ls_imaged_claim_flag
LONG     ll_rehab_sheet_handle
w_rehab_sheet lw_rehab_sheet

If Not dw_basic_claim.RowCount() > 0 Then
	Return 1
End If

ll_claim = dw_basic_claim.GetItemNumber(1,"claim_no")


/* This select was causing sporadic 999 DB errors.  Replaced with datastore code above.  Rob Head 98/08/18.
*/
SELECT	imaged_claim_flag
INTO		:ls_imaged_claim_flag
FROM		CLAIM_MASTER
WHERE		claim_no = :ll_claim
USING		ImageTrans;

li_error_status = ImageTrans.nf_Handle_Error("w_sheet","Datastore Retrieve: Select from CLAIM_MASTER","wf_list_documents")	
If li_error_status < 0 Then
	Return -1
ElseIf li_error_status = 100 Then
	If dw_basic_claim.GetItemString(1,"imaged_flag") = "Y" Then
		MessageBox("Data Integrity", "Claim and Imaging imaged_flag differ.~r~nThis must be fixed before you can work with the claim.~r~nPlease report this to the HelpDesk.")
		Return -1
	End If
	Return 0
End If
//Destroy ds_1

// If the imaged_claim_flag is different from the claim's imaged_flag
// then report this and assume imaging is right
If dw_basic_claim.GetItemString(1,"imaged_flag") <> ls_imaged_claim_flag Then
	MessageBox("Data Integrity", "Claim and Imaging imaged_flag differ.~r~nThis must be fixed before you can work with the claim.~r~nPlease report this to the HelpDesk.")
	Return -1
End If

// Remove any filters that may be set
dw_documents.SetFilter("")
dw_documents.Filter()

// Now that the filters have been removed, we need to manually rest the 'filtered list' indicator on the rehab sheet, if its open
wf_find_related_rehab(ll_rehab_sheet_handle,lw_rehab_sheet)
IF ll_rehab_sheet_handle > 0 THEN
	lw_rehab_sheet.dw_documents.object.t_filtered_indicator.visible = 0
END IF

// The imaged_claim_flag field is passed in as an arguement here because
// if the claim is an imaged claim then we only want to see imaged documents in the file
// if the claim is not imaged then there will only be non-imaged documents and the arguement still works

dw_documents.Retrieve(ll_claim, ls_imaged_claim_flag,'Y')
li_error_status = SQLCA.nf_Handle_Error("w_sheet","dw_documents","wf_list_documents")	
IF li_error_status < 0 THEN
	Return -1
ElseIf li_error_status = 100 THEN
   Return 1
END IF

// If the claim is not imaged then filter for just sent correspondence
If ls_imaged_claim_flag = "N" Then
	dw_documents.SetFilter("doc_file_extension = 'doc' AND sent_flag = 'Y' OR doc_file_extension <> 'doc'")
	dw_documents.Filter()
End If

IF dw_documents.RowCount() > 0 THEN
	dw_documents.uf_ProcessSelect(dw_documents.GetRow(),"Mouse")
	If ls_imaged_claim_flag = "Y" Then
	   im_menu.m_tools.m_senddocuments.Enabled 		= True
	End If
	
	im_menu.m_tools.m_documentarchiving.Enabled  = True 
   im_menu.m_tools.m_senddocuments.Enabled 		= True
   im_menu.m_tools.m_viewdocuments.Enabled 		= True
   im_menu.m_tools.m_highlightdocuments.Enabled = True
   im_menu.m_tools.m_printdocuments.Enabled 		= True
   im_menu.m_tools.m_printdocumentsmaxof5.Enabled = True
	dw_documents.Enabled									= True
	cb_refresh_document_list.Enabled					= True
	

ELSE
	SELECT COUNT(*) 
	INTO :ll_cnt
	FROM DOCUMENT_INDEX_ARCHIVE
	WHERE claim_no = :ll_claim
	USING		ImageTrans;
	
	li_error_status = ImageTrans.nf_Handle_Error("w_sheet","DOCUMENT_ARCHIVE SELECT","wf_list_documents")	
	IF li_error_status < 0 THEN
		Return -1
	END IF
	IF ll_cnt > 0 THEN
		im_menu.m_tools.m_documentarchiving.Enabled  = True
	END IF
   im_menu.m_tools.m_senddocuments.Enabled 		= False
   im_menu.m_tools.m_viewdocuments.Enabled 		= False
   im_menu.m_tools.m_highlightdocuments.Enabled = False
   im_menu.m_tools.m_printdocuments.Enabled 		= False
   im_menu.m_tools.m_printdocumentsmaxof5.Enabled = False
	dw_documents.Enabled									= False
	cb_refresh_document_list.Enabled					= False
	
END IF

Return 1
end function

public function integer wf_set_claim (long claim_no);// wf_set_claim - This function retrieves the basic claim info for the requested claim.
//                It also performs basic claim validation and returns an integer to the calling process.
// Return Codes:   1		Claim Retrieved successfully
//						 0		Claim not found
//						-1		History Claim or Individual
//						-2		Related claim
//						-3		Database Error
//
Integer  li_ReturnCode = 0, li_rtn, li_counter, li_count, li_registered_count
Long     ll_related_claim_no, ll_individual_no, ll_rtn, ll_temp
String   ls_language_code, ls_title_bar, ls_admin_region_code
Datetime ldt_server_datetime , ldt_eligibility_start_date, ldt_eligibility_end_date
DataWindowChild ldwc_phone

//	Clear the previous claim information from the worksheet.
wf_clear_worksheet()

//	If the claim database is available, retrieve the claim
IF SQLCA.ServiceAvailable() THEN
	SELECT individual_no
	  INTO :ll_individual_no
	  FROM CLAIM
	 WHERE claim_no = :claim_no
	 USING SQLCA ;
	
	li_rtn = SQLCA.nf_handle_error("w_sheet", "", "wf_set_claim - SELECT individual_no FROM CLAIM") 
	IF li_rtn < 0 THEN
		RETURN -3
	END IF
	
	dw_basic_claim.getchild("phone", ldwc_phone)
	ldwc_phone.SetTransObject(sqlca)
	ll_rtn = ldwc_phone.Retrieve(ll_individual_no)
	IF ll_rtn = 0 THEN ldwc_phone.InsertRow(0)
			
	dw_basic_claim.Retrieve(claim_no)
	IF SQLCA.nf_handle_error("w_sheet","dw_basic_claim - retrieve of claim: " + String(claim_no,'0000000'),"wf_set_claim") < 0 THEN
		RETURN -3
	END IF

	li_ReturnCode = dw_basic_claim.RowCount()
	IF li_ReturnCode = 0 THEN
		MessageBox("Invalid Claim Number","No record found for requested claim!",Exclamation!)
		RETURN li_ReturnCode
	END IF
	

	IF ldwc_phone.rowcount() > 0 THEN
		ll_rtn = dw_basic_claim.SetItem(1,"phone",ldwc_phone.getitemstring(1,"phone_combination"))
		ll_rtn = ldwc_phone.ScrollToRow(0)				
	END IF

	// Set up the title bar to display the claimant's number and name.
	ls_title_bar = "# " + &
						String(claim_no) + " - " + &
						Trim(dw_basic_claim.GetItemString(1,"given_names")) + " " + &
						Trim(dw_basic_claim.GetItemString(1,"last_name"))

	ls_language_code = dw_basic_claim.GetItemString(1,"language_code")
	IF Upper(ls_language_code) = 'E' OR Upper(ls_language_code) = 'F' THEN
		ls_title_bar = ls_title_bar + "(" + Upper(ls_language_code) + ")"
	END IF

	ls_admin_region_code = dw_basic_claim.GetItemString(1,"admin_region_code")
	IF Upper(ls_admin_region_code) = 'PRV' THEN
		IF dw_basic_claim.GetItemString(1,"imaged_flag") = 'Y' THEN
			ls_title_bar = ls_title_bar + " - Imaged-paper?"
		ELSE
			ls_title_bar = ls_title_bar + " - Non-Imaged"
		END IF
	ELSE
		IF dw_basic_claim.GetItemString(1,"imaged_flag") = 'Y' THEN
			ls_title_bar = ls_title_bar + " - Imaged"
		ELSE
			ls_title_bar = ls_title_bar + " - Non-Imaged"
		END IF
	END IF

	IF NOT ISNULL(dw_basic_claim.GetItemDateTime(1,"individual_death_date")) THEN					
		ls_title_bar = ls_title_bar + " - " + "Deceased"			
	END IF
	
	//	MA June 07,2000 / Added caution flag to title bar
	// kdm - Sept 2005 - caution for any claim participant
	SELECT	Count(*)
	INTO		:li_counter
	FROM		CLAIM_PARTICIPANT		a,
				INDIVIDUAL					b
	WHERE	a.individual_no	= b.individual_no
	AND		a.claim_no		= :claim_no
	AND		b.caution_flag	= 'Y'
	USING SQLCA;
	
	li_rtn = SQLCA.nf_handle_error("w_sheet", "", "wf_set_claim - SELECT eligibility_start_date, eligibility_end_date FROM X001_CURRENT_ELIGIBILITY")
	IF li_rtn < 0 THEN
		RETURN -3
	END IF
	
	IF li_counter > 0 THEN
		ls_title_bar = "CAUTION - " + ls_title_bar
	END IF

	// Add a drug coverage indicator if claim is registered for drug coverage
	SELECT eligibility_start_date, eligibility_end_date 
	  INTO :ldt_eligibility_start_date, :ldt_eligibility_end_date 
	  FROM CLAIM_ELIGIBILITY 
	 WHERE claim_no = :claim_no ; 

	li_rtn = SQLCA.nf_handle_error("w_sheet", "", "wf_set_claim - SELECT eligibility_start_date, eligibility_end_date FROM X001_CURRENT_ELIGIBILITY")
	IF li_rtn < 0 THEN
		RETURN -3
	END IF


	SELECT claim_no 
	  INTO :ll_temp 
	  FROM X001_EXCLUDED_CLAIM  
	 WHERE claim_no = :claim_no
	 AND excluded_flag = "Y" ; 

	li_rtn = SQLCA.nf_handle_error("w_sheet", "", "wf_set_claim - SELECT claim_no FROM X001_EXCLUDED_CLAIM")
	IF li_rtn < 0 THEN
		RETURN -3
	END IF


	This.Title = ls_title_bar
	IF IsValid(w_rehab_sheet) THEN
		IF w_rehab_sheet.wf_get_sheet_handle() = Handle(THIS) THEN
			w_rehab_sheet.Title = 'Rehab Plan - ' + ls_title_bar
		END IF
	END IF

	// Perform basic validation on the claim just retrieved. 
	ll_related_claim_no = dw_basic_claim.GetItemNumber(1,"duplicate_of_claim_no")
	IF ll_related_claim_no > 0 THEN
		im_menu.m_tools.m_eventlog.Enabled = True
		li_ReturnCode = -2
	ELSE
		IF dw_basic_claim.GetItemString(1,"claim_history_flag") = 'Y' OR &
			dw_basic_claim.GetItemString(1,"individual_history_flag") = 'Y' THEN
			li_ReturnCode = -1
		END IF
	END IF
END IF

//	If imaging database is available, Retrieve the documents for the claim.
IF SQLCA.ServiceAvailable() AND ImageTrans.ServiceAvailable() THEN
	IF wf_list_documents() = -1 THEN
		RETURN -3
	END IF
END IF

//	Enable the buttons now that a specific claim has been selected.
// Only do this if the claim database is available since we wouldn't know if this was really a valid claim.
IF SQLCA.ServiceAvailable() THEN
	IF li_ReturnCode >= 0 THEN
		wf_enable_menu("all")
	ELSEIF li_ReturnCode = -1 THEN
		wf_enable_menu("claim/individual")
	ELSEIF li_ReturnCode = -2 THEN
		wf_enable_menu("duplicate")
	END IF
END IF




N_OBJECTHELPER lnv_object_helper
// write to the application log
f_populate_app_log(gs_appname_and_handle ,lnv_object_helper.get_app_component_type(THIS), lnv_object_helper.getpath(THIS), 'wf_set_claim : ' + string(claim_no))

RETURN li_ReturnCode
end function

public subroutine wf_disable_menu ();
wf_menu_toolbar(im_menu.m_tools.m_paymentinquiry,FALSE)
wf_menu_toolbar(im_menu.m_tools.m_paymentmaintenance,FALSE)
wf_menu_toolbar(im_menu.m_tools.m_correspondence,FALSE)
wf_menu_toolbar(im_menu.m_tools.m_eventlog,FALSE)
wf_menu_toolbar(im_menu.m_tools.m_benefits,FALSE)
wf_menu_toolbar(im_menu.m_tools.m_benefitsreadonly,FALSE)

wf_menu_toolbar(im_menu.m_tools.m_claimmaintenance,FALSE)
wf_menu_toolbar(im_menu.m_tools.m_individualmaintenance,FALSE)
wf_menu_toolbar(im_menu.m_tools.m_senddocuments,FALSE)
wf_menu_toolbar(im_menu.m_tools.m_viewdocuments,FALSE)
wf_menu_toolbar(im_menu.m_tools.m_highlightdocuments,FALSE)
wf_menu_toolbar(im_menu.m_tools.m_printdocumentsmaxof5,FALSE)
wf_menu_toolbar(im_menu.m_tools.m_printdocuments,FALSE)
wf_menu_toolbar(im_menu.m_tools.m_documentarchiving,FALSE)
wf_menu_toolbar(im_menu.m_tools.m_accidentstatistics,FALSE)
wf_menu_toolbar(im_menu.m_tools.m_claimreadonly,FALSE)
wf_menu_toolbar(im_menu.m_tools.m_individualreadonly,FALSE)
wf_menu_toolbar(im_menu.m_tools.m_nicorrespondencemaint,FALSE)
wf_menu_toolbar(im_menu.m_tools.m_nifilesloggedoutmaint,FALSE)
wf_menu_toolbar(im_menu.m_tools.m_documentimport,FALSE)
wf_menu_toolbar(im_menu.m_tools.m_overpayments,FALSE)
wf_menu_toolbar(im_menu.m_tools.m_awards,FALSE)
wf_menu_toolbar(im_menu.m_tools.m_awardsreadonly,FALSE)
wf_menu_toolbar(im_menu.m_tools.m_rxcoverage,FALSE)
wf_menu_toolbar(im_menu.m_tools.m_abcc_tools.m_excludedclaimmaintenance,FALSE)

wf_menu_toolbar(im_menu.m_rehab.m_costanalysis,FALSE)
wf_menu_toolbar(im_menu.m_rehab.m_rehabplan,FALSE)
wf_menu_toolbar(im_menu.m_rehab.m_rehabplanreadonly,FALSE)
wf_menu_toolbar(im_menu.m_tools.m_abcc_tools.m_maintainformularyreadonly,FALSE)
wf_menu_toolbar(im_menu.m_tools.m_abcc_tools.m_maintainformulary,FALSE)


Return
end subroutine

public subroutine wf_enable_menu (string as_option);

IF as_option = "all" THEN
	wf_menu_toolbar(im_menu.m_tools.m_paymentinquiry, TRUE)
	wf_menu_toolbar(im_menu.m_tools.m_paymentmaintenance, TRUE)
	wf_menu_toolbar(im_menu.m_tools.m_correspondence, TRUE)
	wf_menu_toolbar(im_menu.m_tools.m_eventlog, TRUE)
	wf_menu_toolbar(im_menu.m_tools.m_benefits, TRUE)
	wf_menu_toolbar(im_menu.m_tools.m_benefitsreadonly, TRUE)
	wf_menu_toolbar(im_menu.m_tools.m_claimmaintenance, TRUE)
	wf_menu_toolbar(im_menu.m_tools.m_individualmaintenance, TRUE)
	wf_menu_toolbar(im_menu.m_tools.m_claimreadonly, TRUE)
	wf_menu_toolbar(im_menu.m_tools.m_individualreadonly, TRUE)
    wf_menu_toolbar(im_menu.m_tools.m_accidentstatistics, TRUE)
	wf_menu_toolbar(im_menu.m_tools.m_nicorrespondencemaint, TRUE)
	wf_menu_toolbar(im_menu.m_tools.m_nifilesloggedoutmaint, TRUE)
	wf_menu_toolbar(im_menu.m_tools.m_documentimport, TRUE)
	wf_menu_toolbar(im_menu.m_tools.m_overpayments, TRUE)
	wf_menu_toolbar(im_menu.m_tools.m_awards, TRUE)
	wf_menu_toolbar(im_menu.m_tools.m_awardsreadonly, TRUE)
	wf_menu_toolbar(im_menu.m_tools.m_rxcoverage, TRUE)
	wf_menu_toolbar(im_menu.m_tools.m_abcc_tools.m_excludedclaimmaintenance, TRUE)
	
	wf_menu_toolbar(im_menu.m_rehab.m_costanalysis, TRUE)
	wf_menu_toolbar(im_menu.m_rehab.m_rehabplan, TRUE)
	wf_menu_toolbar(im_menu.m_rehab.m_rehabplanreadonly, TRUE)	
	wf_menu_toolbar(im_menu.m_tools.m_abcc_tools.m_maintainformularyreadonly, TRUE)	
	wf_menu_toolbar(im_menu.m_tools.m_abcc_tools.m_maintainformulary, TRUE)
	
ELSEIF as_option = "claim/individual" THEN
	wf_menu_toolbar(im_menu.m_tools.m_paymentinquiry, TRUE)
	wf_menu_toolbar(im_menu.m_tools.m_claimmaintenance, TRUE)
	wf_menu_toolbar(im_menu.m_tools.m_individualmaintenance, TRUE)
	wf_menu_toolbar(im_menu.m_tools.m_claimreadonly, TRUE)
	wf_menu_toolbar(im_menu.m_tools.m_individualreadonly, TRUE)
	wf_menu_toolbar(im_menu.m_tools.m_rxcoverage, TRUE)
	wf_menu_toolbar(im_menu.m_tools.m_abcc_tools.m_excludedclaimmaintenance, TRUE)
	
ELSEIF as_option = "duplicate" THEN
	wf_menu_toolbar(im_menu.m_tools.m_paymentinquiry, TRUE)	
	wf_menu_toolbar(im_menu.m_tools.m_claimreadonly, TRUE)
	wf_menu_toolbar(im_menu.m_tools.m_individualreadonly, TRUE)
	wf_menu_toolbar(im_menu.m_tools.m_rxcoverage, TRUE)
	wf_menu_toolbar(im_menu.m_tools.m_benefitsreadonly, TRUE)	
	
END IF


end subroutine

public function integer wf_resize_child (long il_window_index, long il_newwidth, long il_newheight);INTEGER		li_x
BOOLEAN		lb_size_changed
LONG			ll_new_child_width
LONG			ll_new_child_height


//Calculate new height and width
IF il_newwidth + il_child_width_diff >= il_child_min_width THEN
	ll_new_child_width = il_newwidth + il_child_width_diff
	lb_size_changed = True
Else
	ll_new_child_width = il_child_min_width
End if

//Height
IF il_newheight + il_child_height_diff >= il_child_min_height THEN
	ll_new_child_height = il_newheight + il_child_height_diff
	lb_size_changed = True
Else
	ll_new_child_height = il_child_min_height
End if


IF lb_size_changed = True Then	
		//Make sure they are still open	
		IF IsValid(iw_child_window[il_window_index]) THen
			//Resize them if they are open.
			iw_child_window[il_window_index].Resize(ll_new_child_width ,ll_new_child_height)
		End if	
End if

//james
IF gb_additional_logging = TRUE THEN	
		N_OBJECTHELPER lnv_object_helper
		// write to the application log
		f_populate_app_log(gs_appname_and_handle ,lnv_object_helper.get_app_component_type(THIS), lnv_object_helper.getpath(THIS), 'wf_resize_child()' )
	END IF 

return 1
end function

public subroutine wf_register_child (window al_window);BOOLEAN			lb_found
INTEGER			li_x
LONG				ll_new_window_index

FOR li_x = 1 to UpperBound(iw_child_window)
	IF iw_child_window[li_x] = al_window THEN
		//Resize a window that is being opened for the second time
		wf_resize_child(li_x,This.WorkSpaceWidth(),This.WorkSpaceHeight())
		lb_found = TRUE
		exit
	End if
Next


//If the window has not been opened yet then add it to the array.
IF lb_found = False THEN
	
	ll_new_window_index = UpperBound(iw_child_window) + 1
	iw_child_window[ll_new_window_index] = al_window
	//Resize a window that has just been opened for the first time
	wf_resize_child(ll_new_window_index,This.WorkSpaceWidth(),This.WorkSpaceHeight())
End if

end subroutine

public subroutine wf_menu_toolbar (menu am_menu, boolean ab_enable);STRING	ls_toolbar_name
environment env
integer rtn

rtn = GetEnvironment(env)

IF rtn <> 1 THEN 	SignalError(-666,'Could not determine the Operating System.')

//If the Minor Revision is zero then the operating system is Windows 2000
//Removing the toolbaritemname is a workaround for a bug in PB 10.5.1
//Disabled button text would become invisible.
IF env.OSMinorRevision	= 0 Then
	ls_toolbar_name = am_menu.ToolbarItemName
	am_menu.ToolbarItemName = ''
End if

am_menu.Enabled = ab_enable

IF env.OSMinorRevision	= 0 Then
	am_menu.ToolbarItemName = ls_toolbar_name
End if
end subroutine

public function boolean wf_get_basic_claim_rfc ();RETURN ib_basic_claim_rowfocuschanged
end function

public subroutine wf_set_basic_claim_rfc (boolean ab_basic_claim_rowfocuschanged);ib_basic_claim_rowfocuschanged = ab_basic_claim_rowfocuschanged
end subroutine

public subroutine wf_find_related_rehab (ref long al_rehab_sheet_handle, ref w_rehab_sheet aw_rehab_sheet);boolean lb_valid, lb_not_found
LONG    ll_sheet_handle, ll_rehab_sheet_handle
STRING  ls_title
window  lw_window


lb_not_found = TRUE

lw_window = w_frame.GetFirstSheet()
IF IsValid(lw_window) THEN
	DO   
		lw_window = w_frame.GetNextSheet(lw_window)
		lb_valid = IsValid (lw_window)
		IF lb_valid THEN 
			ls_title = Upper(Left(lw_window.Title,10))
			IF ls_title = 'REHAB PLAN' THEN
				ll_sheet_handle = lw_window.DYNAMIC wf_get_sheet_handle()
				IF HANDLE(THIS) = ll_sheet_handle THEN
					lb_not_found = FALSE
					al_rehab_sheet_handle = HANDLE(lw_window)
					aw_rehab_sheet = lw_window
				END IF
			END IF
		ELSE
			lb_not_found = FALSE
		END IF		
	LOOP WHILE lb_not_found
END IF

end subroutine

public function boolean wf_find_related_individual_dashboard ();LONG    ll_sheet_handle


IF IsValid(w_dashboard) THEN
	ll_sheet_handle = w_dashboard.wf_get_sheet_handle()
	IF HANDLE(THIS) = ll_sheet_handle THEN
		RETURN TRUE
	END IF
END IF

RETURN FALSE

end function

event closequery;call super::closequery;BOOLEAN   lb_sheet_match
INTEGER   li_msg_rtn
LONG      ll_rehab_sheet_handle, ll_individual_dashboard_handle
N_PDF     lnv_pdf
w_rehab_sheet lw_rehab_sheet
w_dashboard   lw_individual_dashboard



/*-------------------------------------------------------------------------------------*/
/* If any modules are open, verify that the user really wants to close this worksheet. */
/*-------------------------------------------------------------------------------------*/

IF (IsValid (this.iw_correspond)				or  &
	IsValid (this.iw_event_log) 				or  &
	IsValid (this.iw_benefits) 				or  &
	IsValid (this.iw_payments) 				or  &
	IsValid (this.iw_claim)						or  &			
	IsValid (this.iw_individual)				or  &
	IsValid (this.iw_calculation_details)	or  &
	IsValid (this.iw_ni_correspondence)		or  &
	IsValid (this.iw_ni_files_logged_out)	or  &
	IsValid (this.iw_rx_coverage)        or  &
	IsValid (this.iw_excluded_flag)        or  &
	IsValid (this.iw_treatment)        or  &
	IsValid (this.iw_reissue)            or &
	IsValid (this.iw_payment_inquiry))     THEN
		If MessageBox("Work In Progress", "Do you want to Exit anyway?",Question!,YesNo!,2) = 2 then
			Message.ReturnValue = 1
		END IF
End If


IF dw_document_path.ib_opened_pdf_files THEN
	lnv_pdf = Create n_pdf
	li_msg_rtn = MessageBox('Closing PDF Files','All PDF files will be closed.'&
	                      + '~r~n~r~n'&
	                      + 'Do you want to continue?',Question!,YesNo!,2)
	
	IF li_msg_rtn = 1 THEN
		lnv_pdf.nf_close_pdf_files()
	ELSE
		Message.ReturnValue = 1
	END IF
END IF


/*	Close the rehab sheet if opened.
*/

wf_find_related_rehab(ll_rehab_sheet_handle,lw_rehab_sheet)
IF ll_rehab_sheet_handle > 0 THEN
	CLOSE(lw_rehab_sheet)
END IF

// if the individual dashboard window is open, and it is related to
// the sheet being closed, then close dashboard, and calendar, if it is open also
lb_sheet_match = wf_find_related_individual_dashboard()
IF lb_sheet_match THEN
	CLOSE(w_dashboard)		
	IF IsValid(w_calendar) THEN
		Close(w_calendar)
	END IF
END IF

	
	IF IsValid(w_view_response) THEN
		Close(w_view_response)
	END IF

	IF IsValid(w_travel_expense) THEN
		Close(w_travel_expense)
	END IF
	
	IF IsValid(w_reissue) THEN
		Close(w_reissue)
	END IF

	
IF gb_additional_logging = TRUE THEN	
	N_OBJECTHELPER lnv_object_helper
	// write to the application log
	f_populate_app_log(gs_appname_and_handle ,lnv_object_helper.get_app_component_type(THIS), lnv_object_helper.getpath(THIS), 'closequery ' )
END IF 


end event

event open;call super::open;	INT 	ll_ReturnCode
	datawindowchild		ldwc_phone
	long	ll_rtn

/*	Before associating the data window with the transaction object, check to see if the service
	is available.  If not, don't do the settransobject (the result is that the code will display
	in the drop down instead of the corresponding text)
*/
	IF SQLCA.ServiceAvailable() THEN			
		dw_basic_claim.SetTransObject(SQLCA)		
	END IF

	IF SQLCA.ServiceAvailable() THEN
		dw_documents.SetTransObject (SQLCA)
	END IF


	
	dw_documents.uf_SetSelect(3)


/*	Set up the instance variable for the menu so that we can refer to the sheets menu later
*/
	im_menu = m_cmwb

/*	Create an instance of the user object for the view/print function
*/
	iu_dw_document_path = dw_document_path
	iu_dw_document_path.Hide()
	iu_dw_document_path.uf_set_window_handle(Handle(This))
/*	Clear the worksheet (disables all options) and put an empty row in dw_basic_claim
*/
   wf_clear_worksheet()
	
	wf_initialize_basic_claim()

/*	Set up the claim search user object
*/
	uo_claim_search.uf_set_parent(This)
	uo_claim_search.uf_set_search_type("d_claim_number_search")
	IF not SQLCA.ServiceAvailable() THEN
		uo_claim_search.uf_protect_searchtype("DISABLE")
	END IF
	
	wf_SetResize(True)
	
	inv_resize.of_register(uo_claim_search,'scaletoright&bottom')
	inv_resize.of_register(dw_basic_claim,'scaletoright')
	inv_resize.of_register(dw_documents,'FixedToBottom&ScaleToRight')
	inv_resize.of_register(r_dw_document_normal,'FixedToBottom&ScaleToRight')
	inv_resize.of_register(r_dw_document_maximize,'scaletoright&bottom')
	
	inv_resize.of_register(cb_refresh_document_list,'FixedToRight&Bottom')
	inv_resize.of_register(p_maximize,'FixedToRight&Bottom')
	
	inv_resize.of_register(st_document_splitter_bar,'FixedToBottom&ScaleToRight')
	
	st_document_splitter_bar.of_setrequestor(this)
	st_document_splitter_bar.of_register(dw_documents)
	st_document_splitter_bar.of_register(cb_refresh_document_list,100,0)
	st_document_splitter_bar.of_register(p_maximize,100,0)
	st_document_splitter_bar.of_register(uo_claim_search.dw_search_list_annuity_individual,0,100)	

	




end event

on w_sheet.create
int iCurrent
call super::create
if IsValid(this.MenuID) then destroy(this.MenuID)
if this.MenuName = "m_cmwb" then this.MenuID = create m_cmwb
this.pb_dashboard=create pb_dashboard
this.p_maximize=create p_maximize
this.p_maximize2=create p_maximize2
this.dw_basic_claim=create dw_basic_claim
this.cb_refresh_document_list=create cb_refresh_document_list
this.dw_document_path=create dw_document_path
this.uo_claim_search=create uo_claim_search
this.uo_image_append=create uo_image_append
this.r_dw_document_normal=create r_dw_document_normal
this.r_dw_document_maximize=create r_dw_document_maximize
this.dw_documents=create dw_documents
this.st_document_splitter_bar=create st_document_splitter_bar
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.pb_dashboard
this.Control[iCurrent+2]=this.p_maximize
this.Control[iCurrent+3]=this.p_maximize2
this.Control[iCurrent+4]=this.dw_basic_claim
this.Control[iCurrent+5]=this.cb_refresh_document_list
this.Control[iCurrent+6]=this.dw_document_path
this.Control[iCurrent+7]=this.uo_claim_search
this.Control[iCurrent+8]=this.uo_image_append
this.Control[iCurrent+9]=this.r_dw_document_normal
this.Control[iCurrent+10]=this.r_dw_document_maximize
this.Control[iCurrent+11]=this.dw_documents
this.Control[iCurrent+12]=this.st_document_splitter_bar
end on

on w_sheet.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.pb_dashboard)
destroy(this.p_maximize)
destroy(this.p_maximize2)
destroy(this.dw_basic_claim)
destroy(this.cb_refresh_document_list)
destroy(this.dw_document_path)
destroy(this.uo_claim_search)
destroy(this.uo_image_append)
destroy(this.r_dw_document_normal)
destroy(this.r_dw_document_maximize)
destroy(this.dw_documents)
destroy(this.st_document_splitter_bar)
end on

event activate;call super::activate;If IsValid(w_rehab_sheet) Then
	If	w_rehab_sheet.tab_rehab_plan.tabpage_authorizations.cb_authorization_save.enabled OR &
			w_rehab_sheet.tab_rehab_plan.tabpage_case_monitoring.cb_save_case_monitor.enabled OR &
			w_rehab_sheet.tab_rehab_plan.tabpage_goals_objectives.cb_save_goal_objective.enabled OR &
			w_rehab_sheet.tab_rehab_plan.tabpage_progress.cb_save_progress.enabled OR &
			w_rehab_sheet.tab_rehab_plan.tabpage_task.cb_save_task.enabled OR &
			w_rehab_sheet.tab_rehab_plan.tabpage_action_item.cb_save.enabled OR &
			w_rehab_sheet.tab_rehab_plan.tabpage_voc_profile.cb_save_voc_profile.enabled THEN
		w_rehab_sheet.SetFocus()
		MessageBox('Warning', 'Changes need to be saved on Rehab Plan')
	End If
End If
end event

event resize;call super::resize;INTEGER		li_x

	For li_x = 1 to UpperBound(iw_child_window)		
		//Make sure they are still open	
		IF IsValid(iw_child_window[li_x]) THen
			//Resize them if they are open.
			wf_resize_child(li_x,newwidth,newheight)
		End if
	Next

	IF isvalid(inv_resize) then		
		// T023109 - with the new document splitter bar in place, this ensures that after any resizing to the w_sheet, ie maximizing, minimizing, 
		// opening another module then closing it, dragging the window handle etc, the splitter bar and its registered components are restored
		if p_maximize.picturename <> "restore.bmp" THEN
			p_maximize.picturename = "restore.bmp"    // this ensures the correct branch of p_maximize button code is executed
			p_maximize.triggerevent(Clicked!)
		END IF

	END IF
end event

type pb_dashboard from picturebutton within w_sheet
integer x = 3031
integer y = 20
integer width = 165
integer height = 120
integer taborder = 20
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "views_16.gif"
alignment htextalign = left!
vtextalign vtextalign = top!
end type

event clicked;s_window_message lstr_message
STRING ls_name
LONG  ll_individual_no, ll_index, ll_cntr, ll_exists, ll_step, ll_sheet_handle

IF dw_basic_claim.GetRow() > 0 THEN
	ll_individual_no = dw_basic_claim.GetItemNumber(dw_basic_claim.GetRow(), 'individual_no')
	
	IF IsNull(ll_individual_no) OR ll_individual_no = 0 THEN
		MessageBox('Individual Number','An Individual must be selected prior to opening the Dashboard.', Information!)
		RETURN 
	END IF
	
	ll_sheet_handle = Handle(PARENT)
	
	lstr_message.al_doubleparm[1] = ll_individual_no
	lstr_message.al_doubleparm[2] = ll_sheet_handle
	
	OpenWithParm(w_dashboard, lstr_message)
END IF
end event

type p_maximize from picture within w_sheet
integer x = 2514
integer y = 2252
integer width = 73
integer height = 60
string picturename = "maximize.bmp"
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

event clicked;LONG			ll_current_y
LONG			ll_current_height
LONG			ll_new_height


If this.picturename = "maximize.bmp" Then


	//When the datawindow is maximized, its resize behaviour must change
	inv_resize.of_unregister(dw_documents)
	inv_resize.of_unregister(cb_refresh_document_list)
	inv_resize.of_unregister(p_maximize)
	inv_resize.of_register(dw_documents,'scaletoright&bottom')
	inv_resize.of_register(cb_refresh_document_list,'FixedToRight')
	inv_resize.of_register(p_maximize,'FixedToRight')
	
	st_document_splitter_bar.enabled = false
	st_document_splitter_bar.visible = false	
	
	//	Move and resize the height of the Document List/Index
	dw_documents.Y = r_dw_document_maximize.y
	dw_documents.Height=r_dw_document_maximize.Height

	//	Move the controls that reside on the Document List/Index
	cb_refresh_document_list.Y = r_dw_document_maximize.y + 13
	this.y = r_dw_document_maximize.y + 13


	//	Scroll the current row into view
	IF il_document_row_number > 0 THEN
		dw_documents.ScrollToRow(il_document_row_number)
	END IF

	This.picturename = "restore.bmp"

Else
	
	st_document_splitter_bar.enabled = true
	st_document_splitter_bar.visible = true
	st_document_splitter_bar.Y = r_dw_document_normal.y - 33
	inv_resize.of_unregister(st_document_splitter_bar)
	inv_resize.of_register(st_document_splitter_bar,0,0,100,0)
	st_document_splitter_bar.of_setrequestor(parent)
	st_document_splitter_bar.of_register(dw_documents,0,100)
	
	//When the datawindow is normal sized, its resize behaviour must change
	inv_resize.of_unregister(dw_documents)
	inv_resize.of_unregister(cb_refresh_document_list)
	inv_resize.of_unregister(p_maximize)
	inv_resize.of_register(dw_documents,'FixedToBottom&ScaleToRight')
	inv_resize.of_register(cb_refresh_document_list,'FixedToRight&Bottom')
	inv_resize.of_register(p_maximize,'FixedToRight&Bottom')
	
	//	Move and resize the height of the Document List/Index 
	dw_documents.Y = r_dw_document_normal.y
	dw_documents.Height=r_dw_document_normal.height

	//	Move the controls that reside on the Document List/Index 
	cb_refresh_document_list.Y = r_dw_document_normal.y + 13
	this.y = r_dw_document_normal.y + 13

	//	Scroll the current row into view
	IF il_document_row_number > 0 THEN
		dw_documents.ScrollToRow(il_document_row_number)
	END IF

	This.picturename = "maximize.bmp"
	
	st_document_splitter_bar.bringtotop = TRUE
		
End If


end event

type p_maximize2 from picture within w_sheet
integer x = 3013
integer y = 1084
integer width = 73
integer height = 60
string picturename = "maximize.bmp"
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

event clicked;If this.picturename = "maximize.bmp" Then

	uo_claim_search.uf_shrink_enlarge('ENLARGE')
	
	//	Move the controls that reside on the Document List/Index

	this.Y=515
	This.picturename = "restore.bmp"

Else
	uo_claim_search.uf_shrink_enlarge('SHRINK')
	
	//	Move the controls that reside on the Document List/Index 

	this.Y=1073
	This.picturename = "maximize.bmp"
End If

end event

type dw_basic_claim from u_dw_online within w_sheet
integer x = 27
integer y = 12
integer width = 3163
integer height = 348
integer taborder = 40
string dataobject = "d_basic_claim"
boolean livescroll = false
borderstyle borderstyle = styleraised!
end type

event retrieveend;call super::retrieveend;/*	If the rehab sheet if opened, then trigger the item changed event on the rehab sheet's tombstone
	so that the rehab sheet is refresh with the information of the new claim.
*/



	IF IsValid(w_rehab_sheet) THEN
		ib_basic_claim_RowFocusChanged = TRUE
		w_rehab_sheet.dw_basic_claim.TriggerEvent(Itemchanged!)		
		
		PARENT.post wf_set_basic_claim_RFC(FALSE)
	END IF
	
end event

event itemchanged;call super::itemchanged;
ib_basic_claim_RowFocusChanged = TRUE

PARENT.post wf_set_basic_claim_RFC(FALSE)
end event

event rowfocuschanged;call super::rowfocuschanged;LONG ll_individual_no, ll_sheet_handle
	
IF currentrow > 0 THEN
	ll_individual_no = THIS.GetItemNumber(currentrow,'individual_no')
	IF ll_individual_no > 0 THEN
		IF IsValid(w_dashboard) THEN
			ll_sheet_handle = Handle(PARENT)
			w_dashboard.POST wf_refresh(ll_individual_no,ll_sheet_handle)
		END IF
		
		IF IsValid(w_calendar) THEN
			w_calendar.il_individual_no = ll_individual_no
			w_calendar.wf_initialize_calendar()
		END IF
	END IF
END IF
	
end event

type cb_refresh_document_list from commandbutton within w_sheet
integer x = 2437
integer y = 2256
integer width = 73
integer height = 60
integer taborder = 70
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "R"
end type

on clicked;//
//	Refresh the document list
//

wf_list_documents()

end on

type dw_document_path from u_dw_document_path within w_sheet
boolean visible = false
integer x = 2162
integer y = 368
integer width = 1147
integer taborder = 10
end type

type uo_claim_search from u_claim_search within w_sheet
integer x = 14
integer y = 388
integer width = 3177
integer height = 1772
integer taborder = 50
end type

on uo_claim_search.destroy
call u_claim_search::destroy
end on

type uo_image_append from u_image_append within w_sheet
boolean visible = false
integer x = 1202
integer y = 396
integer width = 261
integer height = 156
integer taborder = 60
boolean bringtotop = true
end type

on uo_image_append.destroy
call u_image_append::destroy
end on

type r_dw_document_normal from rectangle within w_sheet
boolean visible = false
long linecolor = 33554432
integer linethickness = 4
long fillcolor = 268435456
integer x = 23
integer y = 2248
integer width = 2661
integer height = 332
end type

type r_dw_document_maximize from rectangle within w_sheet
boolean visible = false
long linecolor = 33554432
integer linethickness = 4
long fillcolor = 268435456
integer x = 23
integer y = 376
integer width = 3150
integer height = 2212
end type

type dw_documents from u_dw_online within w_sheet
integer x = 23
integer y = 2240
integer width = 2679
integer height = 336
integer taborder = 30
string dataobject = "d_documents"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event doubleclicked;call super::doubleclicked;LONG    ll_doc_id
string ls_doc_type


/*	Get the number of the row that was selected
	Only continue if a row was selected
*/

	il_document_row_number = row
	IF il_document_row_number <= 0 THEN
		RETURN
	END IF

/*	Get the document id for selected row
	View the document
*/
	ll_doc_id =dw_documents.GetItemNumber (il_document_row_number,"ref_docid")
	
	
	/*	Get the document id for selected row
	View the document
*/	
	if uo_image_append.of_init(ll_doc_id)	<= 0 then
		RETURN
	end if
		
		
	ls_doc_type =  uo_image_append.of_get_file_type()
		
	
	CHOOSE CASE ls_doc_type
		/*  Imaged document */ 
		CASE 'IMA', 'TIF'
			if uo_image_append.of_append_image(ll_doc_id) < 0 then	RETURN
		case else
			iu_dw_document_path.f_manage_document(ll_doc_id,"V","NORMAL")
	end choose
	
	IF gb_additional_logging = TRUE THEN	
		N_OBJECTHELPER lnv_object_helper
		// write to the application log
		f_populate_app_log(gs_appname_and_handle ,lnv_object_helper.get_app_component_type(THIS), lnv_object_helper.getpath(THIS), 'doubleclicked - ' + string(  ll_doc_id ) 	+ " docid" )
	END IF 
			
	
		
		

	
end event

event rbuttondown;Long    				ll_rownum
Long                   ll_selected_rows[]
long                    li_count = 1
m_document_popup	lm_document_popup

If This.RowCount() > 0 or this.filteredcount() > 0 Then
	//	Create an instance of the popup menu
	lm_document_popup = Create m_document_popup

	//	Call the menu function to register the datawindow
	m_document_popup.mf_set_datawindow(This)

	//	Popup the menu
	m_document_popup.m_docdetails.popmenu(w_frame.PointerX(),w_frame.PointerY())
	Destroy lm_document_popup
	
	//when changing the sort criteria, a row may still remain highlighted, but not be set as the current row, so do that here. 
	//This is so that when user right-clicks and selects 'more details,' the correct row (document) is used to display the 'more details'
	
	// PR20620 .. Also, users want to be able to select multiple documents, and THEN sort those by date etc, so we need to capture a list of selected row numbers, 
	// before setting the current row,then re-select those remaining rows that had been selected. (Because calling setRow() deselects all other selected rows)

	ll_rownum = this.getSelectedRow(0)
	
	do until ll_rownum = 0
		ll_selected_rows[li_count] = ll_rownum
		ll_rownum = this.getSelectedRow(ll_rownum)
		li_count++
	LOOP
	
	IF upperbound(ll_selected_rows) > 0 THEN
		this.setrow(ll_selected_rows[1]) // this will un-select any other selected rows
		this.scrolltorow(ll_selected_rows[1])
	END IF
	
	li_count = 1
	do until li_count > upperbound(ll_selected_rows)
		this.SelectRow(ll_selected_rows[li_count], TRUE) // now re-select any rows that had been previously selected
		li_count++
	LOOP

End If

IF gb_additional_logging = TRUE THEN	
		N_OBJECTHELPER lnv_object_helper
		// write to the application log
		f_populate_app_log(gs_appname_and_handle ,lnv_object_helper.get_app_component_type(THIS), lnv_object_helper.getpath(THIS), 'rbuttondown' )
	END IF 


end event

event resize;call super::resize;

dw_documents.BringToTop=TRUE
cb_refresh_document_list.BringToTop=TRUE
p_maximize.BringToTop=TRUE
st_document_splitter_bar.BringToTop=TRUE



end event

event rowfocuschanged;call super::rowfocuschanged;LONG ll_sel_row, ll_new_row, ll_rowcount, ll_cntr, ll_multi
BOOLEAN lb_selected


ll_rowcount = dw_documents.RowCount()

FOR ll_cntr = 1 to ll_rowcount
	lb_selected = IsSelected(ll_cntr)
	IF lb_selected = TRUE THEN
		ll_multi = ll_multi + 1
	END IF
NEXT

IF ll_multi = 1 THEN
	ll_sel_row = this.GetSelectedRow(0)

	IF ll_sel_row > 0 THEN
		ll_new_row = currentrow
		THIS.SelectRow(ll_sel_row,FALSE)
		IF ll_new_row > 0 THEN
			THIS.SelectRow(ll_new_row, TRUE)
		END IF
	END IF
END IF
end event

event ue_filter;call super::ue_filter;
STRING 	ls_filter

Open(w_filter_doc_list)

//T023109 - originally I designed w_filter_doc_list as a response window but users wanted ability to sesize it, so had to change it to a popup.
// we pass in a reference to the datawindow being filtered so w_filter_doc_list can do its job directly within the OK button before closing
IF isvalid(w_filter_doc_list) then
	w_filter_doc_list.wf_set_dw(this)
	w_filter_doc_list.wf_show_current_filter()
	w_filter_doc_list.iw_sheet = parent
END IF
end event

type st_document_splitter_bar from u_splitbar_horizontal within w_sheet
integer x = 5
integer y = 2196
integer width = 4000
integer height = 48
boolean bringtotop = true
integer textsize = -7
fontfamily fontfamily = roman!
string facename = "Batang"
string text = "                                                    :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::       "
boolean border = true
long il_min_units_from_bottom = 500
end type

