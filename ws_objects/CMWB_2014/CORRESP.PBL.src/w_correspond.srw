$PBExportHeader$w_correspond.srw
$PBExportComments$Main module window which maintains correspondence (i.e. generates, edit, send and delete)
forward
global type w_correspond from w_a_tool
end type
type cb_edit_type from commandbutton within w_correspond
end type
type dw_get_med_service_providers from u_dw_online within w_correspond
end type
type dw_get_address from u_dw_online within w_correspond
end type
type dw_individuals from u_dw_online within w_correspond
end type
type st_1 from statictext within w_correspond
end type
type sle_find from singlelineedit within w_correspond
end type
type rb_french from radiobutton within w_correspond
end type
type rb_english from radiobutton within w_correspond
end type
type cb_recipients from commandbutton within w_correspond
end type
type cb_preview from commandbutton within w_correspond
end type
type cb_delete from commandbutton within w_correspond
end type
type dw_generated_list from u_dw_online within w_correspond
end type
type cb_send from commandbutton within w_correspond
end type
type cb_edit from commandbutton within w_correspond
end type
type cb_compose from commandbutton within w_correspond
end type
type composed_list_gb from groupbox within w_correspond
end type
type dw_get_doc_sender from u_dw_online within w_correspond
end type
type dw_template_list from u_dw_online within w_correspond
end type
type template_list_gb from groupbox within w_correspond
end type
type lbx_processes from listbox within w_correspond
end type
end forward

global type w_correspond from w_a_tool
string tag = "w_correspond"
integer width = 2720
integer height = 1832
string title = ""
boolean resizable = false
string icon = "None!"
event correspondactivate pbm_custom01
event ue_post_open pbm_custom02
cb_edit_type cb_edit_type
dw_get_med_service_providers dw_get_med_service_providers
dw_get_address dw_get_address
dw_individuals dw_individuals
st_1 st_1
sle_find sle_find
rb_french rb_french
rb_english rb_english
cb_recipients cb_recipients
cb_preview cb_preview
cb_delete cb_delete
dw_generated_list dw_generated_list
cb_send cb_send
cb_edit cb_edit
cb_compose cb_compose
composed_list_gb composed_list_gb
dw_get_doc_sender dw_get_doc_sender
dw_template_list dw_template_list
template_list_gb template_list_gb
lbx_processes lbx_processes
end type
global w_correspond w_correspond

type prototypes
Function ulong CreateToolhelp32Snapshot (ulong dwFlags, ulong th32ProcessID) Library "KERNEL32.DLL"
Function boolean Process32First (ulong hSnapshot, ref PROCESSENTRY32 lppe) Library "KERNEL32.DLL" alias for "Process32First;Ansi"
Function boolean Process32Next (ulong hSnapshot, ref PROCESSENTRY32 lppe) Library "KERNEL32.DLL" alias for "Process32Next;Ansi"
Function boolean CloseHandle (ref ulong hObject) Library "KERNEL32.DLL"
Function boolean TerminateProcess (ulong hProcess, uint uExitCode) LIBRARY "KERNEL32.DLL"
Function boolean GetExitCodeProcess (ulong hProcess, Ref uint lpExitCode) LIBRARY "KERNEL32.DLL"
Function ulong OpenProcess(ulong dwdesiredaccess, boolean binheritHandle,ulong dwprocessid) Library "kernel32.dll"
Function long GetLastError() Library "kernel32.dll"
Function Boolean EnumProcesses(REF processEntry Process, long cb, REF long cbNeeded ) Library "PSAPI.DLL" alias for "EnumProcesses;Ansi"
Function boolean EnumProcessModules( ulong hProcess, REF ModuleEntry Module, long cb, REF long lpcbNeeded ) LIBRARY "PSAPI.DLL" alias for "EnumProcessModules;Ansi"
Function long GetModuleBaseNameA(ulong hProcess, ulong hModule, REF string lpBaseName,long nSize) LIBRARY "PSAPI.DLL" alias for "GetModuleBaseNameA;Ansi"
Function long GetModuleFileNameExA(ulong hProcess, ulong hModule, REF string lpBaseName,long nSize) LIBRARY "PSAPI.DLL" alias for "GetModuleFileNameExA;Ansi"

end prototypes

type variables
long                         vil_total_templates
long                         vil_template_list_rowno
long                         vil_generated_list_rowno

s_correspond_claim   vistr_correspond_claim
w_maintain               viw_maintain
string		   vis_filt
int		   vii_return_code
w_sheet		   viw_active_sheet		   
long		   il_usage,   il_edit_doc_handle
boolean		   vib_change_dot, vib_change_doc
Private:
int		ii_nbr_rows,ii_row_no
long		il_employer_no	
boolean		ib_datawin1

u_word ioo_word

ulong  iul_processid

// for potential future use, listed all
private:
PROCESSENTRY32 ipe_processentry[]
constant ulong TH32CS_SNAPHEAPLIST = 1 //0x00000001
constant ulong TH32CS_SNAPPROCESS  = 2 //0x00000002
constant ulong TH32CS_SNAPTHREAD   = 4 //0x00000004
constant ulong TH32CS_SNAPMODULE   = 8 //0x00000008
constant ulong TH32CS_SNAPALL      = TH32CS_SNAPHEAPLIST + TH32CS_SNAPPROCESS + TH32CS_SNAPTHREAD + TH32CS_SNAPMODULE
constant ulong TH32CS_INHERIT      = 2147483648 //0x80000000
constant ulong  PROCESS_TERMINATE = 1
constant ulong  PROCESS_CREATE_THREAD = 2
constant ulong  PROCESS_SET_SESSIONID = 4
constant ulong  PROCESS_VM_OPERATION = 8
constant ulong  PROCESS_VM_READ = 16
constant ulong  PROCESS_VM_WRITE = 32
constant ulong  PROCESS_DUP_HANDLE = 64
constant ulong  PROCESS_CREATE_PROCESS = 128
constant ulong  PROCESS_SET_QUOTA = 256
constant ulong  PROCESS_SET_INFORMATION = 512
constant ulong  PROCESS_QUERY_INFORMATION = 1024
constant ulong  PROCESS_ALL_ACCESS = 2035711

Environment ie_op_sys
string is_return

end variables

forward prototypes
public function long wf_get_next_corr_no ()
public function long wf_set_default_claimant ()
public function integer wf_set_default_provider (integer ai_nbr_doc_spv, integer ai_nbr_corr_spv)
public function integer wf_update_claimant_address ()
public function long wf_set_default_employer ()
public subroutine wf_set_filter (string expression)
public function boolean wf_destroyprocess (unsignedlong aul_processid)
public function string wf_error (integer ai_error)
public subroutine wf_2000_close_winword ()
public subroutine wf_nt4_close_winword ()
public subroutine wf_reset_buttonss (ref datawindow adw_dwname)
public subroutine wf_reset_buttons (boolean status, string range)
public function boolean wf_check_individual_deceased (long al_individual_no)
public function boolean wf_check_payout_letter_in_claim_ss (long al_claim_no)
public function u_ds wf_check_payout_letter_not_sent_iw (long al_individual_no)
public function any open_word_file (string as_physical_file_path, string as_claim, string as_action)
public function u_ds wf_check_already_payout_letter_in_claim (long al_individual_no)
public function boolean wf_check_future_annuity_end_date (long al_individual_no)
public function boolean wf_check_individual_eligible_for_annuity (long al_individual_no)
public function boolean wf_check_annuity_being_confirmed (long al_individual_no)
public function boolean wf_check_payout_letter_not_sent_ss (long al_individual_no)
public function boolean wf_check_no_calculation_for_payout (long al_individual_no)
public function u_ds wf_check_payout_letter_sent_iw (long al_individual_no)
public function integer wf_check_for_surviving_spouse (long al_claim_no)
end prototypes

event correspondactivate;// ******************************************************************************************************************************
// AUTHOR:	MW George
// Modified:	94.10.20

// ******************************************************************************************************************************
// DECLARATION

long	rc, total_generated



N_OBJECTHELPER lnv_object_helper

f_populate_app_log(gs_appname,lnv_object_helper.get_app_component_type(THIS),lnv_object_helper.getpath(THIS),'activate event')




// ******************************************************************************************************************************
// RE-RETRIEVE THE LIST OF EXISTING DOCUMENTS GENERATED FOR THIS CLAIM, REFRESHING THE 
// WINDOW
/* New code added to remove the highlighting left on by datawindow -dw_documents on the sheet during opening
*/ 
If isvalid(viw_active_sheet) then
	viw_active_sheet.dw_documents.Setredraw(False)
	viw_active_sheet.dw_documents.Selectrow(0,False)
	viw_active_sheet.dw_documents.Setredraw(True)
	
END if
dw_generated_list.Selectrow(0,False)	
rc = dw_generated_list.Retrieve(vistr_correspond_claim.claim_no)
vii_return_code = SQLCA.nf_handle_error("dw_generated_list","w_correspond","on correspondactivate event")
vil_total_templates = dw_template_list.RowCount()
total_generated = dw_generated_list.RowCount()
composed_list_gb.text = "Composed ("+string(total_generated)+"):"

	if vil_template_list_rowno > 0 then
	
		/*
	 	"TEMPLATE" WINDOW WAS LAST SELECTED
   	*/ 
		dw_template_list.Selectrow(0,False)	
//		sle_find.text =  dw_template_list.GetitemString(vil_template_list_rowno,"template_code") // 0825 Uncommented for now
else
		/*
	 	"GENERATED" WINDOW WAS LAST SELECTED
		*/
		dw_generated_list.Selectrow(0,False)
//		sle_find.text = ""
end if

// Clear what's left in the filter which is used by
// sle_find - key_down event
//vis_filt = ""

// Refresh the find template entry
sle_find.text = "" /* these two lines were commented out but have been added back in 082597 */
sle_find.SetFocus()
If ib_datawin1 then dw_template_list.SetFocus() Else dw_generated_list.SetFocus()



end event

event ue_post_open;LONG		 	vli_nbr_svpv,vll_nbr_indiv, ll_clmt_row_no
STRING		full_name

//*********************************************************************************************************************************
//
// Retrieve the template, electronic signature, and correspondence paths, before proceding 
//
//*********************************************************************************************************************************

// Get the english template path from cmwb.ini
	vistr_correspond_claim.corr.engl_tmplt_path = ProfileString(vgs_ini_filename,"CORRESPONDENCE","EnglishTemplate","None")
	
	IF vistr_correspond_claim.corr.engl_tmplt_path = "None" THEN
		MessageBox("Send Error", "Could not read the english template path from the cmwb.ini file."&
		+ "~n~r~n~rPLEASE CONTACT YOUR SYSTEM ADMINISTRATOR",StopSign!)
		Close(this)
		Return
	END IF

// Get the french template path from cmwb.ini
	vistr_correspond_claim.corr.french_tmplt_path = ProfileString(vgs_ini_filename,"CORRESPONDENCE","FrenchTemplate","None")
	
	IF vistr_correspond_claim.corr.french_tmplt_path = "None" THEN
		MessageBox("Send Error", "Could not read the french template path from the cmwb.ini file."&
		+ "~n~r~n~rPLEASE CONTACT YOUR SYSTEM ADMINISTRATOR",StopSign!)
		Close(this)
		Return
	END IF

// Get the composed correspondence path from cmwb.ini
	vistr_correspond_claim.corr.composed_path = ProfileString(vgs_ini_filename,"CORRESPONDENCE","ComposedLetter","None")
	
	IF vistr_correspond_claim.corr.composed_path = "None" THEN
		MessageBox("Send Error", "Could not read the composed correspondence path from the cmwb.ini file."&
		+ "~n~r~n~rPLEASE CONTACT YOUR SYSTEM ADMINISTRATOR",StopSign!)
		Close(this)
		Return
	END IF

// Get the signature path from cmwb.ini
	vistr_correspond_claim.corr.signature_path = ProfileString(vgs_ini_filename,"CORRESPONDENCE","Signature","None")

	IF vistr_correspond_claim.corr.signature_path = "None" THEN
		MessageBox("Send Error", "Could not read the electronic signature path from the cmwb.ini file."&
		+ "~n~r~n~rPLEASE CONTACT YOUR SYSTEM ADMINISTRATOR",StopSign!)
		Close(this)
		Return
	END IF

// Get the composed correspondence path from cmwb.ini
	vistr_correspond_claim.corr.word_path = ProfileString(vgs_ini_filename,"CORRESPONDENCE","wordexe","None")

	IF vistr_correspond_claim.corr.word_path = "None" THEN
		MessageBox("Send Error", "Could not read the word executable path from the cmwb.ini file."&
		+ "~n~r~n~rPLEASE CONTACT YOUR SYSTEM ADMINISTRATOR",StopSign!)
		Close(this)
		Return
	END IF


	this.parentwindow().SetMicroHelp("Validating active sheet completed...")
	
	vistr_correspond_claim.claim_no								= viw_active_sheet.dw_basic_claim.GetItemNumber(1,"claim_no")
	vistr_correspond_claim.Individual_no							=	viw_active_sheet.dw_basic_claim.GetItemNumber(1,"individual_no")
	vistr_correspond_claim.accident_date							=  Date(viw_active_sheet.dw_basic_claim.GetItemDateTime(1,"accident_date"))
	vistr_correspond_claim.region_code							= viw_active_sheet.dw_basic_claim.GetItemString(1,"admin_region_code")
	vistr_correspond_claim.accident_employer_phone_no	= viw_active_sheet.dw_basic_claim.GetItemString(1,"employer_address_telephone_no")
	vistr_correspond_claim.accident_employer_name			= viw_active_sheet.dw_basic_claim.GetItemString(1,"employer_legal_name")
	vistr_correspond_claim.case_manager						= viw_active_sheet.dw_basic_claim.GetItemString(1,"claim_manager_user_id")
	vistr_correspond_claim.image_status							= viw_active_sheet.dw_basic_claim.GetItemString(1,"imaged_flag")
	vistr_correspond_claim.corr.corr_no							= -1
	vistr_correspond_claim.corr.template_type					= " "
	vistr_correspond_claim.corr.template_extension        = ""
	vistr_correspond_claim.corr.comments						= " "
	vistr_correspond_claim.corr.version_no						= " "
	vistr_correspond_claim.corr.document_name				= " "
	vistr_correspond_claim.corr.corr_action						= " "
	vistr_correspond_claim.rcpnt.rcpnt_type_code				= " "
	vistr_correspond_claim.rcpnt.claim_rcpnt_id				= -1
	vistr_correspond_claim.rcpnt.claim_rcpnt_label				= " "
	vistr_correspond_claim.rcpnt.action							= " "
	vistr_correspond_claim.rcpnt.default_status					= " "
	vistr_correspond_claim.ext_addr.address_location_code	= "C"
	vistr_correspond_claim.ext_addr.address_id				= -1
	vistr_correspond_claim.parent_window						= this
	vistr_correspond_claim.sheet_window 						= viw_active_sheet
	vistr_correspond_claim.annuity_payout_individual_no 	= viw_active_sheet.dw_basic_claim.GetItemNumber(1,"individual_no")

// Retrieve the full case manager name from the userid in case manager
	SELECT	 user_first_name+' '+user_last_name 
	INTO 		:full_name
	FROM 	User_Profile
	WHERE 	user_id = :vistr_correspond_claim.case_manager 
	USING 	SQLCA;
	SQLCA.nf_handle_error("w_correspond","ue_post_open event","SELECT	 user_first_name+' '+user_last_name") 
	
	vistr_correspond_claim.case_manager = full_name

// *****************************************************************************************************
// Setup Claim Recipient 
//******************************************************************************************************

	vll_nbr_indiv = dw_individuals.Retrieve(vistr_correspond_claim.claim_no, "CLMT")
	SQLCA.nf_handle_error("w_correspond","ue_post_open event","dw_individuals.Retrieve")
	
	If vll_nbr_indiv = 0 then
		vll_nbr_indiv = wf_set_default_claimant()
	
		If vll_nbr_indiv < 0 THEN
			Close(this)
			Return
		End If

		this.parentwindow().SetMicroHelp("Setting default claimant completed...")
		SetPointer(HourGlass!)	// Reset, because the window functions will set pointer back to arrow at the end of their scripts
	ELSE
	/*		
		 Update claimant address, because it could have changed through the individual module.
		 The Module allows modification of the claimant's address
		 get the latest and greatest individual information and update
		 should retrieve the LABEL CLAIMANT BECAUSE THERE SHOULD ONLY BE ONE
	*/
	  ll_clmt_row_no =	dw_individuals.Find("UPPER(claim_recipient_label) = 'CLAIMANT'and UPPER(correspond_recipient_type_cd) = 'CLMT'",1,dw_individuals.Rowcount())	
		If ll_clmt_row_no > 0 then
			vistr_correspond_claim.rcpnt.claim_rcpnt_id = dw_individuals.GETITEMNUMBER(ll_clmt_row_no,"correspond_recipient_id")
		Else
			MessageBox("DATABASE INTEGRITY ERROR: 512-OK","UNABLE TO FIND THE DEFAULT CLAIMANT LABEL" &
					+"~n~r~n~rPLEASE CONTACT YOUR SYSTEM ADMINISTRATOR.",StopSign!)
			
			Close(this)
			Return
		End If
		wf_update_claimant_address()	
	End If
	vistr_correspond_claim.rcpnt.rcpnt_type_code				= "CLMT"
	vistr_correspond_claim.rcpnt.claim_rcpnt_label			= "Claimant"
		  
//**************************************************************************************
// Setup employer as default recipient
//**************************************************************************************
	vistr_correspond_claim.employer_no =  viw_active_sheet.dw_basic_claim.GetItemNumber(1,"claim_accident_employer_no")
	ii_nbr_rows = dw_individuals.Retrieve(vistr_correspond_claim.claim_no, "EMPL")
	SQLCA.nf_handle_error("w_correspond","ue_post_open event","dw_individuals.Retrieve...EMPL")
	
	/* If there's an accident employer in the tombstone then call default employer  */

	IF vistr_correspond_claim.employer_no <> 0 then  
		IF  ii_nbr_rows > 0 then 
			/* If employer_no's are The same the  default is already set up */
			ii_row_no =	dw_individuals.Find("UPPER(claim_recipient_label) = 'EMPLOYER'and UPPER(correspond_recipient_type_cd) = 'EMPL'",1,dw_individuals.Rowcount())
			If ii_row_no > 0 then il_employer_no = dw_individuals.GETITEMNUMBER(ii_row_no,"recipient_no")
					
			IF vistr_correspond_claim.employer_no <> il_employer_no then 
				IF wf_set_default_employer() < 0 then
						this.Triggerevent("close")
						Return
				END IF
			END IF
	
		ElseIF wf_set_default_employer() < 0 then
				this.Triggerevent("close")
				Return
		End If
	END IF
	this.parentwindow().SetMicroHelp("Setting default employer completed...")


// *****************************************************************************************
// Setup Service provider default recipient
// ******************************************************************************************

	vll_nbr_indiv = dw_individuals.Retrieve(vistr_correspond_claim.claim_no, "SVPV")
	SQLCA.nf_handle_error("w_correspond","ue_post_open event","dw_individuals.Retrieve SVPV") 
	
	vli_nbr_svpv = dw_get_doc_sender.Retrieve(vistr_correspond_claim.claim_no)
	IMAGETRANS.nf_handle_error("w_correspond","ue_post_open event","dw_get_doc_sender.Retrieve")

	If vli_nbr_svpv  > 0 then 		vii_return_code = wf_set_default_provider(vli_nbr_svpv,vll_nbr_indiv)
	this.parentwindow().SetMicroHelp("Setting of  default providers completed...") 
		
//**********************************************************************************************************
// End of Service Provider Set Up
//**********************************************************************************************************	
	If Isvalid(viw_active_sheet) then
		viw_active_sheet.dw_documents.SelectRow(0,False)	
	END IF

	vil_template_list_rowno =1
	SetPointer(HourGlass!)	// Reset, because the window functions will set pointer back to arrow at the end of their scripts
end event

public function long wf_get_next_corr_no ();// ************************************************************************************************
// AUTHOR:	MW George
// Modified:	94.10.20

// ************************************************************************************************
// AUTHOR:	MW George
// Modified:	94.09.28

// ************************************************************************************************
// THIS FUNCTION WILL LOOK AT THE "CLAIM_PARAMETER" TABLE TO DETERMINE THE
// NEXT AVAILABLE "CORRESPOND_NO".  IF A RECORD DOES NOT EXIST IN THIS TABLE
// FOR THE CURRENT CLAIM, ONE WILL BE CREATED WITH ALL FIELDS ASSIGNED A 
// ZERO VALUE WITH THE EXCEPTION OF "LAST_CORRESPOND_NO".
// IF A RECORD DID EXIST, THE VALUE OF THIS FIELD IS INCREMENTED.

// ************************************************************************************************
// DECLARATIONS

int		vli_rc
long	vll_corr_no

N_OBJECTHELPER lnv_object_helper

f_populate_app_log(gs_appname,lnv_object_helper.get_app_component_type(THIS),lnv_object_helper.getpath(THIS),'wf_get_next_corr_no')


vll_corr_no = 1

SQLCA.nf_begin_transaction()

UPDATE	Claim_Parameter
SET			Claim_Parameter.last_correspond_no = Claim_Parameter.last_correspond_no + 1
WHERE		Claim_Parameter.claim_no	= :vistr_correspond_claim.claim_no
USING		SQLCA;

SQLCA.nf_handle_error("Embedded SQL Update Claim_Parameter ","w_correspond","on wf_get_next_corr_no")


Choose Case SQLCA.SQLnrows 

	Case is> 0 	
	
		// ************************************************************************************************
		// RETRIEVE THE CLAIM_PARAMETER RECORD FOR THE CURRENT CLAIM

		SELECT		Claim_Parameter.last_correspond_no
		INTO		:vll_corr_no
		FROM		Claim_Parameter
		WHERE		Claim_Parameter.claim_no = :vistr_correspond_claim.claim_no
		USING		SQLCA;
		vii_return_code = SQLCA.nf_handle_error("Embedded SQL Select on Claim_Parameter ","w_correspond","on wf_get_next_corr_no")
		
		CHOOSE case vii_return_code
		
			Case 0
			  // Do nothing the record you updated can be found
			Case Else
				return -1	
		End CHOOSE
	
	Case 0
					
		
// *******************************************************************************************
		// RECORD DOES NOT EXIST FOR THIS CLAIM SO CREATE ONE
		 
		INSERT into Claim_Parameter
			(claim_no, last_correspond_no)
		VALUES
			(:vistr_correspond_claim.claim_no, 1)
		USING	SQLCA;

		SQLCA.nf_handle_error("Embedded SQL Insert on Claim_Parameter","w_correspond","on wf_get_next_corr_no")

	End Choose   // End of SQLCA.SQLNROWS


SQLCA.nf_commit_transaction()

RETURN vll_corr_no
end function

public function long wf_set_default_claimant ();// ************************************************************************************************
// AUTHOR:	MW George
// Modified:	94.10.20

// ************************************************************************************************
// THIS FUNCTION WILL SET UP A DEFAULT CLAIMANT RECIPIENT SINCE NONE CURRENTLY
// EXISTS AND IT WILL HAVE THE LABEL "CLAIMANT"

// ************************************************************************************************
// DECLARATIONS

long		rc, urc, dbrc, vll_rcpnt_id,vll_individual_no
string	vls_name,sin_no,medicare_no
datetime	vdt_birth_date  
dwitemstatus lb_status
SetPointer(HourGlass!)

N_OBJECTHELPER lnv_object_helper

f_populate_app_log(gs_appname,lnv_object_helper.get_app_component_type(THIS),lnv_object_helper.getpath(THIS),'wf_set_default_claimant')
				
	vll_rcpnt_id = f_get_next_rcpnt_id()

	If vll_rcpnt_id < 0 THEN
		MessageBOX(this.title,"UNABLE TO ASSIGN ADDRESS ID " +String(vll_rcpnt_id)&
		+"~n~r PLEASE CONTACT THE HELPDESK WITH THIS MESSAGE",STOPSIGN!)
		Return -1 
	End If
 
// PR1456 - S.Manzer Added Street Address Oct 27, 2000
// Retrieve the claimant information
	SELECT	str(medicare_no,9),str(sin_no,9),language_code,last_name,given_names,address_line1,city,
				prov_state_code,country_code,telephone_no,
				address_line1,address_line2,birth_date,postal_code
 	INTO
				:vistr_correspond_claim.medicare_no,:vistr_correspond_claim.sin_no,
				:vistr_correspond_claim.language,:vistr_correspond_claim.last_name,:vistr_correspond_claim.first_name,
				:vistr_correspond_claim.street,
				:vistr_correspond_claim.city,:vistr_correspond_claim.province,:vistr_correspond_claim.country,
				:vistr_correspond_claim.claimant_phone_no,:vistr_correspond_claim.address_line1,
				:vistr_correspond_claim.address_line2,:vdt_birth_date,:vistr_correspond_claim.postal_code	
	FROM		INDIVIDUAL
	WHERE		individual_no = :vistr_correspond_claim.individual_no
	USING 	SQLCA;
					
	SQLCA.nf_handle_error("SQL Select on Individual","w_correspond","on wf_set_default_claimant")
	
  	
	  Trim(vistr_correspond_claim.medicare_no)
	Trim(vistr_correspond_claim.sin_no)
	Trim(vistr_correspond_claim.last_name)
	vistr_correspond_claim.birth_date = Date(vdt_birth_date)
	vistr_correspond_claim.rcpnt.claim_rcpnt_id = vll_rcpnt_id
	vistr_correspond_claim.rcpnt.rcpnt_type_code				= "CLMT"
	vistr_correspond_claim.rcpnt.claim_rcpnt_label			= "Claimant"
	
	Choose case (vistr_correspond_claim.language)
		case 'E'
			vistr_correspond_claim.language= 'English'
		case 'F'
			vistr_correspond_claim.language = 'French'
		case else
			vistr_correspond_claim.language = 'Other'
	End choose

	sin_no = vistr_correspond_claim.sin_no
	If not ISNULL (sin_no)   then		
		vistr_correspond_claim.sin_no	= mid(sin_no,1,3) + '-' + mid(sin_no,4,3) + '-' + mid(sin_no,7,3)
	End if	

	medicare_no = vistr_correspond_claim.medicare_no	
	If not ISNULL (medicare_no) then
	vistr_correspond_claim.medicare_no	= mid(medicare_no,1,3) + '-' + mid(medicare_no,4,3) + '-'&
											 + mid(medicare_no,7,3)

// **************************************************************************************************************************
// INSERT THE STR_CLAIM_RCPNT INFO INTO THE INDIVIDUALS LIST
	
	rc = dw_individuals.InsertRow(0)

	if rc < 0  then  	GOTO error_fnd

	dw_individuals.SetItem(rc,"default_address_flag","N")
	dw_individuals.SetItem(rc,"claim_no",vistr_correspond_claim.claim_no)
	dw_individuals.SetItem(rc,"correspond_recipient_type_cd","CLMT")
	dw_individuals.SetItem(rc,"correspond_recipient_subtyp_cd"," ")
	dw_individuals.SetItem(rc,"correspond_recipient_id", vistr_correspond_claim.rcpnt.claim_rcpnt_id)
	dw_individuals.SetItem(rc,"recipient_no",-1)
	dw_individuals.SetItem(rc,"claim_recipient_label","Claimant")
	dw_individuals.SetItem(rc,"address_location_code","C")
	dw_individuals.SetItem(rc,"card_file_flag","N")

	If dw_individuals.modifiedcount() <> 1 then GOTO error_fnd
	lb_status	= dw_individuals.GETITEMSTATUS(rc,0,Primary!)
	
	If lb_status <> NewModified! then
					MessageBox("SAVE ERROR", "An error occurred while attempting to the default claimant"&
					+"~n~r to your recipient list. The status indicates something other than NewModified"&
					+"~n~rPlease report this error to the helpdesk!",Stopsign!)
					Return(-1)
	End if

	SQLCA.nf_begin_transaction()
	
	urc = dw_individuals.Update()

	SQLCA.nf_handle_error("dw_individuals","w_correspond","on wf_set_default_claimant")


// *********************************************************************************************************************
// CREATE THE ADDRESS RECORD
	vls_name	= Trim(vistr_correspond_claim.first_name) + " " + vistr_correspond_claim.last_name

	INSERT into RECIPIENT_ADDRESS
		(correspond_recipient_id, name1, name2, address_line1, address_line2,address_line3, 
		city, province, postal_code, country, fax_no, email_id, correspond_recipient_type_cd,
		correspond_recipient_subtyp_cd, card_file_flag, active_flag)
	VALUES
		(:vistr_correspond_claim.rcpnt.claim_rcpnt_id, :vls_name, " ",
		:vistr_correspond_claim.address_line1, :vistr_correspond_claim.address_line2," ",:vistr_correspond_claim.city,:vistr_correspond_claim.province,
		:vistr_correspond_claim.postal_code,:vistr_correspond_claim.country," "," ", "CLMT", " ",
		"N", "Y")
	USING	SQLCA;
					
	SQLCA.nf_handle_error("Embedded SQL INSERT on RECIPIENT ADDRESS ","w_correspond","on wf_set_default_claimant")
	
	SQLCA.nf_commit_transaction()

	RETURN 0

	error_fnd:
		MessageBox("APPLICATION ERROR: 513-RECIPIENTS","THE DEFAULT CLAIMANT COULD NOT BE ADDED." &
				+ "~r~n~r~nPLEASE CONTACT YOUR SYSTEM ADMINISTRATOR",StopSign!)
	Return -1
END IF
end function

public function integer wf_set_default_provider (integer ai_nbr_doc_spv, integer ai_nbr_corr_spv);	// ************************************************************************************************
// AUTHOR:	MW George
// Modified:	94.10.20

// ************************************************************************************************
// THIS FUNCTION WILL SET UP A DEFAULT SERVICE PROVIDERS if they don't already exist
// AND IT WILL HAVE THE LABEL "SVPV"
//	Arguments:	ai_nbr_corr_spv is the number of Service Provider Recipients already in the claim
//					ai_nbr_doc_spv  is the number of Medical Aid Service Providers that are attached to the claim.					 
// ************************************************************************************************
// DECLARATIONS
int		li_rc,i
long		rc,  vll_rcpnt_id,vll_svpv_no, vll_found, ll_rows, ll_modrows,vll_err_found
string	vls_name,vls_svpv_code
dwitemSTATUS lb_status
SetPointer(HourGlass!)
// CASES:		0 		:	There are no Medical Service Providers already attached to the claim
//					Action:	Add everyone
//
//					>0		:	There are some Medical Service Providers already set up as Recipients
//					Action:	Check to see if any can be added.


N_OBJECTHELPER lnv_object_helper

f_populate_app_log(gs_appname,lnv_object_helper.get_app_component_type(THIS),lnv_object_helper.getpath(THIS),'wf_set_default_provider')


CHOOSE CASE ai_nbr_corr_spv
	CASE 0  
		
// **************************************************************************************************************************
			// Processing when no Service Providers have been set yet.
			//	1.Retreive the Serice Provder number from IMARA where Service Provider no > 0 and not null
			// 2.Find the Service Provider type code in Claim
			// 3.INSERT INFO INTO THE INDIVIDUALS LIST
// ****************************************************************************************************************************
	vll_err_found = 0		
	FOR i= 1 TO ai_nbr_doc_spv
		
			vll_found =-1
			vll_rcpnt_id =-1
					   
			rc = dw_individuals.InsertRow(0)
			IF rc <> -1 then
						vll_svpv_no=dw_get_doc_sender.GetItemNumber(i,"sender")
						vll_found =dw_get_med_service_providers.Retrieve(vll_svpv_no)
						li_rc= SQLCA.nf_handle_error("dw_get_doc_sender","w_correspond","on wf_default provider -retrive event")
						If vll_found < 1 THEN
							MessageBox( "Database Error" + string(SQLCA.SQLDBCODE),+  &
							"An error occurred while adding  Provider No  " +string(vll_svpv_no)&
							+ "~n~r PLEASE INFORM TECHNICAL SERVICES"  )
							vll_err_found = -1
						
							ELSEIF vll_found > 0 then
			 					vls_svpv_code=dw_get_med_service_providers.GetItemString(1,"provider_type_code")
								vls_name = dw_get_med_service_providers.GETITEMSTRING(1,"name")	
								vll_rcpnt_id = f_get_next_rcpnt_id()
						end if	
					
						IF vll_rcpnt_id > 0  and vll_found > 0 THEN
							/* Prior to this date the datawindow was updating i, which is an existing row  */
							dw_individuals.SetItem(rc,"default_address_flag","N")
							dw_individuals.SetItem(rc,"claim_no",vistr_correspond_claim.claim_no)
							dw_individuals.SetItem(rc,"correspond_recipient_type_cd","SVPV")
							dw_individuals.SetItem(rc,"correspond_recipient_subtyp_cd",vls_svpv_code)
							dw_individuals.SetItem(rc,"correspond_recipient_id",vll_rcpnt_id)
							dw_individuals.SetItem(rc,"recipient_no",vll_svpv_no)
							dw_individuals.SetItem(rc,"claim_recipient_label",vls_name )
							dw_individuals.SetItem(rc,"address_location_code","P")
							dw_individuals.SetItem(rc,"card_file_flag","N")
						end if

					ELSE 
						vll_err_found = -1
			end if
		NEXT

//  There are some Service Providers already setup

	CASE  is >0

		FOR i= 1 TO ai_nbr_doc_spv
			vll_found = 1
			vll_rcpnt_id = -1
			vll_svpv_no=dw_get_doc_sender.GetItemNumber(i,"sender")
			ll_rows = dw_get_med_service_providers.Retrieve(vll_svpv_no)
			li_rc = SQLCA.nf_handle_error("dw_get_med_service_providers","w_correspond","in retrive on wf_set_default_provider")	

			If ll_rows > 0 then
	 			vls_svpv_code=dw_get_med_service_providers.GetItemString(1,"provider_type_code")
				vls_name = dw_get_med_service_providers.GETITEMSTRING(1,"name")
			ELSE
				vll_err_found = -1 // Didn't find the service provider from IMARA in CLAIM_DB
			End If

		// See if the recipient is already set up
			vll_found =  dw_individuals.Find("recipient_no =" +string(vll_svpv_no), &
			1,dw_individuals.rowcount())
 
			If vll_found = 0 and ll_rows >0 then 	vll_rcpnt_id = f_get_next_rcpnt_id() else rc= -1
	
			If vll_rcpnt_id >0   THEN 	rc = dw_individuals.InsertRow(0)

			If rc <> -1 then
				dw_individuals.SetItem(rc,"default_address_flag","N")
				dw_individuals.SetItem(rc,"claim_no",vistr_correspond_claim.claim_no)
				dw_individuals.SetItem(rc,"correspond_recipient_type_cd","SVPV")
				dw_individuals.SetItem(rc,"correspond_recipient_subtyp_cd",vls_svpv_code)
				dw_individuals.SetItem(rc,"correspond_recipient_id",vll_rcpnt_id)
				dw_individuals.SetItem(rc,"recipient_no",vll_svpv_no)
				dw_individuals.SetItem(rc,"claim_recipient_label",vls_name)
				dw_individuals.SetItem(rc,"address_location_code","P")
				dw_individuals.SetItem(rc,"card_file_flag","N")													 			
			End if
			
		NEXT
		
	END CHOOSE

/* Do all updates, make sure every row is a new row, when done, test for fatal error on update
*/
		ll_modrows = dw_individuals.modifiedcount()
		long row = 0

		ll_rows = dw_individuals.RowCount( )

		DO WHILE ll_modrows <> 0 
			row = dw_individuals.GetNextModified(row, Primary!)
			IF row > 0 THEN 
				lb_status	= dw_individuals.GETITEMSTATUS(row,0,Primary!)
				If lb_status <> NewModified! then
					MessageBox("SAVE ERROR", "An error occurred while attempting to add this individual"&
					+"~n~r to your recipient list. The status indicates something other than NewModified"&
					+"~n~rPlease report this error to the helpdesk!",Stopsign!)
					Return(-1)
				End if
			ELSE
				ll_modrows = 0 // Gets me out of the loop
			END IF
		LOOP

		SQLCA.nf_begin_transaction()
		
		vll_err_found = dw_individuals.Update()	
		vll_err_found =  SQLCA.nf_handle_error("d_individuals","update","on wf_set_default_provider") 
		
   	if vll_err_found = 0		then
			SQLCA.nf_commit_transaction()
		ELSE
			SQLCA.nf_rollback_transaction()
		end if	
			
return vll_err_found;

end function

public function integer wf_update_claimant_address ();
LONG		rc, urc, dbrc, ll_rcpnt_id
STRING	ls_name,sin_no,medicare_no
datetime	vdt_birth_date 
STRING	ls_name1,ls_address_line1,ls_address_line2,ls_city,ls_province,ls_postal_code,ls_country
INT      li_role_count

N_OBJECTHELPER lnv_object_helper

f_populate_app_log(gs_appname,lnv_object_helper.get_app_component_type(THIS),lnv_object_helper.getpath(THIS),'wf_update_claimant_address')

//T031068 - must first determine if user is a read only role for database access - if so, we must leave this function, as it does updates at the end of it
if f_user_is_readonly() then return 0 

SetPointer(HourGlass!)
/*	NOTE: this routine was copied from the wf_set_default_claimant
	it is not the best way to do this but it will do for now
*/				

// PR1456 - S.Manzer Added Street Oct. 27, 2000 
/*	Retrieve the claimant information
*/
	SELECT	str(medicare_no,9),str(sin_no,9),language_code,last_name,given_names,address_line1,city,
				prov_state_code,country_code,telephone_no,
				address_line1,address_line2,birth_date,postal_code
 	INTO
				:vistr_correspond_claim.medicare_no,:vistr_correspond_claim.sin_no,
				:vistr_correspond_claim.language,:vistr_correspond_claim.last_name,:vistr_correspond_claim.first_name,
				:vistr_correspond_claim.street,
				:vistr_correspond_claim.city,:vistr_correspond_claim.province,:vistr_correspond_claim.country,
				:vistr_correspond_claim.claimant_phone_no,:vistr_correspond_claim.address_line1,
				:vistr_correspond_claim.address_line2,:vdt_birth_date,:vistr_correspond_claim.postal_code
	FROM		INDIVIDUAL
	WHERE		individual_no = :vistr_correspond_claim.individual_no
	USING 	SQLCA;
					
	SQLCA.nf_handle_error("SQL Select on Individual","w_correspond","on wf_set_default_claimant")
	
 	vistr_correspond_claim.birth_date = Date(vdt_birth_date)
 	Trim(vistr_correspond_claim.medicare_no)
	Trim(vistr_correspond_claim.sin_no)
	Trim(vistr_correspond_claim.last_name)
	
	CHOOSE CASE (vistr_correspond_claim.language)
		CASE 'E'
			vistr_correspond_claim.language= 'English'
		CASE 'F'
			vistr_correspond_claim.language = 'French'
		CASE ELSE
			vistr_correspond_claim.language = 'Other'
	END CHOOSE

	sin_no = vistr_correspond_claim.sin_no

	IF NOT IsNull(sin_no) THEN
		vistr_correspond_claim.sin_no	= mid(sin_no,1,3) + '-' + mid(sin_no,4,3) + '-' + mid(sin_no,7,3)
	END IF

	medicare_no = vistr_correspond_claim.medicare_no	
	IF NOT IsNull(medicare_no) THEN
		vistr_correspond_claim.medicare_no	= mid(medicare_no,1,3) + '-' + mid(medicare_no,4,3) + '-'&
											 + mid(medicare_no,7,3)
	END IF
	SQLCA.nf_handle_error("dw_individuals","w_correspond","on wf_set_default_claimant")

/*	Update address record
*/
	ls_name	= Trim(vistr_correspond_claim.first_name) + " " + vistr_correspond_claim.last_name

	SELECT 	name1,
				address_line1,
				address_line2,
				city,
				province,
				postal_code,
				country
	INTO		:ls_name1,
				:ls_address_line1,
				:ls_address_line2,
				:ls_city,
				:ls_province,
				:ls_postal_code,
				:ls_country
	FROM RECIPIENT_ADDRESS
  	WHERE 	correspond_recipient_id = :vistr_correspond_claim.rcpnt.claim_rcpnt_id
	AND  	correspond_recipient_type_cd = "CLMT"
	USING	SQLCA;
	
	// Expect 1 row to be selected
	SQLCA.nf_handle_error('w_correspond', 'Embedded SQL: SELECT RECIPIENT ADDRESS','wf_update_claimant_address',1)
	
	
	
	IF (UPPER(ls_name1) <> UPPER(ls_name) &
		OR UPPER(ls_address_line1) <> UPPER(vistr_correspond_claim.address_line1) &
		OR	UPPER(ls_address_line2) <> UPPER(vistr_correspond_claim.address_line2)	&
		OR	UPPER(ls_city) <> UPPER(vistr_correspond_claim.city) &
		OR	UPPER(ls_province) <> UPPER(vistr_correspond_claim.province) &
		OR	UPPER(ls_postal_code) <> UPPER(vistr_correspond_claim.postal_code) &
		OR	UPPER(ls_country) <> UPPER(vistr_correspond_claim.country)) THEN
		
		SQLCA.nf_begin_transaction()
		
		UPDATE RECIPIENT_ADDRESS
			SET 	name1 = UPPER(:ls_name), 
					address_line1 = UPPER(:vistr_correspond_claim.address_line1), 	
					address_line2 = UPPER(:vistr_correspond_claim.address_line2),	
					city = UPPER(:vistr_correspond_claim.city), 
					province = UPPER(:vistr_correspond_claim.province), 
					postal_code = UPPER(:vistr_correspond_claim.postal_code), 
					country = UPPER(:vistr_correspond_claim.country)
	
		 WHERE 	correspond_recipient_id = :vistr_correspond_claim.rcpnt.claim_rcpnt_id
			AND  	correspond_recipient_type_cd = "CLMT"
		USING	SQLCA;
		//fix up there					
		
		// Expect 1 row to be updated
		SQLCA.nf_handle_error('w_correspond', 'Embedded SQL: UPDATE RECIPIENT ADDRESS','wf_update_claimant_address',1)
				
		SQLCA.nf_commit_transaction()
	END IF
		
	RETURN 0


end function

public function long wf_set_default_employer ();// ************************************************************************************************
// AUTHOR:	Earl Assoon
// Modified:	97.07.07

// ************************************************************************************************
// THIS FUNCTION WILL SET UP A DEFAULT EMPLOYER RECIPIENT SINCE NONE CURRENTLY
// EXISTS AND IT WILL HAVE THE LABEL "Employer"
// ************************************************************************************************
// DECLARATIONS
string	label,label_char,old_label
int	label_start_pos,label_no,i,j,highest_label_no
long	rc, urc, dbrc, vll_rcpnt_id 
dwitemstatus lb_status

N_OBJECTHELPER lnv_object_helper

f_populate_app_log(gs_appname,lnv_object_helper.get_app_component_type(THIS),lnv_object_helper.getpath(THIS),'wf_set_default_employer')


SetPointer(HourGlass!)

IF ii_row_no > 0 then
	
	
	label	= dw_individuals.GetItemString(ii_row_no,"claim_recipient_label")
	label = Label + "$" //For Initial Employer label change	
	highest_label_no	=	0

	/* Change the old label to a unique label name */
	
	For i = 1 to ii_nbr_rows
		old_label =	Upper(dw_individuals.GetItemString(i,"claim_recipient_label"))			
		IF Not match(old_label,"EMPLOYER\$") then continue

		label_start_pos =	POS(old_label,"$",1)

		DO Until Label_start_pos = 0
			j = label_start_pos
			label_start_pos	= POS(old_label,"$",label_start_pos +1)
		Loop

		label_char = (Mid(old_label,j+1,1))
		label_no = INTEGER(label_char)
		If label_no > highest_label_no then highest_label_no = label_no
	next
	
	
	highest_label_no++
	label = Left(label,POS(label,"$",1))+String(highest_label_no)

	
	dw_individuals.SetItem(ii_row_no,"default_address_flag","N")
	dw_individuals.SetItem(ii_row_no,"claim_recipient_label",label)
	lb_status	= dw_individuals.GETITEMSTATUS(ii_row_no,0,Primary!)
	If lb_status <> DataModified! then
				MessageBox("UPDATE ERROR: 513-RECIPIENTS","THE  OLD DEFAULT EMPLOYER COULD NOT BE UPDATED." &
			+ "~r~n~r~nPLEASE CONTACT YOUR SYSTEM ADMINISTRATOR",StopSign!)
		RETURN  -1
	END IF
END IF	

// Get next available recipient id
vll_rcpnt_id = f_get_next_rcpnt_id()

If vll_rcpnt_id < 0 THEN
	MessageBOX(this.title,"UNABLE TO ASSIGN ADDRESS ID " +String(vll_rcpnt_id)&
	+"~n~r PLEASE CONTACT THE HELPDESK WITH THIS MESSAGE",STOPSIGN!)
	Return -1 
End If 

vistr_correspond_claim.rcpnt.claim_rcpnt_id = vll_rcpnt_id 

// Insert employer entry into recipient list
rc = dw_individuals.InsertRow(0)
if rc <> -1 then
	dw_individuals.SetItem(rc,"default_address_flag","Y")
	dw_individuals.SetItem(rc,"claim_no",vistr_correspond_claim.claim_no)
	dw_individuals.SetItem(rc,"correspond_recipient_type_cd","EMPL")
	dw_individuals.SetItem(rc,"correspond_recipient_subtyp_cd","L")
	dw_individuals.SetItem(rc,"correspond_recipient_id",vistr_correspond_claim.rcpnt.claim_rcpnt_id)
	dw_individuals.SetItem(rc,"recipient_no",vistr_correspond_claim.employer_no	)
	dw_individuals.SetItem(rc,"claim_recipient_label","Employer")
	dw_individuals.SetItem(rc,"address_location_code","E")
	dw_individuals.SetItem(rc,"card_file_flag","N")
	lb_status	= dw_individuals.GETITEMSTATUS(rc,0,Primary!)
	If lb_status <> NewModified! then
				MessageBox("UPDATE ERROR: 513-RECIPIENTS","THE  NEW DEFAULT EMPLOYER COULD NOT BE ADDED." &
			+ "~r~n~r~nPLEASE CONTACT YOUR SYSTEM ADMINISTRATOR",StopSign!)
		RETURN  -1
	END IF	


	SQLCA.nf_begin_transaction()

	urc = dw_individuals.Update()
	vii_return_code = SQLCA.nf_handle_error("dw_individuals","w_correspond","on wf_establish_default_employer")

	If vii_return_code = 0 then
		SQLCA.nf_commit_transaction()
	else
		SQLCA.nf_rollback_transaction()
		
		MessageBox("UPDATE ERROR: 513-RECIPIENTS","THE DEFAULT EMPLOYER COULD NOT BE ADDED." &
			+ "~r~n~r~nPLEASE CONTACT YOUR SYSTEM ADMINISTRATOR",StopSign!)
		RETURN  -1
	end if
	
else
	MessageBox("INSERT ERROR: 513-RECIPIENTS","THE DEFAULT EMPLOYER COULD NOT BE ADDED." &
			+ "~r~n~r~nPLEASE CONTACT YOUR SYSTEM ADMINISTRATOR",StopSign!)
	RETURN  -1
End if

RETURN 0


end function

public subroutine wf_set_filter (string expression);string vls_expression, vls_filt
long	vll_found_row

dw_template_list.SetFilter(expression) 
dw_template_list.Filter()
// Reset the filter
if Len(vis_filt) > 0 then							// Do case-insensitive search
	vls_expression = "Mid(Upper(template_code), 1, " + String(Len(vis_filt)) + ") = " + "'" + Upper(vis_filt) + "'" 
	vll_found_row = dw_template_list.Find(vls_expression, 1, dw_template_list.RowCount())
	if vll_found_row > 0 then 	dw_template_list.ScrollToRow(vll_found_row) // Triggers rowfocus changed event
END IF
end subroutine

public function boolean wf_destroyprocess (unsignedlong aul_processid);uint lui_exitcode
boolean  lb_return
ulong hProcess
any external_return
hprocess = OpenProcess(process_all_access,false,aul_processid)
if hprocess < 1 then
	MessageBox('Process Termination',wf_error(GetLastError()),StopSign!)
	return lb_return
end if
external_return = GetExitCodeProcess(hprocess, lui_exitcode)
if TerminateProcess(hprocess, lui_exitcode) then
	 return true
else
	MessageBox('Process Termination',wf_error(GetLastError()),StopSign!)
end if
return lb_return
end function

public function string wf_error (integer ai_error);choose case ai_error
case 0
 Return "The operation completed successfully." //  ERROR_SUCCESS
case 1
 Return "Incorrect function." //  ERROR_INVALID_FUNCTION
case 2
 Return "The system cannot find the file specified." //ERROR_FILE_NOT_FOUND
case 3
 Return "The system cannot find the path specified." // ERROR_PATH_NOT_FOUND
case 4
 Return "The system cannot open the file." //  ERROR_TOO_MANY_OPEN_FILES
case 5
 Return "Access is denied." //  ERROR_ACCESS_DENIED
case 6
 Return "The handle is invalid." //  ERROR_INVALID_HANDLE
case 7
 Return "The storage control blocks were destroyed." //  ERROR_ARENA_TRASHED
case 8
 Return "Not enough storage is available to process this command." // ERROR_NOT_ENOUGH_MEMORY
case 9
 Return "The storage control block address is invalid." // ERROR_INVALID_BLOCK
case 10
 Return "The environment is incorrect." //  ERROR_BAD_ENVIRONMENT
case 11
 Return "An attempt was made to load a program with an incorrect format." // ERROR_BAD_FORMAT
case 12
 Return "The access code is invalid." //  ERROR_INVALID_ACCESS
case 13
 Return "The data is invalid." //  ERROR_INVALID_DATA
case 14
 Return "Not enough storage is available to complete this operation." //ERROR_OUTOFMEMORY
case 15
 Return " The system cannot find the drive specified." //ERROR_INVALID_DRIVE
case 16
 Return "The directory cannot be removed." //  ERROR_CURRENT_DIRECTORY
end choose
Return "Unknown Error " + String(ai_error) + "." //  ERROR_CURRENT_DIRECTORY
end function

public subroutine wf_2000_close_winword ();// clean up any OLE Word 2000 processes in Windows 2000
LONG						ll_rows
ULONG					lul_hSnapshot, lul_th32ProcessID, lul_parent_proc[], lul_word_child_proc
INTEGER					li_counter = 1, li_svchost_rows, li_counter2
PROCESSENTRY32		lpe_processentry[]
STRING					ls_exe
BOOLEAN				lb_return

//when using the TH32CS_SNAPPROCESS flag, the lul_th32ProcessID argument is ignored

SetNull (lul_th32ProcessID)
lul_hSnapshot = CreateToolhelp32Snapshot (TH32CS_SNAPPROCESS,lul_th32ProcessID)

// load structure lpe_processentry with values
if lul_hSnapshot > 0 then
	lpe_processentry[li_counter].dwSize = 296
	if Process32First (lul_hSnapshot, lpe_processentry[li_counter]) then
		do
			li_counter = li_counter + 1
			lpe_processentry[li_counter].dwSize = 296
		loop while Process32Next (lul_hSnapshot, lpe_processentry[li_counter])
	end if
	CloseHandle (lul_hSnapshot)
end if

// check structure for 'svchost.exe'
ll_rows = UpperBound(lpe_processentry)
FOR li_counter = 1 to ll_rows
	ls_exe = Lower(lpe_processentry[li_counter].szexefile)
	IF ls_exe = 'svchost.exe' THEN  // a process by this name parents all winwords created using OLE in correspondence
		li_svchost_rows = li_svchost_rows + 1
		lul_parent_proc[li_svchost_rows] = lpe_processentry[li_counter].th32processid
	END IF
	lbx_processes.AddItem(lpe_processentry[li_counter].szexefile + '~tprocessID: ' + string(lpe_processentry[li_counter].th32processid) + '~tParent processID: ' + string(lpe_processentry[li_counter].th32parentprocessid))
NEXT

// get rid of winword.exes that were parented by svchost.exe
ll_rows = UpperBound(lpe_processentry)
FOR li_counter = 1 to ll_rows
	ls_exe = Lower(lpe_processentry[li_counter].szexefile)
	IF ls_exe = 'winword.exe' and lpe_processentry[li_counter].th32processid <> iul_processid THEN
		FOR li_counter2 = 1 to li_svchost_rows
			IF lpe_processentry[li_counter].th32parentprocessid = lul_parent_proc[li_counter2] THEN
				// get rid of process
				lul_word_child_proc = lpe_processentry[li_counter].th32processid
				lb_return = wf_destroyprocess(lul_word_child_proc)
				iul_processid = lul_word_child_proc
			END IF
		NEXT
	END IF
NEXT
end subroutine

public subroutine wf_nt4_close_winword ();/** NT 4 version **/
// This function will shut down all Word processes.
// It searches structure ModuleEntry for those with winword.exe as part of file path
// and closes them.

ProcessEntry lpe_Process // Window structure (array of 500 ulong)...arbitrary value !
ModuleEntry lme_Module // Window structure (array of 100 long)... idem
long ll_proc_size = 2000 // Size of Process
long ll_mod_size = 400 // Size of Module
long ll_proc_needed // returned size of Process
long ll_mod_needed // returned size of Module
long ll_mod_name // size of returned name of the module
long ll_counter_nt4 // for looping through processes
ulong lul_proc_handle // Handle of the process
boolean lb_proc, lb_mod // return codes for EnumProcesses and EnumProcessModules
string ls_mod_name // Name of the module
string ls_mod_base_name
long ll_mod_base_name
uint	lui_exitcode
any la_external_return

lb_proc = EnumProcesses(lpe_Process, ll_proc_size, ll_proc_needed)
if lb_proc then
	for ll_counter_nt4 = 1 to integer(ll_proc_needed/ 4)
		lul_proc_Handle = OpenProcess(PROCESS_ALL_ACCESS,false,lpe_Process.lpIdProcess[ll_counter_nt4])
		lb_mod = EnumProcessModules(lul_proc_handle, lme_Module, ll_mod_size, ll_mod_needed)
		if ll_mod_needed >= 4 then
			ls_mod_name = space(254)
			ll_mod_Name=GetModuleFileNameExA(lul_proc_handle, lme_Module.lpidmodule[1], ls_mod_name ,254)
			if ll_mod_Name > 0 then
				if Pos(Lower(String(ls_mod_name)), 'winword.exe') <> 0 then
					la_external_return = GetExitCodeProcess(lul_proc_handle, lui_exitcode)
					if TerminateProcess(lul_proc_handle, lui_exitcode) then
						 //OK
					else
						MessageBox('Process Termination',wf_error(GetLastError()),StopSign!)
					end if
				end if
			end if
		end if
		CloseHandle(lul_proc_Handle)
	next
end if


end subroutine

public subroutine wf_reset_buttonss (ref datawindow adw_dwname);string	dw_name
dw_name =adw_dwname.ClassName()
Choose Case dw_name
	Case ("dw_template_list")
		ib_datawin1= True
		dw_generated_list.SelectRow(0,False)	
		If adw_dwname.Rowcount() > 0 then
				If vil_template_list_rowno < 1 then
					 vil_template_list_rowno = 1
					adw_dwname.SelectRow(1,True) 
//					sle_find.text = adw_dwname.GetItemString(1,"template_code")
//					vis_filt	=	sle_find.text
				Else
					adw_dwname.Selectrow(vil_template_list_rowno,True)
					adw_dwname.ScrollToRow(vil_template_list_rowno)
//					sle_find.text = adw_dwname.GetItemString(vil_template_list_rowno,"template_code") 
//					vis_filt	=	sle_find.text
				END if
				cb_compose.enabled 	= True
				cb_preview.enabled	= True
				rb_english.enabled	=	True
				rb_french.enabled		=	True
				sle_find.enabled		=	True
		ELSE
				cb_compose.enabled 	= False
				cb_preview.enabled	= False
				rb_english.enabled	=	False
				rb_french.enabled		=	False
				sle_find.enabled		=	False
		END IF
		cb_send.enabled		=	False
		cb_edit.enabled		=	False
		cb_delete.enabled		=	False
		cb_edit_type.enabled =  FALSE

	Case ("dw_generated_list")
		ib_datawin1= False
		
		cb_compose.enabled 	= 	False
		cb_preview.enabled	= 	False
		rb_english.enabled	=	False
		rb_french.enabled		=	False
		sle_find.enabled		=	False
		dw_template_list.Selectrow(0,False)
		If adw_dwname.Rowcount() > 0 then
			If vil_generated_list_rowno < 1 then
					 vil_generated_list_rowno = 1
					adw_dwname.SelectRow(1,True)
				Else
					adw_dwname.Selectrow(vil_generated_list_rowno,True)
					adw_dwname.ScrollToRow(vil_generated_list_rowno)
				END if
//			vil_generated_list_rowno = 1
//			adw_dwname.Selectrow(1,True)
			cb_send.enabled		=	True
			cb_edit.enabled		=	True
			cb_delete.enabled		=	True
			cb_edit_type.enabled =  TRUE
		ELSE
			cb_send.enabled		=	False
			cb_edit.enabled		=	False
			cb_delete.enabled		=	False
			cb_edit_type.enabled =  FALSE
		END IF
END CHOOSE
end subroutine

public subroutine wf_reset_buttons (boolean status, string range);IF range = "all" THEN
	cb_edit.enabled = status
	cb_send.enabled = status
	cb_delete.enabled = status
	cb_compose.enabled = status
	cb_preview.enabled = status
	cb_edit_type.enabled = status
//	sle_find.displayonly=True  //added 05/14/97
ELSE
	IF dw_generated_list.Getselectedrow(0) > 0 THEN
		cb_edit.enabled = status
		cb_send.enabled = status
		cb_delete.enabled = status
		cb_edit_type.enabled = status
	ELSE
		IF dw_template_list.Getselectedrow(0) > 0 THEN
			cb_compose.enabled = status
			cb_preview.enabled = status
		END IF
	END IF
END IF

cb_recipients.enabled = status
cb_close.enabled = status

dw_generated_list.enabled = status
dw_template_list.enabled = status

end subroutine

public function boolean wf_check_individual_deceased (long al_individual_no);/* BR The annuity payout letter should be approriate, if the individual is deceased. Refer to Rationale
		Rational and Message
		The individual is deceased so please ensure the annuity payout letter is correctly 
		addressed to the appropriate recipient(s). For example, surviving spouse and/or dependent(s). 
		
		SQL returns 1900-01-01		
*/
DATETIME	ldtm_death_date

SELECT	death_date
INTO		:ldtm_death_date
FROM		INDIVIDUAL a				
WHERE	individual_no			= :al_individual_no
USING 	SQLCA;
	
SQLCA.nf_handle_error('w_correspond', 'wf_check_individual_deceased()', 'SELECT	death_date')

//check for crappy sql null return
IF	String(ldtm_death_date,'YYYY-MM-DD') = "1900-01-01" THEN RETURN FALSE// NOT DECEASED

IF NOT IsNull(ldtm_death_date) THEN RETURN TRUE //DECEASED -- any date but the 1900-01-01
	
RETURN FALSE//not deceased

end function

public function boolean wf_check_payout_letter_in_claim_ss (long al_claim_no);/* The annuity payout letter should not be created for the surviving spouse, if an annuity payout letter was previously sent for the claim. 
     Refer to Rationale
	Rationale
	If there is an annuity payout letter in the claim that has been SENT, then a warning is provided, regardless 
	for which annuity account it was originally created. The DOCUMENT_INDEX table only references non-archived documents and has a sent_flag.
	Message – An Annuity Payout letter exists in this claim. Please review to determine if another one should be generated 
	(i.e.the existing letter is for a previous payout). Continue or Cancel 
	
	-- Correspondence_Status
	
	RETURN TRUE 		-- PAYOUT LETTER ALREADY EXISTS IN CLAIM
	RETURN FALSE 	-- NO PAYOUT LETTER FOUND
	
*/

LONG			ll_count

SELECT	Count(*)
INTO		:ll_count
FROM		CORRESPONDENCE 
WHERE	claim_no 					= :al_claim_no
AND 		template_code 				IN ('ANNPAY01','ANNPAY02') 
AND		correspond_status_code  = 'S'
USING 	SQLCA;

SQLCA.nf_handle_error('w_correspond', 'wf_check_already_payout_letter_in_claim_ss()', 'select count(*) from CORRESPONDENCE...')

IF ISNULL(ll_count) THEN ll_count = 0

IF ll_count > 0 THEN RETURN TRUE

RETURN FALSE
end function

public function u_ds wf_check_payout_letter_not_sent_iw (long al_individual_no);/* The annuity payout letter should not be created for the injured worker, if there is an annuity payout letter 
    that has not been sent in any claims in which the individual is the active claimant. Refer to Rationale
	Rationale
	If  there is an annuity payout letter in any of the claim file(s) for the injured worker that has a status other than SENT, 
	then provide a warning. The DOCUMENT_INDEX table only references non-archived documents and has a sent_flag.
	Is it only as ‘active’ claimant?Yes 
	Provide list of claim(s)
	Message An Annuity Payout letter has been generated but not yet sent in the following claim files: 
	List claims. Continue or Cancel		
*/

DATETIME			ldtm_annuity_end_date
U_DS					lds_check


lds_check = CREATE u_ds
lds_check.dataobject = 'd_annuities_payout_letter_not_sent'
lds_check.SetTransObject(SQLCA)


lds_check.retrieve(al_individual_no)
SQLCA.nf_handle_error('w_correspond', 'wf_check_Payout_letter_not_sent_iw()', 'select * from lds_check...')

RETURN lds_check
end function

public function any open_word_file (string as_physical_file_path, string as_claim, string as_action);long		ll_row_nbr, vll_rc, ll_handle, ll_rtn, ll_claim, ll_action
w_edit lw_this
Window lw_frame

this.title	= as_action +"Claim "+" (#"+ as_claim +")"

ioo_word = Create u_word  

IF NOT FileExists(as_physical_file_path) THEN
	   MessageBox(title,"Unable to locate file  - " + as_physical_file_path + &
		+ "~n~r~n~rPlease select a different document or Contact Helpdesk for assistance",Information!)
	Return -1
END IF

// check if doc is already open
IF f_check_word_doc_open(as_physical_file_path,this) = -99 THEN
	//close(this)
	return -1
end if

// Connect to Word
vll_rc = ioo_word.uf_connect()
IF vll_rc < 0 THEN
	Return -1
END IF

// open word as not read-only and make it visible
vll_rc = ioo_word.uf_file_open(as_physical_file_path, false, true, 0)
IF vll_rc = OLE_ERROR THEN
	f_populate_ole_error('w_edit','w_edit','open','OLE',48,'','')
	Return -1
ELSEIF vll_rc < 0 THEN
	MessageBox("Word Error","Could not open file.",Information!)
	Return -1
END IF

IF IsValid(w_correspond) THEN
	lw_frame = w_correspond.ParentWindow().ParentWindow()  // w_frame
	THIS.x = lw_frame.x + (lw_frame.width  - THIS.Width)/2
	THIS.y = lw_frame.y + (lw_frame.height - THIS.Height)/2
END IF
	
return 0

end function

public function u_ds wf_check_already_payout_letter_in_claim (long al_individual_no);//BR
/* The annuity payout letter should not be created for the injured worker, if an annuity payout letter was previously sent for 
	any claims in which the individual is the active claimant. Refer to Rationale
	If there is an annuity payout letter in the claim(s) that has/have been SENT, then a warning is provided, regardless for which 
	annuity account it was originally created. 
	The DOCUMENT_INDEX table only references non-archived documents and has a sent_flag.

	Should a list of claims involved be provided in the warning message? Yes
	Is it only as ‘active’ claimant?Yes
	Message An Annuity Payout letter exists in one or more claims for the injured worker. Please review to determine
	if another one should be generated (i.e.the existing letter is for a previous payout). Continue or Cancel
	
	RETURN lds_check - handled outside of call
*/
U_DS					lds_check

lds_check = CREATE u_ds
lds_check.dataobject = 'd_check_already_payout_letter_in_claim'
lds_check.SetTransObject(SQLCA)

lds_check.retrieve(al_individual_no)
SQLCA.nf_handle_error('w_correspond', 'wf_check_already_payout_letter_in_claim()', 'lds_check.retrieve(al_individual_no)')

RETURN lds_check
end function

public function boolean wf_check_future_annuity_end_date (long al_individual_no);//BR
/* The annuity payout letter should not be created for the individual, if the annuity eligibility end date is in the future. Refer to Rationale
	Rationale
	Message The individual’s annuity eligibliity end date is in the future, the payout letter should not be generated. Cancel or Continue
	
	The dates should be there - they are there or they are NULL	
*/

DATE			ldt_annuity_end_date


SELECT 	b.annuity_end_date
INTO 		:ldt_annuity_end_date
FROM 	ANNUITY_ACCOUNT a, ANNUITY_ELIGIBILITY b, CLAIM_PARTICIPANT c
WHERE 	a.annuity_account_no 				= b.annuity_account_no
AND  		a.individual_no 							= c.individual_no
AND       	a.individual_no 							= :al_individual_no
AND  		a.claim_role_code 					= c.claim_role_code 
//AND 	b.annuity_eligibility_status_code 	= 'A'
USING SQLCA;
		
SQLCA.nf_handle_error('w_correspond', 'wf_check_future_annuity_end_date()', 'select annuity_end_date FROM ANNUITY_ELIGIBLITY...')

IF ldt_annuity_end_date > DATE(f_server_datetime()) THEN RETURN TRUE

RETURN FALSE
end function

public function boolean wf_check_individual_eligible_for_annuity (long al_individual_no);//BR
/* The annuity payout letter should not be created for the individual, if the individual is not eligible for annuity benefits. Refer to Rationale
		Rationale
	Message This individual is not eligible for annuity benefits, the payout letter should not be generated. Cancel or Continue

	RETURN FALSE -- NOT ELIGIBLE
	RETURN TRUE  -- ELIGIBLE
*/

LONG			li_active_annuity_count


SELECT	Count(*)
INTO		:li_active_annuity_count
FROM		ANNUITY_ELIGIBILITY	a
JOIN		ANNUITY_ACCOUNT	b ON a.annuity_account_no = b.annuity_account_no
WHERE	b.individual_no 							      = :al_individual_no
//AND		a.annuity_eligibility_status_code 	      = 'I'
AND		a.annuity_eligibility_status_code 	      = 'A'
USING 	SQLCA;
	
SQLCA.nf_handle_error('w_correspond', 'wf_check_individual_eligible_for_annuity()', 'SELECT Count(*)')
		
IF li_active_annuity_count = 0 THEN RETURN FALSE// not eligible

RETURN TRUE//eligible
end function

public function boolean wf_check_annuity_being_confirmed (long al_individual_no);//BR
/* The annuity payout letter should not be created for the individual, if the individual’s annuity elgibility is in the process of
     being confirmed Refer to Rationale
	Rationale
	If the individual’s annuity eligibility is in the process of being confirmed (i.e. there is a pending annuity eligibility for the annuity account), 
	then an annuity payout letter is not appropriate at this time.
	Message The Individual has a Confirm Annuity Eligibility checklist in progress. A payout letter should not be generated until the 
	annuity eligibility has been confirmed. Cancel or Continue
	
	RETURN TRUE 		-- IS IN PROCESS OF BEING CONFIRMED
	RETURN FALSE 	-- NOT IN THE PROCESS OF BEING CONFIRMED
*/

LONG		li_annuity_count


SELECT	Count(*)
INTO		:li_annuity_count
FROM		ANNUITY_ELIGIBILITY	a
JOIN		ANNUITY_ACCOUNT	b ON a.annuity_account_no = b.annuity_account_no
WHERE	b.individual_no 							= :al_individual_no
//AND		b.claim_role_code					  	= :as_claim_role_code
AND		a.annuity_eligibility_status_code 	= 'P'
USING SQLCA;

SQLCA.nf_handle_error('w_correspond', 'nf_check_annuity_being_confirmed()', 'SELECT Count(*) INTO :li_annuity_count.')

IF li_annuity_count > 0 THEN RETURN TRUE

RETURN FALSE
end function

public function boolean wf_check_payout_letter_not_sent_ss (long al_individual_no);/* The annuity payout letter should not be created for the surviving spouse, 
	if there is an annuity payout letter in the claim file that has not been sent. Refer to Rationale
	Rationale
	If  there is an annuity payout letter in the claim file that has not been SENT, then provide a warning. 
	The DOCUMENT_INDEX table only references non-archived documents and has a sent_flag.
	Message An Annuity payout letter has been generated but not yet sent. Continue or Cancel
		
	RETURN 
		TRUE 		- Payout Letter Sent
		FALSE 	- Payout Letter Not Sent
*/
LONG					ll_count

SELECT	Count(*) 
INTO		:ll_count
FROM 	CORRESPONDENCE a 
	join CLAIM_PARTICIPANT b ON a.claim_no = b.claim_no 
AND 		b.individual_no 				= :al_individual_no 
AND 		b.claim_role_code 		= 'SS'
AND 		template_code 				IN ('ANNPAY01','ANNPAY02') 
AND        correspond_status_code <> 'S'
USING 	SQLCA;


IF ll_count > 0 THEN RETURN TRUE

SQLCA.nf_handle_error('w_correspond', 'wf_check_Payout_letter_not_sent_ss()', 'select count(*) from CORRESPONDENCE')

RETURN FALSE
end function

public function boolean wf_check_no_calculation_for_payout (long al_individual_no);/* The annuity payout letter should not be created for the individual, if there is not an annuity calculation for payout purposes 
    for the individual’s annuity account (i.e. annuity calculation reason of Payout - Deceased Prior to Age 65 or Payout - Attained Age 65 ).
	Refer to Rationale
	Rationale
	The majority of annuity payouts will require an annuity calculation that will be done via the Calculate Annuity module. 
	There are some cases where it is not possible to do a calculation via the Calculate Annuity module, as follows, 
	in which case, a warning will be provided anyway:
	Ø	Some pre-1993 payouts for injured workers 
	Ø	Some retroactive post-1992 payouts for injured workers where the individual was paid out prior to the use/implementation 
	of the Calculate Annuity module
	Ø	Some retroactive payouts for surviving spouses.

	Message An Annuity Calculation has not been performed for the reason of Payout  Continue or Cancel
	
	RETURN TRUE 		-- THERE IS A CALCULATION
				FALSE 	-- THERE ISN'T A CALCULATION
*/
LONG					ll_claim_count


SELECT 		count(a.claim_no )
INTO			:ll_claim_count
FROM 		ANNUITY_ACCOUNT a
	join ANNUITY_CALC_ACCOUNT_HEADER b 
ON 			a.annuity_account_no = b.annuity_account_no
WHERE 		annuity_calc_reason_code IN ('04','05')
AND 			a.individual_no = :al_individual_no
GROUP BY 	a.claim_no
USING SQLCA;

SQLCA.nf_handle_error('w_correspond', 'wf_check_no_calculation_for_payout()', 'SELECT	count(a.claim_no) FROM ANNUITY_ACCOUNT...')

IF ll_claim_count > 0 THEN RETURN TRUE

RETURN FALSE
end function

public function u_ds wf_check_payout_letter_sent_iw (long al_individual_no);//BR
/* The annuity payout letter should not be created for the injured worker, if an annuity payout letter was previously sent for 
	any claims in which the individual is the active claimant. Refer to Rationale
	If there is an annuity payout letter in the claim(s) that has/have been SENT, then a warning is provided, regardless for which 
	annuity account it was originally created. 
	The DOCUMENT_INDEX table only references non-archived documents and has a sent_flag.

	Should a list of claims involved be provided in the warning message? Yes
	Is it only as ‘active’ claimant?Yes
	Message An Annuity Payout letter exists in one or more claims for the injured worker. Please review to determine
	if another one should be generated (i.e.the existing letter is for a previous payout). Continue or Cancel
	
	RETURN lds_check - handled outside of call
*/
U_DS					lds_check

lds_check = CREATE u_ds
lds_check.dataobject = 'd_check_payout_letter_already_sent'
lds_check.SetTransObject(SQLCA)

lds_check.retrieve(al_individual_no)
SQLCA.nf_handle_error('w_correspond', 'wf_check_already_payout_letter_in_claim()', 'lds_check.retrieve(al_individual_no)')

RETURN lds_check
end function

public function integer wf_check_for_surviving_spouse (long al_claim_no);/*	On composition of an Annuity Payout letter within the Correspondence module, 
	it must be determined which annuity account is to be used in creating the annuity payout letter. 
		
	Only the injured worker and surviving spouse who are involved in the claim that is selected 
	on the Workbench are available for selection. For example, if the injured worker is also a surviving spouse on a different claim, 
	s/he is not considered while in the correspondence module for the selected claim. The other claim would have 
	to be selected and correspondence created under that claim.
	If there is only an annuity account for the injured worker, then it will be the injured worker’s annuity account. 
	
	If there is only an annuity account for the surviving spouse, then it will be the surviving spouse’s annuity account. 
	If there are annuity accounts for both the injured worker and surviving spouse, and only one of them has reached 
	the annuity eligibility end date (ex. injured worker deceased prior to age 65), then the annuity account for that individual must be used. 
	If both the injured worker and surviving spouse have annuity accounts and they have both reached their annuity eligibility end dates, 
	then the annuity account must be selected from a list.  
	
	RETURN 0 -- no eligibility accounts for either the claimant or surviving spouse for this claim, or they both have accounts but neither has reached the end dates
	RETURN 1 - NORMAL CASE USE claim_role_code 'C'
	RETURN 2 - Surviving Spouse - use 'SS'
	RETURN 3 - BOTH, user to pick from list
	RETURN -1 - Error condition
	
*/

U_DS					lds_check
INTEGER				li_rowcount, li_counter,li_ss_counter, li_iw_counter, li_filtered
STRING				ls_role_code
DATETIME			ldtm_annuity_end


lds_check 				= CREATE u_ds
lds_check.dataobject 	= 'd_claims_individuals_for_annuities'
lds_check.SetTransObject(SQLCA)

//retrieve the datawindow with all of our annuitiy information - including SS
li_rowcount = lds_check.retrieve(al_claim_no)
SQLCA.nf_handle_error("w_correspond","wf_check_for_surviving_spouse()","lds_check.retrieve(al_claim_no)")

//If there is no annuity accounts for either the claimant or surviving spouse, return a 0
IF li_rowcount = 0 THEN RETURN 0

li_ss_counter 	= 0
li_iw_counter 	= 0

FOR li_counter = 1 TO li_rowcount
	ls_role_code = lds_check.getitemstring(li_counter,'claim_role_code')
	
	IF ls_role_code = 'SS'  	THEN li_ss_counter ++
	IF ls_role_code = 'C'  	THEN li_iw_counter ++
NEXT


//If there is only an annuity account(s) for the injured worker, then it will be the injured worker’s annuity account. 
IF li_iw_counter >= 1 AND li_ss_counter = 0 THEN RETURN 1

//If there is only an annuity account(s) for the surviving spouse, then it will be the surviving spouse’s annuity account. 
IF li_iw_counter = 0 AND li_ss_counter >= 1 THEN RETURN 2

// both have annuity an account(s)
IF li_iw_counter >= 1 AND li_ss_counter >= 1 THEN 
	
		li_ss_counter 	= 0
		li_iw_counter 	= 0

		FOR li_counter = 1 TO li_rowcount
			ls_role_code           	= lds_check.getitemstring(li_counter,'claim_role_code')
			ldtm_annuity_end   	= lds_check.getitemdatetime(li_counter,'annuity_end_date')
			
			IF ls_role_code = 'SS' 	AND ldtm_annuity_end < f_server_datetime()  THEN li_ss_counter ++
			IF ls_role_code = 'C' 	AND ldtm_annuity_end < f_server_datetime()  THEN li_iw_counter ++
		NEXT
	
		/*If there are annuity accounts for both the injured worker and surviving spouse, and only one of them has reached 
			the annuity eligibility end date (ex. injured worker deceased prior to age 65), then the annuity account for that individual must be used. 
		*/
		IF li_iw_counter >= 1 AND li_ss_counter = 0 THEN RETURN 1
		
		//If there is only an annuity account for the surviving spouse, then it will be the surviving spouse’s annuity account. 
		IF li_iw_counter = 0 AND li_ss_counter >= 1 THEN RETURN 2
		
		/* If both the injured worker and surviving spouse have annuity accounts and they have both reached their annuity eligibility end dates, 
			 then the annuity account must be selected from a list.  
		*/
		IF li_iw_counter >= 1 AND li_ss_counter >= 1  THEN RETURN 3
		
		
		/* If both the injured worker and surviving spouse have annuity accounts but neither have reached their annuity eligibility end dates, 
			 then default to te injured worker.
		*/
		IF li_iw_counter = 0 AND li_ss_counter = 0  THEN RETURN 0
				
END IF 

//If we get this far, there must be a problem
Return -1


end function

event open;call super::open;// ******************************************************************************************************************************
// AUTHOR:	MW George
// Modified:	94.10.24

// ******************************************************************************************************************************
// DECLARATIONS
//
long		rc, total_templates, total_generated, vl_pos, vll_value, vll_usage,l_total_templates,vli_nbr_svpv,urc,&
			vll_nbr_indiv, vll_recipient_id
string	full_name, medicare_no, language, sin_no, phone_no, employer_name, vls_word_path, vls_temp

uint		vli_handle
boolean	vlb_file_exists
string	vls_file_name, word_command, vls_name
/* HOUSEKEEPING/
 Get active sheet, in order to obtain the basic claim information
*/
	viw_active_sheet = w_frame.GetActiveSheet()

	If NOT IsValid(viw_active_sheet)  then
		MessageBox("Correspondence","Unable to determine active sheet. You may be low on resources",StopSign!)
		Close(this)
		Return
	End If
	// Establish Database Connections
	dw_individuals.SetTransObject(SQLCA)
	dw_get_address.SetTransObject(SQLCA)
	dw_get_doc_sender.SetTransObject(IMAGETRANS)
	dw_get_med_service_providers.SETTRANSOBJECT(SQLCA)
	SetPointer(HourGlass!)

// ******************************************************************************************************************************
// RETRIEVE THE LIST OF EXISTING DOCUMENT TEMPLATES FROM THE CORRESPONDENCE
// TEMPLATE LIST.

	rc = dw_template_list.SetTransObject(SQLCA)
	IF rc < 0 THEN
		Error.line = 36
		Error.object = "Datawindwow dw_template_list"
		Error.objectevent = "Settransobject" 
		Error.Text = "THE LIST OF CORRESPONDENCE TEMPLATES COULD NOT BE FOUND"
		SignalError()
	END IF
		
	rb_english.checked = True
	rc = dw_template_list.Retrieve()
	vii_return_code = SQLCA.nf_handle_error("dw_template_list","w_correspond","on open event")
	this.parentwindow().SetMicroHelp("Retrieving list of template completed...")
	IF rc = 0 then
			MessageBox("CORRESPOND ERROR: 511-001","THE TEMPLATE LIST IS EMPTY." &
					+"~n~r~n~rPLEASE CONTACT YOUR SYSTEM ADMINISTRATOR.",StopSign!)
		ELSE

		// Filter out french templates
			dw_template_list.SetFilter("Left(template_version_no ,1) = 'E'") 
			dw_template_list.Filter()

		/* SELECT (HILITE) THE FIRST ROW
		*/
			total_templates = dw_template_list.RowCount()
			l_total_templates = dw_template_list.RowCount()
			template_list_gb.text = "Templates ("+string(l_total_templates)+"):"
//	COMMENTED OUT FOR NOW  04/23/97 Making kyboard changes
//			vil_template_list_rowno = 1
//			rc = dw_template_list.SelectRow(vil_template_list_rowno,TRUE)
//			IF rc < 0 then
//				Error.Text = "AN ERROR OCURRED SELECTING TEMPLATE NUMBER " +String(vil_template_list_rowno)
//				Error.line = 64
//				Error.object = "Window, W-Correspond"
//				Error.objectevent = "Open of window " 
//				SignalError()
//			END IF
	END IF
		/* RETRIEVE THE LIST OF EXISTING DOCUMENTS GENERATED FOR THIS CLAIM
		*/
			vil_generated_list_rowno = 0
			rc = dw_generated_list.SetTransObject(SQLCA)
			IF rc < 0 THEN
					Error.Line = 76
					Error.Object = "Window, W_Correspond"
					Error.objectevent = "Open of window"			
					Error.Text = "THE LIST OF GENERATED CORRESPONDENCE COULD NOT BE FOUND"
					SignalError()
			END IF
	/* display Generated documents
	*/		
			vistr_correspond_claim.claim_no								= viw_active_sheet.dw_basic_claim.GetItemNumber(1,"claim_no")			
			rc = dw_generated_list.Retrieve(vistr_correspond_claim.claim_no)
			vii_return_code = SQLCA.nf_handle_error("dw_generated_list","w_correspond","on open event")
			this.parentwindow().SetMicroHelp("Retrieving list of generated correspondence completed...")
			total_generated = dw_generated_list.RowCount()
	/* Disable buttons associated with the composed list, until the user selects an item */
			cb_edit.enabled = False
			cb_delete.enabled = False
			cb_send.enabled = False
			cb_edit_type.enabled = FALSE
			composed_list_gb.text = "Composed ("+string(total_generated)+"):"
// Setup the find template field
		vil_total_templates = l_total_templates
//			sle_find.SetFocus()
	this.parentwindow().SetMicroHelp("Ready.")
	this.Postevent("ue_post_open")
	
	getEnvironment(ie_op_sys)


end event

event closequery;this.parentwindow().SetMicroHelp("Ready")

// clean up any processes
// must make sure that OS is Windows 2000, 95, 98, ME
Choose Case ie_op_sys.OSType
	Case Windows!
		RETURN 0
	Case WindowsNT!
		CHOOSE Case ie_op_sys.OSMajorRevision
			Case 4
				// do nothing It's Windows 2000 or greater
				IF MessageBox("Close Word?", "Exiting this window will cause all open Microsoft Word windows and background processes to be closed.~nDo you wish to continue?", Question!, YesNo!) = 1 THEN
					RETURN 0
				ELSE
					RETURN 1 // do not close
				END IF
			Case is > 4
				IF MessageBox("Close Word?", "Exiting this window will cause all Microsoft Word windows created by WorkBench to be closed.~nDo you wish to continue?", Question!, YesNo!) = 1 THEN
					RETURN 0
				ELSE
					RETURN 1 // do not close
				END IF
			Case Else
				RETURN 0
		End Choose
	Case Else
		RETURN 0
END CHOOSE
end event

on w_correspond.create
int iCurrent
call super::create
this.cb_edit_type=create cb_edit_type
this.dw_get_med_service_providers=create dw_get_med_service_providers
this.dw_get_address=create dw_get_address
this.dw_individuals=create dw_individuals
this.st_1=create st_1
this.sle_find=create sle_find
this.rb_french=create rb_french
this.rb_english=create rb_english
this.cb_recipients=create cb_recipients
this.cb_preview=create cb_preview
this.cb_delete=create cb_delete
this.dw_generated_list=create dw_generated_list
this.cb_send=create cb_send
this.cb_edit=create cb_edit
this.cb_compose=create cb_compose
this.composed_list_gb=create composed_list_gb
this.dw_get_doc_sender=create dw_get_doc_sender
this.dw_template_list=create dw_template_list
this.template_list_gb=create template_list_gb
this.lbx_processes=create lbx_processes
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_edit_type
this.Control[iCurrent+2]=this.dw_get_med_service_providers
this.Control[iCurrent+3]=this.dw_get_address
this.Control[iCurrent+4]=this.dw_individuals
this.Control[iCurrent+5]=this.st_1
this.Control[iCurrent+6]=this.sle_find
this.Control[iCurrent+7]=this.rb_french
this.Control[iCurrent+8]=this.rb_english
this.Control[iCurrent+9]=this.cb_recipients
this.Control[iCurrent+10]=this.cb_preview
this.Control[iCurrent+11]=this.cb_delete
this.Control[iCurrent+12]=this.dw_generated_list
this.Control[iCurrent+13]=this.cb_send
this.Control[iCurrent+14]=this.cb_edit
this.Control[iCurrent+15]=this.cb_compose
this.Control[iCurrent+16]=this.composed_list_gb
this.Control[iCurrent+17]=this.dw_get_doc_sender
this.Control[iCurrent+18]=this.dw_template_list
this.Control[iCurrent+19]=this.template_list_gb
this.Control[iCurrent+20]=this.lbx_processes
end on

on w_correspond.destroy
call super::destroy
destroy(this.cb_edit_type)
destroy(this.dw_get_med_service_providers)
destroy(this.dw_get_address)
destroy(this.dw_individuals)
destroy(this.st_1)
destroy(this.sle_find)
destroy(this.rb_french)
destroy(this.rb_english)
destroy(this.cb_recipients)
destroy(this.cb_preview)
destroy(this.cb_delete)
destroy(this.dw_generated_list)
destroy(this.cb_send)
destroy(this.cb_edit)
destroy(this.cb_compose)
destroy(this.composed_list_gb)
destroy(this.dw_get_doc_sender)
destroy(this.dw_template_list)
destroy(this.template_list_gb)
destroy(this.lbx_processes)
end on

event activate;viw_active_sheet.dw_documents.SelectRow(0,False)


end event

event close;// clean up any processess
Choose Case ie_op_sys.OSType
	Case Windows!
		CHOOSE Case ie_op_sys.OSMajorRevision
			Case 4,5,6
				// Windows 95, 98, ME
				wf_2000_close_winword()
			Case Else
				Return  // API calls won't work, so exit
		End Choose
	Case WindowsNT!
		CHOOSE Case ie_op_sys.OSMajorRevision
			Case 4
				// Windows NT4
				wf_nt4_close_winword()
			Case is > 4
				// Windows 2000 or greater
				wf_2000_close_winword()
			Case Else
				Return // API calls won't work, so exit
		End Choose
	Case Else
		Return
END CHOOSE

// this error has been populated already
IF is_return = 'signal' THEN
	SignalError()
END IF

end event

event doubleclicked;//lbx_processes.visible = true
//lbx_processes.bringtotop = true
end event

type st_title from w_a_tool`st_title within w_correspond
string text = "Correspondence"
end type

type cb_close from w_a_tool`cb_close within w_correspond
integer x = 2272
end type

event cb_close::clicked;// ******************************************************************************************************************************
// AUTHOR:	MW George
// Modified:	94.09.27
SetPointer(HourGlass!)
/* Code added to set higlighted row in document viewer back to on */
If Isvalid(viw_active_sheet) then
		viw_active_sheet.dw_documents.SetRow(1)
		viw_active_sheet.dw_documents.SelectRow(1,True)
END IF
close(parent)
end event

type cb_edit_type from commandbutton within w_correspond
integer x = 1093
integer y = 1484
integer width = 443
integer height = 96
integer taborder = 110
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Edit Doc &Type"
end type

event clicked;string	comments, status,ls_return
w_edit	viw_edit	
int		vli_rc

N_OBJECTHELPER lnv_object_helper

f_populate_app_log(gs_appname,lnv_object_helper.get_app_component_type(THIS),lnv_object_helper.getpath(THIS),'clicked event')


//  ENSURE THAT SOMETHING WAS SELECTED FROM THE GENERATED LIST
IF vil_generated_list_rowno > 0 THEN
	// The Correspondece has not been already sent or deleted
	status = dw_generated_list.GetItemString(vil_generated_list_rowno,"status")
	IF status = "" THEN
		// SET CORRESPONDENCE INDENTIFIER
		vistr_correspond_claim.corr.doc_id			= dw_generated_list.GetItemNumber(vil_generated_list_rowno,"doc_id")
		vistr_correspond_claim.corr.corr_no			= dw_generated_list.GetItemNumber(vil_generated_list_rowno,"correspond_no")
		vistr_correspond_claim.corr.template_type	= dw_generated_list.GetItemString(vil_generated_list_rowno,"template_code")
		vistr_correspond_claim.corr.comments		= dw_generated_list.GetItemString(vil_generated_list_rowno,"user_comments")
		vistr_correspond_claim.corr.corr_action	= "Edit"
		vistr_correspond_claim.corr.document_name	= dw_generated_list.GetItemString(vil_generated_list_rowno,"physical_file_name")

		// ACQUIRE THE GENERATED CORRESPONDENCE
		SELECT		user_comments
		INTO		:comments
		FROM		CORRESPONDENCE
		WHERE		(claim_no		= :vistr_correspond_claim.claim_no)	AND
					(correspond_no	= :vistr_correspond_claim.corr.corr_no)
		USING		SQLCA;

		vii_return_code = SQLCA.nf_handle_error(" ","w_correspond","on cb_edit clicked")
		IF vii_return_code <> 0 THEN
			MessageBox("DATABASE INTEGRITY ERROR: 511-EDIT","UNABLE TO ACCESS THE SELECTED CORRESPONDENCE." &
						+"~n~r~n~rPLEASE CONTACT YOUR SYSTEM ADMINISTRATOR.",StopSign!)
			close(w_recipients)
		ELSE
			// Disable all buttons on w_correspond
			wf_reset_buttons(False, "all")

			// INVOKE UP512 - MAINTAIN CLAIM CORRESPONDENCE
			vli_rc = OpenWithParm(w_edit, vistr_correspond_claim,viw_active_sheet)
			is_return = Message.StringParm
			IF is_return = 'signal' THEN
				Parent.event trigger close()
			END IF
		END IF
	ELSE
		MessageBox("EDIT MESSAGE: 511-008","You are not permitted to EDIT correspondence~n~rthat have been DELETED or SENT.")
	END IF
END IF
end event

type dw_get_med_service_providers from u_dw_online within w_correspond
boolean visible = false
integer x = 539
integer y = 2112
integer height = 360
integer taborder = 140
string dataobject = "d_med_service_providers"
end type

event dberror;SQLCA.SQLDBCode = sqldbcode
SQLCA.SQLErrText = sqlerrtext
RETURN 1
end event

type dw_get_address from u_dw_online within w_correspond
boolean visible = false
integer x = 1029
integer y = 1648
integer width = 411
integer height = 292
integer taborder = 150
string dataobject = "d_get_address"
end type

event dberror;SQLCA.SQLDBCode = sqldbcode
SQLCA.SQLErrText = sqlerrtext
RETURN 1
end event

type dw_individuals from u_dw_online within w_correspond
boolean visible = false
integer x = 384
integer y = 1632
integer height = 360
integer taborder = 110
string dataobject = "d_individuals"
end type

event dberror;SQLCA.SQLDBCode = sqldbcode
SQLCA.SQLErrText = sqlerrtext
RETURN 1
end event

type st_1 from statictext within w_correspond
integer x = 73
integer y = 196
integer width = 146
integer height = 64
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Find:"
boolean focusrectangle = false
end type

type sle_find from singlelineedit within w_correspond
event key_up pbm_keyup
event key_process pbm_custom01
integer x = 238
integer y = 180
integer width = 512
integer height = 80
integer taborder = 10
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
boolean hideselection = false
end type

event key_up;this.postevent("key_process", message.wordparm, 0)
end event

event key_process;string	vls_expression, vls_ch
long	vll_found_row
integer vli_ascii_num

vls_ch = Char(wparam)
vli_ascii_num = Asc(vls_ch)

IF vli_ascii_num = 40 THEN
/* down arrow go to template list */	
	dw_template_list.SetFocus()
	dw_template_list.ScrollNextRow()
	Return
END IF

// Do case-insensitive search
IF Len(this.Text) > 0 THEN
	vls_expression = "Mid(Upper(template_code), 1, " + String(Len(this.Text)) + ") = " + "'" + Upper(this.Text) + "'" 
	vll_found_row = dw_template_list.Find(vls_expression, 1, dw_template_list.RowCount())

	if vll_found_row > 0 then 
		dw_template_list.ScrollToRow(vll_found_row) // Triggers rowfocus changed event
	else
		this.Text = Mid(this.Text, 1, Len(this.Text) - 1)
		this.SelectText(Len(this.Text) + 1, 0)
		Beep(1)
	end if
else		// Filter length is 0, so unhighlight former selected row
	dw_template_list.SelectRow(vil_template_list_rowno, FALSE)
end if

end event

event getfocus;long vll_white = 1073741824
If not this.DisplayOnly then
	this.SetRedraw(False)
	sle_find.DisplayOnly = False
	sle_find.BackColor = vll_white
	sle_find.BorderStyle = StyleLowered!
	this.SetRedraw(True)
END IF	
end event

type rb_french from radiobutton within w_correspond
integer x = 1390
integer y = 176
integer width = 302
integer height = 72
integer taborder = 30
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "&French"
end type

on losefocus;IF KEYDOWN(keydownarrow!)  or KEYDOWN(keyrightarrow!) then 
	dw_template_list.SetFocus()
END IF
end on

event clicked;long rc,vll_found,vll_white = 1073741824

// Filter out english templates
string	vls_expression

IF dw_template_list.RowCount() > 0 THEN
	vis_filt = dw_template_list.GetItemString(dw_template_list.GetRow(), 'template_code')
END IF
dw_template_list.SelectRow(0,False)
dw_generated_list.SelectRow(0,False)
vls_expression = "Left(template_version_no ,1) = 'F'"
/* Need to set the current row to what was the clicked row before Filter function calls rowfocuschanged */
//dw_template_list.SetRow(vil_template_list_rowno)
wf_set_filter(vls_expression)
vil_total_templates = dw_template_list.RowCount()
template_list_gb.text = "Templates ("+string(vil_total_templates)+"):"

IF vil_total_templates = 0 THEN 
	cb_compose.enabled = False
	cb_preview.enabled = False
END IF

vls_expression = "Mid(Upper(template_code), 1, " + String(Len(vis_filt)) + ") = " + "'" + Upper(vis_filt) + "'" 
vll_found = dw_template_list.Find(vls_expression, 1, dw_template_list.RowCount())
If vll_found = 0 then 
	vll_found = 1
	Beep(1)
End If

If vll_found > 0 then
	dw_template_list.ScrolltoRow(vll_found) //this will trigger rowfocus change 
END IF	

sle_find.SetReDraw(False)
sle_find.DisplayOnly = False
sle_find.BackColor = vll_white
sle_find.BorderStyle = StyleLowered!
sle_find.SetRedraw(True)
sle_find.Setfocus()
end event

event getfocus;long rc,vll_white = 1073741824
/* Code added to set higlighted row in document viewer off if on */
//If Isvalid(viw_active_sheet) then
//		viw_active_sheet.dw_documents.SelectRow(0,False)
//END IF
sle_find.SetReDraw(False)
//sle_find.text = ""
//sle_find.DisplayOnly = False
//sle_find.BackColor = vll_white
//sle_find.BorderStyle = StyleLowered!
//sle_find.SetRedraw(True)
this.enabled=True
end event

type rb_english from radiobutton within w_correspond
integer x = 1024
integer y = 176
integer width = 311
integer height = 72
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "E&nglish"
end type

event losefocus;IF KEYDOWN(keydownarrow!) then 
	dw_template_list.SetFocus()
END IF
//if keydown(keyleftarrow!) then
//	sle_find.SetFocus()
//END IF
end event

event clicked;long rc,vll_found,vll_white = 1073741824
// Filter out french templates

string	vls_expression

IF dw_template_list.RowCount() > 0 THEN
	vis_filt = dw_template_list.GetItemString(dw_template_list.GetRow(), 'template_code')
END IF
dw_template_list.SelectRow(0,False)
dw_generated_list.SelectRow(0,False)

vls_expression = "Left(template_version_no ,1) = 'E'"
wf_set_filter(vls_expression)
vil_total_templates = dw_template_list.RowCount()
template_list_gb.text = "Templates ("+string(vil_total_templates)+"):"

IF vil_total_templates = 0 THEN 
	cb_compose.enabled = False
	cb_preview.enabled = False
END IF
// Added 0825
vls_expression = "Mid(Upper(template_code), 1, " + String(Len(vis_filt)) + ") = " + "'" + Upper(vis_filt) + "'" 
vll_found = dw_template_list.Find(vls_expression, 1, dw_template_list.RowCount())
If vll_found = 0 then 
	vll_found = 1
	Beep(1)
End If

dw_template_list.ScrolltoRow(vll_found)
sle_find.SetRedraw(False)
sle_find.DisplayOnly = False
sle_find.BackColor = vll_white
sle_find.BorderStyle = StyleLowered!
sle_find.SetRedraw(True)
end event

event getfocus;long rc,vll_white = 1073741824
/* Code added to set higlighted row in document viewer off if on */
If Isvalid(viw_active_sheet) then
		viw_active_sheet.dw_documents.SelectRow(0,False)
END IF
sle_find.SetReDraw(False)
//sle_find.text = ""
sle_find.DisplayOnly = False
sle_find.BackColor = vll_white
sle_find.BorderStyle = StyleLowered!
sle_find.SetRedraw(True)
this.enabled=true
end event

type cb_recipients from commandbutton within w_correspond
integer x = 1897
integer y = 1680
integer width = 361
integer height = 96
integer taborder = 120
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Recipients..."
end type

on getfocus;/* Code added to set higlighted row in document viewer off if on */
If Isvalid(viw_active_sheet) then
		viw_active_sheet.dw_documents.SelectRow(0,False)
END IF

end on

event clicked;
N_OBJECTHELPER lnv_object_helper

f_populate_app_log(gs_appname,lnv_object_helper.get_app_component_type(THIS),lnv_object_helper.getpath(THIS),'clicked event')


// Disable all buttons on w_correspond
wf_reset_buttons(False, "all")

OpenWithParm(w_maintain_recipients,vistr_correspond_claim,viw_active_sheet)

end event

type cb_preview from commandbutton within w_correspond
integer x = 443
integer y = 804
integer width = 343
integer height = 96
integer taborder = 60
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Preview..."
end type

event clicked;//**************************************************************************************
// This process will allow the user to preview the template before proceding 
// to generate

string	vls_file_name, vls_doc_path, ls_template_ext
string	vls_word_command
long		vll_usage, vll_value
int		vli_rc
ulong		lul_handle, lul_win_handle

lul_win_handle = Handle(Parent)
vil_template_list_rowno = dw_template_list.GetSelectedRow(0)

ioo_word = Create u_word

// SEE IF A TEMPLATE WAS SELECTED FROM THE TEMPLATE DATAWINDOW
IF vil_template_list_rowno > 0 THEN
	// Disable all buttons on w_correspond
	wf_reset_buttons(False, "all")

	//Determine if template is a macro template(.docm), or docx
	ls_template_ext = dw_template_list.GetItemString(vil_template_list_rowno, "file_extension")
	vls_file_name = dw_template_list.GetItemString(vil_template_list_rowno,"template_code") + '.' + ls_template_ext

	// Determine language 
	IF Left(dw_template_list.GetItemString(vil_template_list_rowno,"template_version_no"),1) = 'E' THEN
		vls_doc_path = vistr_correspond_claim.corr.engl_tmplt_path + vls_file_name
	ELSE
		vls_doc_path = vistr_correspond_claim.corr.french_tmplt_path + vls_file_name
	END IF

	// Connect to Word
	vli_rc = ioo_word.uf_connect()
	IF vli_rc < 0 THEN
		Return -1
	END IF
	
	// open word as read-only and make it visible
	vli_rc = ioo_word.uf_file_open(vls_doc_path,true,true,0)
	IF vli_rc = OLE_ERROR THEN
		f_populate_ole_error('w_correspondence','cb_preview','clicked','OLE',40,'',vistr_correspond_claim.corr.template_type)
		is_return = 'signal'
		parent.event trigger close()
	ELSEIF vli_rc < 0 THEN
		MessageBox("Word Error","Could not open file.",Information!)
	END IF

	// Disable all buttons on w_correspond
	wf_reset_buttons(True, "all")
END IF	
end event

event getfocus;///* Code added to set higlighted row in document viewer off if on */
//If Isvalid(viw_active_sheet) then
//		viw_active_sheet.dw_documents.SelectRow(0,False)
//END IF
long vll_white=1073741824
sle_find.SetReDraw(False)
//sle_find.text = ""
sle_find.DisplayOnly = False
sle_find.BackColor = vll_white
sle_find.BorderStyle = StyleLowered!
sle_find.SetRedraw(True)
end event

type cb_delete from commandbutton within w_correspond
integer x = 750
integer y = 1484
integer width = 343
integer height = 96
integer taborder = 100
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Delete..."
end type

event clicked;// ******************************************************************************************************************************
// AUTHOR:	MW George
// Modified:	94.10.20

// *************************************************************************************************************************
// DECLARATIONS

date		ddate
w_delete	vlw_delete
int			vli_rc

N_OBJECTHELPER lnv_object_helper

f_populate_app_log(gs_appname,lnv_object_helper.get_app_component_type(THIS),lnv_object_helper.getpath(THIS),'clicked event')



/* CHECK TO SEE IF ONE OF THE GENERATED DOCUMENTS HAVE BEEN SELECTED
*/
IF vil_generated_list_rowno > 0 THEN
	
	/*
	 VERIFY THAT THE SELECTED DOCUMENT IS PERMITTED TO BE DELETED
	 (Rules to be determined)
	 CURRENTLY, PERMITTED TO DELETE ANY GENERATED DOCUMENT THAT WAS NOT PREVIOUSLY
	 DELETED OR WAS NOT DISTRIBUTED, SET IN THE COMPUTED FIELD CALLED "STATUS"
	*/
	IF dw_generated_list.GetItemString(vil_generated_list_rowno,"status") <> "" THEN
		MessageBox("DELETE MESSAGE: 511-006","You are not permitted to delete this document.")
	ELSE
		// *************************************************************************************************************************
		// USER IS PERMITTED TO DELETE THE CORRESPONDENCE SO SET CORRESPONDENCE IDENTIFIER
		
		vistr_correspond_claim.corr.corr_no			= dw_generated_list.GetItemNumber(vil_generated_list_rowno,"correspond_no")
		vistr_correspond_claim.corr.template_type	= dw_generated_list.GetItemString(vil_generated_list_rowno,"template_code")
		vistr_correspond_claim.corr.comments		= dw_generated_list.GetItemString(vil_generated_list_rowno,"user_comments")
		vistr_correspond_claim.corr.corr_action	= "Delete"
	
		// *************************************************************************************************************************
		// ACQUIRE THE SELECTED CORRESPONDENCE
	
		SELECT		deleted_date	
		INTO		:ddate
		FROM		CORRESPONDENCE
		WHERE		(claim_no		= :vistr_correspond_claim.claim_no)	AND
					(correspond_no	= :vistr_correspond_claim.corr.corr_no)
		USING		SQLCA;

		vii_return_code = SQLCA.nf_handle_error(" ","w_correspond","on cb_delete clicked")
		IF vii_return_code <> 0 THEN
			MessageBox("DATABASE INTEGRITY ERROR: 511-EDIT","UNABLE TO ACCESS THE SELECTED CORRESPONDENCE." &
					+"~n~r~n~rPLEASE CONTACT YOUR SYSTEM ADMINISTRATOR.",StopSign!)
			close(w_recipients)
		ELSE

			// Disable all buttons on w_correspond
			wf_reset_buttons(False, "all")

			// ********************************************************************************************************************
			// INVOKE UP512 - MAINTAIN CLAIM CORRESPONDENCE
	
			vli_rc = OpenWithParm(w_delete, vistr_correspond_claim, viw_active_sheet)
		END IF
	END IF
END IF
end event

on getfocus;/* Code added to set higlighted row in document viewer off if on */
If Isvalid(viw_active_sheet) then
		viw_active_sheet.dw_documents.SelectRow(0,False)
END IF
cb_preview.enabled = False
cb_compose.enabled = False
rb_english.enabled = False
rb_french.enabled = False
sle_find.enabled = False
end on

type dw_generated_list from u_dw_online within w_correspond
event ue_key_down pbm_keydown
integer x = 55
integer y = 1056
integer width = 2569
integer height = 400
integer taborder = 70
string dataobject = "d_generated_list"
boolean vscrollbar = true
end type

on ue_key_down;long	curr_row,row_count

this.SetReDraw(False)
	curr_row = this.GetRow()
	row_count = this.Rowcount()
	IF curr_row > 0 then
 			
			this.SelectRow(0,False)
			
			IF	KeyDown(keydownarrow!) then
						/* Toggle the row off and on */
						this.SelectRow(curr_row, not this.IsSelected(curr_row))
			END IF			
			
			IF 	KeyDown(keyuparrow!) then
						this.SelectRow(curr_row, not this.IsSelected(curr_row))
			END IF
	
			IF KeyDown(keyhome!) then
					/* Scroll to first */ 
					this.SelectRow(1,not this.ISSelected(curr_row))
					THIS.ScrollToRow(1)
					
			END IF

			IF KeyDown(keyend!) then
					/* Scroll to last  */
					this.SelectRow(row_count,not this.ISSelected(curr_row))
					this.ScrollToRow(row_count)
			END IF
					
		END IF

this.SetReDraw(True)
end on

event clicked;// ************************************************************************************************
// AUTHOR:	MW George
// Modified:	94.09.02

long	vll_prev_row, vll_grey

vll_grey = 67108864
If Isvalid(viw_active_sheet) then
	viw_active_sheet.dw_documents.SelectRow(0,False)
END IF

this.SelectRow(0,False) /* Comment temporialy */

vll_prev_row =  dw_generated_list.GetRow()
vil_generated_list_rowno = row

vil_template_list_rowno = 0

if row = 0 then	
	row = vll_prev_row
end if

if row > 0 then
	// Disable buttons associated with the composed list, until the user selects an item
	cb_compose.enabled = False
	cb_preview.enabled = False
	sle_find.text = ""
	sle_find.DisplayOnly = TRUE
	sle_find.BackColor = vll_grey
	sle_find.BorderStyle = StyleBox!
	//	rb_english.enabled = False
	//	rb_french.enabled = False
	dw_template_list.SelectRow(0,False)

	// enable buttons associated with the template list
	cb_edit.enabled = True
	cb_delete.enabled = True
	cb_send.enabled = True
	cb_edit_type.enabled = True
	this.ScrollToRow(row)
	this.SelectRow(row,True)
	vistr_correspond_claim.corr.comments = dw_generated_list.GetItemString(row,"user_comments")
else
	beep(1)
end if
end event

on rowfocuschanged;vil_generated_list_rowno = this.GetRow()
this.Triggerevent("ue_key_down")

end on

on getfocus;wf_reset_buttonss(dw_generated_list)



end on

event dberror;SQLCA.SQLDBCode = sqldbcode
SQLCA.SQLErrText = sqlerrtext
RETURN 1
end event

type cb_send from commandbutton within w_correspond
integer x = 402
integer y = 1484
integer width = 343
integer height = 96
integer taborder = 90
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Send..."
end type

on getfocus;/* Code added to set higlighted row in document viewer off if on */
If Isvalid(viw_active_sheet) then
		viw_active_sheet.dw_documents.SelectRow(0,False)
END IF
cb_preview.enabled = False
cb_compose.enabled = False
rb_english.enabled = False
rb_french.enabled = False
sle_find.enabled = False
end on

event clicked;w_send	     viw_send
INTEGER		vli_rc
STRING       err_sheet,status, ls_return
LONG          ll_handle


N_OBJECTHELPER lnv_object_helper

f_populate_app_log(gs_appname,lnv_object_helper.get_app_component_type(THIS),lnv_object_helper.getpath(THIS),'clicked event')

// HOUSEKEEPING
err_sheet = viw_active_sheet.title
IF NOT IsValid(viw_active_sheet)  then
		Error.Text = "Error determining sheet "+err_sheet
		Error.object = 'W_Correspond'
		SignalError()
		Return
END IF

IF not isvalid (vistr_correspond_claim) then
		error.Text = "Error determining structure "+err_sheet
		Error.object = 'W_Correspond'
		SignalError()
		Return
END IF

// CHECK THAT A VALID DOCUMENT WAS SELECTED
IF vil_generated_list_rowno > 0 then
	// CHECK TO SEE THAT THE CORRESPONDENCE IS NOT ALREADY DELETED
	// Status used to be checked in W_send, It's been changed to check the sent_flag
	// in table DOCUMENT_INDEX
	IF dw_generated_list.GetItemString(vil_generated_list_rowno,"c_archived_doc") = 'A' THEN
		MessageBox("Archived","This correspondence has been Archived and cannot be reprinted.")
		Return
	END IF
	status = dw_generated_list.GetItemString(vil_generated_list_rowno,"status")
	Choose Case status
		Case "D" // Deleted
			MessageBox("SEND MESSAGE: 511:009","This correspondence has been DELETED.")
			Return
		Case "M" // Mailed
			vli_rc =MessageBox("SEND MESSAGE: 511:009","This correspondence has been sent to MAIL ROOM.~n~r Produce a paper copy?",Question!,OKCANCEL!)
			If vli_rc = 2 then Return
		Case "S" //Sent
			vli_rc = MessageBox("SEND MESSAGE: 511:009","This Claim has already been sent.~n~r Produce a paper copy?",Question!,OKCANCEL!) 
			If vli_rc = 2 then	Return
		Case Else
			// Do nothing for now
	END CHOOSE	
	// Disable all buttons on w_correspond
	wf_reset_buttons(False, "all")
	/* The following is needed by W_send */
	  
	vistr_correspond_claim.corr.comments			=	dw_generated_list.GetItemString(vil_generated_list_rowno,"user_comments")
	vistr_correspond_claim.corr.doc_id			=	dw_generated_list.GetItemNumber(vil_generated_list_rowno,"doc_id")
	vistr_correspond_claim.corr.corr_no			= dw_generated_list.GetItemNumber(vil_generated_list_rowno,"correspond_no")
	vistr_correspond_claim.corr.document_name	= dw_generated_list.GetItemString(vil_generated_list_rowno, "physical_file_name")
	vistr_correspond_claim.corr.template_type	= dw_generated_list.GetItemString(vil_generated_list_rowno,"template_code")
	vistr_correspond_claim.corr.language_code	= dw_generated_list.GetItemString(vil_generated_list_rowno,"correspondence_template_language_code")
	vistr_correspond_claim.rcpnt.action			= status
		
	SELECT	b.cc_allowed_yn, b.auto_email_yn, b.file_extension
	INTO		:vistr_correspond_claim.corr.cc_allowed_yn, :vistr_correspond_claim.corr.auto_email_yn, :vistr_correspond_claim.corr.template_extension
	FROM 	CORRESPONDENCE					a,
				CORRESPONDENCE_TEMPLATE	     b
	WHERE	a.template_code				= b.template_code
	AND		a.template_language_code	= Left(b.template_version_no,1)
	AND		a.claim_no						= :vistr_correspond_claim.claim_no
	AND		a.correspond_no				= :vistr_correspond_claim.corr.corr_no
	USING SQLCA;
	
	SQLCA.nf_handle_error("w_correspond","cb_send","SELECT b.cc_allowed_yn, b.auto_email_yn")	
	
	SELECT  DOCUMENT_INDEX.type_code into  :vistr_correspond_claim.corr.document_type    
	FROM DOCUMENT_INDEX      
	WHERE ( DOCUMENT_INDEX.docid = :vistr_correspond_claim.corr.doc_id )   
	USING IMAGETRANS;
	ImageTrans.nf_handle_error ("W_Correspond","Embedded SQL SELECT","in clicked of cb_send")
	
	vli_rc = OpenWithParm(w_send, vistr_correspond_claim,vistr_correspond_claim.parent_window	)
	is_return = Message.StringParm
	IF is_return = 'signal' THEN
		Parent.event trigger close()
	END IF
	
	ll_handle = Handle(this)
	
	BringWindowToTop(ll_handle)

ELSE
	MessageBox(parent.title,"Invalid document selected. Please choose a valid document",Information!)
	close(parent)
	return
END IF 	
end event

type cb_edit from commandbutton within w_correspond
integer x = 55
integer y = 1484
integer width = 343
integer height = 96
integer taborder = 80
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Edit..."
end type

event clicked;string	comments, status,ls_return
w_edit	viw_edit	
int		vli_rc

N_OBJECTHELPER lnv_object_helper

f_populate_app_log(gs_appname,lnv_object_helper.get_app_component_type(THIS),lnv_object_helper.getpath(THIS),'clicked event')


//  ENSURE THAT SOMETHING WAS SELECTED FROM THE GENERATED LIST
IF vil_generated_list_rowno > 0 THEN
	// The Correspondece has not been already sent or deleted
	status = dw_generated_list.GetItemString(vil_generated_list_rowno,"status")
	IF status = "" THEN
		// SET CORRESPONDENCE IDENTIFIER
		vistr_correspond_claim.corr.doc_id			= dw_generated_list.GetItemNumber(vil_generated_list_rowno,"doc_id")
		vistr_correspond_claim.corr.corr_no			= dw_generated_list.GetItemNumber(vil_generated_list_rowno,"correspond_no")
		vistr_correspond_claim.corr.template_type	= dw_generated_list.GetItemString(vil_generated_list_rowno,"template_code")
		vistr_correspond_claim.corr.comments		= dw_generated_list.GetItemString(vil_generated_list_rowno,"user_comments")
		vistr_correspond_claim.corr.corr_action	= "Edit"
		vistr_correspond_claim.corr.document_name	= dw_generated_list.GetItemString(vil_generated_list_rowno,"physical_file_name")

		// ACQUIRE THE GENERATED CORRESPONDENCE
		SELECT		user_comments
		INTO		:comments
		FROM		CORRESPONDENCE
		WHERE		(claim_no		= :vistr_correspond_claim.claim_no)	AND
					(correspond_no	= :vistr_correspond_claim.corr.corr_no)
		USING		SQLCA;

		vii_return_code = SQLCA.nf_handle_error(" ","w_correspond","on cb_edit clicked")
		IF vii_return_code <> 0 THEN
			MessageBox("DATABASE INTEGRITY ERROR: 511-EDIT","UNABLE TO ACCESS THE SELECTED CORRESPONDENCE." &
						+"~n~r~n~rPLEASE CONTACT YOUR SYSTEM ADMINISTRATOR.",StopSign!)
			close(w_recipients)
		ELSE
			open_word_file(vistr_correspond_claim.corr.document_name,string(vistr_correspond_claim.claim_no),vistr_correspond_claim.corr.corr_action)
		END IF
	ELSE
		MessageBox("EDIT MESSAGE: 511-008","You are not permitted to EDIT correspondence~n~rthat have been DELETED or SENT.")
	END IF
END IF
end event

on getfocus;/* Code added to set higlighted row in document viewer off if on */
If Isvalid(viw_active_sheet) then	viw_active_sheet.dw_documents.SelectRow(0,False)


end on

type cb_compose from commandbutton within w_correspond
integer x = 55
integer y = 804
integer width = 384
integer height = 96
integer taborder = 50
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "C&ompose..."
boolean default = true
end type

event clicked;DATETIME					ldtm_death_date
INTEGER						li_active_annuity_count,   li_counter,  li_return
LONG    						rc, ll_individual_no
STRING 						ls_claimant_active_flag, ls_claim_role_code, ls_claim_list, ls_claim_list_master
BOOLEAN					lb_check
u_ds							lds_return_check
s_window_message 		lstr_message

N_OBJECTHELPER lnv_object_helper

f_populate_app_log(gs_appname,lnv_object_helper.get_app_component_type(THIS),lnv_object_helper.getpath(THIS),'clicked event')


// SEE IF A TEMPLATE WAS SELECTED FROM THE TEMPLATE DATAWINDOW
IF vil_template_list_rowno <= 0 THEN RETURN

vistr_correspond_claim.corr.template_type = dw_template_list.GetItemString(vil_template_list_rowno,"template_code")
	
// Check to be sure that claim is at correct status for NCNOTIC or CRNOTIC
IF (vistr_correspond_claim.corr.template_type = 'NCNOTIC' AND &
	(viw_active_sheet.dw_basic_claim.GetItemString(1,'claim_status_code') <> 'R' OR &
	viw_active_sheet.dw_basic_claim.GetItemString(1,'claim_status_type_code') <> '09')) OR &
	(vistr_correspond_claim.corr.template_type = 'CRNOTICE' AND &
	(viw_active_sheet.dw_basic_claim.GetItemString(1,'claim_status_code') <> 'R' OR &
	viw_active_sheet.dw_basic_claim.GetItemString(1,'claim_status_type_code') <> '06')) THEN

	MessageBox("Error","Can not compose this letter at this time as claim is at the wrong status.")
	RETURN
END IF
	
/*
****************************ANNUITY PAYOUT 01/02****************************
NOTE:
The BRs below will be exercised regardless for which annuity account an annuity payout letter was originally created.
*/
IF vistr_correspond_claim.corr.template_type = 'ANNPAY01' OR vistr_correspond_claim.corr.template_type = 'ANNPAY02' THEN
	
	//BR 1.180	
	/* On composition of an Annuity Payout letter within the Correspondence module, 
	it must be determined which annuity account is to be used in creating the annuity payout letter. 
	
	Only the injured worker and surviving spouse who are involved in the claim that is selected on the Workbench 
	are available for selection. For example, if the injured worker is also a surviving spouse on a different claim, 
	s/he is not considered while in the correspondence module for the selected claim. 
	The other claim would have to be selected and correspondence created under that claim.
	If there is only an annuity account for the injured worker, then it will be the injured worker’s annuity account. 

	If there is only an annuity account for the surviving spouse, then it will be the surviving spouse’s annuity account.  
	If there are annuity accounts for both the injured worker and surviving spouse, and only one of them has 
	reached the annuity eligibility end date (ex. injured worker deceased prior to age 65), then the annuity account for 
	that individual must be used.  If both the injured worker and surviving spouse have annuity accounts and they have 
	both reached their annuity eligibility end dates, then the annuity account must be selected from a list.  

	RETURN 0 -- NO eligibility accounts for either the claimant or surviving spouse for this claim
	RETURN 1 - NORMAL CASE USE claim_role_code 'C'
	RETURN 2 - Surviving Spouse - use 'SS'
	RETURN 3 - BOTH user to pick from list
	RETURN -1 - Something Bad Happened - should never happen
			
	*/
	li_return = wf_check_for_surviving_spouse(vistr_correspond_claim.claim_no)
		
	CHOOSE CASE li_return
		CASE -1  // should never happen
				MessageBox('ERROR','There was an unexpected error determining the eligibility of claimant and or spouse. Call the helpDesk',Stopsign!)
				RETURN
				
		CASE 0, 1 
				// for zero annuity records, or where 'C' and 'SS' have annuity eligibilities that have not reached the end date, (li_return = 0) , 
				// populate the individual_no with that of the injured worker and allow processing to continue.
				// A subsequent BR check (BR 1.220 or BR 1.210) will raise the appropriate messagebox and allow the user to continue or back out
					 
				 //CASE 1 = If there is only an annuity account for the injured worker, then it will be the injured worker’s annuity account. 
			
				SELECT	b.individual_no
				INTO		:ll_individual_no
				FROM     INDIVIDUAL  b 
				JOIN	    CLAIM_PARTICIPANT	c ON b.individual_no = c.individual_no
				WHERE	c.claim_no			= :vistr_correspond_claim.claim_no
				AND		c.claim_role_code	= 'C'
				AND       c.claimant_active_flag = 'Y'
				USING 	SQLCA;
				SQLCA.nf_handle_error('w_correspond', 'cb_compose', 'SELECT	b.individual_no. (C)')
				
				ls_claim_role_code = 'C'
		
		CASE 2//If there is only an annuity account for the surviving spouse, then it will be the surviving spouse’s annuity account.  
				
				SELECT	b.individual_no
				INTO		:ll_individual_no
				FROM    INDIVIDUAL	  b 
				JOIN	    CLAIM_PARTICIPANT c ON b.individual_no = c.individual_no
				WHERE	c.claim_no			= :vistr_correspond_claim.claim_no
				AND		c.claim_role_code	= 'SS'
				USING 	SQLCA;
				SQLCA.nf_handle_error('w_correspond', 'cb_compose', 'SELECT	b.individual_no. (SS)')
				
				ls_claim_role_code = 'SS'
		
		CASE 3	/* If both the injured worker and surviving spouse have annuity accounts and they have both reached their annuity eligibility end dates, 
			 		then the annuity account must be selected from a list.  
					*/
					
				openwithparm(w_injured_worker_or_ss_selection,vistr_correspond_claim.claim_no)
			
				//grab the return
				lstr_message = Message.powerobjectparm

				ll_individual_no 		= lstr_message.al_doubleparm[1]
				ls_claim_role_code  = lstr_message.as_stringparm[1]
				
				
				IF ll_individual_no = 0 THEN RETURN //CANCEL WAS SELECTED
				
	END CHOOSE
	
	//save the claim role code for the individual in the structure for later use
	vistr_correspond_claim.annuity_individual_claim_role = ls_claim_role_code
	
	//default the individual - could be the SS
	IF ll_individual_no > 0 THEN 
		IF 	vistr_correspond_claim.annuity_payout_individual_no <> ll_individual_no THEN 
			vistr_correspond_claim.annuity_payout_individual_no = ll_individual_no
		END IF 
	END IF 

	/*  BR  1.200	The annuity payout letter should be approriate, if the individual is deceased. Refer to Rationale
	*/
	lb_check = wf_check_individual_deceased(ll_individual_no)
	
	IF lb_check = TRUE THEN 
		MessageBox('Deceased Individual','The individual is deceased so please ensure the annuity payout letter is correctly' &
	                        +	'~r~naddressed to the appropriate recipient(s). For example, surviving spouse and/or dependent(s).',Exclamation!)
	END IF

	/* BR 1.210	The annuity payout letter should not be created for the individual, if the annuity eligibility end date is in the future. Refer to Rationale
		Rationale
	*/
	lb_check = wf_check_future_annuity_end_date(ll_individual_no)
	
	IF lb_check = TRUE THEN 
		IF MessageBox('Future Annuity End Date','The individual’s annuity eligibliity end date is in the future, the payout letter should not be generated.' &
							+	'~r~nDo you want to continue?',Exclamation!,YesNo!,2) = 2 THEN
		  	RETURN
		END IF
	END IF
	
	/* BR 1.190	 The annuity payout letter should not be created for the individual, if the individual is not eligible for annuity benefits. Refer to Rationale
	*/
	lb_check = 	wf_check_individual_eligible_for_annuity(ll_individual_no)
	
	IF lb_check = FALSE THEN 
		IF MessageBox('Individual Not Eligible','This individual is not eligible for annuity benefits, the payout letter should not be generated.' &
							+	'~r~nDo you want to continue?',Exclamation!,YesNo!,2) = 2 THEN
		  	RETURN
		END IF
	END IF

	/* BR 1.220	 The annuity payout letter should not be created for the individual, if the individual’s annuity elgibility is in the process of
	     being confirmed. Refer to Rationale
	*/
	lb_check = 	wf_check_annuity_being_confirmed(ll_individual_no)
	
	IF lb_check = TRUE THEN 
		IF MessageBox('Annuity Being Confirmed','The Individual has a Confirm Annuity Eligibility checklist in progress. ' &
			                  +	'~r~nA payout letter should not be generated until the annuity eligibility has been confirmed.' &
							+	'~r~nDo you want to continue?',Exclamation!,YesNo!,2) = 2 THEN
		  	RETURN
		END IF
	END IF
	
	/* BR 1.230	The annuity payout letter should not be created for the injured worker, if an annuity payout letter was previously sent for 
		any claims in which the individual is the active claimant. Refer to Rationale
	*/
	lds_return_check = 	wf_check_payout_letter_sent_iw(ll_individual_no)
	
	IF rowcount(lds_return_check) > 0 THEN 
			
		ls_claim_list_master 	= ''
		ls_claim_list				= ''
		//create the list of claims
		FOR li_counter = 1 TO lds_return_check.rowcount()
			ls_claim_list 			= STRING(lds_return_check.getitemnumber(li_counter, 'claim_no'))
			ls_claim_list_master 	= ls_claim_list_master + ls_claim_list + '~r~n'	
		NEXT
		
		IF MessageBox('Annuity Payout letter exists','An Annuity Payout letter exists in one or more claims for the injured worker.. ' &
			                  +	'~r~nPlease review to determine if another one should be generated (i.e.the existing letter is for a previous payout).' &
							+  '~r~nClaim(s):' &
							+  '~r~n' + ls_claim_list_master &
							+	'~r~nDo you want to continue?',Exclamation!,YesNo!,2) = 2 THEN
		  	RETURN		  
		END IF
	END IF
	
	/* BR 1.240	The annuity payout letter should not be created for the surviving spouse, if an annuity payout letter was previously sent for the claim. 
	     Refer to Rationale
	*/
	IF ls_claim_role_code = 'SS'  THEN 
		lb_check = 	wf_check_payout_letter_in_claim_ss(vistr_correspond_claim.claim_no)
		
		IF lb_check = TRUE THEN 
			IF MessageBox('Letter Sent For Claim','An Annuity Payout letter exists in this claim. Please review to determine if another one should be generated. ' &
										+	'~r~n(i.e.the existing letter is for a previous payout)' &
								+	'~r~nDo you want to continue?',Exclamation!,YesNo!,2) = 2 THEN
				RETURN
			END IF
		END IF
	END IF 
	
	/* BR 1.250	The annuity payout letter should not be created for the injured worker if there is an annuity payout letter 
	     that has not been sent in any claims in which the individual is the active claimant. Refer to Rationale
	*/
	lds_return_check = wf_check_Payout_letter_not_sent_iw(ll_individual_no)
	
	IF rowcount(lds_return_check) > 0 THEN 
		
		ls_claim_list_master 	= ''
		ls_claim_list				= ''
		//create the list of claims
		FOR li_counter = 1 TO lds_return_check.rowcount()
			
			ls_claim_list 			= STRING(lds_return_check.getitemnumber(li_counter, 'claim_no'))
			ls_claim_list_master 	= ls_claim_list_master + ls_claim_list + '~r~n'
			
		NEXT
		
		IF MessageBox('Letter Sent For Claim','An Annuity Payout letter has been generated but not yet sent in the following claim files: ' &
			                  +	'~r~n(i.e.the existing letter is for a previous payout)' &
							+  '~r~nClaim(s):' &
							+  '~r~n' + ls_claim_list_master &
							+	'~r~nDo you want to continue?',Exclamation!,YesNo!,2) = 2 THEN
		  	RETURN
		END IF
	END IF
	
	/* BR 1.260	 The annuity payout letter should not be created for the surviving spouse, 
		if there is an annuity payout letter in the claim file that has not been sent. Refer to Rationale
	*/
	IF ls_claim_role_code = 'SS'  THEN 
		lb_check = 	wf_check_Payout_letter_not_sent_ss(ll_individual_no)
		
		IF lb_check = TRUE THEN 
			IF MessageBox('Annuity Letter Generated','An Annuity payout letter has been generated but not yet sent.' &
								+	'~r~nDo you want to continue?',Exclamation!,YesNo!,2) = 2 THEN
				RETURN
			END IF
		END IF
	END IF 

	/* BR 1.270	 The annuity payout letter should not be created for the individual, if there is not an annuity calculation for payout purposes 
	    for the individual’s annuity account (i.e. annuity calculation reason of Payout - Deceased Prior to Age 65 or Payout - Attained Age 65 ).
		Refer to Rationale
	*/
	lb_check = 	wf_check_no_calculation_for_payout(ll_individual_no)
	
	IF lb_check = FALSE THEN 
		IF MessageBox('No annuity calculation for payout','An Annuity Calculation has not been performed for the reason of Payout' &
							+	'~r~nDo you want to continue?',Exclamation!,YesNo!,2) = 2 THEN
		  	RETURN
		END IF
	END IF
	
END IF //end of ANNPAY checks
	
IF vistr_correspond_claim.corr.template_type = 'ANNQUAL' THEN
		
	SELECT	Count(*)
	INTO		:li_active_annuity_count
	FROM		ANNUITY_ELIGIBILITY	a
	JOIN		ANNUITY_ACCOUNT	b ON a.annuity_account_no = b.annuity_account_no
	WHERE	b.individual_no 							= :vistr_correspond_claim.individual_no
	AND		b.claim_role_code						= 'C'
	AND		a.annuity_eligibility_status_code 	= 'P'
	USING SQLCA;
		
	SQLCA.nf_handle_error('w_correspond', 'cb_compose - clicked', 'SELECT Count(*) (A)...')
		
	IF li_active_annuity_count > 0 THEN
		IF MessageBox('Pending Eligibility','The claimant has an annuity eligibility that is in the process of being confirmed.' &
										+	'~r~nThe Annuity Qualification correspondence should not be created.' &
										+	'~r~nIf you create this correspondence, then the document will have to be edited manually.' &
										+	'~r~nDo you want to continue?',Exclamation!,YesNo!,2) = 2 THEN
			RETURN
		END IF
	END IF
		
	SELECT	Count(*)
	INTO		:li_active_annuity_count
	FROM		ANNUITY_ELIGIBILITY	a
	JOIN		ANNUITY_ACCOUNT	b ON a.annuity_account_no = b.annuity_account_no
	WHERE	b.individual_no 							= :vistr_correspond_claim.individual_no
	AND		b.claim_role_code					  	= 'C'
	AND		a.annuity_eligibility_status_code 	= 'A'
	USING SQLCA;
	SQLCA.nf_handle_error('w_correspond', 'cb_compose - clicked', 'SELECT Count(*) (B)...')
		
	IF li_active_annuity_count = 0 THEN
		IF MessageBox('Not Eligible','The claimant is not eligible for an annuity. The Annuity Qualification correspondence should not be created.'&
										+	'~r~nIf you create this correspondence, then the document will have to be edited manually.' &
										+	'~r~nDo you want to continue?',Exclamation!,YesNo!,2) = 2 THEN
			RETURN
		END IF
	END IF
	

	SELECT	b.death_date
	INTO		:ldtm_death_date
	FROM		ANNUITY_ACCOUNT	a
	JOIN		INDIVIDUAL				b ON a.individual_no = b.individual_no
	JOIN		CLAIM_PARTICIPANT	c ON b.individual_no = c.individual_no
	WHERE	c.claim_no			= :vistr_correspond_claim.claim_no
	AND		c.claim_role_code	= 'C'
	USING SQLCA;
	SQLCA.nf_handle_error('w_correspond', 'cb_compose - clicked', 'SELECT b.death_date...')
		
	IF IsNull(ldtm_death_date) THEN
		// OK
	ELSE
		IF MessageBox('Deceased','The claimant is deceased. The Annuity Qualification correspondence should not be created.'&
										+	'~r~nIf you create this correspondence, then the document will have to be edited manually.' &
										+	'~r~nDo you want to continue?',Exclamation!,YesNo!,2) = 2 THEN
			RETURN
		END IF
	END IF
	
	SELECT	c.claimant_active_flag
	INTO		:ls_claimant_active_flag
	FROM		ANNUITY_ACCOUNT	a
	JOIN		INDIVIDUAL				b ON a.individual_no = b.individual_no
	JOIN		CLAIM_PARTICIPANT	c ON b.individual_no = c.individual_no
	WHERE	c.claim_no			= :vistr_correspond_claim.claim_no
	AND		c.individual_no		= :vistr_correspond_claim.individual_no
	AND		a.claim_role_code	= 'C'
	AND		c.claim_role_code	= 'C'
	USING SQLCA;
	SQLCA.nf_handle_error('w_correspond', 'cb_compose - clicked', 'SELECT claimant_active_flag...')
			
	IF ls_claimant_active_flag = 'N' THEN
		IF MessageBox('Inactive Claimant','The claimant is no longer active. The Annuity Qualification correspondence should not be created.'&
										+	'~r~nIf you create this correspondence, then the document will have to be edited manually.' &
										+	'~r~nDo you want to continue?',Exclamation!,YesNo!,2) = 2 THEN
			RETURN
		END IF
	END IF
END IF
	
// GET THE NEXT CORRESPONDENCE NUMBER
rc = wf_get_next_corr_no()
if rc < 0 then
	// Unable to determine next correspondence number
	Error.Text        = SQLCA.sqlerrtext
	IF Error.Text = '' THEN
		Error.Text        = 'Unable to determine next correspondence number.' + &
								  '~r~nClaim Number: '  + String(vistr_correspond_claim.claim_no)   + '.' + &
								  '~r~nTemplate Type: ' + vistr_correspond_claim.corr.template_type + '.'
	ELSE
		Error.Text        = Error.Text + '.~r~nUnable to determine next correspondence number' + &
								  '~r~nClaim Number: '  + String(vistr_correspond_claim.claim_no)   + '.' + &
								  '~r~nTemplate Type: ' + vistr_correspond_claim.corr.template_type + '.' 
	END IF
	Error.is_database = 'CLAIM'
	Error.Object      = 'w_correspond'
	Error.ObjectEvent = 'cb_compose.clicked'
	SignalError()
else
	// Disable all buttons on w_correspond
	wf_reset_buttons(False, "all")
	// SET CORRESPONDENCE INDENTIFIER
	vil_total_templates = dw_template_list.RowCount()

	If vil_template_list_rowno > vil_total_templates then
		Error.Text            = "INVALID TEMPLATE NUMBER "+STRING(vil_template_list_rowno) + &
								      '.~r~nTemplate Type: ' + vistr_correspond_claim.corr.template_type + '.' 
		Error.objectevent   = 'clicked'
		Error.Line             = 352
		Error.object          = 'cb_compose on w_correspond'
		SignalError()
	end if
	
	vistr_correspond_claim.corr.corr_no                     =  rc
	vistr_correspond_claim.corr.version_no                 = dw_template_list.GetItemString(vil_template_list_rowno,"template_version_no")
	vistr_correspond_claim.corr.comments	                 = dw_template_list.GetItemString(vil_template_list_rowno,"template_desc")
	vistr_correspond_claim.corr.signature_required_yn   = dw_template_list.GetItemString(vil_template_list_rowno,"signature_required_yn")
	vistr_correspond_claim.corr.cc_allowed_yn             = dw_template_list.GetItemString(vil_template_list_rowno,"cc_allowed_yn")
	vistr_correspond_claim.corr.corr_action                 = "Generate"
	vistr_correspond_claim.corr.template_extension          = dw_template_list.GetItemString(vil_template_list_rowno,"file_extension")
	// INVOKE UP512 - MAINTAIN CLAIM CORRESPONDENCE
	rc = OpenWithParm(w_maintain, vistr_correspond_claim, viw_active_sheet)
	is_return = Message.StringParm
	IF is_return = 'signal' THEN
		Parent.event trigger close()
	END IF
end if
end event

event getfocus;
///* Code added to set higlighted row in document viewer off if on */
//If Isvalid(viw_active_sheet) then
//		viw_active_sheet.dw_documents.SelectRow(0,False)
//END IF
long vll_white=1073741824
sle_find.SetReDraw(False)
//sle_find.text = ""
sle_find.DisplayOnly = False
sle_find.BackColor = vll_white
sle_find.BorderStyle = StyleLowered!
sle_find.SetRedraw(True)
end event

type composed_list_gb from groupbox within w_correspond
integer x = 18
integer y = 976
integer width = 2638
integer height = 652
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Composed"
end type

type dw_get_doc_sender from u_dw_online within w_correspond
boolean visible = false
integer x = 1509
integer y = 1944
integer height = 360
integer taborder = 130
boolean bringtotop = true
string dataobject = "d_doc_sender"
end type

event itemerror;call super::itemerror;RETURN 1
end event

event dberror;call super::dberror;ImageTrans.SQLDBCode = sqldbcode
ImageTrans.SQLErrText = sqlerrtext
RETURN 1
end event

type dw_template_list from u_dw_online within w_correspond
integer x = 37
integer y = 288
integer width = 2569
integer height = 496
integer taborder = 40
string dataobject = "d_template_list"
boolean vscrollbar = true
end type

event clicked;long	vll_prev_row, vll_white

vll_white = 1073741824
/* Code added to run off highlighted row in document list */
If Isvalid(viw_active_sheet) then
		viw_active_sheet.dw_documents.SelectRow(0,False)
END IF

// DE-SELECT ALL ROWS IN THE GENERATED LIST & TEMPLATES LIST WINDOWS

dw_generated_list.SelectRow(0,False)
dw_template_list.SelectRow(0,False)

// SET THE ROW NUMBER IN THE TEMPLATES LIST TO THE ONE THE USER CLICKED ON, HILITE IT,
// AND THE GENERATED LIST ROW NUMBER TO 0
vll_prev_row = row
vil_template_list_rowno = row
vil_generated_list_rowno = 0

if vil_template_list_rowno = 0   then
	vil_template_list_rowno = vll_prev_row
end if

if vil_template_list_rowno > 0 then
	// Disable buttons associated with the composed list, until the user selects an item
	cb_edit.enabled = False
	cb_delete.enabled = False
	cb_send.enabled = False
	cb_edit_type.enabled = FALSE
	// enable buttons associated with the template list
	cb_compose.enabled = True
	cb_preview.enabled = True
	sle_find.text = ""
	sle_find.DisplayOnly = False
	sle_find.BackColor = vll_white
	sle_find.BorderStyle = StyleLowered!
	this.ScrollToRow(vil_template_list_rowno)
	this.SelectRow(vil_template_list_rowno,True)

	// Populate template search field
	vil_total_templates = dw_template_list.RowCount()
	
	if vil_template_list_rowno > vil_total_templates then
		error.text = 'Invalid row selected'
		this.triggerevent ("dberror")
	end if
//	sle_find.SetFocus()
//	sle_find.text = dw_template_list.GetItemString(vil_template_list_rowno, "template_code")
//	sle_find.SelectText(1,len(sle_find.text)) // Select the entire line
//	sle_find.SelectText(len(sle_find.text)+1,0) // Move the insertion point to the end Commented 0825
//	vis_filt = sle_find.text 
	
//	vistr_correspond_claim.corr.document_type=dw_template_list.GetItemString(vil_template_list_rowno, "document_code")
 	// Hard code this value for now
	vistr_correspond_claim.corr.document_type="LC"
	vistr_correspond_claim.corr.version_no=this.GetItemString(vil_template_list_rowno, "template_version_no")
	vistr_correspond_claim.corr.remote_mail_yn=this.GetItemString(vil_template_list_rowno, "remote_mail_allowed_yn")
else
	beep(1)
end if
end event

event getfocus;wf_reset_buttonss(dw_template_list)
	
end event

event rowfocuschanged;/* Unhighlite the current row */
int row_count,curr_row
	this.SetReDraw(False)
	this.SelectRow(0,False)
	row_count = this.Rowcount()
	If currentrow < 1 or currentrow > row_count then Return /* Make sure you're not attempting to go to an invalid row */
	vil_template_list_rowno=currentrow
//	sle_find.text = dw_template_list.GetItemString(currentrow, "template_code")
		
	/* Next try setting the selectedtext in sle_find  */
	/* Turn on highlighting of current row */
	
	this.SelectRow(currentrow,True)

	IF	KeyDown(keydownarrow!) then
			/* Toggle the row off and on */
			this.SelectRow(currentrow, not this.IsSelected(currentrow))
			this.SelectRow(currentrow,True)
	END IF			
			
	IF 	KeyDown(keyuparrow!) then
				this.SelectRow(currentrow, not this.IsSelected(currentrow))
				this.SelectRow(currentrow,True)
	END IF
	
	IF KeyDown(keyhome!) then
			/* Scroll to first */ 
			this.SelectRow(1,not this.ISSelected(currentrow))
			THIS.ScrollToRow(1)
					
	END IF

	IF KeyDown(keyend!) then
			/* Scroll to last  */
			this.SelectRow(row_count,not this.ISSelected(currentrow))
			this.ScrollToRow(row_count)
	END IF
					
		

this.SetReDraw(True)
//this.Triggerevent("ue_key_down")

	
end event

event doubleclicked;cb_compose.triggerevent(Clicked!)
end event

event dberror;SQLCA.SQLDBCode = sqldbcode
SQLCA.SQLErrText = sqlerrtext
RETURN 1


end event

event itemfocuschanged;this.SelectRow(row,True)
end event

type template_list_gb from groupbox within w_correspond
integer x = 18
integer y = 96
integer width = 2638
integer height = 832
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Templates"
end type

type lbx_processes from listbox within w_correspond
boolean visible = false
integer x = 32
integer y = 84
integer width = 2446
integer height = 1684
integer taborder = 10
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

