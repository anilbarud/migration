$PBExportHeader$cmwb.sra
$PBExportComments$Case Management Work Bench Application
forward
global type cmwb from application
end type
global n_transaction sqlca
global dynamicdescriptionarea sqlda
global dynamicstagingarea sqlsa
global n_dw_error error
global message message
end forward

global variables
long                                          gl_window_handle
int 			vgi_channel
n_transaction 	ImageTrans
s_user_profile		vgst_user_profile
n_user_authorizations	gnv_user_authorizations
string			vgs_corresp_running
string			vgs_ini_filename
string                                         gs_default_dir
int			vgi_max_rcpnt=50
int       gi_no_minutes_to_expire

s_window_array gstr_window_array[]


//------SECURITY VARIABLES-------
n_PFSecurityStub   G_PFSecurity //security user object
transaction	SECTRANS    //security txn object

//
STRING gs_view_runtime_directory
STRING gs_view_temp_directory

STRING gs_adobe_path

int vk_f3 = 114

s_dde_server_parms   gs_dde_server_parm

s_rcpnt_lst	gs_rcpnt_lst[]
s_rcpnt_lst	gs_empty_lst[]

CONSTANT INTEGER OLE_ERROR = -88

//--------LOGON VARIABLE-------
n_sqlserver		gnv_SqlServer

//--------Variables for used by viewer
STRING gs_appname = "cmwb"

STRING gs_view_setting_directory	= 'HKEY_CURRENT_USER\Software\viewer.settings\'		

/* the following instance variables are to be used for the 
   reference_no project 
*/
BOOLEAN gb_save_registry = TRUE

//COOLMENU
long		gl_menu_style
LONG		gl_menu_selectedcolor
LONG		gl_menu_gradientcolor

/* e-pay test mode  */
INTEGER 	 gi_store_id
STRING    gs_default_printer_key, gs_tiff_save_path,gs_default_printer_value_name
STRING    gs_tiff_printer_name

String	      gs_database_server, gs_database

// CHARACTER FILTERING
STRING	gs_filter[] 

/* annuities */
STRING gs_claim_role_code

/* ePhysio Implementation Date */
Datetime gdtm_ephysio_implementation_date

STRING  gs_email_path
STRING  gs_win_auth_flag

/* logging stuff */
BOOLEAN 	gb_additional_logging = false
STRING           gs_appname_and_handle

// handle of worksheet that was just opened
LONG   gl_current_sheet_handle

end variables

global type cmwb from application
string appname = "cmwb"
end type
global cmwb cmwb

type prototypes
Subroutine keybd_event(uint bVk, uint bScan, long dwFlags, long dwExtraInfo) library "user32.dll"
Function long ShowWindow(ulong whandle,long cmd) Library "user32.dll"
Function Boolean BringWindowToTop (long winhandle) Library "user32.dll"
Function int IsWindow(ulong winhandle) Library "user32.dll"

Function Boolean GetUserNameA (ref String user_id, ref ulong length)  Library "Advapi32.dll" alias for "GetUserNameA;Ansi"
Function int PrintIMA(string PathName) Library "tifprn32.dll" alias for "PrintIMA;Ansi"
Function ulong FindWindowA (string class,string windowname) Library "user32.dll" alias for "FindWindowA;Ansi"
Function int CanAccess (string FileName, int mode) Library "tifprn32.dll" alias for "CanAccess;Ansi"
Function int CreateDirectory(string as_path) library "tifprn32.dll" alias for "CreateDirectory;Ansi"
Function int IMAtoTIFF(string fromfile, string tofile) Library 'tifprn32.dll' alias for "IMAtoTIFF;Ansi"



end prototypes

type variables
uint IApplHandle
end variables

event open;/*	All hardcoding done here!
	------------------------------------------------------------
*/
	vgs_ini_filename 		=	"cmwb.ini"
	
	STRING			ls_additional_logging, ls_logging_ids, ls_null, ls_control_character 		
	LONG 			ll_window_handle
	n_pdf		lnv_pdf
	
/*	Check to see if workbench is already running, Get the user id, and verify the .ini file 
*/
	SetPointer(HourGlass!)
	
	f_user_id(vgst_user_profile.user_id)
	If vgst_user_profile.user_id = "         " THEN
		MessageBox("System Error","Could not determine User Id. Application Terminating. Call Help Desk.",StopSign!)
	 	Return
	End If

	If not fileexists(vgs_ini_filename) then
		MessageBox("System Error","Error locating '" + vgs_ini_filename + "'. Application Terminating. Call Help Desk.",StopSign!)
		Return
	End If

/*	get the default directory inorder to find such things as the help file
*/
	gs_default_dir = profilestring(vgs_ini_filename,"DEFAULT DIRECTORY","defaultdir"," ")
	//Set full path to ini
	vgs_ini_filename = GetCurrentDirectory() + "\" + vgs_ini_filename
	
	if ProfileString(vgs_ini_filename,"COOL MENU","MenuState","OFF") = "ON" THEN
		gl_menu_style = profileint(vgs_ini_filename,"COOL MENU","style",1)
		gl_menu_selectedcolor = Long(ProfileString(vgs_ini_filename,"COOL MENU","SelectedColor",'0'))
		gl_menu_gradientcolor = Long(ProfileString(vgs_ini_filename,"COOL MENU","GradientColor",'0'))
	Else
		gl_Menu_Style = 0 //Don't create the n_coolmenu
	END IF
	
	/* Get the database server from the ini (for development only, blank for Production due to security reasons)*/
	gs_database = ProfileString(vgs_ini_filename,"DATABASE","database", "")
	IF gs_database = '' Then SignalError(-666,'"DATABASE.database" missing from INI file.')
	
	
	//Get the Character filter
	gs_Filter[1] = ProfileString(vgs_ini_filename,"CHARACTER FILTERING","filter_1", "")
	gs_Filter[2] = ProfileString(vgs_ini_filename,"CHARACTER FILTERING","filter_2", "")

	Open(w_startup )

//-----------------------New login script-----------------------
	G_PFSecurity = CREATE N_PFSecurity 	//Security Object.
	gnv_SqlServer = CREATE n_sqlserver	//Login Object.
	
	//Initialize some variables
	gnv_SqlServer.of_set_user_id(vgst_user_profile.user_id)
	gnv_SqlServer.of_set_application_name('WORKBENCH')
	gnv_SqlServer.of_set_application_ini(vgs_ini_filename)
	
	gnv_SqlServer.of_login()
	
	//Get the transactions and connect
	gnv_SqlServer.of_get_transaction('CLAIM',SQLCA)
	SQLCA.nf_connect()

	ImageTrans = CREATE n_transaction
	gnv_SqlServer.of_get_transaction('CLAIM',ImageTrans)
	ImageTrans.nf_connect()

	
	//Get database server name
	gs_database_server = gnv_SqlServer.of_get_server_name(gs_database)
	
	//Load the scurity object with secured controls
	gnv_SqlServer.of_initialize_security(G_PFSecurity)	
	
	gnv_SqlServer.of_end_login()
//---------------------End new login script----------------------


//------------------Code added for new viewer--------------------
	gs_view_runtime_directory = ProfileString(vgs_ini_filename,"VIEWER RUNTIME","ViewDir", "")

	if gs_view_runtime_directory= "" then
		MessageBox("System Error","Could not determine Viewer Run Time Application Terminating. Call Help Desk.",StopSign!)
		halt close
	End If

	IF w_startup.cb_cleanup_viewer_files.TriggerEvent(clicked!) < 0 THEN
		HALT CLOSE
	END IF		
//---------------End of code added for new viewer----------------

	
	
	Close(w_startup)
	
/*	Check Service Level - If there were no database connections established, don't continue
	If full services are not available, prompt to see if the user wishes to continue with minimal service 
*/
	If not SQLCA.ServiceAvailable() and &
		not ImageTrans.ServiceAvailable() Then
		Beep(2)
		MessageBox("System Error","All systems are currently down.  Application Terminating.  Please call the help desk.",StopSign!)
		Halt Close
	End If

	String vls_service_list = ""
	If not SQLCA.ServiceAvailable() then 
		vls_service_list = vls_service_list + "~r~nClaim Database:~r~n   Error Number - ~t" + string(SQLCA.SQLdbcode) + "~r~n" + &
								"   Message - ~t" + Left(SQLCA.SQLErrText,55) + "..."
	End If
	If not ImageTrans.ServiceAvailable() then 
		vls_service_list = vls_service_list + "~r~nImaging Database:~r~n   Error Number - ~t" + string(ImageTrans.SQLdbcode) + "~r~n" + &
								"   Message - ~t" + Left(ImageTrans.SQLErrText,55) + "..."
	End If

	If not vls_service_list = "" Then
		Beep(2)
		If MessageBox("System Error","The following Service is not available:~r~n" + vls_service_list + &
							"~r~n~r~nIf you continue, certain menu items will not be available. Do you wish to continue?",Question!,YesNo!) = 2 Then
			Halt Close
		End If
	End If
	
/* Retrieve and store user profile data if the claim database is available 
*/
	If SQLCA.ServiceAvailable() Then
		If f_user_profile() = -1 Then
			HALT CLOSE
		End If
	End If
	
	gnv_user_authorizations = CREATE n_user_authorizations
	//This call will load all authorizations for the user
	gnv_user_authorizations.nf_set_user_id(vgst_user_profile.user_id)
	
/* Set the ePhysio Implementation Date */
   gdtm_ephysio_implementation_date = Datetime(Date(ProfileString(vgs_ini_filename, "ePhysio", "Physio_eBilling_Date", "")))	

	/* set the no minutes for workflow token to expire */
gi_no_minutes_to_expire =ProfileInt(vgs_ini_filename, "workflow token", "no_minutes_to_expire_invite", 0)
	
	
	
/* Set the email path */
	gs_email_path = ProfileString(vgs_ini_filename, 'EMAIL APP','email_app','')
	IF gs_email_path = '' OR IsNull(gs_email_path) THEN
		MessageBox('Email Path Error','The path to the email application cannot be found. Please contact HelpDesk.', StopSign!)
		RETURN
	END IF

	// set up the executeable path to Adobe Reader
	lnv_pdf = Create n_pdf
	lnv_pdf.nf_set_adobe_path(gs_adobe_path)
	
/*	Open the frame and open the first copy of the worksheet 
*/
	Open (w_frame)
	
	// grab and populate the additional logging ini file entries
	ls_additional_logging 	= trim( ProfileString(vgs_ini_filename,"LOGGING","additional_logging", "") )
	ls_logging_ids 				= trim( ProfileString(vgs_ini_filename,"LOGGING","logging_userids", "") )
	ls_control_character  		= trim( ProfileString(vgs_ini_filename,"LOGGING","control_character", "") )
	
	//DETERMINE IF WE DO ADDITIONAL LOGGING 
	IF upper(ls_additional_logging) = 'Y' THEN 
		
		IF  ls_logging_ids <> '' THEN 
			gb_additional_logging = 	f_do_enhanced_logging( ls_control_character, ls_logging_ids )
		ELSE
			gb_additional_logging = TRUE
		END IF 
		
	ELSE
		gb_additional_logging = FALSE
	END IF 
	
	// SET UP THE LOGGING IF APPROPRIATE
	setnull(ls_null)
	
	ll_window_handle = FindWindowA(ls_null,"Case Management Work Bench")
			
	gs_appname_and_handle = gs_appname +  ' : ' +  string(ll_window_handle)
	
	//probably don't need this......
	IF trim(gs_appname_and_handle) = '' OR isnull(gs_appname_and_handle) THEN
		gs_appname_and_handle = gs_appname
	END IF 
	
	// write to the application log
	N_OBJECTHELPER lnv_object_helper
	f_populate_app_log(  gs_appname_and_handle , 900 , 'App - Open',  'Application - Open')
		
	m_frame.m_workbench.m_worksheet.TriggerEvent(Clicked!)

// for nf_handle_error test, import the testing window w_test_nf_handle_error
// and uncomment the next line
//OPENWITHPARM(w_test_nf_handle_error,'CMWB')

	SetPointer(Arrow!)

end event

event systemerror;INTEGER    li_trancount

	If SQLCA.ServiceAvailable() Then
		SQLCA.nf_transaction_count(li_trancount,1,'','','',FALSE)
		// ONLY rollback if trancount > 0
		IF li_trancount > 0 THEN
			// Rollback a transaction
			EXECUTE IMMEDIATE 'ROLLBACK TRANSACTION' USING SQLCA;
		END IF
	End If

li_trancount = 0

	If IsValid(ImageTrans) Then
		If ImageTrans.ServiceAvailable() Then
			ImageTrans.nf_transaction_count(li_trancount,1,'','','',FALSE)
			// ONLY rollback if trancount > 0
			IF li_trancount > 0 THEN
				// Rollback a transaction
				EXECUTE IMMEDIATE 'ROLLBACK TRANSACTION' USING ImageTrans;
			END IF
		End If
	End IF
	
	open(w_error)


Halt Close
end event

event close;/*	APPLICATION SECURITY CODE */

	If IsValid(G_PFSecurity) Then
		destroy G_PFSecurity        //destroy Security Object when exiting application
	End If

/*	Disconnect databases */

	If SQLCA.ServiceAvailable() Then
		Disconnect using SQLCA;
	End If

	If IsValid(ImageTrans) Then
		If ImageTrans.ServiceAvailable() Then
			disconnect using ImageTrans;
		End If
	End IF




end event

on cmwb.create
appname="cmwb"
message=create message
sqlca=create n_transaction
sqlda=create dynamicdescriptionarea
sqlsa=create dynamicstagingarea
error=create n_dw_error
end on

on cmwb.destroy
destroy(sqlca)
destroy(sqlda)
destroy(sqlsa)
destroy(error)
destroy(message)
end on

