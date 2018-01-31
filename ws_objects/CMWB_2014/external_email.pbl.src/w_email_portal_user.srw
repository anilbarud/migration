$PBExportHeader$w_email_portal_user.srw
$PBExportComments$Window to allow user to send selected documents to a specified category
forward
global type w_email_portal_user from window
end type
type hpb_conversion_progress from hprogressbar within w_email_portal_user
end type
type st_attachements from statictext within w_email_portal_user
end type
type dw_users from u_dw_online within w_email_portal_user
end type
type dw_clinic from u_dw_online within w_email_portal_user
end type
type cbx_send_to_general from checkbox within w_email_portal_user
end type
type cb_send_ok from commandbutton within w_email_portal_user
end type
type cb_send_cancel from commandbutton within w_email_portal_user
end type
type oleobject_1 from oleobject within w_email_portal_user
end type
end forward

global type w_email_portal_user from window
integer x = 2199
integer y = 688
integer width = 2610
integer height = 1584
boolean titlebar = true
string title = "Select Email Recipients"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 67108864
event uce__conversion_started pbm_custom01
event uce_conversion_failed pbm_custom02
event uce_conversion_completed pbm_custom03
event uce_file_converted pbm_custom04
hpb_conversion_progress hpb_conversion_progress
st_attachements st_attachements
dw_users dw_users
dw_clinic dw_clinic
cbx_send_to_general cbx_send_to_general
cb_send_ok cb_send_ok
cb_send_cancel cb_send_cancel
oleobject_1 oleobject_1
end type
global w_email_portal_user w_email_portal_user

type variables
BOOLEAN			I_Authorized_Access
LONG           		il_claim_no
STRING				is_filename[], is_pdf_filename[], is_parsed_filenames[], is_source_location, is_destination_location
W_SHEET			iw_sheet
n_resize 				inv_resize
n_pdf_converter  	in_pdf_converter
end variables

forward prototypes
public function string wf_parse_email_filename (string as_full_path)
public function string wf_create_pdf_filename (string as_filename)
public subroutine wf_fail_conversion ()
public subroutine wf_delete_all_files_in_folders (string as_source, string as_destination)
public function string wf_create_subject_line ()
end prototypes

event uce_conversion_failed;messagebox('conversion_failed','conversion_failed')
end event

event uce_conversion_completed;hpb_conversion_progress.Position = 100
hpb_conversion_progress.visible = FALSE
cb_send_ok.enabled = true
end event

event uce_file_converted;hpb_conversion_progress.stepit()

end event

public function string wf_parse_email_filename (string as_full_path);LONG			ll_filename_start_pos, ll_dot_pos

ll_dot_pos    = pos(as_full_path,'.')
IF ll_dot_pos = 0 THEN RETURN ''

ll_filename_start_pos = LastPos(as_full_path,'\') + 1

RETURN Mid(as_full_path,ll_filename_start_pos )
end function

public function string wf_create_pdf_filename (string as_filename);/* returns the new pdf name */
INTEGER		li_pos_of_dot
STRING		ls_replacement, ls_new_string


ls_replacement = 'pdf'

//Pos ( string1, string2 {, start } )
li_pos_of_dot = pos(as_filename, '.')

//Mid("BABE RUTH", 40, 5)
ls_new_string = MID(as_filename, 1 ,li_pos_of_dot)

ls_new_string = ls_new_string + ls_replacement

RETURN ls_new_string
end function

public subroutine wf_fail_conversion ();cb_send_ok.enabled = FALSE
timer(0)
messagebox('Converting Documents','The documents(s) Number(s) are/is taking too long to convert.' +&
                    '~rPlease close the window and try again with less documents')
						  					  
RETURN
end subroutine

public subroutine wf_delete_all_files_in_folders (string as_source, string as_destination);/* In cases where email attachments were created and the application failed leaving the email attachments in place
	we want to delete these files from the appropriate directory, therfore on the open
	of this this application we will look in the directory based on the userid
	and remove any files that exist there
*/
n_filesys 		ln_fsys
STRING 		ls_path, ls_name[], ls_file_to_delete
DATETIME 	ldt_write[]
BOOLEAN 	lb_subdir[]
DOUBLE 		ld_size[]
INTEGER 	li_max, li_counter

IF DirectoryExists(as_destination) = FALSE OR  DirectoryExists(as_source) = FALSE THEN 

ELSE
	
	// sorce files
	ls_path = as_source + '\'
	li_max = ln_fsys.of_GetFiles(ls_path, False, ls_name, ld_size, ldt_write, lb_subdir)
	
	FOR li_counter = 1 TO upperbound(ls_name)
		ls_file_to_delete = as_source + '\'+ ls_name[li_counter]
		filedelete(ls_file_to_delete)	
	NEXT
	
	// destination files
	ls_path = as_destination + '\'
	li_max = ln_fsys.of_GetFiles(ls_path, False, ls_name, ld_size, ldt_write, lb_subdir)
	
	FOR li_counter = 1 TO upperbound(ls_name)
		ls_file_to_delete = as_destination + '\'+ ls_name[li_counter]
		filedelete(ls_file_to_delete)	
	NEXT
END IF 






end subroutine

public function string wf_create_subject_line ();STRING		ls_last_name, ls_given_names, ls_subject

//grab basic claim information
SELECT 	b.given_names, b.last_name
INTO 		:ls_given_names, :ls_last_name 
FROM 	CLAIM 		 	a
	JOIN 	INDIVIDUAL  	b on a.individual_no = b.individual_no
WHERE 	a.claim_no = :il_claim_no
USING	SQLCA;
SQLCA.nf_handle_error("w_email_portal_user","wf_create_subject_line()","SELECT 	given_names, last_name")

// create the subject line
IF ISNULL(il_claim_no) OR il_claim_no < 1 THEN il_claim_no = 0

IF ISNULL(ls_last_name) THEN ls_last_name = ''
IF ISNULL(ls_given_names) THEN ls_given_names = ''

ls_subject = ls_given_names + ' ' + ls_last_name + ' ( ' + string(il_claim_no) + ' )'
IF ISNULL(ls_subject) THEN ls_subject = ''

RETURN ls_subject
end function

event open;/* APPLICATION SECURITY CODE. */
G_PFSecurity.UOF_Check_Access(THIS)
THIS.I_Authorized_Access = TRUE				//declared as an instance variable.

LONG				ll_rowcount, ll_results, ll_rtn
INTEGER			li_counter, li_FileNum, li_return, li_major_revision, li_row
STRING 			ls_application_location_45, ls_source_location, ls_destination_location, ls_application_location, ls_registry_location
STRING			ls_parsed_filenames[], ls_user_profile_default, ls_application_location_40, ls_document_limit, ls_mode, ls_os_dotnet_version
STRING			ls_registry_value_name, ls_test_mode_asnyc
DECIMAL          ldec_version
BOOLEAN		lb_check
s_window_message 	lstr_message 
DATAWINDOWCHILD	ldwc_dropdown
environment 			env

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


lstr_message= Message.PowerObjectParm

//grab a couple things from the message object
ls_mode 		= lstr_message.as_mode
il_claim_no 	= lstr_message.al_doubleparm[1]

FOR li_counter = 1 TO upperbound(lstr_message.as_stringparm)
	is_filename[li_counter] = lstr_message.as_stringparm[li_counter]
NEXT

dw_users.settransobject(sqlca)
dw_clinic.settransobject(SQLCA)
li_row =dw_clinic.insertrow(0)

dw_clinic.setitem(li_row,'name','Please select a Provider...')

dw_clinic.scrolltorow(li_row)

/* from the .ini file the exe used to run the convert */
//used for new os  NOT Implemented right now
ls_application_location_45 = ProfileString(vgs_ini_filename,"PDF CONVERTER","application_async", " ")
IF ls_application_location_45 = "" THEN
	MessageBox("System Error","Could not determine Application. Call Help Desk.",StopSign!)
	CLOSE(THIS)
	RETURN -1 
END IF

//syncr used for old environments - i.e. windows xp
ls_application_location_40 = ProfileString(vgs_ini_filename,"PDF CONVERTER","application_4.0", " ")
IF ls_application_location_40 = "" THEN
	MessageBox("System Error","Could not determine Application. Call Help Desk.",StopSign!)
	CLOSE(THIS)
	RETURN -1 
END IF

/* from the .ini file source files copied from the network drive */
ls_source_location = ProfileString(vgs_ini_filename,"PDF CONVERTER","source_directory", " ")
IF ls_source_location = "" THEN
	MessageBox("System Error","Could not determine Source. Call Help Desk.",StopSign!)
	CLOSE(THIS)
	RETURN -1 
END IF

/* from the .ini converted files in the form of .pdf */
ls_destination_location = ProfileString(vgs_ini_filename,"PDF CONVERTER","destination_directory", " ")
IF ls_destination_location = "" THEN
	MessageBox("System Error","Could not determine Destination. Call Help Desk.",StopSign!)
	CLOSE(THIS)
	RETURN -1 
END IF

/* from the .ini this helps tell what to look for and replace in the .ini file for source &  location*/
ls_user_profile_default = ProfileString(vgs_ini_filename,"PDF CONVERTER","user_profile_default", " ")
IF ls_user_profile_default = "" THEN
	MessageBox("System Error","Could not determine user profile default. Call Help Desk.",StopSign!)
	CLOSE(THIS)
	RETURN -1 
END IF

/* from the .ini file source files copied from the network drive */
ls_registry_location = ProfileString(vgs_ini_filename,"PDF CONVERTER","registry_location", " ")
IF ls_registry_location = "" THEN
	MessageBox("System Error","Could not determine registry_location Call Help Desk.",StopSign!)
	CLOSE(THIS)
	RETURN -1 
END IF

///* from the .ini file source files copied from the network drive */
ls_registry_value_name = ProfileString(vgs_ini_filename,"PDF CONVERTER","registry_value_name", " ")
IF ls_registry_value_name = "" THEN
	MessageBox("System Error","Could not determine Registry_value_name Call Help Desk.",StopSign!)
	CLOSE(THIS)
	RETURN -1 
END IF

//find the user default in the source & destination
IF Match ( ls_source_location, ls_user_profile_default ) = FALSE THEN 
	MessageBox("System Error",'Could not determine PDF CONVERTER Source location/user profile default: ' +  ls_source_location + " Call Help Desk.",StopSign!)
	CLOSE(THIS)
	RETURN  -1
END IF 

IF Match ( ls_destination_location, ls_user_profile_default ) = FALSE THEN 
	MessageBox("System Error",'Could not determine PDF CONVERTER Destination location/user profile default: ' +  ls_destination_location + " Call Help Desk.",StopSign!)
	CLOSE(THIS)
	RETURN -1
END IF 

ls_test_mode_asnyc = ProfileString(vgs_ini_filename,"PDF CONVERTER","test_mode_asnyc", " ")
//IF ls_test_mode_asnyc = "" THEN
//	MessageBox("System Error","Could not determine TEST MODE Call Help Desk.",StopSign!)
//	CLOSE(THIS)
//	RETURN -1 
//END IF

/* this object calls the pdf_converters run function */
in_pdf_converter = create n_pdf_converter

//grab the os so that we know which exe to use
ll_rtn = GetEnvironment(env)

IF ll_rtn <> 1 THEN 
	MessageBox("System Error","Could not determine the operating system. Call Help Desk.",StopSign!)
	CLOSE(THIS)
	RETURN -1 
END IF 


ll_rtn = RegistryGET(ls_registry_location, ls_registry_value_name, RegString!, ls_os_dotnet_version)

ldec_version = dec(left(ls_os_dotnet_version,3)) 

IF ldec_version >= 4.5 THEN 
	ls_application_location = ls_application_location_45	
ELSE
	ls_application_location = ls_application_location_40	
END IF 

IF upper(ls_test_mode_asnyc) = 'N' THEN 
		ls_application_location = ls_application_location_40	
ELSEIF upper(ls_test_mode_asnyc) = 'Y' THEN 
	ls_application_location = ls_application_location_45	
ELSE
	//do nothing
END IF 

IF isnull(ls_application_location) OR ls_application_location = '' THEN 
	MessageBox("System Error","Could not determine the location for the application that does the pdf conversion. Call Help Desk.",StopSign!)
	CLOSE(THIS)
	RETURN -1 	
END IF 

/* create the source and destination folder for the current user */
ls_source_location 		= Replace ( ls_source_location, pos( ls_source_location,'{'), len(ls_user_profile_default) , vgst_user_profile.user_id )
ls_destination_location 	= Replace ( ls_destination_location, pos( ls_destination_location,'{'), len(ls_user_profile_default) , vgst_user_profile.user_id )

//make sure we are working with no files!
wf_delete_all_files_in_folders(ls_source_location, ls_destination_location)

// create the folders if they dont exist -- source folder
ll_rtn = CanAccess(ls_source_location, 00)
IF ll_rtn <> 0 THEN
	ll_rtn = CreateDirectory(ls_source_location)
	IF ll_rtn <> 0 THEN
		MessageBox("ERROR","Problem creating Tif Source directory " + ls_source_location  ,StopSign!)
		CLOSE(THIS)
		RETURN -1
	END IF
END IF

// destination folder
ll_rtn = CanAccess(ls_destination_location, 00)
IF ll_rtn <> 0 THEN
	ll_rtn = CreateDirectory(ls_destination_location)
	IF ll_rtn <> 0 THEN
		MessageBox("ERROR","Problem creating Tif Destination directory " + ls_destination_location  ,StopSign!)
		CLOSE(THIS)
		RETURN -1
	END IF
END IF

// copy the files from the network to the source
FOR li_counter = 1 TO upperbound(is_filename[])
	ls_parsed_filenames[li_counter] 	= 	wf_parse_email_filename(is_filename[li_counter])	
	li_FileNum 								= 	FileCopy (is_filename[li_counter] ,  ls_source_location + '\' + ls_parsed_filenames[li_counter] , FALSE)
	is_parsed_filenames[li_counter] 	=  ls_source_location + '\' + ls_parsed_filenames[li_counter] 
	
	//if the mode is not PDF simply copy the files to the destination
	IF ls_mode = 'PDF' THEN 
		is_pdf_filename[li_counter] 		= 	ls_destination_location + '\' + wf_create_pdf_filename( ls_parsed_filenames[li_counter])
		
		
		IF lower(right(is_parsed_filenames[li_counter],3)) = 'pdf' THEN
			ll_rtn = FileCopy (is_parsed_filenames[li_counter], is_pdf_filename[li_counter])
			IF ll_rtn <> 1  THEN 
				MessageBox("ERROR",'Problem copying files from ' + is_parsed_filenames[li_counter] + ' To ' + is_pdf_filename[li_counter] + '~r Please contact the helpdesk'  ,StopSign!)
				CLOSE(THIS)
				RETURN -1
			END IF
		END IF
	ELSE
		/*
		Integer. Returns values as follows:
				1 - Success
				-1 - Error opening sourcefile
				-2 - Error writing targetfile
		*/
		ll_rtn = FileCopy (  ls_source_location + '\' + ls_parsed_filenames[li_counter] ,  ls_destination_location + '\' + ls_parsed_filenames[li_counter] )
		IF ll_rtn <> 1  THEN 
			MessageBox("ERROR","Problem copying files from " +  ls_source_location + '\' + ls_parsed_filenames[li_counter]  + ' To ' + ls_destination_location + '\' + ls_parsed_filenames[li_counter] + '~r Please contact the helpdesk'  ,StopSign!)
			CLOSE(THIS)
			RETURN -1
		END IF 
	
		is_pdf_filename[li_counter] =  ls_destination_location + '\' + ls_parsed_filenames[li_counter] 
	END IF 
NEXT

//set the progressbar stuff
IF upperbound(is_pdf_filename) > 4 THEN 
	hpb_conversion_progress.visible = TRUE
	hpb_conversion_progress.setstep = (100 / upperbound(is_pdf_filename))
ELSE 
	hpb_conversion_progress.visible = FALSE
END IF 

IF  ls_mode = 'PDF' THEN 
//	//convert the files they should now be in the destination folder
//	IF INTEGER(ls_timer) > 0 THEN 
//		timer(INTEGER(ls_timer) )
//	END IF 
	
	//DO THE CONVERSION
	is_source_location 			= ls_source_location
	is_destination_location 		= ls_destination_location
	li_return = in_pdf_converter.nf_convert_to_pdf(ls_source_location, ls_destination_location, ls_application_location )
	IF li_return <> 1 THEN 
		MessageBox("ERROR","Problem creating the pdf file(s) " + ls_destination_location + '. Please contact the helpdesk'  ,StopSign!)
		CLOSE(THIS)
		RETURN -1
	END IF 
ELSE
	cb_send_ok.enabled = TRUE
END IF 
	

















end event

event closequery;INTEGER	li_parsed_count, li_source_count, li_destination_count, li_counter

li_source_count 		= upperbound(is_parsed_filenames)
li_destination_count 	= upperbound(is_pdf_filename)

IF li_source_count <> li_destination_count  THEN 
	messagebox('File Count Error', 'There was an error in the number of files to delete. Please contact the helpdesk.')
	//return -1
END IF 

FOR li_counter = 1 TO li_source_count
	IF FileDelete(is_parsed_filenames[li_counter] ) = FALSE THEN
		// need to either crash here or rely on next open of the application to cleanup the files
	END IF 
NEXT

FOR li_counter = 1 TO li_destination_count
	IF FileDelete(is_pdf_filename[li_counter] ) = FALSE THEN
		// need to either crash here or rely on next open of the application to cleanup the files
	END IF 
NEXT





end event

on w_email_portal_user.create
this.hpb_conversion_progress=create hpb_conversion_progress
this.st_attachements=create st_attachements
this.dw_users=create dw_users
this.dw_clinic=create dw_clinic
this.cbx_send_to_general=create cbx_send_to_general
this.cb_send_ok=create cb_send_ok
this.cb_send_cancel=create cb_send_cancel
this.oleobject_1=create oleobject_1
this.Control[]={this.hpb_conversion_progress,&
this.st_attachements,&
this.dw_users,&
this.dw_clinic,&
this.cbx_send_to_general,&
this.cb_send_ok,&
this.cb_send_cancel}
end on

on w_email_portal_user.destroy
destroy(this.hpb_conversion_progress)
destroy(this.st_attachements)
destroy(this.dw_users)
destroy(this.dw_clinic)
destroy(this.cbx_send_to_general)
destroy(this.cb_send_ok)
destroy(this.cb_send_cancel)
destroy(this.oleobject_1)
end on

event timer;//BOOLEAN lb_check
//
//lb_check = in_pdf_converter.nf_confirm_files_converted(is_source_location, is_destination_location)
//
//IF lb_check = FALSE THEN 
//	wf_fail_conversion()
//	RETURN -1
//END IF 
end event

type hpb_conversion_progress from hprogressbar within w_email_portal_user
boolean visible = false
integer x = 23
integer y = 1340
integer width = 1010
integer height = 68
unsignedinteger maxposition = 100
integer setstep = 10
boolean smoothscroll = true
end type

type st_attachements from statictext within w_email_portal_user
boolean visible = false
integer x = 23
integer y = 1228
integer width = 2601
integer height = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean focusrectangle = false
end type

type dw_users from u_dw_online within w_email_portal_user
integer x = 14
integer y = 156
integer width = 2555
integer height = 1160
integer taborder = 20
string title = "Email"
string dataobject = "d_ephysio_users"
boolean vscrollbar = true
end type

event constructor;call super::constructor;THIS.uf_setselect(3)
end event

type dw_clinic from u_dw_online within w_email_portal_user
integer y = 24
integer width = 2574
integer height = 88
integer taborder = 10
string dataobject = "d_ephysio_provider_controller"
boolean border = false
boolean livescroll = false
end type

event constructor;call super::constructor;Integer li_row

THIS.uf_setselect(3)



end event

event ue_itemchangeaccepted;call super::ue_itemchangeaccepted;/* 
Populates the users based on the Provider	

SELECT provider_no,            	provider_type_code,            provider_sub_type_code,   
         	name,            			sort_name,            			email_address,   
         	ephysio_flag  
FROM 	PROVIDER   
WHERE 	ephysio_flag 	= 'Y'
AND 		active_flag 		= 'Y'
*/
datawindowchild 	ldwc_provider
INTEGER				li_rows,  li_provider_row
LONG					ll_provider_no
STRING				ls_name

THIS.getchild('name', ldwc_provider)

li_rows 				= ldwc_provider.rowcount()
li_provider_row 	= ldwc_provider.getrow()

IF li_rows > 0 THEN 
	ll_provider_no 	= ldwc_provider.getitemnumber(li_provider_row,'provider_no')
	ls_name           =  ldwc_provider.getitemstring(li_provider_row,'name')
END IF 

/* do the retrieve for the users */
li_rows = dw_users.retrieve(ll_provider_no)
SQLCA.nf_handle_error("w_email_portal_user","dw_users","rowfocuschanged")

IF li_rows <= 0 THEN 
	messagebox('No Rows','No rows were found for Provider: ' + ls_name)	
END IF

dw_users.selectrow(0,false)
cbx_send_to_general.checked= false
end event

type cbx_send_to_general from checkbox within w_email_portal_user
integer x = 1847
integer y = 1324
integer width = 727
integer height = 76
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "  Include General Contact"
end type

event clicked;INTEGER	li_counter, li_total_count, li_claim_value, li_rowcount

li_total_count 	= 0
li_rowcount 		= dw_users.rowcount()

IF li_rowcount <= 0 THEN 
	THIS.CHECKED = FALSE
	RETURN 
END IF 

IF THIS.checked = TRUE THEN
	
	//default to no rows selected
//	dw_users.selectrow(0, FALSE)
		
	//claim value 2 is general contact
	FOR li_counter = 1 TO li_rowcount
		li_claim_value = dw_users.getitemnumber(li_counter,'general_email_contact_flag_enum')
		
		
		IF li_claim_value = 1 THEN 
			dw_users.selectrow(li_counter, TRUE)
			li_total_count ++
		END IF 		
	NEXT	
	
	IF li_total_count = 0 THEN 
		messagebox('No General Contacts' , 'There were no General Contacts found for this clinic. Please select one or more email recipients from the list.') 
		THIS.checked = FALSE
		RETURN
	END IF 		
ELSE
	
	//claim value 2 is general contact
	FOR li_counter = 1 TO li_rowcount
		li_claim_value = dw_users.getitemnumber(li_counter,'general_email_contact_flag_enum')
		
		IF li_claim_value = 1 THEN 
			dw_users.selectrow(li_counter, FALSE)
		END IF	
	NEXT		
END IF
end event

type cb_send_ok from commandbutton within w_email_portal_user
integer x = 919
integer y = 1324
integer width = 334
integer height = 88
integer taborder = 30
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean underline = true
boolean enabled = false
string text = "&OK"
boolean default = true
end type

event clicked;INTEGER					li_rc, li_recipient_count, li_counter
LONG						ll_attachment_count
STRING					ls_test, ls_sendto, ls_recipients[], ls_error, ls_subject
BOOLEAN				   lb_check
OLEObject 				l_oleApplication,  l_olMailItem, l_attachment

setpointer(hourglass!)

lb_check = in_pdf_converter.nf_confirm_files_converted(is_source_location, is_destination_location)
IF lb_check = FALSE THEN
	messagebox('Converting Documents','Please wait for the documents to be converted. Try again in 5 seconds.')
	RETURN
END IF 

ls_subject = wf_create_subject_line()

//e.g   Subject:     Jane Leblanc  (888555)
ls_subject 	=  ls_subject

// Connect to Outlook
l_oleApplication = CREATE OLEObject
li_rc = l_oleApplication.ConnectToNewObject("Outlook.Application")
IF li_rc < 0 THEN 
	MessageBox("Error connecting to Outlook", String(li_rc))
	RETURN 
END IF

l_olMailItem = l_oleApplication.CreateItem(0)//OlItemType Enumeration mailitem

li_recipient_count 	= 1
li_rc 					= dw_users.rowcount()
 
/* load up the recipients with only those that  are selected */
FOR li_counter = 1 TO li_rc
	IF dw_users.ISSELECTED(li_counter) THEN 
		ls_recipients[li_recipient_count] = dw_users.getitemstring(li_counter, 'wif_principal_name')
		li_recipient_count ++
	ELSEIF cbx_send_to_general.checked = true THEN 
		IF dw_users.getitemnumber(li_counter,'general_email_contact_flag_enum') = 1 THEN 
			
			ls_recipients[li_recipient_count] = dw_users.getitemstring(li_counter, 'wif_principal_name')
			li_recipient_count ++
			
		END IF 
	
	END IF 	
NEXT

li_recipient_count = UpperBound(ls_recipients)	

/* make sure there is at least 1 recipient */
IF isnull(li_recipient_count) OR li_recipient_count <= 0 THEN 
	messagebox('Select Recipients', 'No valid Recipients have been selected. Please select a recipient.')
	l_oleApplication.DisconnectObject()
	RETURN -1		
END IF 
		
IF li_recipient_count > 0 THEN
	//Add the recipients to the mail object
	FOR li_counter = 1 TO li_recipient_count
		ls_sendto = ls_recipients[li_counter]
		
		IF isnull(ls_sendto) OR trim(ls_sendto) = '' THEN 
			messagebox('Bad Recipient', 'Recipient: ' + ls_sendto + ' does not exist. Please check that the recipient is correct.')
			l_oleApplication.DisconnectObject()
			RETURN -1		
		END IF 
				
		TRY
				l_olMailItem.Recipients.Add(ls_sendto)
		CATCH (OLERuntimeError MyOLEError) 
			  //	ls_error = ls_sendto + "~t" + MyOLEError.text
		  	ls_error = 'Please ensure outlook is open before proceeding with the creation of this email'
      		messagebox('Recipient not added', ls_error,Exclamation!, OK!, 1)
      		RETURN -1
		END TRY	
	NEXT
END IF
 
/* check for attachments */
ll_attachment_count = UpperBound(is_pdf_filename[] )

/* should be an attachment */
IF isnull(ll_attachment_count) OR ll_attachment_count <= 0 THEN 
	messagebox('Select Attachments', 'No valid Attachments have been selected.')
	l_oleApplication.DisconnectObject()
	RETURN -1		
END IF 

/* add the attachment to the mail message */
IF ll_attachment_count > 0 THEN
	
	l_attachment = l_olMailItem.Attachments
	
	FOR li_counter = 1 TO ll_attachment_count
		ls_test = is_pdf_filename[li_counter]
		
		IF fileexists(ls_test) = FALSE THEN 
			messagebox('Bad File', 'File: ' + ls_test + ' does not exist. Please check that your drives are correctly mapped.')
			l_oleApplication.DisconnectObject()
			RETURN -1		
		END IF 
		
		TRY
					l_attachment.add(ls_test)	
		CATCH (OLERuntimeError AttachmentError) 
			  //	ls_error = ls_sendto + "~t" + MyOLEError.text
		  	ls_error = 'Please ensure outlook is open before proceeding with the creation of this email'
      		messagebox('Attachment not added', ls_error,Exclamation!, OK!, 1)
      		RETURN -1
		END TRY	
	NEXT
END IF
  
/* fill in the rest of the mail message */  
l_olMailItem.Subject 	= ls_subject

/* send workbench to the back */
PARENT.BringToTop = FALSE

/* display the defaulted mail message */ 
l_olMailItem.Display

/* need to do this after the display in order to reconnect the signature to the page */
l_olMailItem.HTMLbody 	= " " + l_olMailItem.HTMLbody 


// this is a test






end event

type cb_send_cancel from commandbutton within w_email_portal_user
event lbuttondown pbm_lbuttondown
integer x = 1266
integer y = 1324
integer width = 334
integer height = 88
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Close"
end type

event lbuttondown;Close(PARENT)
end event

event clicked;Close(PARENT)

end event

type oleobject_1 from oleobject within w_email_portal_user descriptor "pb_nvo" = "true" 
end type

on oleobject_1.create
call super::create
TriggerEvent( this, "constructor" )
end on

on oleobject_1.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

