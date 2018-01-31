$PBExportHeader$w_frame.srw
$PBExportComments$Application Driver - frame window
forward
global type w_frame from w_ancestor
end type
end forward

global type w_frame from w_ancestor
integer height = 3172
string title = "Case Management Work Bench"
long backcolor = 276856960
boolean toolbarvisible = false
end type
global w_frame w_frame

type variables
//     Set up the menu instance for the frame
n_coolmenu		inv_coolmenu


m_frame  im_menu

end variables

forward prototypes
public subroutine wf_remove_orphaned_email_attachments ()
end prototypes

public subroutine wf_remove_orphaned_email_attachments ();/* In cases where email attachments were created and the application failed leaving the email attachments in place
	we want to delete these files from the appropriate directory, therfore on the open
	of this this application we will look in the directory based on the userid
	and remove any files that exist there
*/
n_filesys 		ln_fsys
STRING 		ls_path, ls_name[], ls_source_location, ls_destination_location, ls_directory, ls_file_to_delete, ls_user_profile_default
DATETIME 	ldt_write[]
BOOLEAN 	lb_subdir[], lb_test, lb_directory_exists
DOUBLE 		ld_size[]
INTEGER 	li_cnt, li_max, li_counter

ls_source_location = ProfileString(vgs_ini_filename,"PDF CONVERTER","source_directory", " ")

IF ls_source_location = "" THEN
	MessageBox("System Error",'Could not determine PDF CONVERTER Source location: ' +  ls_source_location + " Call Help Desk.",StopSign!)
	RETURN 
END IF

ls_destination_location = ProfileString(vgs_ini_filename,"PDF CONVERTER","destination_directory", " ")

IF ls_destination_location = "" THEN
	MessageBox("System Error",'Could not determine PDF CONVERTER Destination location: ' +  ls_destination_location + " Call Help Desk.",StopSign!)
	RETURN 
END IF

ls_user_profile_default = ProfileString(vgs_ini_filename,"PDF CONVERTER","user_profile_default", " ")

IF ls_user_profile_default = "" THEN
	MessageBox("System Error","Could not determine PDF CONVERTER user profile default. Call Help Desk.",StopSign!)
	RETURN 
END IF

//find the user default in the source & destination
IF Match ( ls_source_location, ls_user_profile_default ) = FALSE THEN 
	MessageBox("System Error",'Could not determine PDF CONVERTER Source location/User Profile Default: ' +  ls_source_location + " Call Help Desk.",StopSign!)
	RETURN 
END IF 

IF Match ( ls_destination_location, ls_user_profile_default ) = FALSE THEN 
	MessageBox("System Error",'Could not determine PDF CONVERTER Destination location/User Profile Default: ' +  ls_destination_location + " Call Help Desk.",StopSign!)
	RETURN 
END IF 

ls_source_location 		= Replace ( ls_source_location, pos( ls_source_location,left(ls_user_profile_default,1)), len(ls_user_profile_default) , vgst_user_profile.user_id )
ls_destination_location 	= Replace ( ls_destination_location, pos( ls_destination_location,left(ls_user_profile_default,1)), len(ls_user_profile_default) , vgst_user_profile.user_id )

IF DirectoryExists(ls_destination_location) = FALSE OR  DirectoryExists(ls_source_location) = FALSE THEN 

ELSE
	
	// sorce files
	ls_path = ls_source_location + '\'
	li_max = ln_fsys.of_GetFiles(ls_path, False, ls_name, ld_size, ldt_write, lb_subdir)
	
	FOR li_counter = 1 TO upperbound(ls_name)
		ls_file_to_delete = ls_source_location + '\'+ ls_name[li_counter]
		filedelete(ls_file_to_delete)	
	NEXT
	
	// destination files
	ls_path = ls_destination_location + '\'
	li_max = ln_fsys.of_GetFiles(ls_path, False, ls_name, ld_size, ldt_write, lb_subdir)
	
	FOR li_counter = 1 TO upperbound(ls_name)
		ls_file_to_delete = ls_destination_location + '\'+ ls_name[li_counter]
		filedelete(ls_file_to_delete)	
	NEXT
END IF 






end subroutine

event open;//	Code for application security (note: override ancestor here since the code 
//	for the frame's security is a little bit different
STRING ls_set_settings
n_user_setting lnvo_user_setting

	IF G_PFSecurity.UOF_Check_Access(This) THEN
		This.I_Authorized_Access = True						//declared as an instance variable
	ELSE
		This.I_Authorized_Access = False
		Beep(2)
		MessageBox("NO APPLICATION AUTHORIZATION","You are not authorized to use this application.",StopSign!)	
		HALT CLOSE
	END IF

/* Set up the instance variable for the menu (so that we can refer to the frames menu later)	*/

	im_menu 	=	m_frame
	
if gl_menu_style <> 0 THEN
	inv_CoolMenu.of_SetRequestor( this )
	inv_CoolMenu.of_SetMenu( 'm_frame' )
	inv_CoolMenu.of_SetMenu( 'm_cmwb' )
	inv_CoolMenu.of_SetStyle(gl_menu_style)
	IF gl_menu_style = 4 Then
		If gl_menu_selectedcolor > 0 Then
			inv_coolmenu.of_SetXpSelectedColor(gl_menu_selectedcolor)
		End if
		If gl_menu_gradientcolor > 0 Then
			inv_coolmenu.of_set2k3gradient(gl_menu_gradientcolor,RGB(255,255,225))	
		End if
	End if
End if

/*

NOTE that the following code that changes the frame title bar to include the database server name has been commented.

Modification of the title bar text impacts two elements of functionality:
1. The Viewer application is opened when a folder is chosen in the Indexing module (it is also opened elsewhere, but with no impact).
   This application will return focus from itself (the Viewer) back to WorkBench. This means that the indexers do not have to click
	back on WorkBench throughout the day. This issue is due to a call of FindWindowA (in the Viewer) not finding a window with 
	'Case Management Workbench', which has been modified.

2. When an instance of WorkBench is opened, the function f_populate_app_log will record in APP_COMPONENT_RUN_LOG.application the following:
   'cmwb : ' + the application handle. The call of FindWindowA fails in the same way as above.

*/

//IF UPPER(gs_database_server) = 'SQL04' THEN
//	w_frame.Title = 'Case Management Work Bench - (PROD)' //-- removed for now this will be uncommented once we fiqure out what to do with the viewer
//ELSE
//	w_frame.Title = 'Case Management Work Bench - (' + Upper(gs_database_server) + ')' //-- removed for now this will be uncommented once we fiqure out what to do with the viewer
//END IF

/* Set up the toolbarframetitle and toolbarsheettitle so that only the current	*/
/* menu's toolbar displays in the MDI frame.													*/

	cmwb.toolbarframetitle = "CMWB"
	cmwb.toolbarsheettitle = "CMWB"


/*	Set the global tool bar text attribute to true so that the user will see the	*/
/*	words appear under the picture button on the tool bar.								*/

	cmwb.ToolBarText = True

/*	Check to see if the reconnect option needs to be enabled */

	If SQLCA.ServiceAvailable() = False or ImageTrans.ServiceAvailable() = False Then
		im_menu.m_options.m_reconnectsystems.Enabled = True
		im_menu.m_options.m_reconnectsystems.ToolBarItemVisible = True
	End If

/*	Set up the clock */
	
//	open(w_mdi_clock)
	
/* set the registry settings if applicable */
IF vgst_user_profile.registry_viewer_setting_flag = "Y" THEN 
	lnvo_user_setting = create n_user_setting
   lnvo_user_setting.of_setrequestor(this)
	lnvo_user_setting.of_restore(gs_view_setting_directory + gs_appname + "\")
END IF

/* remove any orphaned email message attachments */
wf_remove_orphaned_email_attachments()




end event

on w_frame.create
call super::create
end on

on w_frame.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
end on

event resize;call super::resize;//If IsValid(w_mdi_clock) then w_mdi_clock.wf_parent_resized()
end event

event moved;call super::moved;//If IsValid(w_mdi_clock) then w_mdi_clock.wf_parent_resized ( )
end event

event closequery;call super::closequery;String ls_setting_string
n_user_setting lnvo_user_setting
INTEGER		li_counter, li_upper, li_rtn

li_upper = UpperBound(gstr_window_array)
IF li_upper > 0 THEN
	IF MessageBox('Close Workbench?','You are about to close the Workbench. Do you want to continue?',Question!,YesNo!,2) = 1 THEN
		FOR li_counter = 1 TO li_upper
			li_rtn = Close(gstr_window_array[li_counter].window_element)
			IF li_rtn = -1 THEN RETURN 1
			// the close event for each window will reduce size of array
			li_counter = 0
			li_upper = li_upper - 1
		NEXT
	ELSE
		RETURN 1
	END IF
END IF
	

IF This.windowstate = Normal! AND vgst_user_profile.registry_viewer_setting_flag = "Y" AND TRIM(gs_appname) <> "" THEN
	// check to see which application this is being called from and set accordingly to that
	ls_setting_string = gs_view_setting_directory + gs_appname + "\"

	lnvo_user_setting = CREATE n_user_setting
	lnvo_user_setting.of_setrequestor(This)
	lnvo_user_setting.of_setpossize()
	lnvo_user_setting.of_save(TRUE, ls_setting_string, "")
END IF 

end event

event close;call super::close;IF gb_additional_logging = TRUE THEN	
	N_OBJECTHELPER lnv_object_helper
	// write to the application log
	f_populate_app_log(gs_appname_and_handle,lnv_object_helper.get_app_component_type(THIS), lnv_object_helper.getpath(THIS), 'close - App Close')
END IF 
end event

