$PBExportHeader$n_pfsecurity.sru
$PBExportComments$PowerFrame Security Enabling Object; inherited from n_PFSecurityStub
forward
global type n_pfsecurity from n_pfsecuritystub
end type
end forward

global type n_pfsecurity from n_pfsecuritystub
event uoe_closewindow pbm_custom01
event uoe_showwindow pbm_custom02
end type
global n_pfsecurity n_pfsecurity

type variables
// Used to store the security method for the application.
Integer ii_method

// Used to track today's date in order to see whether or not our permissions are in effect.
Datetime idt_today

// Used to store the already loaded windows, controls and columns in order to avoid going to the database again.
Boolean i_loaded_windows_ib_secured[], i_loaded_windows_ib_secured_controls[] 
String  i_loaded_windows_is_window_name[] 
String  i_loaded_controls_is_control_name[], i_loaded_controls_is_control_type[], i_loaded_controls_is_window_name[] 
Boolean i_loaded_columns_ib_authorized_updating[], i_loaded_columns_ib_authorized_viewing[]  
String  i_loaded_columns_is_column_name[], i_loaded_columns_is_database_name[], i_loaded_columns_is_table_name[] 

// Used instead of a cursor to hold the Windows, Controls, Columns while loading the above arrays. 3 Is compatible with all DBMS's
String i_csr_windows_is_window_name[], i_csr_windows_is_secured_controls[] 
String i_csr_controls_is_control_name[], i_csr_controls_is_control_type[], i_csr_controls_is_window_name[] 
String i_csr_columns_is_column_name[], i_csr_columns_is_database_name[], i_csr_columns_is_table_name[] 

// Used to track the curent window.
Window iw_window

// Used to track the name of the current window.
String is_window_name 

// Used to track the current datawindow.
Datawindow idw_datawindow

// Used to store a list of all profiles the user belongs to.
String is_profiles[] 

end variables

forward prototypes
private function boolean uof_check_loaded_controls ()
private function boolean uof_check_nonloaded_controls ()
private function boolean uof_check_nonloaded_windows ()
private function boolean uof_disable_control (string arg_control_name, string arg_control_type)
private function integer uof_load_control (string arg_window_name, string arg_control_name, string arg_control_type)
private function integer uof_load_window (string arg_window_name, boolean arg_secured, boolean arg_secured_controls)
public function boolean uof_check_access (graphicobject arg_graphic_object)
private function integer uof_load_column (string arg_database_name, string arg_table_name, string arg_column_name, boolean arg_authorized_viewing, boolean arg_authorized_updating)
private function boolean uof_check_loaded_windows (ref boolean arg_authorized_access)
public function integer uof_initialize ()
public function integer uof_login (string arg_user_id, string arg_password, string arg_application_name, n_transaction arg_transobject)
private function boolean uof_disable_column (ref datawindow arg_datawindow, string arg_column_name, boolean arg_authorized_viewing, boolean arg_authorized_updating)
private function boolean uof_check_loaded_columns ()
end prototypes

private function boolean uof_check_loaded_controls ();// uof_check_loaded_controls - searches through the list of secured controls for the window 
//                             we're opening and disables them as it finds them.
//
Boolean lb_endloop, lb_found
Integer li_index, li_lower, li_upper

CHOOSE CASE ii_method
	// For security method 2, we need to serially read through the list of previously opened controls since the list is in chronological (as
	// we opened them) order rather than alphabetical order.  If we find controls for the window in the list, then we disable those controls.
	CASE 2
		li_index = 0
		li_upper = UpperBound(i_loaded_controls_is_control_name)
		lb_found = FALSE
		DO
			li_index = li_index + 1
			IF li_index <= li_upper THEN
				IF i_loaded_controls_is_window_name[li_index] = is_window_name THEN
					uof_disable_control(i_loaded_controls_is_control_name[li_index], i_loaded_controls_is_control_type[li_index]) 
				END IF
			END IF
		LOOP UNTIL (li_index > li_upper)

	// For security method 3, we can use a binary search since the list will be alphabetized according to window name.  Check the list of
	// secured controls to find those controls for the window we're opening.  For those controls we find, disable them.
	CASE 3
		li_lower = 1
		li_upper = UpperBound(i_loaded_controls_is_control_name)
		li_index = Round((li_lower + li_upper) / 2,0)
		lb_found = FALSE
		DO WHILE (NOT lb_found) AND (li_lower <= li_upper)
			IF i_loaded_controls_is_window_name[li_index] = is_window_name THEN
				lb_found = TRUE
				// First step backwards through the array to make sure we are on the first control for this window.
				lb_endloop = FALSE
				DO
					li_index = li_index - 1
					IF li_index >= li_lower THEN
						IF i_loaded_controls_is_window_name[li_index] <> is_window_name THEN
							lb_endloop = TRUE
						END IF
					ELSE
						lb_endloop = TRUE
					END IF
				LOOP UNTIL (lb_endloop)

				// Now go forward through the array and disable all controls for the window until we run out of controls for this window.
				lb_endloop = FALSE
				DO 
					li_index = li_index + 1
					IF li_index <= li_upper THEN
						IF i_loaded_controls_is_window_name[li_index] = is_window_name THEN
							uof_disable_control(i_loaded_controls_is_control_name[li_index], i_loaded_controls_is_control_type[li_index])
						ELSE
							lb_endloop = TRUE
						END IF
					ELSE
						lb_endloop = TRUE
					END IF
				LOOP UNTIL (lb_endloop)
			ELSE
				IF i_loaded_controls_is_window_name[li_index] < is_window_name THEN
					li_lower = li_index + 1
				ELSE
					li_upper = li_index - 1
				END IF
				li_index = Round((li_lower + li_upper) / 2,0)
			END IF
		LOOP
END CHOOSE

RETURN TRUE   // Return a success code 

end function

private function boolean uof_check_nonloaded_controls ();// uof_check_nonloaded_controls - first gets a list of secured controls for the window. For each control, it then checks to see if we've been given direct
//                                access to the control.  If we haven't been given direct access to it, it goes through our list of profiles we belong to and checks to see if
//                                any of them have access to it.  If one of them do, then we have access to the control.  Otherwise, we need to disable the control and add it
//                                to our list of secured controls so next time we won't have to go to the database.
// 
//                                Whenever a cursor is opened in this script, it is loaded into an array structure. This is because all cursors are not created equally.  In some RDBMS's, once
//                                a cursor is open, you can't perform any action against the database except another fetch.  So, we are forced to close the cursor if we want to
//                                do any selects before we reach the end of the cursor.
//
Boolean  lb_authorized_access
DateTime ldt_effective_date
Integer  li_temp_counter, li_profile_counter, li_cursor_pos
String l_csr_controls_is_control_name[], l_csr_controls_is_control_type[], l_csr_controls_is_window_name[]   // patch 

// First get a list of all secured controls for the window.
DECLARE Controls_Cursor CURSOR FOR  
SELECT control, control_type 
  FROM pfcontrol 
 WHERE application = :i_application_name 
   AND window = :is_window_name 
	AND status = 'A' 
 ORDER BY control 
 USING I_TransObject ; 

// For each secured control, check first to see if we have direct access to it.
OPEN Controls_Cursor ; 
li_cursor_pos = 1

i_csr_controls_is_control_name[] = l_csr_controls_is_control_name[] // patch
i_csr_controls_is_control_type[] = l_csr_controls_is_control_type[] // patch
i_csr_controls_is_window_name[]  = l_csr_controls_is_window_name[]  // patch 

DO
	FETCH Controls_Cursor INTO :i_csr_controls_is_control_name[li_cursor_pos], :i_csr_controls_is_control_type[li_cursor_pos] ; 
   li_cursor_pos = li_cursor_pos + 1
LOOP UNTIL I_TransObject.SQLCode <> 0

CLOSE Controls_Cursor ; 

li_cursor_pos = 1
IF UpperBound(i_csr_controls_is_control_name) > 0 THEN
DO
   lb_authorized_access = FALSE
	SELECT effective_date 
	  INTO :ldt_effective_date 
	  FROM pfuser_ctrl 
	 WHERE application = :i_application_name 
	   AND window = :is_window_name 
		AND control = :i_csr_controls_is_control_name[li_cursor_pos] 
		AND userid = :i_user_id 
		AND status = 'A' 
	 USING I_TransObject ; 

	// If we have direct access to the control, then there's no need to disable it and there's no need to load it in our list of secured controls. 
	IF I_TransObject.SQLCode = 0 THEN
		IF ldt_effective_date <= idt_today THEN
			lb_authorized_access = TRUE
		END IF
	END IF

	// Else if we don't have direct access to the control (whether it be because the effective dates are no good or because the record
	// could not be found), check our profiles in order to see if any of them have access.  Step through all of our profiles until we
	// find a profile that has access to the control or we run out of profiles.
	IF NOT lb_authorized_access THEN
		li_temp_counter = 1
		li_profile_counter = UpperBound(is_profiles)
		DO
			SELECT effective_date
			  INTO :ldt_effective_date 
			  FROM pfprof_ctrl 
			 WHERE application = :i_application_name 
			   AND window = :is_window_name 
				AND control = :i_csr_controls_is_control_name[li_cursor_pos]  
				AND profile = :is_profiles[li_temp_counter] 
				AND status = 'A' 
			 USING I_TransObject ; 

			IF I_TransObject.SQLCode = 0 THEN
				IF ldt_effective_date <= idt_today THEN
					lb_authorized_access = TRUE
				END IF
			END IF
			li_temp_counter = li_temp_counter + 1
		LOOP UNTIL (lb_authorized_access) OR (li_temp_counter > li_profile_counter)

		// If we ran out of profiles, then we do not have access to the control.  Add the control to the list of secured control.  
		// Reset the SQLCode in order to continue with the next control.
		IF NOT lb_authorized_access THEN
			IF i_csr_controls_is_control_name[li_cursor_pos] <> "" THEN
				uof_disable_control(i_csr_controls_is_control_name[li_cursor_pos], i_csr_controls_is_control_type[li_cursor_pos])
				IF ii_method = 2 THEN
					uof_load_control(is_window_name, i_csr_controls_is_control_name[li_cursor_pos], i_csr_controls_is_control_type[li_cursor_pos])
				END IF
			END IF
		END IF
	END IF
   li_cursor_pos = li_cursor_pos + 1
LOOP UNTIL li_cursor_pos > UpperBound(i_csr_controls_is_control_name)
END IF

RETURN TRUE   // Return a success code

end function

private function boolean uof_check_nonloaded_windows ();// uof_check_nonloaded_windows - first checks to see if the window has been secured or not.  If it has been secured, it checks to see whether or not we have
//                               direct access to it.  If we do, it returns true.  If we don't, it goes through our list of profiles we belong to and checks 
//                               to see if any of them have access to it.  If one of them do, then we have access to the window and it returns true.
//
Boolean  lb_authorized_access
DateTime ldt_effective_date
Integer  li_profile_counter, li_temp_counter
String   ls_secured_code, ls_secured_controls

// First check to see whether or not the window has been secured.
SELECT secured_code, secured_controls 
  INTO :ls_secured_code, :ls_secured_controls 
  FROM pfwindow 
 WHERE application = :i_application_name 
   AND window = :is_window_name 
 USING I_TransObject ; 

// If a record does not exist for the window, then the window has not been secured.  If we're using security method 2, we need to add 
// it to the list of previously opened windows as being not secured and return true since we have access to it.
IF I_TransObject.SQLCode = 100 THEN
	IF ii_method = 2 THEN
		uof_load_window(is_window_name, FALSE, FALSE)
	END IF
	RETURN TRUE
END IF

// If a record does exist for the window, then we need to check the secured code to determine whether or not the window has been secured.
IF I_TransObject.SQLCode = 0 THEN					
	// If it has been secured, we need to go to the database to determine whether or not we or any of our profiles have access to it.
	IF ls_secured_code = "Y" THEN
		// First check to see if we have direct access to the window.
		SELECT effective_date  
		  INTO :ldt_effective_date  
		  FROM pfuser_win  
		 WHERE application = :i_application_name   
		   AND window = :is_window_name 
			AND userid = :i_user_id 
			AND status = 'A' 
	    USING I_TransObject ; 

		// If we have direct access to the window, check the effective date and time to see if it's in effect.  If it is in effect, then we
		// have access to the window, so we need to go on and check to see if any of the controls on the window have been secured.
		IF I_TransObject.SQLCode = 0 THEN
			IF ldt_effective_date <= idt_today THEN
				IF ii_method = 2 THEN
					uof_load_window(is_window_name, FALSE, ls_secured_controls = "Y")
				END IF
				IF ls_secured_controls = "Y" THEN
					uof_check_nonloaded_controls()
				END IF
				RETURN TRUE
			END IF
		END IF

		// Else if we don't have direct access to the window (whether it be because the effective dates are no good or because the record
		// could not be found), check our profiles in order to see if any of them have access.  Step through all of our profiles until we
		// find a profile that has access to the window or we run out of profiles.
		li_temp_counter = 1
		li_profile_counter = UpperBound(is_profiles)
		DO
			SELECT effective_date
			  INTO :ldt_effective_date 
			  FROM pfprof_win
			 WHERE application = :i_application_name 
				AND window = :is_window_name 
				AND profile = :is_profiles[li_temp_counter] 
				AND status = 'A' 
		    USING I_TransObject ; 
			IF I_TransObject.SQLCode = 0 THEN
				IF ldt_effective_date <= idt_today THEN
					lb_authorized_access = TRUE
					IF ii_method = 2 THEN
						uof_load_window(is_window_name, FALSE, ls_secured_controls = "Y")
					END IF
					IF ls_secured_controls = "Y" THEN
						uof_check_nonloaded_controls()
					END IF
				END IF
			END IF
			li_temp_counter = li_temp_counter + 1
		LOOP UNTIL (lb_authorized_access) OR (li_temp_counter > li_profile_counter)

		// If we ran out of profiles, then we do not have access to the window.  Add the window to the list of secured windows.  Reset
		// the SQLCode in order to continue with the next window.
		IF NOT lb_authorized_access THEN
			IF ii_method = 2 THEN
				uof_load_window(is_window_name, TRUE, ls_secured_controls = "Y")
			END IF
			RETURN FALSE
		END IF

	// Else the window has not been secured, so we know we have access to it.  Now, we need to go check the controls on the window to see if any have been secured.
	ELSE
		IF ii_method = 2 THEN
			uof_load_window(is_window_name, FALSE, FALSE)
		END IF
		IF ls_secured_controls = "Y" THEN	
			uof_check_nonloaded_controls()
		END IF
		RETURN TRUE
	END IF
END IF

RETURN TRUE   // Return a success

end function

private function boolean uof_disable_control (string arg_control_name, string arg_control_type);// uof_disable_control - disables the control argument.  If the control is a menu item, it must traverse the menu structure 
//                       in case the menu item is located in a cascading menu.  If the control is not a menu item, it
//                       determines the type of control it is, locates it in the window's control array and disables it.
// 
Boolean lb_found 
Integer li_index, li_index2, li_index3, li_upper, li_upper2, li_upper3 
CheckBox        lcbx_checkbox
CommandButton   lcb_commandbutton
DataWindow      ldw_datawindow
DropDownListBox lddlb_dropdownlistbox
EditMask        lem_editmask	
ListBox         llb_listbox
MultiLineEdit   lmle_multilineedit
Picture         lp_picture
PictureButton   lpb_picturebutton
RadioButton     lrb_radiobutton
SingleLineEdit  lsle_singlelineedit
UserObject      luo_userobject 

// Initialize necessary variables.
arg_control_name = Lower(arg_control_name)
arg_control_type = Lower(arg_control_type)

// If the control we're disabling is a menu, traverse the menu structure in case the menu item is hidden in a cascaded menu.  Once we find the menu item, disable it.
//IF arg_control_type = "menuitem" THEN
//	lb_found = FALSE
//	li_index = 0
//	li_upper = UpperBound(iw_window.MenuId.Item[])
//	DO
//		li_index = li_index + 1
//		IF li_index <= li_upper THEN
//			IF Lower(iw_window.MenuId.Item[li_index].ClassName()) = arg_control_name THEN
//				lb_found = TRUE
//				//	iw_window.MenuId.Item[li_index].Enabled = FALSE
//				iw_window.MenuId.Item[li_index].Visible = FALSE
//				iw_window.MenuId.Item[li_index].ToolBarItemVisible = FALSE
//			ELSE
//				li_index2 = 0
//				li_upper2 = UpperBound(iw_window.MenuId.Item[li_index].Item[])
//				IF li_upper2 > 0 THEN
//					DO
//						li_index2 = li_index2 + 1
//						IF li_index2 <= li_upper2 THEN
//							IF Lower(iw_window.MenuId.Item[li_index].Item[li_index2].ClassName()) = arg_control_name THEN
//								lb_found = TRUE
//								//	iw_window.MenuId.Item[li_index].Item[li_index2].Enabled = FALSE
//								iw_window.MenuId.Item[li_index].Item[li_index2].Visible = FALSE
//								iw_window.MenuId.Item[li_index].Item[li_index2].ToolBarItemVisible = FALSE
//							ELSE
//								li_index3 = 0
//								li_upper3 = UpperBound(iw_window.MenuId.Item[li_index].Item[li_index2].Item[])
//								DO
//									li_index3 = li_index3 + 1
//									IF li_index3 <= li_upper3 THEN
//										IF Lower(iw_window.MenuId.Item[li_index].Item[li_index2].Item[li_index3].ClassName()) = arg_control_name THEN
//											lb_found = TRUE
//											//	iw_window.MenuId.Item[li_index].Item[li_index2].Item[li_index3].Enabled = FALSE
//											iw_window.MenuId.Item[li_index].Item[li_index2].Item[li_index3].Visible = FALSE
//											iw_window.MenuId.Item[li_index].Item[li_index2].Item[li_index3].ToolBarItemVisible = FALSE
//										END IF
//									END IF
//								LOOP UNTIL (lb_found) OR (li_index3 > li_upper3)
//							END IF
//						END IF
//					LOOP UNTIL (lb_found) OR (li_index2 > li_upper2)
//				END IF
//			END IF
//		END IF
//	LOOP UNTIL (lb_found) OR (li_index > li_upper)
//
//// If the control is not a menu item, it determines the type of control it is, locates it in the window's control array and disables it.
//ELSE
//	li_index = 0
//	li_upper = UpperBound(iw_window.Control[])
//	lb_found = FALSE
//	DO
//		li_index = li_index + 1
//		IF li_index <= li_upper THEN
//			IF Lower(iw_window.Control[li_index].ClassName()) = arg_control_name THEN
//				lb_found = TRUE
//				CHOOSE CASE arg_control_type
//					CASE "checkbox"
//						lcbx_checkbox = iw_window.Control[li_index]
//						lcbx_checkbox.Enabled = FALSE
//					CASE "commandbutton"
//						lcb_commandbutton = iw_window.Control[li_index]
//						// lcb_commandbutton.Enabled = FALSE
//						lcb_commandbutton.Visible = FALSE
//					CASE "dropdownlistbox"
//						lddlb_dropdownlistbox = iw_window.Control[li_index]
//						lddlb_dropdownlistbox.Enabled = FALSE
//					CASE "editmask"
//						lem_editmask = iw_window.Control[li_index]
//						lem_editmask.Enabled = FALSE
//					CASE "listbox"
//						llb_listbox = iw_window.Control[li_index]
//						llb_listbox.Enabled = FALSE
//					CASE "multilineedit"
//						lmle_multilineedit = iw_window.Control[li_index]
//						lmle_multilineedit.Enabled = FALSE
//					CASE "picture"
//						lp_picture = iw_window.Control[li_index]
//						lp_picture.Enabled = FALSE
//					CASE "picturebutton"
//						lpb_picturebutton = iw_window.Control[li_index]
//						// lpb_picturebutton.Enabled = FALSE
//						lpb_picturebutton.Visible = FALSE
//					CASE "radiobutton"
//						lrb_radiobutton = iw_window.Control[li_index]
//						lrb_radiobutton.Enabled = FALSE
//					CASE "singlelineedit"
//						lsle_singlelineedit = iw_window.Control[li_index]
//						lsle_singlelineedit.Enabled = FALSE
//					CASE "datawindow"
//						ldw_datawindow = iw_window.Control[li_index]
//						ldw_datawindow.Enabled = FALSE
//					CASE "userobject"
//						luo_userobject = iw_window.Control[li_index]
//						luo_userobject.Enabled = FALSE
//					END CHOOSE
//			END IF
//		ELSE
//			RETURN FALSE
//		END IF
//	LOOP UNTIL (lb_found) OR (li_index > li_upper)
//END IF

RETURN TRUE  // Return a success code

end function

private function integer uof_load_control (string arg_window_name, string arg_control_name, string arg_control_type);// uof_load_control - loads a control to the list of secured controls.
//
Long ll_index

ll_index = UpperBound(i_loaded_controls_is_control_name) + 1
i_loaded_controls_is_window_name[ll_index] = Lower(arg_window_name)
i_loaded_controls_is_control_name[ll_index] = Lower(arg_control_name)
i_loaded_controls_is_control_type[ll_index] = Lower(arg_control_type)

RETURN 0  // Return a success code

end function

private function integer uof_load_window (string arg_window_name, boolean arg_secured, boolean arg_secured_controls);// uof_load_window - loads and window and its settings to the list of secured windows.
// 
Long ll_index 

ll_index = UpperBound(i_loaded_windows_is_window_name) + 1 
i_loaded_windows_is_window_name[ll_index] = Lower(arg_window_name) 
i_loaded_windows_ib_secured[ll_index] = arg_secured 
i_loaded_windows_ib_secured_controls[ll_index] = arg_secured_controls 

RETURN 0  // Success 

end function

public function boolean uof_check_access (graphicobject arg_graphic_object);// uof_check_access - checks to see if we have access to a window or datawindow.  If we're checking access for a window, it returns true or
//                    false.  If true, it also checks and disables the appropriate controls.  If we're checking access for a datawindow, 
//                    it checks and disables the appropriate columns.
//
//                    When checking access for a window, there are three different methods to determine whether or not we have access to the 
//                    window.  The following gives a brief overview of which functions are involved in which methods.
//
//	METHOD 0
//		{successful login}
//		uof_initialize
//
//		{no security being used for the application}
//
// METHOD 1
//		{successful login}
//		uof_initialize
//
//		{window open event}
//		uof_check_Access
//			uof_check_nonloaded_windows 
//				uof_check_nonloaded_controls
//					uof_disable_control
//
// METHOD 2
//		{successful login}
//		uof_initialize
//
//		{window open event}
//		uof_check_access
//			uof_check_loaded_windows 
//				uof_check_loaded_controls
//					uof_disable_control
//			uof_check_nonloaded_windows 
//				uof_load_window
//				uof_check_nonloaded_controls
//					uof_disable_control
//					uof_load_control
//
// METHOD 3
//		{successful login}
//		uof_initialize
//
//		{window open event}
//		uof_check_access
//			uof_check_loaded_windows 
//				uof_check_loaded_controls
//					uof_disable_control
//
// Keep in mind some of the functions perform differently depending on which method is being used.
// 
Boolean lb_access

// Determine whether we're checking access rights for a window or a datawindow.
CHOOSE CASE arg_graphic_object.TypeOf()
	// If we're checking access rights for a window, instantiate the window and test using the appropriate method.
	CASE Window!
		iw_window = arg_graphic_object
		is_window_name = Lower(iw_window.ClassName())
		CHOOSE CASE ii_method
			// For security methods 1 and 2, go to the database to see whether or not we have access to the window.  If we're using
			// method 2, check our list of loaded windows first to see whether or not we've already checked access for this window.
			CASE 1, 2
				IF ii_method = 2 THEN
					IF uof_check_loaded_windows(lb_access) THEN
						RETURN lb_access
					END IF
				END IF
				RETURN uof_check_nonloaded_windows()

			// If we're using security method 3, we've already loaded all secured windows into our "loaded" array, so there's no need to
			// ever check non-loaded windows.  Note: if we find the window in our list of "loaded" windows, the access argument should
			// always be false since we only preload those windows we don't have access to when using security method 3.
			CASE 3
				IF uof_check_loaded_windows(lb_access) THEN
					RETURN lb_access
				END IF
		END CHOOSE

	// If we're checking access rights for a datawindow, instantiate the datawindow and test using the appropriate method.
	CASE DataWindow!
		CHOOSE CASE ii_method
			// For column level security, we're always using security method  3.  In other words, we've already loaded all secured columns
			// into our "loaded" array, so there's no need to ever check non-loaded columns.  The value returned here means nothing.
			// Checking loaded columns will disable them if we don't have access to them.
			CASE 1, 2, 3
				idw_datawindow = arg_graphic_object
				IF uof_check_loaded_columns() THEN
					RETURN TRUE
				END IF
		END CHOOSE
END CHOOSE

RETURN TRUE   // Return true

end function

private function integer uof_load_column (string arg_database_name, string arg_table_name, string arg_column_name, boolean arg_authorized_viewing, boolean arg_authorized_updating);// uof_load_column - loads a secured column to the list of secured columns
//
Long ll_index 

ll_index = UpperBound(i_loaded_columns_is_column_name) + 1 
i_loaded_columns_is_database_name[ll_index] = Lower(arg_database_name) 
i_loaded_columns_is_table_name[ll_index] = Lower(arg_table_name) 
i_loaded_columns_is_column_name[ll_index] = Lower(arg_column_name) 
i_loaded_columns_ib_authorized_viewing[ll_index] = arg_authorized_viewing 
i_loaded_columns_ib_authorized_updating[ll_index] = arg_authorized_updating 

RETURN 0  // Return a success code

end function

private function boolean uof_check_loaded_windows (ref boolean arg_authorized_access);// uof_check_loaded_windows - This function checks to see if the window we are trying to access has been loaded into our list of windows or not.  
//                            If it has been loaded, it returns true.  Else it returns false.  It also sets the access argument to tell the 
//                            calling script whether or not we have access to that window in the event we find it in the list.  Note: for security
//                            method 3, if we don't find it in the list, then we have access to it.
//
Integer li_index, li_lower, li_upper 

CHOOSE CASE ii_method 
	// For security method 2, we need to serially read through the list of previously opened windows since the list is in chronological (as
	// we opened them) order rather than alphabetical order.  If we find the window in our list, we need to check the secured flag to
	// determine whether or not we have access to it.  If it's secured, we do not have access to it.
	CASE 2
		li_upper = UpperBound(i_loaded_windows_is_window_name) 
		li_index = 0
		DO
			li_index = li_index + 1
			IF li_index <= li_upper THEN
				// If we find it in our list of previously opened windows, check to see whether or not we have access to it.  If we
				// don't, set the access argument to false.  If we do have access to it, check to see if the window's controls are
				// secured and disable those which are.  In either case, the function returns true because we found the window in our list.
				IF i_loaded_windows_is_window_name[li_index] = is_window_name THEN
					IF i_loaded_windows_ib_secured[li_index] THEN
						arg_authorized_access = FALSE
					ELSE
						arg_authorized_access = TRUE
						IF i_loaded_windows_ib_secured_controls[li_index] THEN
							uof_check_loaded_controls()
						END IF
					END IF
					RETURN TRUE 
				END IF
			END IF
		LOOP UNTIL (li_index > li_upper)

	// For security method 3, we can use a binary search since the list will be alphabetized according to window name.  Check the list of
	// secured windows to see if we have access to it...if we find it in the list, we don't have access to it (this is different than
	// security method 2) since we only preloaded those windows we don't have access to.  If we don't find it in the list, we need to check
	// the preloaded controls we don't have access to.
	CASE 3
		li_lower = 1
		li_upper = UpperBound(i_loaded_windows_is_window_name) 
		li_index = Round((li_lower + li_upper) / 2,0)
		DO WHILE li_lower <= li_upper
			IF i_loaded_windows_is_window_name[li_index] = is_window_name THEN
				arg_authorized_access = FALSE 
				RETURN TRUE 
			ELSE
				IF i_loaded_windows_is_window_name[li_index] < is_window_name THEN
					li_lower = li_index + 1
				ELSE
					li_upper = li_index - 1
				END IF
				li_index = Round((li_lower + li_upper) / 2,0)
			END IF
		LOOP

		// At this point, we have cleared security for the window.  Now we need to determine whether or not any controls on the window
		// have been secured.  If they have, we need to go disable those we don't have access to.
		arg_authorized_access = TRUE 
		uof_check_loaded_controls()
END CHOOSE

// If we reach this point, we did not find the window in our list of already loaded windows so we return false...it was not found.
RETURN FALSE

end function

public function integer uof_initialize ();// uof_initialize - loads the security definitions.  If the security method being used for the application is method 3, 
//                  it also preloads all secured objects so that going to the database will no longer be required for 
//                  the remainder of the session.  Note:  we always preload secured columns (unless we have a security method of 0). 
//
//                  Whenever a cursor is opened in this script, it is loaded into an array structure.  This is because all cursors 
//                  are not created equally.  In some RDBMS's, once a cursor is open, you can't perform any action against the database
//                  except another fetch.  So, we are forced to close the cursor if we want to do any selects before we reach the end of the cursor.
//
Boolean  lb_authorized_access, lb_authorized_updating 
Datetime ldt_effective_date
Integer  li_temp_counter, li_profile_counter, li_cursor_pos 
String   ls_profile, ls_secured_controls, ls_secured_update 

// Change the pointer while we load the security definitions
SetPointer(HourGlass!)

// Determine the security method being used for the application.  The default method is 0 (no security being used).
SELECT security_method 
  INTO :ii_method  
  FROM pfapp
 WHERE application = :I_Application_Name
 USING I_TransObject ; 
 
IF I_TransObject.SQLCode <> 0 THEN
	ii_method = 0
END IF

// Get a list of all profiles the user belongs to.
DECLARE Profiles_Cursor CURSOR FOR
 SELECT profile, effective_date 
   FROM pfuser_prof 
  WHERE application = :I_Application_Name 
    AND userid = :I_User_ID 
	 AND status = 'A'  
  ORDER BY profile 
  USING I_TransObject ; 

OPEN Profiles_Cursor ;

// Load the list of profiles into the profiles array so that we can use them later for checking access authority if need be.
DO
	FETCH Profiles_Cursor INTO :ls_profile, :ldt_effective_date ; 
	IF I_TransObject.SQLCode = 0 THEN
		IF ldt_effective_date <= idt_today THEN
	  		li_profile_counter = li_profile_counter + 1
			is_profiles[li_profile_counter] = Upper(ls_profile)
		END IF
	END IF
LOOP UNTIL I_TransObject.SQLCode <> 0

CLOSE Profiles_Cursor ;

// If we're using security method 3, retrieve all actively secured windows into our cursor and check security access priviledges for each of them.
IF ii_method = 3 THEN
	DECLARE Windows_Cursor CURSOR FOR
	SELECT window, secured_controls 
	  FROM pfwindow 
	 WHERE application = :I_Application_Name 
	   AND secured_code = 'Y' 
		AND status = 'A'  
	 ORDER BY window 
	 USING I_TransObject ; 

	// For each secured window, first check to see if we have access to it.  If we do not have access to it, check to see if any of our
	// profiles have access to it.  If none of our profiles have access to it, add it to the list of secured windows.
   // For an explanation of the strange cursor logic see the top of this script.
  	OPEN Windows_Cursor ;
   li_cursor_pos = 1
   DO
		FETCH Windows_Cursor INTO :i_csr_windows_is_window_name[li_cursor_pos], :i_csr_windows_is_secured_controls[li_cursor_pos] ; 
     	li_cursor_pos = li_cursor_pos + 1
   LOOP UNTIL I_TransObject.SQLCode <> 0
   CLOSE Windows_Cursor ;

   li_cursor_pos = 1
	IF UpperBound(i_csr_windows_is_window_name) > 0 THEN
      DO
			lb_authorized_access = FALSE
			SELECT effective_date
			  INTO :ldt_effective_date 
			  FROM pfuser_win 
			 WHERE application = :I_Application_Name 
				AND window = :i_csr_windows_is_window_name[li_cursor_pos] 
				AND userid = :I_User_ID 
				AND status = 'A' 
			 USING I_TransObject ; 

			// If we have access to the window, move on to the next window.
			IF I_TransObject.SQLCode = 0 THEN
				IF ldt_effective_date <= idt_today THEN
					lb_authorized_access = TRUE
				END IF
			END IF

			// Else if we don't have access to the window (whether it be because the effective dates are no good or because the record
			// could not be found), check our profiles in order to see if any of them have access.
			IF NOT lb_authorized_access THEN
				// Step through all of our profiles until we find a profile that has access to the window or we run out of profiles.
				li_temp_counter = 1
				li_profile_counter = UpperBound(is_profiles)
				DO
					SELECT effective_date
					  INTO :ldt_effective_date 
					  FROM pfprof_win 
					 WHERE application = :I_Application_Name 
					   AND window = :i_csr_windows_is_window_name[li_cursor_pos] 
						AND profile = :is_profiles[li_temp_counter] 
						AND status = 'A' 
					 USING I_TransObject ; 
					 
					IF I_TransObject.SQLCode = 0 THEN
						IF ldt_effective_date <= idt_today THEN
							lb_authorized_access = TRUE
						END IF
					END IF
					li_temp_counter = li_temp_counter + 1
				LOOP UNTIL (lb_authorized_access) OR (li_temp_counter > li_profile_counter)
			END IF

			// If we ran out of profiles, then we do not have access to the window.  Add the window to the list of secured windows.  
			// Reset the SQLCode in order to continue with the next window.
			IF NOT lb_authorized_access THEN
				IF i_csr_windows_is_window_name[li_cursor_pos] <> "" THEN
					uof_load_window(i_csr_windows_is_window_name[li_cursor_pos], TRUE, ls_secured_controls = 'Y')
				END IF
			END IF

		li_cursor_pos = li_cursor_pos + 1
		// Loop until we run out of windows to check.
		LOOP UNTIL li_cursor_pos > UpperBound(i_csr_windows_is_window_name)
	END IF
END IF

// If we're using security method 3, retrieve all actively secured controls into our cursor and check security access priviledges for each of them.
IF ii_method = 3 THEN
	DECLARE Controls_Cursor CURSOR FOR
	SELECT window, control, control_type
	  FROM pfcontrol 
	 WHERE application = :I_Application_Name 
	   AND status = 'A' 
	 ORDER BY window, control 
	 USING I_TransObject ; 

	// For each secured control, first check to see if we have access to it.  If we do not have access to it, check to see if any of our
	// profiles have access to it.  If none of our profiles have access to it, add it to the list of secured controls.
   // For an explanation of the strange cursor logic see the top of this script.
  	OPEN Controls_Cursor ;
   li_cursor_pos = 1

   DO
	   FETCH Controls_Cursor INTO :i_csr_controls_is_window_name[li_cursor_pos], :i_csr_controls_is_control_name[li_cursor_pos], :i_csr_controls_is_control_type[li_cursor_pos] ; 
      li_cursor_pos = li_cursor_pos + 1
   LOOP UNTIL I_TransObject.SQLCode <> 0
   CLOSE Controls_Cursor ;

   li_cursor_pos = 1

	IF UpperBound(i_csr_controls_is_control_name) > 0 THEN
   	DO
			lb_authorized_access = FALSE
			SELECT effective_date
			  INTO :ldt_effective_date
			  FROM pfuser_ctrl
			 WHERE application = :i_application_name 
				AND window = :i_csr_controls_is_window_name[li_cursor_pos] 
				AND control = :i_csr_controls_is_control_name[li_cursor_pos] 
				AND userid = :i_user_id 
				AND status = 'A' 
			 USING I_TransObject ; 

			// If we have access to the control, move on the next control.
			IF I_TransObject.SQLCode = 0 THEN
				IF ldt_effective_date <= idt_today THEN
					lb_authorized_access = TRUE
				END IF
			END IF

			// Else if we do not have access to the control (whether it be because the effective dates are no good or because the record
			// could not be found), check our profiles in order to see if any of them have access.
			IF NOT lb_authorized_access THEN
				// Step through all of our profiles until we find one that has access to it or we run out of profiles.
				li_temp_counter = 1
				li_profile_counter = UpperBound(is_profiles)
				DO
					SELECT effective_date
					  INTO :ldt_effective_date
					  FROM pfprof_ctrl 
					 WHERE application = :i_application_name 
					   AND window = :i_csr_controls_is_window_name[li_cursor_pos] 
						AND control = :i_csr_controls_is_control_name[li_cursor_pos] 
						AND profile = :is_profiles[li_temp_counter] 
						AND status = 'A'
					 USING I_TransObject ; 
					IF I_TransObject.SQLCode = 0 THEN
						IF ldt_effective_date <= idt_today THEN
							lb_authorized_access = TRUE
						END IF
					END IF
					li_temp_counter = li_temp_counter + 1
				LOOP UNTIL (lb_authorized_access) OR (li_temp_counter > li_profile_counter)
			END IF

			// If we ran out of profiles, then we do not have access to the control.  Add it to the list of secured controls.  
			// Reset the SQLCode in order to continue with the next control. 
			IF NOT lb_authorized_access THEN
				IF i_csr_controls_is_control_name[li_cursor_pos] <> "" THEN
					uof_load_control(i_csr_controls_is_window_name[li_cursor_pos], i_csr_controls_is_control_name[li_cursor_pos], i_csr_controls_is_control_type[li_cursor_pos]) 
				END IF
			END IF

			li_cursor_pos = li_cursor_pos + 1
		LOOP UNTIL li_cursor_pos > UpperBound(i_csr_controls_is_control_name)  // Loop until we run out of controls to check.
	END IF
END IF

// If we're using security methods 1, 2, or 3, retrieve all actively secured columns into our cursor and check security access priviledges for each of them.
IF ii_method <> 0 THEN
	DECLARE Columns_Cursor CURSOR FOR  
	SELECT database_name, table_name, column_name 
     FROM pfsec_col 
    WHERE application = :i_application_name 
	   AND status = 'A' 
    ORDER BY database_name, table_name, column_name
    USING I_TransObject ; 

	// For each secured column, check to see if we have direct access to it. For an explanation of the strange cursor logic see the top of this script.
   OPEN Columns_Cursor ; 
   li_cursor_pos = 1
   DO
		FETCH Columns_Cursor INTO :i_csr_columns_is_database_name[li_cursor_pos], :i_csr_columns_is_table_name[li_cursor_pos], :i_csr_columns_is_column_name[li_cursor_pos] ; 
      li_cursor_pos = li_cursor_pos + 1
   LOOP UNTIL I_TransObject.SQLCode <> 0
   CLOSE Columns_Cursor ; 
	li_cursor_pos = 1 

	IF UpperBound(i_csr_columns_is_column_name) > 0 THEN
  		DO	
			lb_authorized_access = FALSE
			lb_authorized_updating = FALSE
			SELECT effective_date, secured_update 
			  INTO :ldt_effective_date, :ls_secured_update 
			  FROM pfuser_col 
			 WHERE application = :i_application_name 
				AND database_name = :i_csr_columns_is_database_name[li_cursor_pos] 
				AND table_name = :i_csr_columns_is_table_name[li_cursor_pos] 
				AND column_name = :i_csr_columns_is_column_name[li_cursor_pos] 
				AND userid = :i_user_id 
				AND status = 'A' 
			 USING I_TransObject ; 

			// If we have access to the column, check to see if we have update access for it.  If we do, that means we have both view and update 
			// priviledges so don't load it into our array.  If we don't have update access for the column, load it into our array with view priviledges only.
			IF I_TransObject.SQLCode = 0 THEN
				IF ldt_effective_date <= idt_today THEN
					lb_authorized_access = TRUE
					IF ls_secured_update = 'Y' THEN
						lb_authorized_updating = TRUE
					END IF
				END IF
			END IF

			// Else if we don't have access to the column (whether it be because the effective dates are no good or because the record
			// could not be found), check our profiles in order to see if any of them have access.
			IF NOT lb_authorized_access OR NOT lb_authorized_updating THEN
				// Step through all of our profiles until we find a profile that has access to the column or we run out of profiles.
				li_temp_counter = 1
				li_profile_counter = UpperBound(is_profiles)
				DO
					SELECT effective_date, secured_update  
					  INTO :ldt_effective_date, :ls_secured_update 
					  FROM pfprof_col 
					 WHERE application = :i_application_name 
					   AND database_name = :i_csr_columns_is_database_name[li_cursor_pos] 
						AND table_name = :i_csr_columns_is_table_name[li_cursor_pos] 
						AND column_name = :i_csr_columns_is_column_name[li_cursor_pos] 
						AND profile = :is_profiles[li_temp_counter] 
						AND status = 'A' 
					 USING I_TransObject ; 

					IF I_TransObject.SQLCode = 0 THEN
						IF ldt_effective_date <= idt_today THEN
							lb_authorized_access = TRUE
							IF ls_secured_update = 'Y' THEN
								lb_authorized_updating = TRUE
							END IF
						END IF
					END IF
					li_temp_counter = li_temp_counter + 1
				LOOP UNTIL (lb_authorized_access AND lb_authorized_updating) OR (li_temp_counter > li_profile_counter)
			END IF

			// If we ran out of profiles, then we do not have access to the column.  Add the column to the list of secured columns.  
			// Reset the SQLCode in order to continue with the next column. 
			IF NOT lb_authorized_access OR NOT lb_authorized_updating THEN 
				IF i_csr_columns_is_database_name[li_cursor_pos] <> "" THEN  
					uof_load_column(i_csr_columns_is_database_name[li_cursor_pos], i_csr_columns_is_table_name[li_cursor_pos], i_csr_columns_is_column_name[li_cursor_pos], lb_authorized_access, lb_authorized_updating) 
				END IF
			END IF

			li_cursor_pos = li_cursor_pos + 1
		LOOP UNTIL li_cursor_pos > UpperBound(i_csr_columns_is_column_name)   // Loop until we run out of columns to check 
	END IF
END IF

RETURN 0   // Return a success code

end function

public function integer uof_login (string arg_user_id, string arg_password, string arg_application_name, n_transaction arg_transobject);// uof_login - initializes the security object.  It defines the user id, application name and security transaction object as defined by the
//             incoming arguments.  It also defines today's date and retrieves the security method being used for the application and a list of profiles
//             the user belongs to.  If the security method being used for the application is method 3, it also preloads all secured objects so that
//             going to the database will no longer be required for the remainder of the session.  
//             Note: we always preload secured columns (unless we have a security method of 0).
//
String  ls_password_db
Integer li_no_of_unsucc_logons
Long    ll_rc 

// Define the user id, the application name, today's date and the security transaction object according to the arguments passed in.
i_user_id = arg_user_id 
i_application_name = upper(arg_application_name) 
idt_today = DateTime(Today(), Now())
I_TransObject = arg_transobject 

// Determine the security method being used for the application.  The default method is 0 (no security being used).
SELECT security_method 
  INTO :ii_method  
  FROM pfapp
 WHERE application = :I_Application_Name
 USING I_TransObject ;

ll_rc = I_TransObject.nf_handle_error("", "n_pfsecurity", "uof_login - SELECT security_method FROM pfapp WHERE application = :I_Application_Name") 
CHOOSE CASE ll_rc 
CASE 100 
	MessageBox("Security Error", "Can't Find Application Security Method!", StopSign!, OK!) 
	RETURN -1 
CASE IS < 0 
	RETURN -1 
END CHOOSE 

// If the application is found and the security method is 0, return.
IF (I_TransObject.Sqlcode = 0 AND ii_method = 0) THEN 
	RETURN 1 
END IF

// Check for valid user and get password and logon attempts
SELECT password, no_of_unsucc_logons, first_name, last_name 
  INTO :ls_password_db, :li_no_of_unsucc_logons, :is_user_profile_first_name, :is_user_profile_last_name 
  FROM pfuser 
 WHERE userid = :i_user_id  
 USING I_TransObject ; 

ll_rc = I_TransObject.nf_handle_error("", "n_pfsecurity", "uof_login - SELECT password, no_of_unsucc_logons, first_name, last_name FROM pfuser WHERE userid = :i_user_id") 
CHOOSE CASE ll_rc 
CASE 100 
	MessageBox("Security Error", I_User_id + " Not Registered As An Application User to Security System.", StopSign!, OK!) 
	RETURN -1 
CASE IS < 0 
	RETURN -1 
END CHOOSE	

RETURN 1

end function

private function boolean uof_disable_column (ref datawindow arg_datawindow, string arg_column_name, boolean arg_authorized_viewing, boolean arg_authorized_updating);// uof_disable_column - disables a datawindow column.  There are two ways to disable the column: disable the ability to update the column and
//                      disable the ability to update and view the column.  At the very least, we will disable the ability to update the column.
//
String ls_color

// Specify the color to make the disabled column.  The default is gray.
ls_color = "67108864"  // String(RGB(192,192,192)) 

// Disable the ability to update (change) the column value by removing the tab sequence for the column 
// so that it can't get focus.  Change the background color to indicate it can't get focus.
IF NOT Arg_Authorized_Updating THEN
	Arg_DataWindow.SetTabOrder(arg_column_name, 0)
	Arg_DataWindow.Modify(arg_column_name + ".Background.Color = " + ls_color)
END IF

// If we're also disabling the ability to view the column's value, simply change the text color to match that of the background so that we can't
// see its value.  Note: we need to use a "solid" color because some of the "mixed" colors will not conceal the column's value.
IF NOT arg_authorized_viewing THEN
	Arg_DataWindow.Modify(arg_column_name + ".Color = " + ls_color)
	Arg_DataWindow.Modify(arg_column_name + ".Background.Color = " + ls_color)
END IF

RETURN TRUE   // Return a success code

end function

private function boolean uof_check_loaded_columns ();// uof_check_loaded_columns - compares the columns in the datawindow argument with the secured columns 
//                            in our "loaded" array and disables any it finds using the disable column function.
//
Boolean lb_found
Integer li_datawindow_count, li_datawindow_index, li_secured_count, li_secured_index
String  ls_dbname, ls_dwname, ls_table_column

// Determine the number of columns in the datawindow and the number of secured columns in the application.
li_datawindow_count = Integer(idw_datawindow.Describe("DataWindow.Column.Count"))
li_secured_count = UpperBound(i_loaded_columns_is_column_name)

// If we have a lot of secured columns, then it's probably more efficient to search through the list of secured columns for each column in the datawindow.
IF li_secured_count > li_datawindow_count THEN
	FOR li_datawindow_index = 1 to li_datawindow_count
		ls_dbname = Lower(idw_datawindow.Describe("#" + String(li_datawindow_index) + ".dbName"))
		li_secured_index = 0
		lb_found = FALSE
		DO
			li_secured_index = li_secured_index + 1
			IF li_secured_index <= li_secured_count THEN
            ls_dwname = Lower(i_loaded_columns_is_table_name[li_secured_index] + "." + i_loaded_columns_is_column_name[li_secured_index])
				IF Lower(i_loaded_columns_is_table_name[li_secured_index] + "." + i_loaded_columns_is_column_name[li_secured_index]) = ls_dbname THEN
					uof_disable_column(idw_datawindow, idw_datawindow.Describe("#" + String(li_datawindow_index) + ".Name"), i_loaded_columns_ib_authorized_viewing[li_secured_index], i_loaded_columns_ib_authorized_updating[li_secured_index]) 
				END IF
			END IF
		LOOP UNTIL (lb_found) OR (li_secured_index > li_secured_count)
	NEXT

// Else if we have only a few secured columns, then it's probably more efficient to search through the datawindow for each of the secured columns.
ELSE
	FOR li_secured_index = 1 to li_secured_count
		ls_dbname = Lower(i_loaded_columns_is_table_name[li_secured_index] + "." + i_loaded_columns_is_column_name[li_secured_index])
		li_datawindow_index = 0
      lb_found = FALSE
		DO
			li_datawindow_index = li_datawindow_index + 1
			IF li_datawindow_index <= li_datawindow_count THEN
				ls_table_column = Lower(idw_datawindow.Describe("#" + String(li_datawindow_index) + ".dbName"))
				IF ls_table_column = ls_dbname THEN
					uof_disable_column(idw_datawindow, idw_datawindow.Describe("#" + String(li_datawindow_index) + ".Name"), i_loaded_columns_ib_authorized_viewing[li_secured_index], i_loaded_columns_ib_authorized_updating[li_secured_index]) 
				END IF
			END IF
		LOOP UNTIL (lb_found) OR (li_datawindow_index > li_datawindow_count)
	NEXT
END IF

RETURN TRUE  // Return a success code

end function

on n_pfsecurity.create
call super::create
end on

on n_pfsecurity.destroy
call super::destroy
end on

