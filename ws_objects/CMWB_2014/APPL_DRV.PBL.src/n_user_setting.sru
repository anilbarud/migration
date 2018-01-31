$PBExportHeader$n_user_setting.sru
$PBExportComments$object used to get/set user dependant settings - based on a specific user settings
forward
global type n_user_setting from nonvisualobject
end type
end forward

global type n_user_setting from nonvisualobject
end type
global n_user_setting n_user_setting

type variables
Protected:
window		iw_requestor

boolean	ib_window=True
boolean	ib_menuitems=True
boolean	ib_toolbars=True
boolean	ib_toolbaritemvisible=True
boolean	ib_toolbaritemspace=True
boolean	ib_toolbaritemorder=True
boolean	ib_toolbartitles=True
boolean  ib_restoreuser
BOOLEAN  ib_restoreapp

//Store the NormalState Window Size and Position.
integer	ii_normalstate_x
integer	ii_normalstate_y
integer	ii_normalstate_width
integer	ii_normalstate_height

STRING   is_userkey
STRING   is_appkey
end variables

forward prototypes
public function string of_string (windowstate aws_windowstate)
public function integer of_save (boolean ab_useregistry, string as_keyorini, string as_inisection)
public function integer of_setpos ()
public function integer of_setpossize ()
public function integer of_setuserkey (string as_userkey)
public function integer of_setrestoreuser (boolean ab_switch)
public function integer of_setrestoreapp (boolean ab_switch)
public function integer of_setappkey (string as_appkey)
public function boolean of_IsRestoreApp ()
public subroutine of_setrequestor (window aw_requestor)
public function integer of_restore (string as_regkey)
public function integer of_save (boolean ab_useregistry, string as_keyorini, string as_inisection, string as_subkey, string as_value)
public function integer of_restore (boolean ab_useregistry, string as_keyorini, string as_inisection, string as_subkey, ref string as_value, string as_default)
public function integer of_restore (boolean ab_useregistry, string as_keyorini, string as_inisection)
end prototypes

public function string of_string (windowstate aws_windowstate);//Check parameters
If IsNull(aws_windowstate) Then
	String ls_null
	SetNull(ls_null)
	Return ls_null
End If

Choose Case aws_windowstate

	Case Normal!
		Return "normal"

	Case Maximized!
		Return "maximized"

	Case Minimized!
		Return "minimized"

End Choose

//Invalid parameter value
Return "!"
end function

public function integer of_save (boolean ab_useregistry, string as_keyorini, string as_inisection);/*
	Function:  		of_Save

	Access:  		protected

	Arguments:	
	ab_UseRegistry	Function behavior. - use the registry or an .ini file.
	as_KeyOrIni		The KeyName for use with the Registry or the IniFile name
						for use with an .Ini file.
	as_IniSection	The name of the .Ini section. 

	Returns:  		Integer
						 1 if it succeeds and -1 if an error occurs.

	Description:  	Saves the preference information to either the Registry or
						to an .INI file.
*/
INTEGER				li_rc,li_x, li_y, li_width, li_height

//Check for a window association with this object.
IF IsNull(iw_requestor) OR Not IsValid (iw_requestor) THEN RETURN -1

//Check arguments
IF IsNull(ab_UseRegistry) THEN RETURN -1

//Validate specifics for either Registry or .Ini functionality.
IF NOT ab_UseRegistry THEN RETURN -1

//Clear the section prior to updating it
IF ab_UseRegistry THEN RegistryDelete (as_KeyOrIni, '')

//Save window size and position
li_rc = of_Save (ab_UseRegistry, as_KeyOrIni, as_IniSection, 'window.windowstate', &
of_string(iw_requestor.WindowState))

//If the window is currently Maximized or Minimized then used the 
//stored Position/Size values.  If the window is currently Normal!
//then refresh the stored Position/Size values to ensure accurate data.
If iw_requestor.WindowState=Normal! Then of_SetPosSize()

/*The X,Y,Width,&Height values are for the Normal State Window.*/
li_rc = of_Save (ab_UseRegistry, as_KeyOrIni, as_IniSection, &
	'window.x', string(ii_normalstate_x))
li_rc = of_Save (ab_UseRegistry, as_KeyOrIni, as_IniSection, &
	'window.y', string(ii_normalstate_y))
li_rc = of_Save (ab_UseRegistry, as_KeyOrIni, as_IniSection, &
	'window.width', string(ii_normalstate_width))
li_rc = of_Save (ab_UseRegistry, as_KeyOrIni, as_IniSection, &
	'window.height', string(ii_normalstate_height))

//Success return value
Return 1

end function

public function integer of_setpos ();/*
	Function:  		of_SetPosSize

	Access:  		public

	Arguments:		None

	Returns:  		integer
						 1 if it succeeds.
						 0 if the WindowState is not Normal!.
						-1 if an error occurs.
						
	Description:  	Store in the service the current Position and Size of Requestor.
						This is needed so the service knows the Normal size of the 
						Requestor even when the Requestor is closed as Maximized/Minimized.

			*Note:	This function should be called from the resize and move events.
						This function should only be called when the WindowState is
						of type Normal!.
*/
If IsNull(iw_requestor) Or Not IsValid (iw_requestor) Then Return -1

//Confirm that the WindowState is of type Normal!
If iw_requestor.WindowState = Normal! Then
	ii_normalstate_x = iw_requestor.X
	ii_normalstate_y = iw_requestor.Y
	ii_normalstate_width = iw_requestor.Width
	ii_normalstate_height = iw_requestor.Height
	Return 1
End If

Return 0

end function

public function integer of_setpossize ();/*
	Function:  		of_SetPosSize

	Access:  		public

	Arguments:		None

	Returns:  		integer
						 1 if it succeeds.
						 0 if the WindowState is not Normal!.
						-1 if an error occurs.
						
	Description:  	Store in the service the current Position and Size of Requestor.
						This is needed so the service knows the Normal size of the 
						Requestor even when the Requestor is closed as Maximized/Minimized.

			*Note:	This function should be called from the resize and move events.
						This function should only be called when the WindowState is
						of type Normal!.
*/
If IsNull(iw_requestor) Or Not IsValid (iw_requestor) Then Return -1

//Confirm that the WindowState is of type Normal!
If iw_requestor.WindowState = Normal! Then
	ii_normalstate_x = iw_requestor.X
	ii_normalstate_y = iw_requestor.Y
	ii_normalstate_width = iw_requestor.Width
	ii_normalstate_height = iw_requestor.Height
	Return 1
End If

Return 0

end function

public function integer of_setuserkey (string as_userkey);/*
	Function:	of_SetUserKey

	Access:		public

	Arguments:		
	as_userkey	full registry key for the user.

	Returns:		Integer
	 1 = success
	-1 = error 

	Description:	Sets the value of the registy key for the user
*/

IF IsNull(as_userkey) THEN RETURN -1

is_userkey = as_userkey

RETURN 1
end function

public function integer of_setrestoreuser (boolean ab_switch);/*
	Function:  		of_SetRestoreUser

	Access:  		public

	Arguments:
	ab_switch	   Mode of the service.  False = do not restore app user attributes

	Returns:  		integer
						Returns 1 if it succeeds and -1 if an error occurs.
						
	Description:  	Tells the service to restore or not to restore
						the application user level information.

*/

IF IsNull(ab_switch) THEN RETURN -1

ib_restoreuser = ab_switch

RETURN 1

end function

public function integer of_setrestoreapp (boolean ab_switch);/*

	Function:  		of_SetRestoreApp

	Access:  		public

	Arguments:
	ab_switch	   Mode of the service.  False = do not restore app attributes

	Returns:  		integer
						Returns 1 if it succeeds and -1 if an error occurs.
						
	Description:  	Tells the service to restore or not to restore
						the application information attributes.

*/

IF IsNull(ab_switch) THEN RETURN -1

ib_restoreapp = ab_switch

RETURN 1

end function

public function integer of_setappkey (string as_appkey);/*
	Function:	of_SetAppKey

	Access:		public

	Arguments:		
	as_appkey	Full key value for the application.

	Returns:		Integer
	 1 = success 
	-1 = error 

	Description:	Sets the value for the application key on the registry.
*/
IF IsNull(as_appkey) THEN RETURN -1

is_appkey = as_appkey

RETURN 1
end function

public function boolean of_IsRestoreApp ();/*
	Function:  	of_IsRestoreApp

	Access:  	public

	Arguments:	none

	Returns:  	boolean
	True if the service will restore the application information 
						
	Description:
	Reports if the service is to restore the application information 
*/

return ib_restoreapp


end function

public subroutine of_setrequestor (window aw_requestor);/*
 Function used to set up window that will be used to save various user information

*/

iw_requestor = aw_requestor
end subroutine

public function integer of_restore (string as_regkey);/*
	Function:  		of_Restore

	Access:  		public

	Arguments:	
	as_regkey		The registry key path to read values from.

	Returns:  		Integer
						1 if it succeeds and -1 if an error occurs.

	Description:  	Restores the preference information from the Registry.
*/
integer 		li_rc
window		lw_obj

//Check for a window association with this object.
IF IsNull(iw_requestor) OR NOT IsValid (iw_requestor) THEN RETURN -1

li_rc = of_Restore (TRUE, as_regkey, '')

//Prevent flickering of toolbars.
IF IsValid(lw_obj) THEN	
	lw_obj.SetRedraw(TRUE)
ELSE
	iw_requestor.SetRedraw(TRUE)
END IF

RETURN li_rc
end function

public function integer of_save (boolean ab_useregistry, string as_keyorini, string as_inisection, string as_subkey, string as_value);/*	
	Function:  		of_Save

	Access:  		protected

	Arguments:
	ab_UseRegistry	Function behavior. - use the registry or an .ini file.
	as_KeyOrIni		The KeyName for use with the Registry or the IniFile name
						for use with an .Ini file.
	as_IniSection	The name of the .Ini section. 
	as_SubKey			The key value to be used on either the Registry or .Ini file.
	as_value			The value to be stored on either the Registry or .Ini file.

	Returns:  		integer
						1 if it succeeds and -1 if an error occurs.

	Description:  	Perform the actual put into the Registry or the .Ini file.

*/

IF ab_UseRegistry THEN
	IF RegistrySet (as_KeyOrIni, as_SubKey, as_Value) =1 THEN RETURN 1
	RETURN -1
END IF	

IF SetProfileString (as_KeyOrIni, as_IniSection, as_SubKey, as_Value) = 1 THEN RETURN 1

RETURN -1

end function

public function integer of_restore (boolean ab_useregistry, string as_keyorini, string as_inisection, string as_subkey, ref string as_value, string as_default);/*
	Function:  		of_Restore

	Access:  		protected

	Arguments:
	ab_UseRegistry	Function behavior. - use the registry or an .ini file.
	as_KeyOrIni		The KeyName for use with the Registry or the IniFile name
						for use with an .Ini file.
	as_IniSection	The name of the .Ini section. 
	as_SubKey			The key value to be used on either the Registry or .Ini file.
	as_value			The value to be restored from either the Registry or .Ini file.
							Passed by reference.
   as_default		Used also as a default value, if the desired value is not found.

	Returns:  		integer
						1 if it succeeds.
						0 if the default value was used.

	Description:  	Perform the actual Get from the Registry or the .Ini file.
*/
constant string DEFAULT='$%^'

IF ab_UseRegistry THEN
	IF RegistryGet (as_KeyOrIni, as_SubKey, as_Value) = 1 Then Return 1

	as_Value = as_default
	RETURN 0
END IF 	

RETURN 1

end function

public function integer of_restore (boolean ab_useregistry, string as_keyorini, string as_inisection);/*
	Function:  		of_Restore

	Access:  		protected

	Arguments:	
	ab_UseRegistry	Function behavior. - use the registry or an .ini file.
	as_KeyOrIni		The KeyName for use with the Registry or the IniFile name
						for use with an .Ini file.
	as_IniSection	The name of the .Ini section. 

	Returns:  		Integer
						1 if it succeeds and -1 if an error occurs.

	Description:  	Restores the preference information from either the Registry
						or from an .INI file.
*/
string		ls_x, ls_y, ls_width, ls_height,ls_regvalues[]
integer		li_rc = 1

//Check for a window association with this object.
IF IsNull(iw_requestor) OR NOT IsValid (iw_requestor) THEN RETURN -1

//Check arguments
IF IsNull(ab_UseRegistry) THEN RETURN -1

//Validate specifics for either Registry or .Ini functionality.
IF ab_UseRegistry THEN
	//Check for the existance of the section.	
	RegistryValues(as_KeyOrIni, ls_regvalues)
	IF UpperBound(ls_regvalues) <= 0 THEN RETURN -1

ELSE
		RETURN -1
END IF

IF iw_requestor.WindowState = Normal! THEN
	//Restore Normal State window position and size.
	li_rc = of_Restore (ab_UseRegistry, as_KeyOrIni, as_IniSection, &
		'window.x', ls_x, '')
	li_rc = of_Restore (ab_UseRegistry, as_KeyOrIni, as_IniSection, &
		'window.y', ls_y, '')
	li_rc = of_Restore (ab_UseRegistry, as_KeyOrIni, as_IniSection, &
			'window.width', ls_width, '')
	li_rc = of_Restore (ab_UseRegistry, as_KeyOrIni, as_IniSection, &
			'window.height', ls_height, '')	

IF IsNumber(ls_x) THEN iw_requestor.x = Integer(ls_x)
IF IsNumber(ls_y) THEN iw_requestor.y = Integer(ls_y)

	
	IF iw_requestor.Resizable THEN
		IF IsNumber(ls_width) AND IsNumber(ls_height) THEN
			iw_requestor.Resize(Integer(ls_width), Integer(ls_height))
		ELSEIF IsNumber(ls_width) THEN
			iw_requestor.width = Integer (ls_width)	
		ELSEIF IsNumber(ls_height) THEN
			iw_requestor.height = Integer (ls_height)	
		END IF
	END IF
END IF

RETURN 1
end function

on n_user_setting.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_user_setting.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

