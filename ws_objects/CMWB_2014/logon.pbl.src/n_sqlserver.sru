$PBExportHeader$n_sqlserver.sru
forward
global type n_sqlserver from nonvisualobject
end type
type st_transaction from structure within n_sqlserver
end type
end forward

type st_transaction from structure
	transaction		transaction_object
	long		user_options
end type

global type n_sqlserver from nonvisualobject
end type
global n_sqlserver n_sqlserver

type variables

PRIVATE	STRING		is_nt_user_id
PRIVATE  STRING		is_db_user_id
PRIVATE 	STRING		is_application_name
PRIVATE	STRING		is_login_server_name
PRIVATE	LONG			il_connection_number
PRIVATE	STRING		is_sql_login_id
PRIVATE 	STRING		is_logon_ini
PRIVATE		STRING		is_logon_section = 'Logon'
PRIVATE		STRING		is_logon_path_key = 'Logon_ini'
PRIVATE  STRING		is_application_ini
PRIVATE  STRING		is_hawkdes_ini
CONSTANT INTEGER		lgSUCCESS = 0
CONSTANT INTEGER		lgFAILURE = -1
PRIVATE 	STRING		is_key	= '12345678'
PRIVATE 	STRING		is_user_password

//ib_logged_in is true if of_loggin has completed successfully and we have
//retrieved the user_password from the database.  ib_logged_in is not set back
//to false when of_end_loggin is called because this doesnt' mean we are logged
//out.  of_end_loggin is used only to record the time it takes to complete the
//loggin process.
PRIVATE	BOOLEAN		ib_logged_in
//Set ib_AutoFail to true in the calling script when you want the object 
//to cause an application error automatically when it encounters a severe error.
//Default to true.  Make the calling code explicitly turn it off.  That way they won't
//miss returns code accidentally.
PUBLIC	BOOLEAN		ib_AutoFail	= TRUE
TRANSACTION  			READONLY_SECURITY

PRIVATE st_transaction				ist_transaction[]

datastore				ids_user_options
n_numerical				inv_numerical

end variables

forward prototypes
public function integer of_initialize_security (ref n_pfsecuritystub anv_pfsecurity)
public function integer of_SignalError (integer al_error_number, string as_error_text)
public function integer of_set_application_ini (string as_application_ini)
public function integer of_set_application_name (string as_application_name)
public function integer of_set_user_id (string as_user_id)
private subroutine of_check_application_status ()
public function integer of_login ()
public function integer of_end_login ()
public function string of_get_server_name (string as_database_name)
public function integer nf_set_user_options ()
private function integer of_get_transaction (string as_database_name, string as_connection_parameters, string as_password, string as_user_id, ref transaction at_trans)
public function integer of_string_to_array (string as_string, string as_delimeter, ref string as_string_array[])
public function integer of_get_transaction (string as_database_name, string as_connection_parameters, ref transaction as_trans)
public function integer of_get_transaction (string as_database_name, ref transaction as_trans)
public subroutine of_set_logon_section (string as_section_name)
public subroutine of_set_logon_path_key (string as_logon_path_key)
public subroutine of_check_connection_flag ()
private function integer of_p_begin_user_login (ref string as_user_id, ref string as_password, string as_server_name, string as_application_name, string as_win_auth_flag)
end prototypes

public function integer of_initialize_security (ref n_pfsecuritystub anv_pfsecurity);//*********** Login to the PowerFrame security object*******************

//Verify that we are already logged into the server before trying to 
//read data from the security database.
IF ib_logged_in = False THEN
	SignalError(-666,'Not logged into the server yet.  You cannot log into the PowerFrame user object until you are logged into SqlServer.')	
END IF

//*****Log into the PowerFrame security object.***********************
IF anv_pfsecurity.UOF_Login(is_nt_user_id, '', is_application_name, READONLY_SECURITY) < 0 THEN
	Beep(2)
	MessageBox("NO APPLICATION AUTHORIZATION","You are not authorized to use" &
	+	" this applcation.",StopSign!)	
	HALT CLOSE
END IF

//*****Load Security Definitions. Loads the arrays of secured objects.*****
G_PFSecurity.UOF_Initialize()

//****Load Mail Recipients from Application_Mail_Recipients table for error handler.*****
IF G_PFSecurity.UOF_Retrieve_Mail_Recipients() = -1 THEN
	SignalError(-666,"Unable to load mail recipients from Security database.")
End If

return 1
end function

public function integer of_SignalError (integer al_error_number, string as_error_text);IF ib_AutoFail = True THEN
	SignalError(al_error_number,as_error_text)
END IF

RETURN al_error_number
end function

public function integer of_set_application_ini (string as_application_ini);//Return 	-1 if the as_logon_ini is empty
//				-2 if the as_logon_ini file doesn't exist
CONSTANT 	INTEGER 		cli_FILE_NOT_EXISTS = -2

IF as_application_ini = '' THEN
	RETURN of_SignalError(lgFAILURE,"Application ini file name is blank.")
END IF
	
IF FileExists(as_application_ini) = False THEN
	RETURN of_SignalError(cli_FILE_NOT_EXISTS,"Application ini '" + as_application_ini + " does not exist.")
END IF

is_application_ini = as_application_ini

RETURN lgSUCCESS
end function

public function integer of_set_application_name (string as_application_name);

IF as_application_name = '' THEN
	RETURN of_SignalError(lgFAILURE,"Application name is blank.  Cannot log in with a blank application name.")
END IF

is_application_name = as_application_name

RETURN lgSUCCESS
end function

public function integer of_set_user_id (string as_user_id);
If as_user_id = '' THEN
	RETURN of_SignalError(lgFAILURE,"User id is blank.  Cannot log in using a blank user id.")
END IF

is_nt_user_id = as_user_id

RETURN lgSUCCESS
end function

private subroutine of_check_application_status ();//****** Step 1.5 ***********************************************************
// Determine if the application is available. Determine by reading the table
// Application_Status on the SECURITY database.
STRING		ls_application_available
	
SELECT available_flag
  INTO :ls_application_available
  FROM Application_Status
 WHERE application = :is_application_name
 USING READONLY_SECURITY;

IF ls_application_available = '' THEN
	SignalError(-666,"Application name '" + is_application_name + "' not found in Application Status table.")
END IF

IF Upper(ls_application_available) = 'N' THEN
	Beep(2)
	MessageBox("APPLICATION NOT AVAILABLE","The " + is_application_name + " application is " &
	+ "currently not available. Please try again later.",StopSign!)	
	HALT CLOSE
END IF


end subroutine

public function integer of_login ();//RETURN 	-1	if the application_ini is empty
//RETURN		-2 if begin failed because we could connect to the SECURITY database
//						to get the users password
u_hawkdes 	uo_hawkdes 
STRING		ls_user_password
STRING		ls_sec_ro_user_password
STRING		ls_profile
STRING		ls_section
STRING		ls_default
STRING		ls_db_server_name
STRING		ls_application
INTEGER		li_rtn

READONLY_SECURITY = CREATE n_transaction

If is_application_ini = '' OR is_nt_user_id = '' OR is_application_name = '' Then
	RETURN of_SignalError(-1,"One of the following is missing: application.ini, user_id or application name.")
END IF

uo_hawkdes = CREATE u_hawkdes
uo_hawkdes.ic_key = is_key

//Get the logon ini file name
is_logon_ini = ProfileString(is_application_ini, is_logon_section, is_logon_path_key, "")

//Make sure the logon.ini file exists
IF NOT FileExists(is_logon_ini) THEN
	RETURN of_SignalError(-666,"Error locating '" + is_logon_ini + "'. Application Terminating. Call Help Desk.")
END IF

//Set the password profile
ls_profile = 'Hawkdes'
ls_default = ''

ls_section = 'pw_sec_ro_user'
// Get the un-encryped password for the EIS database
ls_sec_ro_user_password = uo_hawkdes.uf_ProfileString(is_logon_ini, ls_profile, ls_section, ls_default)

//This is a temporary fix for the sec_ro_user_password.  The hawkdes dll returns the wrong password when
//it is mod(len(password),8) = 0.  It adds additional characters to the end.  For this reason we have to 
//take the first half of the returned password as the real password and the remainder will be treated
//as garbage.
ls_sec_ro_user_password = LEFT(ls_sec_ro_user_password,8)
If ls_sec_ro_user_password = '' THEN 
	RETURN of_SignalError(-7,'Error getting the password for sec_ro_user.')
END IF


//Set logged_in here so that the call to of_get_transaction will not fail.  There
//is a check to make sure the object is logged_in.  If it's not then an application
//error will occur.
ib_logged_in = True

//The sec_ro_user account is the only user_id that can execute the stored procedures
//to get the user passwords.
of_get_transaction('SECURITY','',ls_sec_ro_user_password,"sec_ro_user",READONLY_SECURITY)

//Set the autocommit to True for this transaction object only
READONLY_SECURITY.AutoCommit = True

CONNECT USING READONLY_SECURITY;
IF READONLY_SECURITY.SQLDBCode <> 0 THEN 
	RETURN of_SignalError(-2,'Unable to connect to the SECURITY database with sec_ro_user.')
END IF

of_check_application_status()

of_check_connection_flag()

RETURN lgSUCCESS
end function

public function integer of_end_login ();
DECLARE lp_END_USER_LOGIN PROCEDURE FOR p_END_USER_LOGIN
	@input_value1 = :is_db_user_id,
	@input_value2 = :is_application_name,
	@input_value3 = :is_login_server_name,
	@input_value4 = :il_connection_number,
	@input_value5 = :gs_win_auth_flag
USING READONLY_SECURITY;

IF READONLY_SECURITY.SQLDBCode <> 0 THEN
	//Disconnect so we don't leave any orphaned sec_ro_user connections around
	DISCONNECT USING READONLY_SECURITY;
	RETURN of_SignalError(lgFAILURE,"Error Declaring p_END_USER_LOGIN.")
END IF

EXECUTE lp_END_USER_LOGIN;

IF READONLY_SECURITY.SQLDBCode <> 0 THEN
	//Disconnect so we don't leave any orphaned sec_ro_user connections around
	DISCONNECT USING READONLY_SECURITY;
	RETURN of_SignalError(lgFAILURE,"Error Executing stored procedure p_END_USER_LOGIN: " + THIS.READONLY_SECURITY.sqlerrtext)
END IF

DISCONNECT USING READONLY_SECURITY;
IF READONLY_SECURITY.SQLDBCode <> 0 THEN
	RETURN of_SignalError(lgFAILURE,"Error disconnecting sec_ro_user from the database.")
END IF

DESTROY READONLY_SECURITY

//Reset the server name 
is_login_server_name = ''
il_connection_number = 0
is_user_password = ''
ib_logged_in = FALSE

//Loop through the list of transactions and set all the database "User Options"
nf_set_user_options()

return lgSUCCESS
end function

public function string of_get_server_name (string as_database_name);/* need to grab the dbparm and strip out the servername  for each database?
*/
STRING    ls_dbparm, ls_check, ls_temp
INTEGER  li_position, li_counter, li_count
STRING    ls_connection

ls_check = ";"
//DBParm
//DBParm = "ConnectString='DRIVER={SQL SERVER};SERVER=devsql03;DATABASE=CLAIM;QuotedId=no;AnsiNPW=No;UID=<user_id>;PWD=<password>',StaticBind = 0,CommitOnDisconnect='No',DisableBind=1,StripParmNames='Yes'"

ls_connection           = ProfileString(is_logon_ini,as_database_name,"Connection",             " ")

IF gs_win_auth_flag = 'N'  OR ls_connection = 'SA_ONLY' THEN
	ls_dbparm            	= ProfileString(is_logon_ini,as_database_name,"DBParm_SA",           " ") 
ELSE
	ls_dbparm                = ProfileString(is_logon_ini,as_database_name,"DBParm",           " ") 
END IF

IF isnull(ls_dbparm) THEN ls_dbparm = ""

li_position = pos(ls_dbparm,';SERVER=',1)

IF li_position < 0 THEN 
	 of_SignalError(-666,'Position of DBParm could not be determined. Please contact the HelpDesk.')
ELSE 
	li_position = li_position + 8
END IF 

li_count = 0

FOR li_counter = li_position TO LEN(ls_dbparm) 
	
	ls_temp = mid(ls_dbparm, li_counter, 1)
	IF ls_temp = ls_check THEN EXIT
	
	li_count++
	
NEXT

IF  li_counter < 0 THEN //ERROR OUT
	of_SignalError(-666,'Position of DBParm could not be determined. Please contact the HelpDesk.')
END IF 

ls_temp = mid(ls_dbparm, li_position, li_count)

IF ISNULL(ls_temp) THEN ls_temp = ""


RETURN ls_temp
end function

public function integer nf_set_user_options ();INTEGER			li_x, li_y
STRING			ls_configuration_text
STRING			ls_sql
STRING			ls_setting
LONG				ll_user_options
LONG				ll_configuration_value



FOR li_x = 1 to UpperBound(ist_transaction)
	
	//Get the user options for the database transaction
	ll_user_options = ist_transaction[li_x].user_options
	
	//Loop through the list of configuration values
	//If there bits are set ON in the ll_user_options then issue the corresponding SET command
	//to turn them on in the database.
	For li_y = 1 to ids_user_options.Rowcount()
				
		ll_configuration_value = ids_user_options.GetItemNumber(li_y,'value')
		
		IF inv_numerical.of_bitwiseand(ll_user_options,ll_configuration_value) > 0 Then
			ls_setting = 'ON'
		Else
			ls_setting = 'OFF' 
		End if
			
		ls_configuration_text = ids_user_options.GetItemString(li_y,'configuration')			
		ls_sql = 'SET '  + ls_configuration_text + ' ' + ls_setting
		
		EXECUTE IMMEDIATE :ls_sql USING ist_transaction[li_x].transaction_object;
		
		IF ist_transaction[li_x].transaction_object.SQLDBCode <>  0 THEN
			RETURN of_SignalError(lgFAILURE,"Error occured issuing the following statement '" + ls_sql + "'")
		END IF
		
	Next //li_y
	
Next //li_x
end function

private function integer of_get_transaction (string as_database_name, string as_connection_parameters, string as_password, string as_user_id, ref transaction at_trans);BOOLEAN		lb_lock
STRING      ls_dbparm
INTEGER     li_pos_userid, li_pos_password, li_pos_app_name
INTEGER		li_transaction_index
LONG			ll_user_options
STRING		ls_application_name
INTEGER		li_parameter_count
STRING		lsa_connection_parameters[]
INTEGER		li_x
INTEGER		li_equal_pos
STRING		ls_parameter_name
STRING		ls_parameter_value
INTEGER		li_param_start_pos
INTEGER		li_param_end_pos
APPLICATION  la_application
STRING           ls_win_auth_flag
STRING           ls_connection

inv_numerical = create n_numerical

//IF we are not logged in then fail the application
If ib_logged_in = False THEN
	RETURN of_SignalError(-666,'Call of_login before trying to get a transaction.')
END IF

at_trans.DBMS       	= ProfileString(is_logon_ini,as_database_name,"DBMS",             " ")
ls_connection           = ProfileString(is_logon_ini,as_database_name,"Connection",             " ")

IF gs_win_auth_flag = 'N'  OR ls_connection = 'SA_ONLY' THEN
	ls_dbparm            	= ProfileString(is_logon_ini,as_database_name,"DBParm_SA",           " ") 
ELSE
	ls_dbparm                = ProfileString(is_logon_ini,as_database_name,"DBParm",           " ") 
END IF

ll_user_options          = ProfileInt(is_logon_ini,as_database_name,"User_Options",          -1) 
at_trans.Lock			= ProfileString(is_logon_ini,as_database_name,"Lock",           " ") //need to check into this

//Validate the user_options setting
IF ll_user_options < 0 Then
	 RETURN of_SignalError(-666,'The user options setting must not be less than zero. Please contact the HelpDesk.')
END IF

IF ll_user_options > 32767 Then
	 RETURN of_SignalError(-666,'The user options setting must not be greater than 32767. Please contact the HelpDesk.')
END IF



//Need to swap in "LOGID",as_user_id & "LOGPASS",as_password - <user_id> - <password>

//USER_ID *********************************
li_pos_userid   = Pos( ls_dbparm, '<user_id>' )
//make sure everything is okay
IF isnull(li_pos_userid) OR li_pos_userid = 0	THEN 
	 RETURN of_SignalError(-666,'Userid Could not be determined. Please contact the HelpDesk.')
END IF
//do the replacing
ls_dbparm = Replace(ls_dbparm, li_pos_userid, 9, as_user_id )



//PASSWORD *********************************
li_pos_password = Pos( ls_dbparm, '<password>' )
//make sure everything is okay
IF isnull(li_pos_password) OR li_pos_password = 0	THEN 
	RETURN of_SignalError(-666,'Password Could not be determined. Please contact the HelpDesk.')
END IF
//do the replacing
ls_dbparm = Replace(ls_dbparm, li_pos_password,10, as_password )



// APP_NAME *********************************
li_pos_app_name = Pos( ls_dbparm, '<app_name>' )
//make sure everything is okay
IF isnull(li_pos_app_name) OR li_pos_app_name = 0	THEN 
	RETURN of_SignalError(-666,'Application Name Could not be determined. Please contact the HelpDesk.')
END IF
//do the replacing
ls_dbparm = Replace(ls_dbparm, li_pos_app_name,10,is_application_name)


If as_connection_parameters <> '' Then
	
	li_parameter_count = of_string_to_array(as_connection_parameters,',',lsa_connection_parameters)
	For li_x = 1 to UpperBound(lsa_connection_parameters)
		li_equal_pos = Pos(lsa_connection_parameters[li_x],'=')
		
		If li_equal_pos <= 0 Then
			RETURN of_SignalError(-666,'Invalid format specified for Connection String parameters.' + lsa_connection_parameters[li_x])
		END IF
		ls_parameter_name = left(lsa_connection_parameters[li_x],li_equal_pos - 1)
		ls_parameter_value = mid(lsa_connection_parameters[li_x],li_equal_pos + 1)
		
		//If the parameter_value contains an "=" sign, there are probably multiple connection string parameters that 
		//are specified but the are not correctly sperated by a comma.  A likely mistake would be to use a 
		//semicolon instead.  Example:  DATABASE=CLAIM;QUOTEDID=YES
		//This would result in the DATABASE parameter value being parsed as "CLAIM;QUOTEDID=YES" which is not a valid
		//database name. By checking for an "=" we should be able to trap for this kind of error.
		IF pos(ls_parameter_value,'=') > 0 Then
			RETURN of_SignalError(-666,'Invalid Connection String Parameter delimiter specified. Use commas to seperate parameters.')
		End if
		
		li_param_start_pos = pos(Lower(ls_dbparm),Lower(ls_parameter_name))
		IF li_param_start_pos <=0 Then 
			RETURN of_SignalError(-666,'Invalid Connection String parameter specified.' + lsa_connection_parameters[li_x])
		END IF
		
		li_param_end_pos = pos(ls_dbparm,';',li_param_start_pos + 1) - 1
		IF li_param_end_pos <=0 Then
			li_param_end_pos = pos(ls_dbparm,"'",li_param_start_pos + 1) - 1
			If li_param_end_pos <=0 Then 
				RETURN of_SignalError(-666,'Unable to determine the parameter ending position.' 	+ lsa_connection_parameters[li_x])
			END IF
		End if
		
		ls_dbparm = Replace(ls_dbparm,li_param_start_pos,li_param_end_pos - li_param_start_pos + 1,lsa_connection_parameters[li_x])
	Next
End if 


//check it -- more checks if needed
IF isnull(ls_dbparm) OR trim(ls_dbparm) = ""	THEN  
	RETURN of_SignalError(-666,'DBParm Could not be determined. Please contact the HelpDesk.')
END IF
//set the dbparm into the transaction object	
at_trans.DbParm = ls_dbparm

IF ProfileString(is_logon_ini,as_database_name,"AutoCommit",   "FALSE") = "TRUE" THEN 
	lb_lock = TRUE
END IF
at_trans.Autocommit =  lb_lock



//Record each transaction so the of_end_login function can use them to issue SET commands to the database.
IF UPPER(as_database_name) <> 'SECURITY' THEN
	li_transaction_index = UpperBound(ist_transaction) + 1
	ist_transaction[li_transaction_index].transaction_object = at_trans
	ist_transaction[li_transaction_index].user_options = ll_user_options
END IF


RETURN lgSUCCESS
end function

public function integer of_string_to_array (string as_string, string as_delimeter, ref string as_string_array[]);BOOLEAN 			lb_done
LONG				ll_array_bound
INTEGER			li_semicolon_pos

DO
	ll_array_bound = UpperBound(as_String_Array)
	li_semicolon_pos = Pos(as_string,as_delimeter)
	IF li_semicolon_pos = 0 THEN
		as_string_array[ll_array_bound + 1] = as_string
		lb_done = True
	ELSE
		as_string_array[ll_array_bound + 1] = Left(as_string, li_semicolon_pos - 1)
		as_string = Right(as_string, Len(as_String) - ( li_semicolon_pos) )
	END IF
LOOP UNTIL lb_done


RETURN ll_array_bound
end function

public function integer of_get_transaction (string as_database_name, string as_connection_parameters, ref transaction as_trans);//This function is called from outside this object.    This method calls the
//function that does the real work.  The calling code can't call the other
//function directly because is accpets a password and this is the only object
//that should be supplying a password.

STRING		ls_server_name
STRING		ls_user_id

//Make sure we are logged in first
IF ib_logged_in = False THEN
	RETURN of_SignalError(lgFAILURE,"You must login successfully before calling trying to a transaction.")
END IF

//ls_server_name = ProfileString(is_logon_ini,as_database_name,"ServerName",       " ")
ls_server_name = of_get_server_name(as_database_name)

IF is_login_server_name = '' Then
	is_login_server_name = ls_server_name
	ls_user_id = is_nt_user_id
    of_p_Begin_User_Login(ls_user_id,is_user_password,is_login_server_name,is_application_name,gs_win_auth_flag)

	//Set the Database User Id equal to the user id returned from the p_BEGIN_USER_LOGIN
	is_db_user_id = ls_user_id
		
Elseif is_login_server_name <> ls_server_name Then
	SignalError(-666,'You must end the login procedure for server="' + is_login_server_name + '" before you can get transactiosn for server="' + ls_server_name + '".')
	
END IF


RETURN  THIS.of_get_transaction(as_database_name, as_connection_parameters,is_user_password,is_db_user_id,as_trans)
end function

public function integer of_get_transaction (string as_database_name, ref transaction as_trans);



return this.of_get_transaction(as_database_name,'',as_trans)
end function

public subroutine of_set_logon_section (string as_section_name);

is_logon_section = as_section_name
end subroutine

public subroutine of_set_logon_path_key (string as_logon_path_key);

is_logon_path_key = as_logon_path_key
end subroutine

public subroutine of_check_connection_flag ();// Determine if user connects with Windows Authentication
// or with SQL Server Authentication. Determine by reading the view
// of User_Profile.
	
SELECT win_auth_flag
INTO      :gs_win_auth_flag
FROM    v_User_Profile
WHERE user_id = :is_nt_user_id
USING   READONLY_SECURITY;

IF gs_win_auth_flag = '' THEN
	SignalError(-666,"Connection method for " + is_nt_user_id + " could not be determined.")
END IF

If gs_win_auth_flag = 'I' THEN
	SignalError(-666,"Application cannot be accessed with the current connection method.")
END IF

end subroutine

private function integer of_p_begin_user_login (ref string as_user_id, ref string as_password, string as_server_name, string as_application_name, string as_win_auth_flag);STRING		ls_server_name
STRING		ls_application_name


/* This stored procedure is called to get the password for the current user.
*/

DECLARE lp_BEGIN_USER_LOGIN PROCEDURE FOR p_BEGIN_USER_LOGIN
	@input_value1 = :as_user_id ,
	@input_value2 = :as_application_name,
	@input_value3 = :as_server_name,
	@input_value4 = :as_win_auth_flag
USING READONLY_SECURITY;

IF READONLY_SECURITY.SQLDBCode <> 0 THEN 
	RETURN of_SignalError(-3,'Error declaring stored procedure used to get the user password.')
END IF

EXECUTE lp_BEGIN_USER_LOGIN;

IF READONLY_SECURITY.SQLDBCode <> 0 THEN 
	RETURN of_SignalError(-4,'Error executing stored procedure used to get the user password. SQLERROR: ' + THIS. READONLY_SECURITY.sqlerrtext)
END IF

/* The as_user_id returned might be different from the as_user_id passed in. Use the one returned
   when connecting to the Server.
*/
FETCH lp_BEGIN_USER_LOGIN INTO :as_password,:as_user_id,:ls_server_name,:ls_application_name,:il_connection_number;

IF READONLY_SECURITY.SQLDBCode <> 0 THEN 
	RETURN of_SignalError(-4,'Error fetching user password.')
END IF

CLOSE lp_BEGIN_USER_LOGIN;

IF READONLY_SECURITY.SQLDBCode <> 0 THEN 
	RETURN of_SignalError(-6,'Error closing the stored procedure used to get the user password.')
END IF



RETURN 1
end function

on n_sqlserver.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_sqlserver.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

event constructor;ids_user_options = create datastore

ids_user_options.dataobject = 'd_user_options'
end event

