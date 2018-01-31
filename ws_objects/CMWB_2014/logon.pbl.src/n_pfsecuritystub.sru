$PBExportHeader$n_pfsecuritystub.sru
$PBExportComments$PowerFrame security stub
forward
global type n_pfsecuritystub from nonvisualobject
end type
type uos_secured_window from structure within n_pfsecuritystub
end type
type uos_secured_control from structure within n_pfsecuritystub
end type
type uos_secured_column from structure within n_pfsecuritystub
end type
end forward

type uos_secured_window from structure
    string window_name
    boolean secured
    boolean secured_controls
end type

type uos_secured_control from structure
    string window_name
    string control_name
    string control_type
end type

type uos_secured_column from structure
    string database_name
    string table_name
    string column_name
    boolean authorized_viewing
    boolean authorized_updating
end type

global type n_pfsecuritystub from nonvisualobject
event uoe_closewindow pbm_custom01
event uoe_showwindow pbm_custom02
end type
global n_pfsecuritystub n_pfsecuritystub

type variables
// Used to store the application name and user.
String i_application_name
String i_user_id

// Used to talk to the security tables for the application.
n_transaction I_TransObject

// Used to store mail recipients for error handler
String vis_mail_recipients[] 

// Used to store user's first name and last name
String is_user_profile_first_name, is_user_profile_last_name

end variables

forward prototypes
public function boolean uof_check_access (graphicobject arg_graphic_object)
public function integer uof_initialize ()
public function integer uof_login (string arg_user_id, string arg_password, string arg_application_name, n_transaction arg_transobject)
public function string uof_return_mail_recipient (integer vai_counter)
public subroutine uof_retrieve_user_profile (ref string vas_first_name, ref string vas_last_name)
public function integer uof_retrieve_mail_recipients ()
end prototypes

public function boolean uof_check_access (graphicobject arg_graphic_object);// uof_check_access
//
RETURN TRUE 

end function

public function integer uof_initialize ();// uof_initialize 
//
RETURN 1 


end function

public function integer uof_login (string arg_user_id, string arg_password, string arg_application_name, n_transaction arg_transobject);// uof_login
//
RETURN 1 

end function

public function string uof_return_mail_recipient (integer vai_counter);// uof_return_mail_recipients 
//
RETURN vis_mail_recipients[vai_counter] 

end function

public subroutine uof_retrieve_user_profile (ref string vas_first_name, ref string vas_last_name);// uof_retrieve_user_profile
// 
vas_first_name = is_user_profile_first_name  
vas_last_name  = is_user_profile_last_name  

end subroutine

public function integer uof_retrieve_mail_recipients ();// uof_retrieve_mail_recipients 
//
String ls_recipient_name 
Long   ll_rtn, ll_count

ll_count = 1

// Retrieve and store mail recipients for application in user structure
DECLARE MARK CURSOR FOR  
SELECT email_address
  FROM Application_Mail_Recipients
 WHERE application = :i_application_name 
 USING I_TransObject ; 

OPEN MARK ; 

ll_rtn = I_TransObject.nf_handle_error('',"n_pfsecuritystub", "uof_retrieve_mail_recipients - OPEN MARK") 
IF ll_rtn < 0 THEN
	MessageBox("Mail Recipients", "Unable to define mail recipients.") 
	RETURN -1
END IF

DO UNTIL ll_rtn = 100 
	FETCH MARK INTO :ls_recipient_name ; 

	IF I_TransObject.SQLCode = -1 THEN
		IF I_TransObject.SQLDBCODE <> 10083 THEN
			Error.Text = SQLCA.SQLErrText
			Error.WindowMenu = "cmwb"
			Error.Object = "n_pfsecuritystub"
			Error.ObjectEvent = "Cursor retrieve of mail recipients"
			SignalError()
			RETURN -1
		ELSE  
			ll_rtn = 100
			EXIT
		END IF
	ELSE
		ll_rtn = I_TransObject.nf_handle_error('',"n_pfsecuritystub", "uof_retrieve_mail_recipients - FETCH MARK INTO :ls_recipient_name")
		IF ll_rtn < 0 THEN
			MessageBox("Mail Recipients", "Unable to define mail recipients.")
			RETURN -1
		END IF
		IF ll_rtn = 100 THEN
			EXIT 
		END IF
	END IF
	vis_mail_recipients[ll_count] = ls_recipient_name
	ll_count = ll_count + 1
LOOP 

CLOSE MARK ; 

ll_rtn = I_TransObject.nf_handle_error('',"n_pfsecuritystub", "uof_retrieve_mail_recipients - CLOSE MARK") 
IF ll_rtn < 0 THEN
	MessageBox("Mail Recipients", "Unable to define mail recipients.") 
	RETURN -1
END IF

RETURN 0

end function

on n_pfsecuritystub.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_pfsecuritystub.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

