$PBExportHeader$n_mail.sru
forward
global type n_mail from nonvisualobject
end type
end forward

global type n_mail from nonvisualobject
end type
global n_mail n_mail

type variables


mailSession			imSes
mailReturnCode	imRet
mailMessage		imMsg
DataStore			ids_mail_recipients

s_email_setup istr_email_setup
STRING    is_app, is_template
INTEGER  ii_module, ii_reason
STRING    is_action, is_module
STRING    is_claim_status, is_admin_region
STRING    is_subject, is_text, is_recipients
end variables

forward prototypes
public function integer nf_auto_email ()
public function integer nf_get_claimant_info (long al_claim_no, ref string as_claimant_name, ref string as_claim_status, ref string as_admin_region)
public function integer nf_init_values (s_email_setup astr_email_setup)
public function integer nf_setup_message ()
public function integer nf_setup_recipients ()
public function integer nf_send_mail ()
end prototypes

public function integer nf_auto_email ();STRING ls_msmail_recipient_name
STRING ls_message, ls_subject

// Setup Recipients
IF nf_setup_recipients() < 0 THEN
	RETURN -1
END IF

//Setup the email subject and body
IF nf_setup_message() < 0 THEN
	RETURN -1
END IF

//Send the email message
IF nf_send_mail() < 0 THEN
	RETURN -1
END IF

RETURN 0

end function

public function integer nf_get_claimant_info (long al_claim_no, ref string as_claimant_name, ref string as_claim_status, ref string as_admin_region);//Get the information for the email message
SELECT b.given_names + ' ' + b.last_name, c.claim_status_desc + 
	 CASE  WHEN d.claim_status_type_desc IS NULL 
	 	      THEN ' ' 
               ELSE ' - ' + d.claim_status_type_desc
	  END, 
		   e.admin_region_desc
INTO    :as_claimant_name, 
           :as_claim_status, 
		  :as_admin_region
FROM   CLAIM a
		   JOIN INDIVIDUAL b ON a.individual_no = b.individual_no
		   INNER JOIN Claim_Status c  ON a.claim_status_code = c.claim_status_code
	   	   LEFT OUTER JOIN Claim_Status_Type d  ON  a.claim_status_code         = d.claim_status_code 
					  	 									   AND a.claim_status_type_code = d.claim_status_type_code
		   INNER JOIN Admin_Region e ON a.admin_region_code = e.admin_region_code
WHERE a.claim_no = :al_claim_no
USING   SQLCA;

IF SQLCA.nf_handle_error("n_mail","nf_get_claimant_info","SELECT ... FROM CLAIM a, INDIVIDUAL b") < 0 THEN
	RETURN -1
END IF

RETURN 0
end function

public function integer nf_init_values (s_email_setup astr_email_setup);istr_email_setup = astr_email_setup

RETURN 0

end function

public function integer nf_setup_message ();STRING ls_claimant_name, ls_admin_region, ls_claim_status
STRING ls_action, ls_module

//Get email reason
SELECT email_reason_desc
INTO    :ls_action
FROM   Email_Reason
WHERE email_reason_code = :istr_email_setup.ai_email_reason_code
USING   SQLCA;

IF SQLCA.nf_handle_error("n_mail","nf_setup_message","SELECT email_reason_desc") < 0 THEN
	RETURN -1 
END IF

//Get module description
SELECT module_desc
INTO    :ls_module
FROM   Module
WHERE module_code = :istr_email_setup.ai_app_module_code
AND      active_flag = 'Y'
USING   SQLCA;

IF SQLCA.nf_handle_error("n_mail","nf_setup_message","SELECT module_desc") < 0 THEN
	RETURN -1 
END IF

IF nf_get_claimant_info(istr_email_setup.al_claim_no, ls_claimant_name, ls_claim_status, ls_admin_region) < 0 THEN
	RETURN -1
END IF

is_subject   = istr_email_setup.as_subject_type + ' - ' + ls_claimant_name + ' - ' + STRING(istr_email_setup.al_claim_no)

is_text = 'Notification Reason: ' + ls_action + '~r~n' + &
				         'Claim Status: ' + ls_claim_status + '~r~n' + & 
			   	         'Claim Region: ' + ls_admin_region + '~r~n'  + & 
				         'Module: ' + ls_module

RETURN 0
end function

public function integer nf_setup_recipients ();Int		li_upperbound, li_rowcount, li_counter
String	ls_email_recipient_name


ids_mail_recipients = CREATE DATASTORE
ids_mail_recipients.dataobject = 'd_email_notification'
ids_mail_recipients.SetTransObject(SQLCA)

ids_mail_recipients.Retrieve(istr_email_setup.as_application_code, istr_email_setup.ai_app_module_code, istr_email_setup.as_template_code, istr_email_setup.ai_email_reason_code)
SQLCA.nf_handle_error("n_mail","nf_setup_recipients","ids_mail_recipients.Retrieve")

li_rowcount = ids_mail_recipients.RowCount()

IF li_rowcount = 0 THEN
	MessageBox('Warning!','An email could not be composed and sent as there are no active recipients.', information!)
END IF

// Set up mail recipients

FOR li_counter = 1 to li_rowcount
	ls_email_recipient_name = ids_mail_recipients.getitemstring(li_rowcount,"email_address")
	
	IF ls_email_recipient_name = "" THEN
		Messagebox("Warning","Can not send email notification, the recipient name is missing.",Information!)
		Return -1
	ELSE
		IF li_counter = li_rowcount THEN
			is_recipients = is_recipients + ls_email_recipient_name
		ELSE 
			is_recipients = is_recipients + ls_email_recipient_name + ','
		END IF
	END IF
NEXT

Return 1
end function

public function integer nf_send_mail ();STRING ls_from, ls_run
INTEGER li_rtn

ls_from = "default"

ls_run = gs_email_path + ' "' + ls_from + '" "' + is_recipients + '" "' + is_subject + '" "' + is_text + '"'

li_rtn = Run(ls_run, Minimized!) 

RETURN li_rtn
end function

on n_mail.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_mail.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

