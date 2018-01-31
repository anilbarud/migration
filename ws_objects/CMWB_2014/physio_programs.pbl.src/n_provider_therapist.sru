$PBExportHeader$n_provider_therapist.sru
forward
global type n_provider_therapist from nonvisualobject
end type
end forward

global type n_provider_therapist from nonvisualobject
end type
global n_provider_therapist n_provider_therapist

forward prototypes
public function string nf_get_provider_subtype (long al_provider_no)
public function integer nf_get_provider_program_count (long al_provider_no)
public function boolean nf_provider_approved_for_primary (long al_provider_no)
public function boolean nf_can_therapist_program_be_deleted (long al_therapist_no, string as_program_code)
public function boolean nf_can_provider_program_be_deleted (long al_provider_no, string as_program_code)
public function long nf_get_next_therapist_no ()
public function string nf_get_therapist_name (long al_wif_principal_id)
public function boolean nf_check_therapist_license_exists (long al_wif_principal_id, string as_license_type_code, string as_license_prov_state_code)
public function boolean nf_check_license_program_exists (long al_wif_principal_id, string as_program_code)
public function long nf_get_therapist_no (long al_wif_principal_id)
public function boolean nf_check_therapist_exists (long al_wif_principal_id)
public function boolean nf_is_program_a_rehab_program (string as_program_code)
public function integer nf_create_email (long al_principal_id, string as_license_no, string as_license_type, string as_prov_state, string as_name)
public function integer nf_get_token_status (long al_record_no, long al_principal_id, string as_license_no)
public function integer nf_has_therapist_been_rejected (long al_record_no, long al_principal_id, string as_license_no)
public function boolean nf_is_license_unique (string as_no, string as_type, string as_province)
public function boolean nf_is_license_unique_key (long al_therapist_no, string as_type, string as_license_no)
public function string nf_get_location_description (string as_prov_state_code)
public function boolean nf_portal_account_is_active (long al_wif_principal_id)
public function boolean nf_is_license_unique_key_idx1 (string as_type, string as_license_no)
end prototypes

public function string nf_get_provider_subtype (long al_provider_no);/*
This functionality is only applicable for a Provider that is has a sub-type of Physiotherapy Clinic – 
it does not matter whether the physiotherapy clinic is under the Physio contract or whether the physiotherapy clinic is set up for ePhysio.

PROVIDER_SUB_TYPE_CODE
M                  35                     Physio Clinic

*/
STRING	ls_provider_sub_type

SELECT 		provider_sub_type_code 
INTO 			:ls_provider_sub_type 
FROM 		PROVIDER 
WHERE 		provider_no = :al_provider_no
USING		SQLCA;
SQLCA.nf_handle_error("n_provider_therapist", "nf_get_provider_subtype()","SELECT provider_sub_type_code ")	

IF ISNULL(ls_provider_sub_type) THEN ls_provider_sub_type = ''


RETURN ls_provider_sub_type
end function

public function integer nf_get_provider_program_count (long al_provider_no);/*
A Physiotherapy provider must be approved for at least one Physiotherapy program 
(e.g. Work Conditioning, Shoulder program, Back program) if the provider is active.
*/
INTEGER		li_return

SELECT 		count(*) 
INTO 			:li_return 
FROM 		PROVIDER 
WHERE 		provider_no = :al_provider_no
USING		SQLCA;
SQLCA.nf_handle_error("n_provider_therapist", "nf_get_provider_program_count()","SELECT provider_sub_type_code ")	

IF ISNULL(li_return) THEN li_return = 0


RETURN li_return
end function

public function boolean nf_provider_approved_for_primary (long al_provider_no);/* A Physiotherapy provider must be approved for the Primary Physiotherapy program if the provider is active. */

INTEGER		li_count

SELECT 			count(*) 
INTO 				:li_count
FROM 		 	REHAB_PROGRAM_xref_PROVIDER
WHERE          	rehab_program_code = 'P001'
AND 		      	provider_no 			= :al_provider_no
USING			SQLCA;
SQLCA.nf_handle_error("n_provider_therapist", "nf_provider_approved_for_primary()","SELECT count(*) ")	

IF ISNULL(li_count) THEN li_count = 0

IF li_count = 1 THEN RETURN TRUE

RETURN FALSE
end function

public function boolean nf_can_therapist_program_be_deleted (long al_therapist_no, string as_program_code);INTEGER			li_count

// counts draft reports
SELECT 	count(*) 
INTO 		:li_count
FROM 	REPORT_PHYSIO_MASTER  
WHERE 	therapist_no =  :al_therapist_no 
AND 		rehab_program_code = :as_program_code
AND       physio_report_status_code = 'D'
USING	SQLCA;
SQLCA.nf_handle_error("n_provider_therapist", "nf_can_therapist_program_be_deleted","SELECT count(*) ")	

IF li_count > 0 THEN RETURN FALSE


RETURN TRUE
end function

public function boolean nf_can_provider_program_be_deleted (long al_provider_no, string as_program_code);INTEGER			li_count

SELECT 	count(*) 
INTO 		:li_count
FROM 	REPORT_PHYSIO_MASTER  
WHERE 	provider_no =  :al_provider_no 
AND 		rehab_program_code = :as_program_code
USING	SQLCA;
SQLCA.nf_handle_error("n_provider_therapist", "nf_can_provider_program_be_deleted","SELECT count(*) ")	

IF li_count > 0 THEN RETURN FALSE


RETURN TRUE
end function

public function long nf_get_next_therapist_no ();LONG	ll_therapist_no

UPDATE Last_Therapist_No SET last_therapist_no = last_therapist_no + 1 USING SQLCA;
SQLCA.nf_handle_error("Embedded SQL: Update last_therapist_no","n_provider_therapist","nf_get_next_therapist_no")

CHOOSE CASE SQLCA.SQLNRows
	/*	If update was successful (ie. SQLNRows would equal 1), read back the identifier */	
	CASE 1
	
		SELECT Last_Therapist_No.last_therapist_no INTO :ll_therapist_no FROM Last_Therapist_No USING SQLCA;
		 SQLCA.nf_handle_error("Embedded SQL: Update last_therapist_no","n_provider_therapist","nf_get_next_therapist_no")
			
	CASE ELSE
		/*		if anything other than 1 record found, display error*/
		SQLCA.nf_rollback_transaction()
		IF SQLCA.SQLCode <> 0 THEN
			Error.Text 				= "Error during rollback of last_therapist_no in function nf_get_next_therapist_no"
			Error.WindowMenu	=	"w_maintain_provider_therapist"
			Error.Object				=	""
			Error.ObjectEvent		=	"nf_get_next_therapist_no()"
			SignalError()
		END IF		
		MessageBox("Maintain Provider/Therapist - Data Integrity Error", string(SQLCA.SQLNRows) + " record(s) found in last_therapist_no~r~nPlease call the help desk",Exclamation!)
		RETURN -1

	END CHOOSE
		
RETURN ll_therapist_no
end function

public function string nf_get_therapist_name (long al_wif_principal_id);STRING			ls_name

SELECT 	first_name + " " + last_name
INTO 		:ls_name
FROM 	WIF_CUSTOM_PRINCIPAL b
WHERE 	wif_principal_id =  :al_wif_principal_id 
USING	SQLCA;

SQLCA.nf_handle_error("n_provider_therapist", "nf_get_therapist_name()","SELECT 	first_name  last_name")	

IF ISNULL(ls_name)  THEN ls_name =''

RETURN ls_name
end function

public function boolean nf_check_therapist_license_exists (long al_wif_principal_id, string as_license_type_code, string as_license_prov_state_code);/* •	The license number must be unique for the license type and the licensing  province */
INTEGER			li_license_count

SELECT 	count(*)
INTO 		:li_license_count
FROM 	THERAPIST a
	join WIF_CUSTOM_PRINCIPAL b 	ON a.wif_principal_id 	= b.wif_principal_id    
	join THERAPIST_LICENSE c 			ON a.therapist_no 		= c.therapist_no     
WHERE  	a.wif_principal_id  			= :al_wif_principal_id
AND 		c.license_type_code 			= :as_license_type_code 
AND 		c.license_prov_state_code 	= :as_license_prov_state_code
USING	sqlca;

SQLCA.nf_handle_error("n_provider_therapist","nf_is_license_unique","SELECT count(*) ")

IF ISNULL(li_license_count) THEN li_license_count = 0

IF li_license_count > 0 THEN RETURN TRUE

RETURN FALSE //UNIQUE
end function

public function boolean nf_check_license_program_exists (long al_wif_principal_id, string as_program_code);/* •	The license number must be unique for the license type and the licensing  province */
INTEGER			li_license_count

//check if the license already exists......
SELECT 	count(*)
INTO 		:li_license_count
FROM 	THERAPIST a
	join WIF_CUSTOM_PRINCIPAL b 				ON 	a.wif_principal_id 	= b.wif_principal_id
	join REHAB_PROGRAM_xref_THERAPIST c 	ON 	c.therapist_no 		= a.therapist_no              
WHERE  	a.wif_principal_id  			= :al_wif_principal_id
AND 		c.rehab_program_code 		= :as_program_code
USING 	sqlca;
SQLCA.nf_handle_error("n_provider_therapist","nf_check_license_program_exists()","SELECT count(*) ")

IF ISNULL(li_license_count) THEN li_license_count = 0

IF li_license_count > 0 THEN RETURN TRUE

RETURN FALSE //UNIQUE


end function

public function long nf_get_therapist_no (long al_wif_principal_id);LONG			ll_therapist_no

// check to see if this individual is already a therapist
SELECT 	a.therapist_no
INTO 		:ll_therapist_no
FROM 	THERAPIST a
	join WIF_CUSTOM_PRINCIPAL b 	ON a.wif_principal_id 	= b.wif_principal_id         
WHERE  	a.wif_principal_id  = :al_wif_principal_id
USING 	sqlca;

SQLCA.nf_handle_error("n_provider_therapist", "nf_get_therapist_no()","SELECT count(*) ")	

IF ISNULL(ll_therapist_no) OR ll_therapist_no < 1 THEN ll_therapist_no = 0

RETURN ll_therapist_no
end function

public function boolean nf_check_therapist_exists (long al_wif_principal_id);INTEGER			li_count

// check to see if this individual is already a therapist
SELECT 	count(*)
INTO 		:li_count
FROM 	THERAPIST a
	join WIF_CUSTOM_PRINCIPAL b 	ON a.wif_principal_id 	= b.wif_principal_id         
WHERE  	a.wif_principal_id  = :al_wif_principal_id
USING 	sqlca;

SQLCA.nf_handle_error("n_provider_therapist", "nf_check_therapist_approved()","SELECT count(*) ")	

IF li_count > 0 THEN RETURN TRUE


RETURN FALSE
end function

public function boolean nf_is_program_a_rehab_program (string as_program_code);INTEGER			li_count

SELECT 	count(*) 
INTO 		:li_count
FROM 	rehab_program  
WHERE 	rehab_program_code = :as_program_code
AND      	active_flag = 'Y'
USING	SQLCA;
SQLCA.nf_handle_error("n_provider_therapist", "nf_is_program_a_rehab_program","SELECT count(*) ")	

IF li_count > 0 THEN RETURN TRUE


RETURN FALSE
end function

public function integer nf_create_email (long al_principal_id, string as_license_no, string as_license_type, string as_prov_state, string as_name);INTEGER					li_rc, li_recipient_count, li_counter, li_rows, li_wif_claim_value_code
STRING					ls_test, ls_sendto, ls_recipients[], ls_body, ls_email, ls_error, ls_subject
BOOLEAN				lb_check
OLEObject 				l_oleApplication,  l_olMailItem, l_attachment
mailReturnCode    		mRet
mailRecipient        	mRecip

u_ds						lds_entity_admin_for_email


lds_entity_admin_for_email = CREATE u_ds
lds_entity_admin_for_email.DataObject = 'd_entity_admin_for_email'


/*
The entity administrators associated with a therapist’s account must be notified if the therapist becomes delisted and the therapist has a user account.   
*/

// grab the admin if there are none no point in continuing
lds_entity_admin_for_email.settransobject(sqlca)
lds_entity_admin_for_email.retrieve(al_principal_id) // grabs all of the entity admins for the applicable therapist - could be multiple providers
SQLCA.nf_handle_error('n_provider_therapist', 'nf_create_email', 'lds_entity_admin_for_email.retrieve(al_therapist_no)')

ls_subject = 'Therapist has Become delisted'

//e.g   Subject:     Jane Leblanc  (888555)
ls_subject 	=  ls_subject
ls_body 		=  'Therapist: ' + as_name + ' License No. ' + as_license_no + ' License Type: ' + as_license_type + ' Prov/State: ' + as_prov_state + ' Has become delisted.'

// Connect to Outlook
l_oleApplication = CREATE OLEObject
li_rc = l_oleApplication.ConnectToNewObject("Outlook.Application")
IF li_rc < 0 THEN 
	MessageBox("Error connecting to Outlook", String(li_rc))
	RETURN 1
END IF

l_olMailItem = l_oleApplication.CreateItem(0)

li_recipient_count 	= 1
li_rc 					= lds_entity_admin_for_email.rowcount()

// no recipients
IF li_rc < 1 THEN RETURN 1
// for testing
//ls_recipients[li_recipient_count] = 'james.walker@ws-ts.nb.ca'

 
/* load up the recipients with only those that  are selected */
FOR li_counter = 1 TO li_rc
		ls_recipients[li_recipient_count] = lds_entity_admin_for_email.getitemstring(li_counter, 'wif_principal_name')
		li_recipient_count ++
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
		  	ls_error = 'Please ensure outlook is open before proceeding with the creation of this email'
      		messagebox('Recipient not added', ls_error,Exclamation!, OK!, 1)
      		RETURN -1
		END TRY	
	NEXT
END IF
   
/* fill in the rest of the mail message */  
l_olMailItem.Subject 	= ls_subject
l_olMailItem.body 		= ls_body

/* display the defaulted mail message */ 
l_olMailItem.Display

RETURN 1

end function

public function integer nf_get_token_status (long al_record_no, long al_principal_id, string as_license_no);/*
 token_status_code 		token_status_desc_e                               
----------------- 				--------------------    
0                 				Available           
1                 				Used                
2                 				Expired             
3                 				Cancelled          
99                				Error               
*/

INTEGER			li_status_code

SELECT 	b.token_status_code
INTO 		:li_status_code
FROM 	WORKFLOW_THERAPIST_APPROVAL  a
    join 	WORKFLOW_TOKEN b ON a.token_id = b.token_id
WHERE 	a.record_no 					= :al_record_no
AND 		a.therapist_wif_principal_id = :al_principal_id
AND 		a.license_no 					= :as_license_no
USING sqlca;

SQLCA.nf_handle_error("n_provider_therapist","nf_get_token_status","SELECT b.token_status_code")


IF ISNULL(li_status_code) THEN RETURN -1

RETURN li_status_code
end function

public function integer nf_has_therapist_been_rejected (long al_record_no, long al_principal_id, string as_license_no);/*
 token_status_code 		token_status_desc_e                               
----------------- 				--------------------    
0                 				Available           
1                 				Used                
2                 				Expired             
3                 				Cancelled          
99                				Error      

approval_status_code approval_status_desc_e
-------------------- ----------------------
0                    Pending
1                    Approved
2                    Rejected

*/
INTEGER			li_rejected_count

SELECT count(*)
INTO 		:li_rejected_count
FROM 	WORKFLOW_THERAPIST_APPROVAL  a
    join 	WORKFLOW_TOKEN b ON a.token_id = b.token_id
WHERE 	a.record_no 					= :al_record_no
AND 		a.therapist_wif_principal_id = :al_principal_id
AND 		a.license_no 					= :as_license_no
AND        a.approval_status_code       = '2'
USING sqlca;

SQLCA.nf_handle_error("n_provider_therapist","nf_has_therapist_been_rejected()","SELECT count(*)")


IF ISNULL(li_rejected_count) THEN RETURN -1

RETURN li_rejected_count
end function

public function boolean nf_is_license_unique (string as_no, string as_type, string as_province);/* •	The license number must be unique for the license type and the licensing  province */
INTEGER			li_license_count

SELECT 	count(*) 
INTO		:li_license_count
FROM  	THERAPIST_LICENSE  
WHERE 	license_no 							= 		:as_no
AND 		license_type_code 				= 		:as_type
AND 		license_prov_state_code       	= 		:as_province
USING	sqlca;

SQLCA.nf_handle_error("n_provider_therapist","nf_is_license_unique","SELECT count(*) ")


IF li_license_count > 0 THEN RETURN FALSE

RETURN TRUE //UNIQUE
end function

public function boolean nf_is_license_unique_key (long al_therapist_no, string as_type, string as_license_no);/* •	The license number must be unique for the license type and the licensing  province */
INTEGER			li_license_count

SELECT 	count(*) 
INTO		:li_license_count
FROM  	THERAPIST_LICENSE  
WHERE 	therapist_no 			= 		:al_therapist_no
AND 		license_type_code 	= 		:as_type
AND 		license_no       			= 		:as_license_no
USING	sqlca;

SQLCA.nf_handle_error("n_provider_therapist","nf_is_license_unique_key()","SELECT count(*) ")


IF li_license_count > 0 THEN RETURN FALSE

RETURN TRUE //UNIQUE
end function

public function string nf_get_location_description (string as_prov_state_code);STRING			ls_return 

SELECT 	location_desc1 
INTO		:ls_return 
FROM		Location 
WHERE 	prov_state_code 			= :as_prov_state_code
AND		location_type_code 		= 'P'
USING	SQLCA;
SQLCA.nf_handle_error("n_provider_therapist", "nf_get_location_description()","SELECT location_desc1 INTO:ls_return")	

IF isnull(ls_return) THEN ls_return = ''


RETURN ls_return
end function

public function boolean nf_portal_account_is_active (long al_wif_principal_id);INTEGER			li_count

SELECT 	count(*) 
INTO 		:li_count
FROM 	WIF_CUSTOM_PRINCIPAL  
WHERE 	wif_principal_id =  :al_wif_principal_id 
USING	SQLCA;
SQLCA.nf_handle_error("n_provider_therapist", "nf_portal_account_is_active()","SELECT count(*) ")	

IF isnull(li_count)  THEN li_count = 0

IF li_count > 0 THEN RETURN TRUE


RETURN FALSE
end function

public function boolean nf_is_license_unique_key_idx1 (string as_type, string as_license_no);/* •	The license number must be unique for the license type and the licensing  province */
INTEGER			li_license_count

SELECT 	count(*) 
INTO		:li_license_count
FROM  	THERAPIST_LICENSE  
WHERE 	license_type_code 	= 		:as_type
AND 		license_no       		= 		:as_license_no
USING	sqlca;


SQLCA.nf_handle_error("n_provider_therapist","nf_is_license_unique_key_idx1()","SELECT count(*)")


IF li_license_count > 0 THEN RETURN FALSE

RETURN TRUE //UNIQUE
end function

on n_provider_therapist.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_provider_therapist.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

