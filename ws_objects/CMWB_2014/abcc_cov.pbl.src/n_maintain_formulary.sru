$PBExportHeader$n_maintain_formulary.sru
$PBExportComments$controller object for n_claim_participant & n_individual
forward
global type n_maintain_formulary from n_pdc
end type
end forward

global type n_maintain_formulary from n_pdc
end type
global n_maintain_formulary n_maintain_formulary

type variables
N_INDIVIDUAL inv_individual
N_CLAIM_PARTICIPANT inv_participant
LONG il_edit_row
LONG il_claim_no, il_individual_no, il_current_row
DATASTORE ids_eligibility_coverage, ids_eligibility_history
DATASTORE ids_formulary_coverage, ids_formulary_history
BOOLEAN ib_switched = FALSE, ib_auto_created = FALSE
s_window_message istr_message
end variables

forward prototypes
public function integer nf_set_formulary_no ()
public function integer nf_basic_edit_checks (string as_value, integer ai_check)
public function long nf_get_eligibility_no ()
public function integer nf_add_last_number (string as_type)
public function integer nf_check_status ()
public function long nf_get_formulary_no ()
public subroutine nf_init ()
public function integer nf_retrieve (long al_claim_no)
public function long nf_get_claim_no ()
public function integer nf_add_event ()
public function integer nf_add_formulary ()
public function integer nf_check_bus_rule ()
public function integer nf_check_mandatory ()
public function integer nf_insert (long al_claim_no)
public function integer nf_add_eligibility ()
public function integer nf_create_eligibility_history (long al_claim_no)
public function integer nf_create_formulary_history (long al_claim_no)
public function boolean nf_has_record_been_exported (long al_claim_no, long al_record_no, string as_type)
public function integer nf_get_open_count (long al_claim_no)
public function date nf_get_new_date (string as_case)
public function integer nf_save ()
public function integer nf_add_automated_event (integer ai_type)
public function integer nf_switch_formulary (long al_claim_no)
public function date nf_get_abcc_work_day ()
public function date nf_get_termination_date (long al_claim_no, string as_type, long al_record_no)
public function date nf_get_effective_date (long al_claim_no, long al_record_no)
public function integer nf_delete_formulary (long al_claim_no)
public function integer nf_validate_secondary_formulary (integer ai_row)
public function integer nf_validate_primary_formulary (integer ai_row)
public function integer nf_create_auto_formulary ()
public function integer nf_delete_eligibility (long al_claim_no)
public function integer nf_validate_formulary ()
public function boolean nf_check_modified_status (integer ai_datawindow)
public function integer nf_cancel ()
public function integer nf_create_auto_eligibility ()
public function integer nf_validate_eligibility ()
public function string nf_check_if_active_formulary (string as_formulary_code)
public function date nf_get_accident_date ()
public function string nf_get_last_name ()
public function string nf_get_given_names ()
public function long nf_get_individual_no ()
public function integer nf_add_drug_alert ()
public function integer nf_add_individual_alert_event (long al_row)
public function integer nf_validate_alert ()
public function long nf_get_alert_no ()
public function integer nf_terminate_drug_alert (long al_row)
public function integer nf_get_event_info (string as_alert_type, ref string as_event_type, ref string as_event_desc)
end prototypes

public function integer nf_set_formulary_no ();RETURN 1
end function

public function integer nf_basic_edit_checks (string as_value, integer ai_check);/*  this will contain basic edit checks - things will be converted from 
    the string datatype dependant upon the edit check
*/
CHOOSE CASE ai_check
		
	CASE 1//CHECK FOR NULL VALUE
		IF isnull(as_value) THEN RETURN -1
		
	CASE 2//CHECK FOR INVALID DATE
		IF NOT ISDATE(as_value) THEN RETURN -1
		
	CASE 3//CHECK FOR EMPTY STRING OR SPACES
		IF LEN(TRIM(as_value)) < 3 THEN RETURN -1
		
	CASE 4//VALID IF ISNULL
		IF NOT isnull(as_value) THEN RETURN -1
		
	CASE 5
		
END CHOOSE

RETURN 1
end function

public function long nf_get_eligibility_no ();LONG	ll_no

/* To ensure that we get the next number, Update the Last_Eligibility_Record_No 
   table incrementing the last_eligibility_record_no by 1  (This will lock it so 
	no one else can get in). Then, read it back  
*/

UPDATE Last_Eligibility_Record_No 
   SET last_eligibility_record_no = last_eligibility_record_no + 1 
 USING SQLCA;

SQLCA.nf_handle_error("n_maintain_formulary","nf_get_eligibility_no()","UPDATE Last_Eligibility_Record_No")

IF SQLCA.SQLNRows = 1 THEN  // successful (ie. SQLNRows would equal 1), read back the identifier
	
	SELECT last_eligibility_record_no 
	  INTO :ll_no 
	  FROM Last_Eligibility_Record_No 
	 USING SQLCA;
	 
	SQLCA.nf_handle_error("n_maintain_formulary","nf_get_eligibility_no()","SELECT last_eligibility_record_no")
	
ELSE // display error
	SQLCA.nf_rollback_transaction()
	
	MessageBox("Data Integrity Error", String(SQLCA.SQLNRows) + " record(s) found in Last_Eligibility_Record_No~r~nPlease call the help desk",Exclamation!)
	RETURN -1
END IF

RETURN ll_no
end function

public function integer nf_add_last_number (string as_type);/* Function adds last number to the tables that have new modified records
   Pass in "FORMULARY",  "ELIGIBILITY" OR "DRUG_ALERT" AND THE FUNCTION  should
	do the rest.
*/
INTEGER li_counter
LONG    ll_record_no

CHOOSE CASE as_type
		
	CASE "ELIGIBILITY"
		FOR  li_counter = 1 TO idw_dw[4].rowcount()
			IF idw_dw[4].getitemstatus(li_counter,0, PRIMARY!) = newmodified! THEN
				
				/* grab the last_eligibility_record_no */
				ll_record_no = nf_get_eligibility_no()
				
				IF ISNULL(ll_record_no) OR ll_record_no < 1 THEN RETURN -1
				
				idw_dw[4].setitem(li_counter,"eligibility_record_no",ll_record_no)
				
			END IF 
		NEXT
		
	CASE "FORMULARY"
		
		FOR  li_counter = 1 TO idw_dw[1].rowcount()
			IF idw_dw[1].getitemstatus(li_counter,0, PRIMARY!) = newmodified! THEN
				
				/* grab the last_Formulary_record_no */
            ll_record_no = nf_get_formulary_no()

            IF ISNULL(ll_record_no) OR ll_record_no < 1 THEN RETURN -1
            idw_dw[1].setitem(li_counter,"formulary_record_no",ll_record_no)
				
			END IF 
		NEXT
	CASE "DRUG_ALERT"
		FOR  li_counter = 1 TO idw_dw[12].RowCount()
			IF idw_dw[12].GetItemStatus(li_counter,0, PRIMARY!) = NewModified! THEN
				
				/* grab the last_drug_alert_no */
           	 	ll_record_no = nf_get_alert_no()

				IF ISNULL(ll_record_no) OR ll_record_no < 1 THEN RETURN -1
				idw_dw[12].SetItem(li_counter,"drug_alert_no",ll_record_no)
				
			END IF 
		NEXT
		
	CASE ELSE
		RETURN -1
END CHOOSE

RETURN 1
end function

public function integer nf_check_status ();w_sheet  lw_active_sheet
STRING   ls_claim_status, ls_claim_status_type
LONG     ll_claim_no

/* can only be added if the claim is active or final. This is just a second safety check
	is also checked from the calling code
*/
lw_active_sheet      = w_frame.GetActiveSheet()

/* grab the claim number */
ll_claim_no = nf_get_claim_no()

//validate the claim number
IF ISNULL(ll_claim_no) OR ll_claim_no < 1 THEN RETURN -1

ls_claim_status      = lw_active_sheet.dw_basic_claim.GetItemString(1,"claim_status_code")
ls_claim_status_type = lw_active_sheet.dw_basic_claim.GetItemString(1,"claim_status_type_code")

IF ls_claim_status <> "A" THEN
	IF ls_claim_status <> "F" THEN
		RETURN -1
	ELSE
		CHOOSE CASE ls_claim_status_type
			CASE "01","02","03","04"
			CASE ELSE
				RETURN -1
		END CHOOSE	
	END IF 
END IF

RETURN 1
end function

public function long nf_get_formulary_no ();LONG	ll_no

// To ensure that we get the next number, Update the Last_Formulary_Record_No table incrementing the  
// last_formulary_record_no by 1  (This will lock it so no one else can get in). Then, read it back                 
UPDATE Last_Formulary_Record_No 
   SET last_formulary_record_no = last_formulary_record_no + 1 
 USING SQLCA;

SQLCA.nf_handle_error("n_maintain_formulary","nf_get_formulary_no()","UPDATE Last_Formulary_Record_No")

IF SQLCA.SQLNRows = 1 THEN  // successful (ie. SQLNRows would equal 1), read back the identifier
	
	SELECT last_formulary_record_no 
	  INTO :ll_no 
	  FROM Last_Formulary_Record_No 
	 USING SQLCA;
	 
	SQLCA.nf_handle_error("n_maintain_formulary","nf_get_formulary_no()","SELECT last_formulary_record_no")
	
ELSE // display error
	SQLCA.nf_rollback_transaction()
	
	MessageBox("Data Integrity Error", String(SQLCA.SQLNRows) + " record(s) found in Last_Formulary_Record_No~r~nPlease call the help desk",Exclamation!)
	RETURN -1
END IF

RETURN ll_no
end function

public subroutine nf_init ();U_DWA ldw_dw[]

/*	register the parent and the datawindows */
ldw_dw[1] = idw_dw[1]//dw_formulary												
ldw_dw[2] = idw_dw[2]//dw_rx														  
ldw_dw[3] = idw_dw[3]//dw_event	
ldw_dw[4] = idw_dw[4]//dw_eligibility
ldw_dw[5] = idw_dw[5]//dw_formulary_pres_individual
ldw_dw[6] = idw_dw[6]//dw_rx_special_auth
ldw_dw[7] = idw_dw[7]//dw_rx_sa_restriction
ldw_dw[8] = idw_dw[8]//dw_rx_special_auth_history
ldw_dw[9]= idw_dw[9]//dw_eligibility_history
ldw_dw[10]= idw_dw[10]//dw_formulary_history
ldw_dw[11]= idw_dw[11]//dw_restricition_history
ldw_dw[12]= idw_dw[12]//dw_drug_alert
															
/*	set up the commit flag to only allow this controller to commit */
nf_set_commit(TRUE)

RETURN
end subroutine

public function integer nf_retrieve (long al_claim_no);long	ll_individual_no

//make sure the argument is valid
IF ISNULL(al_claim_no) OR al_claim_no < 1 THEN RETURN -1

//CLAIM_FORMULARY
idw_dw[1].Retrieve(al_claim_no)
SQLCA.nf_handle_error('n_maintain_formulary','nf_retrieve()','idw_dw[1].Retrieve') 

//PAYMENT_PRESCRIPTION
idw_dw[2].Retrieve(al_claim_no)
SQLCA.nf_handle_error('n_maintain_formulary','nf_retrieve()','idw_dw[2].Retrieve') 

//CLAIM_EVENT
idw_dw[3].Retrieve(al_claim_no)
SQLCA.nf_handle_error('n_maintain_formulary','nf_retrieve()','idw_dw[3].Retrieve') 

//CLAIM_ELIGIBILITY
idw_dw[4].Retrieve(al_claim_no)
SQLCA.nf_handle_error('n_maintain_formulary','nf_retrieve()','idw_dw[4].Retrieve') 

//dw_formulary_pres_individual
Select individual_no
Into   :ll_individual_no
From  CLAIM
Where claim_no = :al_claim_no
Using SQLCA;
SQLCA.nf_handle_error('n_maintain_formulary','nf_retrieve()','idw_dw[5].Retrieve') 

idw_dw[5].Retrieve(ll_individual_no)
SQLCA.nf_handle_error('n_maintain_formulary','nf_retrieve()','idw_dw[5].Retrieve') 

//dw_rx_special_auth
idw_dw[6].Retrieve(al_claim_no)
SQLCA.nf_handle_error('n_maintain_formulary','nf_retrieve()','idw_dw[6].Retrieve') 

//dw_rx_special_auth_history
idw_dw[8].Retrieve(al_claim_no)
SQLCA.nf_handle_error('n_maintain_formulary','nf_retrieve()','idw_dw[8].Retrieve') 

//dw_eligibility_history
idw_dw[9].Retrieve(al_claim_no)
SQLCA.nf_handle_error('n_maintain_formulary','nf_retrieve()','idw_dw[9].Retrieve') 

//dw_formulary_history		
idw_dw[10].Retrieve(al_claim_no)
SQLCA.nf_handle_error('n_maintain_formulary','nf_retrieve()','idw_dw[10].Retrieve') 

//dw_drug_alerts
idw_dw[12].Retrieve(al_claim_no)
SQLCA.nf_handle_error('n_maintain_formulary','nf_retrieve()','idw_dw[12].Retrieve') 

RETURN 1

end function

public function long nf_get_claim_no ();w_sheet      lw_active_sheet
LONG         ll_claim_no

lw_active_sheet      = w_frame.GetActiveSheet()
ll_claim_no          = lw_active_sheet.dw_basic_claim.GetItemNumber(1,"claim_no")

IF ISNULL(ll_claim_no) OR ll_claim_no < 1 THEN ll_claim_no = 0

RETURN ll_claim_no
end function

public function integer nf_add_event ();/* 3.1.11.	Log Event (i.e. Add event)

The user will have the ability to log an event and then forward a 
message to one or more users' inbaskets; the user can choose to 'carbon copy' 
the message to another users' inbaskets.

The comment field has a maximum size of  ?? bytes on the DB

Field	          Entered by User	Mandatory	Notes
---------------------------------------------------------------------------------
Event Number	 No	            Yes	System genereated number
Event Type	    Yes	            Yes	Must be a valid event type from the list
Event Specific	 Yes	            Yes	Must be a valid event specific type for the event type (from the list)
Event Date	    No ?	            Yes	The date the event is logged 
Comment	       Yes	            Yes

CLAIM_EVENT
event_no,       event_date,     event_type_code,   event_specific_code,   
event_comment, create_date  

 create a local copy of n_event_log it contains the functionality that we need
it contains two functions
1. nf_create_auto_event()
2. nf_next_claim_event_no()
*/

LONG  ll_claim_no
S_WINDOW_MESSAGE lstr_message

//grab the claim number
ll_claim_no = nf_get_claim_no()

//make sure we have a claim number
IF ISNULL(ll_claim_no) OR ll_claim_no < 1 THEN RETURN -1

lstr_message.al_doubleparm[1] = ll_claim_no
lstr_message.al_doubleparm[2] = nf_get_individual_no()
lstr_message.al_doubleparm[3] = 0

lstr_message.adt_dateparm[1] = nf_get_accident_date()
lstr_message.as_stringparm[1] = 'C'								//claim_role_code
lstr_message.as_stringparm[2] = nf_get_last_name()
lstr_message.as_stringparm[3] = nf_get_given_names()
lstr_message.as_stringparm[4] = 'C'								//event_category_code
lstr_message.as_stringparm[5] = ''								//event_type_code -- this is used to set the event parameters for an automatic event add
lstr_message.as_stringparm[6] = ''								//event_specific_code -- this is used to set the event parameters for an automatic event add
lstr_message.as_stringparm[7] = 'Y'								//allow parameter change -- this is used to set the event parameters for an automatic event add
lstr_message.as_stringparm[8] = 'N'								//add new event
lstr_message.as_stringparm[9] = ''
lstr_message.as_stringparm[10]= ''
lstr_message.as_stringparm[11]= ''


/* have to open this as a response window...problem */
openWithParm(w_event_log_response,lstr_message)

//Re-retrieve the information once the event log window is closed
idw_dw[3].settransobject(sqlca)
idw_dw[3].retrieve(ll_claim_no)
SQLCA.nf_handle_error("n_maintain_formulary","nf_add_event()","idw_dw[3].retrieve()") 
  
RETURN 1
end function

public function integer nf_add_formulary ();/*
Add Formulary	- to add a new secondary formulary

3.1.6.	Add Secondary Formulary

A secondary formulary can only be added if the claim is Active or Final (01-04).

The formulary type is not enabled. The formulary code that is entered must be a valid formulary code 
and can be selected from the list of valid formulary codes. The effective date must be a valid date. 
The termination date must be either a valid date or to 0000-00-00 which represents a null date. 
The comment field must be entered by the user. Refer to the BRs for the secondary formulary.

Field	            Enterable	Mandatory	Notes
----------------------------------------------------------------------------
Formulary Type	   No	         Yes	      Always has a value of 'Secondary'
Formulary Code	   Yes	      Yes	      Must be a valid formulary code
Effective Date	   Yes	      Yes	      Must be a valid date & cannot be null
Termination Date	Yes	      Yes	      Must be a valid date or is null
Comment	         Yes	      Yes	 

3.1.4.1.	Add Primary Coverage Period
A new coverage period for the active, primary formulary code can be added for a registered claim. 
This would be done by the user as a means of creating a 'gap' in the coverage period for the primary formulary.
If no gap in coverage is required, the user would update the termination date or the effective date of the 
existing coverage record.

The user is restricted from adding a new primary formulary code unless the claim's accident nature of 
injury no longer maps to the same formulary code that is assigned to the active, primary formulary. Refer to 
the BR document for the specific conditions under which a new primary formulary code can be added. 

*/
LONG		ll_row, ll_claim_no
DATE     ldt_null_date

/* grab the claim number */
ll_claim_no = nf_get_claim_no()

//make sure it is valid
IF ISNULL(ll_claim_no) OR ll_claim_no < 0 THEN RETURN -1

/* grab the inserted row */
ll_row = idw_dw[1].insertrow(0)

// make sure we have a valid row
IF ISNULL(ll_row) OR ll_row < 0 THEN RETURN -1

//set up the base datawindow stuff
idw_dw[1].Setrow(ll_row)
idw_dw[1].scrolltorow(ll_row)

setnull(ldt_null_date)

/* last_Formulary_record_no set in another function at the end */
idw_dw[1].setitem(ll_row,"formulary_type_code","S")
idw_dw[1].setitem(ll_row,"claim_no",ll_claim_no)
idw_dw[1].setitem(ll_row,"manual_entry_flag","Y")
idw_dw[1].setitem(ll_row,"primary_noi_code"," ")
idw_dw[1].setitem(ll_row,"primary_active_flag","I")
idw_dw[1].setitem(ll_row,"export_no",0)
idw_dw[1].setitem(ll_row,"export_user_id",' ')
idw_dw[1].setitem(ll_row,"export_date",ldt_null_date)  

RETURN 1
end function

public function integer nf_check_bus_rule ();RETURN 0


end function

public function integer nf_check_mandatory ();RETURN 0
end function

public function integer nf_insert (long al_claim_no);RETURN 0

end function

public function integer nf_add_eligibility ();/*
	3.1.3.1.	 Add Claim Eligibility Coverage Period
	A new coverage period for the claim eligibility can be 
	added for a registered claim. This would be done by the user 
	as a means of creating a 'gap' in the coverage period for the claim eligibility. 
	If there is no gap in coverage required, the user would update the 
	termination date or the effective date of the existing coverage record.
	
	For example: 	Claim Eligibility is from January 1, 2004  - Nov 1, 2004. 
	The claim returns to work for a period of  time and then has a 
	recurrence on May 30, 2005 and the claim is reopened for benefits. 
	The user could create a new coverage period beginning on May 30, 2005.
*/

LONG ll_row, ll_claim_no
DATE ldt_null_date

/* grab the claim number */
ll_claim_no = nf_get_claim_no()

// make sure we have a claim number
IF ISNULL(ll_claim_no) OR ll_claim_no < 0 THEN RETURN -1

/* grab the inserted row */
ll_row = idw_dw[4].insertrow(0)

/* make sure we have a valid row */
IF ISNULL(ll_row) OR ll_row < 0 THEN RETURN -1

/* set up the datawindow stuff */
idw_dw[4].Setrow(ll_row)
idw_dw[4].scrolltorow(ll_row)
idw_dw[4].setcolumn("eligibility_start_date")
idw_dw[4].setfocus()

/* The last_eligibility_record_no record is set in the near the end */
setnull(ldt_null_date)
idw_dw[4].setitem(ll_row,"claim_no",ll_claim_no)
idw_dw[4].setitem(ll_row,"comment"," ")//User has to fill this in
idw_dw[4].setitem(ll_row,"manual_entry_flag","Y")
idw_dw[4].setitem(ll_row,"export_no",0)
idw_dw[4].setitem(ll_row,"export_user_id",'')
idw_dw[4].setitem(ll_row,"export_date",ldt_null_date)         

RETURN 1
end function

public function integer nf_create_eligibility_history (long al_claim_no);STRING       ls_sql
INTEGER      li_counter, li_check
dwItemStatus l_status

li_check = 0
/* CHECK AND SEE IF EITHER OF THE DATES HAVE BEEN UPDATED OR THERE ARE NEW RECORDS */
FOR li_counter = 1 TO idw_dw[4].rowcount()
	
	l_status = idw_dw[4].GetItemStatus(li_counter,0,Primary!)
	IF l_status = newmodified! OR l_status = datamodified! THEN
		li_check = 1
		EXIT
	END IF 	
NEXT

IF li_check = 1  THEN

	ls_sql = "p_CREATE_CLAIM_ELIGIBILITY_HISTORY " + string(al_claim_no) 
	
	EXECUTE IMMEDIATE :ls_sql USING SQLCA;
	SQLCA.nf_handle_error('n_maintain_formulary','nf_create_eligibility_history','p_CREATE_CLAIM_ELIGIBILITY_HISTORY')
	
	IF sqlca.sqldbcode < 0  THEN
		SQLCA.SQLERRtext = "FATAL ERROR.  Problem creating Eligibility History."
		SQLCA.nf_handle_error("n_maintain_formulary","nf_create_eligibility_history()","ls_sql = p_CREATE_CLAIM_ELIGIBILITY_HISTORY")
	END IF 
END IF 

RETURN 1
end function

public function integer nf_create_formulary_history (long al_claim_no);STRING       ls_sql
INTEGER      li_counter, li_check
dwItemStatus l_status

li_check = 0

/* CHECK AND SEE IF EITHER OF THE DATES HAVE BEEN UPDATED OR THERE ARE NEW RECORDS */
FOR li_counter = 1 TO idw_dw[1].rowcount()	
	l_status = idw_dw[1].GetItemStatus(li_counter,0,Primary!)
	IF l_status = newmodified! OR l_status = datamodified! THEN
		li_check = 1
		EXIT
	END IF 
NEXT

IF li_check <> 1 THEN RETURN 1

ls_sql = "p_CREATE_FORMULARY_HISTORY " + string(al_claim_no) 

EXECUTE IMMEDIATE :ls_sql USING SQLCA;
SQLCA.nf_handle_error('n_maintain_formulary','nf_create_formulary_history','p_CREATE_CLAIM_FORMULARY_HISTORY')

IF sqlca.sqldbcode < 0  THEN
	SQLCA.SQLERRtext = "FATAL ERROR.  Problem creating Formulary History."
	SQLCA.nf_handle_error("n_maintain_formulary","nf_create_formulary_history()","ls_sql = p_CREATE_CLAIM_ELIGIBILITY_HISTORY")
END IF 

RETURN 1
end function

public function boolean nf_has_record_been_exported (long al_claim_no, long al_record_no, string as_type);/* This function will determine if a record has been exported

	PASS IN: Claim Number & Start Date (KEY Fields), type - (FORMULARY - ELIGIBILITY)
	
	RETURNS: BOOLEAN - TRUE - FALSE
*/
LONG ll_count, ll_count_2

CHOOSE CASE as_type
	CASE "ELIGIBILITY"
		
		SELECT count(*)
		  INTO :ll_count 
	     FROM CLAIM_ELIGIBILITY_HISTORY  
	    WHERE claim_no              = :al_claim_no
		   AND eligibility_record_no = :al_record_no
			AND export_no             > 0
		 USING SQLCA;	 
			
		SQLCA.nf_handle_error('n_maintain_formulary','nf_has_record_been_exported()','SELECT(*) A')
		
		IF ISNULL(ll_count) THEN ll_count = 0
      IF ll_count > 0 THEN RETURN TRUE
			
      SELECT count(*) 
		  INTO :ll_count 
	     FROM CLAIM_ELIGIBILITY  
	    WHERE claim_no              = :al_claim_no
		   AND eligibility_record_no = :al_record_no
			AND export_no             > 0
		 USING SQLCA;	 
		 
	    SQLCA.nf_handle_error('n_maintain_formulary','nf_has_record_been_exported()','SELECT(*) B')
		 
		 IF ISNULL(ll_count) THEN ll_count = 0
       IF ll_count > 0 THEN RETURN TRUE
		
	CASE "FORMULARY"
					
		SELECT count(*) 
		  INTO :ll_count 
	     FROM CLAIM_FORMULARY_HISTORY a  
	    WHERE a.claim_no            = :al_claim_no
		   AND a.formulary_record_no = :al_record_no
			AND a.export_no              > 0
       USING SQLCA;	 
		 
	   SQLCA.nf_handle_error('n_maintain_formulary','nf_has_record_been_exported()','SELECT(*) C') 
		
		IF ISNULL(ll_count) THEN ll_count = 0
      IF ll_count > 0 THEN RETURN TRUE
		
		SELECT count(*) 
		  INTO :ll_count 
	     FROM CLAIM_FORMULARY a  
	    WHERE a.claim_no            = :al_claim_no
		   AND a.formulary_record_no = :al_record_no
			AND a.export_no              > 0
	    USING SQLCA;	 
			  
	    SQLCA.nf_handle_error('n_maintain_formulary','nf_has_record_been_exported()','SELECT(*) D') 
		 
		IF ISNULL(ll_count) THEN ll_count = 0
      IF ll_count > 0 THEN RETURN TRUE
		
	CASE "DELETE_ELIGIBILITY_IND"
			
		SELECT count(*) 
		  INTO :ll_count 
	     FROM CLAIM_ELIGIBILITY 
	    WHERE claim_no              = :al_claim_no
			AND eligibility_record_no = :al_record_no
			AND export_no             > 0
		 USING SQLCA;	 
		 
	    SQLCA.nf_handle_error('n_maintain_formulary','nf_has_record_been_exported()','SELECT(*) F') 
		 
		IF ISNULL(ll_count) THEN ll_count = 0
      IF ll_count > 0 THEN RETURN TRUE
		
		SELECT count(*) 
		  INTO :ll_count 
		  FROM CLAIM_ELIGIBILITY_HISTORY 
		 WHERE claim_no              = :al_claim_no
		   AND eligibility_record_no = :al_record_no
			AND export_no             > 0
	    USING SQLCA;	 
		 
	    SQLCA.nf_handle_error('n_maintain_formulary','nf_has_record_been_exported()','SELECT(*) G')
		 
		IF ISNULL(ll_count) THEN ll_count = 0
      IF ll_count > 0 THEN RETURN TRUE
		 
	CASE "DELETE_FORMULARY_IND"
		
		SELECT count(*) 
		  INTO :ll_count 
	     FROM CLAIM_FORMULARY  
	    WHERE claim_no            = :al_claim_no
			AND formulary_record_no = :al_record_no
			AND export_no           > 0
		 USING SQLCA;	 
		 
	    SQLCA.nf_handle_error('n_maintain_formulary','nf_has_record_been_exported()','SELECT(*) I') 
		 
		IF ISNULL(ll_count) THEN ll_count = 0
      IF ll_count > 0 THEN RETURN TRUE
		
		SELECT count(*)
		  INTO :ll_count 
		  FROM CLAIM_FORMULARY_HISTORY 
		 WHERE claim_no            = :al_claim_no
			AND formulary_record_no = :al_record_no
			AND export_no           > 0
	    USING SQLCA;	 
		 
	    SQLCA.nf_handle_error('n_maintain_formulary','nf_has_record_been_exported()','SELECT(*) J') 
		 
		IF ISNULL(ll_count) THEN ll_count = 0
      IF ll_count > 0 THEN RETURN TRUE
				
END CHOOSE

IF ISNULL(ll_count) THEN ll_count = 0
IF ll_count > 0 THEN RETURN TRUE

RETURN FALSE
		
end function

public function integer nf_get_open_count (long al_claim_no);/*
1.160 The user must be warned that the claim eligibility coverage has expired 
      and formulary coverage is still open under the following conditions:
·	All of the claim eligibility coverage periods for the claim are expired and at 
   least one formulary coverage period has not expired

2.170 The user must be warned that all formulary coverage has expired and 
      claim eligibility coverage is still open under the following conditions:
All of the formulary coverage periods have expired and at least one of the 
claim eligibility coverage periods for the claim has not  expired

RETURN 1 - CASE 1.160
RETURN 2 - CASE 2.170
RETURN 0 - EVERYTHING EVEN - NO ACTION
*/

INTEGER li_count_formulary, li_count_eligibility
DATE    ldt_current

ldt_current = DATE(f_server_datetime())
	
//see if there is a null value
SELECT count(*) INTO :li_count_formulary
  FROM CLAIM_FORMULARY
 WHERE claim_no            = :al_claim_no
	AND (formulary_end_date IS NULL
	 OR formulary_end_date  >= :ldt_current)
 USING SQLCA;
					 
SQLCA.nf_handle_error('n_maintain_formulary','nf_get_open_count()','SELECT A')
				
//default to 0 for NULL
IF ISNULL(li_count_formulary) THEN li_count_formulary = 0
							
SELECT count(*) INTO :li_count_eligibility
  FROM CLAIM_ELIGIBILITY
 WHERE claim_no              = :al_claim_no
	AND (eligibility_end_date IS NULL
	 OR eligibility_end_date  >= :ldt_current)
 USING SQLCA;
						 
SQLCA.nf_handle_error('n_maintain_formulary','nf_get_open_count()','SELECT B')

//default to 0 for NULL
IF ISNULL(li_count_eligibility) THEN li_count_eligibility = 0
					
IF li_count_formulary > 0 AND li_count_eligibility = 0 THEN RETURN 1
IF li_count_eligibility > 0 AND li_count_formulary = 0 THEN RETURN 2

RETURN 0

end function

public function date nf_get_new_date (string as_case);// Addes 1 months to todays date and returns that new date.
DATETIME ldtm_new_date, ldtm_today
DATE     ldt_today


CHOOSE CASE as_case
	CASE "MONTH"
		
	SELECT top 1 DATEADD(MONTH,1,getdate())
	  INTO :ldtm_new_date
	  FROM sysobjects;
			
	SQLCA.nf_handle_error("n_maintain_formulary","nf_get_new_date()","CASE MONTH")

CASE "YEAR"
	
	SELECT top 1 DATEADD(YEAR,1,getdate())
	  INTO :ldtm_new_date
	  FROM sysobjects;
			
	SQLCA.nf_handle_error("n_maintain_formulary","nf_get_new_date()","CASE YEAR")

	
CASE "FIVE_YEARS"
	
	SELECT top 1 DATEADD(YEAR,5,getdate())
	  INTO :ldtm_new_date
	  FROM sysobjects;
			
	SQLCA.nf_handle_error("n_maintain_formulary","nf_get_new_date()","CASE FIVE_YEARS")
	
END CHOOSE

RETURN DATE(ldtm_new_date)

end function

public function integer nf_save ();/* called from the right click popup menu this
   function will save any and all changes made to the datawindows
	
	idw_dw[1] = dw_formulary
	idw_dw[2] = dw_rx    - NOT UPDATED
	idw_dw[3] = dw_event - NOT UPDATED
	idw_dw[4] = dw_eligibility
	idw_dw[12] = dw_drug_alert
	
	NOTE: Need to determine what action was taken 
	i.e ADD CLAIM_ELIGIBILITY
	    UPDATE CLAIM_ELIGIBILITY
		 ECT ECT ECT
*/
INTEGER li_counter, li_error 
LONG    ll_claim_no, ll_cnt
w_sheet      lw_active_sheet

/* grab the claim number and make sure it is valid */
ll_claim_no = nf_get_claim_no()

IF ISNULL(ll_claim_no) OR ll_claim_no < 1  THEN 
	messagebox("Error","Claim number could not be determined. please try again or contact the Helpdesk")
	RETURN -1
END IF 

/* first check and see if anything has been modified on idw_dw[4] - dw_eligibility
    the only column that can be modified is the eligibility_end_date
*/
IF nf_check_modified_status(4) = TRUE THEN
	
	/* create the new eligibility record for 3.1.3.2.Update Claim Eligibility Coverage */
	IF nf_create_auto_eligibility() = -1  THEN RETURN -1
	
	/* validate the formulary information */
	IF nf_validate_eligibility() = -1 THEN RETURN -1
	
	/* run the stored procedure for the eligibility update */
	IF nf_create_eligibility_history(ll_claim_no) < 1 THEN RETURN -1
	
	/* create an automated event when modifications are completed */
	IF nf_add_automated_event(2) = -1 THEN RETURN -1
	
	/*  do up the last number table for newmodified records */
	IF nf_add_last_number("ELIGIBILITY") < 1 THEN RETURN -1
	
	/* update the eligibility datawindow */
   idw_dw[4].update()
	SQLCA.nf_handle_error('n_maintain_formulary','nf_save()','idw_dw[4].update() ') 
	
END IF 

/* first check and see if anything has been modified on idw_dw[1] - dw_formulary*/
IF nf_check_modified_status(1) = TRUE THEN
	
	/* create the new formulary record for 3.1.3.2.Update Claim Formulary Coverage */
	IF nf_create_auto_formulary() = -1  THEN RETURN -1
	
	/* validate the formulary information */
	IF nf_validate_formulary() = -1 THEN RETURN -1
	
	/* run the stored procedure for the formulary update */
	IF nf_create_formulary_history(ll_claim_no) < 1 THEN RETURN -1
	
	/* create an automated event when modifications are completed */
	IF nf_add_automated_event(1) = -1 THEN RETURN -1
	
	/*  do up the last number table for newmodified records */
	IF nf_add_last_number("FORMULARY") < 1 THEN RETURN -1
	
	/* update the formulary datawindow */
	idw_dw[1].update() 
	SQLCA.nf_handle_error('n_maintain_formulary','nf_save()','idw_dw[1].update()') 
	
	
END IF 

/* first check and see if anything has been modified on idw_dw[12] - dw_drug_alert*/
IF nf_check_modified_status(12) = TRUE THEN
	
	IF nf_validate_alert() < 0 THEN RETURN -1

	/*  do up the last number table for newmodified records */
	IF nf_add_last_number("DRUG_ALERT") < 1 THEN RETURN -1

	IF nf_add_individual_alert_event(il_current_row) < 0 THEN
		SQLCA.nf_rollback_transaction()
		RETURN -1
	END IF
	
	idw_dw[12].Update() 
	SQLCA.nf_handle_error('n_maintain_formulary','nf_save()','idw_dw[12].Update()') 
	
	idw_dw[12].Object.DataWindow.HorizontalScrollPosition=1

	lw_active_sheet	= w_frame.GetActiveSheet()
	lw_active_sheet.dw_basic_claim.Retrieve(ll_claim_no)
	
END IF

RETURN 1


end function

public function integer nf_add_automated_event (integer ai_type);STRING       ls_event_type, ls_event_comment, ls_event_code, ls_formulary_code, ls_message
STRING       ls_end_date, ls_formulary_type, ls_previous_end_date, ls_message_final, ls_form_comment
STRING       ls_comment_working

INTEGER      li_counter, li_check, li_error
DATE         ldt_start, ldt_end, ldt_start_org, ldt_end_org
LONG         ll_claim_no, ll_event_no
dwItemStatus l_status
n_event_log  lnv_event_log


/* check the argument passed into the function 
	Two new automated claim event types:
	1) Formulary Coverage Change
	2) Eligibility Coverage Change

	Event_Type
	Column	              Value	                   Value	
	event_type_code	     ‘036’	                   ‘035’	
	event_type_desc	     Formulary Coverage Change Eligibility Coverage Change	
	event_category_code	  ‘C’	                      ‘C’	
	generated_method_code  ‘A’	                      ‘A’	
	event_log_entry_flag	  ‘N’	                      ‘N’	
	progress_entry_flag	  ‘N’	                      ‘N’	
	action_item_entry_flag ‘N’	                      ‘N’	
	symbol_type_code	     blank	                   blank	
	active_flag	           ‘Y’	                      ‘Y’	

	There are no event specific types for the new event types.
	
	*/

IF ai_type <> 1 AND ai_type <> 2  THEN RETURN -1

/* CREATE an instance of n_event_log */
lnv_event_log = create n_event_log

/* grab the claim number */
ll_claim_no = nf_get_claim_no()

//validate the claim number
IF ISNULL(ll_claim_no) OR ll_claim_no < 1 THEN RETURN -1

//default to nothing
ls_event_code = ""

/* check and see if we need to create an event */
CHOOSE CASE ai_type
	CASE 1
		FOR li_counter = 1 TO idw_dw[1].rowcount()
			
			li_check = 0
			 
			l_status = idw_dw[1].GetItemStatus(li_counter,0,Primary!)
			IF l_status = newmodified! THEN li_check = 1
			
			IF li_check = 0 THEN 
				l_status = idw_dw[1].GetItemStatus(li_counter,"formulary_start_date",Primary!)
				IF l_status = datamodified! THEN li_check = 2
			END IF 
			
			IF li_check = 0 THEN
				l_status = idw_dw[1].GetItemStatus(li_counter,"formulary_end_date",Primary!)
				IF l_status = datamodified! THEN li_check = 3
			END IF 
			
			IF li_check = 0 THEN 
				l_status = idw_dw[1].GetItemStatus(li_counter,"formulary_code",Primary!)
				IF l_status = datamodified! THEN li_check = 4
			END IF 
				
			IF li_check = 0 THEN CONTINUE //NO CHANGE NEEDED	
			
			ldt_start     = DATE(idw_dw[1].GetItemdatetime(li_counter,"formulary_start_date"))
			ldt_end       = DATE(idw_dw[1].GetItemdatetime(li_counter,"formulary_end_date"))
			ldt_start_org = DATE(idw_dw[1].GetItemdatetime(li_counter,"formulary_start_date",primary!,true))
			ldt_end_org   = DATE(idw_dw[1].GetItemdatetime(li_counter,"formulary_end_date",primary!,true))
			
			ls_formulary_code = idw_dw[1].GetItemstring(li_counter,"formulary_code")
			ls_formulary_type = idw_dw[1].GetItemstring(li_counter,"formulary_type_code")
			
			ls_form_comment    = idw_dw[1].GetItemstring(li_counter,"formulary_comment")
			ls_comment_working = " - Formulary Code: " + ls_formulary_code + " Comment: " + ls_form_comment
			
			ls_end_date          = string(ldt_end,"yyyy-mm-dd")
			ls_previous_end_date = string(ldt_end_org,"yyyy-mm-dd")
			IF ISNULL(ls_end_date) OR ls_end_date = "" THEN ls_end_date = "Open-Ended"
			IF ISNULL(ls_previous_end_date) OR ls_previous_end_date = "" THEN ls_previous_end_date = "Open-Ended"
			
			//ls_message    = ls_formulary_code + " " + string(ldt_start,"yyyy-mm-dd") + " To " + ls_end_date
			ls_message  = string(ldt_start,"yyyy-mm-dd") + " To " + ls_end_date
					
			/* create the message based on the action identified above */
			CHOOSE CASE li_check
				CASE 1//newmodified
					ls_message_final = 'A new Claim Formulary coverage period has been created: '+ls_message+  " " + ls_comment_working + '.'
				CASE 2//start date modified
					ls_message_final = 'Formulary coverage period has been modified. Revised coverage is '+ls_message+ '.'
				CASE 3//end date modified
					IF ls_formulary_type = "P" THEN
						ls_message_final = 'The Primary Formulary ('+ls_formulary_code+') End Date has changed to '+ls_end_date+' .'
					ELSE
						ls_message_final = 'The Secondary Formulary ('+ls_formulary_code+') End Date has changed to '+ls_end_date+' .'
					END IF 				
				CASE 4//formulary code modified
					ls_message_final = 'The Nature of Injury maps to a different formulary. The new Formulary code is: '+ls_formulary_code+'.'

				CASE ELSE//should never happen
					CONTINUE
			END CHOOSE
			
	      ls_event_type = "036"	
				
			/* grab the next event number */
			ll_event_no = lnv_event_log.nf_next_claim_event_no(ll_claim_no)

			//validate the event number
			IF ISNULL(ll_event_no) OR ll_event_no < 1 THEN RETURN -1

			//create the auto event and return 
			li_error =  lnv_event_log.nf_create_auto_event(ll_claim_no,ll_event_no,ls_event_type,ls_message_final,ls_event_code)
	
			IF li_error = -1 THEN RETURN -1	
				
		NEXT
	CASE 2
		
		FOR li_counter = 1 TO idw_dw[4].rowcount()
		
			li_check = 0
			
			l_status = idw_dw[4].GetItemStatus(li_counter,0,Primary!)
			IF l_status = newmodified! THEN li_check = 1
			
			IF li_check = 0 THEN 
				l_status = idw_dw[4].GetItemStatus(li_counter,"eligibility_start_date",Primary!)
				IF l_status = datamodified! THEN li_check = 2
			END IF
			
			IF li_check = 0 THEN
				l_status = idw_dw[4].GetItemStatus(li_counter,"eligibility_end_date",Primary!)
				IF l_status = datamodified! THEN li_check = 3
			END IF 
				
			IF li_check = 0 THEN CONTINUE //NO CHANGE NEEDED	
			
			ldt_start     = DATE(idw_dw[4].GetItemdatetime(li_counter,"eligibility_start_date"))
			ldt_end       = DATE(idw_dw[4].GetItemdatetime(li_counter,"eligibility_end_date"))
			ldt_start_org = DATE(idw_dw[4].GetItemdatetime(li_counter,"eligibility_start_date",primary!,true))
			ldt_end_org   = DATE(idw_dw[4].GetItemdatetime(li_counter,"eligibility_end_date",primary!,true))
			
			ls_end_date = string(ldt_end,"yyyy-mm-dd")
			IF ISNULL(ls_end_date) OR ls_end_date = "" THEN ls_end_date = "Open-Ended"
			ls_message    = string(ldt_start,"yyyy-mm-dd") + " To " + ls_end_date

			/* create the message based on the action identified above */
			CHOOSE CASE li_check
				CASE 1//new modified
					ls_message_final = 'A new Claim Eligibility coverage period has been created from '+ls_message+' .'
				CASE 2//start date modified
					ls_message_final = 'Eligibility coverage has been modified. Revised coverage is '+ls_message+ '.'
				CASE 3//end date modified
					ls_message_final = 'The Eligibility coverage End Date has changed To '+ls_end_date+' .'
				CASE ELSE//should never happen
					CONTINUE
			END CHOOSE
			
			ls_event_type = "035"
				
			/* grab the next event number */
			ll_event_no = lnv_event_log.nf_next_claim_event_no(ll_claim_no)

			//validate the event number
			IF ISNULL(ll_event_no) OR ll_event_no < 1 THEN RETURN -1

			//create the auto event and return 
			li_error = lnv_event_log.nf_create_auto_event(ll_claim_no,ll_event_no,ls_event_type,ls_message_final,ls_event_code)
			IF li_error = -1 THEN RETURN -1	
				
		NEXT
END CHOOSE 

RETURN 1



end function

public function integer nf_switch_formulary (long al_claim_no);/*
3.1.8.	Switch Primary

** 	is enabled if the formulary code of the active primary formulary 
differs from the formulary code that is derived from the claim accident 
nature of injury code

The primary formulary can only be switched if the claim is Active or Final (01-04) 
and if the existing primary formulary code differs from the formulary code that is 
derived based on the current accident nature of injury.

When the user wants to switch the primary formulary to the new formulary code, 
the current primary formulary must be made inactive and terminated on the date
that formulary is switched. 

a) Old primary - made inactive
Field	            Update	Notes
Formulary Type	   No	      No change
Formulary Code	   No	      No change
Effective Date	   No	      No change
Termination Date	No	      Always set to the date the primary is switched
Comment	         No	      No change

b) New primary

Field	            Entered by User	Mandatory	Notes
Formulary Type	   No	               Yes	      Always has a value of 'Primary'
Formulary Code	   No	               Yes	      Formulary code based on the current accident NOI
Effective Date	   Yes	            Yes	      Must be a valid date & cannot be null
Termination Date	Yes	            Yes	      Must be a valid date or is null
Comment	         Yes	            Yes	
 */
 
LONG		ll_row, ll_count, ll_claim_no
STRING   ls_formulary_type, ls_noi_code, ls_formulary_code, ls_primary_active_flag, ls_formulary_check
DATE     ldt_null_date, ldt_current, ldt_end_date
INTEGER  li_counter, li_rowcount
DATETIME ldtm_next_day

/* grab the current date */
ldt_current = DATE(f_server_datetime())
setnull(ldt_null_date)

ll_claim_no = al_claim_no

//grab the rowccount
li_rowcount = idw_dw[1].rowcount()
IF ISNULL(li_rowcount) OR li_rowcount < 1 THEN RETURN -1

//check the passed in claim number 
IF ISNULL(al_claim_no) OR al_claim_no < 1 THEN RETURN -1

// double check that we are allowed to do this for the switch formulary button
SELECT COUNT(*) INTO :ll_count
  FROM CLAIM_FORMULARY a
 WHERE a.primary_active_flag = 'Y' 
   AND a.formulary_type_code = 'P' 
	AND a.claim_no            = :al_claim_no
   AND NOT EXISTS (SELECT * 
                     FROM ACCIDENT b, X001_Noi_Formulary_Xref c 
	                 WHERE a.claim_no              = b.claim_no 
                      AND b.nature_of_injury_code = c.noi_code 
                      AND c.formulary_code        = a.formulary_code)
 USING SQLCA;
 
 SQLCA.nf_handle_error('n_maintain_formulary','nf_switch_formulary()','SELECT COUNT(*) INTO :ll_count - B') 

IF ll_count < 1 THEN 
	messagebox("Formulary Switch","Could not switch nature of Injury due to: No change in accident NOI Code")
	RETURN -1
END IF 

/* grab the next bluecross date - it will be used for the end date */

ldtm_next_day = DATETIME(nf_get_abcc_work_day())

/* grab the Primary_noi_code */
SELECT nature_of_injury_code 
  INTO :ls_noi_code
  FROM ACCIDENT   
 WHERE claim_no = :al_claim_no
 USING SQLCA;
 
 SQLCA.nf_handle_error('n_maintain_formulary','nf_switch_formulary()','SELECT nature_of_injury_code') 

IF ISNULL(ls_noi_code) OR TRIM(ls_noi_code) = "" THEN
	messagebox("Formulary Switch","Could not determine the Nature of Injury Code")
	RETURN -1
END IF 

/* Formulary Code	- No Yes Formulary code based on the current accident NOI */
SELECT formulary_code 
  INTO :ls_formulary_code 
  FROM X001_Noi_Formulary_Xref
 WHERE noi_code = :ls_noi_code
 USING SQLCA;

 SQLCA.nf_handle_error('n_maintain_formulary','nf_switch_formulary()','SELECT formulary_code ') 

IF ISNULL(ls_formulary_code) OR TRIM(ls_formulary_code) = "" THEN
	messagebox("Formulary Switch","Could not determine the Formulary Code")
	RETURN -1
END IF 

// check that new formulary is active before switching
select count(*)
into   :ll_count
from   Formulary
where  formulary_code = :ls_formulary_code
and    active_flag    = 'Y'
using SQLCA;

SQLCA.nf_handle_error('n_maintain_formulary','nf_switch_formulary()','SELECT count(*) from Formulary ... active_flag ') 

IF ll_count = 0 THEN
	messagebox("Formulary Switch","The Formulary Code that is associated with the claim nature of injury is inactive.")
	RETURN -1
END IF

//grab the formulary_type_code 
FOR li_counter = 1 TO idw_dw[1].rowcount()
	
		/* find the current Primary record.... we will be changing the Termination Date */
		ls_primary_active_flag = idw_dw[1].getitemstring(li_counter,"primary_active_flag")
		ls_formulary_type      = idw_dw[1].getitemstring(li_counter,"formulary_type_code")
      ldt_end_date           = DATE(idw_dw[1].getitemdatetime(li_counter,"formulary_end_date"))
		ls_formulary_check     = idw_dw[1].getitemstring(li_counter,"formulary_code")
		
		IF ls_primary_active_flag = "Y" AND ls_formulary_type = "P" THEN 
			
			IF ls_formulary_check = ls_formulary_code THEN CONTINUE
			
			/* set this records termination date to be = next bluecross date */
			IF ldt_end_date >= ldt_current OR ISNULL(ldt_end_date) THEN
				idw_dw[1].setitem(li_counter,"formulary_end_date",date(ldtm_next_day))
			END IF 
			
			idw_dw[1].setitem(li_counter,"primary_active_flag","N")
			
			/* may have to protect these columns */
		END IF 
NEXT
			

/* now we need to set up the new record */
ll_row = idw_dw[1].insertrow(0)
IF ISNULL(ll_row) OR ll_row < 1 THEN RETURN -1

/* last_Formulary_record_no populated later on - set the default */
idw_dw[1].setitem(ll_row,"formulary_type_code","P")
idw_dw[1].setitem(ll_row,"claim_no",al_claim_no)
idw_dw[1].setitem(ll_row,"manual_entry_flag","Y")
idw_dw[1].setitem(ll_row,"primary_noi_code",ls_noi_code)
idw_dw[1].setitem(ll_row,"primary_active_flag","Y")
idw_dw[1].setitem(ll_row,"export_no",0)
idw_dw[1].setitem(ll_row,"export_user_id",'')
idw_dw[1].setitem(ll_row,"export_date",ldt_null_date) 
idw_dw[1].setitem(ll_row,"formulary_code",ls_formulary_code)

idw_dw[1].accepttext()

RETURN 1
end function

public function date nf_get_abcc_work_day ();Datetime 	ldtm_next_day, ldtm_two_day, ldtm_date
Date			ldt_today, ldt_date
LONG        ll_count

ldt_today = DATE(f_server_datetime())

//PR8364 - Check to see if the Eligiblity Export has already run for today
SELECT COUNT(*)
INTO        :ll_count
FROM      X001_EXPORT_HISTORY
WHERE   export_date >= :ldt_today 
AND          export_date <  DateAdd(day, 1, :ldt_today)
USING     SQLCA;

SQLCA.nf_handle_error('n_maintain_formulary','nf_get_abcc_work_day()','SELECT COUNT(*) INTO :ll_count')

IF ll_count = 0 THEN
	
	SELECT DATEADD(day,1,min(calendar_date))
	  INTO :ldtm_next_day
	  FROM Company_Calendar 
	 WHERE parent_company_code = 'ABCC'
		AND calendar_date >= :ldt_today
		AND working_days = 1.00 
	 USING SQLCA;
		
	SQLCA.nf_handle_error('n_maintain_formulary','nf_get_abcc_work_day()','SELECT... FROM Company_Calendar')
	
	IF SQLCA.SQLNRows = 0  OR ISNULL(ldtm_next_day) Then
		MessageBox('ABCC Calendar','The ABCC Calendar is not loaded. Call the helpdesk.')
	END IF	
	
	ldt_date = DATE(ldtm_next_day)

ELSE

	SELECT DATEADD(DAY,1,MIN(calendar_date))
	INTO       :ldtm_two_day
	FROM     Company_Calendar 
	WHERE parent_company_code = 'ABCC'
	AND        calendar_date > :ldt_today
	AND        working_days = 1.00 
	USING    SQLCA;
		
	SQLCA.nf_handle_error('n_maintain_formulary','nf_get_abcc_work_day()','SELECT... FROM Company_Calendar')
	
	IF SQLCA.SQLNRows = 0  OR ISNULL(ldtm_two_day) Then
		MessageBox('ABCC Calendar','The ABCC Calendar is not loaded. Call the helpdesk.')
	END IF	
	
	ldt_date = DATE(ldtm_two_day)
	
END IF

RETURN ldt_date

end function

public function date nf_get_termination_date (long al_claim_no, string as_type, long al_record_no);/* This function will grab the exported termination date.
   This Termination date will be used for comparison purposes.
	i.e. IF we change a exported termination date and than compare against the 
	changed date we are not really comparing against the real exported termination date.

	PASS IN: Claim Number & Start Date (KEY Fields), type - (FORMULARY - ELIGIBILITY)
	
	RETURNS: exported termination date
	
	MODIFIED: We will be going against record number now instead of the start date
*/
DATETIME ldtm_termination
DATE     ldt_return

CHOOSE CASE as_type
	CASE "ELIGIBILITY"
		
		SELECT a.eligibility_end_date 
		  INTO :ldtm_termination 
	     FROM CLAIM_ELIGIBILITY_HISTORY a  
	    WHERE a.claim_no              = :al_claim_no
		   AND a.eligibility_record_no = :al_record_no
			AND a.export_no             = (SELECT max(export_no)  
			                                 FROM CLAIM_ELIGIBILITY_HISTORY b 
									              WHERE a.eligibility_record_no = b.eligibility_record_no
									                AND a.claim_no              = b.claim_no
													    AND b.export_no             > 0)
	    USING SQLCA;	 
		 
	    SQLCA.nf_handle_error('n_maintain_formulary','nf_get_termination_date()','SELECT eligibility_end_date') 
		
	CASE "FORMULARY"
		
		SELECT a.formulary_end_date 
		  INTO :ldtm_termination 
	     FROM CLAIM_FORMULARY_HISTORY a  
	    WHERE a.claim_no            = :al_claim_no
		   AND a.formulary_record_no = :al_record_no
			AND a.export_no           = (SELECT max(export_no)  
			                                FROM CLAIM_FORMULARY_HISTORY b 
									             WHERE a.formulary_record_no = b.formulary_record_no
									               AND a.claim_no            = b.claim_no
														AND b.export_no           > 0)
	    USING SQLCA;	 
		 
	    SQLCA.nf_handle_error('n_maintain_formulary','nf_get_termination_date()','SELECT formulary_end_date') 
		
END CHOOSE

ldt_return = date(ldtm_termination)

IF ldt_return = DATE("1900-01-01") THEN setnull(ldt_return)
	 				
RETURN ldt_return
		
end function

public function date nf_get_effective_date (long al_claim_no, long al_record_no);/* This function will grab the exported effective date.
	PASS IN: Claim Number, formulary_code
	RETURNS: exported effective date
*/
DATETIME ldtm_effective
DATE     ldt_return
	
SELECT a.formulary_start_date 
  INTO :ldtm_effective 
  FROM CLAIM_FORMULARY_HISTORY a  
 WHERE a.claim_no            = :al_claim_no
	AND a.formulary_record_no = :al_record_no
	AND a.export_no           = (SELECT max(export_no)  
	                               FROM CLAIM_FORMULARY_HISTORY b 
							            WHERE a.formulary_record_no = b.formulary_record_no
							              AND a.claim_no            = b.claim_no)
 USING SQLCA;	 
		 
SQLCA.nf_handle_error('n_maintain_formulary','nf_get_effective_date()','SELECT formulary_start_date') 

ldt_return = date(ldtm_effective)

IF ldt_return = DATE("1900-01-01") THEN setnull(ldt_return)
	 				
RETURN ldt_return
end function

public function integer nf_delete_formulary (long al_claim_no);/*
2.30	A claim formulary coverage period must not be deleted if the claim formulary 
      coverage period has been exported to Bluecross 
		...The user may delete a claim formulary coverage period that was created as long as they 
		delete it before it gets exported to Bluecross
		
		-- Delete Formulary	- to delete a secondary formulary that has not been exported
		
DEVELOPERS NOTE: This will be handled from within the calling code. 
                 I believe there is a right click menu option that will call the delete code
					  most likely a delete function.
					  
3.1.7.	Delete Secondary Formulary

A secondary formulary may be deleted if the formulary has not been exported to ABCC. 

*/
LONG    ll_export_no, ll_row, ll_formulary_record_no, ll_count
LONG    ll_eligibility_record_no, ll_event_no
STRING  ls_formulary_type_code, ls_formulary_code
STRING  ls_message, ls_event_type, ls_event_code, ls_end_date, ls_entry_flag
DATE    ldt_start, ldt_end, ldt_current
INTEGER li_error, li_counter
n_event_log lnv_event_log

/* make sure it is a secondary formulary - Primary cannot be deleted */
ll_row = idw_dw[1].getrow()

IF isnull(ll_row) OR ll_row = 0 THEN 
	messagebox("Delete?","Please select a record to delete before deleting")
	RETURN -1
END IF 

ls_formulary_type_code = idw_dw[1].GetItemstring(ll_row,'formulary_type_code')
ll_export_no           = idw_dw[1].GetItemNumber(ll_row,'export_no')
ldt_start              = DATE(idw_dw[1].GetItemdatetime(ll_row,'formulary_start_date',PRIMARY!,TRUE))
ls_formulary_code      = idw_dw[1].GetItemstring(ll_row,'formulary_code')
ll_formulary_record_no = idw_dw[1].GetItemnumber(ll_row,'formulary_record_no')

ldt_current = date(f_server_datetime())

/* formulary type check */
IF trim(ls_formulary_code) = "" THEN
	messagebox("Validation Error","No Formulary Code.")
	RETURN -1
END IF

//double check that nothing has been updated
IF nf_check_modified_status(0) = TRUE THEN
	MESSAGEBOX("Delete FORMULARY","Please Save or Cancel current work before proceeding with Delete.")
	RETURN -1
END IF 

/* export_no check */
IF nf_has_record_been_exported(al_claim_no,ll_formulary_record_no,"DELETE_FORMULARY_IND") = TRUE THEN
	messagebox("Validation Error BR# 2.30","A Claim FORMULARY coverage period must not be deleted " +& 
	            "~r if the coverage period has been exported to Bluecross ")
	RETURN -1
END IF 

//validate the claim number
IF ISNULL(al_claim_no) OR al_claim_no < 1 THEN 
	messagebox("Validation Error","Missing Claim Number.")
	RETURN -1
END IF

ldt_end   = DATE(idw_dw[1].GetItemdatetime(ll_row,"formulary_end_date"))

/* CLAIM_FORMULARY.formulary_code '  ' CLAIM_FORMULARY.formulary_start_date to CLAIM_FORMULARY.formulary_end_date*/
ls_end_date   = string(ldt_end,"yyyy-mm-dd")
IF ISNULL(ls_end_date) OR ls_end_date = "" THEN ls_end_date = "Open-Ended"
ls_message    = "Delete: " + ls_formulary_code + " " + string(ldt_start,"yyyy-mm-dd") + " To " + ls_end_date
ls_event_type = "036"	

/* give the user an out. */
IF messagebox("Delete Formulary", ls_message +"?", question!,yesno!) = 2 THEN RETURN 1

//create an instance of the object
lnv_event_log = create n_event_log
		 
/* grab the next event number */
ll_event_no = lnv_event_log.nf_next_claim_event_no(al_claim_no)

//validate the event number
IF ISNULL(ll_event_no) OR ll_event_no < 1 THEN 
	messagebox("Validation Error","Missing Event Number.")
	RETURN -1
END IF

//create the auto event and return 
li_error =  lnv_event_log.nf_create_auto_event(al_claim_no,ll_event_no,ls_event_type,ls_message,ls_event_code)
IF li_error = -1 THEN 
	messagebox("Validation Error","Failed to create Auto Event in Formulary Delete")
	RETURN -1
END IF

/* passes everything - delete the row from the datawindow */
IF idw_dw[1].deleterow(ll_row) = -1 THEN 
	messagebox("Validation Error","Failed to Delete Row in Formulary Delete")
	RETURN -1
END IF

/* MAKE THIS SIMPLE DELETE & SAVE */
IF idw_dw[1].UPDATE() = -1 THEN 
	messagebox("Validation Error","Datawindow Update Failed in Formulary Delete")
	RETURN -1
END IF


RETURN 1

end function

public function integer nf_validate_secondary_formulary (integer ai_row);LONG     ll_claim_no, ll_formulary_record_no, ll_export_no
DATE     ldt_current, ldt_original_start
STRING   ls_formulary_type_code, ls_formulary_code, ls_formulary_code_org
BOOLEAN  lb_check
INTEGER  li_count

idw_dw[1].accepttext()

// CHECK IT
IF ISNULL(ai_row) OR ai_row < 1 THEN RETURN -1

/* each record has to be checked individually basic checks done in calling function */
ll_claim_no            = idw_dw[1].GetItemNumber(ai_row,'claim_no')
ls_formulary_type_code = idw_dw[1].GetItemstring(ai_row,'formulary_type_code')
ls_formulary_code      = idw_dw[1].GetItemstring(ai_row,'formulary_code')
ls_formulary_code_org  = idw_dw[1].GetItemstring(ai_row,'formulary_code',PRIMARY!,TRUE)
ldt_original_start     = DATE(idw_dw[1].GetItemdatetime(ai_row,'formulary_start_date',PRIMARY!,TRUE))
ll_formulary_record_no = idw_dw[1].GetItemnumber(ai_row,'formulary_record_no')
ll_export_no           = idw_dw[1].GetItemnumber(ai_row,'export_no')

/* check and see if this record has been exported */
/* check and see if this record has been exported  -- should check that NULL = NULL */
IF ll_export_no > 0 THEN
   lb_check = TRUE
ELSE
   lb_check = nf_has_record_been_exported(ll_claim_no,ll_formulary_record_no,"FORMULARY")
END IF 
	
ldt_current = DATE(f_server_datetime())
	
// check that we have the right formulary type
IF ls_formulary_type_code <> "S" THEN
	messagebox("Validation Error", "Wrong Formulary Type for validation.")
	RETURN -1
END IF
	
/* 4.10 The formulary code assigned to a secondary formulary must be a WHSCC formulary code 
*/
SELECT count(*) 
  INTO :li_count
  FROM Formulary
 WHERE formulary_code = :ls_formulary_code
 USING SQLCA;
	 
 SQLCA.nf_handle_error('n_maintain_formulary','nf_validate_secondary_formulary','SELECT - BR4.10') 

IF ISNULL(li_count) OR li_count = 0 THEN
	messagebox("Validation Error BR# 4.10", "The formulary code assigned to a secondary formulary must be a WorkSafeNB formulary code.")
	RETURN -1
END IF

/* 4.20 The formulary code must not be revised for a secondary formulary that has been exported 
*/
IF lb_check = TRUE THEN
	IF ls_formulary_code <> ls_formulary_code_org THEN
		messagebox("Validation Error BR# 4.20", "The secondary formulary code has been exported and can not be revised.")
		RETURN -1
	END IF
END IF 

/* 4.30	The formulary code assigned to a secondary formulary coverage that is not exported 
			must not be a formulary code that is assigned to the active, primary formulary; 
			however, the active primary formulary may be assigned a formulary code that is 
			assigned to an existing secondary formulary coverage  
	... This situation may arise if the primary formulary is switched to another formulary code 
	    and there is already a secondary formulary that has the same formulary code. However, 
		 if the user wants to create a new Secondary formulary, they should be prevented from assigning 
		 the formulary code of the active primary formulary. In all cases, of course, the BRs must ensure 
		 there is no overlapping formulary coverages created, regardless of whether it is a primary 
		 or secondary type.
*/
IF lb_check = FALSE THEN 
	
	SELECT count(*) 
	  INTO :li_count
	  FROM CLAIM_FORMULARY
	 WHERE formulary_type_code = "P"
		AND claim_no            = :ll_claim_no
		AND formulary_code      = :ls_formulary_code 
		AND primary_active_flag = "Y"
	 USING SQLCA;
		 
	SQLCA.nf_handle_error('n_maintain_formulary','nf_validate_secondary_formulary','SELECT - BR4.30') 
	
	IF ISNULL(li_count) THEN li_count = 0 
		
	IF li_count > 0 THEN
		messagebox("Validation Error BR# 4.30", "The formulary code is already assigned to the active, primary formulary.")
		RETURN -1	
	END IF 
END IF 
	
RETURN 1
end function

public function integer nf_validate_primary_formulary (integer ai_row);LONG     ll_claim_no, ll_formulary_no, ll_export_no, ll_count
DATE     ldt_current, ldt_exported_termination_date, ldt_end_check, ldt_switch_check
DATE     ldt_formulary_start, ldt_formulary_end, ldt_original_start, ldt_original_end
DATE     ldt_check_start, ldt_check_end
STRING   ls_formulary_type_code, ls_formulary_code, ls_formulary_code_org, ls_active_flag, ls_flag_check
STRING   ls_formulary_type_check, ls_formulary_check
BOOLEAN  lb_check
INTEGER  li_counter_2

ll_claim_no            = idw_dw[1].GetItemNumber(ai_row,'claim_no')
ldt_formulary_start    = DATE(idw_dw[1].GetItemdatetime(ai_row,'formulary_start_date'))
ldt_formulary_end      = DATE(idw_dw[1].GetItemdatetime(ai_row,'formulary_end_date'))
ll_formulary_no        = idw_dw[1].GetItemNumber(ai_row,'formulary_record_no')
ll_export_no           = idw_dw[1].GetItemNumber(ai_row,'export_no')
ldt_original_start     = DATE(idw_dw[1].GetItemdatetime(ai_row,'formulary_start_date',PRIMARY!,TRUE))
ldt_original_end       = DATE(idw_dw[1].GetItemdatetime(ai_row,'formulary_end_date',PRIMARY!,TRUE))
ls_formulary_type_code = idw_dw[1].GetItemstring(ai_row,'formulary_type_code')
ls_formulary_code      = idw_dw[1].GetItemstring(ai_row,'formulary_code')
ls_formulary_code_org  = idw_dw[1].GetItemstring(ai_row,'formulary_code',PRIMARY!,TRUE)
ls_active_flag         = idw_dw[1].GetItemstring(ai_row,'primary_active_flag')

ldt_current = DATE(f_server_datetime())
idw_dw[1].accepttext()

// check that we have the right formulary type
IF ls_formulary_type_code = "S" THEN
	messagebox("Validation Error", "Wrong Formulary Type for validation")
	RETURN -1
END IF

/* make sure we have values to pass through */
IF ISNULL(ll_claim_no) OR ll_claim_no < 1  THEN 
	messagebox("Validation Error", "Invalid Claim Number")
	RETURN -1
END IF
IF ISNULL(ldt_original_start) THEN 
	messagebox("Validation Error", "Invalid Start Date")
	RETURN -1
END IF
IF ISNULL(ls_formulary_code) OR TRIM(ls_formulary_code) = "" THEN 
	messagebox("Validation Error", "Invalid Formulary Code")
	RETURN -1
END IF

IF ISNULL(ls_active_flag) OR TRIM(ls_active_flag) = "" THEN 
	messagebox("Validation Error", "Invalid Active Flag")
	RETURN -1
END IF

/* check and see if this record has been exported  -- should check that NULL = NULL */
IF ll_export_no > 0 THEN
	lb_check = TRUE
ELSE
	lb_check = nf_has_record_been_exported(ll_claim_no,ll_formulary_no,"FORMULARY")
END IF 
	
//grab the exported termination date - It could be NULL...which is valid
IF ll_export_no > 0 THEN
	ldt_exported_termination_date = ldt_original_end
ELSE	
	ldt_exported_termination_date = nf_get_termination_date(ll_claim_no,"FORMULARY",ll_formulary_no)
END IF 

/* set up the end date if null */
IF ISNULL(ldt_formulary_end) THEN 
	ldt_end_check = DATE("2078-01-01")
ELSE
	ldt_end_check = ldt_formulary_end
END IF 	
		
/* 3.10	A claim must have only one active, primary formulary coverage */
// grab the status of the row - if it's new than it must be a switch which is fine 

IF ls_active_flag = "Y" AND idw_dw[1].GetItemStatus(ai_row,0, Primary!) = datamodified!  THEN

	SELECT count(*) INTO :ll_count 
	  FROM CLAIM_FORMULARY  
	 WHERE claim_no            = :ll_claim_no
		AND formulary_record_no <> :ll_formulary_no
		AND formulary_type_code = "P"
		AND primary_active_flag = "Y"
		AND formulary_code      <> :ls_formulary_code
	 USING SQLCA;
	 
	SQLCA.nf_handle_error('n_maintain_formulary','nf_validate_primary_formulary()','SELECT...- BR3.10') 
	
	IF ll_count > 0 THEN
		messagebox("Validation Error BR# 3.10","A claim must have only one active primary formulary coverage.")
		RETURN -1
	END IF 
	
	FOR li_counter_2 = 1 TO idw_dw[1].rowcount()
		IF li_counter_2 = ai_row THEN CONTINUE
		
		ls_formulary_type_check = idw_dw[1].GetItemstring(li_counter_2,'formulary_type_code')
		ls_flag_check           = idw_dw[1].GetItemstring(li_counter_2,'primary_active_flag')
		ls_formulary_check      = idw_dw[1].GetItemstring(li_counter_2,'formulary_code')
		
		IF ls_formulary_type_check = "P" AND ls_flag_check = "Y" AND ls_formulary_check <> ls_formulary_code THEN 
			messagebox("Validation Error BR# 3.10","A claim must have only one active primary formulary coverage.")
			RETURN -1
		END IF 
	NEXT
	
END IF 

/*
3.20	Primary formulary coverage must not overlap with any other primary coverage period, 
      regardless of the formulary code and regardless of whether the primary formulary is active
	... For example: If a claim has a primary formulary of F2 for Feb 1, 2005 to June 30, 2005 and 
	    then the primary formulary is switched to formulary F6, the user cannot create a new primary 
		 formulary coverage for F6 that falls within Feb 1 - June 30 as this would give the claimant 
		 two primary formularies at the same time
*/

//IF ib_switched = FALSE THEN
 	
   SELECT count(*) INTO :ll_count 
	  FROM CLAIM_FORMULARY  
	 WHERE claim_no               =  :ll_claim_no
		AND formulary_record_no    <> :ll_formulary_no
		AND formulary_type_code    = "P"
		AND ((formulary_start_date BETWEEN :ldt_formulary_start AND :ldt_end_check)
		 OR (formulary_end_date    BETWEEN :ldt_formulary_start AND :ldt_end_check)
		 OR (formulary_start_date > :ldt_formulary_start AND formulary_end_date < :ldt_end_check)
		 OR (:ldt_formulary_start BETWEEN formulary_start_date AND formulary_end_date))
    USING SQLCA;	
	 
    SQLCA.nf_handle_error('n_maintain_formulary','nf_validate_primary_formulary()','SELECT count(*)... - BR3.20A') 
	
//ELSE

	FOR li_counter_2 = 1 TO idw_dw[1].rowcount()
		IF li_counter_2 = ai_row THEN CONTINUE
		
		ldt_check_start = DATE(idw_dw[1].GetItemdatetime(li_counter_2,'formulary_start_date'))
      ldt_check_end   = DATE(idw_dw[1].GetItemdatetime(li_counter_2,'formulary_end_date'))
		ls_formulary_type_check = idw_dw[1].GetItemstring(li_counter_2,'formulary_type_code')
		
		IF ISNULL(ldt_check_end) THEN ldt_check_end = DATE("2078-01-01")
		
		IF ls_formulary_type_check = "P" THEN 
			IF ldt_formulary_start >= ldt_check_start AND ldt_formulary_start <= ldt_check_end THEN 
				ll_count = 1
			END IF 
			IF ldt_formulary_start <= ldt_check_start AND ldt_formulary_start >= ldt_check_end THEN 
				ll_count = 1
			END IF 
		END IF 
	NEXT
	
//END IF 

IF ll_count > 0 THEN
	messagebox("Validation Error BR# 3.20","The primary formulary coverage must not overlap with any other primary coverage period.")
	RETURN -1
END IF 

/* 3.30	The formulary code assigned to an active, primary formulary must be the derived from 
         the accident nature of injury code for the claim
	...For a primary formulary coverage, the formulary code is derived from a look-up table that cross-references 
	the NOI code to a valid WHSCC formulary code
*/
IF ls_active_flag = "Y" THEN 
	
	SELECT COUNT(*) INTO :ll_count
     FROM CLAIM_FORMULARY a
    WHERE a.claim_no       = :ll_claim_no
	   AND EXISTS (SELECT * 
                    FROM ACCIDENT b, X001_Noi_Formulary_Xref c 
	                WHERE a.claim_no              = b.claim_no 
                     AND b.nature_of_injury_code = c.noi_code 
							AND c.formulary_code        = :ls_formulary_code)
    USING SQLCA;
	
	SQLCA.nf_handle_error('n_maintain_formulary','nf_validate_primary_formulary()','SELECT count(*) - BR3.30') 
	
	IF isnull(ll_count) THEN ll_count = 0

	IF ll_count = 0 THEN
		messagebox("Validation Error BR# 3.30","The formulary code assigned to an active primary formulary must be " +&
					  "~rderived from the accident nature of injury code for the claim.")
		RETURN -1
	END IF 
END IF 

/* 3.40	The primary formulary may be "switched" to another formulary code if the primary formulary 
         code that is currently assigned to the active, primary formulary differs from the derived 
			formulary code based on the accident nature of injury code for the claim
			
			DONE IN SWITCH BUTTON
*/

RETURN 1



end function

public function integer nf_create_auto_formulary ();/*
3.1.4.2.	 Update Primary Formulary Coverage

The primary formulary effective date can only be updated to 
a date that is earlier than the existing effective date. 
The primary formulary termination date can be updated to a 
valid date or to 0000-00-00 which represents a null date. 

If the effective date is modified, the module must create a 
new coverage period record for the primary formulary.  
Refer to section 3.1.3. on Claim Eligibility Coverage for details.

Field	           Updatable	Mandatory Notes
Formulary Type	  No	      Yes	    Always has a value of 'Primary'
Formulary Code	  No	      Yes	    Formulary code is based on the accident NOI
Effective Date	  Yes	      Yes	    Must only be updated to an earlier date
Termination Date Yes	      Yes	    Must be a valid date or is null
Comment	        Yes	      Yes	
*/

//NOTE: If this returns a -1  than make sure the canel button is triggered. 
//NOTE: This code will be called at the end from the save button.

LONG	  ll_row, ll_claim_no, ll_record_no
STRING  ls_formulary_type_code, ls_formulary_code, ls_active_flag, ls_noi_code
STRING  ls_comment
DATE    ldt_null_date, ldt_effective_date, ldt_effective_org, ldt_termination_new
INTEGER li_counter, li_rowcount

/* grab the claim number */
ll_claim_no = nf_get_claim_no()
IF ISNULL(ll_claim_no) OR ll_claim_no < 0 THEN RETURN -1

/* can only be added if the claim is active or final. This is just a second safety check
	is also checked from the calling code
*/
IF nf_check_status() <> 1 THEN 
	messagebox("Invalid Status", "Invalid Claim status for the creation of Auto Formulary" )
	RETURN -1 
END IF 

// grab the rowcount for the datawindow 
li_rowcount = idw_dw[1].rowcount()

// make sure it's valid
IF ISNULL(li_rowcount) OR li_rowcount < 0 THEN RETURN -1

/* check the datawindow for a modified record where the effective date has been changed */
FOR  li_counter = 1 TO li_rowcount
	IF idw_dw[1].getitemstatus(li_counter,0,PRIMARY!) = DATAMODIFIED! THEN
		IF idw_dw[1].getitemstatus(li_counter,"formulary_start_date",PRIMARY!) = DATAMODIFIED! THEN
			
			//GRAB THE ORIGINAL VALUE TO BE RESET BACK INTO THIS ROW AND THAN CREATE THE NEW ROW WITH THE MODIFIED DATE.
			ldt_effective_org = DATE(idw_dw[1].GetItemDatetime(li_counter,"formulary_start_date", Primary!, TRUE))
			
			//Grab the modified value
			ldt_effective_date = DATE(idw_dw[1].GetItemDatetime(li_counter,"formulary_start_date"))

			IF ISNULL(ldt_effective_date) THEN
				messagebox("Validation Error", "Eligibility Start Date is invalid: Must Not Be Null")
				RETURN -1
			END IF 
	 
			IF ISNULL(ldt_effective_org) THEN 
				messagebox("Validation Error", "Invalid Original Start Date")
				RETURN -1
			END IF
			
			IF ldt_effective_date < date("1997-01-01") THEN
				messagebox("Validation Error","The effective date of a claim formulary coverage period " +&
					"~rmust be greater than or equal to January 01, 1997")
				RETURN -1
			END IF 
			
			// if they = than do nuttin continue on
			IF ldt_effective_org = ldt_effective_date THEN CONTINUE
			
			//check and see if it is a primary or secondary
			ls_formulary_type_code = idw_dw[1].GetItemstring(li_counter,"formulary_type_code")
			ls_formulary_code      = idw_dw[1].GetItemstring(li_counter,"formulary_code")
			ls_noi_code            = idw_dw[1].GetItemstring(li_counter,"primary_noi_code")
			ls_active_flag         = idw_dw[1].GetItemstring(li_counter,"primary_active_flag")
			ll_record_no           = idw_dw[1].GetItemnumber(li_counter,"formulary_record_no")
			
			/* 2.180 A new formulary coverage period must be created under the following conditions
            ·	the effective date is revised for a formulary coverage that has been set up on Blue Cross
			*/
			IF nf_has_record_been_exported(ll_claim_no,ll_record_no,"FORMULARY") = FALSE THEN CONTINUE
			
			IF ISNULL(ls_formulary_type_code) OR trim(ls_formulary_type_code) = "" THEN 
				messagebox("Validation Error BR# 2.180", "Invalid Formulary Type Code")
				RETURN -1
			END IF
			
			IF ISNULL(ls_formulary_code) OR trim(ls_formulary_code) = "" THEN 
				messagebox("Validation Error BR# 2.180", "Invalid Formulary Code")
				RETURN -1
			END IF
			
			IF (ls_formulary_type_code = "P" AND (ISNULL(ls_noi_code) OR trim(ls_noi_code) = "")) THEN 
				messagebox("Validation Error BR# 2.180", "Invalid NOI Code")
				RETURN -1
			END IF
			
			IF ISNULL(ls_active_flag) OR trim(ls_active_flag) = "" THEN 
				messagebox("Validation Error BR# 2.180", "Invalid Primary Active Flag")
				RETURN -1
			END IF
			
			// do the stuff....or not
			IF ldt_effective_org > ldt_effective_date THEN
				
				//create the comment
				ls_comment = "Start Date modified: " + string(ldt_effective_org,"yyyy-mm-dd") + " To " + string(ldt_effective_date,"yyyy-mm-dd")
				
				//Now create the new termination date
				ldt_termination_new = RelativeDate(ldt_effective_org, - 1)
				
				//check it
				IF ISNULL(ldt_termination_new) THEN RETURN -1
				
				// now set the origonal date back to the premodified date - before we do the insert
				idw_dw[1].setitem(li_counter,"formulary_start_date",datetime(ldt_effective_org))
				
				/* grab the inserted row */
				ll_row = idw_dw[1].insertrow(0)
				
				/* make sure we have a valid row */
				IF ISNULL(ll_row) OR ll_row < 0 THEN RETURN -1
				
				/* grab the last_formulary_record_no on the save */
				
				//set the default
				setnull(ldt_null_date)
				idw_dw[1].setitem(ll_row,"claim_no",ll_claim_no)
				idw_dw[1].setitem(ll_row,"formulary_code",ls_formulary_code)
				idw_dw[1].setitem(ll_row,"formulary_type_code",ls_formulary_type_code)
				idw_dw[1].setitem(ll_row,"formulary_start_date",datetime(ldt_effective_date))
				idw_dw[1].setitem(ll_row,"formulary_end_date",datetime(ldt_termination_new))
				idw_dw[1].setitem(ll_row,"formulary_comment",ls_comment)
				idw_dw[1].setitem(ll_row,"manual_entry_flag","Y")
				idw_dw[1].setitem(ll_row,"export_no",0)
				idw_dw[1].setitem(ll_row,"export_user_id",'')
				idw_dw[1].setitem(ll_row,"export_date",ldt_null_date)
				idw_dw[1].setitem(ll_row,"primary_noi_code",ls_noi_code)
            idw_dw[1].setitem(ll_row,"primary_active_flag",ls_active_flag)
					
					
				ib_auto_created = TRUE
			ELSE
				/* The new date is after the current date - 
					no addition needed but obviously will have to pass validation
				*/
			END IF 
		END IF 
	END IF 		
NEXT

RETURN 1
end function

public function integer nf_delete_eligibility (long al_claim_no);/*
2.30	A claim ELIGIBILITY coverage period must not be deleted if the claim ELIGIBILITY 
      coverage period has been exported to Bluecross 
		...The user may delete a claim ELIGIBILITY coverage period that was created as long as they 
		delete it before it gets exported to Bluecross
		
		-- Delete ELIGIBILITY	- to delete has not been exported
		
DEVELOPERS NOTE: This will be handled from within the calling code. 
               				  
1.30	A claim eligibility coverage period must not be deleted if the claim eligibility coverage 
      period has been exported to Bluecross
*/
LONG    ll_row, ll_eligibility_record_no, ll_event_no, ll_count
DATE    ldt_start, ldt_end, ldt_current
STRING  ls_message, ls_event_type, ls_event_code, ls_end_date, ls_entry_flag
INTEGER li_error, li_counter
DATETIME ldtm_min_eligibility_start, ldtm_min_formulary_start
n_event_log lnv_event_log

/* make sure it is an ELIGIBILITY - Primary cannot be deleted */
ll_row = idw_dw[4].getrow()

IF isnull(ll_row) OR ll_row = 0 THEN 
	messagebox("Delete?","Please select a record to delete before deleting")
	RETURN -1
END IF 

//double check that nothing has been updated
IF nf_check_modified_status(0) = TRUE THEN
	MESSAGEBOX("Delete ELIGIBILITY","Please Save or Cancel current work before proceeding with Delete.")
	RETURN -1
END IF 

//validate the claim number
IF ISNULL(al_claim_no) OR al_claim_no < 1 THEN RETURN -1

//grab the current date
ldt_current = date(f_server_datetime())

ll_eligibility_record_no = idw_dw[4].GetItemnumber(ll_row,'eligibility_record_no')
IF ISNULL(ll_eligibility_record_no) THEN RETURN -1 

IF ll_eligibility_record_no > 0 THEN
	IF nf_has_record_been_exported(al_claim_no,ll_eligibility_record_no,"DELETE_ELIGIBILITY_IND") = TRUE THEN
		messagebox("Validation Error BR# 1.30","A Claim Eligibility coverage period must not be deleted " +& 
	            "~rif the coverage period has been exported to Bluecross")
		RETURN -1
	END IF 
END IF 

//ls_entry_flag = idw_dw[4].GetItemstring(ll_row,"manual_entry_flag")
ldt_start = DATE(idw_dw[4].GetItemdatetime(ll_row,"eligibility_start_date"))
ldt_end   = DATE(idw_dw[4].GetItemdatetime(ll_row,"eligibility_end_date"))

/* 2.70	The effective date of a formulary coverage period that has not been set up on Blue Cross 
		   must be greater than or equal to the minimum effective date of all of the claim eligibility 
		   for the claim
*/
SELECT min(formulary_start_date)
  INTO :ldtm_min_formulary_start
  FROM CLAIM_FORMULARY a
 WHERE claim_no   = :al_claim_no
   AND export_no = 0
   AND NOT EXISTS (SELECT * 
		               FROM CLAIM_FORMULARY_HISTORY b
		              WHERE a.claim_no            = b.claim_no
		                AND a.formulary_record_no = b.formulary_record_no
			             AND b.export_no           > 0)

 USING SQLCA;
		
SQLCA.nf_handle_error('n_maintain_formulary','nf_delete_eligibility()','SELECT... - BR2.70')
		
IF NOT ISNULL(ldtm_min_eligibility_start) THEN

   SELECT min(eligibility_start_date)
	  INTO :ldtm_min_eligibility_start
	  FROM CLAIM_ELIGIBILITY a
	 WHERE claim_no               = :al_claim_no
	   AND eligibility_record_no <> :ll_eligibility_record_no 
	 USING SQLCA;
		
	SQLCA.nf_handle_error('n_maintain_formulary','nf_delete_eligibility()','SELECT... - BR2.70')
		
	IF DATE(ldtm_min_formulary_start) < DATE(ldtm_min_eligibility_start) THEN
		messagebox("Validation Error BR# 5.30","The effective date must be greater "+&
	              "~rthan or equal to "+string(ldtm_min_eligibility_start,"yyyy-mm-dd")+'.')
		RETURN -1
	END IF
END IF 
		
/* event_comment	CLAIM_ELIGIBILITY.eligibility_start_date to CLAIM_ELIGIBILITY.eligibility_end_date*/
ls_end_date   = string(ldt_end,"yyyy-mm-dd")
IF ISNULL(ls_end_date) OR ls_end_date = "" THEN ls_end_date = "Open-Ended"
ls_message    = "Delete: " + string(ldt_start,"yyyy-mm-dd") + " To " + ls_end_date

/* give the user an out. */
IF messagebox("Delete Eligibility", ls_message + "?", question!,yesno!) = 2 THEN RETURN 1

//create an instance of the object
lnv_event_log = create n_event_log

ls_event_type = "035"

//default to nothing
ls_event_code = ""
	
/* grab the next event number */
ll_event_no = lnv_event_log.nf_next_claim_event_no(al_claim_no)

//validate the event number
IF ISNULL(ll_event_no) OR ll_event_no < 1 THEN 
	messagebox("Validation Error","Could Not Determine Event Number in Eligibility Delete")
	RETURN -1
END IF

//create the auto event and return 
li_error =  lnv_event_log.nf_create_auto_event(al_claim_no,ll_event_no,ls_event_type,ls_message,ls_event_code)
IF li_error = -1 THEN 
	messagebox("Validation Error","Could not create Auto event in Eligibility Delete")
	RETURN -1
END IF
				
/* passes everything - delete the row from the datawindow */
IF idw_dw[4].deleterow(ll_row) = -1 THEN 
	messagebox("Validation Error","Could Not Delete Row in Eligibility Delete")
	RETURN -1
END IF

/* MAKE THIS SIMPLE DELETE & SAVE */
IF idw_dw[4].UPDATE() = -1 THEN 
	messagebox("Validation Error","Could Not Update information in Eligibility Delete")
	RETURN -1
END IF

RETURN 1

end function

public function integer nf_validate_formulary ();LONG     ll_claim_no, ll_formulary_no, ll_export_no, ll_count, ll_formulary_record_no
INTEGER  li_counter, li_rowcount, li_error, li_status, li_response, li_count, li_counter_2
STRING   ls_formulary_type_code, ls_formulary_code, ls_comment, ls_formulary_check
dwItemStatus l_status
DATE     ldt_current, ldt_exported_termination_date, ldt_exported_effective_date
DATE     ldt_formulary_start, ldt_formulary_end, ldt_original_start, ldt_original_end, ldt_end_check 
DATE     ldt_date_check, ldt_check_start, ldt_check_end, ldt_next_day
DATETIME ldtm_min_eligibility_start, ldtm_death_date
BOOLEAN  lb_check
STRING   ls_char

// grab the current datetime/date
ldt_current = date(f_server_datetime())
IF ISNULL(ldt_current) THEN RETURN -1 

// GRAB THE ROWCOUNT
li_rowcount = idw_dw[1].rowcount()
IF ISNULL(li_rowcount) OR li_rowcount < 1 THEN RETURN -1

/* each record has to be checked individually */
FOR li_counter = 1 TO li_rowcount
	
	//check and see the status of the row - no changes continue
   l_status = idw_dw[1].GetItemStatus(li_counter,0, Primary!)
	CHOOSE CASE l_status
		CASE NOTMODIFIED!
			li_status = 0
			//CONTINUE
		CASE NEWMODIFIED!
			//VALIDATE
			li_status = 1
		CASE DATAMODIFIED!
			//VALIDATE
			li_status = 2
		CASE NEW!
			li_status = 0
			//CONTINUE
		CASE ELSE
			li_status = 0
			//CONTINUE
	END CHOOSE
	
	ll_claim_no            = idw_dw[1].GetItemNumber(li_counter,'claim_no')
	ldt_formulary_start    = DATE(idw_dw[1].GetItemdatetime(li_counter,'formulary_start_date'))
	ldt_formulary_end      = DATE(idw_dw[1].GetItemdatetime(li_counter,'formulary_end_date'))
	ll_formulary_no        = idw_dw[1].GetItemNumber(li_counter,'formulary_record_no')
	ll_export_no           = idw_dw[1].GetItemNumber(li_counter,'export_no')
	ldt_original_start     = DATE(idw_dw[1].GetItemdatetime(li_counter,'formulary_start_date',PRIMARY!,TRUE))
	ldt_original_end       = DATE(idw_dw[1].GetItemdatetime(li_counter,'formulary_end_date',PRIMARY!,TRUE))
	ls_formulary_type_code = idw_dw[1].GetItemstring(li_counter,'formulary_type_code')
	ls_formulary_code      = idw_dw[1].GetItemstring(li_counter,'formulary_code')
	ls_comment             = idw_dw[1].GetItemstring(li_counter,'formulary_comment')
	ll_formulary_record_no = idw_dw[1].GetItemnumber(li_counter,'formulary_record_no')
	
	/* Do some reasonable edit checks on the values we will be using to test with */
	IF ISNULL(ll_claim_no) OR ll_claim_no < 1 THEN
		messagebox("Validation Error", "Claim Number is invalid")
		RETURN -1
	END IF 
	
	IF ISNULL(ldt_formulary_start) THEN
		messagebox("Validation Error", "Formulary Start Date is invalid - Must be entered")
		RETURN -1
	END IF 
	
	IF ISNULL(ll_export_no) OR ll_export_no < 0 THEN
		messagebox("Validation Error", "Export Number is invalid")
		RETURN -1
	END IF
	
	IF ISNULL(ls_formulary_type_code) OR trim(ls_formulary_type_code) = "" THEN
		messagebox("Validation Error", "formulary_type_code is invalid - Must be entered")
		RETURN -1
	END IF
	
	IF ISNULL(ls_comment) OR TRIM(ls_comment) = "" OR LEN(TRIM(ls_comment)) < 3 THEN
		messagebox("Validation Error", "A valid comment must be entered and must be at least three characters long.")
		RETURN -1
	END IF
	
	ls_char = LEFT(ls_comment,1) 
	IF (Asc(ls_char) >= Asc("a") AND Asc(ls_char) <= Asc("z")) OR (Asc(ls_char) >= Asc("A") AND Asc(ls_char) <= Asc("Z")) THEN
		//continue
	ELSE
		messagebox("Validation Error", "A valid comment must be entered and must not start with a blank space.")
		RETURN -1
	END IF
	
	IF ISNULL(ldt_original_start) THEN 
		messagebox("Validation Error", "Invalid Start Date")
		RETURN -1
	END IF
	
	IF ISNULL(ls_formulary_code) OR TRIM(ls_formulary_code) = "" THEN 
		messagebox("Validation Error", "Invalid Formulary Code")
		RETURN -1
	END IF
	
	/* 2.60	The effective date of a formulary coverage period must be greater than 
				or equal to January 01, 1997
	*/
	IF ldt_formulary_start < date("1997-01-01") THEN
			messagebox("Validation Error BR# 2.60","The effective date of the formulary coverage period " +&
						  "~rmust be greater than or equal to January 01, 1997")
			RETURN -1
	END IF 
	
	// Added for PR
	IF ldt_formulary_start > date("2079-06-01") THEN
			messagebox("Validation Error","The effective date of the formulary coverage period is to far in the future.")
			RETURN -1
	END IF 
	
	/* 2.100	The termination date of a formulary coverage period must be greater than or equal to the effective date 
		      of the formulary coverage period
	*/
	IF NOT ISNULL(ldt_formulary_end) THEN
		IF ldt_formulary_end < ldt_formulary_start THEN
			messagebox("Validation Error BR# 2.100","The termination date of the formulary coverage period must be greater than" +&
						  "~ror equal to "+String(ldt_formulary_start,"yyyy-mm-dd"))
			RETURN -1
		END IF
	END IF 
	
	/* 2.110	The termination date of a formulary coverage period must not be more than five years 
	         in the future if the date is not 'open-ended'  
   */
	ldt_date_check = nf_get_new_date("FIVE_YEARS")
	
	IF NOT ISNULL(ldt_formulary_end) THEN
		IF ldt_formulary_end > ldt_date_check THEN
			messagebox("Validation Error BR# 2.110","The termination date of a formulary coverage period must not be more than five years " +&
						  "~rin the future if the date is not open-ended")
			RETURN -1
		 END IF
	 END IF 
	
	//if nothing has happened then continue
	IF li_status = 0 THEN CONTINUE
	
	/* check and see if it is just the comment that has been updated...if so return 1
	   no furthur checks are needed
	*/
	IF idw_dw[1].GetItemstatus(li_counter,'formulary_start_date',PRIMARY!)   = notmodified! &
	  AND idw_dw[1].GetItemstatus(li_counter,'formulary_end_date',PRIMARY!)  = notmodified! &
	  AND idw_dw[1].GetItemstatus(li_counter,'formulary_code',PRIMARY!)      = notmodified! &
	  AND idw_dw[1].GetItemstatus(li_counter,'formulary_type_code',PRIMARY!) = notmodified! &
	  AND idw_dw[1].GetItemstatus(li_counter,'formulary_comment',PRIMARY!)   = datamodified! THEN
	  		CONTINUE
	END IF 
	
	/* check and see if this record has been exported  -- should check that NULL = NULL */
	IF ll_export_no > 0 THEN
	   lb_check = TRUE
	ELSE
	   lb_check = nf_has_record_been_exported(ll_claim_no,ll_formulary_record_no,"FORMULARY")
	END IF 
	
	//grab the exported termination date - It could be NULL...which is valid
	IF ll_export_no > 0 THEN
	   ldt_exported_termination_date = ldt_original_end
	ELSE	
	   ldt_exported_termination_date = nf_get_termination_date(ll_claim_no,"FORMULARY",ll_formulary_no)
   END IF 

	/* do basic validations that are common for Primary and Secondary */

	/* 2.10	Formulary coverage for a claim must not overlap with any other 
	         formulary coverage period for the same formulary code
				
		..."Coverage must not overlap" means: The effective date must not be {greater than or equal 
		to the effective date and less than or equal to the termination date} of any other formulary 
		coverage periods having the same formulary code and the termination date must not be 
		{greater than or equal to the effective date and less than or equal to the termination date} 
		of any other formulary coverage periods for formulary coverage periods having the same 
	   formulary code
	*/
	IF ISNULL(ldt_formulary_end) THEN 
		ldt_end_check = DATE("2078-01-01")
	ELSE
		ldt_end_check = ldt_formulary_end
	END IF 
	
	SELECT count(*) INTO :ll_count 
	  FROM CLAIM_FORMULARY  
	 WHERE claim_no               =  :ll_claim_no
		AND formulary_record_no    <> :ll_formulary_no
		AND ((formulary_start_date BETWEEN :ldt_formulary_start AND :ldt_end_check)
		 OR (formulary_end_date    BETWEEN :ldt_formulary_start AND :ldt_end_check)
		 OR (formulary_start_date > :ldt_formulary_start AND formulary_end_date < :ldt_end_check)
		 OR (:ldt_formulary_start BETWEEN formulary_start_date AND formulary_end_date))
		AND formulary_code = :ls_formulary_code	
	 USING SQLCA;
		 
	 SQLCA.nf_handle_error('n_maintain_formulary', 'nf_validate_formulary()', 'SELECT count(*)... - BR2.10 A') 
		
	 IF ll_count > 0 THEN
		 messagebox("Validation Error BR# 2.10","Formulary coverage for a claim must not overlap with any " +&
															 "~rother coverage period for the same formulary code")
		 RETURN -1
	 END IF 
	 
	  FOR li_counter_2 = 1 TO idw_dw[1].rowcount()
	
		 IF li_counter_2 = li_counter THEN CONTINUE
		
		 ldt_check_start = DATE(idw_dw[1].GetItemdatetime(li_counter_2,'formulary_start_date'))
       ldt_check_end   = DATE(idw_dw[1].GetItemdatetime(li_counter_2,'formulary_end_date'))
		 ls_formulary_check = idw_dw[1].GetItemstring(li_counter_2,'formulary_code')
		 
		 IF ISNULL(ldt_check_end) THEN ldt_check_end = DATE("2078-01-01")
		  
		 IF ldt_formulary_start >= ldt_check_start AND ldt_formulary_start <= ldt_check_end AND ls_formulary_check = ls_formulary_code THEN 
			 ll_count = 1
		 END IF 
		 IF ldt_formulary_start <= ldt_check_start AND ldt_formulary_start >= ldt_check_end AND ls_formulary_check = ls_formulary_code THEN 
			 ll_count = 1
		 END IF 
		
		 //CHECK RULE 1.20 HERE AS WELL
		 IF ldt_formulary_end > ldt_current OR ISNULL(ldt_formulary_end) THEN 
				IF ((ldt_check_end > ldt_current OR ISNULL(ldt_check_end)) AND ls_formulary_check = ls_formulary_code) THEN ll_count = 2
		 END IF  
	 NEXT
	
	 IF ll_count = 1 THEN
		 messagebox("Validation Error BR# 2.10","Claim Formulary coverage must not overlap any other Claim Formulary coverage period/Formulary Code")
		 RETURN -1
	 END IF 
	 
	 IF ll_count = 2 THEN
		 messagebox("Validation Error BR# 2.20","A claim must have only one claim Formulary coverage period" +&
															  "~rwith a termination date that is open-ended or is in the future for a specific Formulary Code")
		 RETURN -1
	 END IF 
	 
	/* 2.20	A claim must have only one formulary coverage period for a 
	         formulary code with a termination date that is open-ended or is in the future 
	
	   ...This is a Reasonable Edit check that prevents the user from creating more than one coverage 
		period for a formulary code that has a termination date in the future. For example: If the user 
		has already set up formulary W6 with coverage from June 30 - Sept 1 2006, do not allow another 
		coverage period for formulary F6 of  Oct 1 - Nov 30 2006. It is not a reasonable business need 
		to create two coverage periods for a formulary that both are future dates.
		
		except for automatically created records than ignore this rule
	*/
	
	IF ((ldt_formulary_end > ldt_current OR isnull(ldt_formulary_end)) AND ib_auto_created = FALSE) THEN
		
		SELECT count(*) INTO :ll_count 
		  FROM CLAIM_FORMULARY 
		 WHERE claim_no            =  :ll_claim_no
			AND formulary_record_no <> :ll_formulary_no
			AND (formulary_end_date >  :ldt_current OR formulary_end_date IS NULL)
         AND formulary_code      =  :ls_formulary_code
		 USING SQLCA;
		 
		 SQLCA.nf_handle_error('n_maintain_formulary','nf_validate_formulary()','SELECT count(*)... - BR2.20') 
	
		IF ll_count > 0 THEN
			messagebox("Validation Error BR# 2.20","A claim must must have only one claim formulary coverage period" +&
															  "~rwith a termination date that is open-ended or is in the future")
			RETURN -1
		END IF 
	END IF 
	
	/* 2.120	The termination date of a formulary coverage period should not be more than 
		      one year in the future if the date is not 'open-ended'  
   */
	
		ldt_date_check = nf_get_new_date("YEAR")
		
		//IF ldt_formulary_end > Date(Year(ldt_current) + 1, Month(ldt_current), Day(ldt_current)) THEN
		IF ldt_formulary_end > ldt_date_check THEN
			li_response = messagebox("Validation Warning BR# 2.120","The termination date of a formulary coverage period should not be more than " +&
		                            "~rone year in the future if the date is not open-ended...Continue?", question!,yesno!)
		
			IF li_response = 2 THEN RETURN -1
		END IF 

   /* 2.30	A formulary coverage period must not be deleted if the formulary coverage 
	         has been exported to Bluecross 
				
			   DEVELOPERS NOTE: This will be handled from within the calling code. 
	                 I believe there is a delete button that will call the delete code
						  most likely a delete function. nf_delete_formulary()
	*/
	
	/* 2.40	A formulary coverage period must not be updated under the following conditions:
				·	The claim is registered and the claim status is not one of the following statuses
				o	Active or Finalled (Finalled-Final, Finaled-First&Final, Finaled-LostTime Medical Aid Only 
				or Finalled-No Lost Time)
				
				NOTE: THIS IS HANDLED ELSEWHERE - JUST A CHECK
	*/
	IF ldt_formulary_start <> ldt_original_start OR ldt_formulary_end <> ldt_original_end THEN
		SELECT count(*) INTO :ll_count 
		  FROM CLAIM_FORMULARY a , CLAIM b, X001_REGISTRATION c
		 WHERE a.claim_no   = :ll_claim_no
			AND a.claim_no   = b.claim_no
			AND a.claim_no   = c.claim_no
			AND NOT (b.claim_status_code = 'A' OR (b.claim_status_code = 'F' AND b.claim_status_type_code IN('01','02','03','04')))
		 USING SQLCA;
		 
		 SQLCA.nf_handle_error('n_maintain_formulary','nf_validate_formulary()','SELECT count(*)... - BR2.40') 
	
		IF ll_count > 0 THEN
			messagebox("Validation Error BR# 2.30","To update a claim formulary coverage period, the claim must be registered "+&
			          "~rand claim status must be either Active or Final (Final, F&F, NLT, LTMed Aid Only).")
			RETURN -1
		END IF
	END IF 

	/* 2.50	A formulary coverage period must not be added under either of the following conditions
				·	The claim is not registered  OR
				·	The claim is registered and the claim status is not one of the following statuses
				o	Active or Finalled (Finalled-Final, Finaled-First&Final, Finaled-LostTime Medical Aid Only 
				or Finalled-No Lost Time)
	*/
	IF li_status = 1 THEN 
		SELECT count(*) INTO :ll_count 
		  FROM CLAIM_FORMULARY a , CLAIM b
		 WHERE a.claim_no   = :ll_claim_no
			AND a.claim_no   = b.claim_no
			AND NOT EXISTS (SELECT * FROM X001_REGISTRATION c WHERE a.claim_no = c.claim_no)
		 USING SQLCA;
		 
		 SQLCA.nf_handle_error('n_maintain_formulary','nf_validate_formulary()','SELECT count(*) - BR2.50') 
	
		IF ll_count > 0 THEN
			messagebox("Validation Error BR# 2.50","To add a claim formulary coverage period, "+&
			           "~rthe claim must be registered and claim status must be either Active or Final (Final, F&F, NLT, LTMedAidOnly).")
			RETURN -1
		END IF 
		
		SELECT count(*) INTO :ll_count 
		  FROM CLAIM_FORMULARY a , CLAIM b
		 WHERE a.claim_no   = :ll_claim_no
			AND a.claim_no   = b.claim_no
			AND NOT (b.claim_status_code = 'A' OR (b.claim_status_code = 'F' AND b.claim_status_type_code IN('01','02','03','04')))
		 USING SQLCA;
		 
		 SQLCA.nf_handle_error('n_maintain_formulary', 'nf_validate_formulary()', 'SELECT count(*) - BR2.50') 
	
		IF ll_count > 0 THEN
			messagebox("Validation Error BR# 2.50","Invalid Claim Status, a claim formulary coverage period cannot be added.")
		END IF 
	END IF
	
	/* EFFECTIVE DATE RULES */

	IF lb_check = FALSE THEN 
		
		SELECT min(eligibility_start_date)
		  INTO :ldtm_min_eligibility_start
		  FROM CLAIM_ELIGIBILITY a
		 WHERE claim_no   = :ll_claim_no
		 USING SQLCA;
		
		SQLCA.nf_handle_error('n_maintain_formulary','nf_validate_formulary()','SELECT... - BR2.70')
		
		IF ISNULL(ldtm_min_eligibility_start) THEN
			messagebox("Validation Error","No Minimum Eligibility Start Date has been detected. Please contact the Helpdesk.")
			RETURN -1
		END IF 
		
		/* 5.30	The effective date of a formulary coverage period that has not been set up on Blue Cross 
		         must be greater than or equal to the minimum effective date of all of the claim eligibility 
					for the claim ?????
		*/
		IF ldt_formulary_start < DATE(ldtm_min_eligibility_start) THEN
			messagebox("Validation Error BR# 5.30","The effective date of the formulary coverage period must be greater "+&
		              "~rthan or equal to "+String(DATE(ldtm_min_eligibility_start),"yyyy-mm-dd"))
			RETURN -1
		END IF
		
		
		/* 2.80	The effective date for a formulary coverage that has not been set up on Blue Cross 
		         must not be more than one month in the future
		*/
		ldt_date_check = nf_get_new_date("MONTH")
		
		IF ldt_formulary_start > ldt_date_check THEN
			messagebox("Validation Error BR# 2.80","The effective date for a formulary coverage that has not been set up on Blue Cross" +& 
		              "~rmust not be more than one month in the future")
			RETURN -1
		END IF
		
	ELSE//BEEN EXPORTED
	
		/* 2.90 The effective date for a formulary coverage that has been set up on Blue Cross
		       must be less than or equal to the effective date on Blue Cross for that formulary coverage 
		*/
		IF ldt_formulary_start <> ldt_original_start THEN
			IF ldt_formulary_start > ldt_exported_effective_date THEN
				messagebox("Validation Error BR# 2.90","A changed effective date for a formulary coverage that is already set up on Blue Cross "+&
				           "~rmust be earlier than "+String(ldt_exported_effective_date,"yyyy-mm-dd"))
				RETURN -1
			END IF 
		END IF 
	END IF 
			
	/* 2.130	The termination date of a formulary coverage period that has not been exported must 
	         be less than or equal to the individual's death date if the individual is deceased 
				
				APPEARS TO HAVE BEEN COMBINED WITH 2.140
	*/

	/* 2.140	The termination date of a formulary coverage period must be less than or equal 
	         to the individual's death date under either of the following conditions:
		1.	the formulary coverage has not been set up on Blue Cross:
			a.	the individual is deceased
		2.	the formulary coverage has been set up on Blue Cross:
			a.	the individual is deceased and
			b.	the termination date on Blue Cross is the current date or is in the past and
			c.	the death date is greater than the termination date on Blue Cross
	*/
	
	SELECT death_date INTO :ldtm_death_date
	  FROM CLAIM a, INDIVIDUAL b
	 WHERE a.claim_no      = :ll_claim_no
		AND a.individual_no = b.individual_no
	 USING SQLCA;
				 
	 SQLCA.nf_handle_error('n_maintain_formulary','nf_validate_formulary()','SELECT death_date BR2.140')
				
	IF lb_check = FALSE THEN
		IF NOT ISNULL(ldtm_death_date) THEN
			IF ldt_formulary_end > date(ldtm_death_date) THEN
				messagebox("Validation Error BR# 2.130","The Termination Date of the claim eligibility coverage period " +&
	                    "~rmust be less than or equal to the individual's death date of"+String(Date(ldtm_death_date),"yyyy-mm-dd"))
				RETURN -1
			END IF
		END IF 
	ELSE
		IF NOT ISNULL(ldtm_death_date) THEN
			IF ldt_exported_termination_date <= ldt_current AND date(ldtm_death_date) > ldt_exported_termination_date THEN
				IF ldt_formulary_end > date(ldtm_death_date) THEN
					messagebox("Validation Error BR# 2.130","The Termination Date of the claim formulary coverage period " +&
	                    "~rmust be less than or equal to the individual's death date of "+String(Date(ldtm_death_date),"yyyy-mm-dd"))
					RETURN -1
				END IF
			END IF 
		END IF 
	END IF 
	
   /* 2.150	The termination date of a formulary coverage period must be greater than or equal
	         to the termination date on Blue Cross under the following conditions:
		·	the formulary coverage has been set up on Blue Cross
		·	the termination date on Blue Cross is the current date or is in the past
		
		...In this case, the termination date does not need to be the date of the next eligibility 
		run as coverage is already terminated when it gets processed at Bluecross. Example A:  
		Coverage is from Jan 1, 2004 to Feb 28, 2005.  On May 6th, the user revises the termination 
		date to be March 30th  due to a change in decision. The user may revise the termination date 
		to be any date greater than Feb 28th as Bluecross does not have to be concerned with a client 
		having had coverage on the date it was processed.  However, if the termination date had been 
		July 22nd, the user would be restricted to revising the termination date to the next time Bluecross 
		processes the file as the claim is still receiving coverage on the date of processing.
	*/
	
	IF lb_check = TRUE AND (ldt_formulary_end <> ldt_original_end) THEN
		IF NOT ISNULL(ldt_formulary_end) THEN
			IF ldt_exported_termination_date <= ldt_current THEN
				IF ldt_formulary_end < ldt_exported_termination_date THEN 
					messagebox("Validation Error BR# 2.150","The termination date must be greater than or equal to "+String(ldt_exported_termination_date,"yyyy-mm-dd"))
					RETURN -1
				END IF 
			END IF
		END IF
	END IF 

   /* 2.160	(REVISED) The termination date of a formulary coverage period must be equal 
	   to the date of the next scheduled Bluecross Eligibility Process run under the following 
		conditions:
		· the formulary coverage has been set up on Blue Cross
		· the termination date on Blue Cross is in the future or is open-ended
		· the individual is deceased

		...If the existing termination date is a future date, the user must revise it to be a 
		date greater than or equal to the next Blue Cross Eligibility Process run;  Blue Cross will 
		actually process the eligibility file one day after the WHSCC Eligibility Export. The Bluecross 
		'working days' can be found in the Company_Calendar; based on this information, the next Blue 
		Cross Eligibility Process run would be the day following the first available working day for 
		Blue Cross that is greater than or equal to the current date. E.g. If the coverage termination 
		date is a future date of Dec 31, 2006 and the current date is Friday, October 28, 2005 then the 
		first working date for Blue Cross is October 28, 2005. Therefore, the user must enter a termination date 

	 2.165 (NEW) The termination date of a formulary coverage period must be greater than or 
	   equal to the date of the next scheduled Bluecross Eligibility Process run under the 
		following conditions:
		· the formulary coverage has been set up on Blue Cross
		· the termination date on Blue Cross is in the future or is open-ended
		· the individual is not deceased
	
	*/
	IF lb_check = TRUE THEN
		
		
		ldt_next_day = nf_get_abcc_work_day()	
				
		 IF ISNULL(ldt_next_day) Then
			 MessageBox('ABCC Calendar','The ABCC Calendar is not loaded. Please notify Helpdesk.')
			 RETURN -1
		 END IF	
		
		 IF ISNULL(ldt_exported_termination_date) OR ldt_exported_termination_date > ldt_current THEN
			IF NOT ISNULL(ldtm_death_date) THEN 
				IF ldt_formulary_end <> ldt_next_day OR isnull(ldt_formulary_end) THEN
					messagebox("Validation Error BR# 2.160","The Formulary Coverage termination date must be equal to "+String(ldt_next_day,"yyyy-mm-dd"))
					RETURN -1
				END IF 
			ELSE
				IF ldt_formulary_end < ldt_next_day THEN
					messagebox("Validation Error BR# 2.165","The Formulary Coverage must be greater than or equal to "+string(ldt_next_day,"yyyy-mm-dd"))
					RETURN -1
				END IF 	
			END IF 
		END IF 
	END IF 
	
   /* 2.170 The user must be warned that all formulary coverage has expired and claim eligibility 
	         coverage is still open under the following conditions:
      ·	All of the formulary coverage periods have expired and at least one of the claim eligibility  
         coverage periods for the claim has not  expired
			
		...170 Claim Eligibility Coverage supercedes formulary coverage when it comes to determining whether 
		or not a DIN is accepted for coverage; For example: if the user enters a claim eligibility termination 
		date of June 1, 2005 but the claim still has a formulary coverage period that does not expired until 
		Sept 2005, the claim will not be covered for the DINs in that formulary as the claim's eligibility period 
		is expired. This is only a warning message as there may be cases where they want to temporarily terminate 
		the claim without making the formulary coverage changes. It is also valid to have a claim eligibility coverage 
		that is not expired while having all formulary coverage for a claim expired. This typically occurs when the 
		DIN coverage is being handled exclusively through Special Drug Authorizations and not through any formularies 
		- the claim eligibility must be open if the SDA is to be covered. SDA's are entered exclusively on the Bluecross 
		System.
   */	
	IF ISNULL(ldt_formulary_end) OR ldt_formulary_end > ldt_current THEN
		
		SELECT count(*) INTO :li_count
		  FROM CLAIM_ELIGIBILITY
		 WHERE claim_no              = :ll_claim_no
			AND (eligibility_end_date is null
			 OR eligibility_end_date  >= :ldt_current)
		 USING SQLCA;
						 
	 	SQLCA.nf_handle_error('n_maintain_formulary','nf_validate_formulary()','SELECT - BR2.170B')
					
		//default to 0 for NULL
		IF ISNULL(li_count) THEN li_count = 0
					
		//If we have some open tell them
		IF li_count > 0 THEN CONTINUE

   	messagebox("Informational Warning BR# 5.20","The Claim Formulary coverage is Open without an open Eligibility coverage period.", information!)
   		
	ELSE
	
		//see if there is a null value
		SELECT count(*) INTO :li_count
		  FROM CLAIM_FORMULARY
		 WHERE claim_no            = :ll_claim_no
			AND (formulary_end_date  is null
			 OR formulary_end_date  >= :ldt_current)
			AND formulary_record_no <> :ll_formulary_no
		 USING SQLCA;
					 
		 SQLCA.nf_handle_error('n_maintain_formulary','nf_validate_formulary()','SELECT - BR2.170A')
				
		//default to 0 for NULL
		IF ISNULL(li_count) THEN li_count = 0
				
		//if li_count = 0 THEN make sure we don't have any open formularies
		IF li_count = 0 THEN 
				
			 SELECT count(*) INTO :li_count
				FROM CLAIM_ELIGIBILITY
			  WHERE claim_no             = :ll_claim_no
				 AND (eligibility_end_date is null
				  OR eligibility_end_date >= :ldt_current)
			  USING SQLCA;
						 
	 		 SQLCA.nf_handle_error('n_maintain_formulary','nf_validate_formulary()','SELECT - BR2.170B')
					
			//default to 0 for NULL
			IF ISNULL(li_count) THEN li_count = 0
					
			//If we have some open tell them
			IF li_count = 0 THEN CONTINUE

   		messagebox("Informational Warning BR# 5.20"," The Claim Eligiblity coverage is open without an open Formulary Coverage period.", information!)
   		
   	END IF 
	END IF 
	
	/* 2.180 A new formulary coverage period must be created under the following conditions
		·	the effective date is revised for a formulary coverage that has been set up on Blue Cross
		NOT A BR - JUST A CHECK
	*/
	
	/* now split the validation up between Primary and secondary and pass in the row */
	CHOOSE CASE ls_formulary_type_code
		CASE "S"
			
			li_error = nf_validate_secondary_formulary(li_counter)
			
		CASE "P"
			
			li_error = nf_validate_primary_formulary(li_counter)
			
		CASE ELSE //PROBLEM
			RETURN -1
	END CHOOSE
	
	IF li_error = -1 THEN RETURN -1 //make sure this works
	
NEXT

RETURN 1
end function

public function boolean nf_check_modified_status (integer ai_datawindow);/* checks the status of the varius datawindows in order to determine if they have been modified or not
   IF they have been modified the function return true
	
   CASE = 0 check all return if any are valid
*/
dwItemStatus l_status

CHOOSE CASE ai_datawindow
	CASE 1//formulary
		
		idw_dw[1].accepttext()
		IF idw_dw[1].modifiedcount() > 0 THEN RETURN TRUE
				
	CASE 2//prescription - NEVER GETS UPDATED
	CASE 3//event - NEVER GETS UPDATED
		
	CASE 4//eligibility
		
		idw_dw[4].accepttext()
		IF idw_dw[4].modifiedcount() > 0 THEN RETURN TRUE
	CASE 0
		
		idw_dw[1].accepttext()
		IF idw_dw[1].modifiedcount() > 0 THEN RETURN TRUE
		
		idw_dw[4].accepttext()
		IF idw_dw[4].modifiedcount() > 0 THEN RETURN TRUE
		
		idw_dw[12].AcceptText()
		IF idw_dw[12].ModifiedCount() > 0 THEN RETURN TRUE
			
	CASE 12
		
		idw_dw[12].AcceptText()
		IF idw_dw[12].ModifiedCount() > 0 THEN RETURN TRUE
		
END CHOOSE

RETURN FALSE
end function

public function integer nf_cancel ();LONG ll_claim_no, ll_event_row, ll_eligibility_row, ll_formulary_row, ll_rx_row, ll_sa_row, ll_sa_hist_row , ll_rx_individ_row,ll_elig_hist_row, ll_form_hist_row

idw_dw[1].accepttext()
idw_dw[4].accepttext()

/* grab the claim number */
ll_claim_no = nf_get_claim_no()

/* do a basic check */
IF ISNULL(ll_claim_no) OR ll_claim_no < 0  THEN 
	messagebox("Invalid Claim Number","The Claim number could not be determined")
	RETURN -1
END IF 

/* check to see if any of the datawindows have been modified */
/* first check and see if anything has been modified on idw_dw[1] - dw_formulary*/
IF nf_check_modified_status(1) = TRUE OR nf_check_modified_status(4) = TRUE THEN
	IF messagebox("Cancel?","Information has been modified. Continue with cancel?",Question!,yesno!) = 2 THEN 
		RETURN -1
	END IF 	
END IF 


/* now I want to find the current row for the bottom two datawindows */
ll_event_row       = idw_dw[3].getrow()
ll_eligibility_row = 1
ll_formulary_row   = idw_dw[1].getrow()
ll_rx_row				= idw_dw[2].getrow()
ll_rx_individ_row	= idw_dw[5].getrow()
ll_sa_row			= idw_dw[6].getrow()
ll_sa_hist_row		= idw_dw[8].getrow()
ll_elig_hist_row		= idw_dw[9].getrow()
ll_form_hist_row	= idw_dw[10].getrow()

//if there isn't anything than simply set the row to 1 - it will be checked before going to it.
IF isnull(ll_event_row) OR ll_event_row < 1  THEN ll_event_row = 1
IF isnull(ll_formulary_row) OR ll_formulary_row < 1  THEN ll_formulary_row = 1
IF isnull(ll_rx_row) OR ll_rx_row < 1  THEN ll_rx_row = 1

/* retrieve everything */
nf_retrieve(ll_claim_no)

/* Now reset the rows to the last ones the users were on */
//formulary
IF idw_dw[1].rowcount() >= ll_formulary_row THEN
	idw_dw[1].setrow(ll_formulary_row)
	idw_dw[1].scrolltorow(ll_formulary_row)
	idw_dw[1].selectrow(ll_formulary_row,TRUE)
END IF 

//prescription
IF idw_dw[2].rowcount() >= ll_rx_row THEN
	idw_dw[2].setrow(ll_rx_row)
	idw_dw[2].scrolltorow(ll_rx_row)
END IF 

//event
IF idw_dw[3].rowcount() >= ll_event_row THEN
	idw_dw[3].setrow(ll_event_row)
	idw_dw[3].scrolltorow(ll_event_row)
END IF 

//eligibility
IF idw_dw[4].rowcount() >= ll_eligibility_row THEN
	idw_dw[4].setrow(ll_eligibility_row)
	idw_dw[4].scrolltorow(ll_eligibility_row)
	idw_dw[4].selectrow(ll_eligibility_row,TRUE)
END IF 

//prescription by individual
IF idw_dw[5].rowcount() >= ll_rx_individ_row THEN
	idw_dw[5].setrow(ll_rx_individ_row)
	idw_dw[5].scrolltorow(ll_rx_individ_row)
	idw_dw[5].selectrow(ll_rx_individ_row,TRUE)
END IF

//special auth
IF idw_dw[6].rowcount() >= ll_sa_row THEN
	idw_dw[6].setrow(ll_sa_row)
	idw_dw[6].scrolltorow(ll_sa_row)
	idw_dw[6].selectrow(ll_sa_row,TRUE)
END IF

//sa history
IF idw_dw[8].rowcount() >=  ll_sa_hist_row THEN
	idw_dw[8].setrow(ll_sa_hist_row)
	idw_dw[8].scrolltorow(ll_sa_hist_row)
	idw_dw[8].selectrow(ll_sa_hist_row,TRUE)
END IF

//eligibility history
IF idw_dw[9].rowcount() >=  ll_sa_hist_row THEN
	idw_dw[9].setrow(ll_sa_hist_row)
	idw_dw[9].scrolltorow(ll_sa_hist_row)
	idw_dw[9].selectrow(ll_sa_hist_row,TRUE)
END IF

//formulary history
IF idw_dw[10].rowcount() >=  ll_sa_hist_row THEN
	idw_dw[10].setrow(ll_sa_hist_row)
	idw_dw[10].scrolltorow(ll_sa_hist_row)
	idw_dw[10].selectrow(ll_sa_hist_row,TRUE)
END IF

RETURN 1
end function

public function integer nf_create_auto_eligibility ();/*
If the effective date is modified by the user, the module 
must automatically create a new coverage period record for 
the claim eligibility. The reason for this is that on the 
Bluecross system, the effective date is a primary key field 
and it cannot be updated; subsequently, a new coverage record 
must be created. This should only be done when the data is 
saved and ready to be committed. The new coverage period record 
will have an effective date equal to the effective date that the 
user entered and a termination date equal to one day less than the 
old effective date.

For example:	Claim Eligibility is Feb 18 - June 30.  If the user 
	modifies the effective date to be February 1st, the module will 
	need to create a new coverage record with a coverage period of 
	Feb 1 - Feb 17. The end result will be two coverage records for 
	the claim with no gap in coverage.
	Feb 1	  to	Feb 17
	Feb 18	  to	June 30
*/

//NOTE: If this returns a -1  than make sure the canel button is triggered. 
//NOTE: This code will be called at the end from the save button.

LONG		    ll_row, ll_claim_no, ll_eligibility_record_no
DATE         ldt_null_date, ldt_start_date, ldt_start_org, ldt_end_date,ldt_end_org
INTEGER      li_counter, li_rowcount, li_status 
dwItemStatus l_status
BOOLEAN      lb_check
STRING       ls_comment

/* grab the claim number */
ll_claim_no = nf_get_claim_no()

IF ISNULL(ll_claim_no) OR ll_claim_no < 0 THEN RETURN -1

/* can only be added if the claim is active or final. This is just a second safety check
	is also checked from the calling code
*/
IF nf_check_status() <> 1 THEN 
	messagebox("Invalid Status", "Invalid Claim status for the creation of Auto Eligibility" )
	RETURN -1 
END IF 

// grab the rowcount for the datawindow 
li_rowcount = idw_dw[4].rowcount()

// make sure it's valid
IF ISNULL(li_rowcount) OR li_rowcount < 0 THEN RETURN -1

/* check the datawindow for a modified record where the effective date has been changed */
FOR li_counter = 1 TO li_rowcount
	
	// grab the status of the row
	l_status = idw_dw[4].getitemstatus(li_counter,0,PRIMARY!)
	
	CHOOSE CASE l_status
		CASE NotModified!
			li_status = 1
		CASE DataModified!
			li_status = 2
		CASE New!
			li_status = 3
		CASE NewModified!
			li_status = 4
    END CHOOSE
		
	 //IF  it's one of these statuses than continue	
	 IF li_status = 1 or li_status = 3 THEN CONTINUE
	 
	 CHOOSE CASE li_status
		CASE 2//datamodified! (modify)
			
			ldt_start_date = DATE(idw_dw[4].GetItemDatetime(li_counter,"eligibility_start_date"))
			ldt_start_org  = DATE(idw_dw[4].GetItemDatetime(li_counter,"eligibility_start_date", Primary!, TRUE))
			ldt_end_date   = DATE(idw_dw[4].GetItemDatetime(li_counter,"eligibility_end_date"))
			ldt_end_org    = DATE(idw_dw[4].GetItemDatetime(li_counter,"eligibility_end_date", primary!,TRUE))
			ll_eligibility_record_no = idw_dw[4].GetItemnumber(li_counter,'eligibility_record_no')
				
			/* 1.180 A new claim eligibility coverage period must be created under the following conditions
           ·	the effective date is revised for a claim eligibility coverage that has been set up on Blue Cross
			*/
			/* check and see if this record has been exported  -- should check that NULL = NULL */
			lb_check = nf_has_record_been_exported(ll_claim_no,ll_eligibility_record_no,"ELIGIBILITY")
			
			IF ISNULL(ldt_start_date) THEN
				messagebox("Validation Error", "Eligibility Start Date is invalid: Must Not Be Null")
				RETURN -1
			END IF 
			 
			IF lb_check = TRUE THEN
				IF ldt_start_org < ldt_start_date  THEN 
					messagebox("Validation Error BR# 1.80", "Modify Eligibility Start Date Cannot be greater than the original Start Date.")
					RETURN -1
				END IF
			END IF 
				
			IF NOT ISNULL(ldt_end_date) THEN
				IF ldt_start_date > ldt_end_date THEN
					messagebox("Validation Error BR# 1.90", "Eligibility Export End Date is less than the Start Date.")
					RETURN -1
				END IF
			END IF
	
			IF ISNULL(ldt_start_org) THEN 
				messagebox("Validation Error", "Invalid Original Start Date")
				RETURN -1
			END IF
			
			IF ldt_start_date < date("1997-01-01") THEN
				messagebox("Validation Error BR# 1.60","The effective date of a claim eligibility coverage period " +&
					"~rmust be greater than or equal to January 01, 1997")
				RETURN -1
			END IF 
			
			IF idw_dw[4].getitemstatus(li_counter,"eligibility_start_date",PRIMARY!) = datamodified! THEN
				
				//nothing has really been done to the record
				IF ldt_start_org <= ldt_start_date THEN CONTINUE
				//IF lb_check = FALSE THEN CONTINUE
				
				IF ldt_start_org < DATE("2004-01-01") OR (ldt_start_org >= DATE("2004-01-01") AND ldt_start_date >= DATE("2004-01-01")) THEN
					
					/* set it back to the old date */
					idw_dw[4].setitem(li_counter,"eligibility_start_date",ldt_start_org)
					
					/* if it puts a modify date on it set it to notmodified */
					idw_dw[4].SetItemStatus(li_counter, 0,Primary!, NotModified!)
					
					//create the comment
					ls_comment = "Start Date modified: " + string(ldt_start_org,"yyyy-mm-dd") + " To " + string(ldt_start_date,"yyyy-mm-dd")
					
					//insert split new record
					ll_row = idw_dw[4].insertrow(0)
				
					/* make sure we have a valid row */
					IF ISNULL(ll_row) OR ll_row < 0 THEN RETURN -1
				
				   setnull(ldt_null_date)
					idw_dw[4].setitem(ll_row,"claim_no",ll_claim_no)
					idw_dw[4].setitem(ll_row,"eligibility_start_date",ldt_start_date)
					idw_dw[4].setitem(ll_row,"eligibility_end_date",relativedate(ldt_start_org, -1))
					idw_dw[4].setitem(ll_row,"comment",ls_comment)
					idw_dw[4].setitem(ll_row,"manual_entry_flag","Y")
					idw_dw[4].setitem(ll_row,"export_no",0)
					idw_dw[4].setitem(ll_row,"export_user_id",'')
					idw_dw[4].setitem(ll_row,"export_date",ldt_null_date) 	
				
				END IF 
				
				IF ldt_start_org >= DATE("2004-01-01") AND ldt_start_date < DATE("2004-01-01") THEN
					
					/* set it back to the old date */
					idw_dw[4].setitem(li_counter,"eligibility_start_date",ldt_start_org)
					
					/* if it puts a modify date on it set it to notmodified */
					idw_dw[4].SetItemStatus(li_counter, 0,Primary!, NotModified!)
					
					//create the comment
					ls_comment = "Start Date modified: " + string(ldt_start_org,"yyyy-mm-dd") + " To " + string(ldt_start_date,"yyyy-mm-dd")
					
					//insert split new record
					ll_row = idw_dw[4].insertrow(0)
				
					/* make sure we have a valid row */
					IF ISNULL(ll_row) OR ll_row < 0 THEN RETURN -1
				
				   setnull(ldt_null_date)
					idw_dw[4].setitem(ll_row,"claim_no",ll_claim_no)
					idw_dw[4].setitem(ll_row,"eligibility_start_date",ldt_start_date)
					idw_dw[4].setitem(ll_row,"eligibility_end_date",DATE("2003-12-31"))
					idw_dw[4].setitem(ll_row,"comment",ls_comment)
					idw_dw[4].setitem(ll_row,"manual_entry_flag","Y")
					idw_dw[4].setitem(ll_row,"export_no",0)
					idw_dw[4].setitem(ll_row,"export_user_id",'')
					idw_dw[4].setitem(ll_row,"export_date",ldt_null_date) 
					
					IF ldt_start_org <> DATE("2004-01-01") THEN
						//insert split new record
						ll_row = idw_dw[4].insertrow(0)
					
						/* make sure we have a valid row */
						IF ISNULL(ll_row) OR ll_row < 0 THEN RETURN -1
					
						setnull(ldt_null_date)
						idw_dw[4].setitem(ll_row,"claim_no",ll_claim_no)
						idw_dw[4].setitem(ll_row,"eligibility_start_date",DATE("2004-01-01"))
						idw_dw[4].setitem(ll_row,"eligibility_end_date",relativedate(ldt_start_org, -1))
						idw_dw[4].setitem(ll_row,"comment",ls_comment)
						idw_dw[4].setitem(ll_row,"manual_entry_flag","Y")
						idw_dw[4].setitem(ll_row,"export_no",0)
						idw_dw[4].setitem(ll_row,"export_user_id",'')
						idw_dw[4].setitem(ll_row,"export_date",ldt_null_date) 
					END IF 
				END IF
			END IF 
			
			//TERMINATION DATE
			
			IF idw_dw[4].getitemstatus(li_counter,"eligibility_end_date",PRIMARY!) = datamodified! THEN
				IF (ldt_end_date >= DATE("2004-01-01") AND ldt_end_org >= DATE("2004-01-01")) OR (ldt_end_date < DATE("2004-01-01") AND ldt_end_org < DATE("2004-01-01")) THEN
					//no furthur work required
				END IF 
				
				IF ldt_end_org < DATE("2004-01-01") AND ldt_end_date >= DATE("2004-01-01") THEN
				  
					idw_dw[4].setitem(li_counter,"eligibility_end_date",DATE("2003-12-31"))
					
					//create the comment
					ls_comment = "End Date modified: " + string(ldt_end_org,"yyyy-mm-dd") + " To " + string(ldt_end_date,"yyyy-mm-dd")
					
				   //insert split new record
					ll_row = idw_dw[4].insertrow(0)
				
					/* make sure we have a valid row */
					IF ISNULL(ll_row) OR ll_row < 0 THEN RETURN -1
				
				   setnull(ldt_null_date)
					idw_dw[4].setitem(ll_row,"claim_no",ll_claim_no)
					idw_dw[4].setitem(ll_row,"eligibility_start_date",DATE("2004-01-01"))
					idw_dw[4].setitem(ll_row,"eligibility_end_date",ldt_end_date)
					idw_dw[4].setitem(ll_row,"comment",ls_comment)
					idw_dw[4].setitem(ll_row,"manual_entry_flag","Y")
					idw_dw[4].setitem(ll_row,"export_no",0)
					idw_dw[4].setitem(ll_row,"export_user_id",'')
					idw_dw[4].setitem(ll_row,"export_date",ldt_null_date) 
				END IF
			END IF 
			
		CASE 4//newmodified! (add)

			//grab the values we need to look at eligibility_start and _end date
	      ldt_start_date = DATE(idw_dw[4].GetItemDatetime(li_counter,"eligibility_start_date"))
			IF ISNULL(ldt_start_date) THEN RETURN -1//should never happen basic edit check
			
			ldt_end_date = DATE(idw_dw[4].GetItemDatetime(li_counter,"eligibility_end_date"))
			
			IF ldt_start_date >= date("2004-01-01") OR ldt_end_date < date("2004-01-01") THEN CONTINUE
			
		   IF ldt_start_date < date("2004-01-01") AND  ldt_end_date >= date("2004-01-01") THEN 
				
				//set the original date back 
				idw_dw[4].setitem(li_counter,"eligibility_end_date",DATE("2003-12-31"))
				
				//create the comment
				ls_comment = "Create Auto Eligibility: " + string(ldt_start_date,"yyyy-mm-dd") + " To " + string(ldt_end_date,"yyyy-mm-dd")
					
				//insert split new record
				ll_row = idw_dw[4].insertrow(0)
				
				/* make sure we have a valid row */
				IF ISNULL(ll_row) OR ll_row < 0 THEN RETURN -1
				
				/* grab the last_eligibility_record_no on the save */		
				setnull(ldt_null_date)
   			idw_dw[4].setitem(ll_row,"claim_no",ll_claim_no)
				idw_dw[4].setitem(ll_row,"eligibility_start_date",DATE("2004-01-01"))
				idw_dw[4].setitem(ll_row,"eligibility_end_date",ldt_end_date)
				idw_dw[4].setitem(ll_row,"comment",ls_comment)
				idw_dw[4].setitem(ll_row,"manual_entry_flag","Y")
				idw_dw[4].setitem(ll_row,"export_no",0)
				idw_dw[4].setitem(ll_row,"export_user_id",'')
				idw_dw[4].setitem(ll_row,"export_date",ldt_null_date) 
				
			END IF 		
	END CHOOSE 
NEXT

RETURN 1
end function

public function integer nf_validate_eligibility ();LONG     ll_claim_no, ll_eligibility_no, ll_export_no, ll_count,  ll_count_rows, x, li_count2
DATE     ldt_current, ldt_exported_termination_date, ldt_check_start, ldt_check_end
DATE     ldt_eligibility_start, ldt_eligibility_end, ldt_original_start, ldt_original_end
DATE     ldt_end_check,ldt_date_check, ldt_eligibility_start_check, ldt_next_day
INTEGER  li_response, li_check, li_rowcount, li_counter, li_count, li_counter_2
STRING   ls_comment, ls_char
BOOLEAN  lb_check
DATETIME ldtm_death_date, ldtm_accident_date, ldtm_min_formulary_start
dwItemStatus l_status

//grab the current date
ldt_current = DATE(f_server_datetime())

/* check each row for its status either newmodified or datamodified */
li_rowcount = idw_dw[4].rowcount()
IF ISNULL(li_rowcount) OR li_rowcount < 1 THEN RETURN 1

FOR li_counter = 1 TO li_rowcount
	
	 // grab the current status
	 l_status = idw_dw[4].GetItemStatus(li_counter, 0, Primary!)
	 
	 //only check if modified or newmodified
	 CHOOSE CASE l_status
		 CASE newmodified!
			 li_check = 1
		 CASE datamodified!
			 li_check = 2
		 CASE ELSE
			 li_check = 0
	 END CHOOSE
	 

	 //ELSE - DO THE CHECKS
	 ll_claim_no           = idw_dw[4].GetItemnumber(li_counter,'claim_no')
	 ldt_eligibility_start = DATE(idw_dw[4].GetItemdatetime(li_counter,'eligibility_start_date'))
	 ldt_eligibility_end   = DATE(idw_dw[4].GetItemdatetime(li_counter,'eligibility_end_date'))
	 ll_eligibility_no     = idw_dw[4].GetItemNumber(li_counter,'eligibility_record_no')
	 ll_export_no          = idw_dw[4].GetItemNumber(li_counter,'export_no')
	 ldt_original_start    = DATE(idw_dw[4].GetItemdatetime(li_counter,'eligibility_start_date',PRIMARY!,TRUE))
	 ldt_original_end      = DATE(idw_dw[4].GetItemdatetime(li_counter,'eligibility_end_date',PRIMARY!,TRUE))
	 ls_comment            = idw_dw[4].GetItemstring(li_counter,'comment')
		
	 /* check and see if this record has been exported  -- should check that NULL = NULL */
	 IF ll_export_no > 0 THEN
		 lb_check = TRUE
	 ELSE
	    lb_check = nf_has_record_been_exported(ll_claim_no,ll_eligibility_no,"ELIGIBILITY")
	 END IF 
		
	 /* Do some reasonable edit checks on the values we will be using to test with */
	 IF ISNULL(ll_claim_no) OR ll_claim_no < 1 THEN
		 messagebox("Validation Error", "Claim Number is invalid")
		 RETURN -1
	 END IF 
	
	 IF ISNULL(ldt_eligibility_start) THEN
		 messagebox("Validation Error", "Eligibility Start Date is invalid: Must Not Be Null")
		 RETURN -1
	 END IF 
	
	 IF ISNULL(ll_export_no) OR ll_export_no < 0 THEN
		 messagebox("Validation Error", "Export Number is invalid")
		 RETURN -1
	 END IF 
	
	 /* 1.90 The termination date of a claim eligibility coverage period must be greater than or 
			   equal to the effective date of the claim eligibility coverage period 
	 */
	 IF NOT ISNULL(ldt_eligibility_end) THEN
		 IF ldt_eligibility_start > ldt_eligibility_end THEN
			 messagebox("Validation Error BR# 1.90","The termination date of a claim eligibility coverage period must " +&
					     "~rbe greater than or equal to the effective date of the claim eligibility coverage period.")
					     RETURN -1
	    END IF
    END IF
	
	 //comment must be at least three characters
	 IF ISNULL(ls_comment) OR TRIM(ls_comment) = "" OR LEN(TRIM(ls_comment)) < 3 THEN
		 messagebox("Validation Error", "A valid comment must be entered and must be at least three characters long.")
		 RETURN -1
	 END IF
	 
	 ls_char = LEFT(ls_comment,1) 
	 IF (Asc(ls_char) >= Asc("a") AND Asc(ls_char) <= Asc("z")) OR (Asc(ls_char) >= Asc("A") AND Asc(ls_char) <= Asc("Z")) THEN
		 //continue
	 ELSE
		 messagebox("Validation Error", "A valid comment must be entered and must not start with a blank space.")
		 RETURN -1
	 END IF
	
	 //start date cannot be null
	 IF ISNULL(ldt_original_start) THEN 
		 messagebox("Validation Error", "Invalid Start Date")
		 RETURN -1
	 END IF
	
	 /* 1.60	The effective date of a claim eligibility coverage period must be greater than 
				or equal to January 01, 1997
	 */
	 IF ldt_eligibility_start < date("1997-01-01") THEN
		 messagebox("Validation Error BR# 1.60","The effective date of a claim eligibility coverage period " +&
					"~rmust be greater than or equal to January 01, 1997")
		 RETURN -1
	 END IF 
	
	 /* 1.100  The termination date of a claim eligibility coverage period must not be 
				more than five years in the future if the date is not open-ended
	 */
	 IF NOT ISNULL(ldt_eligibility_end) THEN
		
		 ldt_date_check = nf_get_new_date("FIVE_YEARS")
		 IF ldt_eligibility_end > ldt_date_check THEN
			 messagebox("Validation Error BR# 1.100","The termination date of a claim eligibility coverage period must not be " +& 
							  "~rmore than five years in the future if the date is not open-ended")
			 RETURN -1
		 END IF
	 END IF 
	 						
	 /* 1.65  The effective date for a claim eligibility coverage that has not been set 
	          up on Blue Cross must be greater than or equal to the claim accident date
	 */
	 IF lb_check = FALSE THEN 
		
		SELECT accident_date 
		  INTO :ldtm_accident_date
		  FROM CLAIM
		 WHERE claim_no = :ll_claim_no
		 USING SQLCA;
		 
	   SQLCA.nf_handle_error('n_maintain_formulary','nf_validate_eligibility()','SELECT accident_date ... BR1.65') 
	
	   IF ldt_eligibility_start < DATE(ldtm_accident_date) THEN
			 messagebox("Validation Error BR 1.65","The effective date for a claim eligibility coverage that has not been set "+&
	                  "~rup on Blue Cross must be greater than or equal to the claim accident date") 
			 RETURN -1	
		END IF 
	 END IF 
	
	 //If it's <> 1 or 2 then continue
	// messagebox("before continue","")
	 IF li_check = 0 THEN CONTINUE
	// messagebox("after continue","")
	 
	 /* check and see if it is just the comment that has been updated...if so return 1
	    no furthur checks are needed
	 */
	 IF idw_dw[4].GetItemstatus(li_counter,'eligibility_start_date',PRIMARY!) = notmodified! &
	    AND idw_dw[4].GetItemstatus(li_counter,'eligibility_end_date',PRIMARY!) = notmodified! &
	    AND idw_dw[4].GetItemstatus(li_counter,'comment',PRIMARY!) = datamodified! THEN
	  		 CONTINUE
	 END IF 
	 
	 /* 5.30	The effective date of a formulary coverage period that has not been set up on Blue Cross 
		   must be greater than or equal to the minimum effective date of all of the claim eligibility 
		   for the claim
	*/
	SELECT min(formulary_start_date)
	  INTO :ldtm_min_formulary_start
	  FROM CLAIM_FORMULARY a
	 WHERE claim_no  = :ll_claim_no
		AND export_no = 0
		AND NOT EXISTS (SELECT * 
								FROM CLAIM_FORMULARY_HISTORY b
							  WHERE a.claim_no            = b.claim_no
		                   AND a.formulary_record_no = b.formulary_record_no
			                AND b.export_no           > 0)

    USING SQLCA;
		
	 SQLCA.nf_handle_error('n_maintain_formulary','nf_validate_eligibility()','SELECT... - BR5.30')
	
	 IF NOT ISNULL(ldtm_min_formulary_start) THEN
			ldt_eligibility_start_check = DATE(idw_dw[4].GetItemdatetime(1,'eligibility_start_date'))
			
			li_check = 0
			FOR li_counter_2 = 1 TO idw_dw[4].rowcount()
					ldt_eligibility_start_check = DATE(idw_dw[4].GetItemdatetime(li_counter_2,'eligibility_start_date'))
					
					IF DATE(ldtm_min_formulary_start) >= ldt_eligibility_start_check THEN 
						li_check = 1
						EXIT
					END IF 
			NEXT
			
			IF li_check = 0  THEN 
					messagebox("Validation Error BR# 5.30","The effective date of a formulary coverage period must be greater "+&
						  "~rthan or equal to the minimum effective date of the claim eligibility.")
			      RETURN -1
		  END IF
	 END IF 
		
	 //grab the exported termination date - It could be NULL...which is valid
	 IF ll_export_no > 0 THEN
		 ldt_exported_termination_date = ldt_original_end
	 ELSE	
	    ldt_exported_termination_date = nf_get_termination_date(ll_claim_no,"ELIGIBILITY",ll_eligibility_no)
    END IF 
	// MESSAGEBOX("exported termination date",string(ldt_exported_termination_date))
	 
	 /*
	 1.10	Claim eligibility coverage for a claim must not overlap with any other claim eligibility coverage period 
	 ..."Coverage must not overlap" means: The effective date must not be {greater than or equal to the effective date 
		  and less than or equal to the termination date} of any other claim eligibility coverage periods and the 
		  termination date must not be {greater than or equal to the effective date and less than or equal to the 
		  termination date} of any other claim eligibility coverage periods 
	 */	 
	 IF ISNULL(ldt_eligibility_end) THEN 
		 ldt_end_check = DATE("2078-01-01")
	 ELSE
		 ldt_end_check = ldt_eligibility_end
	 END IF
	 
	 SELECT count(*) INTO :ll_count 
	   FROM CLAIM_ELIGIBILITY  
	  WHERE claim_no               = :ll_claim_no
		 AND eligibility_record_no <> :ll_eligibility_no
		 AND ((eligibility_start_date BETWEEN :ldt_eligibility_start AND :ldt_end_check )
		  OR (eligibility_end_date    BETWEEN :ldt_eligibility_start AND :ldt_end_check )
		  OR (eligibility_start_date > :ldt_eligibility_start AND eligibility_end_date < :ldt_end_check)
		  OR (:ldt_eligibility_start BETWEEN eligibility_start_date AND eligibility_end_date))
	  USING SQLCA;	 
		 
	 SQLCA.nf_handle_error('n_maintain_formulary','nf_validate_eligibility()','SELECT count(*)... A- BR1.10') 
	
	 FOR li_counter_2 = 1 TO idw_dw[4].rowcount()
			
		 IF li_counter_2 = li_counter THEN CONTINUE
		
		 ldt_check_start = DATE(idw_dw[4].GetItemdatetime(li_counter_2,'eligibility_start_date'))
       ldt_check_end   = DATE(idw_dw[4].GetItemdatetime(li_counter_2,'eligibility_end_date'))
		  
		 IF ISNULL(ldt_check_end) THEN ldt_check_end = DATE("2078-01-01")
		   
		 IF ldt_eligibility_start >= ldt_check_start AND ldt_eligibility_start <= ldt_check_end THEN 
			 ll_count = 1
		 END IF 
		 IF ldt_eligibility_start <= ldt_check_start AND ldt_eligibility_start >= ldt_check_end THEN 
			 ll_count = 1
		 END IF 
		
		 //CHECK RULE 1.20 HERE AS WELL
		 IF ldt_eligibility_end > ldt_current OR ISNULL(ldt_eligibility_end) THEN 
				IF ldt_check_end > ldt_current OR ISNULL(ldt_check_end) THEN ll_count = 2
		 END IF  
	 NEXT
	
	 IF ll_count = 1 THEN
		 messagebox("Validation Error BR# 1.10","Claim eligibility coverage must not overlap any other claim eligibility coverage period.")
		 RETURN -1
	 END IF 
	 
	 IF ll_count = 2 THEN
		 messagebox("Validation Error BR# 1.20","A claim must have only one claim eligibility coverage period" +&
															  "~rwith a termination date that is open-ended or is in the future.")
		 RETURN -1
	 END IF 
	 
	 /*
	 1.20	A claim must must have only one claim eligibility coverage period with a termination date 
		 	that is open-ended or is in the future 
			...This is a Reasonable Edit check that prevents the user from creating more than one 
			coverage period for claim eligibility that has a termination date in the future. It is not a 
			reasonable business need to create two coverage periods for a formulary that both are future dates.
	 */
	 IF ldt_eligibility_end > ldt_current OR isnull(ldt_eligibility_end) THEN
		
		 SELECT count(*) INTO :ll_count 
		   FROM CLAIM_ELIGIBILITY  
		  WHERE claim_no               = :ll_claim_no
			 AND eligibility_record_no <> :ll_eligibility_no
			 AND (eligibility_end_date > :ldt_current OR eligibility_end_date IS NULL)
		  USING SQLCA;
			 
		  SQLCA.nf_handle_error('n_maintain_formulary','nf_validate_eligibility()','SELECT count(*) - BR1.20') 
	
		 IF ll_count > 0 THEN
			 messagebox("Validation Error BR# 1.20","A claim must have only one claim eligibility coverage period" +&
															  "~rwith a termination date that is open-ended or is in the future.")
			 RETURN -1
		 END IF 
	 END IF 

	 /*
	 1.30	A claim eligibility coverage period must not be deleted if the claim eligibility 
			coverage period has been exported to Bluecross 
			...The user may delete a claim eligibility coverage period that was created as long as they 
			delete it before it gets exported to Bluecross
			
	 DEVELOPERS NOTE: This will be handled from within the calling code. 
						  I believe there is a delete button that will call the delete code
						  most likely a delete function. nf_delete_eligibility()
	 */

	 /*
	 1.40	A claim eligibility coverage period must not be updated under the following conditions:
	 ·	The claim is registered AND
	 ·	the claim status is not one of the following statuses
	 o	Active or Finalled (Finalled-Final, Finaled-First&Final, Finaled-LostTime Medical Aid Only 
		or Finalled-No Lost Time)
	 */
	 IF ldt_eligibility_start <> ldt_original_start OR ldt_eligibility_end <> ldt_original_end THEN
		
		 SELECT count(*) INTO :ll_count 
		   FROM CLAIM_ELIGIBILITY a , CLAIM b, X001_REGISTRATION c
		  WHERE a.claim_no   = :ll_claim_no
			 AND a.claim_no   = b.claim_no
			 AND a.claim_no   = c.claim_no
			 AND NOT (b.claim_status_code = 'A' OR (b.claim_status_code = 'F' AND b.claim_status_type_code IN('01','02','03','04')))
		  USING SQLCA;
			 
		  SQLCA.nf_handle_error('n_maintain_formulary', 'nf_validate_eligibility()', 'SELECT count(*) INTO :ll_count - BR1.40') 
		
		 IF ll_count > 0 THEN
			 messagebox("Validation Error BR 1.40","A claim eligibility coverage period cannot be updated " +&
						  "~rfor a claim that is not Active or Finalled.")
			 RETURN -1
		 END IF 
	 END IF 

	 /*
	 1.50	A claim eligibility coverage period must not be added under either of the following conditions:
	 ·	The claim is not registered OR
	 ·	The claim is registered and the claim status is not one of the following statuses
	 o	Active or Finalled (Finalled-Final, Finaled-First&Final, Finaled-LostTime Medical Aid Only or Finalled-No Lost Time)
	 */
	 IF li_check = 1 THEN 
		 SELECT count(*) INTO :ll_count 
		   FROM CLAIM_ELIGIBILITY a , CLAIM b
		  WHERE a.claim_no   = :ll_claim_no
			 AND a.claim_no   = b.claim_no
			 AND NOT (b.claim_status_code = 'A' OR (b.claim_status_code = 'F' AND b.claim_status_type_code IN('01','02','03','04')))
			 AND EXISTS (SELECT * FROM X001_REGISTRATION c WHERE a.claim_no = c.claim_no)
		  USING SQLCA;
			 
		  SQLCA.nf_handle_error('n_maintain_formulary','nf_validate_eligibility()','SELECT count(*) - BR1.50 A') 
		
		 IF ll_count > 0 THEN
			 messagebox("Validation Error BR# 1.50","A claim eligibility coverage period cannot be added for a claim status that is not Active or Finalled.")
			 RETURN -1
		 END IF 
		
		 SELECT count(*) INTO :ll_count 
			FROM X001_REGISTRATION c WHERE c.claim_no = :ll_claim_no
		  USING SQLCA;
			 
		 SQLCA.nf_handle_error('n_maintain_formulary','nf_validate_eligibility()','SELECT count(*) - BR1.50 B') 
		
		IF ISNULL(ll_count) OR ll_count = 0 THEN
			messagebox("Validation Error BR# 1.50","A claim Eligibility Coverage period cannot be added unless the claim is registered")
			RETURN -1
		END IF 
	 END IF 
	
	 /* $$$$$$$$$$$$$$$$$ EFFECTIVE DATE RULE S$$$$$$$$$$$$$$$$$$$$ */
	
	 IF lb_check = FALSE THEN
	
	 /* 1.70	The effective date for a claim eligibility coverage that has not been 
	         set up on Blue Cross must not more than one month in the future
				REVISED: MUST NOT BE GREATER THAN THE CURRENT DATE
	 */
		 IF ldt_eligibility_start > ldt_current THEN
			 messagebox("Validation Error BR# 1.70","The effective date for a claim eligibility coverage can not be greater than the current date.")
			 RETURN -1
		 END IF 			
	 END IF 
	
	 /* 1.80 The effective date for a claim eligibility coverage that has been set up on Blue Cross 
	          must be be less than or equal to the effective date on Blue Cross 
				  
		...When the effective date is revised for a coverage period that has already been exported, 
			the user must only change it to a date that is earlier than the effective date that was 
			last exported. The reason for this is that since the coverage record was already 
			exported to Bluecross, a Rx may have been processed for that effective date. 
			For example: If a claim has a coverage period of Feb 20, 2005 to Sept 1, 2005, 
			The user cannot revise the effective date later on to Feb 21, 2005 as the claimant may 
			have had a Rx processed for Feb 20, 2005.
						
		 *********************SEE RULE 1.10*********************
		 
		 -- the ldtm_original_start should be correct as it is what we use to determine if it has been exported
	  */
	 IF lb_check = TRUE THEN
		 IF (ldt_eligibility_start > ldt_original_start) THEN
			 messagebox("Validation Error BR# 1.80","The effective date must be less than or equal to "+String(ldt_original_start,"yyyy-mm-dd")+".")
			 RETURN -1
		 END IF 
	 END IF 
	
	/* $$$$$$$$$$$$$$$$$ TERMINATION DATE RULES S$$$$$$$$$$$$$$$$$$$$ */

	/* 1.110  The termination date of a claim eligibility coverage period should not 
				 be more than one year in the future if the date is not open-ended 
	*/
	
	ldt_date_check = nf_get_new_date("YEAR")
	
	IF ldt_eligibility_end > ldt_date_check THEN
	//IF ldt_eligibility_end > Date(Year(ldt_current) + 1, Month(ldt_current), Day(ldt_current)) THEN
		li_response = messagebox("Validation Error BR# 1.110"," The termination date of a claim eligibility coverage period should not "+&
								"~rbe more than one year in the future if the date is not open-ended. Proceed?", question!,yesno!)
					
		IF li_response = 2 THEN RETURN -1
	END IF 
			
	/* 1.120 The termination date of a claim eligibility coverage period must be 
	         less than or equal to the individual's death date under either of the following conditions:
		1.	the claim eligibility coverage has not been set up on Blue Cross:
			a.	the individual is deceased
		2.	the claim eligibility coverage has been set up on Blue Cross:
			a.	the individual is deceased and
			b.	the termination date on Blue Cross is the current date or is in the past and
			c.	the death date is greater than the termination date on Blue Cross
	*/
	
	SELECT death_date INTO :ldtm_death_date
	  FROM CLAIM a, INDIVIDUAL b
	 WHERE a.claim_no      = :ll_claim_no
		AND a.individual_no = b.individual_no
	 USING SQLCA;
				 
	 SQLCA.nf_handle_error('n_maintain_formulary','nf_validate_eligibility()','SELECT death_date BR1.120')
				
	IF lb_check = FALSE THEN
		IF NOT ISNULL(ldtm_death_date) THEN
			IF ldt_eligibility_end > DATE(ldtm_death_date) OR isnull(ldt_eligibility_end) THEN
				messagebox("Validation Error BR# 1.120","The Termination Date of a claim eligibility coverage period " +&
	                    "~rmust be less than or equal to the individual's death date of "+String(ldtm_death_date,"yyyy-mm-dd"))
				RETURN -1
			END IF
		END IF 
	ELSE
		IF NOT ISNULL(ldtm_death_date) THEN
			IF ldt_exported_termination_date <= ldt_current AND DATE(ldtm_death_date) > ldt_exported_termination_date THEN
				IF (ldt_eligibility_end > DATE(ldtm_death_date) OR ISNULL(ldt_eligibility_end)) THEN
				messagebox("Validation Error BR# 1.120","The Termination Date of a claim eligibility coverage period " +&
	                    "~rmust be less than or equal to the individual's death date of "+String(ldtm_death_date,"yyyy-mm-dd"))
				RETURN -1
			END IF
			END IF 
		END IF 
	END IF 
		
   /* 1.130 SEEMS TO HAVE BEEN COMBINED INTO 1.120 */
			
	/* 1.140	The termination date of a claim eligibility coverage period must be greater than or
	         equal to the termination date on Blue Cross under the following conditions:
            ·	the claim eligibility coverage has been set up on Blue Cross
            ·	the termination date on Blue Cross is the current date or is in the past
		
				...You cannot enter a termination date that is before the existing termination date, 
				if the existing termination date has already expired. The reason for this is that the 
				claim could have had a prescription during that time period.  For example: If the coverage 
				has expired and was from January 1, 2005 to May 30, 2005 you cannot enter a new termination 
				date of May 26, 2005 as the claim may have had a prescription up until May 30th. 
	*/
	IF lb_check = TRUE THEN
		IF NOT ISNULL(ldt_eligibility_end) THEN
			IF ldt_exported_termination_date <= ldt_current THEN 
				IF ldt_eligibility_end < ldt_exported_termination_date THEN 
					messagebox("Validation Error BR# 1.140","The termination date of a claim eligibility coverage period that has been exported "+&
								  "~rmust be greater than or equal to "+String(ldt_exported_termination_date,"yyyy-mm-dd"))
					RETURN -1
					
				END IF 
			END IF 
		END IF 
	
		/* 1.150	(REVISED) The termination date of a claim eligibility coverage period 
		   must be equal to the date of the next scheduled Bluecross Eligibility Process run 
			under the following conditions:
			
			· the claim eligibility coverage has been set up on Blue Cross
			· the termination date on Blue Cross is in the future or is open-ended
			· the individual is deceased
			
			...You must not terminate coverage prior to the next eligibility file processing on 
				Bluecross, if the existing coverage has not yet expired. The reason for this rule 
				is that a claim with a non-expired coverage period continues to have Rx coverage 
				up until the Bluecross system applies the change to the termination date. Thus, 
				the earliest date that you can terminate a non-expired coverage is the date of 
				the Bluecross update. The eligibility export file that we send to Bluecross gets 
				processed after we export it at 8:00 pm at night.
				
			1.155 (NEW) The termination date of a claim eligibility coverage period must be greater 
			than or equal to the date of the next scheduled Bluecross Eligibility Process run under 
			the following conditions:
			
			· the claim eligibility coverage has been set up on Blue Cross
			· the termination date on Blue Cross is in the future or is open-ended
			· the individual is not  deceased	
				
		*/
		
		ldt_next_day = nf_get_abcc_work_day()	
				
		 IF ISNULL(ldt_next_day) Then
			 MessageBox('ABCC Calendar','The ABCC Calendar is not loaded. Please notify helpdesk.')
			 RETURN -1
		 END IF	
		 
		// messagebox("next bc day", string(ldt_next_day))
		// messagebox("exported termination date", string(ldt_exported_termination_date))
		// messagebox("eligibility end date", string(ldt_eligibility_end))
			 
		 IF ISNULL(ldt_exported_termination_date) OR ldt_exported_termination_date > ldt_current THEN
				IF NOT ISNULL(ldtm_death_date) THEN 
					IF ldt_eligibility_end <> ldt_next_day OR isnull(ldt_eligibility_end)  THEN
						messagebox("Validation Error BR# 1.150","The termination date of this claim eligibility coverage period that has been exported " +&
								"~rmust be equal to "+String(ldt_next_day,"yyyy-mm-dd"))
							RETURN -1
					END IF
				ELSE	
					//messagebox("should hit this", string(ldt_eligibility_end) + " < " + string(ldt_next_day) )
					IF ldt_eligibility_end < ldt_next_day THEN
						messagebox("Validation Error BR# 1.155","The termination date of this claim eligibility coverage period " +&
								"~rmust be greater than or equal to "+string(ldt_next_day,"yyyy-mm-dd"))
							RETURN -1
					END IF	
				END IF 
		  END IF
	END IF 

	/*
   1.160 The user must be warned that the claim eligibility coverage has expired and formulary coverage 
	      is still open under the following conditions:
         ·	All of the claim eligibility coverage periods for the claim are expired and at least one 
			   formulary coverage period has not expired
				
			...Claim Eligibility Coverage supercedes formulary coverage when it comes to determining whether 
			or not a DIN is accepted for coverage; For example: if the user enters a claim eligibility termination 
			date of June 1, 2005 but the claim still has a formulary coverage period that does not expired until 
			Sept 2005, the claim will not be covered for the DINs in that formulary as the claim's eligibility 
			period is expired. This is only a warning message as there may be cases where they want to temporarily 
			terminate the claim without making the formulary coverage changes. It is also valid to have a claim 
			eligibility coverage that is not expired while having all formulary coverage for a claim expired. 
			This typically occurs when the DIN coverage is being handled exclusively through Special Drug 
			Authorizations and not through any formularies - the claim eligibility must be open if the SDA 
			is to be covered. SDA's are entered exclusively on the Bluecross System.			
	*/	
         
	IF ISNULL(ldt_eligibility_end) OR ldt_eligibility_end > ldt_current THEN
		
		 SELECT count(*) INTO :li_count
			FROM CLAIM_FORMULARY
		  WHERE claim_no           = :ll_claim_no
			 AND (formulary_end_date is null
			  OR formulary_end_date >= :ldt_current)
		  USING SQLCA;
						 
		SQLCA.nf_handle_error('n_maintain_formulary','nf_validate_eligibility()','SELECT - BR1.160B')

		//default to 0 for NULL
		IF ISNULL(li_count) THEN li_count = 0
		
		//PR7439
		
		ll_count_rows = idw_dw[1].Rowcount()
		FOR x = 1 to ll_count_rows
			IF IsNull(idw_dw[1].getitemdatetime(x,"formulary_end_date")) OR Date(idw_dw[1].getitemdatetime(x,"formulary_end_date"))>= ldt_current THEN
				li_count2 ++
			END IF
		NEXT	
	
		//If we have some open tell them
		IF li_count = 0 AND li_count2 = 0 THEN
	   		messagebox("Informational Warning BR# 5.20","The Eligibility Coverage is open without an open Formulary Coverage period.",information!)
		END IF 

	
	ELSE			
	// see if there is a null value
		 SELECT count(*) INTO :li_count
			FROM CLAIM_ELIGIBILITY
		  WHERE claim_no             = :ll_claim_no
			 AND (eligibility_end_date is null
			  OR eligibility_end_date >= :ldt_current)
			 AND eligibility_record_no <> :ll_eligibility_no
		  USING SQLCA;
					 
		SQLCA.nf_handle_error('n_maintain_formulary','nf_validate_eligibility()','SELECT - BR1.160A')
				
			//default to 0 for NULL
			IF ISNULL(li_count) THEN li_count = 0
				
			//if li_count = 0 THEN make sure we don't have any open formularies
			IF li_count = 0 THEN 
				
			 SELECT count(*) INTO :li_count
				FROM CLAIM_FORMULARY
			  WHERE claim_no           = :ll_claim_no
				 AND (formulary_end_date is null
				  OR formulary_end_date >= :ldt_current)
			  USING SQLCA;
						 
			SQLCA.nf_handle_error('n_maintain_formulary','nf_validate_eligibility()','SELECT - BR1.160B')
					
			//default to 0 for NULL
			IF ISNULL(li_count) THEN li_count = 0
					
			//If we have some open tell them
			IF li_count > 0 THEN

   			messagebox("Informational Warning","The Formulary Coverage is open without an open Claim Eligibility Period.", information!)
   		
			END IF 
   	END IF 
	END IF 
		
	/* 
	1.170 The claim eligibility coverage period must be split into two separate coverage periods 
	- one coverage for the period from the effective date to December 31, 2003 and the other 
	  coverage period from January 01, 2004 until the termination date - 
	  under the following conditions:
	·	the effective date that the user requests  is less than 2004.01.01 
	·	the termination date that the user requests is greater than or equal to  2004.01.01
		
			DONE IN THE CODE 
	*/
	
	/*
	1.180 A new claim eligibility coverage period must be created under the following conditions
   ·	the effective date is revised for a claim eligibility coverage that has been set up on Blue Cross
	
			DONE IN THE CODE 
   */	
	
NEXT

RETURN 1
end function

public function string nf_check_if_active_formulary (string as_formulary_code);STRING	ls_active_flag

select active_flag
into   :ls_active_flag
from   Formulary
where  formulary_code = :as_formulary_code
using SQLCA;

SQLCA.nf_handle_error('n_maintain_formulary','nf_check_if_active_formulary()','select active_flag...') 

RETURN ls_active_flag

end function

public function date nf_get_accident_date ();w_sheet      lw_active_sheet
DATE			ldt_accident_date

lw_active_sheet	= w_frame.GetActiveSheet()
ldt_accident_date = Date(lw_active_sheet.dw_basic_claim.GetItemDateTime(1,"accident_date"))

IF ISNULL(ldt_accident_date) THEN ldt_accident_date = Date('1900-01-01')

RETURN ldt_accident_date
end function

public function string nf_get_last_name ();w_sheet      lw_active_sheet
STRING		ls_last_name

lw_active_sheet	= w_frame.GetActiveSheet()
ls_last_name = lw_active_sheet.dw_basic_claim.GetItemString(1,"last_name")

RETURN ls_last_name
end function

public function string nf_get_given_names ();w_sheet      lw_active_sheet
STRING		ls_given_names

lw_active_sheet	= w_frame.GetActiveSheet()
ls_given_names = lw_active_sheet.dw_basic_claim.GetItemString(1,"given_names")

RETURN ls_given_names
end function

public function long nf_get_individual_no ();w_sheet      lw_active_sheet
LONG         ll_individual_no

lw_active_sheet	= w_frame.GetActiveSheet()
ll_individual_no		= lw_active_sheet.dw_basic_claim.GetItemNumber(1,"individual_no")

IF ISNULL(ll_individual_no) OR ll_individual_no < 1 THEN ll_individual_no = 0

RETURN ll_individual_no
end function

public function integer nf_add_drug_alert ();LONG ll_row, ll_claim_no, ll_individual_no, ll_alerts
DATE ldt_null_date, ldt_current_date

ldt_current_date = DATE(f_server_datetime())

il_claim_no = nf_get_claim_no()

IF IsNull(il_claim_no) OR il_claim_no < 0 THEN RETURN -1

il_individual_no = nf_get_individual_no()

IF IsNull(il_individual_no) OR il_individual_no < 0 THEN RETURN -1

ll_row = idw_dw[12].InsertRow(0)
il_current_row = ll_row

idw_dw[12].SetRow(il_current_row)
idw_dw[12].ScrollToRow(il_current_row)
idw_dw[12].SelectRow(il_current_row, TRUE)

IF IsNull(ll_row) OR ll_row < 0 THEN RETURN -1

SetNull(ldt_null_date)

idw_dw[12].SetItem(ll_row,'individual_no',il_individual_no)
idw_dw[12].SetItem(ll_row,'drug_alert_type_code', '')
idw_dw[12].SetItem(ll_row,'effective_date',ldt_current_date)
idw_dw[12].SetItem(ll_row,'alert_comment','')
idw_dw[12].SetItem(ll_row,'terminated_comment','')
idw_dw[12].SetItem(ll_row,'terminated_date', ldt_null_date)
idw_dw[12].SetItem(ll_row,'terminated_user_id','')

idw_dw[12].SetColumn('drug_alert_type_code')
idw_dw[12].Object.DataWindow.HorizontalScrollPosition=1
idw_dw[12].SetFocus()

RETURN 0
end function

public function integer nf_add_individual_alert_event (long al_row);DATASTORE lds_event
n_event_log lnv_event
LONG ll_row, ll_event_no, ll_alert_no
DATE ld_current_date, ldt_terminated
STRING ls_event_type, ls_event_specific, ls_comment, ls_comment2, ls_event_desc, ls_alert_type

ld_current_date = DATE(f_server_datetime())

lds_event = CREATE DATASTORE
lds_event.DataObject = 'd_alert_event'
lds_event.SetTransObject(SQLCA)

lnv_event = CREATE n_event_log

IF idw_dw[12].GetRow() > 0 THEN
	ls_alert_type = idw_dw[12].GetItemString(al_row, 'drug_alert_type_code')
	IF IsNull(ls_alert_type) OR ls_alert_type = '' THEN
		MessageBox('Error','There was a problem retrieving the Drug Alert Type.', Information!)
		RETURN -1
	END IF
	
	IF nf_get_event_info(ls_alert_type, ls_event_type, ls_event_desc) < 0 THEN
		MessageBox('Error','There was a problem retrieving the event information.', Information!)
		RETURN -1
	END IF
	
	ll_alert_no = idw_dw[12].GetItemNumber(al_row, 'drug_alert_no')
	
	IF ll_alert_no <= 0 THEN
		MessageBox('Error','There was a problem getting the Drug Alert Number.', Information!)
		RETURN -1
	END IF
	
	IF idw_dw[12].GetItemStatus(al_row, 0, PRIMARY!) = New! OR idw_dw[12].GetItemStatus(al_row, 0, PRIMARY!) = NewModified! THEN
		ls_event_specific = 'N' 
		ls_comment = 'Drug Alert #' + STRING(ll_alert_no) + ' for claim ' + STRING(il_claim_no) + ' was created on '  +  STRING(ld_current_date,'yyyy-mm-dd') +  ' by ' +  STRING(vgst_user_profile.user_id)  + '.'
		ls_comment2 = idw_dw[12].GetItemString(al_row, 'alert_comment')
		IF IsNull(ls_comment) OR IsNull(ls_comment2) THEN
			MessageBox('Error','There was a problem setting the Event Comment.', Information!)
			RETURN -1
		END IF
		ls_comment = ls_comment + ' ' + ls_comment2
	ELSEIF idw_dw[12].GetItemStatus(al_row, 0, PRIMARY!) = DataModified! THEN
		ldt_terminated = DATE(idw_dw[12].GetItemDateTime(al_row, 'terminated_date'))
		IF NOT IsNull(ldt_terminated) AND ldt_terminated > DATE('1900-01-01') THEN
			ls_event_specific = 'T'
			ls_comment = 'Drug Alert #' + STRING(ll_alert_no) + ' for claim ' + STRING(il_claim_no) + ' was terminated on ' + STRING(ldt_terminated,'yyyy-mm-dd') + ' by ' + & 
								 STRING(vgst_user_profile.user_id) + '.'
			ls_comment2 = idw_dw[12].GetItemString(al_row, 'terminated_comment')
			IF IsNull(ls_comment)  OR IsNull(ls_comment2)  THEN
				MessageBox('Error','There was a problem setting the Event Comment.', Information!)
				RETURN -1
			END IF
			ls_comment = ls_comment + ' ' + ls_comment2
		ELSE
			MessageBox('Error','Unable to determine the Termination Date', Information!)
			RETURN -1
		END IF
	ELSE
		RETURN 0
	END IF
	
	ls_comment = f_clean_string_4(ls_comment)
	
	ll_event_no = lnv_event.nf_next_individual_event_no(il_individual_no)  
	
	IF ll_event_no > 0 THEN
		
		ll_row = lds_event.InsertRow(0)
	
		lds_event.SetItem(ll_row, 'individual_no', il_individual_no)
		lds_event.SetItem(ll_row, 'individual_event_no',ll_event_no)
		lds_event.SetItem(ll_row, 'event_date', ld_current_date)
		lds_event.SetItem(ll_row, 'event_type_code', ls_event_type)
		lds_event.SetItem(ll_row, 'event_specific_code',ls_event_specific)
		lds_event.SetItem(ll_row, 'event_comment', TRIM(ls_comment))
		lds_event.SetItem(ll_row, 'event_category_code', 'I')
	ELSE
		MessageBox('Error','There was a problem getting the next Individual Event number.', Information!)
		RETURN -1
	END IF
	
	lds_event.Update()
	IF SQLCA.nf_handle_error("n_maintain_formulary","nf_add_individual_alert","lds_event.Update()") < 0 THEN
		RETURN -1
	END IF
END IF

RETURN 0
end function

public function integer nf_validate_alert ();LONG ll_rowcount, ll_alerts, ll_row
DATE ldt_terminated
STRING ls_alert_type, ls_alert_comment, ls_term_comment

ll_row = idw_dw[12].GetRow()

ll_rowcount = idw_dw[4].RowCount()
IF ll_rowcount = 0 THEN
	MessageBox('Error','A Drug Alert cannot be created for this claim as it is not registered for drug coverage.',Information!)
	RETURN -1
END IF

ls_alert_type = idw_dw[12].GetItemString(ll_row, 'drug_alert_type_code')
IF IsNull(ls_alert_type) OR ls_alert_type = '' THEN
	MessageBox('Error','The Alert Type must be selected.', Information!)
	RETURN -1
END IF

ls_alert_comment = idw_dw[12].GetItemString(ll_row, 'alert_comment')
IF IsNull(ls_alert_comment) OR TRIM(ls_alert_comment) = '' THEN
	MessageBox('Error','The Alert Comment is required when adding a new Drug Alert.', Information!)
	RETURN -1
END IF

ldt_terminated = DATE(idw_dw[12].GetItemDateTime(ll_row, 'terminated_date'))
ls_term_comment = idw_dw[12].GetItemString(ll_row, 'terminated_comment')
IF NOT IsNull(ldt_terminated) THEN
	IF IsNull(ls_term_comment) OR TRIM(ls_term_comment) = '' THEN
		MessageBox('Error','The Termination Comment is required when terminating a Drug Alert.', Information!)
		RETURN -1
	END IF
END IF


RETURN 0
end function

public function long nf_get_alert_no ();LONG	ll_no

UPDATE Last_Drug_Alert_No 
SET          last_drug_alert_no = last_drug_alert_no + 1 
USING     SQLCA;

SQLCA.nf_handle_error("n_maintain_formulary","nf_get_alert_no()","UPDATE Last_Drug_Alert_No")

IF SQLCA.SQLNRows = 1 THEN  // successful (ie. SQLNRows would equal 1), read back the identifier
	
	SELECT last_drug_alert_no 
	  INTO     :ll_no 
	  FROM   Last_Drug_Alert_No 
	 USING   SQLCA;
	 
	SQLCA.nf_handle_error("n_maintain_formulary","nf_get_alert_no()","SELECT last_drug_alert_no")
	
ELSE // display error
	SQLCA.nf_rollback_transaction()
	
	MessageBox("Data Integrity Error", String(SQLCA.SQLNRows) + " record(s) found in Last_Drug_Alert_No~r~nPlease call the help desk",Exclamation!)
	RETURN -1
END IF

RETURN ll_no
end function

public function integer nf_terminate_drug_alert (long al_row);DATE ldt_today

ldt_today = DATE(f_server_datetime())

il_claim_no = nf_get_claim_no()

IF IsNull(il_claim_no) OR il_claim_no < 0 THEN RETURN -1

il_individual_no = nf_get_individual_no()

IF IsNull(il_individual_no) OR il_individual_no < 0 THEN RETURN -1

idw_dw[12].SetItem(al_row, 'terminated_user_id',vgst_user_profile.user_id)
idw_dw[12].SetItem(al_row, 'terminated_date', ldt_today)

idw_dw[12].SetRow(al_row)
idw_dw[12].ScrollToRow(al_row)
idw_dw[12].SelectRow(al_row, TRUE)
idw_dw[12].SetColumn('terminated_comment')
			
idw_dw[12].Object.DataWindow.HorizontalScrollPosition=idw_dw[12].Object.DataWindow.HorizontalScrollMaximum 
idw_dw[12].SetFocus()

RETURN 0
end function

public function integer nf_get_event_info (string as_alert_type, ref string as_event_type, ref string as_event_desc);STRING ls_event_type

CHOOSE CASE as_alert_type
	CASE 'F'
		as_event_type = '044'
	CASE 'D'
		as_event_type = '045'
	CASE ELSE
		RETURN -1
END CHOOSE

SELECT event_type_desc
INTO       :as_event_desc
FROM     Event_Type
WHERE event_type_code = :as_event_type
USING    SQLCA;

IF SQLCA.nf_handle_error("n_maintain_formulary","nf_get_alert_desc","SELECT event_type_desc") < 0 THEN
	RETURN - 1
END IF

RETURN 0
end function

on destructor;call n_pdc::destructor;	IF IsValid(inv_participant) THEN
		Destroy(inv_participant)
	END IF
	IF IsValid(inv_individual) THEN
		Destroy(inv_individual)
	END IF
end on

on n_maintain_formulary.create
call super::create
end on

on n_maintain_formulary.destroy
call super::destroy
end on

