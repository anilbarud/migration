$PBExportHeader$n_event_log.sru
$PBExportComments$Contains functions to get the next event number and log an automatic event
forward
global type n_event_log from nonvisualobject
end type
end forward

global type n_event_log from nonvisualobject
end type
global n_event_log n_event_log

forward prototypes
public function long nf_next_claim_event_no (long al_claim_no)
public function integer nf_create_auto_event (long al_claim_no, long al_event_no, string as_event_type_code, string as_event_comment, string as_event_specific_code)
public function integer nf_validate_event_category (string as_event_category_code)
public function integer nf_validate_event_type (string as_event_category_code, string as_event_type_code, ref string as_active_flag)
public function integer nf_validate_event_specific_code (string as_event_type_code, string as_event_specific_code, ref string as_generated_method_code, ref string as_active_flag)
public function integer nf_create_auto_individual_event (long al_individual_no, long al_individual_event_no, string as_event_type_code, string as_event_comment, string as_event_specific_code)
public function long nf_next_individual_event_no (long al_individual_no)
end prototypes

public function long nf_next_claim_event_no (long al_claim_no);LONG	ll_result, ll_event_no

/* Function Name: f_next_claim_event_no                                            
	Purpose:       The purpose of this module is to get the next available event number 
	Arguments:     none                                                                                  
	Return Values: The next available event number                                                       

	NOTE:	this function should only be called once per update since it gets the max from the table.  
*/

/*	get the next available event no for the claim
*/
	SELECT Max(event_no)
	  INTO :ll_event_no
	  FROM CLAIM_EVENT
	 WHERE claim_no = :al_claim_no;
	
	ll_result =  SQLCA.nf_handle_error("Embedded SQL: SELECT CLAIM_EVENT","n_event_log","nf_create_auto_event")
	IF ll_result < 0 THEN
		Return ll_result
	END IF
	IF IsNull(ll_event_no) THEN
		ll_event_no = 1
	ELSE
		ll_event_no = ll_event_no + 1
	END IF


Return ll_event_no
end function

public function integer nf_create_auto_event (long al_claim_no, long al_event_no, string as_event_type_code, string as_event_comment, string as_event_specific_code);LONG		ll_result
INTEGER	li_count
DATE		ld_current_date
STRING  ls_clean_string

/*	This function records automatic events

	 Get the current date from the server
*/
ld_current_date = Date(f_server_datetime())

ls_clean_string = f_clean_string_4(as_event_comment)

	INSERT INTO CLAIM_EVENT(claim_no, event_no, event_date, event_type_code,
		event_comment, event_specific_code) VALUES
		(:al_claim_no, :al_event_no, :ld_current_date, :as_event_type_code, 
		:as_event_comment, :as_event_specific_code)
	USING SQLCA;

	ll_result =  SQLCA.nf_handle_error("n_event_log","Embedded SQL: Insert CLAIM_EVENT","nf_create_auto_event")
	IF ll_result < 0 THEN
		Return ll_result
	END IF

	IF SQLCA.SQLNRows <> 1 THEN
		SQLCA.nf_rollback_transaction()
		Return -1
	END IF

Return 0
end function

public function integer nf_validate_event_category (string as_event_category_code);INTEGER     li_count

SELECT Count(*)
INTO   :li_count
FROM   Event_Category
WHERE  event_category_code = :as_event_category_code
USING SQLCA;
SQLCA.nf_handle_error('n_event_log','embedded SQL: SELECT Count(*) FROM Event_Category...','nf_validate_event_category')

RETURN li_count
end function

public function integer nf_validate_event_type (string as_event_category_code, string as_event_type_code, ref string as_active_flag);INTEGER   li_count

SELECT Count(*)
INTO   :li_count
FROM   Event_Type
WHERE  event_category_code = :as_event_category_code
AND    event_type_code     = :as_event_type_code
USING SQLCA;
SQLCA.nf_handle_error('n_event_log','embedded SQL: SELECT Count(*) FROM Event_Type...','nf_validate_event_type')


IF li_count > 0 THEN
	SELECT active_flag
	INTO   :as_active_flag
	FROM   Event_Type
	WHERE  event_category_code = :as_event_category_code
	AND    event_type_code     = :as_event_type_code
	USING SQLCA;
	SQLCA.nf_handle_error('n_event_log','embedded SQL: SELECT active_flag FROM Event_Type...','nf_validate_event_type')
END IF

RETURN li_count
end function

public function integer nf_validate_event_specific_code (string as_event_type_code, string as_event_specific_code, ref string as_generated_method_code, ref string as_active_flag);INTEGER   li_count

SELECT Count(*)
INTO   :li_count
FROM   Event_Specific a
JOIN   Event_Type          b ON a.event_type_code = b.event_type_code
WHERE  a.event_type_code     = :as_event_type_code
AND    a.event_specific_code = :as_event_specific_code
USING SQLCA;
SQLCA.nf_handle_error('n_event_log','embedded SQL: SELECT Count(*) FROM Event_Specific_Code, Event_Type...','nf_validate_event_specific_code')

IF li_count <> 0 THEN
	SELECT b.generated_method_code,
	       a.active_flag
	INTO   :as_generated_method_code,
	       :as_active_flag
	FROM   Event_Specific a
	JOIN   Event_Type          b ON a.event_type_code = b.event_type_code
	WHERE  a.event_type_code     = :as_event_type_code
	AND    a.event_specific_code = :as_event_specific_code
	USING SQLCA;
	SQLCA.nf_handle_error('n_event_log','embedded SQL: SELECT generated_method_code, active_flag FROM Event_Specific_Code, Event_Type...','nf_validate_event_specific_code')
END IF

RETURN li_count
	
end function

public function integer nf_create_auto_individual_event (long al_individual_no, long al_individual_event_no, string as_event_type_code, string as_event_comment, string as_event_specific_code);LONG		ll_result
INTEGER	li_count
DATE		ld_current_date
STRING  ls_clean_string

/*	This function records automatic individual events

	 Get the current date from the server
*/
ld_current_date = Date(f_server_datetime())

ls_clean_string = f_clean_string_4(as_event_comment)

INSERT INDIVIDUAL_EVENT
(      individual_no,
	    individual_event_no,
		 event_date,
		 event_type_code,
		 event_comment,
		 event_specific_code)
VALUES (
       :al_individual_no,
		 :al_individual_event_no,
		 :ld_current_date,
		 :as_event_type_code,
		 :as_event_comment,
		 :as_event_specific_code )
USING SQLCA;

SQLCA.nf_handle_error("n_event_log","Embedded SQL: Insert INDIVIDUAL_EVENT","nf_create_auto_individual_event")

IF SQLCA.SQLNRows <> 1 THEN
	SQLCA.nf_rollback_transaction()
	Return -1
END IF

Return 0
end function

public function long nf_next_individual_event_no (long al_individual_no);// nf_next_individual_event_no
//
LONG	ll_result, ll_individual_event_no

/*	To ensure that we get the next number without, Update the Last_Payment_No table incrementing the 
	last_payment_no by 1  (This will lock it so no one else can get in). Then, read it back          
*/
	UPDATE Last_Individual_Event_No SET last_individual_event_no = last_individual_event_no + 1 using SQLCA;

	ll_result = SQLCA.nf_handle_error("Embedded SQL: Update Last_Individual_Event_No","n_event_log","nf_next_individual_event_no")
	IF ll_result < 0 THEN
		Return -1
	END IF

	CHOOSE CASE SQLCA.SQLNRows
/*	If update was successful (ie. SQLNRows would equal 1), read back the identifier
*/	
		CASE 1
			SELECT Last_Individual_Event_No.last_individual_event_no INTO :ll_individual_event_no FROM Last_Individual_Event_No using SQLCA;
			ll_result = SQLCA.nf_handle_error("Embedded SQL: Select Last_Individual_Event_No","n_event_log","nf_next_individual_event_no")
			IF ll_result < 0 THEN
				Return -1
			END IF
		CASE ELSE
/*		if anything other than 1 record found, display error
*/
		SQLCA.nf_rollback_transaction()
		IF SQLCA.SQLCode <> 0 THEN
			Error.Text = "Error during rollback of Last_Individual_Event_No in function nf_next_individual_event_no"
			Error.WindowMenu="n_event_log"
			Error.Object=""
			Error.ObjectEvent="nf_next_individual_event_no"
			SignalError()
		END IF		
		MessageBox("Payment Module - Data Integrity Error", string(SQLCA.SQLNRows) + " record(s) found in Last_Individual_Event_No~r~nPlease call the help desk",Exclamation!)
		Return -1

	END CHOOSE
		
Return ll_individual_event_no


end function

on n_event_log.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_event_log.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

