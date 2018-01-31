$PBExportHeader$n_br_bluecross.sru
$PBExportComments$Object used to hold general methods for e-pay project
forward
global type n_br_bluecross from nonvisualobject
end type
end forward

global type n_br_bluecross from nonvisualobject
end type
global n_br_bluecross n_br_bluecross

type variables
DataStore	ids_claim_formulary
N_EVENT_LOG inv_event
N_IMAGING	inv_imaging

STRING	is_catname = 'MA - Review'
STRING	is_action = "FR"

end variables

forward prototypes
public function boolean uf_check_if_excluded_claim (long al_claim)
public function boolean uf_check_for_individual_info (long al_claim)
public function boolean uf_claim_is_eligible (long al_claim)
public function integer uf_is_registered (long al_claim_no)
public function integer uf_determine_claim_status (long al_claim, ref string as_claim_status_code, ref string as_claim_status_type)
public function date uf_determine_effective_start_date (long al_claim)
public function boolean uf_determine_penltd (long al_claim, string as_claim_status, string as_claim_type)
public function boolean uf_determine_valid_status (long al_claim, string as_claim_status, string as_claim_type)
public function boolean uf_check_for_death_date (long al_claim)
public function long uf_get_last_eligibility_no ()
public function integer uf_create_eligibility (long al_claim, datetime adtm_start_date, datetime adtm_end_date, string as_comment)
public function integer uf_log_auto_event (long al_claim, string as_event_type_code, string as_event_specific, string as_comment)
public function integer nf_create_formulary_history (long al_claim_no)
public function integer nf_create_eligibility_history (long al_claim_no)
public function integer uf_split_eligibility (long al_claim, date adt_start_date, datetime adt_eligibility_date, datetime adt_today, string as_comment)
public function integer uf_determine_eligibility_setup (long al_claim, datetime adtm_accident_date)
public function integer uf_determine_formulary_setup (long al_claim, datetime adtm_accident_date)
public function integer uf_log_claim_event_for_accdate_change (long al_claim, datetime adt_accident_date)
public function integer uf_create_new_coverage_rec (long al_claim, datetime adt_effective_start, datetime adt_accident_date)
public function date uf_add_3months_today ()
public function integer uf_determine_termination_date (long al_claim, ref date adt_termination_date)
public function boolean uf_check_for_eligibility (long al_claim, string as_claim_status_code, string as_claim_status_type)
public function datetime uf_get_bluecross_next_day ()
public function integer uf_send_doc (long al_claim, long al_event_no)
public function integer uf_log_claim_event_for_noi_change (long al_claim, string as_claim_status, string as_claim_type)
public function integer uf_update_cec_on_adjudication (long al_claim, string as_claim_status, string as_claim_type)
public function integer nf_validate_primary_formulary_status (long al_claim_no, string as_filter)
public function integer uf_update_termdate_for_registered_claims (long al_claim, string as_claim_status, string as_claim_type)
public function string nf_check_if_active_formulary (string as_formulary_code)
public function integer uf_determine_secondary_termination_date (date adt_primary_start_date, date adt_primary_termination_date, string as_primary_formulary_code, string as_secondary_formulary_code, ref date adt_secondary_termination_date)
end prototypes

public function boolean uf_check_if_excluded_claim (long al_claim);// This function determines if the Claim is excluded from Blue Cross Coverage

INT		li_rtn = 0
LONG		ll_count
BOOLEAN 	lb_excluded



SELECT COUNT(*)
INTO   :ll_count
FROM   X001_EXCLUDED_CLAIM
WHERE  claim_no = :al_claim
AND    excluded_flag = 'Y'
USING  SQLCA;

SQLCA.nf_handle_error("n_br_bluecross","uf_check_if_excluded_claim()","select from X001_EXCLUDED_CLAIM")

IF ll_count > 0 THEN
	lb_excluded = TRUE
ELSE
	lb_excluded = FALSE
END IF	



return lb_excluded
end function

public function boolean uf_check_for_individual_info (long al_claim);
// This function checks that all the required individual information is present inorder for the 
// claimiant to be registered for drug coverage.

DATE		ldt_birth_date
STRING	ls_gender, ls_postal_code
BOOLEAN	lb_eligible	= TRUE

SELECT	I.birth_date, I.sex, I.postal_code
INTO		:ldt_birth_date, :ls_gender, :ls_postal_code
FROM		INDIVIDUAL I, CLAIM C
WHERE		C.claim_no = :al_claim
AND   	C.individual_no = I.individual_no
USING		SQLCA;

SQLCA.nf_handle_error("n_br_bluecross","uf_check_for_individual_info()","select from INDIVIDUAL")

IF ISNULL(ldt_birth_date) OR STRING(ldt_birth_date) = '1900-01-01' OR STRING(ldt_birth_date) = '' THEN
	lb_eligible = FALSE
END IF

IF ISNULL(ls_gender) OR ls_gender = '' THEN
	lb_eligible = FALSE
END IF

IF ISNULL(ls_postal_code) OR ls_postal_code = '' THEN
	lb_eligible = FALSE
END IF


RETURN lb_eligible
end function

public function boolean uf_claim_is_eligible (long al_claim);// This function determines if a claim is already eligible & registered

BOOLEAN  lb_eligible = FALSE
LONG		ll_count_claim_1, ll_count_claim_2

SELECT COUNT(*)
INTO 	 :ll_count_claim_1
FROM	 X001_REGISTRATION
WHERE	 claim_no = :al_claim
USING	 SQLCA;

SQLCA.nf_handle_error("n_br_bluecross","uf_claim_is_eligiblity()","select from X001_REGISTRATION")


SELECT COUNT(*)
INTO 	 :ll_count_claim_2
FROM	 CLAIM_ELIGIBILITY
WHERE	 claim_no = :al_claim
USING	 SQLCA;

SQLCA.nf_handle_error("n_br_bluecross","uf_claim_is_eligiblity()","select from CLAIM_ELIGIBILITY")

// The claim is already eligible if the claim is registered and in the eligibility table.

IF ll_count_claim_1 > 0 AND ll_count_claim_2 > 0 THEN
	lb_eligible = TRUE
END IF	


RETURN lb_eligible
end function

public function integer uf_is_registered (long al_claim_no);// This function checks to see if the Claim is currently registered

INT 	li_rtn
LONG	ll_count



SELECT COUNT(*)
INTO	 :ll_count
FROM 	 X001_REGISTRATION
WHERE  claim_no = :al_claim_no
USING	 SQLCA;

SQLCA.nf_handle_error("n_br_bluecross","uf_is_registered()","select from X001_REGISTRATION")

IF ll_count > 0 THEN 
	RETURN 1 //Claim is registered
END IF	

RETURN 0

end function

public function integer uf_determine_claim_status (long al_claim, ref string as_claim_status_code, ref string as_claim_status_type);// Determine the claim status

LONG		ll_count
STRING	ls_status_code, ls_status_type_code

IF ISNULL(as_claim_status_code) or as_claim_status_code = '' THEN

	SELECT claim_status_code, claim_status_type_code
	INTO	 :ls_status_code, :ls_status_type_code
	FROM	 CLAIM
	WHERE  claim_no = :al_claim
	USING	 SQLCA;

	SQLCA.nf_handle_error("n_br_bluecross","uf_check_for_eligiblity()","select from CLAIM")
ELSE
	ls_status_code = as_claim_status_code
	ls_status_type_code = as_claim_status_type
END IF	


CHOOSE CASE ls_status_code
CASE 'A'
	// ACTIVE
	as_claim_status_code = 'A'
	as_claim_status_type = ''
CASE 'F'
	CHOOSE CASE ls_status_type_code
	CASE '01'
		// FINALLED - FINAL
			as_claim_status_code = 'F'
			as_claim_status_type = '01'
	CASE '02' 
		// FINALLED - FIRST & FINAL
			as_claim_status_code = 'F'
			as_claim_status_type = '02'
	CASE '03'
		// FINALLED - LOST TIME MA ONLY
			as_claim_status_code = 'F'
			as_claim_status_type = '03'
	CASE '04'
		// FINALLED - NO LOST TIME & NATURE OF INJURY IS CODED
		
		SELECT COUNT(*)
		INTO	 :ll_count
		FROM	 ACCIDENT
		WHERE	 claim_no = :al_claim
		AND	 (nature_of_injury_code IS NOT NULL OR nature_of_injury_code <> '')
		USING	 SQLCA;
		
		SQLCA.nf_handle_error("n_br_bluecross","uf_determine_claim_status()","select from ACCIDENT")
		
		IF ll_count > 0 THEN
			as_claim_status_code = 'F'
			as_claim_status_type = '04'
		END IF

	END CHOOSE
END CHOOSE


RETURN 1
end function

public function date uf_determine_effective_start_date (long al_claim);DATE 		ldt_effective_start
DATETIME ldt_accident_date


SELECT	accident_date
INTO		:ldt_accident_date
FROM 		CLAIM 
WHERE		claim_no = :al_claim
USING 	SQLCA;

SQLCA.nf_handle_error("n_br_bluecross","uf_determine_effective_start_date()","select from CLAIM")

IF DATE(ldt_accident_date) > DATE('1997-01-01') THEN
	ldt_effective_start = DATE(ldt_accident_date)
ELSE
	ldt_effective_start = DATE('1997-01-01')
END IF

RETURN ldt_effective_start
end function

public function boolean uf_determine_penltd (long al_claim, string as_claim_status, string as_claim_type);// A claim that is Finalled must be receiving PEN or LTD

LONG		ll_count
BOOLEAN	lb_pen_ltd


IF as_claim_status = 'F' AND (as_claim_type = '01' OR as_claim_type = '02' OR as_claim_type = '03' &
   OR as_claim_type = '04') THEN
	
	SELECT 	COUNT(*)
	INTO 		:ll_count
	FROM 		OPENING
	WHERE		claim_no = :al_claim
	AND 		opening_type_code IN ('LTD','PEN')
	USING 	SQLCA;
	
	SQLCA.nf_handle_error("n_br_bluecross","uf_determine_penltd","select from OPENING")

	IF ll_count > 0 THEN
		lb_pen_ltd = TRUE
	ELSE
		lb_pen_ltd = FALSE
	END IF
END IF


RETURN lb_pen_ltd
end function

public function boolean uf_determine_valid_status (long al_claim, string as_claim_status, string as_claim_type);
DATE		ldt_today
LONG		ll_count
BOOLEAN  lb_valid_status = FALSE


ldt_today = DATE(f_server_datetime())

IF as_claim_status = 'A' THEN lb_valid_status = TRUE 
IF as_claim_status = 'F' AND (as_claim_type = '01' OR as_claim_type = '02' OR as_claim_type = '03') THEN lb_valid_status = TRUE

IF as_claim_status = 'F' AND as_claim_type = '04' THEN
	
	SELECT COUNT(*)
	INTO	 :ll_count
	FROM	 ACCIDENT
	WHERE	 claim_no = :al_claim
	AND	 (nature_of_injury_code IS NOT NULL OR nature_of_injury_code <> '')
	USING	 SQLCA;
		
	SQLCA.nf_handle_error("n_br_bluecross","uf_determine_valid_status()","select from ACCIDENT")
	
	IF ll_count > 0 THEN
		lb_valid_status = TRUE
	ELSE 
		lb_valid_status = FALSE
	END IF
	
END IF

RETURN lb_valid_status
end function

public function boolean uf_check_for_death_date (long al_claim);DATETIME	ldt_death_date



SELECT I.death_date
INTO   :ldt_death_date
FROM   CLAIM C, INDIVIDUAL I
WHERE  C.claim_no = :al_claim
AND    I.individual_no = C.individual_no
USING	 SQLCA;

SQLCA.nf_handle_error('n_br_bluecross','uf_check_for_death_date','SELECT')

IF ISNULL(ldt_death_date) THEN
	RETURN FALSE
ELSE
	RETURN TRUE
END IF	


end function

public function long uf_get_last_eligibility_no ();LONG ll_last_eligibility_record_no, ll_error

UPDATE Last_Eligibility_Record_No
SET last_eligibility_record_no = last_eligibility_record_no +1
USING SQLCA;

IF SQLCA.nf_handle_error("Embedded SQL","n_br_bluecross","nf_get_last_eligibility_no") < 0 THEN RETURN -1

SELECT last_eligibility_record_no
INTO :ll_last_eligibility_record_no
FROM Last_Eligibility_Record_No
USING SQLCA;

IF SQLCA.nf_handle_error("Embedded SQL","n_br_bluecross","nf_get_last_eligibility_no") < 0 THEN RETURN -1

IF ll_error < 0 THEN
	RETURN - 1
END IF



RETURN ll_last_eligibility_record_no
end function

public function integer uf_create_eligibility (long al_claim, datetime adtm_start_date, datetime adtm_end_date, string as_comment);LONG		ll_eligibility_rec_no, ll_export_no
DATETIME	ldtm_create_date
STRING	ls_manual_entry_flag, ls_create_user_id


//Get Last Eligibility Number
ll_eligibility_rec_no = uf_get_last_eligibility_no()
IF ll_eligibility_rec_no = -1 THEN RETURN -1

ldtm_create_date = f_server_datetime()
ls_manual_entry_flag = "N"
ll_export_no = 0
ls_create_user_id = vgst_user_profile.user_id

	
	
INSERT INTO CLAIM_ELIGIBILITY
(eligibility_record_no,claim_no,eligibility_start_date,eligibility_end_date, comment, manual_entry_flag, &
export_no,create_date,create_user_id)
Values (:ll_eligibility_rec_no, :al_claim, :adtm_start_date, :adtm_end_date, :as_comment, &
:ls_manual_entry_flag, :ll_export_no, :ldtm_create_date, :ls_create_user_id)
USING SQLCA;

IF SQLCA.nf_handle_error("n_br_bluecross","uf_create_eligibility()","INSERT into CLAIM_ELIGIBILITY") < 0 THEN
	RETURN -1 
END IF	


RETURN 0
end function

public function integer uf_log_auto_event (long al_claim, string as_event_type_code, string as_event_specific, string as_comment);/* This function creates the automatic event based on the following:

CLAIM_EVENT - Eligibility Coverage Change.

Column Name	Value
claim_no	claim_no  
event_no	Max(event_no) for claim_no + 1
event_date	Current Date
event_type_code	'036'
event_specific_code	''
event_comment	CLAIM_ELIGIBILITY.eligibility_start_date to CLAIM_ELIGIBILITY.eligibility_end_date
event_category_code	'C'

CLAIM_EVENT - Formulary Coverage Change.

Column Name	Value
claim_no	claim_no  
event_no	Max(event_no) for claim_no + 1
event_date	Current Date
event_type_code	'035'
event_specific_code	''
event_comment	CLAIM_FORMULARY.formulary_code '  ' CLAIM_FORMULARY.formulary_start_date to CLAIM_FORMULARY.formulary_end_date
event_category_code	'C'
*/

LONG		ll_event_no

inv_event = Create n_event_log


ll_event_no = inv_event.nf_next_claim_event_no(al_claim)
IF ll_event_no = -1 THEN
	MessageBox("Event No","Unable to determine next event no, cannot create event log.")
	Return -1 
END IF

//Create Event
IF inv_event.nf_create_auto_event(al_claim,ll_event_no,as_event_type_code,as_comment,as_event_specific) < 0 THEN
	RETURN -1
END IF

RETURN 0
end function

public function integer nf_create_formulary_history (long al_claim_no);STRING ls_sql

ls_sql = "p_CREATE_FORMULARY_HISTORY " + string(al_claim_no) 

EXECUTE IMMEDIATE :ls_sql USING SQLCA;
SQLCA.nf_handle_error('n_br_bluecross','','nf_create_formulary_history - p_CREATE_CLAIM_FORMULARY_HISTORY')

IF sqlca.sqldbcode < 0  THEN
	SQLCA.SQLERRtext = "FATAL ERROR.  Problem creating Formulary History."
	SQLCA.nf_handle_error("n_br_bluecross","","nf_create_eligibility_history - ls_sql = p_CREATE_CLAIM_ELIGIBILITY_HISTORY")
END IF 


RETURN 1
end function

public function integer nf_create_eligibility_history (long al_claim_no);STRING ls_sql

ls_sql = "p_CREATE_CLAIM_ELIGIBILITY_HISTORY " + string(al_claim_no) 

EXECUTE IMMEDIATE :ls_sql USING SQLCA;
SQLCA.nf_handle_error('n_br_bluecross','','nf_create_eligibility_history - p_CREATE_CLAIM_ELIGIBILITY_HISTORY')

IF sqlca.sqldbcode < 0  THEN
	SQLCA.SQLERRtext = "FATAL ERROR.  Problem creating Eligibility History."
	SQLCA.nf_handle_error("n_br_bluecross","","nf_create_eligibility_history() - ls_sql = p_CREATE_CLAIM_ELIGIBILITY_HISTORY")
END IF 

RETURN 1
end function

public function integer uf_split_eligibility (long al_claim, date adt_start_date, datetime adt_eligibility_date, datetime adt_today, string as_comment);// This function splits the eligibility record with a before record <= 2003-12-31 and an after record >= 2004-01-01

INT 		li_return
LONG		ll_eligibility_rec_no, ll_export_no
DATETIME ldt_eligibility_start, ldt_eligibility_end_date, ldt_export_date
DATETIME ldt_eligibility_start_new, ldt_eligibility_end_new, ldt_create_date
STRING	ls_comment, ls_manual_entry_flag, ls_export_user_id, ls_create_user_id, ls_event_comment
STRING   ls_date1, ls_date2



//Get Last Eligibility Number
ll_eligibility_rec_no = uf_get_last_eligibility_no()

IF ll_eligibility_rec_no = -1 THEN RETURN -1

/* run the stored procedure for the eligibility update */
IF nf_create_eligibility_history(al_claim) < 1 THEN RETURN -1

// Update old record <= 2003-12-31

UPDATE CLAIM_ELIGIBILITY
SET    eligibility_end_date = '2003-12-31 00:00:00'
WHERE  claim_no            = :al_claim
AND	 eligibility_end_date = :adt_eligibility_date
USING SQLCA;

IF SQLCA.nf_handle_error("n_br_bluecross","uf_split_eligibility","select from UPDATE_ELIGIBILITY") < 0 THEN RETURN -1

// Insert new record >= 2004-01-01

ls_comment = "New record due to " + as_comment  
ls_manual_entry_flag = "N"
ll_export_no = 0
ldt_create_date = f_server_datetime()
ls_create_user_id = vgst_user_profile.user_id


ldt_eligibility_start_new = DATETIME(DATE('2004-01-01'))
ldt_eligibility_end_new = adt_today


INSERT INTO CLAIM_ELIGIBILITY
(eligibility_record_no,claim_no,eligibility_start_date,eligibility_end_date, comment, manual_entry_flag, &
export_no,create_date,create_user_id)
Values (:ll_eligibility_rec_no, :al_claim, :ldt_eligibility_start_new, :ldt_eligibility_end_new, :ls_comment, &
:ls_manual_entry_flag, :ll_export_no, :ldt_create_date, :ls_create_user_id)
USING SQLCA;

IF SQLCA.nf_handle_error("n_br_bluecross","uf_split_eligibility()","INSERT into CLAIM_ELIGIBILITY") < 0 THEN
	RETURN -1
END IF	


//Log an event for an Eligibility date change
ls_date1 = STRING(ldt_eligibility_start_new,"yyyy-mm-dd")
ls_date2 = STRING(ldt_eligibility_end_new,"yyyy-mm-dd")
ls_event_comment = 'A new Claim Eligibility coverage period has been created from '+ls_date1+' to '+ls_date2+' .'
IF uf_log_auto_event(al_claim,'035','',ls_event_comment) < 0 THEN RETURN -1

RETURN 0

end function

public function integer uf_determine_eligibility_setup (long al_claim, datetime adtm_accident_date);LONG	ll_count
DATETIME  ldtm_min_start_date

SELECT MIN(eligibility_start_date)
INTO   :ldtm_min_start_date
FROM   CLAIM_ELIGIBILITY
WHERE  claim_no = :al_claim
USING  SQLCA;

IF SQLCA.nf_handle_error("n_br_bluecross","uf_determine_eligibility_setup","select from X001_ELIGIBILITY") < 0 THEN
	RETURN -1
END IF

IF ISNULL(ldtm_min_start_date) OR DATE(ldtm_min_start_date) = DATE('1900-01-01') THEN RETURN 0 //Doesn't Apply

SELECT COUNT(*)
INTO   :ll_count
FROM   X001_ELIGIBILITY
WHERE  claim_no = :al_claim
AND    eligibility_start_date <= :ldtm_min_start_date
USING  SQLCA;

IF SQLCA.nf_handle_error("n_br_bluecross","uf_determine_eligibility_setup","select from X001_ELIGIBILITY") < 0 THEN
	RETURN -1
END IF	


IF ll_count = 0 AND DATE(adtm_accident_date) > DATE(ldtm_min_start_date) THEN
	RETURN -1
END IF

RETURN 0

end function

public function integer uf_determine_formulary_setup (long al_claim, datetime adtm_accident_date);LONG	ll_count
DATETIME ldtm_min_start_date



SELECT MIN(formulary_start_date)
INTO   :ldtm_min_start_date
FROM   CLAIM_FORMULARY
WHERE  claim_no = :al_claim
AND    formulary_code IS NOT NULL
USING  SQLCA;

IF SQLCA.nf_handle_error("n_br_bluecross","uf_determine_formulary_setup","select from CLAIM_FORMULARYY") < 0 THEN
	RETURN -1
END IF

IF ISNULL(ldtm_min_start_date) OR DATE(ldtm_min_start_date) = DATE('1900-01-01') THEN RETURN 0 //Doesn't Apply

SELECT COUNT(*)
INTO   :ll_count
FROM   X001_FORMULARY
WHERE  claim_no = :al_claim
AND    formulary_start_date <= :ldtm_min_start_date
USING  SQLCA;

IF SQLCA.nf_handle_error("n_br_bluecross","uf_determine_formulary_setup","select from X001_FORMULARY") < 0 THEN
	RETURN -1
END IF	

IF ll_count = 0 AND DATE(adtm_accident_date) > DATE(ldtm_min_start_date) THEN
	RETURN -1
END IF

RETURN 0

end function

public function integer uf_log_claim_event_for_accdate_change (long al_claim, datetime adt_accident_date);Long		ll_return, ll_count, ll_event_no
Boolean	lb_valid_status, lb_pen_ltd
String	ls_nature_of_injury_code, ls_formulary_code_compare, ls_formulary_code, ls_event_type_code, ls_event_comment
String	ls_event_specific_code
Datetime ldtm_eligibility_start_date, ldtm_ninety_seven

inv_event = Create n_event_log

ldtm_ninety_seven = DATETIME(DATE('1997-01-01'))

SELECT MIN(eligibility_start_date)
INTO   :ldtm_eligibility_start_date
FROM   CLAIM_ELIGIBILITY
WHERE  claim_no = :al_claim
USING  SQLCA;

SQLCA.nf_handle_error("n_br_bluecross","uf_log_claim_event_for_accdate_change()","select CLAIM_ELIGIBILITY")

//IF adt_accident_date >= ldtm_ninety_seven AND adt_accident_date < ldtm_eligibility_start_date THEN
IF adt_accident_date < ldtm_eligibility_start_date AND DATE(ldtm_eligibility_start_date) > DATE('1997-01-01') THEN

	//Create the new coverage record
   IF	uf_create_new_coverage_rec(al_claim,ldtm_eligibility_start_date,adt_accident_date) < 0 THEN
		RETURN -1 
	END IF	

	ll_event_no = inv_event.nf_next_claim_event_no(al_claim)
	IF ll_event_no = -1 THEN
		MessageBox("Event No","Unable to determine next event no, cannot create event log.")
		Return -1 
	END IF

	ls_event_type_code = '029'
	ls_event_comment = 'The claim accident date has changed, the primary formulary effective date may be impacted.'
	ls_event_specific_code = 'RQX'
	
	//Create Event
	IF inv_event.nf_create_auto_event(al_claim,ll_event_no,ls_event_type_code,ls_event_comment,ls_event_specific_code) < 0 THEN
		RETURN -1
	ELSE
		Messagebox('Information','An automatic event has been logged. The accident date has changed~n'+&
		'and it may impact the primary formulary effective date.',Information!,OK!)
	END IF	
		
	//Send event to the Medical Advisor Review Bucket 
	IF uf_send_doc(al_claim,ll_event_no) < 0 THEN RETURN -1

END IF

RETURN 0
end function

public function integer uf_create_new_coverage_rec (long al_claim, datetime adt_effective_start, datetime adt_accident_date);// This function is used to create a new coverage period record.

INT 		li_return
LONG		ll_eligibility_rec_no, ll_export_no
DATETIME ldt_eligibility_start, ldt_eligibility_end_date, ldt_export_date, ldtm_start_2004, ldtm_end_2003
DATETIME ldtm_eligibility_start_new, ldtm_eligibility_end_new, ldt_create_date
STRING	ls_comment, ls_manual_entry_flag, ls_export_user_id, ls_create_user_id, ls_event_comment


SELECT eligibility_start_date,eligibility_end_date
INTO :ldt_eligibility_start, :ldt_eligibility_end_date
FROM CLAIM_ELIGIBILITY
WHERE claim_no = :al_claim
AND	eligibility_start_date = :adt_effective_start
USING SQLCA;

SQLCA.nf_handle_error("n_br_bluecross","uf_create_new_coverage_rec()","select from CLAIM_ELIGIBILITY")

ls_comment = "New record due to accident date change."
ls_manual_entry_flag = "N"
ll_export_no = 0
ldt_create_date = f_server_datetime()
ls_create_user_id = vgst_user_profile.user_id

IF DATE(adt_accident_date) < DATE('1997-01-01') THEN
	ldtm_eligibility_start_new = DATETIME(DATE('1997-01-01'))
ELSE	
	ldtm_eligibility_start_new = adt_accident_date
END IF	
ldtm_eligibility_end_new = DATETIME( RelativeDate ( DATE(ldt_eligibility_start), -1 ))

/* run the stored procedure for the eligibility update */
IF nf_create_eligibility_history(al_claim) < 1 THEN RETURN -1

// Check for a Split. If the new record covers 2004-01-01 then another record is required.

IF DATE(ldtm_eligibility_start_new) < DATE('2004-01-01') AND DATE(ldtm_eligibility_end_new) > DATE('2004-01-01') THEN
	// Two splits must be done
	ldtm_end_2003 = DATETIME(DATE('2003-12-31'))
	ldtm_start_2004 = DATETIME(DATE('2004-01-01'))
	IF uf_create_eligibility(al_claim, ldtm_eligibility_start_new, ldtm_end_2003, ls_comment) < 0 THEN RETURN -1
	IF uf_create_eligibility(al_claim, ldtm_start_2004, ldtm_eligibility_end_new, ls_comment) < 0 THEN RETURN -1
ELSE
	// Single split 
	IF uf_create_eligibility(al_claim, ldtm_eligibility_start_new, ldtm_eligibility_end_new, ls_comment) < 0 THEN RETURN -1
END IF

//Log an event for an Eligibility date change
ls_event_comment = 'A new Claim Eligibility coverage period has been created from '+STRING(ldtm_eligibility_start_new,"yyyy-mm-dd")+' to '+STRING(ldtm_eligibility_end_new,"yyyy-mm-dd")+' .'
IF uf_log_auto_event(al_claim,'035','',ls_event_comment) < 0 THEN RETURN -1

RETURN li_return
end function

public function date uf_add_3months_today ();// Addes 3 months to todays date and returns that new date.

DATE		ldt_today, ldt_new_date
LONG		ll_month, ll_year
STRING	ls_day, ls_month, lsdt_new_date, ls_today, ls_year
DATETIME ldtm_new_date, ldtm_today

ldt_today = DATE(f_server_datetime())
ldtm_today = DATETIME(ldt_today)


SELECT Distinct DATEADD(MONTH,3,GetDate())
INTO   :ldtm_new_date
FROM   sysobjects
USING  SQLCA;

SQLCA.nf_handle_error("n_br_bluecross","uf_add3months_today","select from sysobjects")

RETURN DATE(ldtm_new_date)
end function

public function integer uf_determine_termination_date (long al_claim, ref date adt_termination_date);// This fucntion determines what the termination date is for the claimiant's drug coverage

DATE		ldt_today
DATE		ldt_past_date, ldt_future_date, ldt_start_date, ldt_comparison
DATETIME	ldt_accident_date, ldt_death_date, ldt_eligibility_end_date, adtm_termination_date, ldtm_start_date
DATETIME ldtm_comparison
STRING	as_claim_status_code,as_claim_status_type, ls_month, ls_day
LONG 		ll_count
LONG		ll_year, ll_month


ldt_today = DATE(f_server_datetime())

/* 2.70 A claim must be registered with a coverage termination date that is the later of  the 
individual's death date or January 01, 1997 for both the claim coverage and the primary formulary
coverage when the individual is deceased  */


SELECT	I.death_date, C.accident_date
INTO		:ldt_death_date, :ldt_accident_date
FROM 		CLAIM C, INDIVIDUAL I
WHERE		C.claim_no = :al_claim
AND		I.individual_no = C.individual_no
USING 	SQLCA;

SQLCA.nf_handle_error("n_br_bluecross","uf_determine_termination_date()","select from CLAIM")

IF ISNULL(ldt_death_date) THEN
	//CONTINUE
ELSE
	IF DATE(ldt_death_date) > DATE('1997-01-01') THEN
		adt_termination_date = DATE(ldt_death_date)
	ELSE
		adt_termination_date = DATE('1997-01-01')
	END IF
	RETURN 1
END IF


/* 2.80 An active claim that is eligible to be registered must be registered with no termination 
date for both the claim coverage and the primary formulary coverage, provided that
·	The individual is not deceased
·	The termination date has not been manually overridden by the user */

// Find the manually overriden termination date

SELECT COUNT(*)
INTO	 :ll_count
FROM 	 X001_OVERRIDE_ELIGIBILITY
WHERE	 claim_no = :al_claim
USING	 SQLCA;

SQLCA.nf_handle_error("n_br_bluecross","uf_determine_termination_date()","select from X001_OVERRIDE_ELIGIBILITY")

IF ll_count > 0 THEN
	SELECT eligibility_end_date
	INTO	 :ldt_eligibility_end_date
	FROM 	 X001_OVERRIDE_ELIGIBILITY
	WHERE	 claim_no = :al_claim
	USING	 SQLCA;

	SQLCA.nf_handle_error("n_br_bluecross","uf_determine_termination_date()","select from X001_OVERRIDE_ELIGIBILITY")

	adt_termination_date = DATE(ldt_eligibility_end_date)
	RETURN 1
END IF		

// Determine the Claim Status Code & Type
uf_determine_claim_status(al_claim, as_claim_status_code, as_claim_status_type)

IF as_claim_status_code = 'A' THEN
	SetNull(adt_termination_date)
	RETURN 1
END IF

/* 2.90 A finalled claim (Final, First&Final, LostTime Medical Aid Only or No Lost Time with NOI 
coded) that is eligible to be registered must be registered with no termination date for both 
the claim coverage and the primary formulary coverage provided that:
·	The claim has a pension or a long-term disability benefit
·	The individual is not deceased
·	The termination date has not been manually overridden by the user

2.100 A finalled claim (Final, First&Final, LostTime Medical Aid Only or No Lost Time with NOI 
coded) that is eligible to be registered must be registered with a termination date that is 
three months from the coverage effective date  for both the claim coverage and the primary 
formulary coverage, provided that:
·	The claim does not have a pension or a long-term disability benefit
·	The individual is not deceased
·	The accident date is less than or equal to six months prior to the date of registration  
·	The termination date has not been manually overridden by the user
 

2.110 A finalled claim (Final, First&Final, LostTime Medical Aid Only or No Lost Time with NOI 
coded) that is eligible to be registered must be registered with a termination date of three 
months from the registration date for both the claim coverage and the primary formulary coverage 
provided that:
·	The claim does not have a pension or a long-term disability benefit
·	The individual is not deceased
·	The accident date is more than six months prior to the date of registration  
·	The termination date has not been manually overridden by the user
*/

IF as_claim_status_code = 'F' AND (as_claim_status_type = '01' OR as_claim_status_type = '02' OR as_claim_status_type = '03' OR as_claim_status_type = '04') THEN
	
	// determine if the claimiant is receiving PEN OR LTD
	SELECT 	COUNT(*)
	INTO 		:ll_count
	FROM 		OPENING
	WHERE		claim_no = :al_claim
	AND 		opening_type_code IN ('LTD','PEN')
	USING 	SQLCA;
	
	SQLCA.nf_handle_error("n_br_bluecross","uf_determine_termination_date()","select from OPENING")
	
	IF ll_count > 0 THEN
		SetNull(adt_termination_date)
		RETURN 1
	END IF

	// Get the date 

	SELECT Distinct DATEADD(MONTH,-6,GetDate())
	INTO   :ldtm_comparison
	FROM   sysobjects
	USING  SQLCA;

	SQLCA.nf_handle_error("n_br_bluecross","uf_determine_termination_date()","select from sysobjects")
	
	IF DATE(ldtm_comparison) <= DATE(ldt_accident_date) THEN
		// the accident date is less than or equal to 6 months prior to the date of registration.
		// must be registered with a termination date 3 months from the coverage effective date.

		
		SELECT	accident_date, DATEADD(MONTH,3,accident_date)
		INTO		:ldtm_start_date, :adtm_termination_date
		FROM 		CLAIM 
		WHERE		claim_no = :al_claim;

		SQLCA.nf_handle_error("n_br_bluecross","uf_determine_termination_date()","select from CLAIM")

		IF DATE(ldtm_start_date) > DATE('1997-01-01') THEN
			adt_termination_date = DATE(adtm_termination_date)
		ELSE
			adt_termination_date = DATE('1997-03-01')
		END IF

	ELSE
		// the accident date is more than 6 months prior to the date of registration.
		// must be registered with a termination date of 3 months from the registered date.
		
		SELECT Distinct DATEADD(MONTH,3,GetDate())
		INTO   :adtm_termination_date
		FROM   sysobjects
		USING  SQLCA;

		SQLCA.nf_handle_error("n_br_bluecross","uf_determine_termination_date","select from sysobjects")

		adt_termination_date = DATE(adtm_termination_date)

	END IF	
		
END IF
	
RETURN 1
end function

public function boolean uf_check_for_eligibility (long al_claim, string as_claim_status_code, string as_claim_status_type);// This function determines if the claim is eligible to be registered for BlueCross Drug Coverage

INT		li_rtn
LONG		ll_count, ll_count_claim
BOOLEAN	lb_eligible, lb_excluded_claim
STRING	ls_status_code, ls_status_type_code, as_status

IF ISNULL(as_claim_status_code) or as_claim_status_code = '' THEN
	// Determine the Claim Status Code & Type
	uf_determine_claim_status(al_claim, as_claim_status_code, as_claim_status_type)
END IF	

IF as_claim_status_code = 'A' OR (as_claim_status_code = 'F' AND (as_claim_status_type = '01' OR &
	as_claim_status_type = '02' OR as_claim_status_type = '03' OR as_claim_status_type = '04')) THEN
	IF as_claim_status_type = '04'THEN
		//check if there is an NOI 
			SELECT COUNT(*)
			INTO	 :ll_count
			FROM	 ACCIDENT
			WHERE	 claim_no = :al_claim
			AND	 (nature_of_injury_code IS NULL OR nature_of_injury_code = '')
			USING	 SQLCA;
		
			SQLCA.nf_handle_error("n_br_bluecross","", "uf_check_for_eligibility - select from ACCIDENT")

			IF ll_count > 0  THEN
				lb_eligible = FALSE
				RETURN lb_eligible
			END IF	
	END IF
ELSE
	lb_eligible = FALSE
	RETURN lb_eligible
END IF


// The must not be an excluded claim	
// Return TRUE if the claim is an excluded claim. The eligibilty is then FALSE
// they are not eligible to be registered.
IF uf_check_if_excluded_claim(al_claim) = TRUE THEN
		lb_eligible = FALSE
		RETURN lb_eligible
END IF

// The claim must not already be registered

SELECT COUNT(*)
INTO 	 :ll_count_claim
FROM	 X001_REGISTRATION
WHERE	 claim_no = :al_claim
USING	 SQLCA;

SQLCA.nf_handle_error("n_br_bluecross","uf_check_for_eligiblity()","select from X001_REGISTRATION")

IF ll_count_claim <> 0 THEN 
	lb_eligible = FALSE
ELSE
	lb_eligible = TRUE
END IF

Return lb_eligible
end function

public function datetime uf_get_bluecross_next_day ();Datetime 	ldtm_next_day, ldtm_two_day, ldtm_date
Date			ldt_today
LONG        ll_count

ldt_today = DATE(f_server_datetime())

//PR8364 - Check to see if the Eligiblity Export has already run for today
SELECT COUNT(*)
INTO        :ll_count
FROM      X001_EXPORT_HISTORY
WHERE   export_date >= :ldt_today 
AND          export_date <  DateAdd(day, 1, :ldt_today)
USING     SQLCA;

SQLCA.nf_handle_error('n_br_bluecross','uf_get_bluecross_next_day()','SELECT COUNT(*) INTO :ll_count')

IF ll_count = 0 THEN
	
	SELECT DATEADD(day,1,min(calendar_date))
	  INTO :ldtm_next_day
	  FROM Company_Calendar 
	 WHERE parent_company_code = 'ABCC'
		AND calendar_date >= :ldt_today
		AND working_days = 1.00 
	 USING SQLCA;
		
	SQLCA.nf_handle_error('n_br_bluecross','uf_get_bluecross_next_day()','SELECT... FROM Company_Calendar')
	
	IF SQLCA.SQLNRows = 0  OR ISNULL(ldtm_next_day) Then
		MessageBox('ABCC Calendar','The ABCC Calendar is not loaded. Call the helpdesk.')
	END IF	
	
	ldtm_date = ldtm_next_day

ELSE

	SELECT DATEADD(DAY,1,MIN(calendar_date))
	INTO       :ldtm_two_day
	FROM     Company_Calendar 
	WHERE parent_company_code = 'ABCC'
	AND        calendar_date > :ldt_today
	AND        working_days = 1.00 
	USING    SQLCA;
		
	SQLCA.nf_handle_error('n_br_bluecross','uf_get_bluecross_next_day()','SELECT... FROM Company_Calendar')
	
	IF SQLCA.SQLNRows = 0  OR ISNULL(ldtm_two_day) Then
		MessageBox('ABCC Calendar','The ABCC Calendar is not loaded. Call the helpdesk.')
	END IF	
	
	ldtm_date = ldtm_two_day
	
END IF

RETURN ldtm_date
end function

public function integer uf_send_doc (long al_claim, long al_event_no);LONG		ll_fldid, ll_selected_row,	ll_catid, ll_setid, ll_docid, ll_results, ll_claim_no, ll_event_no, ll_count
STRING   ls_action, ls_action_date, ls_keyword, ls_claimant_name
STRING   ls_last_name, ls_given_names
DATETIME	ldt_action_date

/* Create an instance of the user object for the imaging functions.
*/
inv_imaging = CREATE n_imaging

/* Get variables, and validate.
*/

SELECT i.last_name, i.given_names
INTO   :ls_last_name, :ls_given_names
FROM 	 INDIVIDUAL i, CLAIM c
WHERE  c.claim_no = :al_claim
AND	 i.individual_no = c.individual_no
USING SQLCA;

SQLCA.nf_handle_error("n_br_bluecross","uf_send_doc","SELECT FROM INDIVIDUAL")

ls_claimant_name =  TRIM(ls_given_names) + " " + TRIM(ls_last_name)
ll_claim_no		  = al_claim
ls_action        = is_action

// Check to make sure the MA Review bucket exists
SELECT  COUNT(*)
INTO    :ll_count
FROM 	  Medical_Advisor_Routing
WHERE   Medical_Advisor_Routing.setid = :vgst_user_profile.default_image_working_setid
USING   ImageTrans;

SQLCA.nf_handle_error("n_br_bluecross","uf_send_doc","SELECT FROM Medical_Advisor_Routing")

IF ll_count = 0 THEN
	MessageBox('ERROR','The MA - Review bucket has not been set-up in your User_Profile. The Event Log just created will not automatically be sent to the Medical Advisor.',Exclamation!,ok!)
	RETURN -1
END IF

/* Get the category id number for the Medical Advisor */
SELECT CAT.catid, CAT.setid
INTO   :ll_catid, :ll_setid
FROM   CAT,
       Medical_Advisor_Routing 
WHERE  CAT.catid = Medical_Advisor_Routing.medical_advisor_catid
AND    Medical_Advisor_Routing.setid = :vgst_user_profile.default_image_working_setid
USING  ImageTrans;

IF SQLCA.nf_handle_error("n_br_bluecross","uf_send_doc","SELECT FROM CAT") < 0 THEN RETURN -1

IF ll_setid = 0 THEN
	MessageBox('WARNING',"The auto event just created can not be sent to the Medical Advisor's Review Bucket.")
END IF	

ldt_action_date  = f_server_datetime()
ls_keyword       = "See event "+String(al_event_no) 



/* Create a work folder.
*/
ImageTrans.nf_begin_transaction()

// this function begins & commits its own transaction, then inserts data outside of txn,
// so it must be enclosed within its own txn

ll_fldid = inv_imaging.nf_create_workfolder("uf_send_doc",ll_catid)
IF ll_fldid = -1 THEN
	MessageBox("Warning","Unable to create work folder.")
	Return -1
END IF

ImageTrans.nf_commit_transaction()


ImageTrans.nf_begin_transaction()

/* Index the work folder.
*/
INSERT INTO CLAIM_WORKING
(folderid , claim_no , action_code, action_date,action_note)
Values (:ll_fldid,:ll_claim_no,:ls_action,:ldt_action_date,:ls_keyword)
USING ImageTrans;

IF ImageTrans.nf_handle_error("n_br_bluecross","uf_send_doc","Update CLAIM_WORKING") < 0 THEN
	RETURN -1 
END IF

/* Update the folder with the new name.
*/
UPDATE FLD
	SET fldname = Upper(:ls_action) + CONVERT(varchar(10),:ll_claim_no) + :ls_claimant_name
 WHERE fldid = :ll_fldid
USING ImageTrans;

IF ImageTrans.nf_handle_error("n_br_bluecross", "", "uf_send_doc - Update FLD") < 0 THEN
	RETURN -1
END IF

ImageTrans.nf_commit_transaction()

RETURN 0

end function

public function integer uf_log_claim_event_for_noi_change (long al_claim, string as_claim_status, string as_claim_type);Long		ll_return, ll_event_no
Boolean	lb_valid_status, lb_pen_ltd = TRUE
String	ls_nature_of_injury_code, ls_formulary_code_compare, ls_formulary_code, ls_event_type_code
String   ls_event_comment, ls_event_specific_code

inv_event = Create n_event_log

//Check for a valid status.
lb_valid_status = uf_determine_valid_status(al_claim,as_claim_status,as_claim_type)

IF as_claim_status = 'F' THEN
	IF lb_valid_status = TRUE THEN
		lb_pen_ltd =	uf_determine_penltd(al_claim,as_claim_status,as_claim_type)
	ELSE
		lb_pen_ltd = FALSE
	END IF	
END IF	

// 14.80 A claim event must be logged to notify the user of a potenial impact to the primary formulary code under the 
// following conditions:
// - the claim is registered
// - the claim status is revised to active or (F 01, F 02, F 03, F 04 - and has a LTD or PEN opening)
// - Accident NOI maps to a different Formulary Code than the formulary assigned to the active, primary formulary for the 
//   claim.

IF lb_valid_status = FALSE OR lb_pen_ltd = FALSE THEN RETURN -1

// Get the Active, Primary Formulary
SELECT formulary_code
INTO   :ls_formulary_code
FROM   CLAIM_FORMULARY
WHERE	 claim_no = :al_claim
AND	 formulary_type_code = 'P'
AND    primary_active_flag = 'Y'
USING  SQLCA;

SQLCA.nf_handle_error("n_br_bluecross","uf_log_claim_event_for_noi_change()","select CLAIM_FORMULARY")

// Check if there is a primary formulary to compare to
IF ISNULL(ls_formulary_code) THEN RETURN -1

SELECT x.formulary_code
INTO   :ls_formulary_code_compare
FROM   X001_Noi_Formulary_Xref x, ACCIDENT a
WHERE  x.noi_code = a.nature_of_injury_code
AND    claim_no = :al_claim
USING  SQLCA;

SQLCA.nf_handle_error("n_br_bluecross","uf_log_claim_event_for_noi_change()","select X001_Noi_Formulary_Xref")

// Compare the two formulary codes.
IF ls_formulary_code <> ls_formulary_code_compare THEN 
	//log claim event
	ll_event_no = inv_event.nf_next_claim_event_no(al_claim)
	IF ll_event_no = -1 THEN
		MessageBox("Event No","Unable to determine next event no, cannot create event log.")
		Return -1 
	END IF

	ls_event_type_code = '029'
	ls_event_comment = 'The Nature of Injury maps to a different formulary, please revise the primary formulary '+ls_formulary_code_compare+'.'
	ls_event_specific_code = 'RQX'
	

	IF inv_event.nf_create_auto_event(al_claim,ll_event_no,ls_event_type_code,ls_event_comment,ls_event_specific_code) < 0 THEN
		RETURN -1
	ELSE
		Messagebox('Information','An automatic event has been logged. There has been a Nature Of Injury change~n'+&
		'and the primary formulary may need to be revised.',Information!,OK!)
	END IF	
	
	//Send event to the Medical Advisor Review Bucket 
	IF uf_send_doc(al_claim,ll_event_no) < 0 THEN RETURN -1


END IF




RETURN 0
end function

public function integer uf_update_cec_on_adjudication (long al_claim, string as_claim_status, string as_claim_type);// 14.50 A claim must have the claim eligibility coverage terminated on the date of the adjudication 
// decision when all of the following are true:
// - the claim is registered
// - the claim status changes to one of the following :
//					- Finalled - Terminated 3rd party
//					- Finalled - out of Province
//					- Rejected
//					- Adjudicated Review
// - The individual is not deceased
// - Claim Eligibility Coverage is a future date or open-ended

DATE		ldt_past_date, ldt_future_date, ldt_comparison, ldt_new_date,ldt_today_date
DATETIME	ldtm_accident_date, ldtm_death_date, ldtm_eligibility_end_date, ldtm_formulary_end_date
DATETIME	ldtm_today, ldtm_new_datetime, ldtm_start_date, ldtm_next_day, ldtm_form_end_date
STRING	as_claim_status_code,as_claim_status_type, ls_month, ls_day, ls_comment, ls_event_comment,ls_form_code
STRING   ls_date
LONG 		ll_count, ll_rows, ll_count2, ll_count3, ll_count_claim_eligibility, ll_count_claim_formulary
LONG		ll_year, ll_month, ll_count_null, ll_record_no = 0, ll_row
BOOLEAN  lb_change_date = FALSE,lb_pen_ltd, lb_valid_status, lb_update = FALSE


ids_claim_formulary = CREATE DATASTORE
ids_claim_formulary.dataobject = 'd_claim_secondary_formulary'
ids_claim_formulary.SetTransObject(SQLCA)

ldt_today_date = DATE(f_server_datetime())
ldtm_today = DATETIME(ldt_today_date) // This will strip the time and put it back on as 00:00:00
ldtm_next_day = uf_get_bluecross_next_day()

SELECT	I.death_date, C.accident_date
INTO		:ldtm_death_date, :ldtm_accident_date
FROM 		CLAIM C, INDIVIDUAL I
WHERE		C.claim_no = :al_claim
AND		I.individual_no = C.individual_no
USING 	SQLCA;

SQLCA.nf_handle_error("n_br_bluecross","uf_update_cec_on_adjudication()","select from CLAIM")

IF ISNULL(ldtm_death_date) THEN
	//CONTINUE
ELSE
	RETURN 0
END IF

// Claim Status must be one of the following:
// Finalled - Terminated 3rd Party F17
// Finalled-Out of Province F05
// Rejected R
// Adjudicated Review J16

IF as_claim_status = 'R' OR (as_claim_status = 'J' AND as_claim_type = '16') THEN lb_valid_status = TRUE 
IF as_claim_status = 'F' AND (as_claim_type = '05' OR as_claim_type = '17') THEN lb_valid_status = TRUE

IF lb_valid_status = FALSE THEN RETURN 0

// Claim Eligibility Coverage Temination Date

SELECT 	COUNT(*)
INTO 		:ll_count
FROM 		CLAIM_ELIGIBILITY
WHERE		claim_no = :al_claim
AND		(eligibility_end_date IS NULL
OR			eligibility_end_date > :ldtm_today)
USING		SQLCA;

SQLCA.nf_handle_error("n_br_bluecross","uf_update_cec_on_adjudication()","select from CLAIM_ELIGIBILITY")

IF ll_count > 0 AND lb_valid_status = TRUE THEN	
	
	/* run the stored procedure for the eligibility update */
	IF nf_create_eligibility_history(al_claim) < 1 THEN RETURN -1
	
	UPDATE CLAIM_ELIGIBILITY
	SET	 eligibility_end_date = :ldtm_next_day, comment = 'Status Change', manual_entry_flag = 'N'
	WHERE  claim_no = :al_claim
	AND	 (eligibility_end_date IS NULL
	OR		 eligibility_end_date > :ldtm_today)
	USING SQLCA;
	
	IF SQLCA.nf_handle_error("n_br_bluecross","uf_update_cec_on_adjudication()","UPDATE CLAIM_ELIGIBILITY") < 0 THEN
		RETURN -1
	END IF	
	
	//Log an event for an Eligibility date change
	ls_event_comment = 'The Eligibility End Date has changed to '+STRING(ldtm_next_day,"yyyy-mm-dd")+' .'
	IF uf_log_auto_event(al_claim,'035','',ls_event_comment) < 0 THEN RETURN -1

	lb_update = TRUE
END IF

// 14.60 A claim must have the primary formulary coverage terminated on the date of the adjudication decision when all 
// of the following are true...

SELECT 	COUNT(*)
INTO 		:ll_count
FROM 		CLAIM_FORMULARY
WHERE		claim_no = :al_claim
AND	   primary_active_flag = 'Y'
AND	   formulary_type_code = 'P'
AND		(formulary_end_date IS NULL
OR			formulary_end_date > :ldtm_today)
USING		SQLCA;

SQLCA.nf_handle_error("n_br_bluecross","uf_update_cec_on_adjudication()","select from CLAIM_FORMULARY")

IF ll_count > 0 AND lb_valid_status = TRUE THEN	
	
	SELECT formulary_start_date 
	INTO   :ldtm_start_date
	FROM   CLAIM_FORMULARY
	WHERE	 claim_no = :al_claim
	AND	 primary_active_flag = 'Y'
	AND	 formulary_type_code = 'P'
	AND	 (formulary_end_date IS NULL
	OR		 formulary_end_date > :ldtm_today)
	USING	 SQLCA;

	SQLCA.nf_handle_error("n_br_bluecross","uf_update_cec_on_adjudication()","select from CLAIM_FORMULARY")
	
	IF ldtm_start_date > ldtm_next_day THEN
		ldtm_form_end_date = ldtm_start_date
	ELSE
		ldtm_form_end_date = ldtm_next_day
	END IF	
	
	/* run the stored procedure for the formulary update */
	IF nf_create_formulary_history(al_claim) < 1 THEN RETURN -1
	
	UPDATE CLAIM_FORMULARY
	SET	 formulary_end_date = :ldtm_form_end_date, comment = 'Status Change', manual_entry_flag = 'N'
	WHERE  claim_no = :al_claim
	AND	 primary_active_flag = 'Y'
	AND	 formulary_type_code = 'P'
	AND	 (formulary_end_date IS NULL
	OR		 formulary_end_date > :ldtm_today)
	USING SQLCA;
	
	IF SQLCA.nf_handle_error("n_br_bluecross","uf_update_cec_on_adjudication()","UPDATE CLAIM_FORMULARY") < 0 THEN
		RETURN -1
	END IF	
	
	//Log an event for a Formulary date change
	ls_event_comment = 'The Primary Formulary End Date has changed to '+STRING(ldtm_next_day,"yyyy-mm-dd")+' .'
	IF uf_log_auto_event(al_claim,'036','',ls_event_comment) < 0 THEN RETURN -1

	lb_update = TRUE
END IF

// Check the Secondary Formulary - there can be multiple secondaries that are updated.
SELECT 	COUNT(*)
INTO 		:ll_count
FROM 		CLAIM_FORMULARY
WHERE		claim_no = :al_claim
AND	   formulary_type_code = 'S'
AND		(formulary_end_date IS NULL
OR			formulary_end_date > :ldtm_today)
USING		SQLCA;

SQLCA.nf_handle_error("n_br_bluecross","uf_update_cec_on_adjudication()","select from CLAIM_ELIGIBILITY")

IF ll_count > 0 AND lb_valid_status = TRUE THEN	
	
	ids_claim_formulary.Retrieve(al_claim,ldtm_today)
	IF SQLCA.nf_handle_error("n_br_bluecross","uf_update_cec_on_adjudication()","UPDATE CLAIM_FORMULARY") < 0 THEN
		RETURN -1
	END IF
	
	FOR ll_row = 1 to ll_count
		ldtm_start_date = ids_claim_formulary.GetItemDateTime(ll_row,"formulary_start_date") 
		IF ldtm_start_date > ldtm_next_day THEN
			ids_claim_formulary.SetItem(ll_row,"formulary_end_date",ldtm_start_date)
		ELSE	
			ids_claim_formulary.SetItem(ll_row,"formulary_end_date",ldtm_next_day)
			ldtm_start_date = ldtm_next_day
		END IF	
		ids_claim_formulary.SetItem(ll_row,"comment","Status Change")
		ids_claim_formulary.SetItem(ll_row,"manual_entry_flag","N")
		ls_form_code = ids_claim_formulary.GetItemString(ll_row,"formulary_code") 
		//Log an event for a Formulary date change
		ls_date = STRING(ldtm_start_date,"yyyy-mm-dd")
		ls_event_comment = 'The Secondary Formulary '+ls_form_code+' End Date has changed to '+ls_date+' .'
		IF uf_log_auto_event(al_claim,'036','',ls_event_comment) < 0 THEN RETURN -1
	NEXT 	
	
	ids_claim_formulary.Update()
	IF SQLCA.nf_handle_error("n_br_bluecross","uf_update_cec_on_adjudication()","UPDATE CLAIM_FORMULARY") < 0 THEN
		RETURN -1
	END IF	

	lb_update = TRUE
END IF

IF	lb_update = TRUE THEN RETURN 1
	
RETURN 0
end function

public function integer nf_validate_primary_formulary_status (long al_claim_no, string as_filter);LONG     ll_rows , ll_counter
STRING   ls_formulary_code , ls_active_flag
U_DS     lds_validate_claim_formulary

lds_validate_claim_formulary = create U_DS
lds_validate_claim_formulary.DataObject = 'd_claim_primary_formulary'
lds_validate_claim_formulary.SetTransObject(SQLCA)

lds_validate_claim_formulary.Retrieve(al_claim_no)

lds_validate_claim_formulary.SetFilter(as_filter)
lds_validate_claim_formulary.Filter()

ll_rows = lds_validate_claim_formulary.RowCount()

// PR 5636 - If the formulary code is inactive,
// then prevent user from changing eligibility or formulary end dates
FOR ll_counter = 1 to ll_rows
	ls_formulary_code = lds_validate_claim_formulary.GetItemString(ll_counter,'formulary_code')
	ls_active_flag = nf_check_if_active_formulary(ls_formulary_code)
	IF ls_active_flag = 'N' THEN
		MessageBox('Data Integrity Problem','A formulary code ('+ls_formulary_code+') associated with this claim ('+String(al_claim_no)+') is inactive.'&
		                                  + '~nThe formulary end dates cannot be changed by this module.'&
													 + '~nPlease contact the HELPDESK.', Exclamation!)
		RETURN -1
	END IF
NEXT

return 0
end function

public function integer uf_update_termdate_for_registered_claims (long al_claim, string as_claim_status, string as_claim_type);// This fucntion determines what the coverage termination date is for registered claims
// that are not deceased, and their status has changed.

DATE		ldt_new_date, ldt_today_date
DATETIME	ldtm_eligibility_end_date, ldtm_formulary_end_date
DATETIME ldt_accident_date, ldt_death_date, ldtm_start, ldtm_today, ldtm_date
STRING	ls_event_comment, ls_date, ls_filter
LONG 		ll_count_null
BOOLEAN  lb_pen_ltd, lb_valid_status, lb_end_date_null = FALSE, lb_update = FALSE
INTEGER  li_rtn

ldt_today_date = DATE(f_server_datetime())
ldtm_today = DATETIME(ldt_today_date) // This will strip the time and put it back on as 00:00:00

SELECT	I.death_date, C.accident_date
INTO		:ldt_death_date, :ldt_accident_date
FROM 		CLAIM C, INDIVIDUAL I
WHERE		C.claim_no = :al_claim
AND		I.individual_no = C.individual_no
USING 	SQLCA;

SQLCA.nf_handle_error("n_br_bluecross","uf_update_termdate_for_registered_claims","select from CLAIM")

IF ISNULL(ldt_death_date) THEN
	//CONTINUE
ELSE
	RETURN -1
END IF

//Determine if the status passed in meets the criteria to allow the termination date change.
lb_valid_status = uf_determine_valid_status(al_claim,as_claim_status,as_claim_type)

IF lb_valid_status = TRUE AND as_claim_status = 'F' THEN
	lb_pen_ltd =	uf_determine_penltd(al_claim,as_claim_status,as_claim_type)
ELSE
	lb_pen_ltd = FALSE
END IF	

IF lb_valid_status = FALSE THEN RETURN -1

// Claim Eligibility Coverage Temination Date Update?

SELECT 	COUNT(*)
INTO 		:ll_count_null
FROM 		CLAIM_ELIGIBILITY
WHERE		claim_no = :al_claim
AND		eligibility_end_date IS NULL
USING		SQLCA;

SQLCA.nf_handle_error("n_br_bluecross","uf_update_termdate_for_registered_claims","select from CLAIM_ELIGIBILITY")

IF ll_count_null > 0 THEN lb_end_date_null = TRUE

// If the termination date is already null we don't need to make it null. Null means Open-Ended.
// The claim eligibility end date must be changed to null when the claim is registered, the status is changed to Active OR
// Finalled 01, 02, 03, or (04 with NOI coded), not deceased and eligibility is not open-ended.

IF ll_count_null = 0 AND (as_claim_status = 'A' OR & 
   (as_claim_status = 'F' AND lb_valid_status = TRUE AND lb_pen_ltd = TRUE)) THEN	
		
	SELECT MAX(eligibility_end_date)
	INTO   :ldtm_eligibility_end_date
	FROM	 CLAIM_ELIGIBILITY
	WHERE	 claim_no = :al_claim
	USING	 SQLCA;	 
	
	SQLCA.nf_handle_error("n_br_bluecross","uf_update_termdate_for_registered_claims","select from CLAIM_ELIGIBILITY")
	
	//Check the start date the record may need to be split based on the 2004-01-01 date rule
	
	SELECT eligibility_start_date
	INTO   :ldtm_start
	FROM   CLAIM_ELIGIBILITY
	WHERE  claim_no =  :al_claim
	AND    eligibility_end_date = :ldtm_eligibility_end_date
	USING  SQLCA;
	
   SQLCA.nf_handle_error("n_br_bluecross","uf_update_termdate_for_registered_claims","select from CLAIM_ELIGIBILITY")	
	
	SetNull(ldtm_date) 
	
	IF DATE(ldtm_start) < DATE("2004-01-01") THEN
		uf_split_eligibility(al_claim,DATE(ldtm_start),ldtm_eligibility_end_date,ldtm_date,'status change')
	ELSE
		
		/* run the stored procedure for the eligibility update */
		IF nf_create_eligibility_history(al_claim) < 1 THEN RETURN -1
		
		UPDATE CLAIM_ELIGIBILITY
		SET	 eligibility_end_date = :ldtm_date, comment = 'Status Change.', manual_entry_flag = 'N'
		WHERE  claim_no = :al_claim
		AND	 eligibility_end_date = :ldtm_eligibility_end_date
		USING SQLCA;
	
		IF SQLCA.nf_handle_error("n_br_bluecross","uf_update_termdate_for_registered_claims","UPDATE CLAIM_ELIGIBILITY") < 0 THEN
			RETURN -1
		END IF	
			
		//Log an event for an Eligibility date change
		ls_event_comment = 'The Eligibility End Date has changed to Open-Ended .'
		IF uf_log_auto_event(al_claim,'035','',ls_event_comment) < 0 THEN RETURN -1
		
		lb_update = TRUE
	END IF

END IF

// Primary Formulary Coverage Temination Date Update?

SELECT 	COUNT(*)
INTO 		:ll_count_null
FROM 		CLAIM_FORMULARY
WHERE		claim_no = :al_claim
AND		primary_active_flag = 'Y'
AND		formulary_type_code = 'P'
AND		formulary_end_date IS NULL
USING		SQLCA;

SQLCA.nf_handle_error("n_br_bluecross","uf_update_termdate_for_registered_claims","select from CLAIM_FORMULARY")

// The formulary end date must be changed to null when the claim is registered, the status is changed to Active OR
// Finalled 01, 02, 03, or 04 with NOI coded, not deceased and eligibility is not open-ended.

IF ll_count_null = 0 AND (as_claim_status = 'A' OR & 
   (as_claim_status = 'F' AND lb_valid_status = TRUE AND lb_pen_ltd = TRUE)) THEN	
	
	SELECT MAX(formulary_end_date)
	INTO   :ldtm_formulary_end_date
	FROM	 CLAIM_FORMULARY
	WHERE	 claim_no = :al_claim
	AND	 primary_active_flag = 'Y'
	AND	 formulary_type_code = 'P'
	USING	 SQLCA;	 
	
	SQLCA.nf_handle_error("n_br_bluecross","uf_update_termdate_for_registered_claims","select from CLAIM_FORMULARY")
	
	// PR 5636 - If the formulary code is inactive for the situation described above, then allow user to 
	// continue with status change, but prevent user from setting formulary end date to null
	ls_filter = 'formulary_end_date = "' + String(ldtm_formulary_end_date,'yyyy-mm-dd') + '"'
	li_rtn = nf_validate_primary_formulary_status(al_claim,ls_filter)
	IF li_rtn = -1 THEN RETURN li_rtn
		
	/* run the stored procedure for the formulary update */
	IF nf_create_formulary_history(al_claim) < 1 THEN RETURN -1
	
	UPDATE CLAIM_FORMULARY
	SET	 formulary_end_date = NULL, comment = 'Status Change', manual_entry_flag = 'N'
	WHERE  claim_no = :al_claim
	AND	 formulary_end_date = :ldtm_formulary_end_date
	AND	 primary_active_flag = 'Y'
	AND	 formulary_type_code = 'P'
	USING SQLCA;
	
	IF SQLCA.nf_handle_error("n_br_bluecross","uf_update_termdate_for_registered_claims","UPDATE CLAIM_FORMULARY") < 0 THEN
		RETURN -1
	END IF	

	//Log an event for a Formulary date change
	ls_event_comment = 'The Primary Formulary End Date has been changed to Open-Ended .'
	IF uf_log_auto_event(al_claim,'036','',ls_event_comment) < 0 THEN RETURN -1
	
	lb_update = TRUE
END IF


// Claim Eligibility Coverage Temination Date Update To 3 months from the final date where the coverage is future or open-ended
// Don't need to check for a split because it is a future dated record.

SELECT 	COUNT(*)
INTO 		:ll_count_null
FROM 		CLAIM_ELIGIBILITY
WHERE		claim_no = :al_claim
AND		(eligibility_end_date IS NULL
OR			eligibility_end_date > :ldtm_today)
USING		SQLCA;

SQLCA.nf_handle_error("n_br_bluecross","uf_update_termdate_for_registered_claims","select from CLAIM_ELIGIBILITY")

IF ll_count_null > 0 AND as_claim_status = 'F' AND lb_valid_status = TRUE AND lb_pen_ltd = FALSE THEN	

	/* run the stored procedure for the eligibility update */
	IF nf_create_eligibility_history(al_claim) < 1 THEN RETURN -1
	
	ldt_new_date = uf_add_3months_today()
	
	UPDATE CLAIM_ELIGIBILITY
	SET	 eligibility_end_date = :ldt_new_date, comment = 'Status Change', manual_entry_flag = 'N'
	WHERE  claim_no = :al_claim
	AND    (eligibility_end_date IS NULL
	OR		 eligibility_end_date > :ldtm_today)
	USING SQLCA;
	
	IF SQLCA.nf_handle_error("n_br_bluecross","uf_update_termdate_for_registered_claims()","UPDATE CLAIM_ELIGIBILITY") < 0 THEN
		RETURN -1
	END IF	

	//Log an event for an Eligibility date change
	ls_date = String(ldt_new_date,"yyyy-mm-dd")
	ls_event_comment = 'The Eligibility End Date has changed to '+ls_date+' .'
	IF uf_log_auto_event(al_claim,'035','',ls_event_comment) < 0 THEN RETURN -1
	
	lb_update = TRUE
END IF

// The primary Formulary must be updated with a termination date that is three months from the finalling date of
// the claim when all the following are true.

SELECT 	COUNT(*)
INTO 		:ll_count_null
FROM 		CLAIM_FORMULARY
WHERE		claim_no = :al_claim
AND	   primary_active_flag = 'Y'
AND	   formulary_type_code = 'P'
AND		(formulary_end_date IS NULL
OR			formulary_end_date > :ldtm_today)
USING		SQLCA;

SQLCA.nf_handle_error("n_br_bluecross","uf_update_termdate_for_registered_claims()","select from CLAIM_FORMULARY")

IF ll_count_null > 0 AND as_claim_status = 'F' AND lb_valid_status = TRUE AND lb_pen_ltd = FALSE THEN	
	
	// PR 5636 - If the formulary code is inactive for the situation described by the above, then allow user to change 
	// claim status, but prevent user from setting formulary end date to 3 mos from today
	ls_filter = 'IsNull(formulary_end_date) or formulary_end_date > "' + String(ldtm_today,'yyyy-mm-dd') + '"'
	li_rtn = nf_validate_primary_formulary_status(al_claim,ls_filter)
	IF li_rtn = -1 THEN RETURN li_rtn
	
	ldt_new_date = uf_add_3months_today()
	
	/* run the stored procedure for the formulary update */
	IF nf_create_formulary_history(al_claim) < 1 THEN RETURN -1
		
	UPDATE CLAIM_FORMULARY
	SET	 formulary_end_date = :ldt_new_date, comment = 'Status Change', manual_entry_flag = 'N'
	WHERE  claim_no = :al_claim
	AND	 primary_active_flag = 'Y'
	AND	 formulary_type_code = 'P'
	AND	 (formulary_end_date IS NULL
	OR		 formulary_end_date > :ldtm_today)
	USING SQLCA;
	
	IF SQLCA.nf_handle_error("n_br_bluecross","uf_update_termdate_for_registered_claims()","UPDATE CLAIM_FORMULARY") < 0 THEN
		RETURN -1
	END IF	
	
	//Log an event for a Formulary date change
	ls_event_comment = 'The Primary Formulary End Date has changed to '+STRING(ldt_new_date,"yyyy-mm-dd")+' .'
	IF uf_log_auto_event(al_claim,'036','',ls_event_comment) < 0 THEN RETURN -1
	lb_update = TRUE
	
END IF
	
IF	lb_update = TRUE THEN RETURN 1

RETURN 0
end function

public function string nf_check_if_active_formulary (string as_formulary_code);STRING	ls_active_flag

select active_flag
into   :ls_active_flag
from   Formulary
where  formulary_code = :as_formulary_code
using SQLCA;

SQLCA.nf_handle_error('n_br_bluecross','nf_check_if_active_formulary()','select active_flag...') 

RETURN ls_active_flag

end function

public function integer uf_determine_secondary_termination_date (date adt_primary_start_date, date adt_primary_termination_date, string as_primary_formulary_code, string as_secondary_formulary_code, ref date adt_secondary_termination_date);// This function determines what the termination date WILL BE for the claimant's automatic secondary drug coverage

INTEGER li_secondary_formulary_duration
STRING  ls_use_primary_values_flag

/*
need duration to determine secondary formulary end date
*/

SELECT secondary_formulary_duration ,     use_primary_values_flag
INTO   :li_secondary_formulary_duration , :ls_use_primary_values_flag
FROM   X001_Auto_Secondary_Formulary
WHERE  primary_formulary_code   = :as_primary_formulary_code
AND    secondary_formulary_code = :as_secondary_formulary_code ;

SQLCA.nf_handle_error('n_br_bluecross','nf_determine_secondary_termination_date','X001_Auto_Secondary_Formulary')

// value is stored as weeks, so convert it into days
li_secondary_formulary_duration = li_secondary_formulary_duration*7

IF ls_use_primary_values_flag = 'Y' THEN
	adt_secondary_termination_date = adt_primary_termination_date
	RETURN 1
ELSE
	// do not automatically use primary values
	IF IsNull(adt_primary_termination_date) THEN
		// use primary start date + retrieved secondary duration as secondary termination date
		adt_secondary_termination_date = RelativeDate(adt_primary_start_date, li_secondary_formulary_duration )
		RETURN 1
	ELSE
		// there is a non-null primary termination date
		IF RelativeDate(adt_primary_start_date, li_secondary_formulary_duration ) > adt_primary_termination_date THEN
			// If date will be beyond the pending primary termination date, use primary termination date
			adt_secondary_termination_date = adt_primary_termination_date
			RETURN 1
		ELSE
			// otherwise use primary start date + retrieved secondary duration as secondary termination date
			adt_secondary_termination_date = RelativeDate(adt_primary_start_date, li_secondary_formulary_duration )
			RETURN 1
		END IF
	END IF
END IF



end function

on n_br_bluecross.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_br_bluecross.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

