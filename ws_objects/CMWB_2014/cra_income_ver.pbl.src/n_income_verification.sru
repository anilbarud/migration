$PBExportHeader$n_income_verification.sru
forward
global type n_income_verification from n_pdc
end type
end forward

global type n_income_verification from n_pdc
end type
global n_income_verification n_income_verification

type variables
U_DW_ONLINE  idw_basic_claim
STRING is_user
LONG il_claim_no, il_individual_no, il_request_no, il_new_request_no

end variables

forward prototypes
public function integer nf_retrieve_details (long al_request_no)
public function integer nf_insert (long al_row)
public function integer nf_check_bus_rule ()
public function integer nf_set_defaults ()
public function long nf_set_identifiers ()
public function long nf_get_next_identifier ()
public function integer nf_set_claim_info (long al_claim_no, long al_individual_no)
public function integer nf_delete (long al_request_no)
public function integer nf_fill_years ()
public function integer nf_get_individual_info (long al_individual_no, ref string as_name)
end prototypes

public function integer nf_retrieve_details (long al_request_no);DATAWINDOWCHILD ldwc_cats
LONG ll_catid, ll_current_row
string ls_lookup

IF idw_dw[1].Retrieve(al_request_no) < 0 THEN
	MessageBox('Error','Error retrieving request details.',Information!)
	RETURN -1
END IF

ll_current_row = idw_dw[1].GetRow()

IF ll_current_row > 0 THEN
	ll_catid = idw_dw[1].GetItemNumber(ll_current_row,'iv_request_inbasket_catid')

	idw_dw[1].GetChild('iv_request_inbasket_catid',ldwc_cats)
	ldwc_cats.SetTransObject(SQLCA)
	ldwc_cats.SetFilter('')
	ldwc_cats.SetDetailHeight(1, ldwc_cats.RowCount(), 64)
	ldwc_cats.SetFilter("User_Category_Xref.user_name_text <>'" + vgst_user_profile.user_id + "'")
 	ldwc_cats.Filter()
	ldwc_cats.SetDetailHeight(1, ldwc_cats.RowCount(), 0)
	ldwc_cats.SetFilter('')
	ldwc_cats.Filter()
	idw_dw[1].SetItem(ll_current_row, "iv_request_inbasket_catid", ll_catid)
END IF
		
		
IF idw_dw[2].Retrieve(al_request_no) < 0 THEN
	MessageBox('Error','Error retrieving tax years.',Information!)
	RETURN -1
END IF

RETURN 0
end function

public function integer nf_insert (long al_row);DATAWINDOWCHILD ldwc_cats
LONG ll_cntr

idw_dw[1].Reset()
idw_dw[1].InsertRow(al_row)
idw_dw[1].ScrollToRow(al_row)

idw_dw[1].GetChild("iv_request_inbasket_catid",ldwc_cats)
ldwc_cats.SetTransObject(SQLCA)
ldwc_cats.SetFilter("User_Category_Xref.user_name_text ='" + vgst_user_profile.user_id + "'")
ldwc_cats.Filter()

FOR ll_cntr = 1 to 5
	idw_dw[2].InsertRow(ll_cntr)
NEXT 

nf_set_defaults()

RETURN 0

end function

public function integer nf_check_bus_rule ();//2015/07/28 T016641 - David Worboys split declaration to one variable per line
DATETIME ldtm_dob
DATETIME ldtm_current_dt
DATETIME ldtm_deadline
LONG		ll_cntr
LONG 		ll_great_yr
LONG 		ll_start_tax_year //2015/07/28 T016641 - David Worboys Changed ll_least_year to something meaningful
LONG 		ll_curr_yr
LONG 		ll_years[]
LONG 		ll_cnt, ll_cntr2
LONG 		ll_year
LONG 		ll_rowcount
LONG 		ll_catid
LONG 		ll_min_yr
LONG 		ll_ten_yrs
LONG 		ll_cnt_cats
LONG 		ll_pending
LONG 		ll_request_no
LONG 		ll_years_entered
LONG 		ll_min_wrn
STRING     ls_sin
DATAWINDOWCHILD ldwc_catid

ldtm_current_dt = f_server_datetime()
ll_curr_yr = YEAR(DATE(ldtm_current_dt))

SELECT  birth_date
INTO     :ldtm_dob
FROM    CLAIM a, INDIVIDUAL b
WHERE  a.individual_no = b.individual_no
AND         a.claim_no = :il_claim_no
USING   SQLCA;

IF SQLCA.nf_handle_error("n_income_verification","nf_check_bus_rule", "SELECT birth_date, sin_no") < 0 THEN
	RETURN -1
END IF

IF idw_dw[1].RowCount() > 0 THEN
	ll_request_no = idw_dw[1].GetItemNumber(idw_dw[1].GetRow(), 'iv_request_no')
	
	//User must select an inbasket to have the request status notification set to. If the user does not have their own inbasket, they must select the default inbasket for CRA notifications
	ll_catid = idw_dw[1].GetItemNumber(idw_dw[1].GetRow(),'iv_request_inbasket_catid')
END IF

IF ll_catid = 0 OR IsNull(ll_catid) THEN
	SQLCA.nf_rollback_transaction()
	MessageBox('Inbasket','You have not selected an Inbasket. This is required for you to receive notification when a Response is received.', Information!) 
	RETURN -1
END IF

IF IsNull(ll_request_no) THEN
	ll_request_no = 0
END IF

ll_great_yr = ll_curr_yr - 1
ldtm_deadline = DATETIME(DATE(STRING(ll_curr_yr) + '-04-30'))

ll_min_yr			= 1970
ll_min_wrn			= ll_curr_yr  - 10
ll_start_tax_year	= YEAR(DATE(ldtm_dob)) + 14 			//2015/07/28 T016641 - David Worboys Changed ll_least_year to ll_start_tax_year

ll_rowcount = idw_dw[2].RowCount()

//User must enter atleast 1 taxation year
FOR ll_cntr = 1 to ll_rowcount
	IF idw_dw[2].GetItemNumber(ll_cntr, 'tax_year') > 0 THEN
		ll_years_entered = ll_years_entered + 1
	END IF	
NEXT 

IF ll_years_entered = 0 THEN
	SQLCA.nf_rollback_transaction()
	MessageBox('Tax Year','You must enter at least one tax year to complete your request.',Information!)
	idw_dw[2].SetColumn('tax_year')
	idw_dw[2].SetRow(1)
	idw_dw[2].SetFocus()
	RETURN -1
END IF

ll_cntr = 0

IF ll_rowcount > 0 THEN
	FOR ll_cntr = 1 to ll_rowcount
		
		ll_year = idw_dw[2].GetItemNumber(ll_cntr, 'tax_year')
		
		IF ll_year > 0 THEN
			
			//User should be warned if a year in this request already exists in a pending request for the same claim
			
			SELECT Count(*)
			INTO       :ll_pending
			FROM     IV_REQUEST a, 
			                IV_REQUEST_DETAIL b
			WHERE a.iv_request_no = b.iv_request_no
			AND        a.claim_no         = :il_claim_no
			AND        b.tax_year           = :ll_year
			AND        a.worksafe_request_status_code = 'RP'
			AND        a.iv_request_no <> :ll_request_no
			USING    SQLCA;
			
			IF SQLCA.nf_handle_error("Embedded SQL: SELECT Count(*) ","n_income_verification","nf_check_bus_rule") < 0 THEN
				RETURN -1
			END IF
			
			IF ll_pending > 0 THEN 
				IF MessageBox('Duplicate Year Requested','The tax year requested, ' + STRING(ll_year) + ', already exists in a Pending request for this claim. ~r~nWould you like to Continue?', Question!, YesNo!) = 2 THEN
					idw_dw[2].SetColumn('tax_year')
					idw_dw[2].SetRow(ll_cntr2)
					idw_dw[2].SetFocus()
					RETURN -1
				END IF
			END IF
			
			//Tax years must all be different within the request
			IF ll_cnt > 0 THEN
				FOR ll_cntr2 = 1 to ll_cnt
					IF ll_year = ll_years[ll_cntr2] THEN
						SQLCA.nf_rollback_transaction()
						MessageBox('Duplicate Year','You have entered a year that already exists in this request. Please enter a different year.',Information!)
							idw_dw[2].SetColumn('tax_year')
							idw_dw[2].SetRow(ll_cntr2)
							idw_dw[2].SetFocus()
						RETURN -1
					END IF
				NEXT 
			END IF
			
			IF ll_year < ll_min_yr THEN
				SQLCA.nf_rollback_transaction()
				MessageBox('Minimum Tax Year','You have entered a year of ' + STRING (ll_year) + '. The earliest tax year you are able to request is ' + STRING(ll_min_yr) + '.' ,Information!) 
				idw_dw[2].SetColumn('tax_year')
				idw_dw[2].SetRow(ll_cntr2)
				idw_dw[2].SetFocus()
				RETURN -1
			END IF
			
			//User may only requests tax information up to 10 years in the past
			IF ll_year < ll_min_wrn THEN
				ll_ten_yrs = ll_ten_yrs + 1
			END IF
			
			//The most recent tax year that can be requested is the tax year prior to the current calendar year.
			IF ll_year > ll_great_yr THEN
				SQLCA.nf_rollback_transaction()
				MessageBox('Maximum Tax Year','The most recent tax year that tax information is available from the Canada Revenue Agency is ' +  STRING(ll_great_yr) + '.',Information!) 
				idw_dw[2].SetColumn('tax_year')
				idw_dw[2].SetRow(ll_cntr2)
				idw_dw[2].SetFocus()
				RETURN -1
			END IF
			
			//User should be warned that info may not be available for the prior tax year if the current date is prior to the filing deadline of April 30th
			IF (ll_year = ll_great_yr AND ldtm_current_dt <= ldtm_deadline) THEN
				MessageBox('Warning','Information for this tax year may not be available yet as the current date is prior to the filing deadline of April 30th',Information!)
			END IF
			
			//The minimum tax year that should be requested is 14 years after the claimant's date of birth
			//2015/07/28 T016641 - David Worboys Changed to an error and placed message and exit block here
			IF ll_year < ll_start_tax_year THEN
				MessageBox('Minimum Tax Year','The earliest tax year that can be requested for this Individual is the year they were 14 years of age, ' + &
							    STRING(ll_start_tax_year)  + '.  There is at least one tax year that is prior to this date',INFORMATION!) 
				idw_dw[2].SetColumn('tax_year')
				idw_dw[2].SetRow(ll_cntr2)
				idw_dw[2].SetFocus()
				RETURN -1
				
			END IF
			
			ll_cnt = ll_cnt + 1
			ll_years[ll_cnt] = ll_year
		ELSE
			idw_dw[2].DeleteRow(ll_cntr)
			ll_cntr = ll_cntr - 1
			ll_rowcount = ll_rowcount - 1
		END IF
	
	NEXT 
END IF

IF ll_ten_yrs > 0 THEN
	IF MessageBox('Minimum Tax Year','One or more tax years requested is more than 10 years in the past. Would you like to continue?', Question!, YesNo!) = 2 THEN
		idw_dw[2].SetColumn('tax_year')
		idw_dw[2].SetRow(1)
		idw_dw[2].SetFocus()
		RETURN -1
	END IF
END IF

//User must only request up to 5 different taxation years per request for a claim
IF ll_cnt > 5 THEN
	SQLCA.nf_rollback_transaction()
	MessageBox('Invalid Tax Years','You are only allowed to enter up to five different tax years per request.',Information!) 
	RETURN -1
END IF


RETURN 0
end function

public function integer nf_set_defaults ();
LONG ll_cntr
DATETIME ldtm_current_datetime

ldtm_current_datetime = f_server_datetime()

idw_dw[1].SetItem(idw_dw[1].GetRow(), 'worksafe_request_status_code', 'RP')
idw_dw[1].SetItem(idw_dw[1].GetRow(), 'claim_no', il_claim_no)
idw_dw[1].SetItem(idw_dw[1].GetRow(), 'individual_no', il_individual_no)
idw_dw[1].SetItem(idw_dw[1].GetRow(), 'request_user_id', vgst_user_profile.user_id)
idw_dw[1].SetItem(idw_dw[1].GetRow(), 'request_date', ldtm_current_datetime)
idw_dw[1].SetItem(idw_dw[1].GetRow(), 'iv_request_comment', '')





RETURN 0
end function

public function long nf_set_identifiers ();LONG ll_cntr, ll_request_no, ll_row, ll_row2


	ll_row   = idw_dw[1].GetRow()
	IF ll_row > 0 THEN
		IF 	idw_dw[1].GetItemNumber(ll_row, 'iv_request_no') = 0 OR IsNull(idw_dw[1].GetItemNumber(ll_row, 'iv_request_no')) THEN
			
			ll_request_no = nf_get_next_identifier()
			
			IF ll_request_no > 0 THEN
				idw_dw[1].SetItem(ll_row, 'iv_request_no', ll_request_no)
			END IF
		END IF
	END IF
	
	ll_row2 = idw_dw[2].RowCount()
	IF ll_row2 > 0 THEN
		ll_request_no = idw_dw[1].GetItemNumber(ll_row,'iv_request_no')
		FOR ll_cntr = 1 to idw_dw[2].RowCount() 
			IF idw_dw[2].GetItemNumber(ll_cntr, 'iv_request_no') = 0 OR IsNull(idw_dw[2].GetItemNumber(ll_cntr,'iv_request_no')) THEN				
				idw_dw[2].SetItem(ll_cntr, 'iv_request_no', ll_request_no)
			END IF
		NEXT
	END IF
	

RETURN 0
end function

public function long nf_get_next_identifier ();LONG ll_new_request_no

UPDATE Last_Iv_Request_No 
SET       last_iv_request_no =  last_iv_request_no + 1 
USING    SQLCA;

SQLCA.nf_handle_error('n_income_verification','nf_get_next_identifier','UPDATE Last_Iv_Request_No')

IF SQLCA.SQLNRows = 1 THEN
	
	SELECT last_iv_request_no 
	INTO     :ll_new_request_no
	FROM   Last_Iv_Request_No
	USING  SQLCA;

	IF SQLCA.nf_handle_error('n_income_verification','nf_get_next_identifier','SELECT last_iv_request_no + 1') < 0 THEN
		RETURN -1
	END IF
ELSE
	SQLCA.nf_rollback_transaction()
	
	MessageBox("Data Integrity Error", String(SQLCA.SQLNRows) + " record(s) found in Last_Iv_Request_No table.~r~nPlease call the help desk",Exclamation!)
	RETURN -1
END IF

RETURN ll_new_request_no


end function

public function integer nf_set_claim_info (long al_claim_no, long al_individual_no);
il_claim_no = al_claim_no
il_individual_no = al_individual_no

RETURN 0
end function

public function integer nf_delete (long al_request_no);
SQLCA.nf_begin_transaction()

//Delete detail records
DELETE IV_REQUEST_DETAIL
WHERE iv_request_no = :al_request_no
USING  SQLCA;
SQLCA.nf_handle_error('w_income_requests', 'cb_delete','DELETE IV_REQUEST_DETAIL')
	
//Delete master record once the details have been removed
DELETE IV_REQUEST
WHERE iv_request_no = :al_request_no
USING  SQLCA;
SQLCA.nf_handle_error('n_income_verification', 'nf_delete','DELETE IV_REQUEST')

SQLCA.nf_commit_transaction()


RETURN 0
end function

public function integer nf_fill_years ();LONG ll_rowcount, ll_cntr

ll_rowcount = idw_dw[2].RowCount()

IF ll_rowcount < 5 THEN
	FOR ll_cntr = ll_rowcount + 1 to 5 
		idw_dw[2].InsertRow(ll_cntr)
	NEXT
END IF

RETURN 0
end function

public function integer nf_get_individual_info (long al_individual_no, ref string as_name);STRING ls_last_name, ls_first_name

SELECT last_name, given_names
INTO :ls_last_name, :ls_first_name
FROM INDIVIDUAL
WHERE individual_no = :al_individual_no
USING  SQLCA;

SQLCA.nf_handle_error("n_income_verification", "nf_get_individual_info", "SELECT last_name, given_names")

as_name = ls_first_name + ' ' + ls_last_name

RETURN 0
end function

on n_income_verification.create
call super::create
end on

on n_income_verification.destroy
call super::destroy
end on

