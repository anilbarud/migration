$PBExportHeader$n_create_claim.sru
$PBExportComments$controller object for creating claims -links n_claim, n_individual and n_claim_participant
forward
global type n_create_claim from n_pdc
end type
end forward

global type n_create_claim from n_pdc
end type
global n_create_claim n_create_claim

type variables
N_CLAIMS inv_claims
N_MAINTAIN_CLAIM_CONTROLLER inv_claim_controller
N_CLAIM_PARTICIPANT inv_claim_participant
N_INDIVIDUAL inv_individual
end variables

forward prototypes
public subroutine nf_insert ()
public function integer nf_retrieve (long al_individual_no)
public function integer nf_check_mandatory ()
public function integer nf_log_events ()
public function integer nf_image_create ()
public function long nf_get_claim_no ()
public function integer nf_update ()
public function integer nf_retrieve2 (long al_individual_no)
public function integer nf_check_medicare (long al_medicare)
public subroutine nf_init ()
public function integer nf_change_item (long al_datawindow)
public function integer nf_check_bus_rule ()
public function long nf_set_identifiers ()
public function integer nf_disable_fields ()
public function integer nf_set_unused_fields ()
end prototypes

public subroutine nf_insert ();/*	insert a row into all visible datawindow
*/

	inv_claims.nf_insert(0)
	inv_individual.nf_insert(0)
	inv_claim_participant.nf_insert(0) 
Return
end subroutine

public function integer nf_retrieve (long al_individual_no);STRING	ls_location_code, ls_region_code

/*	This routine is performed if an individual number is passed into the window as a parameter

	dw 1 = claim create - always new
	dw 2 = individual - with alias name
	dw 3 = main name on individual
*/

	inv_claims.nf_insert(0)
	inv_claim_participant.nf_insert(0)
	inv_individual.nf_retrieve(al_individual_no)

/*	set the admin region code to the default location code that equals the individuals location code
*/
	ls_location_code = idw_dw[2].GetItemString(1, 'location_code')
	SELECT default_admin_region_code
	  INTO :ls_region_code
	  FROM Location
	 WHERE location_code = :ls_location_code;
	IF SQLCA.nf_handle_error('Embedded SQL: Location', 'n_create_claim','nf_set_unused_fields') < 0 THEN
		Return -1
	END IF
	idw_dw[1].SetItem(1,'admin_region_code', ls_region_code)
Return 1
end function

public function integer nf_check_mandatory ();LONG  ll_current_row



ll_current_row = idw_dw[1].GetRow()


IF IsNull(idw_dw[1].GetItemDateTime(ll_current_row,'accident_date')) THEN
  	MessageBox("Error","Missing accident date.  Please enter.")
	idw_dw[1].SetColumn('accident_date')
	idw_dw[1].SetFocus()
  	Return -1
END IF

IF IsNull(idw_dw[1].GetItemString(ll_current_row,'part_of_body_code')) OR Trim(idw_dw[1].GetItemString(ll_current_row,'part_of_body_code')) = '' THEN
  	MessageBox("Error","Missing part of body code.")
	idw_dw[1].SetColumn('part_of_body_code')
	idw_dw[1].SetFocus()
  	Return -1
END IF

IF IsNull(idw_dw[1].GetItemString(ll_current_row,'side_of_body_code')) OR Trim(idw_dw[1].GetItemString(ll_current_row,'side_of_body_code')) = '' THEN
  	MessageBox("Error","Missing side of body code.")
	idw_dw[1].SetColumn('side_of_body_code')
	idw_dw[1].SetFocus()
  	Return -1
END IF

IF IsNull(idw_dw[1].GetItemString(ll_current_row,'admin_region_code')) OR Trim(idw_dw[1].GetItemString(ll_current_row,'admin_region_code')) = '' THEN
  	MessageBox("Error","Missing admin region code.")
   idw_dw[1].SetColumn('admin_region_code')
	idw_dw[1].SetFocus()
  	Return -1
END IF

IF IsNull(idw_dw[1].GetItemString(ll_current_row,'claim_status_code')) OR Trim(idw_dw[1].GetItemString(ll_current_row,'claim_status_code')) = '' THEN
  	MessageBox("Missing Status code","The claim status code must be supplied.")
   idw_dw[1].SetColumn('status_code')
	idw_dw[1].SetFocus()
  	Return -1
END IF


/*	call the check mandatory functions in all the children
*/
	IF inv_individual.nf_check_mandatory() < 0 THEN Return -1
	IF inv_claim_participant.nf_check_mandatory() < 0 THEN Return -1

Return 0
end function

public function integer nf_log_events ();/*	log the creation of the claim event
*/

IF inv_claims.nf_log_events() < 0 THEN Return -1

IF NOT IsNull(idw_dw[2].GetItemNumber(1,'individual_no')) THEN
	IF inv_individual.nf_log_events() < 0 THEN Return -1
END IF

Return 0
end function

public function integer nf_image_create ();N_IMAGING    lnv_imaging

	lnv_imaging = Create n_imaging     // used to log changes in the imaging system


/*	log the creation of the claim in imaging
*/
	IF idw_dw[1].GetItemString(idw_dw[1].GetRow(),'imaged_flag') = 'Y' THEN
		
		ImageTrans.nf_begin_transaction()
		
   	IF lnv_imaging.nf_create_claimsmaster('n_create_claim.nf_image_create', idw_dw[1].GetItemNumber(idw_dw[1].GetRow(),'claim_no'), 'Y') < 1 THEN
      	Return -1
	   END IF
		
		ImageTrans.nf_commit_transaction()
	END IF




end function

public function long nf_get_claim_no ();LONG  ll_row

/*	this function gets the claim number from the first data window
	it is only to be used after nf_get_next_identifier has been called
	it will be called by the parent window to pass the claim numbe back to 
	its parent
*/

	ll_row = idw_dw[1].GetRow()
Return idw_dw[1].GetItemNumber(ll_row,'claim_no')
end function

public function integer nf_update ();STRING		ls_original_part_of_body_code, ls_original_side_of_body_code
LONG			ll_claim_no

LONG ll_error
INT  li_cntr = 1, li_bound

/*Description:		This function updates the datawindows in the array.
*/

li_bound = UpperBound(idw_dw)
DO WHILE li_cntr <= li_bound

	IF li_cntr = 5 or li_cntr = 7 THEN
		li_cntr++
	END IF 
	idw_dw[li_cntr].Update()

	inv_transobj.nf_handle_error("n_pdc","nf_update","updateing dw")
	
	li_cntr ++
LOOP

ll_claim_no                   = idw_dw[1].GetItemNumber(1,'claim_no')
ls_original_part_of_body_code = idw_dw[1].GetItemString(1,'part_of_body_code')
ls_original_side_of_body_code = idw_dw[1].GetItemString(1,'side_of_body_code')


INSERT ACCIDENT
(      claim_no,
       original_part_of_body_code,
		 original_side_of_body_code )
VALUES(:ll_claim_no,
       :ls_original_part_of_body_code,
       :ls_original_side_of_body_code )
USING inv_transobj;

inv_transobj.nf_handle_error("n_pdc","nf_update","updateing dw")

Return 0



end function

public function integer nf_retrieve2 (long al_individual_no);STRING	ls_region_code

ls_region_code = idw_dw[1].GetItemString(1,'admin_region_code')

inv_individual.nf_retrieve(al_individual_no)

//	idw_dw[1] = dw_claim_create
idw_dw[1].SetItem(1,'admin_region_code', ls_region_code)
	
Return 1
end function

public function integer nf_check_medicare (long al_medicare);LONG ll_cnt

SELECT Count(*) 
INTO   :ll_cnt
FROM   CLAIM_PARTICIPANT a,
	    INDIVIDUAL b
WHERE  a.individual_no = b.individual_no
and    b.medicare_no = :al_medicare
USING  SQLCA;

IF SQLCA.nf_handle_error('SELECT Count(*) FROM CLAIM_PARTICIPANT','n_create_claim', 'nf_check_medicare') < 0 THEN
	Return -1
END IF

IF ll_cnt = 0 THEN
	MessageBox('Duplicate Medicare','An Individual that was created through CMS already has this Medicare Number.~n~r' + &
	+ 'This Individual does not have an existing Claim, so you must create the~n~rcurrent Claim without the Medicare Number,' + &
	+ ' and go to the Individual Duplication Fix~n~rto merge the two Individuals.',information!)
END IF

RETURN 0
end function

public subroutine nf_init ();U_DWA ldw_dw[]

/*	NOTE:
	 inv_pdc_child[1]   -  claim object
	               2    -  claim participant object
	               3    -  individual object
*/	

/*	create all the objects that need controlling from here
*/

	inv_claims = Create n_claims
/*	register the parent and the datawindows
*/
	ldw_dw[1] = idw_dw[1]															// claim dw
	ldw_dw[2] = idw_dw[5]															// claim no dw
	inv_claims.nf_init(ldw_dw[], SQLCA, iwi_window_parent)

	inv_claim_participant = Create n_claim_participant
	ldw_dw[1] = idw_dw[4]															// claim participant dw
	inv_claim_participant.nf_init(ldw_dw[], SQLCA, iwi_window_parent)

	inv_individual = Create n_individual
	ldw_dw[1] = idw_dw[2]															// individual dw
	ldw_dw[2] = idw_dw[3]															// individual main name dw
	ldw_dw[3] = idw_dw[6]															// individual name (alias)
	ldw_dw[4] = idw_dw[7]															// next individual no dw
	ldw_dw[5] = idw_dw[8]															// list of claims that the individual is on
	inv_individual.nf_init(ldw_dw[], SQLCA, iwi_window_parent)
	/* PR7034 2008-02-25 R.S. Set the new claim boolean to true, so it can be verified during a claim create, 
	 	when checking the new claimant's age. The other part of this PR is in n_individual.check_bus_rule()
	 */
	inv_individual.ib_new_claim = true
	// end PR
	
/*	set up the commit flag to only allow this controller to commit
*/
	nf_set_commit(TRUE)
	inv_claims.nf_set_commit(FALSE)
	inv_claim_participant.nf_set_commit(FALSE)
	inv_individual.nf_set_commit(FALSE)

Return
end subroutine

public function integer nf_change_item (long al_datawindow);STRING 	ls_location_code, ls_region_code
/*	PURPOSE :
	get the datawindow that triggered the event  - al_datawindow
	this contains the datawindow as it is known by this object
	call the function in the appropriate child and pass 
	it the appropriate datawindow number
*/

	CHOOSE CASE al_datawindow
	   CASE 1
   	// claim create dw
		
			IF idw_dw[1].GetColumnName() = 'accident_date' THEN
/*					verify the date is less than today
*/
	            IF DateTime(Date(Left(idw_dw[1].GetText(),10))) > f_server_datetime() THEN
   	            			MessageBox("Invalid Accident Date", "The accident date cannot be beyond today.")
      	       			idw_dw[1].SetFocus()
						idw_dw[1].SetColumn('accident_date')
						RETURN -1
         	   END IF

				IF Date(Left(idw_dw[1].GetText(),10)) < Date('1900-01-01') THEN
   	           			 MessageBox( "Invalid Accident Date", "The accident date cannot be before 1900.", Exclamation! )
      	         		idw_dw[1].SetFocus()
						idw_dw[1].SetColumn('accident_date')
					  	RETURN -1

					ELSEIF Date(Left(idw_dw[1].GetText(),10)) > Date('2079-06-01') THEN
   	            				MessageBox( "Invalid Accident Date", "The accident date cannot be before 1900.", Exclamation! )
      	         			idw_dw[1].SetFocus()
							idw_dw[1].SetColumn('accident_date')
							RETURN -1

					ELSEIF Date(Left(idw_dw[1].GetText(),10)) < RelativeDate( Date( f_server_datetime()), -180 ) THEN
							MessageBox( "Date Warning", "The accident date is more than 6 months in the past.",&
												Information! )
					END IF
					
			END IF
      		IF inv_claims.nf_change_item(1) = 1 THEN
				RETURN -1
			END IF
	   CASE 2
   	// individual main dw
      	IF inv_individual.nf_change_item(1) < 0 THEN
				RETURN -1
			END IF
			IF idw_dw[2].GetColumnName() = 'location_desc2' THEN
				ls_location_code = idw_dw[2].GetItemString(1, 'location_code')
				IF NOT IsNull(ls_location_code) THEN
					SELECT default_admin_region_code
					  INTO :ls_region_code
					  FROM Location
					 WHERE location_code = :ls_location_code;
					IF SQLCA.nf_handle_error('Embedded SQL: Location', 'n_create_claim','nf_change_item') < 0 THEN
						Return -1
					END IF
					idw_dw[1].SetItem(1,'admin_region_code', ls_region_code)
				END IF
			END IF
		CASE 3
	   // individual main name dw
   	   IF inv_individual.nf_change_item(2) < 0 THEN
				RETURN -1
			END IF
	   CASE 4
   	// claim participant dw
      	inv_claim_participant.nf_change_item(1)
	   CASE ELSE 
   	   Return -1
	END CHOOSE

Return 0
end function

public function integer nf_check_bus_rule ();LONG  	ll_return_value, ll_cnt, ll_medicare, ll_current_row, ll_claim_no, ll_count_sob
Datetime ldtm_today, ldtm_create_date
Date		ldt_today
String		ls_stats_coded, ls_pob, ls_string, ls_sob
DATAWINDOWCHILD ldwc_child
DATAWINDOWCHILD ldwc_sob_child

/*	need to return what the lower functions returned - since the check for 
	individual returns
	other numbers besides -1 for an error
*/
	ll_medicare = idw_dw[2].GetItemNumber(1, 'medicare_no')
	
	ldtm_today = f_server_datetime()
	ldt_today = Date(ldtm_today)
	ll_current_row = idw_dw[1].GetRow()
	
	/*	accident date
	*/
	IF idw_dw[1].GetItemDateTime(ll_current_row,'accident_date') > ldtm_today THEN
   		MessageBox("Invalid Accident Date", "The accident date cannot be beyond today.")
	   	idw_dw[1].SetColumn('accident_date')
		idw_dw[1].SetFocus()
		Return -1
	END IF
	IF Date(idw_dw[1].GetItemDateTime(ll_current_row,'accident_date')) < Date('19000101') THEN
   		MessageBox("Invalid Accident Date", "The accident date cannot be before 1900.")
	   	idw_dw[1].SetColumn('accident_date')
		idw_dw[1].SetFocus()
   	Return -1
	END IF

	/*  Accident date SHOULD not be later than the create date
	*/
	ldtm_create_date = idw_dw[1].getitemdatetime(ll_current_row,'create_date')
		
	IF idw_dw[1].GetItemDateTime(ll_current_row,'accident_date') > ldtm_create_date THEN
   		IF MessageBox("Invalid Accident Date", 'The accident date is greater than the create date of the claim.' &
		   + '~r~nWould you like to continue?',question!,yesno!) = 2 THEN
	   		idw_dw[1].SetColumn('accident_date')
			idw_dw[1].SetFocus()
			Return -1
		END IF
	END IF	
	
	/* part of body
	Get the status of part_of_body_code and only validate if it has been modified and if stats have not been coded.
	*/	
	SELECT coding_complete_flag
	INTO	:ls_stats_coded
	FROM  CLAIM 
	WHERE claim_no = :ll_claim_no;

	IF SQLCA.nf_handle_error( 'n_create_claim', 'nf_check_bus_rule','Embedded SQL: select from CLAIM') < 0 THEN
		Return -1
	END IF
	
	IF ls_stats_coded = 'N' THEN
		ls_pob = idw_dw[1].GetItemString(ll_current_row,'part_of_body_code',Primary!,TRUE)
		ls_string = idw_dw[1].GetItemString(ll_current_row,'part_of_body_code', Primary!,FALSE)
		IF ls_string <> ls_pob THEN
			IF idw_dw[1].GetChild('part_of_body_code',ldwc_child) < 1 THEN
				MessageBox('Error Retrieveing List', 'Unable to retrieve list of valid part of body codes.  Contact help desk.')
				Return -1
			END IF
			IF ldwc_child.Find('part_of_body_code = "' + ls_string +  '"',0, ldwc_child.RowCount()) < 1 THEN
				MessageBox('Error - Part of Body','Invalid part of body code.  Please correct.')
				idw_dw[1].SetColumn('part_of_body_code')
				idw_dw[1].SetFocus()
				Return -1
			END IF 
		END IF
		
		//check Side of Body
		ls_sob = idw_dw[1].GetItemString(ll_current_row,'side_of_body_code',Primary!,TRUE)
		ls_string = idw_dw[1].GetItemString(ll_current_row,'side_of_body_code', Primary!,FALSE)
		IF ls_string <> ls_sob THEN
			IF idw_dw[1].GetChild('side_of_body_code',ldwc_sob_child) < 1 THEN
				MessageBox('Error Retrieveing List', 'Unable to retrieve list of valid side of body codes.  Contact help desk.')
				Return -1
			END IF
		END IF
		
		//Check the POB and SOB combination against the Xref table to see if it's a valid combination
		Select Count(*)
		Into :ll_count_sob
		From Part_Of_Body_xref_Side_Of_Body
		Where part_of_body_code = :ls_pob
		And side_of_body_code = :ls_sob
		And active_flag = 'Y'
		Using SQLCA;
		
		SQLCA.nf_handle_error('n_create_claim','nf_check_bus_rule','Select Count(*)')
		
		IF ll_count_sob < 1 THEN 
			MessageBox('Part of Body/Side of Body Error','The Part of Body and Side of Body combination are invalid. Please select a new combination.',Exclamation!)
			Return -1
		END IF
				
		
	END IF

	ll_return_value = inv_claims.nf_check_bus_rule_shared()
	IF ll_return_value < 0 THEN Return ll_return_value
	
	ll_return_value = inv_individual.nf_check_bus_rule()
	IF  ll_return_value < 0 THEN 
		IF ll_return_value = -202 THEN
			If nf_check_medicare(ll_medicare) < 0 THEN
				Return -1
			END IF			
		END IF
		Return ll_return_value
	END IF
	
	ll_return_value = inv_claim_participant.nf_check_bus_rule()
	IF ll_return_value < 0 THEN Return ll_return_value

/* check to make sure that the accident date is not before the birth date (if entered)
*/
	IF NOT( IsNull(idw_dw[2].GetItemDateTime(1,'birth_date'))) THEN
		IF idw_dw[1].GetItemDateTime(1,'accident_date') < idw_dw[2].GetItemDateTime(1,'birth_date') THEN
			MessageBox('Invalid Accident Date', "The accident date cannot be before the individual's birth date.")
			idw_dw[1].SetColumn("accident_date")
			idw_dw[1].SetFocus()
			Return -1
		END IF
	END IF

/* check to make sure that the accident date is before or the same as the death date
*/
	IF NOT( IsNull(idw_dw[2].GetItemDateTime(1,'death_date'))) THEN
		IF idw_dw[1].GetItemDateTime(1,'accident_date') > idw_dw[2].GetItemDateTime(1,'death_date') THEN
			MessageBox('Invalid Accident Date', "The accident date cannot be after the individual's death date.")
			idw_dw[1].SetColumn("accident_date")
			idw_dw[1].SetFocus()
			Return -1
		END IF
	END IF

/* If country is Canada and Medicare and/or Sin is missing, warn users but allow
   them to continue without them.
*/
	IF ISNULL(idw_dw[2].GetItemNumber(1,'sin_no')) OR idw_dw[2].GetItemNumber(1,'sin_no') = 0 THEN
		IF idw_dw[2].GetItemString(1,'country_code') = 'CAN' THEN
			IF MessageBox('Missing Information','S.I.N. has not been entered. Would you like to enter it now?',question!,yesno!) = 1 THEN
				RETURN -1
			END IF
		END IF
	END IF

	IF ISNULL(idw_dw[2].GetItemNumber(1,'medicare_no')) OR idw_dw[2].GetItemNumber(1,'medicare_no') = 0 THEN
		IF idw_dw[2].GetItemString(1,'country_code') = 'CAN' THEN
			IF MessageBox('Missing Information','Medicare has not been entered. Would you like to enter it now?',question!,yesno!) = 1 THEN
				RETURN -1
			END IF
		END IF
	END IF
	
Return 0
end function

public function long nf_set_identifiers ();LONG ll_claim_no, ll_individual_no, ll_row

/*	get all the keys and set on all the datawindows
*/

	ll_claim_no = inv_claims.nf_get_next_identifier()
	ll_individual_no = inv_individual.nf_get_next_identifier()

	IF ll_claim_no > 0 THEN
/*	set the claim number on the main claim dw
   Also, call the nf_set_new_claim_no() fxn to set the new claim_no in inv_individual
*/
   	ll_row = idw_dw[1].GetRow()
	   IF ll_row > 0 THEN
   	   idw_dw[1].SetItem(ll_row, 'claim_no', ll_claim_no)
			inv_individual.nf_set_new_claim_no(ll_claim_no)
	   ELSE
   	   MessageBox("Error", "Error setting claim number on main claim.")
      	Return -1
	   END IF

/*		the claim participant
*/
	   ll_row = idw_dw[4].GetRow()
   	IF ll_row > 0 THEN
      	idw_dw[4].SetItem(ll_row, 'claim_no', ll_claim_no)
	   ELSE
   	   MessageBox("Error", "Error setting claim number on claim participant.")
      	Return -1
	   END IF
	ELSE
   	MessageBox("Error - No Claim Number", "Error setting claim number - no claim number returned. See system administrator.")
	   Return -1
	END IF

	IF ll_individual_no > 0 THEN
/*		set the individual number on the main individual dw
*/
   	ll_row = idw_dw[2].GetRow()
	   IF ll_row > 0 THEN
   	   idw_dw[2].SetItem(ll_row, 'individual_no', ll_individual_no)
	   ELSE
   	   MessageBox("Error", "Error setting individual number.")
      	Return -1
	   END IF

/*		on the claim dw
*/
	   ll_row = idw_dw[1].GetRow()
   	IF ll_row > 0 THEN
      	idw_dw[1].SetItem(ll_row, 'individual_no', ll_individual_no)
	   ELSE
   	   MessageBox("Error", "Error setting individual number.")
      	Return -1
	   END IF

/*		on the individual name dw
*/
	   ll_row = idw_dw[3].GetRow()
   	IF ll_row > 0 THEN
      	idw_dw[3].SetItem(ll_row, 'individual_no', ll_individual_no)	
	   ELSE
   	   MessageBox("Error", "Error setting individual number.")
      	Return -1
	   END IF

/*		claim participant
*/
	   ll_row = idw_dw[4].GetRow()
   	IF ll_row > 0 THEN
      	idw_dw[4].SetItem(ll_row, 'individual_no', ll_individual_no)
	   ELSE
   	   MessageBox("Error", "Error setting individual number.")
      	Return -1
	   END IF

/*		the individual name table - contains the alias if there is one
*/
	   ll_row = idw_dw[6].RowCount()
   	IF ll_row > 0 THEN
      	idw_dw[6].SetItem(1, 'individual_no', ll_individual_no)
	   END IF
	ELSE
   	Return -1
	END IF

Return 0
end function

public function integer nf_disable_fields ();/* PR4125 - J. Hawker, 2005-03-09
   This functions is used to disable specific columns when 
   creating a claim for an existing individual, if there is a value
	entered. Those values must remain the same.
*/


IF idw_dw[3].GetItemString(idw_dw[3].GetRow(),'last_name') > '' THEN
	idw_dw[3].Modify("last_name.Protect=1")
	idw_dw[3].Modify("last_name.Background.Color='553648127'")
END IF

IF idw_dw[3].GetItemString(idw_dw[3].GetRow(),'given_names') > '' THEN
	idw_dw[3].Modify("given_names.Protect=1")
	idw_dw[3].Modify("given_names.Background.Color='553648127'")
END IF

IF NOT ISNULL(idw_dw[2].GetItemDateTime(idw_dw[2].GetRow(),'birth_date')) THEN
	idw_dw[2].Modify("birth_date.Protect=1")
	idw_dw[2].Modify("birth_date.Background.Color='553648127'")
END IF

IF idw_dw[2].GetItemNumber(idw_dw[2].GetRow(),'sin_no') > 0 THEN
	idw_dw[2].Modify("sin_no.Protect=1")
	idw_dw[2].Modify("sin_no.Background.Color='553648127'")
END IF

IF idw_dw[2].GetItemNumber(idw_dw[2].GetRow(),'medicare_no') > 0 THEN
	idw_dw[2].Modify("medicare_no.Protect=1")
	idw_dw[2].Modify("medicare_no.Background.Color='553648127'")
END IF

IF idw_dw[2].GetItemString(idw_dw[2].GetRow(),'sex') = 'M' OR idw_dw[2].GetItemString(idw_dw[2].GetRow(),'sex') = 'F' THEN
	idw_dw[2].Modify("sex.Protect=1")
	idw_dw[2].Modify("sex.Background.Color='553648127'")
END IF

RETURN 0
end function

public function integer nf_set_unused_fields ();STRING	ls_location_code,	ls_region_code
LONG		ll_row

/*	set up all the key columns 
	default the claim region codes to the same as the individual 
	location code region
*/
	ll_row = idw_dw[2].GetRow()
	ls_location_code = idw_dw[2].GetItemString(ll_row, 'location_code')
	SELECT county_code
	  INTO :ls_region_code
	  FROM Location
	 WHERE location_code = :ls_location_code;
	IF SQLCA.nf_handle_error('Embedded SQL: Location', 'n_create_claim','nf_set_unused_fields') < 0 THEN
		Return -1
	END IF
	ll_row = idw_dw[1].GetRow()
	idw_dw[1].SetItem(ll_row,'accident_county_code', ls_region_code)
	idw_dw[1].SetItem(ll_row,'accident_residence_county_code', ls_region_code)

/*	call the functions to set the unused fields
*/
	IF inv_claims.nf_set_unused_fields() < 0 THEN Return -1
	IF inv_individual.nf_set_unused_fields() < 0 THEN Return -1
	IF inv_claim_participant.nf_set_unused_fields() < 0 THEN Return -1

Return 0

end function

on destructor;call n_pdc::destructor;	IF IsValid(inv_claim_participant) THEN
		Destroy(inv_claim_participant)		
	END IF
	IF IsValid(inv_claims) THEN
		Destroy(inv_claims)		
	END IF
	IF IsValid(inv_individual) THEN
		Destroy(inv_individual)		
	END IF

end on

on n_create_claim.create
call super::create
end on

on n_create_claim.destroy
call super::destroy
end on

