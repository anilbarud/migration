$PBExportHeader$n_claim_employer.sru
forward
global type n_claim_employer from nonvisualobject
end type
end forward

global type n_claim_employer from nonvisualobject
end type
global n_claim_employer n_claim_employer

type variables
u_ds			ids_employer_operation

CONSTANT    INTEGER  COVERED = 1
CONSTANT    INTEGER  NOT_COVERED = -1
CONSTANT    DATE 		SIN_MANDATORY_DATE  = 2005-01-01

BOOLEAN		ib_suppress_mult_op_warning
BOOLEAN		ib_op_warning_suppressed
BOOLEAN		ib_suppress_mult_pc_warning 
BOOLEAN		ib_pc_warning_suppressed

STRING 		is_claim_employer_type_desc

BOOLEAN		ib_employer_parameters_set
BOOLEAN		ib_claim_parameters_set
BOOLEAN		ib_individual_parameters_set

//EMPLOYER
LONG			il_employer_no
LONG			il_operation_no
LONG			il_sold_to_employer_no
LONG			il_sold_to_operation_no
STRING		is_employer_type_code
STRING		is_billing_cycle_code
STRING		is_operation_status_code
STRING		is_volunteer_flag
DATETIME		idt_operation_inactive_date
DATETIME		idt_operation_start_date
STRING      is_classification_system_code
STRING      is_classification_system_type_code

//CLAIM
LONG			il_claim_no
DATETIME		idt_accident_date

//INDIVIDUAL
LONG			il_individual_no
STRING		is_claimant_full_name
LONG			il_sin_no

CONSTANT 	BOOLEAN		ib_user_confirmation_required = TRUE

CONSTANT		INTEGER		CLAIM = 1
CONSTANT    INTEGER		PAYMENT = 2

end variables

forward prototypes
public function integer nf_isvalid_accident_employer_for_claim ()
public function integer nf_isvalid_accident_employer ()
public function integer nf_set_claimant_parameters (long al_individual_no)
public function integer nf_set_claimant_parameters (string as_claimant_full_name, long al_sin_no)
public function integer nf_set_claim_parameters (long al_claim_no, datetime adt_accident_date)
public function integer nf_validate_sin_no ()
public function integer nf_validate_personal_coverage ()
private function integer nf_validate_operation_coverage ()
public function integer nf_isvalid_cost_alloc_employer_for_claim ()
public function integer nf_validate_volunteer_coverage (integer ai_cost_alloc_source)
public function integer nf_isvalid_cost_alloc_employer ()
public function integer nf_isvalid_cost_alloc_emp_for_payment (boolean ab_suppress_mult_op_warning, boolean ab_suppress_mult_pc_warning)
public subroutine nf_reset_variables ()
public function integer nf_set_employer_parameters (long al_employer_no, long al_operation_no)
end prototypes

public function integer nf_isvalid_accident_employer_for_claim ();INTEGER		li_rtn

is_claim_employer_type_desc = 'Accident Employer'

//Make sure the calling code set all the parameters before we get into validation.
IF NOT ib_employer_parameters_set Then
	SignalError(-666,'nf_set_employer_parameters must be called before calling this function.')
End if

IF NOT ib_claim_parameters_set Then
	SignalError(-666,'nf_set_claim_parameters must be called before calling this function.')
End if


li_rtn = nf_isvalid_accident_employer()
IF li_rtn = -1 Then 
	RETURN -1
END IF


//The operation must be covered on the accident date
li_rtn = nf_validate_operation_coverage()
IF li_rtn <> COVERED THEN
	RETURN li_rtn
END IF

return 1
end function

public function integer nf_isvalid_accident_employer ();//RETURN -1  INVALID -- THIS OPERATION CANNOT BE AN ACCIDENT EMPLOYER
//RETURN 1   VALID   -- THIS OPERATION CAN BE AN ACCIDENT EMPLOYER


IF NOT ib_employer_parameters_set Then
	SignalError(-666,'nf_set_employer_parameters must be called before calling this function.')
End if

//Zero is valid for an accident employer
IF il_employer_no = 0 and il_operation_no = 0 Then
	RETURN 1
END IF


//BR_2.150
//Accident employer must be "Assessed" or "Self Insured"
CHOOSE CASE is_employer_type_code
	CASE 'A','S'
		//Valid
		
	CASE 'U','P','G','R','F'
		MessageBox('Invalid employer type','The Accident Employer must be a "Assessed" or "Self Insured" employer. Choose another employer.') 
		RETURN -1
		
	CASE ELSE
		SignalError(-666,'Invalid employer type code.')			
END CHOOSE	

//BR_2.110
//The NB Govt Silicosis Reserve cannot be the accident employer
IF il_employer_no = 65009 Then
	MessageBox('Invalid employer','The accident employer cannot be 65009 - NB Govt Silicosis Reserve.')
	RETURN -1
END IF

RETURN 1
end function

public function integer nf_set_claimant_parameters (long al_individual_no);//INDIVIDUAL
il_individual_no = 0
is_claimant_full_name = ''
il_sin_no = 0
ib_individual_parameters_set = False

//INDIVIDUAL NUMBER
IF al_individual_no = 0 or IsNull(al_individual_no) Then
	SignalError(-666,'Individual number is missing.')
End if

//Get some information about the individual
SELECT given_names + ' ' + last_name
       last_name,
		 sin_no
INTO  :is_claimant_full_name,
		:il_sin_no
FROM INDIVIDUAL
WHERE individual_no = :al_individual_no
USING SQLCA;


SQLCA.nf_handle_error('n_claim_employer','nf_validate_personal_coverage','kevin select from INDIVIDUAL ' + String(al_individual_no))

IF SQLCA.SQLNRows = 0 Then
	SignalError(-666,'Invalid individual number.')
End if

IF is_claimant_full_name = '' or IsNull(is_claimant_full_name) Then
	SignalError(-666,'Invalid individual name.')
End if


is_claimant_full_name = WordCap(is_claimant_full_name)
il_individual_no = al_individual_no

ib_individual_parameters_set = TRUE

return 1
end function

public function integer nf_set_claimant_parameters (string as_claimant_full_name, long al_sin_no);/*	This function is called when the calling module is updating 
	the SIN number.
*/



//INDIVIDUAL
il_individual_no = 0
is_claimant_full_name = ''
il_sin_no = 0
ib_individual_parameters_set = False

IF IsNull(al_sin_no) or al_sin_no < 0 Then
	SignalError(-666,'Invalid sin number')
End if

is_claimant_full_name = WordCap(as_claimant_full_name)
il_sin_no = al_sin_no

ib_individual_parameters_set = TRUE

RETURN 1
end function

public function integer nf_set_claim_parameters (long al_claim_no, datetime adt_accident_date);
//CLAIM
il_claim_no = 0
SetNull(idt_accident_date)
ib_claim_parameters_set = False

IF al_claim_no <= 0 Then
	SignalError(-666,'Claim number cannot be zero')
End if


IF IsNull(adt_accident_date) Then
	SignalError(-666,'Accident date cannot be NULL.')
END IF

//Set the parameters to be used in evaludating BR's
il_claim_no 					= al_claim_no
idt_accident_date 			= adt_accident_date

ib_claim_parameters_set = TRUE

return 1
end function

public function integer nf_validate_sin_no ();//This function is not meant to validate the actual sin number
//it only determine if the sin number is required.

//Sin number is required for individuals who's claim is cost allocated and 
//the accident date is >= 2005-01-01

IF il_employer_no > 0 Then
	IF idt_accident_date >= DateTime(SIN_MANDATORY_DATE) and il_sin_no = 0 THEN
		MessageBox('Claimant SIN number missing','Claim #' + String(il_claim_no) + ' has an accident date on or after 2005-01-01. The claimant SIN number must be entered in order for the claim to be cost allocated.')
		RETURN -1
	END IF
END IF

RETURN 1
end function

public function integer nf_validate_personal_coverage ();u_ds 						lds_personal_coverage
s_window_message		lst_message
INTEGER					li_rtn	
STRING					ls_search
LONG						ll_found_row
BOOLEAN					lb_sin_match
STRING					ls_filter

//Enforces        BR 12.100
//                BR 12.110
//                BR 12.120


// do not bother with function if employer is non-assessed
IF is_employer_type_code <> 'A' THEN RETURN 1
	
//Retrieve volunteer coverage
lds_personal_coverage = create u_ds
lds_personal_coverage.dataobject = 'd_cost_alloc_personal_coverage'
lds_personal_coverage.SetTransObject(SQLCA)


//BR_12.130
//Personal coverage not available prior to TEAMS implementation
IF is_billing_cycle_code = 'A' and Date(idt_accident_date) < 1999-01-01 Then
	IF ib_pc_warning_suppressed = FALSE THEN
		MessageBox('Claim #' + String(il_claim_no) + ' - Verify coverage','Personal coverage was not available prior to 1999 for Annual Assessed employer. Cost Allocation will be allowed but you must contact Assessment Services to verify coverage.') 
	END IF
	
	IF ib_suppress_mult_pc_warning THEN
		ib_pc_warning_suppressed = TRUE
	END IF
	RETURN COVERED

//BR_12.140
//Personal coverage not available prior to MAAP implementation	
ELSEIF is_billing_cycle_code = 'M' and Date(idt_accident_date) < 1996-01-01 Then
	IF ib_pc_warning_suppressed = FALSE THEN
		MessageBox('Claim #' + String(il_claim_no) + ' - Verify coverage','Personal coverage was not available prior to 1996 for Monthly Assessed employer. Cost Allocation will be allowed but you must contact Assessment Services to verify coverage.')
	END IF
	
	IF ib_suppress_mult_pc_warning THEN
		ib_pc_warning_suppressed = TRUE
	END IF
	RETURN COVERED
	
END IF



//Determine which message to show in the title depending on what the user
//will be presented with.
IF idt_accident_date < DateTime(SIN_MANDATORY_DATE) Then
	lst_message.as_stringparm[1] = 'Claim #' + String(il_claim_no) + ' - Individuals with Personal Coverage at the time of the accident.'
Else
	lst_message.as_stringparm[1] = 'Claim #' + String(il_claim_no) + ' - Personal Coverage for ' + is_claimant_full_name
END IF



//This datawindow will retrieve all individual coverage records for the operation
//for the accident year
li_rtn = lds_personal_coverage.Retrieve(il_employer_no,il_operation_no,idt_accident_date)

IF li_rtn = -1 Then
	SignalError(-666,'Error retrieving personal coverage information')
End if

SQLCA.nf_handle_error('n_employer','nf_initialize','lds_personal_coverage.Retrieve')

//If there is no Personal Coverage records then the claimant is
//covered by the Operation Coverage
IF lds_personal_coverage.RowCount() = 0 Then
	RETURN COVERED
END IF



//SIN number is mandatory after 2005-01-01 so we must get a match on the SIN number
IF idt_accident_date >= DateTime(SIN_MANDATORY_DATE) THEN
	ls_search = 'sin_no = ' + String(il_sin_no)
	ll_found_row = lds_personal_coverage.Find(ls_search,1,lds_personal_coverage.RowCount())
	
	
	IF ll_found_row < 0 Then
		SignalError(-666,'Error finding sin_no')
		
	ELSEIF ll_found_row = 0 Then
		//If there is no Personal Coverage records that match on sin, then the claimant is
		//covered by the Operation Coverage
		RETURN COVERED
		
	ELSEIF ll_found_row > 0 Then
		//If a match has been found based on SIN then filter the datawindow
		//to show only the matching record. The user will be asked to confirm this
		//in a messagebox
		
		lb_sin_match = True
		
		li_rtn = lds_personal_coverage.SetFilter(ls_search)
		If li_rtn = -1 Then SignalError(-666,'Error setting filter for coverage.')
		
		li_rtn = lds_personal_coverage.Filter()
		If li_rtn = -1 Then SignalError(-666,'Error setting filter for coverage.')
	END IF		
End if


//At this point, we know there are personal coverage records for the accident year
//we need to filter out the ones that don't cover the accident date.
ls_search = "coverage_start_date <= Date('" + String(idt_accident_date,'yyyy-mm-dd') + "')" +&
            " and coverage_end_date   >= Date('" + String(idt_accident_date,'yyyy-mm-dd') + "')" 

ll_found_row = lds_personal_coverage.Find(ls_search,1,lds_personal_coverage.RowCount())
		
IF ll_found_row < 0 Then
	SignalError(-666,'Error finding coverage dates')
	
ELSEIF ll_found_row = 0 Then
	//The individual had personal coverage for the accident year but not on the accident date.
	IF idt_accident_date >= DateTime(SIN_MANDATORY_DATE) and lb_sin_match = True Then //2005-01-01 and later
		lst_message.as_stringparm[2] = 'Claimant, ' + is_claimant_full_name + ', did NOT have Personal Coverage with employer / operation (' + String(il_employer_no) + ' / ' + String(il_operation_no)  + ') at the time of the accident (' +&
		                               String(idt_accident_date,'yyyy-mm-dd') + ').~r~n~r~n' +&
												 'You must consult with Assessment Services to ensure the personal coverage information  is correct.~r~nThe person may not have coverage at all, or  the type of coverage may affect the benefit level.~r~n~r~n' +&
												 'Did s/he have coverage at the time of the accident (' + String(idt_accident_date,'yyyy-mm-dd') + ')?'
												 
	Elseif idt_accident_date < DateTime(SIN_MANDATORY_DATE) Then
		//Must be earlier than 2005-01-01 and no match on dates
		//Then the individual is covered under Operation Coverage
		RETURN COVERED
		
	End if
	
ELSEIF ll_found_row > 0 Then
	//If a match has been found based on coverage_dates then filter the datawindow
	//to show only the matching record. The user will be asked to confirm this
	//in a messagebox
	
	//If the datawindow has already been fitlered for sin the add the 
	//coverage filter to it.
	IF ls_filter <> '' Then
		ls_filter = ' and ' + ls_search
	Else
		ls_filter = ls_search
	End if
	
	
	li_rtn = lds_personal_coverage.SetFilter(ls_filter)
	If li_rtn = -1 Then SignalError(-666,'Error setting filter for coverage.')
	
	li_rtn = lds_personal_coverage.Filter()
	If li_rtn = -1 Then SignalError(-666,'Error setting filter for coverage.')
	
	lst_message.as_stringparm[2] = 'Did claimant, ' + is_claimant_full_name + ', have Personal Coverage with employer / operation (' + String(il_employer_no) + ' / ' + String(il_operation_no)  + ') at the time of the accident (' + String(idt_accident_date,'yyyy-mm-dd') + ')?'
	
END IF


//For accidents before 2005-01-01 if there are any volunteer records that cover
//the accident date, we will show them to the user and ask the user to confirm
//that the claimant had coverage at the time of the accident.
lst_message.apo_powerobjectparm[1] = lds_personal_coverage

IF ib_user_confirmation_required THEN
	//Show message
	OpenWithParm(w_individual_coverage_confirmation_list,lst_message)
	//Doubleparm contains the users response
	RETURN message.doubleparm
ELSE
	//If user confirmation is not required then just return 1 (valid)
	RETURN 1
END IF


end function

private function integer nf_validate_operation_coverage ();INTEGER			li_rtn
STRING			ls_employer_operation
u_ds				lds_seasonal_coverage


ls_employer_operation = String(il_employer_no) + '/' + String(il_operation_no)


//Only "Assessed" and "Self Insured" employers have a coverage period
//other Employer Accounts have continuous coverage from "The Dawn of Time" until "Judgment Day".
IF is_employer_type_code <> 'A' and is_employer_type_code <> 'S' Then
	RETURN COVERED
END IF



//BR_11.50 START  *********
//If the inactive date is filled in, make sure it is after the accident date
IF NOT IsNull(idt_operation_inactive_date) Then
	
	IF idt_operation_inactive_date < idt_accident_date Then
		//The accident occured after the operation became inactive. We will prevent this but 
		//first we must determine which error message to give the user.		
		
		//If the accident happend after the operation became inactive, prevent cost allocation, 
		//if the operation was sold to another operation inform the user so they can cost allocate
		//to the sold to employer.	
		IF il_sold_to_employer_no > 0 Then //Operation was sold			
		
			MessageBox('Employer/Group Sold',"Employer " + String(il_employer_no) + " Operation " +&
							  String(il_operation_no) + " was sold to " + " Employer " +&
							  String(il_sold_to_employer_no) + " Operation " + String(il_sold_to_operation_no) + ".~r~r" +&
							  "You must Cost allocate to Employer " + String(il_sold_to_employer_no) + " Operation " +&
							  String(il_sold_to_operation_no) + ".")

		Elseif Il_sold_to_employer_no = 0 Then
			
		//If the accident happend after the operation became inactive, and the operation was NOT sold
		//to another operation, prevent cost allocation.
			MessageBox('Invalid ' + is_claim_employer_type_desc,is_claim_employer_type_desc + ' Operation (' + ls_employer_operation + ') became inactive on ' + String(idt_operation_inactive_date,'yyyy-mm-dd')+&
			', and the accident did not occur until ' + String(idt_accident_date,'yyyy-mm-dd') + '. Choose another Employer / Operation.')
		
		Else
			SignalError(-666,'Invalid "Sold to" data.')
			
		End if
		
		RETURN NOT_COVERED
	END IF
End if
//BR_11.50 END   **********



//BR_11.40 START **********
//For self insured employers coverage is "the beginning of time" so don't
//validate the accident date against the start_date. Only Assessed employers
//have their operation_start_date validated against the accident date.
IF idt_operation_start_date > idt_accident_date Then
	if is_employer_type_code = 'A' Then	

		MessageBox('Invalid ' + is_claim_employer_type_desc,'Operation (' + ls_employer_operation + ') did not start operations until ' + String(ids_employer_operation.GetItemDateTime(1,'operation_start_date'),'yyyy-mm-dd')+&
						', and the accident occured on ' + String(idt_accident_date,'yyyy-mm-dd') + '. Choose another employer/operation.')
		
		RETURN NOT_COVERED		
	ELSEIF is_employer_type_code = 'S' Then
		
		IF ib_op_warning_suppressed = FALSE THEN
			MessageBox(is_claim_employer_type_desc + ' Warning','Operation (' + ls_employer_operation + ') did not start operations until ' + String(ids_employer_operation.GetItemDateTime(1,'operation_start_date'),'yyyy-mm-dd')+&
							', and the accident occured on ' + String(idt_accident_date,'yyyy-mm-dd') + '. This is just a warning.')
		END IF
		
		IF ib_suppress_mult_op_warning THEN
			// prevent multiple warnings
			ib_op_warning_suppressed = TRUE
		END IF 
		RETURN COVERED		
	END IF
END IF



//BR_11.60 START **********

lds_seasonal_coverage = create u_ds
lds_seasonal_coverage.DataObject = 'd_cost_alloc_seasonal_coverage'
lds_seasonal_coverage.SetTransObject(SQLCA)

//Returns all active seasonal coverage records for the accident year
li_rtn = lds_seasonal_coverage.Retrieve(il_employer_no,il_operation_no,idt_accident_date)

IF li_rtn = -1 Then
	SignalError(-666,'Error retrieving seasonal coverage information')
End if

SQLCA.nf_handle_error('n_employer','nf_initialize','lds_seasonal_coverage.Retrieve')

//If seasonal coverage exists
IF lds_seasonal_coverage.RowCount() > 0 Then
	IF lds_seasonal_coverage.GetItemDateTime(1,'coverage_start_date') > idt_accident_date Then
		MessageBox('Invalid ' + is_claim_employer_type_desc,'In ' + String(year(Date(idt_accident_date))) + ', Seasonal Operation (' + ls_employer_operation + ') coverage started on ' + String(lds_seasonal_coverage.GetItemDateTime(1,'coverage_start_date'),'yyyy-mm-dd')+&
						', and the accident occured on ' + String(idt_accident_date,'yyyy-mm-dd') + '. Choose another employer/operation.')
		
		RETURN NOT_COVERED
	END IF
	
	IF lds_seasonal_coverage.GetItemDateTime(1,'coverage_end_date') < idt_accident_date Then
		MessageBox('Invalid ' + is_claim_employer_type_desc,'In ' + String(year(Date(idt_accident_date))) + ', Seasonal Operation (' + ls_employer_operation + ') was only covered until ' + String(lds_seasonal_coverage.GetItemDateTime(1,'coverage_end_date'),'yyyy-mm-dd')+&
						', and the accident did not occur until ' + String(idt_accident_date,'yyyy-mm-dd') + '. Choose another employer/operation.')
		
		RETURN NOT_COVERED
	END IF
END IF



RETURN COVERED
end function

public function integer nf_isvalid_cost_alloc_employer_for_claim ();INTEGER		li_rtn


is_claim_employer_type_desc = 'Cost Allocation Employer'

//Make sure the calling code set all the parameters before we get into validation.
IF NOT ib_employer_parameters_set Then
	SignalError(-666,'nf_set_employer_parameters must be called before calling this function.')
End if

IF NOT ib_claim_parameters_set Then
	SignalError(-666,'nf_set_claim_parameters must be called before calling this function.')
End if

IF NOT ib_individual_parameters_set Then
	SignalError(-666,'nf_set_claimant_parameters must be called before calling this function.')
End if


li_rtn = nf_isvalid_cost_alloc_employer()
IF li_rtn = -1 Then 
	RETURN -1
END IF

IF is_employer_type_code = 'P' Then
	MessageBox('Claim #' + String(il_claim_no) + ' - Invalid employer type','The Cost Allocation Employer must not be the "Pension" account.')
	return -1
End if


//The operation must be covered on the accident date
li_rtn = nf_validate_operation_coverage()
			
IF li_rtn <> COVERED THEN RETURN li_rtn 


// Validate that if the accident >= 2005, the SIN exists for the claimant
li_rtn = nf_validate_sin_no()

IF li_rtn <> COVERED THEN RETURN li_rtn 


//The individual must have been covered on the accident date
IF is_volunteer_flag = 'Y' Then
	li_rtn = nf_validate_volunteer_coverage(CLAIM)
Else
	li_rtn = nf_validate_personal_coverage()
End if
			
IF li_rtn <> COVERED THEN RETURN li_rtn 



RETURN 1
end function

public function integer nf_validate_volunteer_coverage (integer ai_cost_alloc_source);u_ds 						lds_volunteer_coverage
s_window_message		lst_message
INTEGER					li_rtn	
STRING					ls_search
LONG						ll_found_row
STRING					ls_title
STRING					ls_message_body


STRING					ls_filter


//Enforces BR 12.60
//         BR 12.70
//         BR 12.80
//         BR 12.90

	
//Retrieve volunteer coverage
lds_volunteer_coverage = create u_ds

//Determine the coverage type we are validating and load the appropriate dataobject
//Personal and Volunteer coverage are mutually exclusive
lds_volunteer_coverage.dataobject = 'd_cost_alloc_volunteer_coverage'
lds_volunteer_coverage.SetTransObject(SQLCA)


//Determine which message to show in the title depending on what the user
//will be presented with.
IF idt_accident_date < DateTime(SIN_MANDATORY_DATE) Then
	lst_message.as_stringparm[1] = 'Claim #' + String(il_claim_no) + ' - Individuals with Volunteer Coverage at the time of the accident.'
Else
	lst_message.as_stringparm[1] = 'Claim #' + String(il_claim_no) + ' - Volunteer Coverage for ' + is_claimant_full_name
END IF

lst_message.as_stringparm[2] = 'Did claimant, ' + is_claimant_full_name + ', have Volunteer Coverage with Employer / Operation (' + String(il_employer_no) + ' / ' + String(il_operation_no) + ') at the time of the accident (' + String(idt_accident_date,'yyyy-mm-dd') + ')?'



//This datawindow will retrieve all individual coverage records for the operation
//that cover the accident date
li_rtn = lds_volunteer_coverage.Retrieve(il_employer_no,il_operation_no,idt_accident_date)

IF li_rtn = -1 Then
	SignalError(-666,'Error retrieving coverage information')
End if

SQLCA.nf_handle_error('n_employer','nf_initialize','lds_volunteer_coverage.Retrieve')

IF lds_volunteer_coverage.RowCount() = 0 Then
	MessageBox('Claim #' + String(il_claim_no) + ' - No coverage','Employer / Operation (' + String(il_employer_no) + ' / ' + String(il_operation_no) + ') has no volunteer coverage for the year of the accident (' + String(Year(Date(idt_accident_date))) + '). Call Assessment Services.')
	RETURN NOT_COVERED
END IF



//SIN number is mandatory after 2005-01-01 so we must get a match on the SIN number
IF idt_accident_date >= DateTime(SIN_MANDATORY_DATE) THEN
	
	//We must find a match on SIN after 2005-01-01
	ls_search = 'sin_no = ' + String(il_sin_no)
	ll_found_row = lds_volunteer_coverage.Find(ls_search,1,lds_volunteer_coverage.RowCount())
		
	IF ll_found_row < 0 Then
		SignalError(-666,'Error finding sin_no')
		
	ELSEIF ll_found_row = 0 Then
		
		IF ai_cost_alloc_source = CLAIM THEN
				
			MessageBox('Claim #' + String(il_claim_no) + ' - No Volunteer Coverage Found','This claim is cost allocated to a volunteer Employer / Operation (' + String(il_employer_no) + ' / ' + String(il_operation_no) + '). The individual, identified by SIN - ' + String(il_sin_no,'###-###-###') + ', has no volunteer coverage for the year of the accident (' + String(Year(Date(idt_accident_date))) + '). Call Assessment Services.')
			RETURN NOT_COVERED
		ELSE
			MessageBox('Claim #' + String(il_claim_no) + ' - No Volunteer Coverage Found','This payment must not be cost allocated to volunteer Employer / Operation (' + String(il_employer_no) + ' / ' + String(il_operation_no) + '). The individual, identified by SIN - ' + String(il_sin_no,'###-###-###') + ', has no volunteer coverage for the year of the accident (' + String(Year(Date(idt_accident_date))) + '). Call Assessment Services.')
			RETURN NOT_COVERED
		END IF
	
	ELSEIF ll_found_row > 0 Then
		//If a match has been found based on SIN then filter the datawindow
		//to show only the matching record. The user will be asked to confirm this
		//in a messagebox
		
		ls_filter = ls_search
		li_rtn = lds_volunteer_coverage.SetFilter(ls_filter)
		If li_rtn = -1 Then SignalError(-666,'Error setting filter for coverage.')
		
		li_rtn = lds_volunteer_coverage.Filter()
		If li_rtn = -1 Then SignalError(-666,'Error setting filter for coverage.')
		
	END IF		
End if



//At this point, we know there are volunteer records for the accident year
//we need to filter out the ones that don't cover the accident date.
ls_search = "coverage_start_date <= Date('" + String(idt_accident_date,'yyyy-mm-dd') + "')" +&
            " and coverage_end_date   >= Date('" + String(idt_accident_date,'yyyy-mm-dd') + "')" 

ll_found_row = lds_volunteer_coverage.Find(ls_search,1,lds_volunteer_coverage.RowCount())
		
IF ll_found_row < 0 Then
	SignalError(-666,'Error finding coverage dates')
	
ELSEIF ll_found_row = 0 Then
	ls_title = 'Claim #' + String(il_claim_no) + ' - No Volunteer Coverage'
	ls_message_body = 'Employer / Operation (' + String(il_employer_no) + '/' + String(il_operation_no) + ') has no volunteer coverage for the accident date (' + String(idt_accident_date,'yyyy-mm-dd') + ').'

	IF ai_cost_alloc_source = CLAIM OR (ai_cost_alloc_source = PAYMENT and idt_accident_date >= DateTime(SIN_MANDATORY_DATE)) THEN
		
		MessageBox(ls_title,ls_message_body + ' Call Assessment Services.')
		RETURN NOT_COVERED
	Elseif ai_cost_alloc_source = PAYMENT and idt_accident_date < DateTime(SIN_MANDATORY_DATE) THEN
		li_rtn = MessageBox(ls_title, ls_message_body + ' Do you want to continue?',Question!,YesNo!,2)
		IF li_rtn = 2 Then
			RETURN NOT_COVERED
		END IF
	Else
		SignalError(-666,'n_claim_employer.nf_validate_volunteer_coverage error.')
	End if
ELSEIF ll_found_row > 0 Then
	
	//If the datawindow has already been fitlered for sin then add the 
	//coverage filter to it.
	IF ls_filter <> '' Then
		ls_filter += ' and ' + ls_search
	Else
		ls_filter = ls_search
	End if
	
	
	li_rtn = lds_volunteer_coverage.SetFilter(ls_filter)
	If li_rtn = -1 Then SignalError(-666,'Error setting filter for coverage. Filter = "' + ls_filter + '".')
	
	li_rtn = lds_volunteer_coverage.Filter()
	If li_rtn = -1 Then SignalError(-666,'Error setting filter for coverage.')
	
END IF		

//For accidents before 2005-01-01 if there are any volunteer records that cover
//the accident date, we will show them to the user and ask the user to confirm
//that the claimant had coverage at the time of the accident.
lst_message.apo_powerobjectparm[1] = lds_volunteer_coverage

		
IF ib_user_confirmation_required THEN
	//Show message
	OpenWithParm(w_individual_coverage_confirmation_list,lst_message)
	//Doubleparm contains the users response
	RETURN message.doubleparm
ELSE
	//If user confirmation is not required then just return 1 (valid)
	RETURN 1
END IF






end function

public function integer nf_isvalid_cost_alloc_employer ();//RETURN -1  INVALID -- THIS OPERATION CANNOT BE A CLAIMS COST ALLOCATION EMPLOYER
//RETURN 1   VALID   -- THIS OPERATION CAN BE A CLAIMS COST ALLOCATION EMPLOYER


IF NOT ib_employer_parameters_set Then
	SignalError(-666,'nf_set_employer_parameters must be called before calling this function.')
End if

//Zero is valid for a cost allocation employer
IF il_employer_no = 0 and il_operation_no = 0 Then
	RETURN 1
END IF


//BR_12.20 
//Cannot cost allocate to "unregistered" or "Pension" employers
CHOOSE CASE is_employer_type_code
	CASE 'A','G','R','S','F'
		//Valid
		
	CASE 'U'		
		MessageBox('Claim #' + String(il_claim_no) + ' - Invalid employer type','The Cost Allocation Employer cannot be an "Unregistered" employer. Choose another employer.') 
		RETURN -1
				
END CHOOSE	


//BR_12.30
//Cannot cost allocate to special reserve account used for Special Survivor Payments.
IF il_employer_no = 6000 or il_employer_no = 7000 Then
	MessageBox('Claim #' + String(il_claim_no) + ' - Invalid employer','Employer #' + String(il_employer_no) + ' is a Special Reserve Account and cannot be used as the ' + is_claim_employer_type_desc + '.')
	RETURN -1
END IF


//BR_12.40
//Cannot cost allocate to "pending" operation
IF is_operation_status_code = 'P' THEN
	MessageBox('Claim #' + String(il_claim_no) + ' - Invalid operation status','Cost Allocation to a "Pending" Operation is not permitted.')
	RETURN -1
END IF



RETURN 1
end function

public function integer nf_isvalid_cost_alloc_emp_for_payment (boolean ab_suppress_mult_op_warning, boolean ab_suppress_mult_pc_warning);INTEGER		li_rtn

ib_suppress_mult_op_warning = ab_suppress_mult_op_warning
ib_suppress_mult_pc_warning = ab_suppress_mult_pc_warning

is_claim_employer_type_desc = 'Cost Allocation Employer'

//Make sure the calling code set all the parameters before we get into validation.
IF NOT ib_employer_parameters_set Then
	SignalError(-666,'nf_set_employer_parameters must be called before calling this function.')
End if

IF NOT ib_claim_parameters_set Then
	SignalError(-666,'nf_set_claim_parameters must be called before calling this function.')
End if

IF NOT ib_individual_parameters_set Then
	SignalError(-666,'nf_set_claimant_parameters must be called before calling this function.')
End if


li_rtn = nf_isvalid_cost_alloc_employer()
IF li_rtn = -1 Then 
	RETURN -1
END IF


//The operation must be covered on the accident date
li_rtn = nf_validate_operation_coverage()
			
IF li_rtn <> COVERED THEN RETURN li_rtn 

// Validate that if the accident >= 2005, the SIN exists for the claimant
li_rtn = nf_validate_sin_no()

IF li_rtn <> COVERED THEN RETURN li_rtn 

//The individual must have been covered on the accident date
IF is_volunteer_flag = 'Y' Then
	li_rtn = nf_validate_volunteer_coverage(PAYMENT)
Else
	li_rtn = nf_validate_personal_coverage()
End if
			
IF li_rtn <> COVERED THEN RETURN li_rtn 



RETURN 1
end function

public subroutine nf_reset_variables ();ib_suppress_mult_op_warning = FALSE
ib_suppress_mult_pc_warning = FALSE
ib_op_warning_suppressed = FALSE
ib_pc_warning_suppressed = FALSE
end subroutine

public function integer nf_set_employer_parameters (long al_employer_no, long al_operation_no);//RETURN 1  -- VALID (OPERATION EXISTS)
//RETURN 0  -- VALID (NO OPERATION FILLED IN)
//RETURN -1 -- INVALID (OPERATION DOESN'T EXIST)

//EMPLOYER
il_employer_no					= 0
il_operation_no	= 0
il_sold_to_employer_no	= 0
il_sold_to_operation_no	= 0
is_employer_type_code = ''
is_billing_cycle_code = ''
is_operation_status_code = ''
is_volunteer_flag = ''
SetNull(idt_operation_inactive_date)
SetNull(idt_operation_start_date)
ib_employer_parameters_set = False
is_classification_system_code = ''
is_classification_system_type_code = ''

INTEGER		li_rtn

//Employer/Operation cannot be negative
IF al_employer_no < 0 or al_operation_no < 0 Then
	SignalError(-666,'Employer/operation cannot be negative.')
END IF

//Null values are not allowed
If IsNull(al_employer_no)  or IsNull(al_operation_no) Then
	SignalError(-666,'Employer/operation is null.')
END IF

//Zeros are valid as parameters. If cost allocation is required then zeros are not allowed
IF al_employer_no = 0 and al_operation_no = 0 Then
	SignalError(-666,'0/0 is not an Employer/operation.')
End if


//Make sure the employer exists in the OPERATION table
li_rtn = ids_employer_operation.Retrieve(al_employer_no,al_operation_no)
IF li_rtn = -1 Then	SignalError(-666,'Error retrieving employer information')

SQLCA.nf_handle_error('n_employer','nf_initialize','li_rtn = ids_employer_operation.Retrieve(al_employer_no,al_operation_no)')

IF ids_employer_operation.RowCount() = 0 Then
	MessageBox('Invalid employer/operation','Employer (' + String(al_employer_no) + ') operation (' + String(al_operation_no) + ') does not exist.')
	RETURN -1
End if



//Set the instance variables to be used in validating the BR's
il_employer_no 			 		= al_employer_no
il_operation_no 			 		= al_operation_no
is_employer_type_code 	 		= ids_employer_operation.GetItemString(1,'employer_type_code')
is_billing_cycle_code  	 		= ids_employer_operation.GetItemString(1,'billing_cycle_code')
is_operation_status_code 		= ids_employer_operation.GetItemString(1,'operation_status_code')
is_volunteer_flag			 		= ids_employer_operation.GetItemString(1,'volunteer_flag')
idt_operation_inactive_date 	= ids_employer_operation.GetItemDateTime(1,'operation_inactive_date')
idt_operation_start_date		= ids_employer_operation.GetItemDateTime(1,'operation_start_date')
il_sold_to_employer_no 			= ids_employer_operation.GetItemNumber(1,'sold_to_employer_no')
il_sold_to_operation_no 		= ids_employer_operation.GetItemNumber(1,'sold_to_operation_no')
is_classification_system_code   = ids_employer_operation.GetItemString(1,'classification_system_code')
is_classification_system_type_code   = ids_employer_operation.GetItemString(1,'classification_system_type_code')


ib_employer_parameters_set = TRUE

RETURN 1
end function

on n_claim_employer.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_claim_employer.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

event constructor;
ids_employer_operation = create u_ds
ids_employer_operation.dataobject = 'd_employer_cost_alloc_validation'
ids_employer_operation.SetTransObject(SQLCA)

end event

