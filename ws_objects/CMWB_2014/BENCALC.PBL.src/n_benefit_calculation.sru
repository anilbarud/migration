$PBExportHeader$n_benefit_calculation.sru
forward
global type n_benefit_calculation from nonvisualobject
end type
end forward

global type n_benefit_calculation from nonvisualobject
end type
global n_benefit_calculation n_benefit_calculation

type variables
LONG   il_claim_no
// - The annual cpp deduction calculated by routine 100-cpp
decimal idec_cpp_rate //- The applicable cpp rate for a given year
decimal idec_cpp_max //- The maximum annual cpp deduction for a given year
decimal idec_cpp_exempt // - The applicable annual cpp exemption for a given year
decimal idec_uic_rate  // - the applicable uic rate for a given year
decimal idec_uic_max_premium // - the applicable maximum annual uic premium for a given year
decimal idec_uic_max_earnings //- The applicable maximum annual uic insurable earnings for a given year
decimal idec_uic_min          //- the applicable minimum annual insurable earnings for a given year
decimal idec_R
decimal idec_K
decimal idec_max_annual_gross
decimal idec_max_td1
decimal idec_V
decimal idec_nb_v
decimal idec_annual_gross
decimal idec_cppd_index
decimal idec_cpi_index
decimal idec_basic_pers_exempt
decimal idec_married_exemption
decimal idec_fixed_amount
decimal idec_income_parameter_rate
decimal idec_KP 	//sr64
decimal idec_prov_basic_exempt
decimal idec_prov_married_exempt
decimal idec_can_emp_cr

date id_accident_date
date id_index_date
datetime idt_from_yearly_factor
integer ii_assessment_year,ii_period
integer ii_release
end variables

forward prototypes
public function long f_get_unapplied_claim_txn (long val_claim_no, long val_benefit_calculation_no)
public function decimal f_calculate_uic (date vad_index_date, decimal vac_annual_gross)
public function boolean f_isvalid_tax_year (date vad_tax_date)
public function long f_retrieve_payment_no (string vas_where_criteria)
public function long f_get_next_ben_calc_no (long al_claim_no)
public function integer f_lock_opening (long al_claim_no, long al_opening_no, date adt_accident_recurrence_date)
public function long f_retrieve_bencalc_no (long al_claimno, long al_bencalcno)
public function datetime f_isvalid_effective_from_date (long al_claimno, long al_openingno)
public function long f_retrieve_cost_analysis_option (long al_claimno, long al_bencalcno)
public function decimal f_calculate_cpp (date adt_index_date, decimal adec_annual_gross, datetime adt_create_datetime)
public function decimal f_calc_annual_net_income (decimal ad_annual_gross, decimal ad_cpp_return, decimal ad_uic_return, decimal ad_income_tax_payable)
public function decimal f_calc_annual_taxable_income (decimal ad_annual_gross, decimal ad_uic_return, decimal ad_cpp_return)
public subroutine f_set_index_date (date adt_date)
public subroutine f_set_td1_amount (decimal ad_td_one)
public subroutine f_set_annual_gross (decimal adc_amount)
public function boolean f_check_effective_from_date (date adt_effective_from)
public function integer f_check_amounts (integer ai_control, decimal ad_amount_one, decimal ad_amount_two, ref string as_message)
public function decimal f_calc_pre93_tax (decimal ad_income_amount, decimal ad_uic_return, decimal ad_cpp_return)
public subroutine f_set_prov_td1 (decimal adec_prov_td1)
public function integer f_populate_work_variables (date adt_effective, string as_earnings_type)
public function integer f_populate_bencalc_vars (long al_claim_no, integer al_benefit_calc_no, integer al_opening_no)
public function decimal f_calculate_tax (date vad_index_date, decimal vac_exemption_amt, decimal vac_prov_exemption, decimal vac_annual_gross, decimal c, decimal u, datetime adt_create_datetime)
public function integer f_get_r_and_k (date adt_from_date, decimal adc_amount)
public function integer f_get_v_and_kp (date ad_index_date, decimal adec_annual_tax)
public function integer f_get_exemption_amounts (date adt_index_date, ref decimal vac_td1_basic_exemption, ref decimal vac_td1_married_exemption, ref decimal adec_prov_td1_basic, ref decimal adec_prov_td1_married)
public function integer f_get_income_parameter (date adt_from_date, decimal adc_amount)
public function decimal f_get_max_avg_earnings (date adt_index_date)
public function decimal f_index_cpi_amount (decimal vac_amount, long val_base_year, date adt_index_date)
public function decimal f_index_cppd_amount (decimal vac_amount, long val_base_year, date adt_index_date)
public function integer f_get_max_release (integer ai_old_release, datetime adtm_yearly_factor)
public function integer f_get_release_no (long al_bencalc_no, long al_claim_no, ref datetime adtm_yearly_factor)
public subroutine f_set_release_no (integer ai_release_no)
public function long f_retrieve_benefit_entitlement (long al_claimno, long al_bencalcno)
end prototypes

public function long f_get_unapplied_claim_txn (long val_claim_no, long val_benefit_calculation_no);/* ---------------------------------------------------------------------------------------------------- */
/* Function Name: f_get_unapplied_claim_txn                                                             */
/*                                                                                                      */
/* Purpose:       The purpose of this function is to find the latest unapplied_claim_txn record for the */
/*						claim_no and benefit_calculation_no passed                                            */
/*                                                                                                      */
/* Arguments:		Arguments Passed:                                                                     */
/*																																		  */
/*						val_claim_no 					- The claim number to search for                           */
/*						val_benefit_calculation_no - The benefit calculation number to search for             */
/*																																		  */
/* Return Values: Successful	-	Returns the txn_no (or 0, if none found)                               */
/*                Failure		-	-1  (database error)                                                   */
/* ---------------------------------------------------------------------------------------------------- */

Long		vll_txn_no = 0

//	
//	Retrieve Unapplied Claim Transaction


  SELECT max(UNAPPLIED_CLAIM_TXN.txn_no)
    INTO :vll_txn_no  
    FROM PAYMENT,   
         UNAPPLIED_CLAIM_TXN  
   WHERE ( PAYMENT.payment_no = UNAPPLIED_CLAIM_TXN.payment_no ) and  
         ( ( PAYMENT.claim_no = :val_claim_no ) AND  
         ( PAYMENT.benefit_calculation_no = :val_benefit_calculation_no ) )   ;

	SQLCA.nf_handle_error("Embedded SQL: Select from UNAPPLIED_CLAIM_TXN","n_benefit_calculation","f_get_unapplied_claim_txn")

	If IsNull(vll_txn_no) Then vll_txn_no = 0

	Return vll_txn_no

end function

public function decimal f_calculate_uic (date vad_index_date, decimal vac_annual_gross);/* ----------------------------------------------------------------------------------------------------	*/
/* Function Name: f_calculate_uic																								*/
/*																																			*/
/* Purpose:       This function calculates UIC using figures based on the index date passed.					*/
/*																																			*/
/* Arguments:     Parameters passed by calling routine:																	*/
/*																																			*/
/*                vad_index_date     - The date for which the calculation is to be performed (must			*/
/*                                     be a date not a year since tax calculations can change				*/
/*                                     within a year)																		*/
/*                vac_annual_gross   - The annual gross income for which UIC is applicable					*/
/*																																			*/
/* Return Values: Annual CPP																										*/
/*																																			*/
/* ----------------------------------------------------------------------------------------------------	*/


	Decimal	vlc_work,	U

//
// Check the date passed. If there is an available tax routine, call it.
//	Otherwise, return 0
//


// ----------------------------------------------------------------------------------
// Calculate the annual Unemployment Insurance premium as follows:
// 
//	U =	uic_rate * vac_annual_gross
//			where:
//				U is either zero or a positive number no greater than uic_max2 and
//				vac_annual_gross is greater than or equal to uic_min
// ----------------------------------------------------------------------------------
//   IF ii_period < 31 THEN//("1997 01 01")
		If vac_annual_gross >= idec_uic_min then
			If vac_annual_gross >= idec_uic_max_earnings then
				U = idec_uic_max_premium
			Else
				U = round(vac_annual_gross * idec_uic_rate,2)
			End If
		Else
			U	=	0
		End If
//	ELSE
///*	Starting in 1997, there is no longer a minimum. Therefore a small formula change to deduct from 
//	the insurable earnings until the maximum is reached
//*/
//		IF vac_annual_gross >= idec_uic_max_earnings THEN
//			U = idec_uic_max_premium			
//		ELSE	
//			U = Round(vac_annual_gross * idec_uic_rate,2)
//		END IF
//	END IF

Return U
end function

public function boolean f_isvalid_tax_year (date vad_tax_date);/* ----------------------------------------------------------------------------------------------------	*/
/*	Function Name: 	f_isvalid_tax_year																						*/
/*																																			*/
/*	Purpose:				This function checks that tax routines are available for a given tax date				*/
/*																																			*/
/*																																			*/
/*	Arguments:			Date	-	vad_tax_date	-	The tax date to be tested											*/
/*																																			*/
/*																																			*/
/*																																			*/
/*	Return Value:		Boolean	-	True					-	Returned when tax date is valid							*/
/*										-	False					-	Returned when tax date is not valid						*/
/*																																			*/
/* ----------------------------------------------------------------------------------------------------	*/

/* this code has changed to allow for calculation of tax years previous to "1993-01-01"
   the above date has been replaced with "1982-01-01" for project P10105
*/

/*

Checks to see if the Effective from date is greater then 
the minimum date that is held by the Yearly_Factor table and less then the max date

Pass in effective from date

RETURNS
TRUE  - date is alright
FALSE - date fails validation

*/
BOOLEAN lb_check
DATETIME	ldt_min_date,ldt_max_date

SELECT min(yearly_factor_date) , max(yearly_factor_date)
INTO :ldt_min_date, :ldt_max_date
FROM Yearly_Factor
USING SQLCA;

SQLCA.nf_handle_error("SELECT min(yearly_factor_date) ","n_benefit_calculation","f_isvalid_tax_year")

IF vad_tax_date < Date(ldt_min_date) OR Year(vad_tax_date) > Year(Date(ldt_max_date)) THEN //sr64
	lb_check = FALSE
ELSE
	lb_check = TRUE
END IF

RETURN lb_check

end function

public function long f_retrieve_payment_no (string vas_where_criteria);/* ---------------------------------------------------------------------------------------------------- */
/* Function Name: f_retrieve_payment_no                                                                 */
/*                                                                                                      */
/* Purpose:       The purpose of this function is to see if a PAYMENT record exists for the             */
/*						specified criteria.																						  */
/*                                                                                                      */
/* Arguments:		Arguments Passed:                                                                     */
/*																																		  */
/*						vas_where_criteria			-	The "where" clause to be used to limit the search.      */
/*																																		  */
/* Return Values: Successful	-	Returns the maximum payment_no (or 0, if none found)                   */
/*                Failure		-	-1  (database error)                                                   */
/* ---------------------------------------------------------------------------------------------------- */

DECLARE	payment_cursor DYNAMIC CURSOR FOR SQLSA;
Long		vll_payment_no = 0
String	vls_sql_select



If vas_where_criteria = '' or IsNull(vas_where_criteria) Then
	vls_sql_select = 	"SELECT payment_no "			+	&
							"FROM PAYMENT "
Else
	vls_sql_select = 	"SELECT payment_no "			+	&
							"FROM PAYMENT "					+	&
							"WHERE " + vas_where_criteria
End If



PREPARE SQLSA FROM :vls_sql_select ;

OPEN DYNAMIC payment_cursor ;
	SQLCA.nf_handle_error("OPEN DYNAMIC CURSOR: payment_cursor","n_benefit_calculation","f_retrieve_payment_no")

FETCH payment_cursor INTO :vll_payment_no ;
	If SQLCA.SQLCode = -1 Then
		If SQLCA.SQLDBCODE = 10083 Then
			vll_payment_no = 0
		Else
			Error.Text = SQLCA.SQLErrText
			Error.WindowMenu="w_benefits"
			Error.Object="n_benefit_calculations"
			Error.ObjectEvent="f_retrieve_payment_no"
			SignalError()
			Return -1
		End If
	Else
		SQLCA.nf_handle_error("FETCH DYNAMIC CURSOR: payment_cursor","n_benefit_calculation","f_retrieve_payment_no")
	End If

CLOSE payment_cursor ;
	SQLCA.nf_handle_error("CLOSE DYNAMIC CURSOR: payment_cursor","n_benefit_calculation","f_retrieve_payment_no")

Return vll_payment_no
end function

public function long f_get_next_ben_calc_no (long al_claim_no);/* ---------------------------------------------------------------------------------------------------- */
/* Function Name: f_get_next_ben_calc_no                                                                */
/*                                                                                                      */
/* Purpose:       The purpose of this function is to find the next available benefit calculation        */
/*						number for the claim_no passed                                            				  */
/*                                                                                                      */
/* Arguments:		Arguments Passed:                                                                     */
/*																																		  */
/*						al_claim_no 					- The claim number to search for                           */
/*																																		  */
/* Return Values: Successful	-	Returns the benefit_calculation_no (or 0, if none found)               */
/*                Failure		-	-1  (database error)                                                   */
/* ---------------------------------------------------------------------------------------------------- */

Long		ll_benefit_calculation_no

//	
//	Retrieve BENEFIT_CALCULATION record


  SELECT max(BENEFIT_CALCULATION.benefit_calculation_no)
    INTO :ll_benefit_calculation_no  
    FROM BENEFIT_CALCULATION
   WHERE ( BENEFIT_CALCULATION.claim_no = :al_claim_no )   ;

	SQLCA.nf_handle_error("Embedded SQL: Select from BENEFIT_CALCULATION","n_benefit_calculation","f_get_max_benefit_calculation")

	If IsNull(ll_benefit_calculation_no) Then 
		ll_benefit_calculation_no = 1
	ELSE
		ll_benefit_calculation_no = ll_benefit_calculation_no + 1
	END IF

	Return ll_benefit_calculation_no





end function

public function integer f_lock_opening (long al_claim_no, long al_opening_no, date adt_accident_recurrence_date);/* ----------------------------------------------------------------------------------------------------	*/
/*	Function Name: 	f_lock_opening																							   */
/*																																			*/
/*	Purpose:				This function puts a data lock on the opening record.  This is used to ensure       */
/*							that no-one removes the opening record while someone is creating a benefit       	*/
/*							calculation for it.																						*/
/*																																			*/
/*																																			*/
/*	Arguments:			Long	-	al_claim_no		-	Claim to be changed												   */
/*							Long	-	al_opening_no	-	opening to be changed.										      */
/*							Date	-	adt_accident_recurrence_date - Accident/Recurrence date 			   			*/
/*																																			*/
/*	Return Value:		Integer	-	Success		1		-	Value	returned when successful							*/
/*										-	Failure		-1		-	Value returned when not successful						*/
/*																																			*/
/* ----------------------------------------------------------------------------------------------------	*/

LONG 			ll_result, ll_last_benefit_calculation_no
DATETIME 	ldtm_accident_recurrence_date

ldtm_accident_recurrence_date = DateTime(adt_accident_recurrence_date)

	
// Update a field on the opening record so the save option can ensure that no-one
//	deletes it while a save of the Benefit Calculation data is in progress

	UPDATE	OPENING
		SET 	accident_recurrence_date = :ldtm_accident_recurrence_date
		WHERE claim_no = :al_claim_no 		and
				opening_no = :al_opening_no  	and
				accident_recurrence_date = :ldtm_accident_recurrence_date using SQLCA;
	SQLCA.nf_handle_error("Embedded SQL: Update opening","n_benefit_calculation","f_set_transitional_claim_flag")


// Determine whether or not record was updated

	If SQLCA.SQLNRows = 0 Then
			MessageBox("Benefit Calculation Module - Data Access Conflict","Error updating opening record~r~n" + &
							"Possible Cause: Someone else may be updating this opening~r~n~r~n" + &
							"No data has been saved or updated!",StopSign!)
			Return -1
	End If

			


//	Otherwise, exit successfully
	
	Return 1

end function

public function long f_retrieve_bencalc_no (long al_claimno, long al_bencalcno);/* This function determines if there are any records existing in the PERIODIC_AWARD table for
	the given claim_no and benefit_calculation_no.

	Arguments:	al_claimno		- The claim_no to use in the PERIODIC_AWARD table search.
					al_bencalcno	- The benefit_calculation_no to use in the PERIODIC_AWARD table search.
*/
	Long		ll_awardcount

	SELECT count(*)
	  INTO :ll_awardcount
	  FROM PERIODIC_AWARD
	 WHERE claim_no = :al_claimno
		AND benefit_calculation_no = :al_bencalcno
	 USING SQLCA;

	IF SQLCA.SQLCode = -1 THEN
		IF SQLCA.SQLDBCODE = 10083 THEN
			ll_awardcount = 0
		ELSE
			Error.Text = SQLCA.SQLErrText
			Error.WindowMenu="w_benefits"
			Error.Object="n_benefit_calculations"
			Error.ObjectEvent="f_retrieve_bencalc_no"
			SignalError()
			RETURN -1
		END IF
	ELSE
		SQLCA.nf_handle_error("SELECT count(*) INTO :ll_awardcount","n_benefit_calculation","f_retrieve_bencalc_no")
	END IF

	RETURN ll_awardcount

end function

public function datetime f_isvalid_effective_from_date (long al_claimno, long al_openingno);/* This function determines the maximum award_end_date from the PERIODIC_AWARD table for
	the given claim_no and opening_no.

	Arguments:	al_claimno		- The claim_no to use in the PERIODIC_AWARD table search.
					al_openingno	- The opening_no to use in the PERIODIC_AWARD table search.
*/
	DATETIME	ldt_awardenddate

	SetNull(ldt_awardenddate)

	SELECT Max(award_end_date)
	  INTO :ldt_awardenddate
	  FROM PERIODIC_AWARD
	 WHERE claim_no = :al_claimno
		AND opening_no = :al_openingno
	 USING SQLCA;

	IF SQLCA.SQLCode = -1 THEN
		IF SQLCA.SQLDBCODE = 10083 THEN
			SetNull(ldt_awardenddate)
		ELSE
			Error.Text = SQLCA.SQLErrText
			Error.WindowMenu="w_benefits"
			Error.Object="n_benefit_calculations"
			Error.ObjectEvent="f_isvalid_effective_from_date"
			SignalError()
			RETURN ldt_awardenddate
		END IF
	ELSE
		SQLCA.nf_handle_error("Max(awardenddate) INTO :ldt_awardenddate","n_benefit_calculation","f_isvalid_effective_from_date")
	END IF

	RETURN ldt_awardenddate

end function

public function long f_retrieve_cost_analysis_option (long al_claimno, long al_bencalcno);/* This function determines if there are any records existing in the COST_ANALYSIS_OPTION table for
	the given claim_no and benefit_calculation_no.

	Arguments:	al_claimno		- The claim_no to use in the COST_ANALYSIS_OPTION table search.
					al_bencalcno	- The benefit_calculation_no to use in the COST_ANALYSIS_OPTION table search.
*/
	Long		ll_cost_option_count

	SELECT count(*)
	  INTO :ll_cost_option_count
	  FROM COST_ANALYSIS_OPTION
	 WHERE claim_no = :al_claimno  and
	       (rloe_benefit_calc_no = :al_bencalcno  or ltd_benefit_calc_no = :al_bencalcno  or capitalization_benefit_calc_no = :al_bencalcno)
	 USING SQLCA;

	IF SQLCA.SQLCode = -1 THEN
		IF SQLCA.SQLDBCODE = 10083 THEN
			ll_cost_option_count = 0
		ELSE
			Error.Text = SQLCA.SQLErrText
			Error.WindowMenu="w_benefits"
			Error.Object="n_benefit_calculations"
			Error.ObjectEvent="f_retrieve_cost_analysis_option"
			SignalError()
			RETURN -1
		END IF
	ELSE
		SQLCA.nf_handle_error("SELECT count(*) INTO :ll_cost_option_count","n_benefit_calculation","f_retrieve_cost_analysis_option")
	END IF

	
	RETURN ll_cost_option_count

end function

public function decimal f_calculate_cpp (date adt_index_date, decimal adec_annual_gross, datetime adt_create_datetime);/* ----------------------------------------------------------------------------------------------------	*/
/* Function Name: f_calculate_cpp																								*/
/*																																			*/
/* Purpose:       This function calculates CPP using figures based on the index date passed.					*/
/*																																			*/
/* Arguments:     Parameters passed by calling routine:																	*/
/*																																			*/
/*                vad_index_date     - The date for which the calculation is to be performed (must			*/
/*                                     be a date not a year since tax calculations can change				*/
/*                                     within a year)																		*/
/*                vac_annual_gross   - The annual gross income for which CPP is applicable					*/
/*																																			*/
/* Return Values: Annual CPP																										*/
/*																																			*/
/* ----------------------------------------------------------------------------------------------------	*/


	Decimal	ldec_work,	C

// Commented as a result of PR1890 - the calculations are table driven.

//
// Check the date passed. If there is an available tax routine, call it.
//	Otherwise, return 0
//

//Choose Case ii_period //adt_index_date

/*	Case 31,32//Date("1997 01 01") to Date("1997 12 31")
		/* The UIC, CPP and provincial tax rates changed July 1, 1997, effective for the entire year, but as
			the changes were implemented in the middle of the year we weren't notified and so we have many
			ben calcs with the wrong values. We never change a ben calc once created, and don't store all
			fields (ie CPP and UIC), so to recalculate the values (although incorrectly) for all ben calcs created
			in 1997, we have to do this....
		*/
		IF adt_create_datetime > DateTime(Date("1997/01/01"),Time("00:00:00 AM")) AND &
			adt_create_datetime < DateTime(Date("1998/01/09"),Time("11:59:59 AM")) THEN
			idec_cpp_rate		=	0.02925				
			idec_cpp_max		=	944.78			
			idec_cpp_exempt	=	3500.00
		ELSE
			idec_cpp_rate		=	0.03				// changed Jan9/98  from 0.02925
			idec_cpp_max		=	969.00			// 944.78
			idec_cpp_exempt	=	3500.00
		END IF
End Choose
*/

// ---------------------------------------------------------------------------------
// Calculate the Annual Canada Pension Plan deduction as follows:
//
//	C = 	cpp_rate * (adec_annual_gross - cpp_exempt)
// 		where:
//				C is either zero or a positive number no greater than cpp_max
// ---------------------------------------------------------------------------------

	ldec_work = adec_annual_gross - idec_cpp_exempt
	If ldec_work > 0 then
		C = round(idec_cpp_rate * ldec_work,2)
		If C > idec_cpp_max then
			C = idec_cpp_max
		End If
	Else
		C = 0
	End If

return C
end function

public function decimal f_calc_annual_net_income (decimal ad_annual_gross, decimal ad_cpp_return, decimal ad_uic_return, decimal ad_income_tax_payable);/* now we need to calculate the annual net income
*/
DECIMAL ldec_return

ldec_return = (((ad_annual_gross - ad_cpp_return) - ad_uic_return) - ad_income_tax_payable)

RETURN ldec_return

end function

public function decimal f_calc_annual_taxable_income (decimal ad_annual_gross, decimal ad_uic_return, decimal ad_cpp_return);/*
This routine generates the annual taxable income for a given Gross annual income.
It calculates Annual Taxable Income for Pre-1993 Taxation Periods


ARGS: 		ad_annual_gross

returns : 	ad_return - this routine generates the annual tabable income "A" 
                      	for a given gross annual income "WS_ANNUAL_GROSS

round all values

The formula is;

yearly_factor_date          
--------------------------- 
Jan 1 1982 12:00AM  1        
Jul 1 1982 12:00AM  2       
Jan 1 1983 12:00AM  3      
Jul 1 1983 12:00AM  4     
Jan 1 1984 12:00AM  5    
Jul 1 1984 12:00AM  6   
Jan 1 1985 12:00AM  7  
Jul 1 1985 12:00AM  8 
Jan 1 1986 12:00AM  9
Jul 1 1986 12:00AM  10
Jan 1 1987 12:00AM  11
Jul 1 1987 12:00AM  12
Jan 1 1988 12:00AM  13
Jul 1 1988 12:00AM  14
Jan 1 1989 12:00AM  15
Jul 1 1989 12:00AM  16
Jan 1 1990 12:00AM  17
Jul 1 1990 12:00AM  18
Jan 1 1991 12:00AM  19
Jul 1 1991 12:00AM  20
Jan 1 1992 12:00AM  21
Jul 1 1992 12:00AM  22
	
*/
decimal ldec_percent,ldec_employee_deduction,ldec_deduction  
decimal ldec_work_one,ldec_work_two,ldec_work_three,ldec_return

Choose Case ii_period

	Case 1,2
		
		/*formula may be found on page 7 of MC34 (1981, Draft) published by revenue canada, taxation
		   STEP 1: - calculation of the Gross
			          calculate 3% of gross not to exceed 500
		   Date("1982 01 01") to Date("1982 06 30")-
			Date("1982 07 01") to Date("1982 12 31")-
		
		*/
		  ldec_percent            = 0.03
		  ldec_employee_deduction = 500
		  ldec_deduction          = 100
				
	Case 3,4,5,6,7,8,9,10,11,12,13
		/*
		  January 1983 to January 1988
		  
		  THE FORMULA USED MAY BE FOUND ON PAGE 16 OF MC42      
        PUBLISHED BY REVENUE CANADA, TAXATION 
		  Date("1983 01 01") to Date("1983 06 30")-3
		  Date("1983 07 01") to Date("1983 12 31")-4
		  Date("1984 01 01") to Date("1984 06 30")-5
		  Date("1984 07 01") to Date("1984 12 31")-6
		  Date("1985 01 01") to Date("1985 06 30")-7
		  Date("1985 07 01") to Date("1985 12 31")-8
		  Date("1986 01 01") to Date("1986 06 30")-9
		  Date("1986 01 01") to Date("1986 12 31")-10
		  Date("1987 01 01") to Date("1987 06 30")-11
		  Date("1987 07 01") to Date("1987 12 31")-12
		  Date("1988 01 01") to Date("1988 06 30")-13
		*/
		  ldec_percent             = 0.2
		  ldec_employee_deduction  = 500
		  ldec_deduction           = 100
      /* CALCULATE 20% OF GROSS, NOT TO EXCEED $500 (EMPL. EXP. DEDUC.)  
      */	
	Case 14,15,16,17,18,19,20,21,22
		/* (ANNUAL TAXABLE INCOME) = P (I - F - U1) - HD - F1           
         IN MC BOOKLET PUBLISHED BY REVENUE CANADA, TAXATION.           
                                                                      
         BASED ON THE FOLLOWING COMMENTS THIS FORMULA IS SIMPLIFIED TO    
         THE FOLLOWING:                                                   
                                                                      
         A (ANNUAL TAXABLE INCOME) = I (WS-ANNUAL-GROSS)	
		   Date("1988 07 01") to Date("1988 12 31")-14
			Date("1989 01 01") to Date("1989 06 30")-15
			Date("1989 07 01") to Date("1989 12 31")-16
			Date("1990 01 01") to Date("1990 06 30")-17
			Date("1990 07 01") to Date("1990 12 31")-18
			Date("1991 01 01") to Date("1991 06 30")-19
			Date("1991 07 01") to Date("1991 12 31")-20
			Date("1992 01 01") to Date("1992 06 30")-21
			Date("1992 07 01") to Date("1992 12 31")-22
		*/
		ldec_return = ad_annual_gross
		RETURN ldec_return

	Case Else//should be post 93 Ben Calc
		ldec_return = ad_annual_gross
		RETURN ldec_return
End Choose

/* step one -calculate  a % of the gross, not to exceed the amount
             dictated by the period
*/
	ldec_work_one = round(ad_annual_gross * ldec_percent,2)

	IF ldec_work_one > ldec_employee_deduction  THEN
		ldec_work_one = ldec_employee_deduction  
   END IF
    
/* step two -add cpp deduction, uic premium, 100 deduction and % of gross
*/
	ldec_work_two = ad_uic_return + ad_cpp_return + ldec_deduction + ldec_work_one 
   
/* step three - calculate taxable income
*/
 	ldec_work_three = ldec_work_two + idec_basic_pers_exempt
	
	ldec_return = ad_annual_gross - ldec_work_three 

	IF ldec_return < 0 THEN
		ldec_return = 0
	END IF
	
	RETURN round(ldec_return,2)
return 1
end function

public subroutine f_set_index_date (date adt_date);/* 
   This function will be used to set the taxation_period to be used
	there are 18 different periods (as of the year 2000) this date will decide which
	one is used in the code
*/
id_index_date = adt_date
end subroutine

public subroutine f_set_td1_amount (decimal ad_td_one);/* passed from the calling structure this is populated from the 
   the dropdown datawindow which may be any amount up to the max td_1 amount
*/

idec_basic_pers_exempt = ad_td_one
end subroutine

public subroutine f_set_annual_gross (decimal adc_amount);idec_annual_gross = adc_amount
end subroutine

public function boolean f_check_effective_from_date (date adt_effective_from);/*

Checks to see if the Effective from date is greater then 
the minimum date that is held by the Yearly_Factor table

Pass in effective from date

RETURNS
TRUE  - date is alright
FALSE - date fails validation

*/
BOOLEAN lb_check
date	ld_check
DATETIME	ldt_min_date

SELECT min(yearly_factor_date) 
INTO :ldt_min_date
FROM Yearly_Factor
USING SQLCA;

SQLCA.nf_handle_error("SELECT min(yearly_factor_date) ","n_benefit_calculation","f_check_effective_from_date")

IF adt_effective_from < date(ldt_min_date) THEN
	lb_check = FALSE
ELSE
	lb_check = TRUE
END IF

RETURN lb_check

end function

public function integer f_check_amounts (integer ai_control, decimal ad_amount_one, decimal ad_amount_two, ref string as_message);
/* this function does various edit checks based on amounts and period
   many of these may be handled from the calling scripts.
*/

CHOOSE CASE ai_control
	CASE 1//capable earnings cannot exceed actual earnings
		IF ad_amount_one > ad_amount_two THEN
			as_message = "Capable earnings exceed actual earnings"
			return -1
		END IF
			
	CASE 2//amount can not be less then basic personal exemption
		
		IF ad_amount_one < ad_amount_two THEN
			as_message = "Amount is less then basic personal exemption"
			return -1
		END IF
		
	CASE 3
		IF ad_amount_one > idec_max_td1 THEN
			as_message = "Amount is greater then " + string(round(idec_max_td1,2))
			return -1
		END IF
			
	CASE 4//see if annual gross exceeds maximum
		IF ad_amount_one > idec_max_annual_gross THEN
			as_message = "Amount is greater then " + string(round(idec_max_annual_gross,2)) + " , which is the maximum allowed."
			return -1
		END IF
					
END CHOOSE

RETURN 1
end function

public function decimal f_calc_pre93_tax (decimal ad_income_amount, decimal ad_uic_return, decimal ad_cpp_return);/*
This routine calculates the annual income tax payable given the annual taxable income
ARGS: ad_annual_gross
**********************************NOTE*****************************************
The values for this function changed in 1988 - see ue_note(user_event) does not fire
*******************************************************************************

returns : ad_return - annual uic premium calculated by this routine is either 0 or a positive 
		      number no greater then id_uic_max_premium 

round all values


The formula is;
	option 4 in the MC Booklet published annual by revenue canada, taxation.
A - The annual taxable income input to this routine - (idec_annual_gross)
T - The Annual income tax payable output by this routine (ldec_return)
R - AND - K table contains the "R" and "K" factors from table 4 in the MC Booklet
	  they are the applicable factors as determined by routine 500 table lookup
	  (idec_k,idec_r)
V - The Applicable provincial tax rate for new brunswick (idec_v)
L - An index used by 500 - table lookup
K1	-	Net federal tax credit
K2	-	Annual cpp tax credit & Annual uic tax credit
K3	-	Other federal tax credits
D	-	Federal Surtax
P	-	Number of pay periods in year (1 - Annual)
I	-	Gross Remuneration (vac_annual_gross)
F	-	Employee's registered pension plan
U1	-	Union Dues
HD	-	Annual deduction for residing in a prescribed area per line 16
		of TD1 revised 1988 07 01
F1	-	Annual deduction authorized by district taxation office
T1	-	Annual Federal Tax Deduction
T2	-	Annual Provincial Tax Deduction
T	-	Annual Federal and Provincial Tax Deduction
V1	-	Provincial Surtax

Basic-Federal-Tax 		   = an intermediate value used in this routine
Federal-surtax 			   = an intermediate value used in this routine
Surtaxed-basic-federal-tax = an intermediate value used by this routine
provincial-tax 			   = an intermediate value used by this routine
tax-work 			         = a work variable used in this routine
federal-surtax-work1 		= a work variable used in this routine 
federal-surtax-work2 		= a work variable used in this routine 

Period	yearly_factor_date          
			--------------------------- 
1			Jan 1 1982 12:00AM          
2			Jul 1 1982 12:00AM          
3			Jan 1 1983 12:00AM          
4			Jul 1 1983 12:00AM          
5			Jan 1 1984 12:00AM          
6			Jul 1 1984 12:00AM          
7			Jan 1 1985 12:00AM          
8			Jul 1 1985 12:00AM          
9			Jan 1 1986 12:00AM          
10			Jul 1 1986 12:00AM          
11			Jan 1 1987 12:00AM          
12			Jul 1 1987 12:00AM          
13			Jan 1 1988 12:00AM          
14			Jul 1 1988 12:00AM          
15			Jan 1 1989 12:00AM          
16			Jul 1 1989 12:00AM          
17			Jan 1 1990 12:00AM          
18			Jul 1 1990 12:00AM          
19			Jan 1 1991 12:00AM          
20			Jul 1 1991 12:00AM          
21			Jan 1 1992 12:00AM          
22			Jul 1 1992 12:00AM          



*/

decimal ldec_basic_federal_tax,ldec_tax_work,ldec_federal_surtax_work1
decimal ldec_federal_surtax_work2,ldec_federal_surtax ,ldec_surtaxed_basic_federal_tax 	
decimal ldec_provincial_tax,ldec_return,ldec_federal_tax_reduction
decimal ldec_reduced_federal_tax, ldec_fed_tax_upper_limit
decimal ldec_fed_tax_lower_limit, ldec_fed_tax_middle_limit

decimal ldec_work_one,ldec_work_two,ldec_work_three

//temp variables due to tax change 1988
decimal ldec_t3_ra,ldec_K1,ldec_K2_cpp,ldec_K2_ui,ldec_K2,ldec_t3,ldec_d,ldec_t1,ldec_t2,ldec_v1

Choose Case ii_period

	Case 1,2,3,4	
		/*The R AND K Table elements are those for option 4 in the MC
		  Booklet published by revenue canada, Taxation	
		  
		  Date("1982 01 01") to Date("1982 06 30")
		  Date("1982 07 01") to Date("1982 12 31")
		  Date("1983 01 01") to Date("1983 06 30")
		  Date("1983 07 01") to Date("1983 12 31")
		*/
		ldec_fed_tax_lower_limit= 200
		
	Case 5,6
		//Date("1984 01 01") to Date("1984 06 31")
		//Date("1984 07 01") to Date("1984 12 31")
		ldec_fed_tax_lower_limit = 200
		ldec_fed_tax_middle_limit = 6000
		ldec_fed_tax_upper_limit = 8000	
		
	Case 7,8
		//Date("1985 01 01") to Date("1985 06 31")
		//Date("1985 07 01") to Date("1985 12 31")
		ldec_fed_tax_lower_limit =  100
		ldec_fed_tax_middle_limit = 6000
		ldec_fed_tax_upper_limit       = 7000
		
	Case 9//Date("1986 01 01") to Date("1986 06 30")
		  
	Case 10
		/*THE FORMULA USED IS OPTION 4 IN THE MC BOOKLET PUBLISHED       
        ANNUALLY BY REVENUE CANADA, TAXATION. 
		  Date("1986 07 01") to Date("1986 12 31")
		*/		
	Case 11,12
		/*THE FORMULA USED IS OPTION 4 IN THE MC BOOKLET PUBLISHED       
        ANNUALLY BY REVENUE CANADA, TAXATION.
		  Date("1987 01 01") to Date("1987 06 31")
		  Date("1987 07 01") to Date("1987 12 31")
		*/		
	Case 13//Date("1988 01 01") to Date("1988 06 30")
				
	Case 14,15,16,17,18,19,20,21,22
		/*
		 Date("1988 07 01") to Date("1988 12 31")-14 
		 Date("1989 01 01") to Date("1989 06 30")-15
		 Date("1989 07 01") to Date("1989 12 31")-16
		 Date("1990 01 01") to Date("1990 06 31")-17
		 Date("1990 07 01") to Date("1990 12 31")-18
		 Date("1991 01 01") to Date("1991 06 30")-19
		 Date("1991 07 01") to Date("1991 12 31")-20
		 Date("1992 01 01") to Date("1992 06 30")-21
		 Date("1992 07 01") to Date("1992 12 31")-22
      */
     // Calculate T3 (ANNUAL BASIC FEDERAL TAX)                         
     // Calculate Intermediate values first
	  //set up our work variables based on the case
	  IF ii_period = 14 THEN
		  ldec_work_one   = 81.26
		  ldec_work_two   = 117.40
	  ELSEIF ii_period = 15 OR ii_period = 16 THEN
		  ldec_work_one   = 89.25
		  ldec_work_two   = 104.31
		  ldec_work_three = 15000
	  ELSEIF ii_period = 17 OR ii_period = 18 THEN
		  ldec_work_one   = 97.61
		  ldec_work_two   = 127.30
		  ldec_work_three = 15000
	  ELSEIF ii_period = 19 THEN
		  ldec_work_one   = 107.53
		  ldec_work_two   = 135.25
		  ldec_work_three = 12500
	  ELSEIF ii_period = 20 THEN
		  ldec_work_one   = 107.53
		  ldec_work_two   = 168.31
		  ldec_work_three = 12500
	  ELSEIF ii_period = 21 OR ii_period = 22 THEN
		  ldec_work_one   = 118.32
		  ldec_work_two   = 188.29
		  ldec_work_three = 12500
	  ELSE
		
	  END IF
	  
     ldec_t3_ra  = round(idec_R * ad_income_amount,2)                              
     ldec_K1     = round(0.17 * idec_basic_pers_exempt,2 )                          
	  IF ad_cpp_return = 0 THEN
	  	 ldec_K2_cpp = 0
	  ELSE	
     	 ldec_K2_cpp = round(0.17 * ad_cpp_return,2)  //idec_annual_gross                         	
     	 IF ldec_K2_cpp > ldec_work_one THEN						
        	 ldec_K2_cpp = ldec_work_one
		 END IF
	  END IF 	 
	
	  IF ad_uic_return = 0 THEN
		 ldec_k2_ui = 0
	  ELSE	 
       ldec_K2_ui   = round(0.17 * ad_uic_return,2)	//idec_annual_gross 			
       IF ldec_K2_ui > ldec_work_two THEN						
          ldec_K2_ui = ldec_work_two 
	    END IF
	  END IF 
     ldec_K2 = ldec_K2_cpp + ldec_K2_ui                                 
      
     ldec_t3 = ldec_t3_ra - idec_K - ldec_K1 - ldec_K2                          
     IF ldec_t3 < 0 THEN                                                
        ldec_t3 = 0 
	  END IF
		 
		 // Calculate FEDERAL SURTAX based on period     
		 IF ii_period = 16 OR ii_period = 17 OR ii_period = 18 THEN                            
           IF ldec_t3 <= ldec_work_three THEN                                            
      			ldec_d = round(0.05 * ldec_t3,2)
			  ELSE
				   ldec_d = round((0.05 * ldec_t3) + (0.03 * (ldec_t3 - ldec_work_three)),2)   
			  END IF
                                                              
           IF ldec_d < 0 THEN                                        		
              ldec_d = 0
			  END IF
		ELSEIF ii_period = 19 OR ii_period = 20 OR ii_period = 21 THEN
			                                     
           IF ldec_t3 <= ldec_work_three THEN                                             
               ldec_d = round(0.05 * ldec_t3,2)       
			  ELSE
               ldec_d = round((0.05 * ldec_t3) + (0.05 * (ldec_t3 - ldec_work_three)),2) 
			  END IF
			  
           IF ldec_d < 0 THEN                                        		
              ldec_d = 0
			  END IF
		ELSEIF ii_period = 22 THEN
			
			IF ldec_t3 <= ldec_work_three THEN
            ldec_d = round(0.04 * ldec_t3,2) 
			ELSE
            ldec_d = round((0.04 * ldec_t3) + (0.05 * (ldec_t3 - ldec_work_three)),2)         
         END IF  
			  
			IF ldec_d < 0 THEN                                        		
            ldec_d = 0
			END IF
	    ELSE                                
			 ldec_d = round(0.03 * ldec_t3,2)                               
			 IF ldec_d < 0 THEN
				ldec_d = 0
			 END IF
	    END IF
		 
      // Calculate ANNUAL FEDERAL TAX DEDUCTIION                         
      ldec_t1 = ldec_t3 + ldec_d
		
      // Calculate ANNUAL PROVINCIAL TAX DEDUCTION	
		IF ii_period = 20 THEN
			IF (idec_V * ldec_t3) <= 13500 THEN
         		ldec_v1 = 0
 			ELSE
				ldec_v1 = (0.08 * (( idec_V * ldec_t3) - 13500))   // PR 1890 - kdm
		   	END IF
			ldec_t2 = round((idec_V * ldec_t3) + ldec_v1,2)
		ELSEIF ii_period = 21 OR ii_period = 22 THEN
			IF (idec_V * ldec_t3) <= 13500 THEN
				ldec_v1 = 0
         	ELSE
            		ldec_v1 = 0.08 * (( idec_V * ldec_t3) - 13500)
			END IF
         	ldec_t2 = round((idec_V * ldec_t3) + ldec_v1,2)           
		ELSE
			ldec_t2 = round(idec_V * ldec_t3,2)
	  	END IF
		
      // Calculate ANNUAL FEDERAL AND PROVINCIAL TAX DEDUCTION		
      ldec_return = ldec_t1 + ldec_t2
		
		return ldec_return
						
	End Choose

/* step one -look up applicable values for "R" and "K" in the table
             these will be table driven
*/

/* step two -calculate basic federal tax
*/
	ldec_tax_work 			      = round(ad_income_amount * idec_R,2)
	ldec_Basic_Federal_Tax 		= ldec_tax_work - idec_K
	
/* OPTIONAL CODE PERIOD SPECIFIC
*/

CHOOSE CASE ii_period
	CASE 1,2,3,4
		
	/* calculate basic federal tax reduction (flat 200 for 1982 and for 1983)
	   populate in period specific variable
	NEXT
		Subtract basic-basic-federal-tax-reduction from basic-federal-tax
		giving reduced-basic-federal-tax
	*/
	 ldec_federal_tax_reduction = ldec_fed_tax_lower_limit		// PR 1890 - kdm
	 ldec_reduced_federal_tax = (ldec_Basic_Federal_Tax - ldec_federal_tax_reduction)
	IF ldec_reduced_federal_tax < 0 THEN
		ldec_reduced_federal_tax = 0
	END IF
	
	/* Calculate Provincial Tax
	*/
	ldec_provincial_tax = round(ldec_Basic_Federal_Tax * idec_V,2)
	
	/* Add reduced federal tax and provincial tax 
	*/
	ldec_return = ldec_reduced_federal_tax + ldec_provincial_tax
	
	IF ldec_return < 0 THEN    //PR1890 - SMANZER Can not have a Neg tax amount.
		ldec_return = 0
	END IF	
	
	//return out
	return ldec_return
	
CASE 5,6,7,8
	 
    /*CALCULATE BASIC FEDERAL TAX REDUCTION  
	 */                     
	IF ldec_Basic_Federal_Tax <= ldec_fed_tax_lower_limit THEN                         
       ldec_federal_tax_reduction = ldec_Basic_Federal_Tax
	END IF
	
	IF ldec_Basic_Federal_Tax > ldec_fed_tax_lower_limit and ldec_Basic_Federal_Tax <= ldec_fed_tax_middle_limit THEN
		  ldec_federal_tax_reduction = ldec_fed_tax_lower_limit   // PR 1890 - kdm
	END IF
	
	IF ldec_Basic_Federal_Tax > ldec_fed_tax_middle_limit and ldec_Basic_Federal_Tax <= ldec_fed_tax_upper_limit  THEN
		ldec_federal_tax_reduction = (ldec_fed_tax_lower_limit - (.1 * (ldec_Basic_Federal_Tax - ldec_fed_tax_middle_limit))) 
      IF ldec_federal_tax_reduction < 0 THEN
			ldec_federal_tax_reduction = 0
		END IF
	END IF
	
   IF ldec_Basic_Federal_Tax > ldec_fed_tax_upper_limit  THEN
		ldec_federal_tax_reduction = 0
	END IF
		  
		 
  
     /* calculate basic federal tax reduction  
	   populate in period specific variable
	NEXT
		Subtract basic-basic-federal-tax-reduction from basic-federal-tax
		giving reduced-basic-federal-tax
	*/
	 ldec_reduced_federal_tax = (ldec_Basic_Federal_Tax - ldec_federal_tax_reduction)
	IF ldec_reduced_federal_tax < 0 THEN
		ldec_reduced_federal_tax = 0
	END IF
	
	/* Calculate Provincial Tax
	*/
	ldec_provincial_tax = round(ldec_Basic_Federal_Tax * idec_V,2)
	
	/* Add reduced federal tax and provincial tax 
	*/
	ldec_return = ldec_reduced_federal_tax + ldec_provincial_tax
	
	IF ldec_return < 0 THEN    //PR1890 - SMANZER Can not have a Neg tax amount.
		ldec_return = 0
	END IF	
	
	return ldec_return
CASE 9
	/* CALCULATE FEDERAL SURTAX                                         
	*/
   IF ad_income_amount <= 29744.00 THEN                          
      ldec_federal_surtax = 0
	ELSE
	
		ldec_federal_surtax_work1 = 0.05 * (((idec_R * ad_income_amount) - idec_K) - 6000)                        
		IF ldec_federal_surtax_work1 < 0  THEN
			ldec_federal_surtax_work1 = 0
		END IF
		
		ldec_federal_surtax_work2  =   0.05 * (((idec_R * ad_income_amount) - idec_K) - 15000)                                  
		IF ldec_federal_surtax_work2 < 0  THEN
			ldec_federal_surtax_work2 = 0
		END IF  
		
		ldec_federal_surtax = ldec_federal_surtax_work1 + ldec_federal_surtax_work2
		IF ldec_federal_surtax < 0  THEN
			ldec_federal_surtax = 0
		END IF  
	END IF
	
	/* CALCULATE SURTAXED BASIC FEDERAL TAX 
	*/
	ldec_surtaxed_basic_federal_tax  = ldec_Basic_Federal_Tax  + ldec_federal_surtax                 
			
	/* CALCULATE PROVINCIAL TAX
	*/
	ldec_provincial_tax = round(ldec_Basic_Federal_Tax * idec_V,2)	
		
	/* ADD SURTAXED FEDERAL TAX AND PROVINCIAL TAX
	*/
	ldec_return = ldec_surtaxed_basic_federal_tax + ldec_provincial_tax
	
	IF ldec_return < 0 THEN    //PR1890 - SMANZER Can not have a Neg tax amount.
		ldec_return = 0
	END IF	
	
	return ldec_return
CASE 10
	/* CALCULATE FEDERAL SURTAX                                         
	*/
   IF ad_income_amount <= 29744.00 THEN
		ldec_federal_surtax = 0.03 * ldec_Basic_Federal_Tax                    
	ELSE
	
		ldec_federal_surtax_work1 = 180 + 0.08 * (((idec_R * ad_income_amount) - idec_K) - 6000)                        
		IF ldec_federal_surtax_work1 < 0  THEN
			ldec_federal_surtax_work1 = 0
		END IF
		
		ldec_federal_surtax_work2  =   0.05 * (((idec_R * ad_income_amount) - idec_K) - 15000)                                  
		IF ldec_federal_surtax_work2 < 0  THEN
			ldec_federal_surtax_work2 = 0
		END IF  
		
		ldec_federal_surtax = ldec_federal_surtax_work1 + ldec_federal_surtax_work2
		IF ldec_federal_surtax < 0  THEN
			ldec_federal_surtax = 0
		END IF  
	END IF
	
	/* CALCULATE SURTAXED BASIC FEDERAL TAX 
	*/
	ldec_surtaxed_basic_federal_tax  = ldec_Basic_Federal_Tax  + ldec_federal_surtax                 
			
	/* CALCULATE PROVINCIAL TAX
	*/
	ldec_provincial_tax = round(ldec_Basic_Federal_Tax * idec_V,2)	
		
	/* ADD SURTAXED FEDERAL TAX AND PROVINCIAL TAX
	*/
	ldec_return = ldec_surtaxed_basic_federal_tax + ldec_provincial_tax
	
	IF ldec_return < 0 THEN    //PR1890 - SMANZER Can not have a Neg tax amount.
		ldec_return = 0
	END IF	
	
	return ldec_return
CASE 11,12,13
	
	/* CALCULATE FEDERAL SURTAX                                         
	*/
   ldec_federal_surtax = round(.03 * ldec_Basic_Federal_Tax,2)
	IF ldec_federal_surtax < 0  THEN
		ldec_federal_surtax = 0
	END IF 
	
   /* CALCULATE SURTAXED BASIC FEDERAL TAX  
	*/
   ldec_surtaxed_basic_federal_tax = ldec_Basic_Federal_Tax + ldec_federal_surtax
	                           
   /* CALCULATE PROVINCIAL TAX
	*/
	ldec_provincial_tax = round(ldec_Basic_Federal_Tax * idec_V,2)	
          
   /* ADD SURTAXED FEDERAL TAX AND PROVINCIAL TAX
	*/
	ldec_return = ldec_surtaxed_basic_federal_tax + ldec_provincial_tax
	
	IF ldec_return < 0 THEN    //PR1890 - SMANZER Can not have a Neg tax amount.
		ldec_return = 0
	END IF	

	return ldec_return
	  
END CHOOSE

RETURN 1





















end function

public subroutine f_set_prov_td1 (decimal adec_prov_td1);/*	New function added for SR 64 - January 2001 BenCalc tax changes
	New TONI (tax-on-income) method of calculating provincial taxes implemented effective Jan. 1, 2001)
*/

/* passed from the calling structure this is populated from the 
   the dropdown datawindow which may be any amount up to the max td_1 amount
*/

idec_prov_basic_exempt = adec_prov_td1


end subroutine

public function integer f_populate_work_variables (date adt_effective, string as_earnings_type);/* process used to populate our work variables
   these will come from the database or user input

*/
//is_frequency             = as_frequency
//id_average_gross         = ad_average_gross
//id_weekly_cpp_benefit    = ad_weekly_cpp_benefit
//id_capable_average_gross = ad_capable_average_gross

/* select Defined variables based on the period being processed
 id_cpp_deduction    // - The annual cpp deduction calculated by routine 100-cpp
 id_cpp_rate         // - The applicable cpp rate for a given year
 id_cpp_max          // - The maximum annual cpp deduction for a given year
 id_cpp_exempt       // - The applicable annual cpp exemption for a given year
 id_uic_rate         // - the applicable uic rate for a given year
 id_uic_max_premium  // - the applicable maximum annual uic premium for a given year
 id_uic_max_earnings // - The applicable maximum annual uic insurable earnings for a given year
 id_uic_min          // - the applicable minimum annual insurable earnings for a given year
 idec_V              // - the applicable provincial tax rate for a given year
 id_max_annual_gross
 id_max_td1
 ii_release
*/
/* Make sure that the date is valid - it must be between the following values
*/

/*
PR 1890

If the taxation year is less than the effective date's year, then use July taxation period for taxation year

Else if the taxation year is the same as the effective date's year and the effective month falls between Jan and June, then use Jan taxation period for taxation year
	
	otherwise if the taxation year is the same as the effective date's year and the effective month falls between July and Dec, then use July taxation period for taxation year
*/

/* P10151-55 - J. Hawker, 2006.06.14 - Added release_no and canada_employment_credit 
   to the SELECT statements. 
*/

INTEGER li_effective_year, li_effective_month
DATETIME ldtm_yearly_factor, ldtm_yearly_factor_mth, ldtm_min_yearly_factor

li_effective_year = Year(adt_effective)
li_effective_month = Month(adt_effective)

SELECT yearly_factor_date
INTO   :ldtm_yearly_factor_mth
FROM   Yearly_Factor 
WHERE  datepart(year,yearly_factor_date) = datepart(year,:id_index_date)
AND    Month(yearly_factor_date) = Month(:id_index_date);

SELECT Max(yearly_factor_date), Min(yearly_factor_date)
INTO   :ldtm_yearly_factor, :ldtm_min_yearly_factor
FROM   Yearly_Factor 
WHERE  datepart(year,yearly_factor_date) = datepart(year,:id_index_date);

IF as_earnings_type = 'Frozen' THEN
	 SELECT max_td1_exemption,   
		  max_annual_gross_earnings,
		  cpp_rate,
		  cpp_max_deduction,
		  cpp_exemption,
		  cppd_index_factor,
		  cpi_index_factor,
		  uic_rate,
		  uic_min_earnings,
		  uic_max1_insurable_earnings,
		  uic_max2_premium,
 	       v_prov_tax_rate,
		  yearly_factor_date,
		  release_no,
		  canada_employment_credit
	INTO :idec_max_td1 ,              
		  :idec_max_annual_gross ,    
		  :idec_cpp_rate ,
		  :idec_cpp_max ,
		  :idec_cpp_exempt,
		  :idec_cppd_index,
		  :idec_cpi_index,
		  :idec_uic_rate ,
		  :idec_uic_min ,
		  :idec_uic_max_earnings,
		  :idec_uic_max_premium ,
		  :idec_V ,
		  :idt_from_yearly_factor  ,
		  :ii_release,
		  :idec_can_emp_cr
   FROM Yearly_Factor  
     WHERE yearly_factor_date = :ldtm_yearly_factor_mth
		 AND release_no           = (SELECT Max(release_no)
 				                	  	       FROM    Yearly_Factor 
										  WHERE  yearly_factor_date = :ldtm_yearly_factor_mth)
		;
											
ELSEIF li_effective_year > Year(id_index_date) THEN				// use july of year prev to effective year
	 SELECT max_td1_exemption,   
		  max_annual_gross_earnings,
		  cpp_rate,
		  cpp_max_deduction,
		  cpp_exemption,
		  cppd_index_factor,
		  cpi_index_factor,
		  uic_rate,
		  uic_min_earnings,
		  uic_max1_insurable_earnings,
		  uic_max2_premium,
 	       v_prov_tax_rate,
		  yearly_factor_date ,
		  release_no,
		  canada_employment_credit
	INTO :idec_max_td1 ,              
		  :idec_max_annual_gross ,    
		  :idec_cpp_rate ,
		  :idec_cpp_max ,
		  :idec_cpp_exempt,
		  :idec_cppd_index,
		  :idec_cpi_index,
		  :idec_uic_rate ,
		  :idec_uic_min ,
		  :idec_uic_max_earnings,
		  :idec_uic_max_premium ,
		  :idec_V ,
		  :idt_from_yearly_factor,
		  :ii_release,
		  :idec_can_emp_cr
   FROM Yearly_Factor  
     WHERE yearly_factor_date = :ldtm_yearly_factor
		 AND release_no         = (SELECT  Max(release_no)
 				                	  	       FROM    Yearly_Factor 
										  WHERE  yearly_factor_date = :ldtm_yearly_factor)
											;
											
ELSEIF li_effective_year = Year(id_index_date) THEN   //use same year as index date year
	CHOOSE CASE li_effective_month
		CASE IS <= 6			//use jan of effective year
			 SELECT max_td1_exemption,   
					  max_annual_gross_earnings,
					  cpp_rate,
					  cpp_max_deduction,
					  cpp_exemption,
					  cppd_index_factor,
					  cpi_index_factor,
					  uic_rate,
					  uic_min_earnings,
					  uic_max1_insurable_earnings,
					  uic_max2_premium,
					  v_prov_tax_rate,
					  yearly_factor_date,
					  release_no,
					  canada_employment_credit
				INTO :idec_max_td1 ,              
					  :idec_max_annual_gross ,    
					  :idec_cpp_rate ,
					  :idec_cpp_max ,
					  :idec_cpp_exempt,
					  :idec_cppd_index,
					  :idec_cpi_index,
					  :idec_uic_rate ,
					  :idec_uic_min ,
					  :idec_uic_max_earnings,
					  :idec_uic_max_premium ,
					  :idec_V ,
					  :idt_from_yearly_factor,
					  :ii_release,
					  :idec_can_emp_cr
				FROM Yearly_Factor  
				  WHERE yearly_factor_date = :ldtm_min_yearly_factor
					 AND release_no         =   (SELECT  Max(release_no)
 				                	  	      			   FROM    Yearly_Factor
											      	   WHERE  yearly_factor_date = :ldtm_min_yearly_factor)
														;
	CASE IS >= 7				// use july of effective year
		SELECT max_td1_exemption,   
			  max_annual_gross_earnings,
			  cpp_rate,
			  cpp_max_deduction,
			  cpp_exemption,
			  cppd_index_factor,
			  cpi_index_factor,
			  uic_rate,
			  uic_min_earnings,
			  uic_max1_insurable_earnings,
			  uic_max2_premium,
			  v_prov_tax_rate,
			  yearly_factor_date,
			  release_no,
			  canada_employment_credit
		INTO :idec_max_td1 ,              
			  :idec_max_annual_gross ,    
			  :idec_cpp_rate ,
			  :idec_cpp_max ,
			  :idec_cpp_exempt,
			  :idec_cppd_index,
			  :idec_cpi_index,
			  :idec_uic_rate ,
			  :idec_uic_min ,
			  :idec_uic_max_earnings,
			  :idec_uic_max_premium ,
			  :idec_V ,
			  :idt_from_yearly_factor,
			  :ii_release,
			  :idec_can_emp_cr
		FROM Yearly_Factor  
		  WHERE yearly_factor_date = :ldtm_yearly_factor
			 AND release_no         =  (SELECT  Max(release_no)
 				                	  	      	  FROM    Yearly_Factor
			 								  WHERE  yearly_factor_date = :ldtm_yearly_factor)
												;
	END CHOOSE
END IF

// PR 1890 - If type of earnings is 'Frozen' then we need to reset the yearly factor to the index date
//IF as_earnings_type = 'Frozen' THEN
//	idt_from_yearly_factor = DateTime(id_index_date)
//END IF


SQLCA.nf_handle_error("Embedded SQL: Retrieve Yearly_Factors","f_populate_work_variables","f_populate_work_variables")	

/* P10151-55 - J. Hawker, 2006.06.14 - Since there can be more than one period for certain date
   (i.e. release # 1 and 2 etc...), added DISTINCT to the SELECT statement
*/

SELECT Count(DISTINCT yearly_factor_date)
INTO   :ii_period
FROM   Yearly_Factor  
WHERE  yearly_factor_date  <= :idt_from_yearly_factor;

SQLCA.nf_handle_error("Embedded SQL: Retrieve count Yearly_Factors","f_populate_work_variables","f_populate_work_variables")
		  

RETURN 1

end function

public function integer f_populate_bencalc_vars (long al_claim_no, integer al_benefit_calc_no, integer al_opening_no); 
 /* select Defined variables based on the period being processed
 idec_cpp_rate         // - The applicable cpp rate for a given year
 idec_cpp_max          // - The maximum annual cpp deduction for a given year
 idec_cpp_exempt       // - The applicable annual cpp exemption for a given year
 idec_uic_rate         // - the applicable uic rate for a given year
 idec_uic_max_premium  // - the applicable maximum annual uic premium for a given year
 idec_uic_max_earnings // - The applicable maximum annual uic insurable earnings for a given year
 idec_uic_min          // - the applicable minimum annual insurable earnings for a given year
 idec_V              // - the applicable provincial tax rate for a given year
 idec_max_annual_gross
 idec_max_td1
 ii_release 		
*/
/* Make sure that the date is valid - it must be between the following values
*/
 
/* P10151-55 - J. Hawker, 2006.06.14 - Added release_no and canada_employment_credit 
   to the SELECT statement and also modified the FROM clause. These values are no longer 
	stored in the BENEFIT_CALCULATION table and need to be retrieved from the Yearly_Factor table
*/ 
 
 SELECT a.max_td1_exemption,   
		  a.max_annual_gross_earnings,
		  a.cpp_rate,
		  a.cpp_max_deduction,
		  a.cpp_exemption,
		  a.cppd_index_factor,
		  a.cpi_index_factor,
		  a.uic_rate,
		  a.uic_min_earnings,
		  a.uic_max1_insurable_earnings,
		  a.uic_max2_premium,
           a.v_prov_tax_rate,
		  b.yearly_factor_date,
		  a.release_no,
		  a.canada_employment_credit
	INTO :idec_max_td1 ,              
		  :idec_max_annual_gross ,    
		  :idec_cpp_rate ,
		  :idec_cpp_max ,
		  :idec_cpp_exempt,
		  :idec_cppd_index,
		  :idec_cpi_index,
		  :idec_uic_rate ,
		  :idec_uic_min ,
		  :idec_uic_max_earnings,
		  :idec_uic_max_premium ,
		  :idec_V ,
		  :idt_from_yearly_factor,
		  :ii_release,
		  :idec_can_emp_cr
   FROM Yearly_Factor a, BENEFIT_CALCULATION b
  WHERE a.yearly_factor_date     = b.yearly_factor_date
	 AND a.release_no                 = b.release_no
    AND b.claim_no                     = :al_claim_no 
	 AND b.benefit_calculation_no = :al_benefit_calc_no 
	 AND b.opening_no                = :al_opening_no;

SQLCA.nf_handle_error("Embedded SQL: Retrieve Yearly_Factors","f_populate_bencalc_variables","f_populate_bencalc_variables")


/* P10151-55 - J. Hawker, 2006.06.14 - Since there can be more than one period for certain date
   (i.e. release # 1 and 2 etc...), added DISTINCT to the SELECT statement
*/
 SELECT Count(DISTINCT yearly_factor_date)
   INTO :ii_period
   FROM Yearly_Factor  
  WHERE yearly_factor_date  <= :idt_from_yearly_factor;

SQLCA.nf_handle_error("Embedded SQL: Retrieve count Yearly_Factors","f_populate_bencalc_variables","f_populate_bencalc_variables")
		  

RETURN 1

end function

public function decimal f_calculate_tax (date vad_index_date, decimal vac_exemption_amt, decimal vac_prov_exemption, decimal vac_annual_gross, decimal c, decimal u, datetime adt_create_datetime);// ----------------------------------------------------------------------------------------------------	
// Function Name: f_calculate_tax																								
//																																			
// Purpose:       This function calculates Tax using figures based on the index date passed.					
//																																			
// Arguments:     Parameters passed by calling routine:																	
//																																			
//                vad_index_date     	- The date for which the calculation is to be performed (must		
//                                       be a date not a year since tax calculations can change				
//                                       within a year)																	
//						vac_exemption_amt		- The TD1 exemption amount														
//                vac_annual_gross   	- The annual gross income for which CPP is applicable					
//						C						 	- Annual CPP deduct amount														
//						U						 	- Annual UIC deduct amount														
//						adt_create_datetime	- The date the ben calc was created (this is only applicable for  
//													  1997 ben calcs)																	
//																																			
// Return Values: Annual Tax																										
//																																			
// ----------------------------------------------------------------------------------------------------	
Decimal	K1, K2, T3, T3_current, K4, K4_income
Decimal	rfactor,	kfactor,	D
Decimal	A, T1, V1, T2
Decimal	V, T, li_r_rate, li_v_rate, li_exceeding_vt3_amount, li_rate
DECIMAL	K1P, K2P, T4	// Added for SR64 - Jan. 2001 Tax Changes

DECIMAL		ldec_exceeding_amt , ldec_not_exceeding_amt
DECIMAL		ldec_tax_reduction_base_amt , ldec_phase_out_rate
INTEGER		li_rows
U_DS			lds_Prov_Low_Income_Tax_Reduction


//PR1890 removed hardcoding - table driven
/*IF ii_period = 31 OR ii_period = 32 THEN
	// The UIC, CPP and provincial tax rates changed July 1, 1997, effective for the entire year, but as
	//	the changes were implemented in the middle of the year we weren't notified and so we have many
	//	ben calcs with the wrong values. We never change a ben calc once created, and don't store all
	//	fields (ie CPP and UIC), so to recalculate the values (although incorrectly) for all ben calcs created
	//	in 1997, we have to do this....
	IF adt_create_datetime > DateTime(Date("1997/01/01"),Time("00:00:00 AM")) AND &
		adt_create_datetime < DateTime(Date("1998/01/09"),Time("11:59:59 AM")) THEN
		idec_v	=	0.640
	ELSE	
		idec_v	=	0.630		// changed Jan9/98 from 0.64
	END IF
END IF */

// -----------------------------------------------------------------------------------------------------
//	Translation:
// ------------
// 	A	-	Annual Taxable Income
//		P	-	Number of pay periods in year (1 - Annual)
//		I	-	Gross Remuneration (vac_annual_gross)
//		F	-	Employee's registered pension plan
//		U1	-	Union Dues
//		HD	-	Annual deduction for residing in a prescribed area per line 16
//				of TD1 revised 1988 07 01
//		F1	-	Annual deduction authorized by district taxation office
//		T3	-	Annual Basic Federal Tax
//		R	-	rfactor
//		K	-	kfactor
//		K1	-	Net federal tax credit
//		K2	-	Annual cpp tax credit & Annual uic tax credit
//		K3	-	Other federal tax credits
//    K4 -  Canada Employment Credit								//Added for P10151-55
//		K1P -	Provincial non-refundable personal tax credit	//Added for SR64
//		K2P -	CPP & EI prvincial tax credits						//Added for SR64
//		D	-	Federal Surtax
//		T1	-	Annual Federal Tax Deduction
//		T2	-	Annual Provincial Tax Deduction
//		T3 -	Annual basic federal tax
//		T4 -	Annual basic provincial tax							//Added for SR64
//		T	-	Annual Federal and Provincial Tax Deduction
//		V	-	Provincial Tax Rate
//		V1	-	Provincial Surtax
// -----------------------------------------------------------------------------------------------------

// Annual Taxable Income (A)
// 	A = P (I - F - U1) - HD - F1
//				Since F, U1, HD and F1 are ignored, this calculation can be
//				simplified to A = I
	A = vac_annual_gross

/* P10151-55, J. Hawker - Formula change for July 2006 tax changes,
                          K4 is included in the calculation.
*/
// Annual Basic Federal Tax (T3)
// 	T3 = (R * A) - K - K1 - K2 - K3 - K4
//				where K1 = 0.17 * exemption amount
//		      		K2 = 0.17 * (annual cpp & uic)
//		      		K3   Ignored

//	IF index_date >= July 1/98 and <= Dec 31/98 THEN changes to K1 
//	calculation apply

//	Non-Refundable Personal Tax Credit (K1)
//		Where A1 <= $6,956
//			K1 = 0.17 x (TC + $500)
//		Where A1 > $6,956 <= $19,456
//			K1 = 0.17 x (TC + [$500 - (0.04 x [A1 - $6,956])])
//		Where A1 > $19,456
//			K1 = 0.17 x TC


/* P10151-55 - J. Hawker, 2006.06.14 - The release_no column has been added to the R_And_K  
               table to keep track of any changes that are made to the tax information. 
*/
SELECT r_rate
INTO   :li_r_rate
FROM   R_And_K
WHERE  datepart(year,effective_date)  = datepart(year,:vad_index_date) 
AND    datepart(month,effective_date) = datepart(month,:vad_index_date) 
AND    exceeding_taxable_income       = 0 
AND    release_no                     = :ii_release
USING  SQLCA;

SQLCA.nf_handle_error("Embedded SQL: Retrieve r_rate from R_And_K","f_calculate_tax","f_calculate_tax")

/* P10151-55 - J. Hawker, 2006.06.14 - The release_no column has been added to the Nb_V_And_Kp  
               table to keep track of any changes that are made to the tax information. 
*/
SELECT v_rate
INTO   :li_v_rate
FROM   Nb_V_And_Kp
WHERE  datepart(year,effective_date)  =  datepart(year,:vad_index_date)
AND    datepart(month,effective_date) =  datepart(month,:vad_index_date) 
AND    exceeding_taxable_income       = 0
AND    release_no                     = :ii_release
USING SQLCA;

SQLCA.nf_handle_error("Embedded SQL: Retrieve v_rate from Nb_V_And_K","f_calculate_tax","f_calculate_tax")
 

	IF vad_index_date >= Date('1998 07 01') AND vad_index_date <= Date('1998 12 31') THEN
		IF A <= 6956 THEN
			K1 = Round(li_r_rate * (vac_exemption_amt + 500), 2)
		ELSEIF A > 6956 AND A <= 19456 THEN
			K1 = Round(li_r_rate * (vac_exemption_amt + (500 - (0.04 * (A - 6956)))), 2)
		ELSE // i.e. A > 19456
			K1 = Round(li_r_rate * vac_exemption_amt, 2)
		END IF
//PR1832 - March 1,2001		
	ELSEIF vad_index_date >= Date('2001 01 01') THEN
		// Dates on or after Jan. 1, 2001 - rate reduced from 17% to 16% effective Jan. 1/01
		K1 = Round(li_r_rate * vac_exemption_amt,2)
	ELSE	// Dates 1) before Jul 1/98, or 2) after Dec 31/98 but before Jan. 1, 2001
		K1 = Round(li_r_rate * vac_exemption_amt,2)
	END IF

	
// SR 64 - Jan. 2001 tax changes - New TONI (tax on income) method for provincial tax calculations effective 01/01/2001
// Calculate Provincial non-refundable personal tax credit
	IF vad_index_date >= Date('2001 01 01') THEN
		K1P = li_v_rate * vac_prov_exemption
	END IF
	
//		OLD VERSIONS OF K1 & K2 FORMULAS 
//		K1 =	Round(0.17 * vac_exemption_amt,2)
//		K2 =	Round(0.17 * (C + U),2)

// SR 64 - Jan. 2001 tax changes - federal rate reduced from .17 to .16
/* PR4097 - The value has changed from 0.16 to 0.15 for 2006. The values should not be hard coded
   but due to lack of time, this is a temporary solution.   Further research will be done
	in a future PR - PR# 5400.
*/
/* PR5400 - 2006.05.11, J.Hawker - The value that was hard coded when calculating the UIC tax 
   credit has been replaced with li_r_rate so the application will not need to be changed 
	everytime the federal rate changes.
*/
	
	K2	= Round(li_r_rate * C,2) + Round(li_r_rate * U,2)
	
// SR 64 - Jan. 2001 tax changes - New TONI (tax on income) method for provincial tax calculations effective 01/01/2001
// Calculate Provincial CPP & EI contribution tax credits
	IF vad_index_date >= Date('2001 01 01') THEN
		K2P = Round(li_v_rate * (C + U), 2)
	END IF

/* P10151-55, J. Hawker - Calculate K4 for formula change
*/
	
	K4_income = Round(li_r_rate * A,2)
	
	K4 = Round(li_r_rate * idec_can_emp_cr,2)   
															 
	IF K4_income < K4 THEN
		K4 = K4_income
	END IF
	
	IF K4 < 0 THEN						  
		K4 = 0
	END IF

//	Annual basic federal tax (T3)
/* P10151-55, J. Hawker - Formula change for July 2006 tax changes,
                          K4 is included in the calculation.
*/
	T3 =	Round(idec_r * A, 2) - idec_k - K1 - K2 - K4
	IF T3 < 0 THEN
		T3 = 0
	END IF

// SR 64 - Jan. 2001 tax changes - New TONI (tax on income) method for provincial tax calculations effective 01/01/2001
// Calculate Basic Provincial Tax (T4)
	//	T4 = (V x A) - KP - K1P - K2P 
	IF vad_index_date >= Date('2001 01 01') AND vad_index_date < Date('2001 07 01') THEN
		T4 = (idec_nb_v * A) - idec_kp - K1P - K2P	
	END IF

//If the Annual Income < 10000 provincial tax = 0
//IF Annual Income between 10,000 and 15,200 provincial tax calculated at a reduced rate.

//PR3368 - Instead of using datastore, now using embedded select to prevent further app. changes

/* P10151-55 - J. Hawker, 2006.06.14 - The release_no column has been added to the Prov_Low_Income_Tax_Reduction  
               table to keep track of any changes that are made to the tax information. 
*/
	SELECT	exceeding_amount , not_exceeding_amount , tax_reduction_base_amount , phase_out_rate
	INTO 		:ldec_exceeding_amt , :ldec_not_exceeding_amt , :ldec_tax_reduction_base_amt , :ldec_phase_out_rate
	FROM		Prov_Low_Income_Tax_Reduction      
	WHERE	   effective_date = :vad_index_date
	AND      release_no     = :ii_release
	USING    SQLCA;   

	IF SQLCA.SQLCODE = 0 THEN
	
		IF A < ldec_exceeding_amt THEN
			T4 = 0
		ELSE	
			IF A >= ldec_exceeding_amt AND A < ldec_not_exceeding_amt THEN
				T4 = (idec_nb_v * A) - (ldec_tax_reduction_base_amt -(ldec_phase_out_rate*(A - ldec_exceeding_amt))) - idec_kp - K1P - K2P
			ELSE 	
				T4 = (idec_nb_v * A) - idec_kp - K1P - K2P
			END IF
		END IF	
	END IF
	
	IF T4 < 0 THEN
		T4 = 0
	END IF

//	IF index_date >= July 1/98 and <= Dec 31/98 THEN changes to 
//	calculation of D apply                        
//
//	Federal Surtax (D)
//		Where T3 <= $8,333
//			D = $0
//		Where T3 > $8,333 <= $12,500
//			D = 0.03 x T3 - ($250 - [0.06 x (T3 - $8,333)]), max $375
//		Where T3 > $12,500
//			D = 0.03 x T3 + [0.05 x (T3 - $12,500)]
//
//  Changes by Ed Lenarczyk - July 09, 1999
//    Further changes made to Federal Surtax calculation.  This has been cut in half
//	 but because half of the year has already passed, Surtax is removed for 
//	 the amounts below $12,500.
	
//	IF vad_index_date >= Date('2000 01 01') AND vad_index_date <= Date('2000 12 31') THEN
//		IF T3 <= 12500 THEN
//			D = 0
//		ELSE
//			D = Round(0.05 * (T3 - 12500),2)
//			IF D < 0 THEN 
//				D = 0
//			END IF
//		END IF

	IF vad_index_date >= Date('2001 01 01') THEN		// SR 64 - Federal surtax eliminated effective Jan. 1, 2001
		D = 0
	ELSEIF ii_assessment_year = 2000 AND vad_index_date >= Date('2000 01 01') THEN  //PR 1890 - kdm
		IF vad_index_date >= Date('2000 07 01') THEN
			T3_current = 18500
		ELSE
			T3_current = 12500
		END IF

		IF T3 <= T3_current THEN
			D = 0
		ELSE
			D = Round(0.05 * (T3 - T3_current),2)
			IF D < 0 THEN 
				D = 0
			END IF
		END IF
	//
	//   Ed Lenarczyk  -  End changes  Jun 29, 2000
	//
	ELSEIF vad_index_date >= Date('1999 07 01') AND vad_index_date <= Date('1999 12 31') THEN
		IF T3 <= 12500 THEN
			D = 0
		ELSE
			D = Round(0.05 * (T3 - 12500),2)
		END IF
	ELSEIF vad_index_date >= Date('1998 07 01') AND vad_index_date < Date('1999 07 01') THEN
		IF T3 <= 8333 THEN
			D = 0
		ELSEIF T3 > 8333 AND T3 <= 12500 THEN
			D = .03 * T3 - (250 - (.06 * (T3 - 8333)))
			IF D > 375 THEN
				D = 375
			END IF
		ELSE /* IF T3 > 12500 */
			D = Round((0.03 * T3) + (0.05 * (T3 - 12500)),2)
		END IF
	ELSEIF vad_index_date < Date('1998 07 01') THEN  // Index dates prior to July 1/98
		IF T3 <= 12500 THEN
			D = Round(0.03 * T3, 2)
		ELSE
			D = Round((0.03 * T3) + (0.05 * (T3 - 12500)),2)
		END IF
	END IF

	IF D < 0 THEN
		D = 0
	END IF

//	Annual Federal Tax Deduction (T1)
//		T1 = T3 + D
	T1 = T3 + D

// SR 64 - Jan. 2001 tax changes - New TONI (tax on income) method for provincial tax calculations effective 01/01/2001
// Calculate Provincial Tax Deduction (T2)
// Formula:  T2 = T4 + V1


//Removed hardcoding of Provincial Surtax June 18,2001

/* P10151-55 - J. Hawker, 2006.06.14 - The release_no column has been added to the Prov_Tax_Income_Parameter  
               table to keep track of any changes that are made to the tax information. 
*/

SELECT rate, exceeding_vt3_amount
INTO   :li_rate, :li_exceeding_vt3_amount
FROM   Prov_Tax_Income_Parameter
WHERE  datepart(year,effective_date)  =  datepart(year,:vad_index_date) 
AND    datepart(month,effective_date) =  datepart(month,:vad_index_date) 
AND    exceeding_vt3_amount          <= :T4 
AND    not_exceeding_vt3_amount       > :T4
AND    release_no                     = :ii_release
USING  SQLCA;

SQLCA.nf_handle_error("Embedded SQL: Retrieve rate from Prov_Tax_Income_Parameter","f_calculate_tax","f_calculate_tax")



//	IF vad_index_date >= Date('2001 Jan 01') THEN
//	//	First need to calculate provincial surtax
//		IF T4 <= 13500.00 THEN
//			V1 = 0
//		ELSEIF T4 > 13500 THEN
//			V1 = 0.08 * (T4 - 13500)
//		END IF

// Annual Provincial Tax Deduction (T2)
//		T2 = (V * T3) + V1
//
//		Note: A change to NB Provincial taxes on 19930701 forced the change to the 
//				formula for V1 for 19930701 thru 19931231 -- The previous formula used 
//				V throughout, but due to the fact that this change in the provinical rate  
//				was retro-active to January , the value of V from July to December (1993) 
//				is .64, while the actual new rate is .62 for the year.  Due to this, .62 is hard-
//				coded in the formula for tax dates of 19930701 thru 19931231 as specified in MC59(E).
//
//				where V1 (19930701 to 19931231)	=  IF (.62 * T3) <= 13500 THEN v1 = 0
//																IF (.62 * T3) > 13500 and <= 13950 THEN v1 = .16 * [(.62 * T3) - 13500]
//																IF (.62 * T3) > 13950 THEN v1 = .08 * [(V * T3) - 13500]
//				where V1 (all other dates)			= 	IF (V * T3) <= 13500 THEN V1 = 0
//																IF (V * T3) >  13500 THEN V1 = 0.08 * ((V * T3) - 13500)

// Change made to the formula below by Ed Lenarczyk  Jan 29, 2000
//	 To accommodate change in the middle of the year.  Taxation Year as well as 
//  index date have to be taken into account when calculating Provincial Tax.

IF vad_index_date >= Date('2001 Jan 01') THEN
	V1 = li_rate * (T4 - li_exceeding_vt3_amount)

ELSEIF ii_assessment_year = 2000 AND vad_index_date >= Date('2000 Jul 01') THEN
		
		/* populate our work variables from the table - Prov_Tax_Income_Parameter we need to 
         populate the following work variables (instance) idec_income_parameter_rate 
	      and idec_fixed_amount - for project 10105
      */
		f_get_income_parameter(vad_index_date,(0.57 * T3))
	
		IF (0.57 * T3) > 13500 THEN
			V1 = idec_fixed_amount + (idec_income_parameter_rate * ((idec_v * T3) - 13500))
		ELSEIF (0.57 * T3) > 6179 THEN
			V1 = idec_fixed_amount
		ELSEIF (0.57 * T3) > 5952 THEN
			V1 = idec_fixed_amount + (idec_income_parameter_rate * ((idec_v * T3) - 5952))
		ELSEIF (0.57 * T3) > 2065 THEN
			V1 = idec_fixed_amount + (idec_income_parameter_rate * ((idec_v * T3) - 2065))
		ELSEIF (0.57 * T3) > 1989 THEN
			V1 = idec_income_parameter_rate * ((idec_v * T3) - 1989)
		ELSE
			V1 = 0.00
		END IF
	ELSEIF vad_index_date > Date('1993 06 30') and vad_index_date < Date('1994 01 01') THEN
		IF (0.62 * T3) <= 13500 THEN
			V1 = 0
		ELSEIF (0.62 * T3) <= 13950 THEN
			V1 = 0.16 * ((0.62 * T3) - 13500)
		ELSE
			V1 = 0.08 * ((idec_v * T3) - 13500)
		END IF
	ELSE
		IF (idec_v * T3) <= 13500 THEN
			V1 = 0
		ELSE
			V1 = 0.08 * ((idec_v * T3) - 13500)
		END IF
	END IF

// SR129 July 2001 Tax Changes SMANZER
	IF V1 < 0 THEN 
		V1 = 0
	END IF

/* Calculate provincial tax deduction - Added for SR 64 (Jan. 2001 Tax Changes)
*/
	IF vad_index_date < Date("2001 01 01") THEN
		T2 = Round((idec_v * T3) + V1,2)
	ELSE	// Jan. 1, 2001 or later
		T2 = T4 + V1
	END IF

// Annual Federal and Provincial Tax Deduction (T)
//		T	=	T1 + T2
	T	=	T1 + T2			

RETURN T
end function

public function integer f_get_r_and_k (date adt_from_date, decimal adc_amount);/* now select The variables we need from the R_And_K table
*/

/* P10151-55 - J. Hawker, 2006.06.14 - The release_no column has been added to the R_And_K  
               table to keep track of any changes that are made to the tax information. Added 
					release_no to the SELECT statement
*/

DATETIME		ldt_max_date
	
	SELECT max(effective_date)
	INTO   :ldt_max_date
	FROM   R_And_K  
  WHERE   effective_date <= :id_index_date ;
		
SQLCA.nf_handle_error("Embedded SQL: Select... from R_And_K","f_get_r_and_k","f_get_r_and_k")

		
	SELECT r_rate,
			 k_constant
	  INTO :idec_R ,      					
          :idec_K 
	  FROM R_And_K  
    WHERE effective_date = :ldt_max_date
      AND (:adc_amount > exceeding_taxable_income AND 
	       :adc_amount <= not_exceeding_taxable_income) // PR 1890 - kdm
	   AND release_no   = :ii_release;		  
			  
SQLCA.nf_handle_error("Embedded SQL: Select... from R_And_K","f_get_r_and_k","f_get_r_and_k")


RETURN 1
end function

public function integer f_get_v_and_kp (date ad_index_date, decimal adec_annual_tax);/*	SR 64 (Jan. 2001 Tax Changes):  Provincial tax calculations are changing from a tax-on-tax method to a tax-on-income
	(TONI) method.  A new table, Nb_V_And_Kp, created for the new N.B. provincial income brackets and related factors.
	This function will select the required fields from this new Nb_V_And_Kp table.
*/
	
/* P10151-55 - J. Hawker, 2006.06.14 - The release_no column has been added to the Nb_V_And_Kp  
               table to keep track of any changes that are made to the tax information. Added 
					release_no to the SELECT statement
*/	

DATETIME		ldt_max_date

	SELECT max(effective_date)
	INTO   :ldt_max_date
	FROM   Nb_V_And_Kp
  WHERE   effective_date <= :ad_index_date ;
		
SQLCA.nf_handle_error("Embedded SQL: Select... from R_And_K","f_get_v_and_kp","f_get_v_and_kp")

		
	SELECT v_rate,
			 kp_constant
	  INTO :idec_nb_v,      					
          :idec_KP  
	  FROM Nb_V_And_Kp
    WHERE effective_date = :ldt_max_date
      AND (:adec_annual_tax > exceeding_taxable_income AND 
	       :adec_annual_tax <= not_exceeding_taxable_income) 
	   AND release_no = :ii_release;		  
			  
SQLCA.nf_handle_error("Embedded SQL: Select... from Nb_V_And_Kp", "f_get_v_and_k","f_get_v_and_k")


RETURN 1

end function

public function integer f_get_exemption_amounts (date adt_index_date, ref decimal vac_td1_basic_exemption, ref decimal vac_td1_married_exemption, ref decimal adec_prov_td1_basic, ref decimal adec_prov_td1_married);/* ----------------------------------------------------------------------------------------------------	*/
/*	Function Name: 	f_get_exemption_amounts																					*/
/*																																			*/
/*	Purpose:				The purpose of this function is to get the basic personal exemption amount and the	*/
/*							married exemption amount for a specified tax/index year.										*/
/*																																			*/
/*	Arguments:			Integer				-	val_tax_index_year			-	The year to search for				*/
/*							Decimal(Returned)	-	vac_td1_basic_exemption		-	The basic exemption for the year	*/
/*							Decimal(Returned)	-	vac_td1_married_exemption	-	The married exemption amount		*/
/*																																			*/
/*																																			*/
/*	Return Value:		Integer	-	Success				-	1																	*/
/*										-	Failure				-	-1																	*/
/*																																			*/
/* ----------------------------------------------------------------------------------------------------	*/
DATETIME ldtm_yearly_factor

/* P10151-55 - J. Hawker, 2006.06.14 - The release_no column has been added to the Yearly_Factor  
               table to keep track of any changes that are made to the tax information. 
*/

SELECT max(yearly_factor_date)
INTO   :ldtm_yearly_factor
FROM   Yearly_Factor 
WHERE  yearly_factor_date <= :adt_index_date
USING  SQLCA;

SQLCA.nf_handle_error("SELECT max(yearly_factor_date)","f_get_exemption_amounts","f_get_exemption_amounts")

  SELECT td1_basic_exemption,
         td1_married_exemption,
			prov_td1_basic_exemption,
			prov_td1_married_exemption
    INTO :vac_td1_basic_exemption,   
         :vac_td1_married_exemption,
			:adec_prov_td1_basic,
			:adec_prov_td1_married
   FROM Yearly_Factor 
  WHERE yearly_factor_date = :ldtm_yearly_factor
	 AND release_no         = :ii_release
 	;
	
SQLCA.nf_handle_error("Embedded SQL: Retrieve Yearly_Factors","f_get_exemption_amounts","f_get_exemption_amounts")

	IF NOT vac_td1_basic_exemption > 0 THEN
		vac_td1_basic_exemption   = 0
		vac_td1_married_exemption = 0
	END IF

	/*	Added for SR 64:  Jan. 2001 tax changes
	*/
	IF NOT adec_prov_td1_basic > 0 THEN
		adec_prov_td1_basic = 0
		adec_prov_td1_married = 0
	END IF

Return 1


end function

public function integer f_get_income_parameter (date adt_from_date, decimal adc_amount);/* now select The variables we need from the R_And_K table
*/

/* P10151-55 - J. Hawker, 2006.06.14 - The release_no column has been added to the Prov_Tax_Income_Parameter 
               table to keep track of any changes that are made to the tax information.  Added release_no
					to the SELECT statement.
*/

DATETIME		ldt_max_date
	
	SELECT max(effective_date)
	INTO   :ldt_max_date
	FROM   Prov_Tax_Income_Parameter 
  WHERE   effective_date <= :id_index_date ;
		
SQLCA.nf_handle_error("Embedded SQL: Select... from Prov_Tax_Income_Parameter","f_get_income_parameter","f_get_income_parameter")

		
	SELECT fixed_amount,
			 rate
	  INTO :idec_fixed_amount ,      					
          :idec_income_parameter_rate  		
	  FROM Prov_Tax_Income_Parameter  
    WHERE effective_date = :ldt_max_date
      AND (:adc_amount >= exceeding_vt3_amount AND 
	       :adc_amount < not_exceeding_vt3_amount)
		AND release_no  = :ii_release;		  
			  
SQLCA.nf_handle_error("Embedded SQL: Select... from Prov_Tax_Income_Parameter","f_get_income_parameter","f_get_income_parameter")


RETURN 1
end function

public function decimal f_get_max_avg_earnings (date adt_index_date);/* ---------------------------------------------------------------------------------------------------- */
/* Function Name: f_get_max_avg_earnings                                                                */
/*                                                                                                      */
/* Purpose:       The purpose of this function is to obtain the maximum average earnings for a          */
/*						specified year.                                                                       */
/*                                                                                                      */
/* Arguments:     Parameters passed by calling routine:                                                 */
/*                                                                                                      */
/*                adt_index_date - The date 															                 */
/*                                                                                                      */
/*	Return Values:	Successful	-	Returns the max average earnings amount                                */
/*						Error			-	Returns -1                                       							  */
/*																																		  */
/*						  																												  */
/* ---------------------------------------------------------------------------------------------------- */

 /* 
 	NOTE: This function has been altered for P10105 we no longer look at the index year    				  
	      We now use the index date to grab values from the Yearly_Factor table   	 
 */
  
  /* P10151-55 - J. Hawker, 2006.06.14 - The release_no column has been added to the Yearly_Factor  
               table to keep track of any changes that are made to the tax information. Need to first
					get the max. yearly_factor_date and use that when determining the max. release_no below
  */
  
	Decimal	vlc_max_avg_earnings = 0,	vlc_indexed_amount = 0
	Long		vll_counter
	DATETIME ldtm_yearly_factor

	If IsNull(adt_index_date) Then
		Return 0
	End If

	SELECT Max(yearly_factor_date) 
	INTO   :ldtm_yearly_factor
	FROM   Yearly_Factor 
	WHERE  yearly_factor_date <= :adt_index_date;

	SQLCA.nf_handle_error("SELECT Max(yearly_factor_date)","f_get_max_avg_earnings","f_get_max_avg_earnings")

  SELECT max_annual_gross_earnings
    INTO :vlc_max_avg_earnings
    FROM Yearly_Factor  
  WHERE yearly_factor_date = :ldtm_yearly_factor
	 AND release_no         = :ii_release
											  ;

	SQLCA.nf_handle_error("Embedded SQL: Retrieve Maximum Average Earnings","f_get_max_avg_earnings","f_get_max_avg_earnings")

	If IsNull(vlc_max_avg_earnings) Then vlc_max_avg_earnings = 0

	Return vlc_max_avg_earnings
end function

public function decimal f_index_cpi_amount (decimal vac_amount, long val_base_year, date adt_index_date);/* ---------------------------------------------------------------------------------------------------- */
/* Function Name: f_index_cpi_amount                                                                    */
/*                                                                                                      */
/* Purpose:       The purpose of this function is to index a payment amount that was earned in a        */
/*						specified year ahead to the indexation year.                                          */
/*                                                                                                      */
/* Arguments:     Parameters passed by calling routine:                                                 */
/*                                                                                                      */
/*						vac_amount - The payment amount to be indexed (annual amount)                         */
/*                val_base_year - The year in which the payment amount was earned or is known           */
/*                val_index_year- The year in which the payment is to be indexed to                     */
/*                                                                                                      */
/*	Return Values:	Successful	-	Returns the indexed amount                                             */
/*						Error			-	Returns -1   (database error)                                          */
/* ---------------------------------------------------------------------------------------------------- */

/* This function has changed for P10105 table definition of Yearly_Factor has changed to eliminate the 
      the year argument and exchange it for the index date - we want to keep the period i.e. one period per
	   calculation
*/

	Decimal	vlc_cpi_index_factor,	vlc_indexed_amount = 0
	Long		vll_counter,vll_year_counter,vll_year
   STRING	vls_date_creation
	DATE     vdt_work_date
	DATETIME ldtm_yearly_factor

	vlc_indexed_amount 	=	vac_amount
	vll_counter 			=	val_base_year
	vll_year             =  YEAR(adt_index_date)

	Do While vll_counter < vll_year
		/* due to table changes we need to calculate the year date - this may need to be changed depending on
		   the outcome of tax meetings
		*/
			vls_date_creation = string(vll_counter) + "/12/12"
			vdt_work_date     = relativedate(date(vls_date_creation),365)
			IF NOT isdate(string(vdt_work_date)) THEN
				vll_year_counter = -1
				DO UNTIL isdate(string(vdt_work_date))
					vdt_work_date = relativedate(vdt_work_date, vll_year_counter)	
					vll_year_counter   = vll_year_counter -1
				LOOP
			END IF
		
		  /* P10151-55 - J. Hawker, 2006.06.14 - The release_no column has been added to the Yearly_Factor  
               table to keep track of any changes that are made to the tax information. Need to first
					get the max. yearly_factor_date and use that when determining the max. release_no below
        */	
		  SELECT Max(yearly_factor_date)
		  INTO   :ldtm_yearly_factor
		  FROM   Yearly_Factor
		  WHERE  yearly_factor_date <= :vdt_work_date;
		  
		  SQLCA.nf_handle_error("SELECT yearly_factor_date","f_index_amount","f_index_amount")
		  
		
		  SELECT cpi_index_factor 
		  INTO   :vlc_cpi_index_factor
		  FROM   Yearly_Factor
		  WHERE  yearly_factor_date = :ldtm_yearly_factor
		  AND    release_no         = (SELECT Max(release_no)
		  									 FROM   Yearly_Factor
											 WHERE  yearly_factor_date = :ldtm_yearly_factor)
											  ;

			SQLCA.nf_handle_error("Embedded SQL: Retrieve Yearly_Factors","f_index_amount","f_index_amount")
			

			vlc_indexed_amount = vlc_indexed_amount * vlc_cpi_index_factor
			vll_counter ++
	Loop
	
	Return Round(vlc_indexed_amount,2)
end function

public function decimal f_index_cppd_amount (decimal vac_amount, long val_base_year, date adt_index_date);/* ---------------------------------------------------------------------------------------------------- */
/* Function Name: f_index_cppd_amount                                                                   */
/*                                                                                                      */
/* Purpose:       The purpose of this function is to index a cppd amount that was earned in a           */
/*						specified year ahead to the indexation year.                                          */
/*                                                                                                      */
/* Arguments:     Parameters passed by calling routine:                                                 */
/*                                                                                                      */
/*						vac_amount - The cppd amount to be indexed (annual amount)                         	  */
/*                val_base_year - The year in which the amount was earned or is known           		  */
/*                adt_index_date - Extract year in which the amount is to be indexed to                 */
/*                                                                                                      */
/*	Return Values:	Successful	-	Returns the indexed amount                                             */
/*						Error			-	Returns -1   (database error)                                          */
/* ---------------------------------------------------------------------------------------------------- */

/* This function has changed for P10105 table definition of Yearly_Factor has changed to eliminate the 
   the year argument and exchange it for the index date - we want to keep the period i.e. one period per
	calculation
*/

	Decimal	vlc_cppd_index_factor,	vlc_indexed_amount = 0
	Long		vll_year,vll_counter,vll_year_counter
	STRING	vls_date_creation
	DATE     vdt_work_date
	DATETIME ldtm_yearly_factor

   vll_year             =  YEAR(adt_index_date)
	vlc_indexed_amount 	=	vac_amount
	vll_counter 			=	val_base_year

	Do While vll_counter < vll_year
		
		/* due to table changes we need to calculate the year date
		*/
			vls_date_creation = string(vll_counter) + "/12/12"
			vdt_work_date     = relativedate(date(vls_date_creation),365)
			IF NOT isdate(string(vdt_work_date)) THEN
				vll_year_counter = -1
				DO UNTIL isdate(string(vdt_work_date))
					vdt_work_date = relativedate(vdt_work_date, vll_year_counter)	
					vll_year_counter   = vll_year_counter -1
				LOOP
			END IF
		
		  /* P10151-55 - J. Hawker, 2006.06.14 - The release_no column has been added to the Yearly_Factor  
               table to keep track of any changes that are made to the tax information. Need to first
					get the max. yearly_factor_date and use that when determining the max. release_no below
        */		
		  SELECT max(yearly_factor_date) 
		  INTO   :ldtm_yearly_factor
 		  FROM   Yearly_Factor 
		  WHERE  yearly_factor_date <= :vdt_work_date;
		
			SQLCA.nf_handle_error("SELECT max(yearly_factor_date)","f_index_amount","f_index_amount")
			
		
		  SELECT cppd_index_factor 
		  INTO   :vlc_cppd_index_factor
		  FROM   Yearly_Factor  
        WHERE  yearly_factor_date = :ldtm_yearly_factor
		  AND    release_no         = (SELECT Max(release_no)
		  								     FROM    Yearly_Factor
											WHERE   yearly_factor_date = :ldtm_yearly_factor)
	 ;

			SQLCA.nf_handle_error("Embedded SQL: Retrieve Yearly_Factors","f_index_amount","f_index_amount")

			vlc_indexed_amount = vlc_indexed_amount * vlc_cppd_index_factor
			vll_counter ++
	Loop


	Return Round(vlc_indexed_amount,2)
end function

public function integer f_get_max_release (integer ai_old_release, datetime adtm_yearly_factor);INTEGER li_max

SELECT Max(release_no)
INTO   :li_max
FROM   Yearly_Factor
WHERE  yearly_factor_date = :adtm_yearly_factor
USING  SQLCA;

IF li_max <> ai_old_release THEN
	ii_release = li_max
END IF

RETURN ii_release
end function

public function integer f_get_release_no (long al_bencalc_no, long al_claim_no, ref datetime adtm_yearly_factor);// f_get_release_no - Gets the yearly_factor_date and release_no based on the claim number and benefit calculation number.
// 
Integer li_rtn

SELECT release_no, yearly_factor_date
  INTO :ii_release, :adtm_yearly_factor
  FROM BENEFIT_CALCULATION
 WHERE claim_no = :al_claim_no
   AND benefit_calculation_no = :al_bencalc_no
 USING SQLCA ; 

li_rtn = SQLCA.nf_handle_error("n_benefit_calculation", "", "f_get_release_no - SELECT release_no, yearly_factor_date FROM BENEFIT_CALCULATION")


IF IsNull(adtm_yearly_factor) = FALSE THEN
	SELECT MAX(release_no) 
	  INTO :ii_release 
	  FROM Yearly_Factor 
	 WHERE yearly_factor_date = :adtm_yearly_factor ; 

	li_rtn = SQLCA.nf_handle_error("n_benefit_calculation", "", "f_get_release_no - SELECT MAX(release_no) FROM Yearly_Factor ")
END IF 

IF ii_release < 0 THEN
	ii_release = 0
END IF

RETURN ii_release
end function

public subroutine f_set_release_no (integer ai_release_no);//Set the instance variable with the release number that should be used.

ii_release = ai_release_no
end subroutine

public function long f_retrieve_benefit_entitlement (long al_claimno, long al_bencalcno);// f_retrieve_benefit_entitlement - determines if there are any active records existing in the 
//                                  BENEFIT_ENTITLEMENT table for the given claim_no and benefit_calculation_no.
//
//	Arguments:	al_claimno   - The claim_no to use in the PERIODIC_AWARD table search.
//					al_bencalcno - The benefit_calculation_no to use in the BENEFIT_ENTITLEMENT table search.
//
Long    ll_count
Integer li_rtn 

SELECT count(*)
  INTO :ll_count
  FROM BENEFIT_ENTITLEMENT
 WHERE claim_no = :al_claimno 
	AND benefit_calculation_no = :al_bencalcno 
	AND active_flag = 'Y' 
 USING SQLCA ;

IF SQLCA.SQLCode = -1 THEN
	IF SQLCA.SQLDBCODE = 10083 THEN
		ll_count = 0
	ELSE
		Error.Text = SQLCA.SQLErrText
		Error.WindowMenu = "w_calculation_details"
		Error.Object = "n_benefit_calculations"
		Error.ObjectEvent = "f_retrieve_benefit_entitlement"
		SignalError()
		RETURN -1
	END IF
ELSE
	li_rtn = SQLCA.nf_handle_error("n_benefit_calculation", "", "f_retrieve_benefit_entitlement - SELECT count(*) FROM BENEFIT_ENTITLEMENT WHERE claim_no = :al_claimno AND benefit_calculation_no = :al_bencalcno AND active_flag = Y")
END IF

RETURN ll_count

end function

on n_benefit_calculation.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_benefit_calculation.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

