$PBExportHeader$n_return_to_work.sru
forward
global type n_return_to_work from n_pdc
end type
end forward

global type n_return_to_work from n_pdc
end type
global n_return_to_work n_return_to_work

type variables
DATAWINDOW	idw_qual
DATAWINDOW idw_elig
DATASTORE    ids_employment_date
end variables

forward prototypes
public function integer nf_change_item (long al_datawindow)
public function integer nf_check_bus_rule ()
public function integer nf_check_bus_rule_eligibility ()
public function integer nf_check_employment_date (long al_claim_no, long al_incentive_no, long al_tier_no)
public function integer nf_check_qualification_start (long al_claim_no, datetime adtm_employment_start_date, long al_incentive_no)
end prototypes

public function integer nf_change_item (long al_datawindow);Return 0
end function

public function integer nf_check_bus_rule ();Decimal  ldec_est_capable
Long 		ll_row, ll_claim_no, ll_opening_no, ll_count, ll_rtw_incentive_no, ll_tier_no, ll_rtw_incentive,ll_exists
Date		ldt_after_date, ldt_current_date
Datetime ldtm_accident_recurrence_date, ldtm_max_qual_date, ldtm_min_qual_date, ldtm_benefit_end_date
Datetime ldtm_employment_start_date, ldtm_effective_date, ldtm_employment_start_date_last, ldtm_current_date, ldtm_opening_date


ldt_current_date = Date(f_server_datetime())
ldtm_current_date = f_server_datetime()

ll_row = idw_dw[1].getrow()

ll_claim_no =  idw_dw[1].GetItemDecimal(ll_row,'claim_no')
ll_opening_no =  idw_dw[1].GetItemDecimal(ll_row,'opening_no')
ll_rtw_incentive =  idw_dw[1].GetItemDecimal(ll_row,'rtw_incentive_no')

//There must be an opening selected

IF ll_opening_no = 0 OR Isnull(ll_opening_no) THEN
	Messagebox("Error","An Opening Number must be chosen.",Exclamation!)
	idw_dw[1].SetColumn('opening_no')
	idw_dw[1].SetFocus()
	Return -1
END IF 		


// I Check the Estimated Capable Earnings
ldec_est_capable =  idw_dw[1].GetItemDecimal(ll_row,'avg_monthly_capable_earning_amt')

IF ldec_est_capable <= 0 OR Isnull(ldec_est_capable) THEN
	Messagebox("Error","The Estimated Capable Earnings must be greater than zero.",Exclamation!)
	idw_dw[1].SetColumn('avg_monthly_capable_earning_amt')
	idw_dw[1].SetFocus()
	Return -1
END IF 	

// II Check the employment start date 
ldtm_employment_start_date = idw_dw[1].GetItemDateTime(ll_row,'employment_start_date')

IF Not IsNull(ldtm_employment_start_date) THEN
	
	//Check the employment start date against the MIN and MAX qualification Dates
	Select min_qualification_date, max_qualification_date
	Into    :ldtm_min_qual_date, :ldtm_max_qual_date
	From   Rtw_Incentive_Qualification_Parameter
	Where rtw_incentive_type_code = 'JS'
	Using SQLCA;
	
	SQLCA.nf_handle_error("n_return_to_work","nf_check_bus_rule","select from Rtw_Incentive_Qualification_Parameter")
	
	IF ldtm_employment_start_date < ldtm_min_qual_date THEN
		Messagebox("Error","The Employment Start Date must be after the minimum qualification date of "+String(ldtm_min_qual_date,"YYYY-MM-DD")+" .",Exclamation!)
		idw_dw[1].SetColumn('employment_start_date')
		idw_dw[1].SetFocus()
		Return -1
	END IF
	
	IF ldtm_employment_start_date > ldtm_max_qual_date THEN
		Messagebox("Error","The Employment Start Date must be before the maximum qualification date of "+String(ldtm_max_qual_date,"YYYY-MM-DD")+" .",Exclamation!)
		idw_dw[1].SetColumn('employment_start_date')
		idw_dw[1].SetFocus()
		Return -1
	END IF
	
	//The employment start date must be on or after the benefit start date of the associated opening.
		
	Select   benefit_start_date
	Into     :ldtm_opening_date
	From   OPENING
	Where claim_no = :ll_claim_no
	And     opening_no = :ll_opening_no
	Using   SQLCA;
	
	SQLCA.nf_handle_error("n_return_to_work","nf_check_bus_rule","select from OPENING")
	
	IF ldtm_employment_start_date < ldtm_opening_date THEN
		Messagebox("Error","The Employment Start Date must be on or after the benefit start date of  "+String(ldtm_opening_date,"YYYY-MM-DD")+" .",Exclamation!)
		idw_dw[1].SetColumn('employment_start_date')
		idw_dw[1].SetFocus()
		Return -1
	END IF
		

	//Check for additional qualification records
	
	Select		count(*)
	Into        :ll_count
	From      RTW_INCENTIVE_QUALIFICATION
	Where    claim_no = :ll_claim_no
	Using      SQLCA;
	
	SQLCA.nf_handle_error("n_return_to_work","nf_check_bus_rule","select from Rtw_Incentive_Tier")
	
	IF ll_count > 1  THEN
		IF nf_check_qualification_start(ll_claim_no,ldtm_employment_start_date,ll_rtw_incentive) < 0 THEN
				RETURN -1
		END IF	


	ELSEIF ll_count = 1 THEN
			//Check that the employment start date is after the last qualification's employment start date.
	
		Select   a.employment_start_date, a.rtw_incentive_no
		Into      :ldtm_employment_start_date_last, :ll_rtw_incentive_no
		From    RTW_INCENTIVE_QUALIFICATION a
		Where  a.claim_no = :ll_claim_no
		And      a.rtw_incentive_no <> :ll_rtw_incentive
    		And      a.employment_start_date = (Select MAX(employment_start_date)
					    									From      RTW_INCENTIVE_QUALIFICATION
													    Where    claim_no = :ll_claim_no
														And        rtw_incentive_no <> :ll_rtw_incentive)
		Using SQLCA;
		
		SQLCA.nf_handle_error("n_return_to_work","nf_check_bus_rule","select from RTW_INCENTIVE_QUALIFICATION")
		
		Select  DATEADD(MONTH,MAX(a.months_from_employment_start),:ldtm_employment_start_date_last)
		Into    :ldt_after_date
		From   Rtw_Incentive_Tier AS a
		Join     RTW_INCENTIVE_PAYMENT_ELIGIBILITY AS b
		On       a.tier_no = b.tier_no
		And     b.claim_no = :ll_claim_no
		And     b.rtw_incentive_no = :ll_rtw_incentive_no
		Using   SQLCA;
		
		SQLCA.nf_handle_error("n_return_to_work","nf_check_bus_rule","select from Rtw_Incentive_Tier")

		IF IsNull(ldt_after_date) THEN   // There isn't a payment eligibility use the greatest qualification start date to compare.
				ldt_after_date = DATE(ldtm_employment_start_date_last)
		END IF	
		
		IF DATE(ldtm_employment_start_date) < ldt_after_date THEN
			Messagebox("Error","The Employment Start Date must be after the latest employment start date and greatest tier.",Exclamation!)
			idw_dw[1].SetColumn('employment_start_date')
			idw_dw[1].SetFocus()
			Return -1
		END IF	
    END IF

	
	// The start date must be after the opening's accident recurrence date
	
	Select  accident_recurrence_date, benefit_end_date
	Into     :ldtm_accident_recurrence_date, :ldtm_benefit_end_date
	From   OPENING
	Where claim_no = :ll_claim_no
	And     opening_no = :ll_opening_no
	Using SQLCA;
	
	SQLCA.nf_handle_error("n_return_to_work","nf_check_bus_rule","select from OPENING")
	
	IF ldtm_employment_start_date <= ldtm_accident_recurrence_date THEN
		Messagebox("Error","The Employment Start Date must be after the accident/recurrence date.",Exclamation!)
		idw_dw[1].SetColumn('employment_start_date')
		idw_dw[1].SetFocus()
		Return -1
	END IF
		
	IF ldtm_employment_start_date < ldtm_benefit_end_date THEN
		Messagebox("Warning","The Employment Start Date should be on or after the benefit end date.",Exclamation!)
	END IF		
	
END IF
	
// III The combination of estimated capable earnings and the employment start date must be unique.

Select  count(*)
Into     :ll_count
From   RTW_INCENTIVE_QUALIFICATION
Where claim_no = :ll_claim_no
And    avg_monthly_capable_earning_amt = :ldec_est_capable
And    employment_start_date = :ldtm_employment_start_date
And    rtw_incentive_no <> :ll_rtw_incentive
Using SQLCA;

SQLCA.nf_handle_error("n_return_to_work","nf_check_bus_rule","select from RTW_INCENTIVE_QUALIFICATION III")
	
IF ll_count >= 1 THEN 
		Messagebox("Error","The combination of estimated capable earnings and the employment start date must be unique.",Exclamation!)
		idw_dw[1].SetColumn('employment_start_date')
		idw_dw[1].SetFocus()
		Return -1
END IF 

Return 0
end function

public function integer nf_check_bus_rule_eligibility ();Decimal  ldec_est_capable, ldc_tier_income
Long 		ll_row, ll_claim_no, ll_opening_no, ll_count, ll_rtw_incentive_no, ll_tier_no, ll_rtn, ll_count_tiers
Date		ldt_after_date, ldt_current_date
Datetime ldtm_accident_recurrence_date, ldtm_max_qual_date, ldtm_min_qual_date, ldtm_benefit_end_date
Datetime ldtm_employment_start_date, ldtm_effective_date
String     ls_qualification_flag, ls_eligibility_flag


ldt_current_date = Date(f_server_datetime())

ll_row = idw_dw[1].getrow()

ll_claim_no =  idw_dw[1].GetItemDecimal(ll_row,'claim_no')
ll_opening_no =  idw_dw[1].GetItemDecimal(ll_row,'opening_no')
ls_qualification_flag = idw_dw[1].GetitemString(ll_row,'qualification_flag')
ll_row = idw_dw[2].getrow()
ll_tier_no = idw_dw[2].getitemnumber(ll_row,'tier_no')
ldc_tier_income = idw_dw[2].getitemnumber(ll_row,'avg_monthly_employment_income_amt')
ls_eligibility_flag =  idw_dw[2].getitemstring(ll_row,'payment_eligibility_flag')

IF ls_qualification_flag = 'N' THEN
	Messagebox("Error","An eligibility can not be entered for an incentive that has not qualified, please cancel.",Exclamation!)
	Return -1
END IF


IF Isnull(ll_tier_no) THEN
	Messagebox("Error","A tier no must be entered.",Exclamation!)
	idw_dw[2].SetColumn('tier_no')
	idw_dw[2].SetFocus()
	Return -1
END IF	

// The Rtw_Incentive_Tier number of months after the qualification employment start date must have elapsed before a payment eligibility tier can be added.

ldtm_employment_start_date = idw_dw[1].GetitemDatetime(idw_dw[1].getrow(),'employment_start_date')
ll_rtw_incentive_no = idw_dw[1].GetitemNumber(idw_dw[1].getrow(),'rtw_incentive_no')

//see if the same tier has been entered twice under the same qualification
// 
//Select Count(*)
//into    :ll_count_tiers
//From  RTW_INCENTIVE_PAYMENT_ELIGIBILITY
//Where claim_no = :ll_claim_no
//And     rtw_incentive_no = :ll_rtw_incentive_no
//And     tier_no = :ll_tier_no
//And     payment_eligibility_flag <> :ls_eligibility_flag
//Using SQLCA;
//
//SQLCA.nf_handle_error("n_return_to_work","nf_check_bus_rule_eligibility","select from Rtw_Incentive_Tier")
//
//IF ll_count_tiers > 0 THEN
//	Messagebox("Error","The same tier no can not be entered twice under the same qualification.",Exclamation!)
//	idw_dw[2].SetColumn('tier_no')
//	idw_dw[2].SetFocus()
//	Return -1
//END IF	


Select  DATEADD(MONTH,MAX(a.months_from_employment_start),:ldtm_employment_start_date)
Into    :ldt_after_date
From   Rtw_Incentive_Tier AS a		
Where  a.tier_no = :ll_tier_no
Using   SQLCA;
		
SQLCA.nf_handle_error("n_return_to_work","nf_check_bus_rule_eligibility","select from Rtw_Incentive_Tier")

IF ldt_current_date < ldt_after_date THEN
		Messagebox("Error","This tier is not yet eligible, the tier number of months has not passed.",Exclamation!)
		idw_dw[2].SetColumn('tier_no')
		idw_dw[2].SetFocus()
		Return -1
END IF

// A tier can be added once per qualification if not eligible under a previous qualification.

Select  count(*)
Into     :ll_count
From   RTW_INCENTIVE_PAYMENT_ELIGIBILITY
Where claim_no = :ll_claim_no
And     tier_no = :ll_tier_no
And     payment_eligibility_flag = 'Y'
And    avg_monthly_employment_income_amt = :ldc_tier_income
Using  SQLCA;

SQLCA.nf_handle_error("n_return_to_work","nf_check_bus_rule_eligibility","select from RTW_INCENTIVE_PAYMENT_ELIGIBILITY")

IF ll_count > 0 THEN
		Messagebox("Error","This tier already exists and is eligible for payment.",Exclamation!)
		idw_dw[2].SetColumn('tier_no')
		idw_dw[2].SetFocus()
		Return -1
END IF	

// The max effective date of the tier is less than or equal to the qualification employment start date

ll_row = idw_dw[1].getrow()
ldtm_employment_start_date = idw_dw[1].GetItemDateTime(ll_row,'employment_start_date')

Select effective_date
Into    :ldtm_effective_date
From   Rtw_Incentive_Tier
Where tier_no = :ll_tier_no
Using   SQLCA;

SQLCA.nf_handle_error("n_return_to_work","nf_check_bus_rule_eligibility","select from RTW_INCENTIVE_PAYMENT_ELIGIBILITY")

IF ldtm_effective_date > ldtm_employment_start_date THEN
		Messagebox("Error","The tier no selected is not valid. The qualification employment start date is prior to the effective date of this tier.",Exclamation!)
		idw_dw[2].SetColumn('tier_no')
		idw_dw[2].SetFocus()
		Return -1
END IF	

IF ldc_tier_income <= 0  OR Isnull(ldc_tier_income) THEN
	MessageBox("Error","The Average Employment Income must be more than $0.00.",Exclamation!)
	idw_dw[2].SetColumn('avg_monthly_employment_income_amt')
	idw_dw[2].SetFocus()
	Return -1
END IF

// Check that the qualification employment start date and tier number of months fit properly between any existing qualification employment start dates
// and tiers that may be currently entered.

ll_rtn = nf_check_employment_date(ll_claim_no,ll_rtw_incentive_no,ll_tier_no)

IF ll_rtn < 0 THEN
	Return -1
END IF	


Return 0
end function

public function integer nf_check_employment_date (long al_claim_no, long al_incentive_no, long al_tier_no);DateTime ldtm_employment_start_date, ldtm_combined_date, ldtm_existing_start_date, ldtm_end_date
Long ll_rowcount, x


ids_employment_date = CREATE DATASTORE
ids_employment_date.DataObject = 'd_rtw_end_date'
ids_employment_date.SetTransObject(SQLCA)



Select a.employment_start_date, DATEADD(MONTH,c.months_from_employment_start, a.employment_start_date)
Into    :ldtm_employment_start_date, :ldtm_combined_date
From  RTW_INCENTIVE_QUALIFICATION a, Rtw_Incentive_Tier c
Where a.claim_no = :al_claim_no
And   a.rtw_incentive_no = :al_incentive_no
And   a.rtw_incentive_type_code = 'JS'
And   a.qualification_flag = 'Y'
And   c.tier_no  = :al_tier_no
And   c.active_flag = 'Y'
Using Sqlca;

SQLCA.nf_handle_error("n_return_to_work","nf_check_employment_date","")

IF IsNull(ldtm_combined_date) THEN
	Return 1 //There isn't a tier entered
END IF	

ids_employment_date.Retrieve(al_claim_no)
SQLCA.nf_handle_error("n_return_to_work","nf_check_employment_date","")

ll_rowcount = ids_employment_date.RowCount()
IF ll_rowcount = 0 THEN 
	Return 0 // There aren't any tiers entered don't need to compare
END IF	

For x = 1 To ll_rowcount
	ldtm_existing_start_date = ids_employment_date.getitemdatetime(x,"employment_start_date")
	ldtm_end_date =  ids_employment_date.getitemdatetime(x,"end_date")
	
	IF NOT IsNull(ldtm_end_date) THEN
		IF ldtm_employment_start_date < ldtm_existing_start_date AND  (ldtm_combined_date > ldtm_end_date  OR ldtm_combined_date > ldtm_existing_start_date) THEN
			Messagebox("ERROR","The combination of qualification and tier months has overlapped the existing qualification and tiers.",Exclamation!)
			Return -1 
		END IF
	ELSE
		IF ldtm_employment_start_date < ldtm_existing_start_date  AND ldtm_combined_date > ldtm_existing_start_date THEN
			Messagebox("ERROR","The combination of qualification and tier months has overlapped the existing qualification's employment start date.",Exclamation!)
			Return -1 
		END IF
	END IF
	
NEXT	

Return 0


end function

public function integer nf_check_qualification_start (long al_claim_no, datetime adtm_employment_start_date, long al_incentive_no);DateTime ldtm_modified_employment_start_date, ldtm_existing_start_date, ldtm_end_date, ldtm_employment_start_date, ldt_after_date
Long        ll_rowcount, x, ll_incentive_no


ll_rowcount = idw_dw[3].RowCount()

SQLCA.nf_handle_error("n_return_to_work","nf_check_qualifiction_start","select from RTW_INCENTIVE_QUALIFICATION")

IF ll_rowcount = 1 THEN 
	Return 0 // There aren't any other qualifications don't compare
END IF	

FOR x = 1 TO ll_rowcount
	ll_incentive_no =  idw_dw[3].Getitemnumber(x,"rtw_incentive_no")
	ldtm_employment_start_date = idw_dw[3].GetitemDatetime(x,"employment_start_date")
	
	
	IF ll_incentive_no <  al_incentive_no THEN 
		//If the incentive is before the current incentive no then the employment start date must be before the current employment start date
		
		Select  DATEADD(MONTH,MAX(a.months_from_employment_start),:ldtm_employment_start_date)
		Into    :ldt_after_date
		From   Rtw_Incentive_Tier AS a
		Join     RTW_INCENTIVE_PAYMENT_ELIGIBILITY AS b
		On       a.tier_no = b.tier_no
		And     b.claim_no = :al_claim_no
		And     b.rtw_incentive_no = :ll_incentive_no
		Using   SQLCA;
		
		SQLCA.nf_handle_error("n_return_to_work","nf_check_qualifiction_start","select from RTW_INCENTIVE_PAYMENT_ELIGIBILITY")

		IF IsNull(ldt_after_date) THEN   // There isn't a payment eligibility use the qualification start date to compare.
			ldt_after_date = ldtm_employment_start_date
		END IF	
	
		IF adtm_employment_start_date < ldt_after_date THEN
			Messagebox("Error","The Employment Start Date must be after the employment start date of qualification no "+String(ll_incentive_no)+".",Exclamation!)
			idw_dw[1].SetColumn('employment_start_date')
			idw_dw[1].SetFocus()
			Return -1
		END IF
	END IF
	
	IF ll_incentive_no >  al_incentive_no THEN 
		IF adtm_employment_start_date >= ldtm_employment_start_date THEN
			Messagebox("Error","The Employment Start Date must be before the employment start date of qualification no "+String(ll_incentive_no)+".",Exclamation!)
			idw_dw[1].SetColumn('employment_start_date')
			idw_dw[1].SetFocus()
			Return -1
		END IF
	END IF		
		
NEXT
	

Return 0


end function

on n_return_to_work.create
call super::create
end on

on n_return_to_work.destroy
call super::destroy
end on

