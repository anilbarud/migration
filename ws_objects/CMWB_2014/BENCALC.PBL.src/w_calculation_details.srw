$PBExportHeader$w_calculation_details.srw
forward
global type w_calculation_details from w_a_tool
end type
type gb_cppd_deductions from groupbox within w_calculation_details
end type
type cb_list_bencalcs from commandbutton within w_calculation_details
end type
type cb_save from commandbutton within w_calculation_details
end type
type cb_cancel from commandbutton within w_calculation_details
end type
type cb_delete from commandbutton within w_calculation_details
end type
type st_window_title from statictext within w_calculation_details
end type
type dw_benefit_calculation_details from u_dw_online within w_calculation_details
end type
type cb_next from commandbutton within w_calculation_details
end type
type cb_prev from commandbutton within w_calculation_details
end type
type st_taxation_period from statictext within w_calculation_details
end type
type st_frozen_92_taxyear from statictext within w_calculation_details
end type
type cb_add_earnings from commandbutton within w_calculation_details
end type
type dw_benefit_calculation_capable_data from u_dw_online within w_calculation_details
end type
type dw_benefit_calculation_display_computed from u_dw_online within w_calculation_details
end type
type dw_benefit_calculation_deductions from u_dw_online within w_calculation_details
end type
type dw_benefit_calculation_preaccident_data from u_dw_online within w_calculation_details
end type
type dw_benefit_calculation_remunerations from u_dw_online within w_calculation_details
end type
type cb_delete_remuneration from commandbutton within w_calculation_details
end type
type gb_regular_deductions from groupbox within w_calculation_details
end type
type dw_benefit_calculation_frozen_data from u_dw_online within w_calculation_details
end type
type uo_tabs from u_top_bar within w_calculation_details
end type
type dw_benefit_calculation_cppd from u_dw_online within w_calculation_details
end type
end forward

global type w_calculation_details from w_a_tool
integer width = 2757
integer height = 1920
boolean resizable = false
gb_cppd_deductions gb_cppd_deductions
cb_list_bencalcs cb_list_bencalcs
cb_save cb_save
cb_cancel cb_cancel
cb_delete cb_delete
st_window_title st_window_title
dw_benefit_calculation_details dw_benefit_calculation_details
cb_next cb_next
cb_prev cb_prev
st_taxation_period st_taxation_period
st_frozen_92_taxyear st_frozen_92_taxyear
cb_add_earnings cb_add_earnings
dw_benefit_calculation_capable_data dw_benefit_calculation_capable_data
dw_benefit_calculation_display_computed dw_benefit_calculation_display_computed
dw_benefit_calculation_deductions dw_benefit_calculation_deductions
dw_benefit_calculation_preaccident_data dw_benefit_calculation_preaccident_data
dw_benefit_calculation_remunerations dw_benefit_calculation_remunerations
cb_delete_remuneration cb_delete_remuneration
gb_regular_deductions gb_regular_deductions
dw_benefit_calculation_frozen_data dw_benefit_calculation_frozen_data
uo_tabs uo_tabs
dw_benefit_calculation_cppd dw_benefit_calculation_cppd
end type
global w_calculation_details w_calculation_details

type variables
//
// Variables required by this module
//

Date			idt_server_date, idt_frozen_92_taxyear	
DateTime		              idtm_accident_recurrence_date
Long			il_opening_no, &
			il_claim_no, &
			il_benefit_calculation_no
n_benefit_calculation	in_bencalcs
s_window_message 	istr_bencalc_parameters

Integer		 ii_deduction_row
Integer		 ii_remuneration_row
Integer      ii_copy_tax_index_year
Integer      ii_release, ii_old_release

decimal {2}  idec_copy_td1_exemption_amount
decimal {2}  idec_copy_prov_td1_exemption

boolean      ib_copy_ben_calc, ib_new_ben_calc
boolean      ib_detail
boolean      ib_copy_first
BOOLEAN		 ib_suppress_exemption_msg = TRUE

string       is_frozen_exists
U_DS			ids_rtw_incentive_linked_bencalcs

end variables

forward prototypes
public subroutine wf_calculate_award ()
public subroutine wf_recalculate_all ()
public function integer wf_read_only ()
public subroutine wf_determine_tax_index_year (date adt_accident_recurrence_date, date adt_effective_date, ref integer ai_tax_index_year)
public function integer wf_compute_earnings (string as_earnings_column)
public function string wf_exemption_mismatch (decimal adec_fed_exemption_entered, decimal adec_fed_basic_exemption, decimal adec_fed_married_exemption, decimal adec_prov_exemption_entered, decimal adec_prov_basic_exemption, decimal adec_prov_married_exemption, date adt_index_period)
public subroutine wf_set_rtw_deduction (long al_row)
public subroutine wf_set_rtw_remuneration (long al_row)
public function integer wf_check_benefit_level (decimal adec_benefit_level_percentage, date adt_effective_from_date, string as_transitional_claim_flag, decimal adec_regular_award_amount, decimal adec_transitional_award_amount)
public subroutine wf_set_effective_date (ref dwobject a_dwo)
protected function integer wf_verify_copied_ltd_bencalc ()
public function long wf_check_rtw_bus_rules ()
public function integer wf_85_pct_transitional_passed (decimal adec_benefit_level_percentage, decimal adec_regular_award_amount, decimal adec_transitional_award_amount)
private function string wf_verify_copied_frozen_data (ref datastore ads_benefit_calculation_frozen_data)
private function string wf_verify_copied_ltd_bencalc_parameters (ref datastore ads_benefit_calculation_details)
private function string wf_verify_copied_preaccident_data (ref datastore ads_benefit_calculation_preaccident_data)
private function string wf_verify_cppd_deduction (ref datastore ads_benefit_calculation_cppd)
private function string wf_verify_regular_deductions (ref datastore ads_benefit_calculation_deductions, integer ai_deduction_count)
public subroutine wf_set_rtw_capable ()
end prototypes

public subroutine wf_calculate_award ();/* ---------------------------------------------------------------------------------------------------- */
/* Function Name: wf_calculate_award                                                                    */
/*                                                                                                      */
/* Purpose:       The purpose of this module is to calculate the benefit award                          */
/*                                                                                                      */
/* Arguments:                                                                                           */
/*                                                                                                      */
/* Return Values:                                                                                       */
/* ---------------------------------------------------------------------------------------------------- */

DECIMAL	ldec_loe_100_percent,				ldec_loe_reduced,				ldec_combined_earnings,		ldec_preacc_earnings_reduced,		&
			ldec_excess_amount,				ldec_cppd_deduct_amount,		ldec_award_amount,				ldec_preacc_gross_pay,				&
			ldec_frozen_net_pay,				ldec_transitional_amt,		ldec_loe_90_percent,			ldec_trans_cppd_deduct_amount,		&
			ldec_trans_total_deductions,	ldec_trans_award_amount,		ldec_avg_net_pay,				ldec_capable_net_pay,					&
			ldec_benefit_level_percentage,	ldec_preacc_net_pay,			ldec_remun_net_pay,														&
			ldec_reduction_factor,			ldec_total_deductions,		ldec_regular_award_amount,			&
			ldec_cppd_net_amt,					ldec_sum_deductions, ldec_display_regular_award_amount

STRING	ls_award_freq_code, ls_transitional_claim_flag,	ls_top_up_flag, ls_pre_93

DATETIME	ldt_effective_date


/* For project 10105 Ben Calc - pre93 the following is true
   The excess amount field is not deducted for transitional claims in which the regular
	calculation of earnings is at 90% (i.e. having a transitional claim flag of "inapplicable"
	based on a benefit effective date prior to January 01, 1993)
	
	For example
	Claim Accident/Excess amount--Benefit Effective--Transitional Claim--Frozen Earnings--Regular
	recurrence date deducted    --Date             --Flag              --Entered        --Calc%
	___________________________________________________________________________________________
	1989/07/01							1991/09/12			 Inapplicable		   NO					  90%
	NO
	___________________________________________________________________________________________
	1989/07/01							1995/03/26			 Yes Or No			   Yes				  85%
	Yes(from 85%)
	___________________________________________________________________________________________
	1993/04/16							1994/11/14			 Inapplicable        NO					  85%
	Yes
	
	"when doing a calc with an effective date from jan 1/82 to dec 31/92. The amount entered in the 
	 renumeration tab should not be used in the calculation
	 
	 The pre-1993 should be done exactly like they are done on the right hand side of the calculation tab.
	 
	 LOE 90% which is the pre-accident indexed net earnings (subject to max) minus the capable net
	 earnings multiply by 90%
	 
	 No combined earnings nor excess amount should be used in the calculations
	 
	 If you use the left side of the calculation tab, only the following field should be calc.
	 
	 100% LOE        = net indexed average earnings - net capable earnings
	 90% LOE         = 90% of 100% LOE
	 CPPD amount     = 90% LOE/net indexed average earnings * CPPD amount (see below***)
	 other deduction = $entered
	 Total deduction = CPPD amount + other deductions
	 Award amount    = $90% LOE - Total Deduction.
	 
	 *** Lanteigne, Gisele : prior to Jan 1/90, the CPPD is fully deducted... Now after talking to you, 
	 I thought about the real effective date for the cppd partial deduction. I didn't remember if they 
	 were all recalculated effective Jan 1/90 or if it was at the indexation date I looked it up in 
	 Adjudication Manual #10-37-06 & it's at the effective date Jan 1/90
*/

//
//	Get the fields we're going to need from the datawindows
//


	ldec_benefit_level_percentage	=	dw_benefit_calculation_details.GetItemDecimal(1,"benefit_level_percentage")
	ls_award_freq_code 				=	dw_benefit_calculation_details.GetItemString(1,"award_freq_code")

	If dw_benefit_calculation_deductions.GetRow() > 0 Then
		ldec_total_deductions = dw_benefit_calculation_deductions.GetItemDecimal(1,"sum_deduction_freq_pay")
		If IsNull(ldec_total_deductions) then ldec_total_deductions = 0
	Else
		ldec_total_deductions = 0
	End If
	
   ldt_effective_date            =  dw_benefit_calculation_details.getitemdatetime(1,"effective_from_date")
	ldec_avg_net_pay					=	dw_benefit_calculation_preaccident_data.GetItemDecimal(1,"avg_net_pay")
	ldec_preacc_net_pay 				=	dw_benefit_calculation_preaccident_data.GetItemDecimal(1,"preacc_net_pay")
	ls_top_up_flag						=	dw_benefit_calculation_details.GetItemString(1,"top_up_flag")
	ldec_cppd_net_amt					=	dw_benefit_calculation_cppd.GetItemDecimal(1,"cppd_indexed_monthly_amount")
	ls_transitional_claim_flag		=	dw_benefit_calculation_details.GetItemString(1,"transitional_claim_flag")
	
	/* check to see if it is a pre 93 ben calc
	*/
	
	IF DATE(ldt_effective_date) < Date("1993/01/01") THEN
		ls_pre_93 = "Y"
	ELSE
		ls_pre_93 = "N"
	END IF

	If dw_benefit_calculation_capable_data.GetRow() > 0 Then
		ldec_capable_net_pay = dw_benefit_calculation_capable_data.GetItemDecimal(1,"pre93_capable_net_pay")
		If IsNull(ldec_capable_net_pay) Then ldec_capable_net_pay = 0
	Else
		ldec_capable_net_pay = 0
	End If

	If dw_benefit_calculation_remunerations.GetRow() > 0 Then
		ldec_remun_net_pay = dw_benefit_calculation_remunerations.GetItemDecimal(1,"compute_net_remunerations")
		If IsNull(ldec_remun_net_pay) Then ldec_remun_net_pay = 0
	Else
		ldec_remun_net_pay = 0
	End If


	If dw_benefit_calculation_frozen_data.GetRow() > 0 Then
		ldec_frozen_net_pay = dw_benefit_calculation_frozen_data.GetItemDecimal(1,"frozen_net_pay")
		If IsNull(ldec_frozen_net_pay) Then ldec_frozen_net_pay = 0
	Else
		ldec_frozen_net_pay = 0
	End If


//	Check to see if the claim is a transitional claim.  If so, we must force the regular calculation to 
//	be at 85% regardless of the current benefit percentage.

	If ls_transitional_claim_flag = "Y" Then	
		ldec_benefit_level_percentage = .85
	End If

//	First, Calculate Regular Benefits


		If ldec_capable_net_pay >= ldec_avg_net_pay Then
			ldec_loe_100_percent	=	0
			ldec_loe_reduced		=	0
		Else
			ldec_loe_100_percent	=	ldec_avg_net_pay - ldec_capable_net_pay											//	Calculate 100 percent loe
			ldec_loe_reduced		=	Round(ldec_benefit_level_percentage * ldec_loe_100_percent,2)				//	Calculate reduced loe based the percentage
		End IF
		
		IF ls_pre_93 = "Y" THEN
			ldec_combined_earnings = 0
		ELSE
			ldec_combined_earnings = ldec_loe_reduced + ldec_remun_net_pay
		END IF


		ldec_preacc_earnings_reduced = Round(ldec_benefit_level_percentage * ldec_preacc_net_pay, 2)			//	Calculate reduced pre-accident net earnings
															
		ldec_excess_amount = 0
		If ldec_preacc_earnings_reduced < ldec_combined_earnings Then
			If ls_top_up_flag = "Y" Then
				ldec_excess_amount = 0
				dw_benefit_calculation_display_computed.Modify("st_top_up_provision.Text = '(TopUp Prov)'")
			Else
				/* The excess amount displayed on the Calculation tab - could you verify if the following rules are
				   correct: Excess amount is not calculated when:
					(1) - The benefit level percentage is 90% and the benefit effective date is < 1993/01/01
					OR
					(2) - The Top - up provision is in effect (this  rule was always there)
				*/
			
				IF ldec_benefit_level_percentage = .90 AND DATE(ldt_effective_date) < Date("1993/01/01") THEN
					ldec_excess_amount = 0
				ELSE
					ldec_excess_amount = ldec_combined_earnings - ldec_preacc_earnings_reduced							//	Calculate Excess Amount
					dw_benefit_calculation_display_computed.Modify("st_top_up_provision.Text = ''")
				END IF
			End If
		Else
			ldec_excess_amount = 0
			dw_benefit_calculation_display_computed.Modify("st_top_up_provision.Text = ''")
		End If

		ldec_cppd_net_amt = f_convert_frequency("M",ldec_cppd_net_amt,ls_award_freq_code)

// * Note - Prior to 1990/01/01 the CPPD Deduction should be the full amount.
		If ldec_avg_net_pay > 0 then
			IF DATE(ldt_effective_date) < DATE("1990/01/01") THEN
				ldec_reduction_factor = 1 
			ELSE	
				ldec_reduction_factor = ldec_loe_100_percent / ldec_avg_net_pay
			END IF
			ldec_cppd_deduct_amount = Round(ldec_reduction_factor * ldec_cppd_net_amt,2)								//	Calculate CPP Disability deduction
		End If


		ldec_sum_deductions = ldec_total_deductions + ldec_excess_amount + ldec_cppd_deduct_amount				// Calculate sum of all deductions


		ldec_regular_award_amount = 0																							//	Calculate regular benefit payable
		If ldec_loe_reduced > ldec_total_deductions Then 
			ldec_regular_award_amount = ldec_loe_reduced - ldec_sum_deductions
		End IF
			



//	Now, if the claim is a transitional claim, calculate transitional claim benefit award

	If ls_transitional_claim_flag = "Y" Then

			If ldec_frozen_net_pay > ldec_capable_net_pay Then
				ldec_transitional_amt = ldec_frozen_net_pay - ldec_capable_net_pay
			Else
				ldec_transitional_amt = 0
			End If

			ldec_loe_90_percent = Round(ldec_transitional_amt * .90		,2)

			If ldec_frozen_net_pay > 0 then
				ldec_reduction_factor = ldec_transitional_amt / ldec_frozen_net_pay
				ldec_trans_cppd_deduct_amount = Round(ldec_reduction_factor * ldec_cppd_net_amt,2)
			Else
				ldec_trans_cppd_deduct_amount = 0
			End If

			ldec_trans_total_deductions = ldec_total_deductions + ldec_trans_cppd_deduct_amount

			If ldec_trans_total_deductions < ldec_loe_90_percent Then
				ldec_trans_award_amount = ldec_loe_90_percent - ldec_trans_total_deductions
			Else			
				ldec_trans_award_amount = 0
			End If

	End IF

			
	If dw_benefit_calculation_details.GetItemString(1,"calculation_type_code") = "A" Then
		If ls_transitional_claim_flag = "Y" and ldec_trans_award_amount > ldec_regular_award_amount Then
			If ldec_trans_award_amount < 0 Then
				dw_benefit_calculation_details.SetItem(1,"award_amount",0)
			Else
				dw_benefit_calculation_details.SetItem(1,"award_amount",ldec_trans_award_amount)
			End IF
		Else
			If ldec_regular_award_amount < 0 Then
				dw_benefit_calculation_details.SetItem(1,"award_amount",0)
			Else
				dw_benefit_calculation_details.SetItem(1,"award_amount",ldec_regular_award_amount)
			End IF
		End If
	End If

	If ls_transitional_claim_flag = "Y" and ldec_trans_award_amount > ldec_regular_award_amount Then
		dw_benefit_calculation_details.SetItem(1,"cppd_net_amount",ldec_trans_cppd_deduct_amount)
	Else
		dw_benefit_calculation_details.SetItem(1,"cppd_net_amount",ldec_cppd_deduct_amount)
	End If


	//	Display fields for regular calculation

		dw_benefit_calculation_display_computed.SetItem(1,"loe_100_percent",ldec_loe_100_percent)
		dw_benefit_calculation_display_computed.SetItem(1,"loe_reduced",ldec_loe_reduced)
		dw_benefit_calculation_display_computed.SetItem(1,"combined_earnings",ldec_combined_earnings)
		dw_benefit_calculation_display_computed.SetItem(1,"loe_percentage",ldec_benefit_level_percentage)
		dw_benefit_calculation_display_computed.SetItem(1,"preacc_earnings_reduced",ldec_preacc_earnings_reduced)
		dw_benefit_calculation_display_computed.SetItem(1,"excess_amount",ldec_excess_amount)
		dw_benefit_calculation_display_computed.SetItem(1,"cppd_regular_deduct_amount",ldec_cppd_deduct_amount)
		dw_benefit_calculation_display_computed.SetItem(1,"other_deduction_amount",ldec_total_deductions)
		ldec_display_regular_award_amount = ldec_loe_reduced - ldec_sum_deductions
		dw_benefit_calculation_display_computed.SetItem(1,"regular_award_amount",ldec_display_regular_award_amount)

	//	Display fields for transitional calculation

		dw_benefit_calculation_display_computed.SetItem(1,"loe_90_percent",ldec_loe_90_percent)
		dw_benefit_calculation_display_computed.SetItem(1,"transitional_award_amount",ldec_trans_award_amount)
		dw_benefit_calculation_display_computed.SetItem(1,"cppd_trans_deduct_amount",ldec_trans_cppd_deduct_amount)
end subroutine

public subroutine wf_recalculate_all ();
	If wf_compute_earnings("PreAccident") = -1 Then return
	
	If wf_compute_earnings("Average") = -1 Then Return
	
	If dw_benefit_calculation_frozen_data.RowCount() > 0 Then
		If wf_compute_earnings("Frozen") = -1 Then Return	
	End If

	If dw_benefit_calculation_capable_data.RowCount() > 0 Then
		If wf_compute_earnings("Capable") = -1 Then return
	End IF

	If dw_benefit_calculation_remunerations.RowCount() > 0 Then
		If wf_compute_earnings("Remunerations") = -1 Then return
	End IF

	If dw_benefit_calculation_deductions.RowCount() > 0 Then
		If wf_compute_earnings("Deductions") = -1 Then return
	End If

	If wf_compute_earnings("CPPD") = -1 Then return

	wf_calculate_award()

end subroutine

public function integer wf_read_only ();/* Function to Display the Calculation Details screen in Read-Only mode
*/
	

/* Disable the DELETE button and the SAVE button
*/

cb_delete.enabled = FALSE
cb_save.enabled = FALSE

return 0
 


end function

public subroutine wf_determine_tax_index_year (date adt_accident_recurrence_date, date adt_effective_date, ref integer ai_tax_index_year);/* ---------------------------------------------------------------------------------------------------- */
/* Function Name: wf_determine_tax_index_year                                                           */
/*                                                                                                      */
/* Purpose:       The purpose of this function is to calculate the dollar year for the effective date.  */
/*                If the effective date month is on or after the accident date/ injury recurrence month,*/
/*                then the index year is the effective date year.  Otherwise, the index year is the     */
/*                effective date year - 1.                                                              */
/*                                                                                                      */
/* Arguments:     Parameters passed by calling routine:                                                 */
/*                                                                                                      */
/*                vad_accident_recurrence_date 	- The date of recurrence of injury                      */
/*						vad_effective_date      - The effective date                                          */
/*                                                                                                      */
/*                                                                                                      */
/*   Returns:     vai_tax_index_year      - The annual tax year                                         */
/*                                                                                                      */
/* ---------------------------------------------------------------------------------------------------- */


//	Variable Declarations

	INTEGER	li_comp_inj_month


//	Calculate compensation injury month

	li_comp_inj_month = month(adt_accident_recurrence_date)


// If the effective date month is less than the accident/recurrence date month, then
//	Set the tax index year to the previous year

/* code to check the year added for project 10105 - up until 1990 the If the 
   effective date month is less than the accident/recurrence date month, then
	we DO NOT set the year -1 rather we calculate based on January 1
*/


	If month(adt_effective_date) >= li_comp_inj_month OR YEAR(adt_effective_date) < 1990 then
		ai_tax_index_year = year(adt_effective_date)
	Else
		ai_tax_index_year = year(adt_effective_date) - 1
	End If



	return
end subroutine

public function integer wf_compute_earnings (string as_earnings_column); 
/* ---------------------------------------------------------------------------------------------------- */
/* Function Name: wf_compute_earnings                                                                   */
/*                                                                                                      */
/* Purpose:       The purpose of this module is to compute the values for the earnings column and       */
/*						populate the column                                                                   */
/*                                                                                                      */
/* Arguments:     none                                                                                  */
/*                                                                                                      */
/* Return Values: 1 - success                                                                           */
/*                -1  failure  (database error)                                                         */
/* ---------------------------------------------------------------------------------------------------- */

/* *******NOTE*****************
   Project P10105 has created an addition to this code to allow for the automatic calculations
	of pre 93 bencalcs - this will be kept seperate in the business rule n_calculation_service
	once we have determined which index period it is we will set the boolean lb_pre_93 to true
	which will determine which business rule object to use
	This object includes all of the different cases in which the calculations can be based
	All calculations will be called from THIS/Windows Source code
*/

LONG			ll_rownum,						ll_index_taxation_year,		ll_gross_pay_base_year,	&
				ll_counter
						

DATE			ld_effective_from_date,		ld_index_date

DATETIME		ldt_create_datetime

DECIMAL		ldec_td1_exemption_amount,		ldec_gross_pay,						ldec_annual_gross,&
				ldec_max_avg_earnings,			ldec_annual_tax,						ldec_annual_uic,&
				ldec_annual_cpp,					ldec_net_pay,							ldec_annual_net,&
				ldec_avg_indexed_gross_pay,	ldec_preacc_indexed_gross_pay,	ldec_cppd_indexed_gross_pay,&
				ldec_total_annual_gross,		ldec_uic_insurable_earnings,		ldec_cpp_insurable_earnings,&
				ldec_taxable_earnings,        ldec_basic_exemption,				ldec_details_froz_annual_gross

STRING		ls_award_freq_code,				ls_earn_freq_code,					ls_paid_uic_flag,&
				ls_paid_cpp_flag,					ls_paid_tax_flag						

BOOLEAN		lb_pre_93
DECIMAL		ld_weekly_net
STRING      ls_message
INTEGER     li_error, li_claim_no
DATE        ldt_yearly_factor_date, ldt_frozen_92_taxyear
DECIMAL		ldec_prov_td1_exemption_amt, ldec_fed_td1_exemption_amt // Added for SR 64 - Jan. 2001 Tax Changes

// PR 2806 -kdm
DECIMAL ldec_td1_basic_exemption , ldec_td1_married_exemption , ldec_prov_td1_basic_exemption , ldec_prov_td1_married_exemption
STRING ls_exemption_mismatch

/*	Get the values we need from dw_benefit_calculation_details
*/
	ll_rownum							=	dw_benefit_calculation_details.GetRow()
	ldt_frozen_92_taxyear			=	idt_frozen_92_taxyear
	ld_effective_from_date			=	Date(dw_benefit_calculation_details.GetItemDateTime(ll_rownum,"effective_from_date"))
	ll_index_taxation_year			=	dw_benefit_calculation_details.GetItemNumber(ll_rownum,"index_taxation_year")
	ls_award_freq_code				=	dw_benefit_calculation_details.GetItemString(ll_rownum,"award_freq_code")
   ldt_yearly_factor_date        =  date(dw_benefit_calculation_details.GetItemdatetime(ll_rownum,"yearly_factor_date"))
/* SR 64 - Jan. 2001 tax changes - Formerly just had to track federal TD1 exemption. Now must track both federal &
	provincial TD1 deductions.*/
	ldec_prov_td1_exemption_amt	=	dw_benefit_calculation_details.GetItemDecimal(ll_rownum,"prov_td1_exemption_amount")
	ldec_fed_td1_exemption_amt		=	dw_benefit_calculation_details.GetItemDecimal(ll_rownum,"td1_exemption_amount")

/*	If the index year is 1997, get the ben calc create date to determine which version of 1997 values to use
	when calculating the 1997 CPP and provincial taxes. This is because the rates changed during 1997, but we don't 
	save the UIC, CPP and taxes, so we have to recalculate and need to recalculate with the original values to
	get the awards to match.
*/
	IF ll_index_taxation_year = 1997 THEN
		ldt_create_datetime = dw_benefit_calculation_details.GetItemDateTime(ll_rownum,"create_date")
	END IF

/* Check to see if ldt_effective_from_date < min effective date in the yearly factor table
   if it is then we do not want to continue with this.
*/
   IF in_bencalcs.f_check_effective_from_date(ld_effective_from_date) = FALSE THEN
   	Return 1
	END IF

/*	Initialize
*/
	ldec_annual_cpp = 0
	ldec_annual_uic = 0
	ldec_annual_tax = 0

// Ed Lenarczyk  - Feb 04, 2000   ***   BEGIN  CHANGES   ***
//                 Changes to make sure the latest information for the taxation year is used

	If ll_index_taxation_year = year(ld_effective_from_date) THEN
		ld_index_date = Date(year(ld_effective_from_date),month(ld_effective_from_date),day(ld_effective_from_date))
	ELSE
		ld_index_date = Date(ll_index_taxation_year,12,31)
	End If
// Ed Lenarczyk  - Feb 04, 2000  ***  END  CHANGES  ***

/*	Recalculate the appropriate earnings column
*/

/* For project P10105 we will now set the boolean to decide wether to use the new business rule object
   or the existing business rule object - must the index date and work variables
*/
	IF ld_index_date < date("1993/01/01") THEN
		lb_pre_93 = TRUE
	ELSE
		lb_pre_93 = FALSE
	END IF
	
/* If a year factor date is present (not null) then we will want to use that instead of any calculation
   based on the index_date we should be able to simply substitute
*/
	IF NOT ISNULL(ldt_yearly_factor_date) THEN
		ld_index_date = ldt_yearly_factor_date
	END IF
	
/* Check that we have a valid year
*/
	IF not in_bencalcs.f_isvalid_tax_year(ld_index_date) THEN
   	Return 1
	END IF
	
	in_bencalcs.f_set_index_date(ld_index_date)
	IF ib_detail = TRUE THEN
    	/* Retrieve values from the BENEFIT_CALCULATION TABLE */
	 	in_bencalcs.f_populate_bencalc_vars(il_claim_no,il_benefit_calculation_no,il_opening_no)
	ELSE
		in_bencalcs.f_populate_work_variables(ld_effective_from_date,'')
	END IF	
	
	/* P10151-55 - J. Hawker, 2006.06.14 - The release_no column has been added to the Yearly_Factor  
               table to keep track of any changes that are made to the tax information. 
					Moved this code from the top of the function so the variables are set before 
					any data is retrieved.
	*/
	SELECT td1_basic_exemption , td1_married_exemption , prov_td1_basic_exemption , prov_td1_married_exemption
	INTO :ldec_td1_basic_exemption , :ldec_td1_married_exemption , :ldec_prov_td1_basic_exemption , :ldec_prov_td1_married_exemption
	FROM Yearly_Factor
	WHERE yearly_factor_date = :ldt_yearly_factor_date
	AND   release_no         = :in_bencalcs.ii_release
	USING SQLCA;
	
	IF SQLCA.nf_handle_error('SELECT td1_basic_exemption...','w_calculation_details','wf_compute_earnings') < 0 THEN
		Return -1
	END IF
	
	in_bencalcs.f_set_td1_amount(ldec_fed_td1_exemption_amt)
	in_bencalcs.f_set_prov_td1 (ldec_prov_td1_exemption_amt)
	dw_benefit_calculation_details.setitem(1,"yearly_factor_date",in_bencalcs.idt_from_yearly_factor)
	in_bencalcs.ii_assessment_year = ll_index_taxation_year
//code added END P10105

	Choose case as_earnings_column

	Case "PreAccident"


		ll_rownum					=	dw_benefit_calculation_preaccident_data.GetRow()
		ls_earn_freq_code		   =	dw_benefit_calculation_preaccident_data.GetItemString(ll_rownum,"preacc_earn_freq_code")
		ldec_gross_pay				=	dw_benefit_calculation_preaccident_data.GetItemDecimal(ll_rownum,"preacc_gross_pay")
		ls_paid_uic_flag			=	dw_benefit_calculation_preaccident_data.GetItemString(ll_rownum,"preacc_paid_uic_flag")
		ls_paid_cpp_flag			=	dw_benefit_calculation_preaccident_data.GetItemString(ll_rownum,"preacc_paid_cpp_flag")
		ll_gross_pay_base_year	=	dw_benefit_calculation_preaccident_data.GetItemNumber(ll_rownum,"preacc_gross_pay_base_year")


		/* If displaying the details the preacc_indexed_gross_pay should be retrieved from the BENEFIT CALCULATION TABLE
		   PR1890 */
		IF ib_detail = TRUE THEN 
			ldec_preacc_indexed_gross_pay = dw_benefit_calculation_preaccident_data.GetItemNumber(ll_rownum,"preacc_indexed_gross_pay")
		ELSE
		/*	Index the amount ahead
		*/
			If ldec_gross_pay > 0 and ll_gross_pay_base_year < ll_index_taxation_year Then
				ldec_preacc_indexed_gross_pay = in_bencalcs.f_index_cpi_amount(ldec_gross_pay,ll_gross_pay_base_year,ld_index_date)
				If ldec_preacc_indexed_gross_pay = -1 Then
					Close(This)
					Return -1
				End If
			Else 
				ldec_preacc_indexed_gross_pay = ldec_gross_pay
			End If
		END IF
		
			ldec_annual_gross = f_convert_frequency(ls_earn_freq_code,ldec_preacc_indexed_gross_pay,"A")
		
			
		/*	Calculate cpp, uic, tax, annual net and frequency net
		   *****NOTE********* code changed for calculations for P10105
		*/
			If ls_paid_cpp_flag	= "Y" Then 
				ldec_annual_cpp = in_bencalcs.f_calculate_cpp(ld_index_date,ldec_annual_gross,ldt_create_datetime)
			END IF
			
			If ls_paid_uic_flag	= "Y" Then 
				ldec_annual_uic = in_bencalcs.f_calculate_uic(ld_index_date,ldec_annual_gross)
			END IF
			

			ldec_annual_tax = in_bencalcs.f_calc_annual_taxable_income(ldec_annual_gross,ldec_annual_uic,ldec_annual_cpp)

		/* now we need to reset the R and K factors based on our taxable income amount
		*/
			in_bencalcs.f_get_r_and_k(ld_index_date,ldec_annual_tax)
		/*	SR 64:  Reset provincial V and KP factors (added for TONI provincial taxation changes, effective Jan. 2001)
		*/
			IF ld_index_date >= Date("2001/01/01") THEN
				in_bencalcs.f_get_v_and_kp(ld_index_date,ldec_annual_tax)
			END IF
			
			IF lb_pre_93 = TRUE THEN
				ldec_annual_tax = in_bencalcs.f_calc_pre93_tax(ldec_annual_tax,ldec_annual_uic,ldec_annual_cpp)
			ELSE
		/* SR 64 - Jan. 2001 tax changes - added provincial basic & married TD1 deductions as arguments to
													  f_calculate_tax	*/
		// PR 2806 - validation of exemptions before making calculations							 	  
			ls_exemption_mismatch = wf_exemption_mismatch (ldec_fed_td1_exemption_amt,ldec_td1_basic_exemption , ldec_td1_married_exemption, &
				ldec_prov_td1_exemption_amt, ldec_prov_td1_basic_exemption , ldec_prov_td1_married_exemption,ldt_yearly_factor_date)
			
			IF ls_exemption_mismatch <> 'NONE' THEN
				RETURN -1
			END IF
			
			ldec_annual_tax = in_bencalcs.f_calculate_tax(ld_index_date,ldec_fed_td1_exemption_amt,ldec_prov_td1_exemption_amt,ldec_annual_tax,ldec_annual_cpp,ldec_annual_uic,ldt_create_datetime)
			END IF 
			ldec_annual_net = ldec_annual_gross - (ldec_annual_cpp + ldec_annual_uic + ldec_annual_tax)
		
			ldec_net_pay = f_convert_frequency("A",ldec_annual_net,ls_award_freq_code)
		/*	Store the fields we just computed
		*/
			dw_benefit_calculation_preaccident_data.SetItem(ll_rownum,"preacc_indexed_gross_pay",ldec_preacc_indexed_gross_pay)
			dw_benefit_calculation_preaccident_data.SetItem(ll_rownum,"preacc_annual_gross",ldec_annual_gross)
			dw_benefit_calculation_preaccident_data.SetItem(ll_rownum,"preacc_annual_cpp",ldec_annual_cpp)
			dw_benefit_calculation_preaccident_data.SetItem(ll_rownum,"preacc_annual_uic",ldec_annual_uic)
			dw_benefit_calculation_preaccident_data.SetItem(ll_rownum,"preacc_annual_tax",ldec_annual_tax)
			dw_benefit_calculation_preaccident_data.SetItem(ll_rownum,"preacc_annual_net",ldec_annual_net)
			dw_benefit_calculation_preaccident_data.SetItem(ll_rownum,"preacc_net_pay",ldec_net_pay)
			
	Case "Average"

		ll_rownum					=	dw_benefit_calculation_preaccident_data.GetRow()
		ls_earn_freq_code		   =	dw_benefit_calculation_preaccident_data.GetItemString(ll_rownum,"avg_earn_freq_code")
		ldec_gross_pay				=	dw_benefit_calculation_preaccident_data.GetItemDecimal(ll_rownum,"avg_gross_pay")
		ls_paid_uic_flag			=	dw_benefit_calculation_preaccident_data.GetItemString(ll_rownum,"avg_paid_uic_flag")
		ls_paid_cpp_flag			=	dw_benefit_calculation_preaccident_data.GetItemString(ll_rownum,"avg_paid_cpp_flag")
		ll_gross_pay_base_year	=	dw_benefit_calculation_preaccident_data.GetItemNumber(ll_rownum,"preacc_gross_pay_base_year")


		/* When displaying the details the avg_indexed_gross_pay should be retrieved from the BENEFIT CALCULATION TABLE
		   PR1890 */
		IF ib_detail = TRUE THEN 
			ldec_avg_indexed_gross_pay = dw_benefit_calculation_preaccident_data.GetItemNumber(ll_rownum,"avg_indexed_gross_pay")
		ELSE
		/*	Index the amount ahead
		*/
			If ldec_gross_pay > 0 and ll_gross_pay_base_year < ll_index_taxation_year Then
				ldec_avg_indexed_gross_pay = in_bencalcs.f_index_cpi_amount(ldec_gross_pay,ll_gross_pay_base_year,ld_index_date)
				If ldec_avg_indexed_gross_pay = -1 Then 
					Close(This)
					Return -1
				End If
			Else
				ldec_avg_indexed_gross_pay = ldec_gross_pay
			End If
		END IF
			ldec_annual_gross = f_convert_frequency(ls_earn_freq_code,ldec_avg_indexed_gross_pay,"A")


		/*	Apply the maximum earnings
		*/
			ldec_max_avg_earnings = in_bencalcs.f_get_max_avg_earnings(ld_index_date)
			If ldec_max_avg_earnings = -1 Then
				Close(This)
				Return -1
			Else
				IF ldec_max_avg_earnings > 0 THEN
					If ldec_annual_gross > ldec_max_avg_earnings Then 
						ldec_annual_gross = ldec_max_avg_earnings
						dw_benefit_calculation_preaccident_data.Modify("st_capped.Visible='1'")
					Else
						dw_benefit_calculation_preaccident_data.Modify("st_capped.Visible='0'")
					End If
				END IF
			End If

		/*	Calculate cpp, uic, tax, annual net and frequency net
		   take note of pre 93 benefit calculation code
		*/
			If ls_paid_cpp_flag = "Y" Then
				ldec_annual_cpp = in_bencalcs.f_calculate_cpp(ld_index_date,ldec_annual_gross,ldt_create_datetime)
			END IF	
					
			If ls_paid_uic_flag = "Y" Then 
				ldec_annual_uic = in_bencalcs.f_calculate_uic(ld_index_date,ldec_annual_gross)
			END IF
	
			ldec_annual_tax = in_bencalcs.f_calc_annual_taxable_income(ldec_annual_gross,ldec_annual_uic,ldec_annual_cpp)
			
		/* now we need to reset the R and K factors based on our taxable income amount
		*/
			in_bencalcs.f_get_r_and_k(ld_index_date,ldec_annual_tax)
		//	SR 64:  Reset provincial V and KP factors (added for TONI provincial taxation changes, effective Jan. 2001)
			IF ld_index_date >= Date("2001/01/01") THEN
				in_bencalcs.f_get_v_and_kp(ld_index_date,ldec_annual_tax)
			END IF

			IF lb_pre_93 = TRUE THEN
				ldec_annual_tax = in_bencalcs.f_calc_pre93_tax(ldec_annual_tax,ldec_annual_uic,ldec_annual_cpp)
			ELSE
			/* SR 64 - Jan. 2001 tax changes - added provincial basic & married TD1 deductions as arguments to
											  f_calculate_tax	*/
			// PR 2806 - validation of exemptions before making calculations							 	  
			ls_exemption_mismatch = wf_exemption_mismatch (ldec_fed_td1_exemption_amt,ldec_td1_basic_exemption , ldec_td1_married_exemption, &
				ldec_prov_td1_exemption_amt, ldec_prov_td1_basic_exemption , ldec_prov_td1_married_exemption,ldt_yearly_factor_date)
			
			IF ls_exemption_mismatch <> 'NONE' THEN
				RETURN -1
			END IF
			
				ldec_annual_tax = in_bencalcs.f_calculate_tax(ld_index_date,ldec_fed_td1_exemption_amt,ldec_prov_td1_exemption_amt,ldec_annual_tax,ldec_annual_cpp,ldec_annual_uic,ldt_create_datetime)
			END IF 
			ldec_annual_net = ldec_annual_gross - (ldec_annual_cpp + ldec_annual_uic + ldec_annual_tax)
			
			ldec_net_pay	 = f_convert_frequency("A",ldec_annual_net,ls_award_freq_code)
		/*	Store the computed fields
		*/
			dw_benefit_calculation_preaccident_data.SetItem(ll_rownum,"avg_indexed_gross_pay",ldec_avg_indexed_gross_pay)
			dw_benefit_calculation_preaccident_data.SetItem(ll_rownum,"avg_annual_gross",ldec_annual_gross)
			dw_benefit_calculation_preaccident_data.SetItem(ll_rownum,"avg_annual_cpp",ldec_annual_cpp)
			dw_benefit_calculation_preaccident_data.SetItem(ll_rownum,"avg_annual_uic",ldec_annual_uic)
			dw_benefit_calculation_preaccident_data.SetItem(ll_rownum,"avg_annual_tax",ldec_annual_tax)
			dw_benefit_calculation_preaccident_data.SetItem(ll_rownum,"avg_annual_net",ldec_annual_net)
			dw_benefit_calculation_preaccident_data.SetItem(ll_rownum,"avg_net_pay",ldec_net_pay)

	Case "Frozen"
		
		/* The frozen earnings portion of the benefit calculation will be automatically calculated 
			based on the frozen earnings entered by the user. The basis for the frozen earnings 
			calculations will be the 1992 taxation period calculations, formulaes and taxation 
			parameters with the exception of the TD1 basic exemption parameter. The TD1 basic exemption
			parameter will be from the current taxation period. For example: if the benefit calculations being
			calculated is for the taxation period of Janurary 2000, the frozen earnings will be calculated using 
			all of the 1992 calculations and parameters other then the TD1 basic exemption for 1992 instead, the 
			Janurary 2000 TD1 basic exemption would be used
			
			RETURNS: SETS (frozen net earnings)
		*/
		
		
		ll_rownum			= 	dw_benefit_calculation_frozen_data.GetRow()

		IF ll_rownum > 0 THEN
			ls_earn_freq_code	=	dw_benefit_calculation_frozen_data.GetItemString(ll_rownum,"frozen_earn_freq_code")
			ldec_gross_pay		=	dw_benefit_calculation_frozen_data.GetItemDecimal(ll_rownum,"frozen_gross_pay")
			ldec_annual_net	=	dw_benefit_calculation_frozen_data.GetItemDecimal(ll_rownum,"frozen_annual_net_pay")
			
			ldec_annual_gross	      = f_convert_frequency(ls_earn_freq_code,ldec_gross_pay,"A")
			ldec_net_pay				= f_convert_frequency("A",ldec_annual_net,ls_award_freq_code)
			
			IF isvalid(in_bencalcs) THEN
				//grab the flags for uic and cpp to see if we need to modify the amounts based on this change
				IF dw_benefit_calculation_preaccident_data.rowcount() > 0 THEN
					ls_paid_uic_flag = dw_benefit_calculation_frozen_data.GetItemString(1,"frozen_uic_flag")
					ls_paid_cpp_flag = dw_benefit_calculation_frozen_data.GetItemString(1,"frozen_cpp_flag")
				END IF
				
				/* for the purpose of this we need to grab the ACTUAL TD1
					being used, we will do this by defaulting this variable at the local
					level and then reset the instance variable with the value.
					QUESTION: isthis the current TD1 OR the one from the year Actual 
					year being calculated - if it is the current one then we need to
					do a select and grab the current TD1
				*/
				ldec_basic_exemption = in_bencalcs.idec_basic_pers_exempt
				IF ldec_basic_exemption = 0 THEN
					ldec_basic_exemption = ldec_fed_td1_exemption_amt
				END IF

  				/* now we need to default the rest of the variables based on the 
  				default year of 1992 - after this is complete we will repopulate
   			the instance variable idec_basic_pers_exempt with our ldec_basic_exemption
	   		variable.
		   	Note * The frozen earnings are calculated after 1992/01/01
			   */
				
				in_bencalcs.f_set_index_date(ldt_frozen_92_taxyear)
								
				/* default the work variables based on the 1992 period
				
				PR 1890 - indicate that this is a Frozen Earnings calculation so that the month within 1992 is not changed
				inadvertently to July 1992 for effective dates
				*/
				in_bencalcs.f_populate_work_variables(ld_effective_from_date, 'Frozen')
				st_frozen_92_taxyear.Text = String(ldt_frozen_92_taxyear,'yyyy-mm-dd')
				/* reset the exemption amount based on the TRUE period
				*/
				in_bencalcs.idec_basic_pers_exempt = ldec_basic_exemption 
					
				/*	Apply the maximum earnings
				*/
				// S.Manzer Temporary Change
				ldec_max_avg_earnings = in_bencalcs.f_get_max_avg_earnings(ldt_frozen_92_taxyear)
				If ldec_max_avg_earnings = -1 Then
					Close(This)
					Return -1
				Else
					IF ldec_max_avg_earnings > 0 THEN
						If ldec_annual_gross > ldec_max_avg_earnings Then 
							ldec_annual_gross = ldec_max_avg_earnings
						End If
					END IF
				End If
				
				IF ls_paid_cpp_flag	= "Y" THEN 
					ldec_annual_cpp = in_bencalcs.f_calculate_cpp(ld_index_date,ldec_annual_gross,ldt_create_datetime)
				ELSE
					ldec_annual_cpp = 0 
				END IF
				
				dw_benefit_calculation_frozen_data.Object.frozen_cpp.Text = '$' + String(Round(ldec_annual_cpp,2))
				
				IF ls_paid_uic_flag	= "Y" THEN 
					ldec_annual_uic = in_bencalcs.f_calculate_uic(ld_index_date,ldec_annual_gross)
				ELSE
					ldec_annual_uic = 0 
				END IF	
				
				dw_benefit_calculation_frozen_data.Object.frozen_uic.Text = '$' + String(Round(ldec_annual_uic,2))
				
				ldec_annual_tax = in_bencalcs.f_calc_annual_taxable_income(ldec_annual_gross,ldec_annual_uic,ldec_annual_cpp)
				
				/* now we need to reset the R and K factors based on our taxable income amount
				*/
				//S.Manzer Tempory Change
				in_bencalcs.f_get_r_and_k(ldt_frozen_92_taxyear,ldec_annual_tax)
		//	SR 64:  Reset provincial V and KP factors (added for TONI provincial taxation changes, effective Jan. 2001)
			IF ld_index_date >= Date("2001/01/01") THEN
				in_bencalcs.f_get_v_and_kp(ld_index_date,ldec_annual_tax)
			END IF
				
				/* set the annual gross from the user input
				*/
				in_bencalcs.f_set_annual_gross(ldec_annual_gross)
				
				ldec_annual_tax = in_bencalcs.f_calc_pre93_tax(ldec_annual_tax,ldec_annual_uic,ldec_annual_cpp)
				
				dw_benefit_calculation_frozen_data.Object.frozen_annual_tax.Text = '$' + String(Round(ldec_annual_tax,2))
				
				ldec_annual_net = ldec_annual_gross - (ldec_annual_cpp + ldec_annual_uic + ldec_annual_tax)
				
				//based on the new net set the net based on the frequency supplied
				ldec_net_pay = f_convert_frequency("A",ldec_annual_net,ls_award_freq_code)	
		
				IF (is_frozen_exists = "Y"	and ib_detail = true) or &
					(IsNull(ld_effective_from_date) and ib_copy_ben_calc = true) THEN
// PR1882 - SMANZER 					
					ldec_annual_gross = f_convert_frequency(ls_earn_freq_code,dw_benefit_calculation_frozen_data.getitemnumber(1,"frozen_gross_pay"),"A")
					IF ldec_max_avg_earnings > 0 THEN
						If ldec_annual_gross > ldec_max_avg_earnings Then 
							ldec_annual_gross = ldec_max_avg_earnings
						End If
					END IF
				// S. Manzer Temporary Change	
					dw_benefit_calculation_frozen_data.SetItem(ll_rownum,"frozen_annual_gross",ldec_annual_gross)
					dw_benefit_calculation_frozen_data.SetItem(ll_rownum,"frozen_net_pay",dw_benefit_calculation_frozen_data.getitemnumber(1,"frozen_net_pay"))
					dw_benefit_calculation_frozen_data.setitem(ll_rownum,"frozen_annual_net_pay",dw_benefit_calculation_frozen_data.getitemnumber(1,"frozen_annual_net_pay"))
				ELSE
					dw_benefit_calculation_frozen_data.SetItem(ll_rownum,"frozen_annual_gross",ldec_annual_gross)
					dw_benefit_calculation_frozen_data.SetItem(ll_rownum,"frozen_net_pay",ldec_net_pay)
					dw_benefit_calculation_frozen_data.setitem(ll_rownum,"frozen_annual_net_pay",ldec_annual_net)
				END IF
			END IF
		END IF
	Case "Capable"

		ll_rownum			=	dw_benefit_calculation_capable_data.GetRow()
		ls_earn_freq_code	=	dw_benefit_calculation_capable_data.GetItemString(ll_rownum,"pre93_capable_earn_freq_code")
		ldec_gross_pay		=	dw_benefit_calculation_capable_data.GetItemDecimal(ll_rownum,"pre93_capable_gross_pay")
		ls_paid_uic_flag	=	dw_benefit_calculation_capable_data.GetItemString(ll_rownum,"pre93_capable_uic_flag")
		ls_paid_cpp_flag	=	dw_benefit_calculation_capable_data.GetItemString(ll_rownum,"pre93_capable_cpp_flag")

		ldec_annual_gross	=	f_convert_frequency(ls_earn_freq_code,ldec_gross_pay,"A")
		

		/*	Calculate cpp, uic, tax, annual net and frequency net
		*/	
		If ls_paid_cpp_flag = "Y" Then 
			ldec_annual_cpp = in_bencalcs.f_calculate_cpp(ld_index_date,ldec_annual_gross,ldt_create_datetime)
		END IF	
					
		If ls_paid_uic_flag = "Y" Then 
			ldec_annual_uic = in_bencalcs.f_calculate_uic(ld_index_date,ldec_annual_gross)	
		END IF
				
		ldec_annual_tax = in_bencalcs.f_calc_annual_taxable_income(ldec_annual_gross,ldec_annual_uic,ldec_annual_cpp)
		
		/* now we need to reset the R and K factors based on our taxable income amount
		*/
		in_bencalcs.f_get_r_and_k(ld_index_date,ldec_annual_tax)
		//	SR 64:  Reset provincial V and KP factors (added for TONI provincial taxation changes, effective Jan. 2001)
			IF ld_index_date >= Date("2001/01/01") THEN
				in_bencalcs.f_get_v_and_kp(ld_index_date,ldec_annual_tax)
			END IF
			
		IF lb_pre_93 = TRUE THEN
			ldec_annual_tax = in_bencalcs.f_calc_pre93_tax(ldec_annual_tax,ldec_annual_uic,ldec_annual_cpp)
		ELSE
		/* SR 64 - Jan. 2001 tax changes - added provincial basic & married TD1 deductions as arguments to
													  f_calculate_tax	*/
		// PR 2806 - validation of exemptions before making calculations							 	  
		ls_exemption_mismatch = wf_exemption_mismatch (ldec_fed_td1_exemption_amt,ldec_td1_basic_exemption , ldec_td1_married_exemption, &
			ldec_prov_td1_exemption_amt, ldec_prov_td1_basic_exemption , ldec_prov_td1_married_exemption,ldt_yearly_factor_date)
		
		IF ls_exemption_mismatch <> 'NONE' THEN
			RETURN -1
		END IF
			
			ldec_annual_tax = in_bencalcs.f_calculate_tax(ld_index_date,ldec_fed_td1_exemption_amt,ldec_prov_td1_exemption_amt,ldec_annual_tax,ldec_annual_cpp,ldec_annual_uic,ldt_create_datetime)
		END IF 	
		ldec_annual_net = ldec_annual_gross - (ldec_annual_cpp + ldec_annual_uic + ldec_annual_tax)
			
		ldec_net_pay	 = f_convert_frequency("A",ldec_annual_net,ls_award_freq_code)
	
		/*	Store the computed fields
		*/
			dw_benefit_calculation_capable_data.SetItem(ll_rownum,"pre93_capable_annual_gross",ldec_annual_gross)
			dw_benefit_calculation_capable_data.SetItem(ll_rownum,"pre93_capable_annual_cpp",ldec_annual_cpp)
			dw_benefit_calculation_capable_data.SetItem(ll_rownum,"pre93_capable_annual_uic",ldec_annual_uic)
			dw_benefit_calculation_capable_data.SetItem(ll_rownum,"pre93_capable_annual_tax",ldec_annual_tax)
			dw_benefit_calculation_capable_data.SetItem(ll_rownum,"pre93_capable_annual_net",ldec_annual_net)
			dw_benefit_calculation_capable_data.SetItem(ll_rownum,"pre93_capable_net_pay",ldec_net_pay)

	Case "Remunerations"

		/*	Calculate the annual gross, the cpp insurable earnings, the uic insurable earnings, and the taxable earnings
		*/
			ll_counter = 1
			ll_rownum = dw_benefit_calculation_remunerations.RowCount()

			Do While ll_counter <= ll_rownum
				ls_earn_freq_code       =  dw_benefit_calculation_remunerations.GetItemString(ll_counter,"remun_earn_freq_code")
				ldec_gross_pay		      =	dw_benefit_calculation_remunerations.GetItemDecimal(ll_counter,"remun_gross_pay")
				ldec_annual_gross	      =	f_convert_frequency(ls_earn_freq_code,ldec_gross_pay,"A")
				dw_benefit_calculation_remunerations.SetItem(ll_counter,"annual_gross",ldec_annual_gross)
				ldec_total_annual_gross = ldec_total_annual_gross + ldec_annual_gross

				If dw_benefit_calculation_remunerations.GetItemString(ll_counter,"paid_cpp_flag") = "Y" Then
					ldec_cpp_insurable_earnings +=  ldec_annual_gross
				End IF

				If dw_benefit_calculation_remunerations.GetItemString(ll_counter,"paid_uic_flag") = "Y" Then
					ldec_uic_insurable_earnings += ldec_annual_gross
				End IF

				If dw_benefit_calculation_remunerations.GetItemString(ll_counter,"tax_flag") = "Y" Then
					ldec_taxable_earnings = ldec_taxable_earnings + ldec_annual_gross
				End IF

				ll_counter ++
			Loop
			
		ls_paid_uic_flag			=	dw_benefit_calculation_remunerations.GetItemString(1,"paid_uic_flag")
		ls_paid_cpp_flag			=	dw_benefit_calculation_remunerations.GetItemString(1,"paid_cpp_flag")

		/*	Calculate the cpp, uic, and taxes based on the values computed above
		*/
			ldec_annual_cpp =	in_bencalcs.f_calculate_cpp(ld_index_date,ldec_cpp_insurable_earnings,ldt_create_datetime)
					
			ldec_annual_uic =	in_bencalcs.f_calculate_uic(ld_index_date,ldec_uic_insurable_earnings)
			
			ldec_annual_tax = in_bencalcs.f_calc_annual_taxable_income(ldec_taxable_earnings,ldec_annual_uic,ldec_annual_cpp)//ldec_total_annual_gross
		/* now we need to reset the R and K factors based on our taxable income amount
		*/
			in_bencalcs.f_get_r_and_k(ld_index_date,ldec_annual_tax)
		//	SR 64:  Reset provincial V and KP factors (added for TONI provincial taxation changes, effective Jan. 2001)
			IF ld_index_date >= Date("2001/01/01") THEN
				in_bencalcs.f_get_v_and_kp(ld_index_date,ldec_annual_tax)
			END IF
			
			IF lb_pre_93 = TRUE THEN
				ldec_annual_tax = in_bencalcs.f_calc_pre93_tax(ldec_annual_tax,ldec_annual_uic,ldec_annual_cpp)
			ELSE
		/* SR 64 - Jan. 2001 tax changes - added provincial basic & married TD1 deductions as arguments to
													  f_calculate_tax	*/
			// PR 2806 - validation of exemptions before making calculations							 	  
				ls_exemption_mismatch = wf_exemption_mismatch (ldec_fed_td1_exemption_amt,ldec_td1_basic_exemption , ldec_td1_married_exemption, &
					ldec_prov_td1_exemption_amt, ldec_prov_td1_basic_exemption , ldec_prov_td1_married_exemption,ldt_yearly_factor_date)
				
				IF ls_exemption_mismatch <> 'NONE' THEN
					RETURN -1
				END IF
			
				ldec_annual_tax = in_bencalcs.f_calculate_tax(ld_index_date,ldec_fed_td1_exemption_amt,ldec_prov_td1_exemption_amt,ldec_taxable_earnings,ldec_annual_cpp,ldec_annual_uic,ldt_create_datetime)
			END IF 

			ldec_annual_net = ldec_total_annual_gross - (ldec_annual_cpp + ldec_annual_uic + ldec_annual_tax)
			ldec_net_pay	 = f_convert_frequency("A",ldec_annual_net,ls_award_freq_code)
		
		/*	Store the computed fields (We're looping here because we want the results to show up on every record, and since
			the computed fields are in the data source, this is a must)
		*/
			ll_counter = 1
			ll_rownum = dw_benefit_calculation_remunerations.RowCount()
			Do While ll_counter <= ll_rownum
				dw_benefit_calculation_remunerations.SetItem(ll_counter,"annual_gross",ldec_annual_gross)
				dw_benefit_calculation_remunerations.SetItem(ll_counter,"total_annual_gross",ldec_total_annual_gross)
				dw_benefit_calculation_remunerations.SetItem(ll_counter,"compute_annual_cpp",ldec_annual_cpp)
				dw_benefit_calculation_remunerations.SetItem(ll_counter,"compute_annual_uic",ldec_annual_uic)
				dw_benefit_calculation_remunerations.SetItem(ll_counter,"compute_annual_tax",ldec_annual_tax)
				dw_benefit_calculation_remunerations.SetItem(ll_counter,"compute_annual_net",ldec_annual_net)
				dw_benefit_calculation_remunerations.SetItem(ll_counter,"compute_net_remunerations",ldec_net_pay)
				ll_counter ++
			Loop

	Case "Deductions"

		ll_counter = 1
		ll_rownum = dw_benefit_calculation_deductions.RowCount()
		Do While ll_counter <= ll_rownum
			ls_earn_freq_code	=	dw_benefit_calculation_deductions.GetItemString(ll_counter,"deduction_earn_freq_code")
			ldec_gross_pay		=	dw_benefit_calculation_deductions.GetItemDecimal(ll_counter,"deduction_amount")
			ldec_net_pay		=	f_convert_frequency(ls_earn_freq_code,ldec_gross_pay,ls_award_freq_code)
			dw_benefit_calculation_deductions.SetItem(ll_counter,"deduction_freq_pay",ldec_net_pay)
			ll_counter ++
		Loop

	Case "CPPD"

		ll_rownum					=	dw_benefit_calculation_cppd.GetRow()
		ldec_gross_pay				=	dw_benefit_calculation_cppd.GetItemDecimal(ll_rownum,"cppd_monthly_amount")
		ll_gross_pay_base_year	=	dw_benefit_calculation_cppd.GetItemNumber(ll_rownum,"cppd_base_year")

		/* When displaying the details the cppd_indexed_gross_pay should be retrieved from the BENEFIT CALCULATION TABLE
		   PR1890 */
		IF ib_detail = TRUE THEN 
			ldec_cppd_indexed_gross_pay = dw_benefit_calculation_preaccident_data.GetItemNumber(ll_rownum,"cppd_indexed_monthly_amount")
		ELSE
		/*	Index the amount ahead (if required)
		*/
			If ldec_gross_pay > 0 and ll_gross_pay_base_year < ll_index_taxation_year Then
				ldec_cppd_indexed_gross_pay = in_bencalcs.f_index_cppd_amount(ldec_gross_pay,ll_gross_pay_base_year,ld_index_date)
				If ldec_annual_gross = -1 Then
					Close(This)
					Return -1
				End If
			Else
				ldec_cppd_indexed_gross_pay = ldec_gross_pay
			End If
		END IF

		/*	Store the computed field
		*/
			dw_benefit_calculation_cppd.SetItem(ll_rownum,"cppd_indexed_monthly_amount",ldec_cppd_indexed_gross_pay)

	End Choose

Return 1
end function

public function string wf_exemption_mismatch (decimal adec_fed_exemption_entered, decimal adec_fed_basic_exemption, decimal adec_fed_married_exemption, decimal adec_prov_exemption_entered, decimal adec_prov_basic_exemption, decimal adec_prov_married_exemption, date adt_index_period);// Function determines if the entered amounts for Federal and Provincial Exemptions are within similar ranges
// and are both above the basic exemption amount.

IF (adec_fed_exemption_entered = adec_fed_basic_exemption) OR (adec_fed_exemption_entered = adec_fed_married_exemption) THEN
ELSEIF (adec_fed_exemption_entered < adec_fed_basic_exemption) AND (adec_fed_exemption_entered <> 0) THEN
	// SR 64: See Ed's note above, re. $0 occasionally allowed for fed. exemption - therefore don't show error msg. if $0
	Messagebox('Invalid Federal TD1 Exemption', 'The Federal TD1 Exemption must be equal to ' + &
	+ 'or greater than the federal basic personal exemption.~r~nThe federal basic personal exemption ' + &
	+ 'for the taxation period ' + String(adt_index_period) + ' is $' + String(adec_fed_basic_exemption))
	Return 'FED'
END IF

IF (adec_prov_exemption_entered = adec_prov_basic_exemption) OR (adec_prov_exemption_entered = adec_prov_married_exemption) THEN
ELSEIF (adec_prov_exemption_entered < adec_prov_basic_exemption) THEN
	IF (adec_prov_exemption_entered = 0) AND Year(adt_index_period) < 2001 THEN
		//don't display messagebox since $0 is default value for pre-2001 prov. exemption
	ELSE
		Messagebox('Invalid Provincial TD1 Exemption', 'The Provincial TD1 Exemption must be equal to ' + &
		+ 'or greater than the provincial basic personal exemption.~r~r~nThe provincial basic personal exemption ' + &
		+ 'for the taxation period ' + String(adt_index_period) + ' is $' + String(adec_prov_basic_exemption))
		Return 'PROV'
	END IF
END IF

IF Year(adt_index_period) >= 2001 THEN
	IF ib_suppress_exemption_msg = FALSE THEN	
		IF (adec_fed_exemption_entered > adec_fed_basic_exemption) AND (adec_fed_exemption_entered < adec_fed_married_exemption) THEN
		//	Federal TD1 exemption is manual entry and is between basic & married amounts
		
			IF (adec_prov_exemption_entered > adec_prov_basic_exemption) AND (adec_prov_exemption_entered < adec_prov_married_exemption) THEN
			//	Both Provincial & Federal manual TD1 exemptions are between basic & married amounts, so is okay
			ELSE
		
			//	Federal TD1 is a manual exemption between basic & married, but Provincial is not
				IF MessageBox('WARNING!','The Federal and Provincial TD1 exemptions should match.~r~n~nThe ' + &
				+ 'Federal TD1 Exemption amount is a manual entry that is greater than the Federal basic amount but ' + &
				+ 'less than the Federal married amount. The Provincial TD1 exemption should also be a manual amount ' + &
				+ 'that is greater than the Provincial basic amount but less than the Provincial married amount.' + &
				+ '~r~nWould you like to continue using the values entered?', Question!, yesno!) = 2 THEN 	
					Return 'FED'
				ELSE
					dw_benefit_calculation_details.SetColumn('prov_td1_exemption_amount')
					ib_suppress_exemption_msg = TRUE
					RETURN 'NONE'
				END IF
			END IF
	
		ELSEIF (adec_fed_exemption_entered > adec_fed_married_exemption) THEN
		//	Federal TD1 exemption is manual entry and is greater than the married amount
	
			IF (adec_prov_exemption_entered > adec_prov_married_exemption) THEN
			//	Both Provincial & Federal manual TD1 exemptions are greater than the married amounts, so is okay
			ELSE
	
			//	Federal TD1 is a manual exemption that is greater than married amount, but Provincial is not
				IF MessageBox('WARNING!','The Federal and Provincial TD1 exemptions should match.~r~n~nThe ' + &
				+ 'Federal TD1 Exemption amount is a manual entry that is greater than the Federal married amount. ' + &
				+ 'The Provincial TD1 exemption should also be a manual amount that is greater than the Provincial ' + &
				+ 'married amount.'+ &
				+ '~r~nWould you like to continue using the values entered?', Question!, yesno!) = 2 THEN
					Return 'FED'
				ELSE
					dw_benefit_calculation_details.SetColumn('prov_td1_exemption_amount')
					ib_suppress_exemption_msg = TRUE
					RETURN 'NONE'
				END IF
			END IF
		
		ELSEIF (adec_fed_exemption_entered = adec_fed_basic_exemption) THEN
		
			IF (adec_prov_exemption_entered <> adec_prov_basic_exemption) AND (adec_prov_exemption_entered <> adec_prov_married_exemption) THEN
	
			// Federal TD1 is basic but Prov. TD1 is a manual entry
				IF MessageBox('WARNING!','The Federal and Provincial TD1 exemptions should match.~r~n~nThe ' + &
				+ 'Federal TD1 exemption amount is Basic but the Provincial TD1 exemption is a manual amount.  ' + &
				+ 'If the Federal TD1 exemption is Basic then the Provincial TD1 exemption should also be Basic.'+ &
				+ '~r~nWould you like to continue using the values entered?', Question!, yesno!) = 2 THEN
					Return 'FED'
				ELSE
					dw_benefit_calculation_details.SetColumn('prov_td1_exemption_amount')
					ib_suppress_exemption_msg = TRUE
					RETURN 'NONE'
				END IF
			END IF
			
		ELSEIF adec_fed_exemption_entered = adec_fed_married_exemption THEN
	
			IF (adec_prov_exemption_entered <> adec_prov_basic_exemption) AND (adec_prov_exemption_entered <> adec_prov_married_exemption) THEN
				
			// Federal TD1 is married but Prov. TD1 is a manual entry
				IF MessageBox('WARNING!','The Federal and Provincial TD1 exemptions should match.~r~n~nThe ' + &
				+ 'Federal TD1 exemption amount is Married but the Provincial TD1 exemption is a manual amount.  ' + &
				+ 'If the Federal TD1 exemption is Married then the Provincial TD1 exemption should also be Married.'+ &
				+ '~r~nWould you like to continue using the values entered?', Question!, yesno!) = 2 THEN
					Return 'FED'
				ELSE
					dw_benefit_calculation_details.SetColumn('prov_td1_exemption_amount')
					ib_suppress_exemption_msg = TRUE
					RETURN 'NONE'
				END IF
			END IF
		
		END IF	
		
	END IF
	
	IF adec_fed_exemption_entered = adec_fed_basic_exemption AND adec_prov_exemption_entered = adec_prov_married_exemption THEN
		MessageBox('TD1 Exemption Mismatch','The Federal and Provincial TD1 exemptions must match.~r~n~nThe ' + &
		+ 'Federal TD1 exemption amount is Basic but the Provincial TD1 exemption amount is Married.  ' + &
		+ 'If the Federal TD1 exemption is Basic then the Provincial TD1 exemption must also be Basic.',Information!)
		Return 'FED'
		
	ELSEIF adec_fed_exemption_entered = adec_fed_married_exemption AND adec_prov_exemption_entered = adec_prov_basic_exemption THEN
		MessageBox('TD1 Exemption Mismatch','The Federal and Provincial TD1 exemptions must match.~r~n~nThe ' + &
		+ 'Federal TD1 exemption amount is Married but the Provincial TD1 exemption amount is Basic.  ' + &
		+ 'If the Federal TD1 exemption is Married then the Provincial TD1 exemption must also be Married.', Information!)
			Return 'FED'			
	END IF	
END IF

RETURN 'NONE'
end function

public subroutine wf_set_rtw_deduction (long al_row);DECIMAL		ldec_reg_LTD_award_amount

SELECT	award_amount
INTO		:ldec_reg_LTD_award_amount
FROM		BENEFIT_CALCULATION
WHERE	claim_no		= :il_claim_no
AND		benefit_calculation_no	= :istr_bencalc_parameters.al_doubleparm[4]
USING SQLCA;

SQLCA.nf_handle_error('select from BENEFIT_CALCULATION', 'w_calculation_details','wf_set_rtw_deduction')

dw_benefit_calculation_deductions.SetItem(al_row,'benefit_calc_deduction_code','LTD')
dw_benefit_calculation_deductions.SetItem(al_row,'deduction_amount',ldec_reg_LTD_award_amount)
dw_benefit_calculation_deductions.SetItem(al_row,'deduction_earn_freq_code','M') // monthly

end subroutine

public subroutine wf_set_rtw_remuneration (long al_row);DECIMAL		ldec_prev_year_capable
STRING		ls_prev_year_capable_type , ls_prev_year_freq_code , ls_prev_year_uic_flag , ls_prev_year_cpp_flag

// get the capable earning values
ldec_prev_year_capable = dw_benefit_calculation_capable_data.GetItemDecimal(1, 'pre93_capable_gross_pay')
ls_prev_year_capable_type = dw_benefit_calculation_capable_data.GetItemString(1, 'pre93_capable_earn_type_code')
ls_prev_year_freq_code	= dw_benefit_calculation_capable_data.GetItemString(1,'pre93_capable_earn_freq_code')
ls_prev_year_uic_flag				= dw_benefit_calculation_capable_data.GetItemString(1,'pre93_capable_uic_flag')
ls_prev_year_cpp_flag				= dw_benefit_calculation_capable_data.GetItemString(1,'pre93_capable_cpp_flag')

// set the remuneration values
dw_benefit_calculation_remunerations.SetItem(al_row,'remun_gross_pay',ldec_prev_year_capable)
IF ls_prev_year_capable_type = 'A' THEN
	dw_benefit_calculation_remunerations.SetItem(al_row,'remun_type_code','WAGE')
ELSEIF ls_prev_year_capable_type = 'E' THEN
	dw_benefit_calculation_remunerations.SetItem(al_row,'remun_type_code','ESTM')
ELSE
	// THERE SHOULD NOT BE ANY OTHER TYPE OF CAPABLE EARNING
	// other than estimate or actual
END IF
dw_benefit_calculation_remunerations.SetItem(al_row,'remun_earn_freq_code',ls_prev_year_freq_code)
dw_benefit_calculation_remunerations.SetItem(al_row,'paid_uic_flag',ls_prev_year_uic_flag)
dw_benefit_calculation_remunerations.SetItem(al_row,'paid_cpp_flag',ls_prev_year_cpp_flag)
dw_benefit_calculation_remunerations.SetItem(al_row,'tax_flag','Y')


end subroutine

public function integer wf_check_benefit_level (decimal adec_benefit_level_percentage, date adt_effective_from_date, string as_transitional_claim_flag, decimal adec_regular_award_amount, decimal adec_transitional_award_amount);/*
8.	Percentage – This is a drop down of the percentage used to calculate the award amount. The percentage 
can be 80%, 85%, 90% or 100%. The claimant (or survivor) will get this percentage of the amount calculated.
Several factors determine what the benefit level percentage will be including the benefit type, the 
accident/recurrence date, the effective date of the calculation, the calculation type and the transitional 
claim flag. 
	a.	If the benefit type is PEN, Survivor, Survivor1 or Survivor2, then the calculation type is manual 
		and the benefit percentage is 100%.
	b.	If the benefit type is LTD or RLOE then
		i.	If the accident/recurrence date is 1998-01-01 or later, the benefit percentage is 85%
		ii.	If the accident/recurrence date is between 1993-01-01 and 1997-12-31 then
			i.	If the effective date is greater than 1997-12-31, the benefit percentage is 85%.
			ii.	If the effective date is between 1993-01-01 and 1997-12-31, the benefit percentage may be 
				80% or 85%. This must be selected by the user.
		iii.	If the accident/recurrence date is less than 1993-01-01 then
			i.	If the effective date is less than 1993-01-01, the benefit percentage is 90%.
			ii.	If the effective date is greater than or equal to 1993-01-01 and the transitional claim flag 
				is ’Yes’, then the benefit percentage is 90% .
			iii.	If the effective date is greater than or equal to 1993-01-01 and the transitional claim flag 
				is ‘No’ then the benefit percentage is 85%. 
				
*/

DATE		ldt_accident_recurrence_date
STRING	ls_opening_type_code
INTEGER	li_rtn


ldt_accident_recurrence_date	= Date(istr_bencalc_parameters.adtm_datetimeparm[1])
ls_opening_type_code			= istr_bencalc_parameters.as_stringparm[3]

	IF ISNULL (adec_benefit_level_percentage) or adec_benefit_level_percentage = 0 THEN
		MessageBox("Benefit Calculation Module","Please enter a value for the Benefit Level Percentage", Exclamation!)
		dw_benefit_calculation_details.SetColumn("benefit_level_percentage")
		Return -1
	END IF
	
	CHOOSE CASE ls_opening_type_code
		CASE 'PEN','S1','S2','SV'
			IF adec_benefit_level_percentage <> 1.00 THEN
				MessageBox("Benefit Calculation Module - Validation Error","Benefit level must be 100% for pension and survivor openings.",Exclamation!)
				Return -1
			END IF
			
		CASE 'RLOE','LTD'
			IF adec_benefit_level_percentage = 1.00 THEN
				MessageBox("Benefit Calculation Module - Validation Error","Benefit level must not be 100% for RLOE and LTD openings.",Exclamation!)
				Return -1
			END IF
			
			IF ldt_accident_recurrence_date >= Date("1998/01/01") THEN
				IF adec_benefit_level_percentage <> 0.85 THEN
					MessageBox("Benefit Calculation Module - Validation Error","Benefit level must be 85% for RLOE and LTD openings with an accident/recurrence date on or after 1998-01-01.",Exclamation!)
					Return -1
				END IF
				
			ELSEIF ldt_accident_recurrence_date >= Date("1993/01/01") AND ldt_accident_recurrence_date <= Date("1997/12/31") THEN
				IF adt_effective_from_date > Date("1997/12/31") THEN
					IF adec_benefit_level_percentage <> 0.85 THEN
						MessageBox("Benefit Calculation Module - Validation Error","Benefit level must be 85% for RLOE and LTD openings with an accident/recurrence date between 1993-01-01 and 1997-12-31 where the effective date is after 1997-12-31.",Exclamation!)
						Return -1
					END IF
				ELSEIF adt_effective_from_date >= Date("1993/01/01") AND adt_effective_from_date <= Date("1997/12/31") THEN
					IF adec_benefit_level_percentage <> 0.85 AND adec_benefit_level_percentage <> 0.80 THEN
						MessageBox("Benefit Calculation Module - Validation Error","Benefit level must be either 80% or 85% for RLOE and LTD openings with both accident/recurrence and effective dates between 1993-01-01 and 1997-12-31.",Exclamation!)
						Return -1
					END IF
				END IF
				
			ELSEIF ldt_accident_recurrence_date < Date("1993/01/01") THEN
				IF adt_effective_from_date < Date("1993/01/01") THEN
					IF adec_benefit_level_percentage <> 0.90 THEN
						MessageBox("Benefit Calculation Module - Validation Error","Benefit level must be 90% for RLOE and LTD openings with an accident/recurrence date and an effective date before 1993-01-01.",Exclamation!)
						Return -1
					END IF
				ELSEIF adt_effective_from_date >= Date("1993/01/01") THEN
					IF as_transitional_claim_flag = 'Y' THEN
						// do not reset the benefit level where the transitional flag should be reset
						// the function below will set the flag properly
						li_rtn = wf_85_pct_transitional_passed(adec_benefit_level_percentage,adec_regular_award_amount,adec_transitional_award_amount)
						IF li_rtn < 0 THEN
							RETURN 0
						ELSEIF li_rtn = 1 THEN
							RETURN 0
						END IF
						
						IF adec_benefit_level_percentage <> 0.90 THEN
							MessageBox("Benefit Calculation Module - Validation Error","Benefit level must be 90% for RLOE and LTD openings with both accident/recurrence before 1993-01-01, effective date on or after 1993-01-01 and transitional flag = Y.",Exclamation!)
							Return -1
						END IF
					ELSE
						// as_transitional_claim_flag = 'N'
						IF adec_benefit_level_percentage <> 0.85 THEN
							MessageBox("Benefit Calculation Module - Validation Error","Benefit level must be 85% for RLOE and LTD openings with accident/recurrence before 1993-01-01, effective date on or after 1993-01-01 and transitional flag = N.",Exclamation!)
							Return -1
						END IF
					END IF
					
				END IF
				
			END IF
		CASE ELSE
			// What would this be?
			
	END CHOOSE
	
RETURN 0
end function

public subroutine wf_set_effective_date (ref dwobject a_dwo);dw_benefit_calculation_details.SetItem(1,'effective_from_date',Date(istr_bencalc_parameters.adtm_datetimeparm[3]))
dw_benefit_calculation_details.SetColumn('effective_from_date')
dw_benefit_calculation_details.Event ItemChanged(1,a_dwo,String(istr_bencalc_parameters.adtm_datetimeparm[3]))
end subroutine

protected function integer wf_verify_copied_ltd_bencalc ();STRING	ls_message, ls_curr_yr_capable_type, ls_added_for_rtw
DATE 		ldt_prev_year_reg_LTD_effective_from_date , ldt_reg_LTD_effective_from_date
DECIMAL	ldec_curr_yr_award_amount, ldec_curr_yr_gross_pay
INTEGER	li_result, li_parameter_rows , li_rows, li_rtw_rowcount, li_counter, li_non_rtw_deductions_count

/*
This function is called when dw_benefit_calculation_details.itemchanged is called for rtw_incentive_flag. It's also
called in cb_save.clicked.
*/

U_DS			lds_benefit_calculation_details, lds_benefit_calculation_preaccident_data
U_DS			lds_benefit_calculation_frozen_data, lds_benefit_calculation_cppd
U_DS			lds_benefit_calculation_deductions , lds_benefit_calculation_remunerations


// verify that user has not altered BENEFIT CALCULATION PARAMETERS from copied regular "current" year LTD bencalc
lds_benefit_calculation_details = Create U_DS
lds_benefit_calculation_details.DataObject = 'd_benefit_calculation_details'
lds_benefit_calculation_details.SetTransObject(SQLCA)

li_parameter_rows = lds_benefit_calculation_details.Retrieve(il_claim_no,istr_bencalc_parameters.al_doubleparm[4])

IF li_parameter_rows = 1 THEN
	ls_message = wf_verify_copied_LTD_bencalc_parameters(lds_benefit_calculation_details)
	IF ls_message <> '' THEN
		MessageBox('RTW Incentive Error','The '+ls_message+' is not the same as for the LTD Benefit Calculation that was copied.'+&
													'~n~nThis can only be a RTW benefit calculation if '+ls_message+' is changed to match.',StopSign!)
		RETURN 2
	END IF
END IF


// verify that user has not altered BENEFIT CALCULATION PRE-ACCIDENT data from copied regular "current" year LTD bencalc
lds_benefit_calculation_preaccident_data = Create U_DS
lds_benefit_calculation_preaccident_data.DataObject = 'd_benefit_calculation_preaccident_data'
lds_benefit_calculation_preaccident_data.SetTransObject(SQLCA)

lds_benefit_calculation_details.ShareData(lds_benefit_calculation_preaccident_data)
li_rows = lds_benefit_calculation_preaccident_data.RowCount()

// verify that copied preaccident data will pass BRs
IF li_parameter_rows = 1 THEN
	ls_message = wf_verify_copied_preaccident_data(lds_benefit_calculation_preaccident_data)
	IF ls_message <> '' THEN
		MessageBox('RTW Incentive Error','The '+ls_message+' is not the same as for the LTD Benefit Calculation that was copied.'+&
													'~n~nThis can only be a RTW benefit calculation if '+ls_message+' is changed to match.',StopSign!)
		RETURN 2
	END IF
END IF


// verify that user has not altered BENEFIT CALCULATION FROZEN data from copied regular "current" year LTD bencalc
lds_benefit_calculation_frozen_data = Create U_DS
lds_benefit_calculation_frozen_data.DataObject = 'd_benefit_calculation_frozen_data'
lds_benefit_calculation_frozen_data.SetTransObject(SQLCA)

li_rows = lds_benefit_calculation_frozen_data.Retrieve(il_claim_no,istr_bencalc_parameters.al_doubleparm[4])

// verify that copied frozen data will pass BRs
IF li_rows = 1 THEN
	ls_message = wf_verify_copied_frozen_data(lds_benefit_calculation_frozen_data)
	IF ls_message <> '' THEN
		MessageBox('RTW Incentive Error','The '+ls_message+' is not the same as for the LTD Benefit Calculation that was copied.'+&
												'~n~nThis can only be a RTW benefit calculation if '+ls_message+' is changed to match.',StopSign!)
		RETURN 2
	END IF
END IF


// verify that user has not altered BENEFIT CALCULATION CPPD DEDUCTION data from copied regular "current" year LTD bencalc
lds_benefit_calculation_cppd = Create U_DS
lds_benefit_calculation_cppd.DataObject = 'd_benefit_calculation_cppd'
lds_benefit_calculation_cppd.SetTransObject(SQLCA)

lds_benefit_calculation_details.ShareData(lds_benefit_calculation_cppd)
li_rows = lds_benefit_calculation_cppd.RowCount()

IF li_parameter_rows = 1 THEN
	ls_message = wf_verify_CPPD_deduction(lds_benefit_calculation_cppd)
	IF ls_message <> '' THEN
		MessageBox('RTW Incentive Error','The '+ls_message+' is not the same as for the LTD Benefit Calculation that was copied.'+&
													'~n~nThis can only be a RTW benefit calculation if '+ls_message+' is changed to match.',StopSign!)
		RETURN 2
	END IF
END IF
	

// verify that user has not altered BENEFIT CALCULATION REGULAR DEDUCTIONS data from copied regular "current" year LTD bencalc
lds_benefit_calculation_deductions = Create U_DS
lds_benefit_calculation_deductions.DataObject = 'd_benefit_calculation_deductions'
lds_benefit_calculation_deductions.SetTransObject(SQLCA)

li_rows = lds_benefit_calculation_deductions.Retrieve(il_claim_no,istr_bencalc_parameters.al_doubleparm[4])
li_rtw_rowcount = dw_benefit_calculation_deductions.RowCount()
FOR li_counter = 1 TO li_rtw_rowcount
	ls_added_for_rtw = dw_benefit_calculation_deductions.GetItemString(li_counter, 'added_for_rtw')
	IF ls_added_for_rtw = '' THEN
		li_non_rtw_deductions_count = li_non_rtw_deductions_count + 1
	END IF
NEXT

// there will eventually be one more row for the RTW bencalc than for the copied regular bencalc
// but at this point in the code the counts should be the same
IF li_rows = 0 THEN
	// OK - no need to check
ELSE
	IF li_rows = li_non_rtw_deductions_count THEN
		// By passing the non-RTW deduction count, the function should only attempt to match all
		// but the last deduction, i.e., the one that was added to match the reg LTD bencalc award.
		ls_message = wf_verify_regular_deductions(lds_benefit_calculation_deductions, li_non_rtw_deductions_count)
		IF ls_message <> '' THEN
		MessageBox('RTW Incentive Error','The '+ls_message+' is not the same as for the LTD Benefit Calculation that was copied.'+&
													'~n~nThis can only be a RTW benefit calculation if '+ls_message+' is changed to match.',StopSign!)
			RETURN 2
		END IF
	ELSE
		MessageBox('RTW Incentive Error','There are more deductions for this benefit calculation' +&
													'~nthan there were for the copied regular LTD benefit calculation.'+&
													'~n~nThis can only be a RTW benefit calculation if the number of deductions match.',StopSign!)
		RETURN 2
	END IF
END IF


// verify that user has not added BENEFIT CALCULATION REMUNERATIONS records from copied regular "current" year LTD bencalc
lds_benefit_calculation_remunerations = Create U_DS
lds_benefit_calculation_remunerations.DataObject = 'd_benefit_calculation_remunerations'
lds_benefit_calculation_remunerations.SetTransObject(SQLCA)

li_rows = lds_benefit_calculation_remunerations.Retrieve(il_claim_no,istr_bencalc_parameters.al_doubleparm[4])
li_rtw_rowcount = dw_benefit_calculation_remunerations.RowCount()

// the counts should be the same
IF li_rows = li_rtw_rowcount THEN
ELSE
	MessageBox('RTW Incentive Error','There are more remunerations for this benefit calculation' +&
												'~nthan there were for the copied regular LTD benefit calculation.'+&
												'~n~nTherefore, this cannot be a RTW Incentive Benefit Calculation.',StopSign!)
	RETURN 2
END IF




// verify if the copied LTD bencalc is linked to another current year bencalc or to a bencalc prior to the previous year
ldt_prev_year_reg_LTD_effective_from_date = Date(ids_rtw_incentive_linked_bencalcs.GetItemDateTime(1,'prev_yrs_ltd_effective_from_date'))
ldt_reg_LTD_effective_from_date  = Date(ids_rtw_incentive_linked_bencalcs.GetItemDateTime(1,'curr_yr_reg_ltd_effective_from_date'))

IF Year(ldt_prev_year_reg_LTD_effective_from_date) <> Year(ldt_reg_LTD_effective_from_date) - 1 THEN
	MessageBox('RTW Incentive Error','The LTD Benefit Calculation that was copied was not copied from'+&
												'~nan LTD Benefit Calculation for the previous year.'+&
												'~n~nTherefore, this cannot be a RTW Incentive Benefit Calculation.',StopSign!)
	RETURN 2
END IF

// warn if the copied LTD award was zero
ldec_curr_yr_award_amount = ids_rtw_incentive_linked_bencalcs.GetItemDecimal(1,'curr_yrs_reg_ltd_award_amount')

IF ldec_curr_yr_award_amount = 0 THEN
	IF MessageBox('RTW Incentive Error','For a RTW Incentive benefit calculation, the LTD Benefit Calculation'+&
												'~nthat was copied should have an award amount greater than zero.'+&
												'~nDo you want to continue?', Exclamation!,YesNo!,2) = 2 THEN
		RETURN 2
	END IF
END IF

// verify if the copied LTD capable amount was zero
ldec_curr_yr_gross_pay = ids_rtw_incentive_linked_bencalcs.GetItemDecimal(1,'curr_yr_reg_pre93_capable_gross_pay') 

IF ldec_curr_yr_gross_pay = 0 THEN
	MessageBox('RTW Incentive Error','The LTD Benefit Calculation that was copied does not have'+&
												'~na capable earnings amount greater than zero.'+&
												'~n~nTherefore, this cannot be a RTW Incentive Benefit Calculation.',StopSign!)
	RETURN 2
END IF

// verify if the copied LTD capable type was actual
ls_curr_yr_capable_type = ids_rtw_incentive_linked_bencalcs.GetItemString(1,'curr_yr_reg_pre93_capable_earn_type_code')
IF ls_curr_yr_capable_type <> 'A' THEN
	MessageBox('RTW Incentive Error','The LTD Benefit Calculation that was copied does not have'+&
												'~nan actual capable earnings record.'+&
												'~n~nTherefore, this cannot be a RTW Incentive Benefit Calculation.',StopSign!)
	RETURN 2
END IF


RETURN 0
end function

public function long wf_check_rtw_bus_rules ();LONG			ll_result
DATE			ldt_effective_date, ldt_copied_effective_date
DATETIME	ldtm_min_qualification_date, ldtm_max_qualification_date
INTEGER		li_count, li_year, li_ltd_benefit_calc_no, li_prior_copied_bencalc_no, li_copied_from_effective_year, li_opening_no


// retrieve datastore that will be used in BR tests
ll_result = ids_rtw_incentive_linked_bencalcs.Retrieve(il_claim_no, istr_bencalc_parameters.al_doubleparm[4])
ldt_effective_date = Date(dw_benefit_calculation_details.GetItemDateTime(1,'effective_from_date'))

// verify that copied LTD ben calc can be used
IF ll_result = 1 THEN
	//OK
ELSEIF ll_result = 0 THEN
	SELECT	Count(*)
	INTO		:li_count
	FROM		BENEFIT_CALCULATION
	WHERE	benefit_calculation_no		= :istr_bencalc_parameters.al_doubleparm[4]
	AND		claim_no							= :il_claim_no
	AND		Year(effective_from_date)	= Year(:ldt_effective_date)
	USING SQLCA;
	
	SQLCA.nf_handle_error("w_calculation_details","wf_check_rtw_bus_rules","select from BENEFIT_CALCULATION (1) ")
	
	IF li_count = 0 THEN
		MessageBox('RTW Incentive Error','This benefit calculation was not copied from another LTD benefit calculation for the current year.' +&
												'~n~nTherefore, this cannot be a RTW Incentive Benefit Calculation.',StopSign!)
		RETURN 2
	END IF
	
	SELECT	Count(*)
	INTO		:li_count
	FROM		BENEFIT_CALCULATION	a
	JOIN		CAPABLE_EARNING		b	ON	a.claim_no					= b.claim_no
													AND	a.benefit_calculation_no	= b.benefit_calculation_no 
	WHERE	a.benefit_calculation_no					= :istr_bencalc_parameters.al_doubleparm[4]
	AND		a.claim_no									= :il_claim_no
	AND		b.pre93_capable_earn_type_code	= 'A'
	USING SQLCA;
	
	SQLCA.nf_handle_error("w_calculation_details","wf_check_rtw_bus_rules","select from BENEFIT_CALCULATION (2) ")
	
	IF li_count = 0 THEN
		MessageBox('RTW Incentive Error','The regular LTD benefit calculation that was copied does not have' +&
												'~nan actual capable earnings record.' +&
												'~n~nTherefore, this cannot be a RTW Incentive Benefit Calculation.',StopSign!)
		RETURN 2
	END IF
	
	SELECT	Count(*)
	INTO		:li_count
	FROM		BENEFIT_CALCULATION	a
	JOIN		BENEFIT_CALCULATION	b	ON	a.claim_no		= b.claim_no
	JOIN		OPENING					c	ON	b.claim_no		= c.claim_no
													AND	b.opening_no	= c.opening_no
	WHERE	a.benefit_calculation_no				= :istr_bencalc_parameters.al_doubleparm[4]
	AND		a.claim_no								= :il_claim_no
	AND		a.copied_from_benefit_calc_no	= b.benefit_calculation_no
	AND		Year(b.effective_from_date)		= Year(a.effective_from_date) - 1
	AND		c.opening_type_code					= 'LTD'
	USING SQLCA;
	
	SQLCA.nf_handle_error("w_calculation_details","wf_check_rtw_bus_rules","select from BENEFIT_CALCULATION (3) ")
	
	IF li_count = 0 THEN
		MessageBox('RTW Incentive Error','The regular LTD benefit calculation that was copied is not linked to' +&
												'~nan LTD benefit calculation for the previous year.' +&
												'~n~nTherefore, this cannot be a RTW Incentive Benefit Calculation.',StopSign!)
		RETURN 2
	END IF
	
ELSEIF ll_result < 0 THEN
	SQLCA.nf_handle_error("w_calculation_details","wf_check_rtw_bus_rules","ItemChanged - retrieved ids_rtw_incentive_linked_bencalcs")
ELSEIF ll_result > 1 THEN
	// can this happen?
END IF

// the effective date must match the effective date for the copied LTD bencalc
// the effective date must be within the range for Rtw_Incentive_Qualification_Parameter

ldt_copied_effective_date = Date(istr_bencalc_parameters.adtm_datetimeparm[3])

SELECT	min_qualification_date , max_qualification_date
INTO		:ldtm_min_qualification_date ,  :ldtm_max_qualification_date
FROM		Rtw_Incentive_Qualification_Parameter
WHERE	rtw_incentive_type_code = 'LTD'
USING	SQLCA;

SQLCA.nf_handle_error("w_calculation_details","wf_check_rtw_bus_rules","select min_qualification_date , max_qualification_date from Rtw_Incentive_Qualification_Parameter")

IF ldt_copied_effective_date < Date(ldtm_min_qualification_date) OR &
		ldt_copied_effective_date > Date(ldtm_max_qualification_date) THEN
	MessageBox('RTW Incentive Error','The effective date entered is not within the range of the RTW Incentive program.' +&
												'~n~nTherefore, this cannot be a RTW Incentive Benefit Calculation.',StopSign!)
	RETURN 2 // reject value, allow focus to change
END IF

/*
	A LTD RTW Incentive benefit calculation must not be associated with a regular LTD benefit calculation, if 
	the latter has an associated regular LTD or an associated RTW Incentive benefit calculation effective in the same year.
*/
li_year = Year(Date(dw_benefit_calculation_details.GetItemDateTime(1,'effective_from_date')))
li_ltd_benefit_calc_no	 = dw_benefit_calculation_details.GetItemNumber(1,'copied_from_benefit_calc_no')

SELECT	c.benefit_calculation_no , Year(a.effective_from_date)
INTO		:li_prior_copied_bencalc_no , :li_copied_from_effective_year
FROM		BENEFIT_CALCULATION a
JOIN		OPENING					b	ON	a.claim_no 		= b.claim_no
												AND	a.opening_no	= b.opening_no
JOIN		BENEFIT_CALCULATION c 	ON	a.claim_no					= c.claim_no
												AND	a.benefit_calculation_no	= c.copied_from_benefit_calc_no
JOIN		OPENING					d	ON	a.claim_no		= d.claim_no
												AND	a.opening_no	= d.opening_no
WHERE	a.claim_no					= :il_claim_no
AND		a.benefit_calculation_no	= :li_ltd_benefit_calc_no
AND		b.opening_type_code	= 'LTD'
AND		d.opening_type_code	= 'LTD'
USING SQLCA;

SQLCA.nf_handle_error("w_calculation_details","wf_check_rtw_bus_rules","select benefit_calculation_no, Year... from BENEFIT_CALCULATION")

IF li_year = li_copied_from_effective_year THEN
	MessageBox('Benefit Calculation Module - Validation Error','A LTD RTW Incentive benefit calculation must not be associated with a regular' +&
																			'~nLTD benefit calculation, if the latter has an associated regular LTD or' +&
																			'~nan associated RTW Incentive benefit calculation effective in the same year.' +&
																			'~nSee benefit calculation #'+String(li_prior_copied_bencalc_no)+'.' +&
																			'~n~nTherefore, this cannot be a RTW Incentive Benefit Calculation.',StopSign!)
	RETURN 2
END IF

/*
Reg LTD bencalc & assoc RTW bencalc must have same opening
*/
SELECT	opening_no
INTO		:li_opening_no
FROM		BENEFIT_CALCULATION
WHERE	claim_no		= :il_claim_no
AND		benefit_calculation_no	= :li_ltd_benefit_calc_no
USING	SQLCA;

SQLCA.nf_handle_error("w_calculation_details","wf_check_rtw_bus_rules","select opening_no from BENEFIT_CALCULATION")

IF li_opening_no <> il_opening_no THEN
	MessageBox('Benefit Calculation Module - Validation Error','The opening for this benefit calculation is not the same as the copied regular LTD benefit calculation.'+&
																			'~n~nTherefore, this cannot be a RTW Incentive Benefit Calculation.',StopSign!)
	RETURN 2
END IF

// verify that copied data will pass BRs
ll_result = wf_verify_copied_LTD_bencalc()
IF ll_result <> 0 THEN RETURN ll_result

RETURN 0
end function

public function integer wf_85_pct_transitional_passed (decimal adec_benefit_level_percentage, decimal adec_regular_award_amount, decimal adec_transitional_award_amount);
/* For a Transitional Claim, if the 85% has passed the 90%, then warn the user that the transitional claim 
	flag in the benefit calculation details will be updated 
*/
STRING		ls_transitional_claim_flag

IF adec_regular_award_amount >= adec_transitional_award_amount THEN
	ls_transitional_claim_flag = "N"
	dw_benefit_calculation_details.SetItem(1,"transitional_claim_flag",ls_transitional_claim_flag)
	IF adec_benefit_level_percentage <> 0.85 THEN
		dw_benefit_calculation_details.SetItem(1,"benefit_level_percentage",.85)
		MessageBox("Benefit Calculation Module - FYI!","The benefit at 85% has passed the frozen benefits at 90%~r~n" + &
					"The award has been set using the 85% calculation~r~n" + &
					"The benefit level percentage will be set to 85%~r~n" + &
					"The transitional claim flag for this benefit calculation will be set to 'No'",Information!)
		RETURN -1
	ELSE
		MessageBox("Benefit Calculation Module - FYI!","The benefit at 85% has passed the frozen benefits at 90%~r~n" + &
					"The award has been set using the 85% calculation~r~n" + &
					"The transitional claim flag for this benefit calculation has been set to 'No'",Information!)
		RETURN -1
	END IF
ELSE
	IF adec_benefit_level_percentage <> 0.9 THEN
		dw_benefit_calculation_details.SetItem(1,"benefit_level_percentage",0.9)
		MessageBox("Benefit Calculation Module - FYI!","The benefit at 85% has not passed the frozen benefits at 90%~r~n" + &
					"The benefit level percentage will be set to .90",Information!)
		RETURN -1
	ELSE
		// OK
		RETURN 1
	END IF
END IF

RETURN 0
end function

private function string wf_verify_copied_frozen_data (ref datastore ads_benefit_calculation_frozen_data);STRING		ls_frozen_earn_freq_code , ls_frozen_cpp_flag , ls_frozen_uic_flag

STRING		ls_frozen_earn_freq_code_NEW , ls_frozen_cpp_flag_NEW , ls_frozen_uic_flag_NEW

DECIMAL		ldec_frozen_gross_pay , ldec_frozen_annual_net_pay , ldec_frozen_net_pay

DECIMAL		ldec_frozen_gross_pay_NEW , ldec_frozen_annual_net_pay_NEW , ldec_frozen_net_pay_NEW

STRING		ls_message , ls_st_award_frequency_text


ls_st_award_frequency_text = dw_benefit_calculation_preaccident_data.Describe("st_award_frequency.Text")

ls_frozen_earn_freq_code			= ads_benefit_calculation_frozen_data.GetItemString(1,'frozen_earn_freq_code')
ls_frozen_cpp_flag					= ads_benefit_calculation_frozen_data.GetItemString(1,'frozen_cpp_flag')
ls_frozen_uic_flag						= ads_benefit_calculation_frozen_data.GetItemString(1,'frozen_uic_flag')

ldec_frozen_gross_pay					= ads_benefit_calculation_frozen_data.GetItemDecimal(1,'frozen_gross_pay')
ldec_frozen_annual_net_pay			= ads_benefit_calculation_frozen_data.GetItemDecimal(1,'frozen_annual_net_pay')
ldec_frozen_net_pay						= ads_benefit_calculation_frozen_data.GetItemDecimal(1,'frozen_net_pay')

dw_benefit_calculation_frozen_data.AcceptText()

ls_frozen_earn_freq_code_NEW	= dw_benefit_calculation_frozen_data.GetItemString(1,'frozen_earn_freq_code')
ls_frozen_cpp_flag_NEW			= dw_benefit_calculation_frozen_data.GetItemString(1,'frozen_cpp_flag')
ls_frozen_uic_flag_NEW				= dw_benefit_calculation_frozen_data.GetItemString(1,'frozen_uic_flag')
	
ldec_frozen_gross_pay_NEW			= dw_benefit_calculation_frozen_data.GetItemDecimal(1,'frozen_gross_pay')
ldec_frozen_annual_net_pay_NEW	= dw_benefit_calculation_frozen_data.GetItemDecimal(1,'frozen_annual_net_pay')
ldec_frozen_net_pay_NEW				= dw_benefit_calculation_frozen_data.GetItemDecimal(1,'frozen_net_pay')


CHOOSE CASE TRUE
	CASE ls_frozen_earn_freq_code <> ls_frozen_earn_freq_code_NEW
		ls_message = 'Frequency (Frozen Earnings)'
	CASE ls_frozen_cpp_flag <> ls_frozen_cpp_flag_NEW
		ls_message = 'Deduct CPP Flag (Frozen Earnings)'
	CASE ls_frozen_uic_flag <> ls_frozen_uic_flag_NEW
		ls_message = 'Deduct UIC Flag (Frozen Earnings)'
		
	CASE ldec_frozen_gross_pay <> ldec_frozen_gross_pay_NEW
		ls_message = 'Gross (Frozen Earnings)'
	CASE ldec_frozen_annual_net_pay <> ldec_frozen_annual_net_pay_NEW
		ls_message = 'Annual Net (Frozen Earnings)'
	CASE ldec_frozen_net_pay <> ldec_frozen_net_pay_NEW
		ls_message = ls_st_award_frequency_text + ' Net (Frozen Earnings)'
		
END CHOOSE

RETURN ls_message

end function

private function string wf_verify_copied_ltd_bencalc_parameters (ref datastore ads_benefit_calculation_details);DATETIME	ldtm_effective_from_date , ldtm_effective_from_date_NEW

STRING		ls_top_up_flag , ls_transitional_claim_flag, ls_calculation_reason_code , ls_calculation_type_code

STRING		ls_top_up_flag_NEW , ls_transitional_claim_flag_NEW, ls_calculation_reason_code_NEW , ls_calculation_type_code_NEW

DECIMAL		ldec_td1_exemption_amount , ldec_preacc_work_days_per_week
DECIMAL		ldec_preacc_work_hours_per_day , ldec_benefit_level_percentage

DECIMAL		ldec_td1_exemption_amount_NEW , ldec_preacc_work_days_per_week_NEW
DECIMAL		ldec_preacc_work_hours_per_day_NEW , ldec_benefit_level_percentage_NEW

STRING		ls_message


ldtm_effective_from_date				= ads_benefit_calculation_details.GetItemDateTime(1,'effective_from_date')
ls_top_up_flag								= ads_benefit_calculation_details.GetItemString(1,'top_up_flag')
ls_transitional_claim_flag					= ads_benefit_calculation_details.GetItemString(1,'transitional_claim_flag')
ls_calculation_type_code				= ads_benefit_calculation_details.GetItemString(1,'calculation_type_code')
ls_calculation_reason_code				= ads_benefit_calculation_details.GetItemString(1,'calculation_reason_code')
ldec_td1_exemption_amount			= ads_benefit_calculation_details.GetItemDecimal(1,'td1_exemption_amount')
ldec_preacc_work_days_per_week	= ads_benefit_calculation_details.GetItemDecimal(1,'preacc_work_days_per_week')
ldec_preacc_work_hours_per_day	= ads_benefit_calculation_details.GetItemDecimal(1,'preacc_work_hours_per_day')
ldec_benefit_level_percentage			= ads_benefit_calculation_details.GetItemDecimal(1,'benefit_level_percentage')

dw_benefit_calculation_details.AcceptText()

ldtm_effective_from_date_NEW				= dw_benefit_calculation_details.GetItemDateTime(1,'effective_from_date')
ls_top_up_flag_NEW								= dw_benefit_calculation_details.GetItemString(1,'top_up_flag')
ls_transitional_claim_flag_NEW					= dw_benefit_calculation_details.GetItemString(1,'transitional_claim_flag')
ls_calculation_type_code_NEW					= dw_benefit_calculation_details.GetItemString(1,'calculation_type_code')
ls_calculation_reason_code_NEW				= dw_benefit_calculation_details.GetItemString(1,'calculation_reason_code')
ldec_td1_exemption_amount_NEW			= dw_benefit_calculation_details.GetItemDecimal(1,'td1_exemption_amount')
ldec_preacc_work_days_per_week_NEW	= dw_benefit_calculation_details.GetItemDecimal(1,'preacc_work_days_per_week')
ldec_preacc_work_hours_per_day_NEW		= dw_benefit_calculation_details.GetItemDecimal(1,'preacc_work_hours_per_day')
ldec_benefit_level_percentage_NEW			= dw_benefit_calculation_details.GetItemDecimal(1,'benefit_level_percentage')

CHOOSE CASE TRUE
	CASE ldtm_effective_from_date <> ldtm_effective_from_date_NEW
		ls_message = 'Effective Date'
	CASE ls_top_up_flag <> ls_top_up_flag_NEW
		ls_message = 'Top-Up Provision in Effect Flag'
	CASE ls_transitional_claim_flag <> ls_transitional_claim_flag_NEW
		ls_message = 'Transitional Claim Flag'
	CASE ls_calculation_type_code <> ls_calculation_type_code_NEW
		ls_message = 'Calculation Type'
	CASE ls_calculation_reason_code <> ls_calculation_reason_code_NEW
		ls_message = 'Calculation Reason'
	CASE ldec_td1_exemption_amount <> ldec_td1_exemption_amount_NEW
		ls_message = 'TD1 Exemption Amount'
	CASE ldec_preacc_work_days_per_week <> ldec_preacc_work_days_per_week_NEW
		ls_message = 'Pre-Accident Work Days per Week'
	CASE ldec_preacc_work_hours_per_day <> ldec_preacc_work_hours_per_day_NEW
		ls_message = 'Pre-Accident Work Hours per Day'
	CASE ldec_benefit_level_percentage <> ldec_benefit_level_percentage_NEW
		ls_message = 'Benefit Level Percentage'
END CHOOSE


RETURN ls_message
end function

private function string wf_verify_copied_preaccident_data (ref datastore ads_benefit_calculation_preaccident_data);STRING		ls_preacc_earn_freq_code , ls_avg_earn_freq_code
STRING		ls_preacc_paid_cpp_flag , ls_avg_paid_cpp_flag
STRING		ls_preacc_paid_uic_flag , ls_avg_paid_uic_flag

STRING		ls_preacc_earn_freq_code_NEW , ls_avg_earn_freq_code_NEW
STRING		ls_preacc_paid_cpp_flag_NEW , ls_avg_paid_cpp_flag_NEW
STRING		ls_preacc_paid_uic_flag_NEW , ls_avg_paid_uic_flag_NEW

INTEGER		li_preacc_gross_pay_base_year

INTEGER		li_preacc_gross_pay_base_year_NEW

DECIMAL		ldec_preacc_gross_pay , ldec_avg_gross_pay
DECIMAL		ldec_preacc_indexed_gross_pay , ldec_avg_indexed_gross_pay
DECIMAL		ldec_preacc_net_pay , ldec_avg_net_pay

DECIMAL		ldec_preacc_gross_pay_NEW , ldec_avg_gross_pay_NEW
DECIMAL		ldec_preacc_indexed_gross_pay_NEW , ldec_avg_indexed_gross_pay_NEW
DECIMAL		ldec_preacc_net_pay_NEW , ldec_avg_net_pay_NEW

STRING		ls_message , ls_st_award_frequency_text


ls_st_award_frequency_text = dw_benefit_calculation_preaccident_data.Describe("st_award_frequency.Text")

li_preacc_gross_pay_base_year		= ads_benefit_calculation_preaccident_data.GetItemNumber(1,'preacc_gross_pay_base_year')

ls_preacc_earn_freq_code			= ads_benefit_calculation_preaccident_data.GetItemString(1,'preacc_earn_freq_code')
ls_avg_earn_freq_code				= ads_benefit_calculation_preaccident_data.GetItemString(1,'avg_earn_freq_code')
ls_preacc_paid_cpp_flag				= ads_benefit_calculation_preaccident_data.GetItemString(1,'preacc_paid_cpp_flag')
ls_avg_paid_cpp_flag					= ads_benefit_calculation_preaccident_data.GetItemString(1,'avg_paid_cpp_flag')
ls_preacc_paid_uic_flag				= ads_benefit_calculation_preaccident_data.GetItemString(1,'preacc_paid_uic_flag')
ls_avg_paid_uic_flag					= ads_benefit_calculation_preaccident_data.GetItemString(1,'avg_paid_uic_flag')

ldec_preacc_gross_pay					= ads_benefit_calculation_preaccident_data.GetItemDecimal(1,'preacc_gross_pay')
ldec_avg_gross_pay						= ads_benefit_calculation_preaccident_data.GetItemDecimal(1,'avg_gross_pay')
ldec_preacc_indexed_gross_pay		= ads_benefit_calculation_preaccident_data.GetItemDecimal(1,'preacc_indexed_gross_pay')
ldec_avg_indexed_gross_pay			= ads_benefit_calculation_preaccident_data.GetItemDecimal(1,'avg_indexed_gross_pay')
ldec_preacc_net_pay						= ads_benefit_calculation_preaccident_data.GetItemDecimal(1,'preacc_net_pay')
ldec_avg_net_pay							= ads_benefit_calculation_preaccident_data.GetItemDecimal(1,'avg_net_pay')

dw_benefit_calculation_preaccident_data.AcceptText()

li_preacc_gross_pay_base_year_NEW	= dw_benefit_calculation_preaccident_data.GetItemNumber(1,'preacc_gross_pay_base_year')

ls_preacc_earn_freq_code_NEW		= dw_benefit_calculation_preaccident_data.GetItemString(1,'preacc_earn_freq_code')
ls_avg_earn_freq_code_NEW			= dw_benefit_calculation_preaccident_data.GetItemString(1,'avg_earn_freq_code')
ls_preacc_paid_cpp_flag_NEW			= dw_benefit_calculation_preaccident_data.GetItemString(1,'preacc_paid_cpp_flag')
ls_avg_paid_cpp_flag_NEW				= dw_benefit_calculation_preaccident_data.GetItemString(1,'avg_paid_cpp_flag')
ls_preacc_paid_uic_flag_NEW			= dw_benefit_calculation_preaccident_data.GetItemString(1,'preacc_paid_uic_flag')
ls_avg_paid_uic_flag_NEW				= dw_benefit_calculation_preaccident_data.GetItemString(1,'avg_paid_uic_flag')
	
ldec_preacc_gross_pay_NEW				= dw_benefit_calculation_preaccident_data.GetItemDecimal(1,'preacc_gross_pay')
ldec_avg_gross_pay_NEW					= dw_benefit_calculation_preaccident_data.GetItemDecimal(1,'avg_gross_pay')
ldec_preacc_indexed_gross_pay_NEW	= dw_benefit_calculation_preaccident_data.GetItemDecimal(1,'preacc_indexed_gross_pay')
ldec_avg_indexed_gross_pay_NEW		= dw_benefit_calculation_preaccident_data.GetItemDecimal(1,'avg_indexed_gross_pay')
ldec_preacc_net_pay_NEW					= dw_benefit_calculation_preaccident_data.GetItemDecimal(1,'preacc_net_pay')
ldec_avg_net_pay_NEW						= dw_benefit_calculation_preaccident_data.GetItemDecimal(1,'avg_net_pay')


CHOOSE CASE TRUE
	CASE li_preacc_gross_pay_base_year <> li_preacc_gross_pay_base_year_NEW
		ls_message = 'Year of Earnings'
		
	CASE ls_preacc_earn_freq_code <> ls_preacc_earn_freq_code_NEW
		ls_message = 'Frequency (Pre-Accident Earnings)'
	CASE ls_avg_earn_freq_code <> ls_avg_earn_freq_code_NEW
		ls_message = 'Frequency (Average Earnings)'
	CASE ls_preacc_paid_cpp_flag <> ls_preacc_paid_cpp_flag_NEW
		ls_message = 'Deduct CPP Flag (Pre-Accident Earnings)'
	CASE ls_avg_paid_cpp_flag <> ls_avg_paid_cpp_flag_NEW
		ls_message = 'Deduct CPP Flag (Average Earnings)'
	CASE ls_preacc_paid_uic_flag <> ls_preacc_paid_uic_flag_NEW
		ls_message = 'Deduct UIC Flag (Pre-Accident Earnings)'
	CASE ls_avg_paid_uic_flag <> ls_avg_paid_uic_flag_NEW
		ls_message = 'Deduct UIC Flag (Average Earnings)'
		
	CASE ldec_preacc_gross_pay <> ldec_preacc_gross_pay_NEW
		ls_message = 'Gross (Pre-Accident Earnings)'
	CASE ldec_avg_gross_pay <> ldec_avg_gross_pay_NEW
		ls_message = 'Gross (Average Earnings)'
	CASE ldec_preacc_indexed_gross_pay <> ldec_preacc_indexed_gross_pay_NEW
		ls_message = 'Indexed Gross (Pre-Accident Earnings)'
	CASE ldec_avg_indexed_gross_pay <> ldec_avg_indexed_gross_pay_NEW
		ls_message = 'Indexed Gross (Average Earnings)'
	CASE ldec_preacc_net_pay <> ldec_preacc_net_pay_NEW
		ls_message = ls_st_award_frequency_text + ' Net (Pre-Accident Earnings)'
	CASE ldec_avg_net_pay <> ldec_avg_net_pay_NEW
		ls_message = ls_st_award_frequency_text + ' Net (Average Earnings)'
		
END CHOOSE

RETURN ls_message

end function

private function string wf_verify_cppd_deduction (ref datastore ads_benefit_calculation_cppd);INTEGER		li_cppd_base_year , li_cppd_base_year_NEW

DECIMAL		ldec_cppd_monthly_amount , ldec_cppd_indexed_monthly_amount

DECIMAL		ldec_cppd_monthly_amount_NEW , ldec_cppd_indexed_monthly_amount_NEW

STRING		ls_message


li_cppd_base_year								= ads_benefit_calculation_cppd.GetItemNumber(1,'cppd_base_year')
ldec_cppd_monthly_amount						= ads_benefit_calculation_cppd.GetItemDecimal(1,'cppd_monthly_amount')
ldec_cppd_indexed_monthly_amount			= ads_benefit_calculation_cppd.GetItemDecimal(1,'cppd_indexed_monthly_amount')

dw_benefit_calculation_cppd.AcceptText()

li_cppd_base_year_NEW							= dw_benefit_calculation_cppd.GetItemNumber(1,'cppd_base_year')
ldec_cppd_monthly_amount_NEW				= dw_benefit_calculation_cppd.GetItemDecimal(1,'cppd_monthly_amount')
ldec_cppd_indexed_monthly_amount_NEW	= dw_benefit_calculation_cppd.GetItemDecimal(1,'cppd_indexed_monthly_amount')


CHOOSE CASE TRUE
	CASE li_cppd_base_year <> li_cppd_base_year_NEW AND li_cppd_base_year <> 0
		// if the copied regular had 0 for cppd base year, then it's OK, as long as
		// there is not an amount in either of the two columns below
		ls_message = 'Base Year (CPPD Deduction)'
	CASE ldec_cppd_monthly_amount <> ldec_cppd_monthly_amount_NEW
		ls_message = 'Monthly Amount (CPPD Deduction)'
	CASE ldec_cppd_indexed_monthly_amount <> ldec_cppd_indexed_monthly_amount_NEW
		ls_message = 'Indexed Monthly (CPPD Deduction)'
		
END CHOOSE

RETURN ls_message

end function

private function string wf_verify_regular_deductions (ref datastore ads_benefit_calculation_deductions, integer ai_deduction_count);STRING		ls_benefit_calc_deduction_code , ls_deduction_earn_freq_code

STRING		ls_benefit_calc_deduction_code_NEW , ls_deduction_earn_freq_code_NEW

DECIMAL		ldec_deduction_amount , ldec_deduction_amount_NEW

STRING		ls_message
INTEGER		li_counter


FOR li_counter = 1 TO ai_deduction_count
	ls_benefit_calc_deduction_code			= ads_benefit_calculation_deductions.GetItemString(li_counter,'benefit_calc_deduction_code')
	ls_deduction_earn_freq_code				= ads_benefit_calculation_deductions.GetItemString(li_counter,'deduction_earn_freq_code')
	
	ldec_deduction_amount						= ads_benefit_calculation_deductions.GetItemDecimal(li_counter,'deduction_amount')
	
	dw_benefit_calculation_deductions.AcceptText()
	
	ls_benefit_calc_deduction_code_NEW	= dw_benefit_calculation_deductions.GetItemString(li_counter,'benefit_calc_deduction_code')
	ls_deduction_earn_freq_code_NEW		= dw_benefit_calculation_deductions.GetItemString(li_counter,'deduction_earn_freq_code')
	
	ldec_deduction_amount_NEW				= dw_benefit_calculation_deductions.GetItemDecimal(li_counter,'deduction_amount')
	
	
	CHOOSE CASE TRUE
		CASE ls_benefit_calc_deduction_code <> ls_benefit_calc_deduction_code_NEW
			ls_message = 'Deduction Type (Regular Deductions)'
		CASE ls_deduction_earn_freq_code <> ls_deduction_earn_freq_code_NEW
			ls_message = 'Frequency (Regular Deductions)'
		CASE ldec_deduction_amount <> ldec_deduction_amount_NEW
			ls_message = 'Deduction Amount (Regular Deductions)'
			
	END CHOOSE
NEXT

RETURN ls_message

end function

public subroutine wf_set_rtw_capable ();DECIMAL		ldec_prev_year_capable
STRING		ls_prev_yrs_capable_type_code , ls_prev_yrs_capable_pre93_capable_cpp_flag , ls_prev_yrs_capable_pre93_capable_uic_flag

// set capable data, disable fields
// ids_rtw_incentive_linked_bencalcs is retrieved in wf_check_RTW_bus_rules call above
// but if no rows are retrieved then this row will never be reached.
ldec_prev_year_capable = ids_rtw_incentive_linked_bencalcs.GetItemDecimal(1,'prev_yrs_capable_pre93_capable_gross_pay')
ls_prev_yrs_capable_type_code = ids_rtw_incentive_linked_bencalcs.GetItemString(1,'prev_yrs_capable_pre93_capable_earn_type_code')
ls_prev_yrs_capable_pre93_capable_cpp_flag = ids_rtw_incentive_linked_bencalcs.GetItemString(1,'prev_yrs_capable_pre93_capable_cpp_flag')
ls_prev_yrs_capable_pre93_capable_uic_flag = ids_rtw_incentive_linked_bencalcs.GetItemString(1,'prev_yrs_capable_pre93_capable_uic_flag')


dw_benefit_calculation_capable_data.SetItem(1, 'pre93_capable_gross_pay',ldec_prev_year_capable)
IF ldec_prev_year_capable <> 0 THEN
	dw_benefit_calculation_capable_data.SetItem(1, 'pre93_capable_earn_type_code',ls_prev_yrs_capable_type_code)
	dw_benefit_calculation_capable_data.SetItem(1, 'pre93_capable_cpp_flag',ls_prev_yrs_capable_pre93_capable_cpp_flag)
	dw_benefit_calculation_capable_data.SetItem(1, 'pre93_capable_uic_flag',ls_prev_yrs_capable_pre93_capable_uic_flag)
ELSE
	// if capable from previous year was zero, 
	// then do not clear the codes & flags
	// when the save button is clicked, the zero amount capable 
	// will not be saved. This will prevent an app error if the user
	// unclicks RTW checkbox & enters an amount
END IF

dw_benefit_calculation_capable_data.Enabled = FALSE
end subroutine

event open;call super::open;Long     ll_rownum, ll_cntr, ll_result, ll_retrieve_no, ll_null, ll_rowcount, ll_copy_benefit_calculation_no
Integer  li_tax_index_year, li_rtn, ll_dw_frea_bene_no
Boolean  lb_new_ben_calc, lb_renumber_remun_no
String   lsa_tabnames[5], ls_mod_string, ls_transitional_claim_flag, ls_top_up_flag, ls_opening_type_code
Decimal  ldec_td1_basic_exemption, ldec_prov_basic_exemption, ldec_td1_married_exemption, ldec_prov_married_exemption
Date     ldt_null, ldt_85%_effective_date, ldt_index_date, ldt_effective_date
Datetime ldtm_yearly_factor, ldt_frea_create_date, ldtm_benefit_start_date

//***********************************************************
// Benefit Calculation Parameters stored in s_window_message
//
// Claim No								al_doubleparm[1]
// Opening No							al_doubleparm[2]
// Benefit Calculation No			al_doubleparm[3]
// Copy Benefit Calculation No	al_doubleparm[4]
// Copy BenCalc Opening No			al_doubleparm[5]
//
// Transitional Claim Flag			as_stringparm[1]
// Top-up Flag							as_stringparm[2]
// Opening Type Code					as_stringparm[3]
//
// Accident Recurrence Date		adtm_datetimeparm[1]
// Benefit Start Date				adtm_datetimeparm[2]
// Effective Date						adtm_datetimeparm[3]
//
// Read/Update Mode					as_mode 
//*********************************************************
Setpointer(Arrow!)

istr_bencalc_parameters = Message.PowerObjectParm
il_claim_no =                    istr_bencalc_parameters.al_doubleparm[1]
il_opening_no =                  istr_bencalc_parameters.al_doubleparm[2]
il_benefit_calculation_no =      istr_bencalc_parameters.al_doubleparm[3]
ll_copy_benefit_calculation_no = istr_bencalc_parameters.al_doubleparm[4]

ls_transitional_claim_flag =     istr_bencalc_parameters.as_stringparm[1]
ls_top_up_flag =                 istr_bencalc_parameters.as_stringparm[2]
ls_opening_type_code =           istr_bencalc_parameters.as_stringparm[3]

idtm_accident_recurrence_date =  istr_bencalc_parameters.adtm_datetimeparm[1]
ldtm_benefit_start_date =        istr_bencalc_parameters.adtm_datetimeparm[2]
ldt_effective_date =             Date(istr_bencalc_parameters.adtm_datetimeparm[3])

idt_server_date = Date(f_server_datetime())

SetNull(ll_null)
SetNull(ldt_null)

//---------------------
// Database Connections
//---------------------
dw_benefit_calculation_details.SetTransObject(SQLCA)
dw_benefit_calculation_deductions.SetTransObject(SQLCA)
dw_benefit_calculation_capable_data.SetTransObject(SQLCA)
dw_benefit_calculation_remunerations.SetTransObject(SQLCA)
dw_benefit_calculation_frozen_data.SetTransObject(SQLCA)

li_rtn = dw_benefit_calculation_details.ShareData(dw_benefit_calculation_preaccident_data)
li_rtn = dw_benefit_calculation_details.ShareData(dw_benefit_calculation_cppd)

//---------------------------------------------------
// Populate the tab control and display the first tab
//---------------------------------------------------
lsa_tabnames[1] = "Pre-Accident"
lsa_tabnames[2] = "Capable"
lsa_tabnames[3] = "Deductions"
lsa_tabnames[4] = "Remunerations"
lsa_tabnames[5] = "Calculation"

uo_tabs.uf_settabs(lsa_tabnames)
dw_benefit_calculation_preaccident_data.visible =	TRUE
dw_benefit_calculation_frozen_data.visible = TRUE

// Determine if Calculation Details should be opened Read-Only
IF il_benefit_calculation_no <> 0 THEN
	ib_detail = TRUE
ELSE
	IF Upper(istr_bencalc_parameters.as_mode) = "READ" THEN
		wf_read_only()
	END IF
	ib_detail = FALSE
END IF

// Set the text of the Frozen 92 Static Text Object
IF NOT (isnull(idtm_accident_recurrence_date)) THEN
	IF Date(idtm_accident_recurrence_date) < Date('1993-01-01') THEN
		st_taxation_period.text = 'Taxation Period'

		IF Date(idtm_accident_recurrence_date) >=  Date('1992-07-01') THEN
			st_frozen_92_taxyear.text = '1992-07-01'
			idt_frozen_92_taxyear = Date('1992-07-01')
		ELSE
			st_frozen_92_taxyear.text = '1992-01-01'
			idt_frozen_92_taxyear = Date('1992-01-01')
		END IF 
	ELSE
		st_frozen_92_taxyear.text = ''
		st_taxation_period.text = ''
	END IF
END IF

// Create the n_benefit_calculations non-visual user object so that the calculation routines are accessible throughout this module.                                                                   
in_bencalcs = CREATE n_benefit_calculation

// The Calculation Details window is opened when a benefit calculation is being created or displayed    
// ADD: 	If the benefit calculation number is zero and the copy_benefit_calculation_no is zero,         
// 		we are adding a new benefit calculation award.            								              
//       NOTE: the Read-Only option of the Benefit Calculation can select the "ADD" option to allow the 
//             user to enter benefit calculations without being able to update the database             
// COPY:	If the copy_benefit_calculation_no is not zero, we are copying the selected benefit calculation
//			as the source of the new benefit calculation award 		   									   	  
// DETAILS: If the benefit calculation number is not zero, we are displaying a benefit calculation.	  

//	Insert a row into the calculation external datawindow
dw_benefit_calculation_display_computed.InsertRow(0)
ib_copy_ben_calc = FALSE

// Determine if this is a COPY of a ben calc  
IF il_benefit_calculation_no = 0 AND ll_copy_benefit_calculation_no <> 0 THEN
	ib_copy_ben_calc = TRUE
END IF

IF il_benefit_calculation_no = 0 AND ll_copy_benefit_calculation_no = 0 THEN
	// ADD: 	CREATE A NEW BENEFIT CALCULATION  
	lb_new_ben_calc = TRUE
	ib_new_ben_calc = lb_new_ben_calc
	ii_copy_tax_index_year = 0
	cb_add_earnings.Enabled = TRUE
	
	//	Insert new rows into the datawindows and enable them. 
	ll_rownum = dw_benefit_calculation_details.InsertRow(0)
	IF ll_rownum < 0 THEN	
		Error.Text = "Error inserting record"
		Error.WindowMenu = "w_calculation_details"
		Error.Object = "dw_benefit_calculation_details"
		Error.ObjectEvent = "open"
		SignalError()
		RETURN
	ELSE
		// Set values for opening type & number, create date & user
		dw_benefit_calculation_details.SetItem(1, 'opening_type_code', ls_opening_type_code)
		dw_benefit_calculation_details.SetItem(1, 'opening_no', il_opening_no)
		dw_benefit_calculation_details.SetItem(1, 'create_date', idt_server_date )
		dw_benefit_calculation_details.SetItem(1, 'create_user_id', vgst_user_profile.user_id)
		dw_benefit_calculation_details.SetItem(1, "transitional_claim_flag", ls_transitional_claim_flag)
		dw_benefit_calculation_details.SetItem(1, "top_up_flag", ls_top_up_flag)  
		dw_benefit_calculation_details.SetItem(1, 'copied_from_benefit_calc_no', 0) // added, not copied
	END IF

	// If the Benefit Calculation is not a Cost Analysis calculation, do not allow the user to enter a calculation note for the benefit calculation. 
	// These notes will be displayed in the Cost Analysis module to help the user in distinguishing which ben calc to use in their cost analysis.
	IF il_opening_no <> 0 THEN  
		dw_benefit_calculation_details.uf_protectcolumn("calculation_note", TRUE)
		dw_benefit_calculation_details.object.calculation_note.background.mode = 1
	END IF

	// Set the Benefit defaults. The award frequency depends on the Opening Type. LTD openings have a Monthly award frequency.	
	IF ls_opening_type_code = "RLOE" THEN   
		dw_benefit_calculation_details.SetItem(1, "award_freq_code", "W")
		dw_benefit_calculation_details.Modify("st_award_frequency.Text='Weekly'")
		dw_benefit_calculation_preaccident_data.Modify("st_award_frequency.Text='Weekly'")
		dw_benefit_calculation_capable_data.Modify("st_award_frequency.Text='Weekly'")
	ELSE
		IF ls_opening_type_code = "LTD" OR ls_opening_type_code = "SV"  OR ls_opening_type_code = "S1" OR ls_opening_type_code = "S2" OR ls_opening_type_code = "PEN" THEN
			dw_benefit_calculation_details.SetItem(1, "award_freq_code", "M")
			dw_benefit_calculation_details.Modify("st_award_frequency.Text='Monthly'")
			dw_benefit_calculation_preaccident_data.Modify("st_award_frequency.Text='Monthly'")
			dw_benefit_calculation_capable_data.Modify("st_award_frequency.Text='Monthly'")
		END IF
	END IF
	
	dw_benefit_calculation_details.Modify("st_accident_recurrence_date.Text='" + string(idtm_accident_recurrence_date,'yyyy-mm-dd') + "'")
		
	dw_benefit_calculation_deductions.InsertRow(0)
	dw_benefit_calculation_deductions.SetItem(1, "deduction_no", 1)
	
	dw_benefit_calculation_capable_data.InsertRow(0)
	dw_benefit_calculation_remunerations.InsertRow(0)
	dw_benefit_calculation_remunerations.SetItem(1, "remun_no", 1)
	
	// Determine if Frozen Data should be displayed. It is only displayed if the benefit calculation has the transitional claim flag set on. 
	// Set the benefit level to 90% for transitional claims and to 85% for RLOE or LTD benefits for a claim with an accident/recurrence prior to January 01, 1993. 
	// All others, set the benefit level to 80%. Prior to January 01, 1993 benefits for RLOE and LTD began at 85%.
	IF ls_transitional_claim_flag = "Y" THEN   
		dw_benefit_calculation_frozen_data.InsertRow(0)
		dw_benefit_calculation_frozen_data.SetItem(1, 'frozen_cpp_flag', 'Y')
		dw_benefit_calculation_frozen_data.SetItem(1, 'frozen_uic_flag', 'Y')
	END IF
	
	IF ls_opening_type_code = "PEN" OR ls_opening_type_code = "SV" OR ls_opening_type_code = "S1" OR ls_opening_type_code = "S2" THEN
		dw_benefit_calculation_details.SetItem(1, "benefit_level_percentage", 1.0)
	ELSE
		IF ls_transitional_claim_flag = "Y" THEN   
			dw_benefit_calculation_details.SetItem(1, "benefit_level_percentage", .90)	
		ELSEIF date(idtm_accident_recurrence_date) < date('1993-01-01') THEN
			dw_benefit_calculation_details.SetItem(1, "benefit_level_percentage", .90)
		ELSE
			IF (ls_opening_type_code = "RLOE" OR ls_opening_type_code = "LTD") THEN 
			ELSE
				dw_benefit_calculation_details.SetItem(1, "benefit_level_percentage",.80)
			END IF
		END IF
	END IF
	dw_benefit_calculation_details.SetItem(1, "calculation_reason_code", "I")
ELSE
	// COPY:		CREATE A NEW BENEFIT CALCULCATION FROM AN EXISTING BENEFIT CALCULCATION  
	// or
	// DETAILS:	DISPLAY AN EXISTING BENEFIT CALCULCATION  
	IF ll_copy_benefit_calculation_no <> 0 THEN    
		ll_retrieve_no = ll_copy_benefit_calculation_no
		lb_new_ben_calc = TRUE
		ii_release = in_bencalcs.f_get_release_no(ll_retrieve_no, il_claim_no, ldtm_yearly_factor)
		cb_add_earnings.Enabled = TRUE
	ELSE
		ll_retrieve_no = il_benefit_calculation_no
		ii_release = in_bencalcs.f_get_release_no(ll_retrieve_no, il_claim_no, ldtm_yearly_factor)
		in_bencalcs.idt_from_yearly_factor = ldtm_yearly_factor
		
		IF Date(ldtm_yearly_factor) < Date('1982-01-01') THEN
			MessageBox('Missing Information', 'This Benefit Calculation will not be displayed properly because we do not have the Yearly Factor information for that year.' + &
														 '~r~nThe Taxation Period is prior to the earliest on-line taxation year of 1982.', Information!)
		END IF

		lb_new_ben_calc = FALSE
		st_title.Text = "Display Benefit Calculation"
		dw_benefit_calculation_capable_data.uf_set_backcolor()
		dw_benefit_calculation_cppd.uf_set_backcolor()
		dw_benefit_calculation_deductions.uf_set_backcolor()
		dw_benefit_calculation_details.uf_set_backcolor()
		dw_benefit_calculation_details.Modify("rtw_incentive_flag.CheckBox.3D=No") // makes checkbox look disabled
		dw_benefit_calculation_display_computed.uf_set_backcolor()
		dw_benefit_calculation_frozen_data.uf_set_backcolor()
		dw_benefit_calculation_preaccident_data.uf_set_backcolor()
		dw_benefit_calculation_remunerations.uf_set_backcolor()
		cb_add_earnings.Enabled = FALSE
	END IF

	in_bencalcs.f_set_release_no(ii_release)

	//	Retrieve the benefit calculation details 
	ib_copy_first= TRUE
	ll_rownum = dw_benefit_calculation_details.Retrieve(il_claim_no, ll_retrieve_no)
	li_rtn = SQLCA.nf_handle_error("w_calculation_details", "", "open - dw_benefit_calculation_details.Retrieve(il_claim_no, ll_retrieve_no)")
	IF li_rtn < 0 THEN 
		Close(This)
		RETURN
	END IF
	IF ll_rownum <= 0 THEN
		MessageBox("Benefit Calculation Module - Data Integrity Error", "Details not found during retrieve of Benefit calculation number " + String(il_benefit_calculation_no), StopSign!)
		Close(This)
		RETURN
	END IF

	// If the benefit level of the copied benefit calculation is 80% for a RLOE or LTD benefit and the claim has an accident/recurrence prior to January 01, 1993, 
	// set the benefit level to 85% as 80% is invalid for these conditions. (The original code did not have this validation and thus, some could exist).
	IF (ls_opening_type_code = "RLOE" OR ls_opening_type_code = "LTD") AND Date(idtm_accident_recurrence_date) < date('1993-01-01') AND dw_benefit_calculation_details.GetItemDecimal(1, "benefit_level_percentage") = .80 THEN
		dw_benefit_calculation_details.SetItem(1, "benefit_level_percentage",.85)
	END IF
	idec_copy_td1_exemption_amount = dw_benefit_calculation_details.GetItemDecimal(1, "td1_exemption_amount")
	idec_copy_prov_td1_exemption = dw_benefit_calculation_details.GetItemDecimal(1, "prov_td1_exemption_amount")	//sr64			
	IF dw_benefit_calculation_details.GetItemString(ll_rownum, "award_freq_code") = "W" THEN
		dw_benefit_calculation_details.Modify("st_award_frequency.Text='Weekly'")
		dw_benefit_calculation_preaccident_data.Modify("st_award_frequency.Text='Weekly'")		
		dw_benefit_calculation_capable_data.Modify("st_award_frequency.Text='Weekly'")
	ELSE
		dw_benefit_calculation_details.Modify("st_award_frequency.Text='Monthly'")
		dw_benefit_calculation_preaccident_data.Modify("st_award_frequency.Text='Monthly'")
		dw_benefit_calculation_capable_data.Modify("st_award_frequency.Text='Monthly'")
	END IF

	// If this is a copied ben calc, then set the create_date to null to ensure calculation of the CPP and tax is correct. Also... P10261 RTW Incentive - populate copied_from_benefit_calc_no
	IF ll_copy_benefit_calculation_no <> 0 THEN
		dw_benefit_calculation_details.SetItem(1, "create_date", ldt_null)
		dw_benefit_calculation_details.SetItem(1, "create_user_id", vgst_user_profile.user_id)
		dw_benefit_calculation_details.SetItem(1, 'copied_from_benefit_calc_no', ll_retrieve_no)
		dw_benefit_calculation_details.SetItem(1, 'opening_no', il_opening_no)
	END IF

	//	Retrieve the dw_benefit_calculation_capable_data
	ll_rownum = dw_benefit_calculation_capable_data.Retrieve(il_claim_no, ll_retrieve_no)
	li_rtn = SQLCA.nf_handle_error("w_calculation_details", "", "open - dw_benefit_calculation_capable_data.Retrieve(il_claim_no,ll_retrieve_no)")
	IF li_rtn < 0 THEN 
		Close(This)
		RETURN
	END IF

	IF lb_new_ben_calc AND not ll_rownum > 0 THEN
		dw_benefit_calculation_capable_data.InsertRow(0)
	END IF

	//	Retrieve the dw_benefit_calculation_remunerations
	ll_rownum = dw_benefit_calculation_remunerations.Retrieve(il_claim_no, ll_retrieve_no)
	li_rtn = SQLCA.nf_handle_error("w_calculation_details", "", "open - dw_benefit_calculation_remunerations.Retrieve(il_claim_no, ll_retrieve_no)")
	IF li_rtn < 0 THEN 
		Close(This)
		RETURN
	END IF

	IF lb_new_ben_calc AND not ll_rownum > 0 THEN
		dw_benefit_calculation_remunerations.InsertRow(0)
		dw_benefit_calculation_remunerations.SetItem(1, "remun_no", 1)
	END IF

	//	Retrieve the dw_benefit_calculation_deductions 
	ll_rownum = dw_benefit_calculation_deductions.Retrieve(il_claim_no, ll_retrieve_no)
	li_rtn = SQLCA.nf_handle_error("w_calculation_details", "", "open - dw_benefit_calculation_deductions.Retrieve(il_claim_no, ll_retrieve_no)")
	IF li_rtn < 0 THEN 
		Close(This)
		RETURN
	END IF

	IF lb_new_ben_calc AND not ll_rownum > 0 THEN
		dw_benefit_calculation_deductions.InsertRow(0)
		dw_benefit_calculation_deductions.SetItem(1, "deduction_no", 1)
	END IF

	// If the benefit calculation is a transitional claim  THEN retrieve the frozen data
	is_frozen_exists = "N"
	IF ls_transitional_claim_flag = "Y" THEN
		ll_rownum = dw_benefit_calculation_frozen_data.Retrieve(il_claim_no, ll_retrieve_no)
		li_rtn = SQLCA.nf_handle_error("w_calculation_details", "", "open - dw_benefit_calculation_frozen_data.Retrieve(il_claim_no,ll_retrieve_no)")
		IF li_rtn < 0 THEN 
			Close(This)
			RETURN
		END IF
		IF ll_rownum <= 0 THEN
			MessageBox("Benefit Calculation Module - Data Integrity Error", "Error retrieving frozen earnings data ~r~nClaim was a transitional claim when benefit calculation was created", StopSign!)
			Close(This)
			RETURN
		ELSE
			is_frozen_exists = "Y"
		END IF
		
		// sr67 change start Since some of the claims have null values in columns that the Frozen Earnings datawindow is using, The label's that display the 
		//      frozen Earnings pre 93 tax rates will be displayed only for Frozen Earning 	calculations that have been created after December 20 2000
		ll_dw_frea_bene_no = dw_benefit_calculation_frozen_data.GetItemNumber(1, "benefit_calculation_no")
		
		SELECT create_date
		  INTO :ldt_frea_create_date
		  FROM FROZEN_EARNING
		 WHERE claim_no = :il_claim_no
		   AND benefit_calculation_no = :ll_dw_frea_bene_no
		 USING SQLCA ; 

		li_rtn = SQLCA.nf_handle_error('w_calculation_details', '', 'open - SELECT create_date FROM FROZEN_EARNING WHERE claim_no = :il_claim_no AND benefit_calculation_no = :ll_dw_frea_bene_no')
		IF li_rtn < 0 THEN
			st_taxation_period.Text = ""
			st_frozen_92_taxyear.Text = ""	
			st_taxation_period.BringToTop = FALSE
			st_frozen_92_taxyear.BringToTop = FALSE
		ELSE
			IF Date(ldt_frea_create_date) >= Date('2000/12/20') THEN
				st_taxation_period.BringToTop = TRUE
				st_frozen_92_taxyear.BringToTop = TRUE	
			END IF
		END IF
	END IF

	IF lb_new_ben_calc THEN
		dw_benefit_calculation_details.SetItem(1, "claim_no", ll_null)
		dw_benefit_calculation_details.SetItem(1, "benefit_calculation_no", ll_null)
		dw_benefit_calculation_details.SetItem(1, "opening_no", ll_null)
		dw_benefit_calculation_details.SetItem(1, "effective_from_date", ldt_null)
		
		// Determine the Tax Index Year for the effective date of the benefit calculation that was copied. This will be used
		//	to determine if the TD1 basic Exmeption amount copied in is still applicable for the new effective date
		wf_determine_tax_index_year(Date(idtm_accident_recurrence_date), ldt_effective_date, ii_copy_tax_index_year)

		IF dw_benefit_calculation_details.GetItemDecimal(1, "benefit_level_percentage") = .80 AND ls_transitional_claim_flag <> "Y" THEN
			// Determine if the tax information is available to adjust to 85% and if so, give the user the option of adjusting.
			// The Tax index Year is determined from the Accident/Recurrence Date and the effective date (which is defaulted
			// to 40 weeks beyond the benefit start date as this is when 85% benefits can begin)
			ldt_85%_effective_date = RelativeDate(Date(ldtm_benefit_start_date), 273)
			wf_determine_tax_index_year(Date(idtm_accident_recurrence_date), ldt_85%_effective_date, li_tax_index_year)
			ldt_index_date = Date(li_tax_index_year, Month(ldt_85%_effective_date), day(ldt_85%_effective_date))

			IF in_bencalcs.f_isvalid_tax_year(ldt_index_date) THEN
				IF MessageBox("Benefit Calculation Module", "Do you want the calculation adjusted to 85%",Question!,YesNo!) = 1 THEN
					dw_benefit_calculation_details.SetItem(1, "effective_from_date", ldt_85%_effective_date)
					dw_benefit_calculation_details.SetItem(1, "index_taxation_year", li_tax_index_year)
					dw_benefit_calculation_details.SetItem(1, "benefit_level_percentage", .85)
					dw_benefit_calculation_details.SetItem(1, "calculation_reason_code", "J")

					// Yearly_Factor table has changed - P10105 function below has become obsolete as these values are now properties of the NVO 
					// and the Yearly_Factor table is Period based and not Yearly based - we now pass the ldt_index_date as opposed to index_year
					ll_result = in_bencalcs.f_get_exemption_amounts(ldt_index_date, ldec_td1_basic_exemption, ldec_td1_married_exemption, ldec_prov_basic_exemption, ldec_prov_married_exemption)
					IF ll_result = -1 THEN
						Close(this)
						RETURN
					END IF
					ls_mod_string = "td1_exemption_amount.Values='" + string(ldec_td1_basic_exemption,'$#,##0.00') + "~t" + String(ldec_td1_basic_exemption) +&
										 "/" + String(ldec_td1_married_exemption,'$#,##0.00') + "~t" + String(ldec_td1_married_exemption) + "'"
					dw_benefit_calculation_details.Modify(ls_mod_string)

					// SR64 - Jan. 2001 Tax Changes - new listbox control added to display new provincial TD1 exemption levels
					ls_mod_string = "prov_td1_exemption_amount.values='" + string(ldec_prov_basic_exemption,'$#,##0.00') + "~t" + &
											string(ldec_prov_basic_exemption) + "/" + string(ldec_prov_married_exemption,'$#,##0.00') + "~t" + string(ldec_prov_married_exemption) + "'"
					dw_benefit_calculation_details.Modify(ls_mod_string)
				END IF
			END IF
		END IF

		dw_benefit_calculation_details.SetItemStatus(1, 0, Primary!, NewModIFied!)

		dw_benefit_calculation_capable_data.SetItem(1, "claim_no", ll_null)
		dw_benefit_calculation_capable_data.SetItem(1, "benefit_calculation_no", ll_null)
		dw_benefit_calculation_capable_data.SetItemStatus(1, 0, Primary!, NewModIFied!)

		IF dw_benefit_calculation_frozen_data.RowCount() > 0 THEN
			dw_benefit_calculation_frozen_data.SetItem(1, "claim_no", ll_null)
			dw_benefit_calculation_frozen_data.SetItem(1, "benefit_calculation_no", ll_null)
			dw_benefit_calculation_frozen_data.SetItemStatus(1, 0, Primary!, NewModIFied!)
		END IF

		ll_cntr = 1
		ll_rowcount = dw_benefit_calculation_remunerations.RowCount()
		DO WHILE ll_cntr <= ll_rowcount
			dw_benefit_calculation_remunerations.SetItem(ll_cntr, "claim_no", ll_null)
			dw_benefit_calculation_remunerations.SetItem(ll_cntr, "benefit_calculation_no", ll_null)
			dw_benefit_calculation_remunerations.SetItemStatus(ll_cntr, 0, Primary!, NewModified!)
			ll_cntr ++
		LOOP

		ll_cntr = 1
		ll_rowcount = dw_benefit_calculation_deductions.RowCount()
		DO WHILE ll_cntr <= ll_rowcount
			dw_benefit_calculation_deductions.SetItem(1, "claim_no", ll_null)
			dw_benefit_calculation_deductions.SetItem(1, "benefit_calculation_no", ll_null)
			dw_benefit_calculation_deductions.SetItemStatus(ll_cntr, 0, Primary!, NewModified!)
			ll_cntr++
		LOOP
	END IF

	wf_recalculate_all()
END IF


IF lb_new_ben_calc THEN
	dw_benefit_calculation_details.Enabled = TRUE
	dw_benefit_calculation_preaccident_data.Enabled = TRUE
	dw_benefit_calculation_cppd.Enabled = TRUE
	dw_benefit_calculation_frozen_data.Enabled = TRUE
	dw_benefit_calculation_capable_data.Enabled = TRUE
	dw_benefit_calculation_remunerations.Enabled = TRUE
	dw_benefit_calculation_deductions.Enabled = TRUE

	IF Upper(istr_bencalc_parameters.as_mode) = "READ"  THEN
		cb_cancel.Enabled =	FALSE
		cb_save.Enabled =	FALSE
		cb_list_bencalcs.Enabled =	TRUE
		cb_close.Enabled = TRUE
		st_title.Text = "Create Benefit Calculation (Read-Only)"
	ELSE
		cb_cancel.Enabled = TRUE
		cb_save.Enabled = TRUE
		cb_list_bencalcs.Enabled = FALSE
		cb_close.Enabled = FALSE
		st_title.Text = "Create Benefit Calculation"
	END IF

	cb_delete.Enabled = FALSE
ELSE
	IF upper(istr_bencalc_parameters.as_mode) <> "READ"  THEN
		cb_delete.Enabled = TRUE
	END IF
	dw_benefit_calculation_details.Enabled = TRUE
	dw_benefit_calculation_details.uf_protect_allattributes(TRUE)
	dw_benefit_calculation_preaccident_data.Enabled	= TRUE
	dw_benefit_calculation_preaccident_data.uf_protect_allattributes(TRUE)
	dw_benefit_calculation_cppd.Enabled = TRUE
	dw_benefit_calculation_cppd.uf_protect_allattributes(TRUE)
	dw_benefit_calculation_frozen_data.Enabled = TRUE
	dw_benefit_calculation_frozen_data.uf_protect_allattributes(TRUE)
	dw_benefit_calculation_capable_data.Enabled = TRUE
	dw_benefit_calculation_capable_data.uf_protect_allattributes(TRUE)
	dw_benefit_calculation_remunerations.Enabled = TRUE
	dw_benefit_calculation_remunerations.uf_protect_allattributes(TRUE)
	dw_benefit_calculation_deductions.Enabled = TRUE
	dw_benefit_calculation_deductions.uf_protect_allattributes(TRUE)
END IF

// RTW checkbox should only be visible for LTD openings
IF ls_opening_type_code = "LTD" THEN
	dw_benefit_calculation_details.Modify("rtw_incentive_flag.Visible='1'")
	ids_rtw_incentive_linked_bencalcs = Create U_DS
	ids_rtw_incentive_linked_bencalcs.DataObject = 'ds_rtw_incentive_linked_bencalcs'
	ids_rtw_incentive_linked_bencalcs.SetTransObject(SQLCA)
ELSE
	dw_benefit_calculation_details.Modify("rtw_incentive_flag.Visible='0'")
END IF

dw_benefit_calculation_details.Modify("st_accident_recurrence_date.Text='" + string(idtm_accident_recurrence_date,'yyyy-mm-dd') + "'")

// Set focus on the Calculation Details 
dw_benefit_calculation_details.SetFocus()

end event

on w_calculation_details.create
int iCurrent
call super::create
this.gb_cppd_deductions=create gb_cppd_deductions
this.cb_list_bencalcs=create cb_list_bencalcs
this.cb_save=create cb_save
this.cb_cancel=create cb_cancel
this.cb_delete=create cb_delete
this.st_window_title=create st_window_title
this.dw_benefit_calculation_details=create dw_benefit_calculation_details
this.cb_next=create cb_next
this.cb_prev=create cb_prev
this.st_taxation_period=create st_taxation_period
this.st_frozen_92_taxyear=create st_frozen_92_taxyear
this.cb_add_earnings=create cb_add_earnings
this.dw_benefit_calculation_capable_data=create dw_benefit_calculation_capable_data
this.dw_benefit_calculation_display_computed=create dw_benefit_calculation_display_computed
this.dw_benefit_calculation_deductions=create dw_benefit_calculation_deductions
this.dw_benefit_calculation_preaccident_data=create dw_benefit_calculation_preaccident_data
this.dw_benefit_calculation_remunerations=create dw_benefit_calculation_remunerations
this.cb_delete_remuneration=create cb_delete_remuneration
this.gb_regular_deductions=create gb_regular_deductions
this.dw_benefit_calculation_frozen_data=create dw_benefit_calculation_frozen_data
this.uo_tabs=create uo_tabs
this.dw_benefit_calculation_cppd=create dw_benefit_calculation_cppd
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.gb_cppd_deductions
this.Control[iCurrent+2]=this.cb_list_bencalcs
this.Control[iCurrent+3]=this.cb_save
this.Control[iCurrent+4]=this.cb_cancel
this.Control[iCurrent+5]=this.cb_delete
this.Control[iCurrent+6]=this.st_window_title
this.Control[iCurrent+7]=this.dw_benefit_calculation_details
this.Control[iCurrent+8]=this.cb_next
this.Control[iCurrent+9]=this.cb_prev
this.Control[iCurrent+10]=this.st_taxation_period
this.Control[iCurrent+11]=this.st_frozen_92_taxyear
this.Control[iCurrent+12]=this.cb_add_earnings
this.Control[iCurrent+13]=this.dw_benefit_calculation_capable_data
this.Control[iCurrent+14]=this.dw_benefit_calculation_display_computed
this.Control[iCurrent+15]=this.dw_benefit_calculation_deductions
this.Control[iCurrent+16]=this.dw_benefit_calculation_preaccident_data
this.Control[iCurrent+17]=this.dw_benefit_calculation_remunerations
this.Control[iCurrent+18]=this.cb_delete_remuneration
this.Control[iCurrent+19]=this.gb_regular_deductions
this.Control[iCurrent+20]=this.dw_benefit_calculation_frozen_data
this.Control[iCurrent+21]=this.uo_tabs
this.Control[iCurrent+22]=this.dw_benefit_calculation_cppd
end on

on w_calculation_details.destroy
call super::destroy
destroy(this.gb_cppd_deductions)
destroy(this.cb_list_bencalcs)
destroy(this.cb_save)
destroy(this.cb_cancel)
destroy(this.cb_delete)
destroy(this.st_window_title)
destroy(this.dw_benefit_calculation_details)
destroy(this.cb_next)
destroy(this.cb_prev)
destroy(this.st_taxation_period)
destroy(this.st_frozen_92_taxyear)
destroy(this.cb_add_earnings)
destroy(this.dw_benefit_calculation_capable_data)
destroy(this.dw_benefit_calculation_display_computed)
destroy(this.dw_benefit_calculation_deductions)
destroy(this.dw_benefit_calculation_preaccident_data)
destroy(this.dw_benefit_calculation_remunerations)
destroy(this.cb_delete_remuneration)
destroy(this.gb_regular_deductions)
destroy(this.dw_benefit_calculation_frozen_data)
destroy(this.uo_tabs)
destroy(this.dw_benefit_calculation_cppd)
end on

event activate;call super::activate;dw_benefit_calculation_details.setfocus()
dw_benefit_calculation_details.SetColumn("effective_from_date")

end event

type st_title from w_a_tool`st_title within w_calculation_details
integer x = 0
integer y = 0
integer width = 2674
string text = " "
end type

type cb_close from w_a_tool`cb_close within w_calculation_details
integer y = 1728
integer width = 352
integer height = 96
integer taborder = 10
end type

type gb_cppd_deductions from groupbox within w_calculation_details
integer x = 82
integer y = 896
integer width = 1070
integer height = 804
integer taborder = 90
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "CPPD Deductions"
end type

type cb_list_bencalcs from commandbutton within w_calculation_details
integer x = 1856
integer y = 1728
integer width = 352
integer height = 96
integer taborder = 190
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "L&ist"
end type

event clicked;W_SHEET	lwi_active_sheet


/* ***********************************************************
	Benefit Calculation Parameters stored in s_window_message

	Claim No								al_doubleparm[1]
	Opening No							al_doubleparm[2]
	Benefit Calculation No			al_doubleparm[3]
	Copy Benefit Calculation No	al_doubleparm[4]

	Transitional Claim Flag			as_stringparm[1]
	Top-up Flag							as_stringparm[2]
	Opening Type Code					as_stringparm[3]
		
	Accident Recurrence Date		adtm_datetimeparm[1]
	Benefit Start Date				adtm_datetimeparm[2]

	Read/Update Mode					as_mode 

	**********************************************************
*/


lwi_active_sheet = w_frame.GetActiveSheet()

istr_bencalc_parameters.as_stringparm[4] = 'w_calculation_details'

OpenWithParm (lwi_active_sheet.iw_benefits,istr_bencalc_parameters,lwi_active_sheet)

IF IsValid (lwi_active_sheet.iw_benefits) THEN
	lwi_active_sheet.iw_benefits.Show()	
END IF

cb_close.TriggerEvent(Clicked!)

end event

type cb_save from commandbutton within w_calculation_details
event ue_save pbm_custom01
integer x = 763
integer y = 1728
integer width = 352
integer height = 96
integer taborder = 170
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Save"
end type

event clicked;String     ls_transitional_claim_flag , ls_deduction_earn_freq_code, ls_pre93_capable_earn_type_code, ls_pre93_capable_earn_freq_code
String     ls_opening_type_code, ls_exemption_mismatch, ls_remun_type_code, ls_remun_earn_freq_code 
String     ls_find_capable, ls_find_remuneration, ls_pre93_capable_uic_flag, ls_pre93_capable_cpp_flag
Long       ll_cntr, ll_recnmbr, ll_result, ll_find_deduction , ll_current_yrs_copied_bencalc_no
Integer    li_count, li_year, li_ltd_benefit_calc_no, li_copied_from_effective_year, li_prior_copied_bencalc_no
Integer    li_found_row, li_found_row_2, li_remun_rowcount, li_counter, li_rtn
Decimal{2} ldec_td1_basic_exemption, ldec_td1_married_exemption, ldec_prov_basic_exemption,ldec_prov_married_exemption
Decimal{2} ldec_fed_basic, ldec_fed_married, ldec_reg_ltd_award_amount
Decimal    ldec_amount, ldec_regular_award_amount, ldec_transitional_award_amount, ldec_benefit_level_percentage, ldec_fed_exemption_entered, ldec_prov_exemption_entered
Decimal    ldec_pre93_capable_gross_pay, ldec_remun_gross_pay, ldec_min_capable_earn_percent_increase
Decimal    ldec_rtw_incentive_capable_earnings, ldec_prev_years_capable_earnings, ldec_ltd_pre93_capable_gross_pay
Date       ldt_effective_from_date
Datetime   ldt_yearly_factor_date, ldt_null_date, ldtm_benefit_start_date 
n_process_run_status ln_process_run_status

//************************************************************************************************************
// P10275 - Daytime Payment Processing
// added new function call to prevent updating of tables used by PRODSVCS Payment Processing
// new object N_PROCESS_RUN_STATUS is used to determine status of processes (in progress = Y/N)
// '014' refers to the Benefit Calculation Maintenance module, '044' refers to the Payment Processing module
//************************************************************************************************************
ln_process_run_status = CREATE N_PROCESS_RUN_STATUS

li_rtn = ln_process_run_status.nf_in_progress('014','044','save',SQLCA)

IF li_rtn = 1 THEN
	RETURN   // module is blocked
END IF
//******************************************************************************************

//	Benefit Calculation Parameters stored in s_window_message
//	Claim No								al_doubleparm[1]
//	Opening No							al_doubleparm[2]
//	Benefit Calculation No			al_doubleparm[3]
//	Copy Benefit Calculation No	al_doubleparm[4]
//
//	Transitional Claim Flag			as_stringparm[1]
//	Top-up Flag							as_stringparm[2]
//	Opening Type Code					as_stringparm[3]
//
//	Accident Recurrence Date		adtm_datetimeparm[1]
//	Benefit Start Date				adtm_datetimeparm[2]
//
//	Read/Update Mode					as_mode 

// Use some local variables to make the code readable!!!!
ldtm_benefit_start_date = istr_bencalc_parameters.adtm_datetimeparm[2]
ls_opening_type_code = istr_bencalc_parameters.as_stringparm[3]

// VALIDATE THE REQUIRED FIELDS AND DO ANY CONSISTENCY VALIDATIONS BEFORE SAVING

// PRE-ACCIDENT DATA
// Validate the Mandatory fields on the PreAccident Data datawindow.  Do an accept text to ensure that
// the last field entered passes the validation tests. If no, set the focus back to the datawindow column

ldec_benefit_level_percentage = dw_benefit_calculation_details.GetItemDecimal(1, "benefit_level_percentage")

IF dw_benefit_calculation_preaccident_data.AcceptText() = -1 THEN
	uo_tabs.uf_display_tab(1)
	dw_benefit_calculation_preaccident_data.SetFocus()
	RETURN
END IF

IF IsNull(dw_benefit_calculation_preaccident_data.GetItemNumber(1, "preacc_gross_pay_base_year")) OR dw_benefit_calculation_preaccident_data.GetItemNumber(1, "preacc_gross_pay_base_year") = 0 THEN
	MessageBox("Benefit Calculation Module", "Please enter a valid year for the Year of Earnings", Exclamation!)
	uo_tabs.uf_display_tab(1)
	dw_benefit_calculation_preaccident_data.SetFocus()
	dw_benefit_calculation_preaccident_data.SetColumn("preacc_gross_pay_base_year")	
	RETURN
ELSE
	IF Long(dw_benefit_calculation_preaccident_data.GetItemNumber(1, "preacc_gross_pay_base_year")) < 1982 THEN
		MessageBox("Benefit Calculation Module", "The Year of Earnings cannot be prior to 1982",Exclamation!)
		RETURN
	ELSEIF Long(dw_benefit_calculation_preaccident_data.GetItemNumber(1, "preacc_gross_pay_base_year")) < YEAR(DATE(idtm_accident_recurrence_date)) THEN
		MessageBox("Benefit Calculation Module", "The Year of Earnings cannot be prior to the Accident/Recurrence date.",Exclamation!)
		RETURN
	END IF
END IF

IF IsNull(dw_benefit_calculation_preaccident_data.GetItemString(1, "preacc_earn_freq_code")) OR dw_benefit_calculation_preaccident_data.GetItemString(1, "preacc_earn_freq_code") = "" THEN
	MessageBox("Benefit Calculation Module", "Please enter a value for the Pre-Accident Earnings Frequency", Exclamation!)
	uo_tabs.uf_display_tab(1)
	dw_benefit_calculation_preaccident_data.SetFocus()
	dw_benefit_calculation_preaccident_data.SetColumn("preacc_earn_freq_code")	
	RETURN
END IF

IF IsNull(dw_benefit_calculation_preaccident_data.GetItemDecimal(1, "preacc_gross_pay")) OR dw_benefit_calculation_preaccident_data.GetItemDecimal(1, "preacc_gross_pay") = 0 THEN
	MessageBox("Benefit Calculation Module", "Please enter a value for the Pre-Accident Gross Earnings", Exclamation!)
	uo_tabs.uf_display_tab(1)
	dw_benefit_calculation_preaccident_data.SetFocus()
	dw_benefit_calculation_preaccident_data.SetColumn("preacc_gross_pay")	
	RETURN
END IF

IF IsNull(dw_benefit_calculation_preaccident_data.GetItemString(1, "preacc_paid_cpp_flag")) OR dw_benefit_calculation_preaccident_data.GetItemString(1, "preacc_paid_cpp_flag") = "" THEN
	MessageBox("Benefit Calculation Module", "Please enter a value for the Pre-Accident CPP Deduction", Exclamation!)
	uo_tabs.uf_display_tab(1)
	dw_benefit_calculation_preaccident_data.SetFocus()
	dw_benefit_calculation_preaccident_data.SetColumn("preacc_paid_cpp_flag")	
	RETURN
END IF

IF IsNull(dw_benefit_calculation_preaccident_data.GetItemString(1, "preacc_paid_uic_flag")) OR dw_benefit_calculation_preaccident_data.GetItemString(1, "preacc_paid_uic_flag") = "" THEN
	MessageBox("Benefit Calculation Module", "Please enter a value for the Pre-Accident UIC Deduction", Exclamation!)
	uo_tabs.uf_display_tab(1)
	dw_benefit_calculation_preaccident_data.SetFocus()
	dw_benefit_calculation_preaccident_data.SetColumn("preacc_paid_uic_flag")	
	RETURN
END IF

IF IsNull(dw_benefit_calculation_preaccident_data.GetItemString(1, "avg_earn_freq_code")) OR dw_benefit_calculation_preaccident_data.GetItemString(1, "avg_earn_freq_code") = "" THEN
	MessageBox("Benefit Calculation Module", "Please enter a value for the Average Earning Frequency", Exclamation!)
	uo_tabs.uf_display_tab(1)
	dw_benefit_calculation_preaccident_data.SetFocus()
	dw_benefit_calculation_preaccident_data.SetColumn("avg_earn_freq_code")	
	RETURN
END IF

IF IsNull(dw_benefit_calculation_preaccident_data.GetItemDecimal(1, "avg_gross_pay")) OR dw_benefit_calculation_preaccident_data.GetItemDecimal(1, "avg_gross_pay") = 0 THEN
	MessageBox("Benefit Calculation Module", "Please enter a value for the Average Gross Earnings", Exclamation!)
	uo_tabs.uf_display_tab(1)
	dw_benefit_calculation_preaccident_data.SetFocus()
	dw_benefit_calculation_preaccident_data.SetColumn("avg_gross_pay")	
	RETURN
END IF

IF IsNull(dw_benefit_calculation_preaccident_data.GetItemString(1, "avg_paid_cpp_flag")) OR dw_benefit_calculation_preaccident_data.GetItemString(1, "avg_paid_cpp_flag") = "" THEN
	MessageBox("Benefit Calculation Module", "Please enter a value for the Average Earnings CPP Deduction", Exclamation!)
	uo_tabs.uf_display_tab(1)
	dw_benefit_calculation_preaccident_data.SetFocus()
	dw_benefit_calculation_preaccident_data.SetColumn("avg_paid_cpp_flag")	
	RETURN
END IF

IF IsNull(dw_benefit_calculation_preaccident_data.GetItemString(1, "avg_paid_uic_flag")) OR dw_benefit_calculation_preaccident_data.GetItemString(1, "avg_paid_uic_flag") = "" THEN
	MessageBox("Benefit Calculation Module", "Please enter a value for the Average Earnings UIC Deduction", Exclamation!)
	uo_tabs.uf_display_tab(1)
	dw_benefit_calculation_preaccident_data.SetFocus()
	dw_benefit_calculation_preaccident_data.SetColumn("avg_paid_uic_flag")	
	RETURN
END IF

// CPPD DEDUCTIONS - If a CPPd deduction was not entered, zero out the cppd_base_year
//                   If a CPPd deduction was entered, the CPPd base year must be entered
IF dw_benefit_calculation_cppd.AcceptText() = -1 THEN
	dw_benefit_calculation_cppd.SetFocus()
	RETURN
END IF

IF NOT dw_benefit_calculation_cppd.GetItemDecimal(1, "cppd_monthly_amount") > 0 THEN
	dw_benefit_calculation_cppd.SetItem(1, "cppd_base_year",0)
ELSE
	IF IsNull(dw_benefit_calculation_cppd.GetItemNumber(1, "cppd_base_year")) OR &
		dw_benefit_calculation_cppd.GetItemNumber(1, "cppd_base_year") = 0 THEN
		MessageBox("Benefit Calculation Module", "Please enter a value for the CPPD Base Year", Exclamation!)
		uo_tabs.uf_display_tab(3)
		dw_benefit_calculation_cppd.SetFocus()
		dw_benefit_calculation_cppd.SetColumn("cppd_base_year")	
		RETURN
	END IF
END IF

//	BENEFIT CALCULATION DETAILS - Validate the Mandatory Fields on the Benefit calculation details datawindow. Do an accept text to ensure that
//                               the last field entered passes the validation tests. If no, set the focus back to the datawindow column
IF dw_benefit_calculation_details.AcceptText() = -1 THEN
	dw_benefit_calculation_details.SetFocus()
	RETURN
END IF

IF IsNull(dw_benefit_calculation_details.GetItemDateTime(1, "effective_from_date")) THEN
	MessageBox("Benefit Calculation Module", "Please enter a value for the Effective Date", Exclamation!)
	dw_benefit_calculation_details.SetColumn("effective_from_date")
	RETURN
ELSE
	IF (dw_benefit_calculation_details.GetItemDateTime(1, "effective_from_date") < ldtm_benefit_start_date) THEN
		MessageBox("Benefit Calculation Module", "Effective Date cannot be less than the Benefit Start Date: " + String(ldtm_benefit_start_date, "mmm d, yyyy"), Exclamation!)
		dw_benefit_calculation_details.SetColumn("effective_from_date")
		RETURN
	END IF
END IF

IF (dw_benefit_calculation_details.GetItemDateTime(1, "effective_from_date")) < idtm_accident_recurrence_date  THEN
	MessageBox("Benefit Calculation Module", "Effective Date cannot be less than the Accident/Recurrence Date: " + String(idtm_accident_recurrence_date, "mmm d, yyyy"), Exclamation!)
	dw_benefit_calculation_details.SetColumn("effective_from_date")
	RETURN
END IF

in_bencalcs.f_retrieve_bencalc_no(il_claim_no,il_benefit_calculation_no)
IF ll_result = -1 THEN 
	Close(parent)
	RETURN
ELSEIF ll_result > 0 THEN 
	MessageBox("Benefit Calculation Module - Validation Error", "You cannot delete a Benefit Calculation that has associated Awards",Exclamation!)
	cb_close.SetFocus()
	RETURN
END IF

in_bencalcs.f_isvalid_effective_from_date(il_claim_no,il_opening_no)
IF ll_result = -1 THEN
	Close(parent)
	RETURN
ELSEIF ll_result > 0 THEN
	MessageBox("Benefit Calculation Module - Validation Warning", "The entered effective_from_date comes before the maximum award_end_date of PERIODIC_AWARDS.",Exclamation!)
END IF

// Yearly_Factor table has changed - P10105 in this case as in all cases the tax index date is no longer valid this has been removed as result as values are now table driven
IF Isvalid(in_bencalcs) THEN
	in_bencalcs.f_get_exemption_amounts	(Date(in_bencalcs.idt_from_yearly_factor),ldec_td1_basic_exemption,ldec_td1_married_exemption,ldec_prov_basic_exemption,ldec_prov_married_exemption)
ELSE
	Close(Parent)
	RETURN
END IF

IF IsNull(dw_benefit_calculation_details.GetItemDecimal(1, "td1_exemption_amount")) THEN
	MessageBox("Benefit Calculation Module", "Please enter a value for the TD1 Exemption Amount", Exclamation!)
	dw_benefit_calculation_details.SetColumn("td1_exemption_amount")
	RETURN
END IF

ldt_yearly_factor_date = dw_benefit_calculation_preaccident_data.GetItemDateTime(1, "yearly_factor_date")

in_bencalcs.ii_release = in_bencalcs.f_get_max_release(in_bencalcs.ii_release, ldt_yearly_factor_date)
IF ii_release = 0 OR IsNull(ii_release) = TRUE THEN
	ii_release = in_bencalcs.ii_release
END IF

// PR1890 - Modified select to poplulate the new BENEFIT_CALCULATION Table values.
// P10151-55 - J.Hawker, 2006.06.14 - Removed most of the SELECT as the columns no longer exist in the BENEFIT_CALCULATION table and do not need to be set.
SELECT td1_basic_exemption, td1_married_exemption, prov_td1_basic_exemption, prov_td1_married_exemption
  INTO :ldec_fed_basic, :ldec_fed_married, :ldec_prov_basic_exemption, :ldec_prov_married_exemption 
  FROM Yearly_Factor 
 WHERE yearly_factor_date = :ldt_yearly_factor_date
   AND release_no = :in_bencalcs.ii_release
 USING SQLCA ; 

li_rtn = SQLCA.nf_handle_error('w_calculation_details', '','cb_save - SELECT td1_basic_exemption, td1_married_exemption, prov_td1_basic_exemption, prov_td1_married_exemption FROM Yearly_Factor ')
IF li_rtn < 0 THEN
	RETURN -1
END IF

ldec_fed_exemption_entered = dw_benefit_calculation_details.GetItemDecimal(1, "td1_exemption_amount")
ldec_prov_exemption_entered = dw_benefit_calculation_details.GetItemDecimal(1, "prov_td1_exemption_amount")

ls_exemption_mismatch = wf_exemption_mismatch(ldec_fed_exemption_entered,ldec_fed_basic,ldec_fed_married, ldec_prov_exemption_entered,ldec_prov_basic_exemption,ldec_prov_married_exemption,Date(ldt_yearly_factor_date))

IF ls_exemption_mismatch = 'FED' THEN
	dw_benefit_calculation_details.SetColumn("td1_exemption_amount")
	RETURN
ELSEIF ls_exemption_mismatch = 'PROV' THEN
	dw_benefit_calculation_details.SetColumn("prov_td1_exemption_amount")
	RETURN
END IF

IF IsNull(dw_benefit_calculation_details.GetItemString(1, "calculation_type_code")) OR dw_benefit_calculation_details.GetItemString(1, "calculation_type_code") = "" THEN
	MessageBox("Benefit Calculation Module", "Please enter a value for the Calc Type", Exclamation!)
	dw_benefit_calculation_details.SetColumn("calculation_type_code")
	RETURN
ELSE
	IF (ls_opening_type_code = 'PEN' OR ls_opening_type_code = 'SV' OR ls_opening_type_code = 'S1' OR ls_opening_type_code = 'S2') AND dw_benefit_calculation_details.GetItemString(1, "calculation_type_code") <> 'M' THEN
		MessageBox("Benefit Calculation Module", "Calc Type must be 'Manual' for opening types 'PEN' OR 'SV' or 'S1' or 'S2' .", Exclamation!)
		dw_benefit_calculation_details.SetColumn("calculation_type_code")
		RETURN
	END IF
END IF

IF IsNull(dw_benefit_calculation_details.GetItemString(1, "award_freq_code")) OR dw_benefit_calculation_details.GetItemString(1, "award_freq_code") = "" THEN
	MessageBox("Benefit Calculation Module", "Please enter a value for the Award Frequency", Exclamation!)
	dw_benefit_calculation_details.SetColumn("award_freq_code")
	RETURN
ELSE
	IF (ls_opening_type_code = 'PEN' OR ls_opening_type_code = 'SV' OR ls_opening_type_code = 'S1' OR ls_opening_type_code = 'S2') AND dw_benefit_calculation_details.GetItemString(1, "award_freq_code") <> 'M' THEN
		MessageBox("Benefit Calculation Module", "Award frequency must be 'Monthly' for opening types 'PEN' OR 'SV' 'OR 'S1' or 'S2'.", Exclamation!)
		dw_benefit_calculation_details.SetColumn("award_freq_code")
		RETURN
	END IF
END IF

IF IsNull(dw_benefit_calculation_details.GetItemString(1, "top_up_flag")) OR dw_benefit_calculation_details.GetItemString(1, "top_up_flag") = "" THEN
	MessageBox("Benefit Calculation Module", "Please enter a value for the Top Up", Exclamation!)
	dw_benefit_calculation_details.SetColumn("top_up_flag")
	RETURN
END IF

IF IsNull(dw_benefit_calculation_details.GetItemString(1, "transitional_claim_flag")) OR dw_benefit_calculation_details.GetItemString(1, "transitional_claim_flag") = "" THEN
	MessageBox("Benefit Calculation Module", "Please enter a value for the Transitional Claim", Exclamation!)
	dw_benefit_calculation_details.SetColumn("transitional_claim_flag")
	RETURN
END IF

IF IsNull(dw_benefit_calculation_details.GetItemNumber(1, "preacc_work_days_per_week")) THEN
	MessageBox("Benefit Calculation Module", "Please enter a value for the Work Days", Exclamation!)
	dw_benefit_calculation_details.SetColumn("preacc_work_days_per_week")
	RETURN
ELSEIF dw_benefit_calculation_details.GetItemNumber(1, "preacc_work_days_per_week") = 0 AND (ls_opening_type_code <> 'PEN' AND ls_opening_type_code <> 'SV' AND  ls_opening_type_code <> 'S1' AND  ls_opening_type_code <> 'S2') THEN
	MessageBox("Benefit Calculation Module", "Please enter a value for the Work Days", Exclamation!)
	dw_benefit_calculation_details.SetColumn("preacc_work_days_per_week")
	RETURN
END IF

IF IsNull(dw_benefit_calculation_details.GetItemNumber(1, "preacc_work_hours_per_day")) THEN
	MessageBox("Benefit Calculation Module", "Please enter a value for the Work Hours", Exclamation!)
	dw_benefit_calculation_details.SetColumn("preacc_work_hours_per_day")
	RETURN
ELSEIF dw_benefit_calculation_details.GetItemNumber(1, "preacc_work_hours_per_day") = 0 AND (ls_opening_type_code <> 'PEN' AND ls_opening_type_code <> 'SV' AND ls_opening_type_code <> 'S1' AND  ls_opening_type_code <> 'S2' ) THEN
	MessageBox("Benefit Calculation Module", "Please enter a value for the Work Hours", Exclamation!)
	dw_benefit_calculation_details.SetColumn("preacc_work_hours_per_day")
	RETURN
END IF

ls_transitional_claim_flag = dw_benefit_calculation_details.GetItemString(1, "transitional_claim_flag")

// Validate Frozen Data 
IF ls_transitional_claim_flag = "Y" THEN
	IF dw_benefit_calculation_frozen_data.AcceptText() < 0 THEN
		uo_tabs.uf_display_tab(1)
		dw_benefit_calculation_frozen_data.SetFocus()
		RETURN
	END IF
END IF

IF dw_benefit_calculation_capable_data.AcceptText() < 0 THEN
	uo_tabs.uf_display_tab(2)
	dw_benefit_calculation_capable_data.SetFocus()
	RETURN
END IF

ldec_pre93_capable_gross_pay = dw_benefit_calculation_capable_data.GetItemNumber(1,'pre93_capable_gross_pay')
IF ldec_pre93_capable_gross_pay < 0 THEN
	dw_benefit_calculation_capable_data.visible = TRUE
	dw_benefit_calculation_capable_data.SetFocus()
	RETURN
END IF

IF dw_benefit_calculation_remunerations.AcceptText() < 0 THEN
	uo_tabs.uf_display_tab(4)
	dw_benefit_calculation_remunerations.SetFocus()
	RETURN
END IF

IF dw_benefit_calculation_deductions.AcceptText() < 0 THEN
	uo_tabs.uf_display_tab(3)
	dw_benefit_calculation_deductions.SetFocus()
	RETURN
END IF

// Ensure that frozen earnings have been entered if required.
IF ls_transitional_claim_flag = "Y" THEN
	ldec_amount = dw_benefit_calculation_frozen_data.GetItemDecimal(dw_benefit_calculation_frozen_data.GetRow(), "frozen_annual_net_pay")
	IF NOT ldec_amount > 0 THEN
		MessageBox("Benefit Calculation Module - Validation Error", "Frozen Earnings must be entered for transitional claims",Exclamation!)
		uo_tabs.uf_display_tab(1)
		dw_benefit_calculation_frozen_data.SetFocus()
		dw_benefit_calculation_frozen_data.SetColumn("frozen_annual_net_pay")
		RETURN
	END IF
	ldec_amount = dw_benefit_calculation_frozen_data.GetItemDecimal(dw_benefit_calculation_frozen_data.GetRow(), "frozen_gross_pay")
	IF not ldec_amount > 0 THEN
		MessageBox("Benefit Calculation Module - Validation Error", "You must provide frozen gross earnings for transitional claims",Exclamation!)
		uo_tabs.uf_display_tab(1)
		dw_benefit_calculation_frozen_data.SetFocus()
		dw_benefit_calculation_frozen_data.SetColumn("frozen_gross_pay")
		RETURN
	END IF
	
	ldec_regular_award_amount = dw_benefit_calculation_display_computed.GetItemDecimal(1, "regular_award_amount")
	ldec_transitional_award_amount = dw_benefit_calculation_display_computed.GetItemDecimal(1, "transitional_award_amount")
	ldec_benefit_level_percentage = dw_benefit_calculation_details.GetItemDecimal(1, "benefit_level_percentage")

	// Also, for a Transitional Claim, if the 85% has passed the 90%, then warn the user that the transitional claim flag in the benefit calculation details will be updated 

	// PR2719 - If PEN,SV,S1,S2 do not check
	IF (ls_opening_type_code = 'PEN' OR ls_opening_type_code = 'SV' OR ls_opening_type_code = 'S1' OR  ls_opening_type_code = 'S2') THEN
	ELSE
		IF ldec_regular_award_amount >= ldec_transitional_award_amount THEN
			ls_transitional_claim_flag = "N"
			dw_benefit_calculation_details.SetItem(1, "transitional_claim_flag", ls_transitional_claim_flag)
			IF ldec_benefit_level_percentage <> 0.85 THEN
				dw_benefit_calculation_details.SetItem(1, "benefit_level_percentage",.85)
				MessageBox("Benefit Calculation Module - FYI!", "The benefit at 85% has passed the frozen benefits at 90%~r~n" + &
							"The award has been set using the 85% calculation~r~nThe benefit level percentage will be set to 85%~r~n" + &
							"The transitional claim flag for this benefit calculation will be set to 'No'",Information!)
			ELSE
				MessageBox("Benefit Calculation Module - FYI!", "The benefit at 85% has passed the frozen benefits at 90%~r~n" + &
							"The award has been set using the 85% calculation~r~nThe transitional claim flag for this benefit calculation has been set to 'No'",Information!)
			END IF
		ELSE
			IF ldec_benefit_level_percentage <> 0.9 THEN
				dw_benefit_calculation_details.SetItem(1, "benefit_level_percentage",0.9)
				MessageBox("Benefit Calculation Module - FYI!", "The benefit at 85% has not passed the frozen benefits at 90%~r~nThe benefit level percentage will be set to .90",Information!)
			END IF
		END IF
	END IF
END IF

ldec_regular_award_amount = dw_benefit_calculation_display_computed.GetItemDecimal(1, "regular_award_amount")
ldec_transitional_award_amount = dw_benefit_calculation_display_computed.GetItemDecimal(1, "transitional_award_amount")
ldt_effective_from_date = Date(dw_benefit_calculation_details.GetItemDateTime(1, "effective_from_date"))

//	Ensure the Benefit Level Percentage are correct depending on whether it is a Transitional Claim.
//	Get the benefit level percentage again in case it was set in the code above
ldec_benefit_level_percentage	= dw_benefit_calculation_details.GetItemDecimal(1, "benefit_level_percentage")
ll_result = wf_check_benefit_level(ldec_benefit_level_percentage , ldt_effective_from_date , ls_transitional_claim_flag, ldec_regular_award_amount , ldec_transitional_award_amount )
IF ll_result = -1 THEN
	// prevent save
	dw_benefit_calculation_details.SetColumn("benefit_level_percentage")
	RETURN
END IF

//		If the capable earnings type is estimated, then remunerations type must be estimated
//		If the capable earnings type is actual, then remunerations type must be "wages or salary"

//		If the remunerations type is estimated, then capable earnings type must be estimated
//		If the remunerations type is "wages or salary", then capable earnings type must be actual
ls_pre93_capable_earn_type_code = dw_benefit_calculation_capable_data.GetItemString(1,'pre93_capable_earn_type_code')
ls_pre93_capable_earn_freq_code = dw_benefit_calculation_capable_data.GetItemString(1,'pre93_capable_earn_freq_code')
ldec_pre93_capable_gross_pay = dw_benefit_calculation_capable_data.GetItemDecimal(1,'pre93_capable_gross_pay')
ls_pre93_capable_uic_flag = dw_benefit_calculation_capable_data.GetItemString(1,'pre93_capable_uic_flag')
ls_pre93_capable_cpp_flag = dw_benefit_calculation_capable_data.GetItemString(1,'pre93_capable_cpp_flag')

li_remun_rowcount = dw_benefit_calculation_remunerations.RowCount()


// BR 3.145
IF ldec_pre93_capable_gross_pay <> 0 THEN
	IF ls_pre93_capable_earn_type_code = 'A' THEN
		ls_find_remuneration =	'( remun_type_code = "WAGE" ) AND remun_gross_pay <> 0'
	
		li_found_row = dw_benefit_calculation_remunerations.Find(ls_find_remuneration, 1 , li_remun_rowcount )
		IF li_found_row = 0 THEN
			MessageBox('Benefit Calculation Error','A benefit calculation having Actual Capable earnings must have a "Wages or Salary" remuneration.',StopSign!)
			RETURN
		END IF
	END IF	
END IF


// The capable earning frequency of a regular LTD benefit calculation, copied from a previous year
// LTD benefit calculation, must be the same as that previous year's capable earning frequency
ll_current_yrs_copied_bencalc_no = dw_benefit_calculation_details.GetItemNumber(1,'copied_from_benefit_calc_no')
li_year = Year(Date(dw_benefit_calculation_details.GetItemDateTime(1,'effective_from_date')))
ls_opening_type_code = dw_benefit_calculation_details.GetItemString(1,'opening_type_code')

IF ll_current_yrs_copied_bencalc_no <> 0 AND ls_opening_type_code = 'LTD' THEN
	SELECT Count(*) 
	  INTO :li_count 
	  FROM BENEFIT_CALCULATION 
	 WHERE claim_no = :il_claim_no 
	   AND benefit_calculation_no = :ll_current_yrs_copied_bencalc_no 
	   AND Year(effective_from_date) = :li_year - 1 
	 USING SQLCA ;
	
	li_rtn = SQLCA.nf_handle_error('w_calculation_details', '','cb_save - SELECT Count(*) FROM BENEFIT_CALCULATION ')

	IF li_count <> 0 THEN
		// Determine if the previous years copied LTD bencalc had a capable earning
		SELECT Count(*) 
		  INTO :li_count 
		  FROM BENEFIT_CALCULATION	a 
		  JOIN CAPABLE_EARNING b ON a.claim_no = b.claim_no AND a.benefit_calculation_no = b.benefit_calculation_no 
		 WHERE a.claim_no = :il_claim_no 
		   AND a.benefit_calculation_no = :ll_current_yrs_copied_bencalc_no 
		   AND Year(a.effective_from_date) = :li_year - 1 
		USING SQLCA ; 
		
		li_rtn = SQLCA.nf_handle_error('w_calculation_details', '','cb_save - SELECT Count(*) FROM BENEFIT_CALCULATION	a JOIN CAPABLE_EARNING b ')
		
		IF li_count <> 0 THEN
			// There was a record, so... determine if the frequency was the same
			SELECT Count(*)
			  INTO :li_count 
			  FROM BENEFIT_CALCULATION a 
			  JOIN CAPABLE_EARNING b ON a.claim_no = b.claim_no AND a.benefit_calculation_no = b.benefit_calculation_no 
			 WHERE a.claim_no = :il_claim_no 
			   AND a.benefit_calculation_no = :ll_current_yrs_copied_bencalc_no 
			   AND Year(a.effective_from_date) = :li_year - 1 
			   AND b.pre93_capable_earn_freq_code = :ls_pre93_capable_earn_freq_code 
			USING SQLCA ;
			
			li_rtn = SQLCA.nf_handle_error('w_calculation_details', '','cb_save - SELECT Count(*) FROM BENEFIT_CALCULATION a JOIN CAPABLE_EARNING b ON a.claim_no = b.claim_no AND a.benefit_calculation_no = b.benefit_calculation_no')
			
			IF li_count = 0 THEN
				MessageBox('Benefit Calculation Error','When an LTD benefit calculation is copied from the previous year, the capable earnings frequency type must match.',StopSign!)
				RETURN
			END IF
		END IF
	END IF
END IF

// **************************************
// *** start - P10261 - RTW Incentive project ***
IF dw_benefit_calculation_details.GetItemString(1, 'rtw_incentive_flag') = 'Y' THEN
	ll_result = wf_check_RTW_bus_rules()
	IF ll_result <> 0 THEN RETURN ll_result
	
	// verify that linked LTD bencalc has at least 120% of capable earnings
	SELECT min_capable_earn_percent_increase
	  INTO :ldec_min_capable_earn_percent_increase
	  FROM Rtw_Incentive_Qualification_Parameter
	 WHERE rtw_incentive_type_code = 'LTD'
	 USING SQLCA ;
	
	li_rtn = SQLCA.nf_handle_error("w_calculation_details", "", "cb_save - SELECT min_capable_earn_percent_increase FROM Rtw_Incentive_Qualification_Parameter")

	ldec_LTD_pre93_capable_gross_pay = ids_rtw_incentive_linked_bencalcs.GetItemDecimal(1,'curr_yr_reg_pre93_capable_gross_pay')
	ldec_prev_years_capable_earnings = ids_rtw_incentive_linked_bencalcs.GetItemDecimal(1,'prev_yrs_capable_pre93_capable_gross_pay')
	
	IF ldec_LTD_pre93_capable_gross_pay = 0 THEN
		MessageBox('RTW Incentive Error','In order to qualify as a RTW Incentive benefit calculation, the LTD benefit calculation~nthat was copied must have a capable earnings amount.',StopSign!)
		RETURN
	END IF
	
	IF IsNull(ldec_prev_years_capable_earnings) THEN
		ldec_prev_years_capable_earnings = 0
	END IF
	
	IF ldec_LTD_pre93_capable_gross_pay >= ldec_prev_years_capable_earnings*(1 + ldec_min_capable_earn_percent_increase/100) THEN
		// OK
	ELSEIF ldec_LTD_pre93_capable_gross_pay < ldec_prev_years_capable_earnings*(1 + ldec_min_capable_earn_percent_increase/100) OR ldec_LTD_pre93_capable_gross_pay = 0 THEN
		MessageBox('RTW Incentive Error','In order to qualify as a RTW Incentive benefit calculation, the LTD benefit calculation' +&
													'~nthat was copied must have a capable earnings amount '+String(ldec_min_capable_earn_percent_increase)+'% greater than~nthe amount for the previous year.',StopSign!)
		RETURN
	END IF
	
	ldec_rtw_incentive_capable_earnings = dw_benefit_calculation_capable_data.GetItemDecimal(dw_benefit_calculation_capable_data.GetRow(),'pre93_capable_gross_pay')
	IF ldec_rtw_incentive_capable_earnings <> ldec_prev_years_capable_earnings THEN
		MessageBox('RTW Incentive Error','The capable earnings for the previous year for the "Capable Earnings Ben Calc" is not equal to the capable earnings for the RTW Incentive Benefit Calculation.',StopSign!)
		RETURN
	END IF
	
	// deductions
	// regular LTD award must match RTW Incentive LTD deduction
	ldec_reg_LTD_award_amount = ids_rtw_incentive_linked_bencalcs.GetItemNumber(1,'curr_yrs_reg_ltd_award_amount')
	ll_find_deduction = dw_benefit_calculation_deductions.Find('benefit_calc_deduction_code = "LTD" and deduction_amount = ' + String(ldec_reg_LTD_award_amount) , 1, dw_benefit_calculation_deductions.RowCount() )
	IF ll_find_deduction = 0 THEN
		MessageBox('RTW Incentive Error','The RTW Incentive benefit calculation deduction must be the same as the award amount for the copied benefit calculation.',StopSign!)
		RETURN
	END IF
	
	// the deduction must be monthly
	ls_deduction_earn_freq_code = dw_benefit_calculation_deductions.GetItemString(ll_find_deduction,'deduction_earn_freq_code')
	IF ls_deduction_earn_freq_code <> 'M' THEN
		MessageBox('RTW Incentive Error','The RTW Incentive benefit calculation deduction must have "Monthly" as a frequency.',StopSign!)
		RETURN
	END IF
	
	//	The remuneration gross amt must be same as capable earnings amt
	ldec_pre93_capable_gross_pay	= dw_benefit_calculation_capable_data.GetItemDecimal(1,'pre93_capable_gross_pay')
	
	li_found_row = dw_benefit_calculation_remunerations.Find('remun_gross_pay = ' + String(ldec_pre93_capable_gross_pay),1,li_remun_rowcount)
	IF li_found_row = 0 THEN
		MessageBox('RTW Incentive Error','For a RTW Incentive Benefit Calculation, the remuneration amount must be equal to the capable earnings amount.',StopSign!)
		RETURN
	END IF

	//	A LTD RTW Incentive benefit calculation must not be associated with a regular LTD benefit calculation, if 
	// the latter has an associated regular LTD or an associated RTW Incentive benefit calculation effective in the same year.
	li_ltd_benefit_calc_no = dw_benefit_calculation_details.GetItemNumber(1,'copied_from_benefit_calc_no')
	
	SELECT c.benefit_calculation_no , Year(a.effective_from_date)
	  INTO :li_prior_copied_bencalc_no , :li_copied_from_effective_year
 	  FROM BENEFIT_CALCULATION a
	  JOIN OPENING	b ON a.claim_no = b.claim_no AND a.opening_no = b.opening_no 
	  JOIN BENEFIT_CALCULATION c ON a.claim_no = c.claim_no AND a.benefit_calculation_no = c.copied_from_benefit_calc_no 
	  JOIN OPENING d ON a.claim_no = d.claim_no AND a.opening_no = d.opening_no 
	 WHERE a.claim_no = :il_claim_no 
	   AND a.benefit_calculation_no = :li_ltd_benefit_calc_no 
	   AND b.opening_type_code = 'LTD'
	   AND d.opening_type_code = 'LTD' 
	 USING SQLCA ; 
	
	li_rtn = SQLCA.nf_handle_error("w_calculation_details", "", "cb_save - SELECT c.benefit_calculation_no , Year(a.effective_from_date) FROM BENEFIT_CALCULATION a JOIN OPENING b JOIN BENEFIT_CALCULATION c JOIN OPENING d ")
	
	IF li_year = li_copied_from_effective_year THEN
		MessageBox('Benefit Calculation Module - Validation Error','A LTD RTW Incentive benefit calculation must not be associated with a regular~nLTD benefit calculation, if the latter has an associated regular LTD or' +&
																				'~nan associated RTW Incentive benefit calculation effective in the same year.~NSee benefit calculation #'+String(li_prior_copied_bencalc_no)+'.',StopSign!)
		RETURN 1
	END IF
ELSE
	// A regular LTD benefit calculation must not be associated with a regular LTD benefit calculation effective 
	//	in the same year, if the latter benefit calculation is associated with a LTD RTW Incentive benefit calculation.
	li_ltd_benefit_calc_no	 = dw_benefit_calculation_details.GetItemNumber(1,'copied_from_benefit_calc_no')
	
	SELECT Year(a.effective_from_date)
	  INTO :li_copied_from_effective_year 
	  FROM BENEFIT_CALCULATION a 
	  JOIN OPENING b ON a.claim_no = b.claim_no AND a.opening_no = b.opening_no 
	  JOIN BENEFIT_CALCULATION c ON a.claim_no = c.claim_no AND a.benefit_calculation_no = c.copied_from_benefit_calc_no
	 WHERE a.claim_no = :il_claim_no 
	   AND a.benefit_calculation_no = :li_ltd_benefit_calc_no 
	   AND b.opening_type_code = 'LTD' 
	   AND c.rtw_incentive_flag = 'Y' 
	USING SQLCA ; 

	li_rtn = SQLCA.nf_handle_error("w_calculation_details", "", "cb_save - SELECT Year(a.effective_from_date) FROM BENEFIT_CALCULATION a JOIN OPENING b JOIN BENEFIT_CALCULATION c ")
	
	IF li_year = li_copied_from_effective_year THEN
		dw_benefit_calculation_details.SetItem(1, 'effective_from_date', ldt_null_date)
		MessageBox('Benefit Calculation Module - Validation Error','A regular LTD benefit calculation must not be associated with a regular LTD ~nbenefit calculation effective in the same year, if the latter benefit calculation ' +&
																				'~nis associated with a LTD RTW Incentive benefit calculation.',StopSign!)
		RETURN
	END IF
END IF
	
// *** end - P10261 - RTW Incentive project ***
// **************************************



SQLCA.nf_begin_transaction()


// Put a lock on the opening record to ensure that it hasn't been deleted (If we haven't already done it)
// and doesn't get deleted until we're done saving the new benefit calculation. Do not put a lock on the opening if this is a Cost Analysis Benefit
IF (ls_transitional_claim_flag = "Y" and ldec_regular_award_amount >= ldec_transitional_award_amount) OR (il_opening_no = 0) THEN
ELSE
	IF in_bencalcs.f_lock_opening(il_claim_no, il_opening_no, Date(idtm_accident_recurrence_date)) = -1 THEN
		Close(parent)
		RETURN
	END IF
END IF

//	Get identifiers and update dw_benefit_calculation_details.
il_benefit_calculation_no = in_bencalcs.f_get_next_ben_calc_no(il_claim_no)
IF il_benefit_calculation_no < 0 THEN
	Close(parent)
	RETURN
END IF

//	If a cppd deduction was not entered, zero out the cppd_base_year. Also, prevent negative CPPD from being saved
IF dw_benefit_calculation_details.GetItemDecimal(1, "cppd_monthly_amount") = 0 THEN
	dw_benefit_calculation_details.SetItem(1, "cppd_base_year",0)
ELSEIF dw_benefit_calculation_details.GetItemDecimal(1, "cppd_monthly_amount") < 0 THEN
	SQLCA.nf_rollback_transaction()
	uo_tabs.uf_setindex(3)
	dw_benefit_calculation_cppd.SetFocus()
	dw_benefit_calculation_cppd.SetColumn('cppd_monthly_amount')
	MessageBox('CPPD Error','The CPPD monthly amount may not be negative',StopSign!)
	RETURN
END IF

//	Get rid of Unwanted Financial Remunerations and then update the identifiers on the ones that are left.
ll_cntr = 1
ll_recnmbr = 1
ll_result = dw_benefit_calculation_remunerations.RowCount()
DO WHILE ll_cntr <= ll_result
	IF dw_benefit_calculation_remunerations.GetItemDecimal(ll_cntr, "remun_gross_pay") = 0 THEN
		dw_benefit_calculation_remunerations.SetItemStatus(ll_cntr,0,Primary!,DataModIFied!)
		dw_benefit_calculation_remunerations.SetItemStatus(ll_cntr,0,Primary!,NotModIFied!)
	ELSEIF dw_benefit_calculation_remunerations.GetItemDecimal(ll_cntr, "remun_gross_pay") < 0 THEN
		SQLCA.nf_rollback_transaction()
		uo_tabs.uf_setindex(4)
		dw_benefit_calculation_remunerations.ScrollToRow(ll_cntr)
		dw_benefit_calculation_remunerations.SetColumn('remun_gross_pay')
		MessageBox('Remuneration Error','Remunerations may not be negative',StopSign!)
		RETURN
	ELSE
		dw_benefit_calculation_remunerations.SetItem(ll_cntr, "claim_no",il_claim_no)
		dw_benefit_calculation_remunerations.SetItem(ll_cntr, "benefit_calculation_no",il_benefit_calculation_no)
		dw_benefit_calculation_remunerations.SetItem(ll_cntr, "remun_no",ll_recnmbr)
		ll_recnmbr ++
	END IF
	ll_cntr ++
LOOP
	
//	Get rid of Unwanted Deductions and then update the identifiers on the ones that are left
// (We have to do the setitemstatus as there is a problem with pb when trying to remove these rows).
ll_cntr = 1
ll_recnmbr = 1
ll_result = dw_benefit_calculation_deductions.RowCount()
DO WHILE ll_cntr <= ll_result
	IF dw_benefit_calculation_deductions.GetItemDecimal(ll_cntr, "deduction_amount") = 0 THEN
		dw_benefit_calculation_deductions.SetItemStatus(ll_cntr,0,Primary!,DataModified!)
		dw_benefit_calculation_deductions.SetItemStatus(ll_cntr,0,Primary!,NotModIFied!)
	ELSEIF dw_benefit_calculation_deductions.GetItemDecimal(ll_cntr, "deduction_amount") < 0 THEN
		SQLCA.nf_rollback_transaction()
		uo_tabs.uf_setindex(3)
		dw_benefit_calculation_deductions.SetFocus()
		dw_benefit_calculation_deductions.ScrollToRow(ll_cntr)
		dw_benefit_calculation_deductions.SetColumn('deduction_amount')
		MessageBox('Regular Deduction Error','Regular Deductions may not be negative',StopSign!)
		RETURN
	ELSE
		dw_benefit_calculation_deductions.SetItem(ll_cntr, "claim_no",il_claim_no)
		dw_benefit_calculation_deductions.SetItem(ll_cntr, "benefit_calculation_no",il_benefit_calculation_no)
		dw_benefit_calculation_deductions.SetItem(ll_cntr, "deduction_no",ll_recnmbr)
		ll_recnmbr ++
	END IF
	ll_cntr ++
LOOP 

//	Delete capable data if no longer required.
IF dw_benefit_calculation_capable_data.GetItemDecimal(1, "pre93_capable_gross_pay") = 0 THEN
	dw_benefit_calculation_capable_data.DeleteRow(0)
ELSEIF dw_benefit_calculation_capable_data.GetItemDecimal(1, "pre93_capable_gross_pay") < 0 THEN
	SQLCA.nf_rollback_transaction()
	uo_tabs.uf_setindex(2)
	dw_benefit_calculation_capable_data.SetColumn('pre93_capable_gross_pay')
	MessageBox('Capable Earnings Error','Capable Earnings may not be negative',StopSign!)
	RETURN
ELSE
	dw_benefit_calculation_capable_data.SetItem(1, "claim_no",il_claim_no)
	dw_benefit_calculation_capable_data.SetItem(1, "benefit_calculation_no",il_benefit_calculation_no)
END IF

dw_benefit_calculation_details.SetItem(1, "benefit_calculation_no",il_benefit_calculation_no)
dw_benefit_calculation_details.SetItem(1, "claim_no",il_claim_no)
dw_benefit_calculation_details.SetItem(1, "opening_no",il_opening_no)
dw_benefit_calculation_details.SetItem(1, "create_date",f_server_datetime())
dw_benefit_calculation_details.SetItem(1, "release_no", in_bencalcs.ii_release)

// Datawindow Updates
li_rtn = dw_benefit_calculation_details.Update()
SQLCA.nf_handle_error("w_calculation_details", "", "cb_save - dw_benefit_calculation_details.Update()")


//	Update frozen if needed
IF ls_transitional_claim_flag = "Y" THEN
	dw_benefit_calculation_frozen_data.SetItem(1, "claim_no",il_claim_no)
	dw_benefit_calculation_frozen_data.SetItem(1, "benefit_calculation_no",il_benefit_calculation_no)

	li_rtn = dw_benefit_calculation_frozen_data.Update()
	SQLCA.nf_handle_error("w_calculation_details", "", "cb_save - dw_benefit_calculation_frozen_data.Update()")	
END IF
	
li_rtn = dw_benefit_calculation_remunerations.Update()
SQLCA.nf_handle_error("w_calculation_details", "", "cb_save - dw_benefit_calculation_remunerations.Update()")


li_rtn = dw_benefit_calculation_deductions.Update()
SQLCA.nf_handle_error("w_calculation_details", "", "cb_save - dw_benefit_calculation_deductions.Update()")

	
li_rtn = dw_benefit_calculation_capable_data.Update()
SQLCA.nf_handle_error("w_calculation_details", "", "cb_save - dw_benefit_calculation_capable_data.Update()")


SQLCA.nf_commit_transaction()


dw_benefit_calculation_details.Enabled = FALSE
dw_benefit_calculation_preaccident_data.Enabled	= FALSE
dw_benefit_calculation_cppd.Enabled = FALSE
dw_benefit_calculation_frozen_data.Enabled = FALSE
dw_benefit_calculation_capable_data.Enabled = FALSE
dw_benefit_calculation_remunerations.Enabled = FALSE
dw_benefit_calculation_deductions.Enabled = FALSE

cb_add_earnings.Enabled = FALSE
cb_delete_remuneration.Enabled = FALSE

cb_cancel.Enabled = FALSE
cb_save.Enabled = FALSE
cb_list_bencalcs.Enabled = TRUE
cb_close.Enabled= TRUE

// set new bencalc number so that it will be the selected row when you return to the benefit list window
istr_bencalc_parameters.al_doubleparm[3] = il_benefit_calculation_no

cb_close.SetFocus()
end event

type cb_cancel from commandbutton within w_calculation_details
integer x = 384
integer y = 1728
integer width = 352
integer height = 96
integer taborder = 160
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Cance&l"
end type

on clicked;//
//	Cancelling a benefit calculation
//

//	Go back to the benefit calculation list.

	cb_list_bencalcs.TriggerEvent(Clicked!)


end on

type cb_delete from commandbutton within w_calculation_details
integer x = 1147
integer y = 1728
integer width = 352
integer height = 96
integer taborder = 180
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Delete"
end type

event clicked;	INTEGER	li_rtn
	LONG		ll_result,		ll_cntr
	STRING	ls_where
	N_PROCESS_RUN_STATUS ln_process_run_status
	
	/******************************************************************************************
	P10275 - Daytime Payment Processing
	- added new function call to prevent updating of tables used by PRODSVCS Payment Processing
	- new object N_PROCESS_RUN_STATUS is used to determine status of processes (in progress = Y/N)
	- '014' refers to the Benefit Calculation Maintenance module, '044' refers to the Payment Processing module
	******************************************************************************************/
	ln_process_run_status = Create N_PROCESS_RUN_STATUS
	
	li_rtn = ln_process_run_status.nf_in_progress('014','044','delete',SQLCA)
	
	IF li_rtn = 1 THEN
		// module is blocked
		return
	END IF
	/******************************************************************************************/


	SetPointer(Hourglass!)

/*	Can not delete a benefit calculation if there are payments or awards for it.*/
	ls_where	=	"claim_no = " + string(il_claim_no) + " and benefit_calculation_no = " + &
						string(il_benefit_calculation_no)
	ll_result	=	in_bencalcs.f_retrieve_payment_no(ls_where)

	IF ll_result = -1 Then
		Close(parent)
		Return
	ELSEIF ll_result > 0 Then
		MessageBox("Benefit Calculation Module - Validation Error","You cannot delete a Benefit Calculation that has associated Payments",Exclamation!)
		cb_close.SetFocus()
		Return
	END IF


/*	Can not delete a benefit calculation if there are awards for it.*/
	ll_result	=	in_bencalcs.f_retrieve_bencalc_no(il_claim_no,il_benefit_calculation_no)
	IF ll_result = -1 Then
		Close(parent)
		Return
	ELSEIF ll_result > 0 Then
		MessageBox("Benefit Calculation Module - Validation Error","You cannot delete a Benefit Calculation that has associated Awards",Exclamation!)
		cb_close.SetFocus()
		Return
	END IF

/*	Can not delete a benefit calculation if there is benefit entitlement for it.*/
	ll_result	=	in_bencalcs.f_retrieve_benefit_entitlement(il_claim_no,il_benefit_calculation_no)
	IF ll_result = -1 Then
		Close(parent)
		Return
	ELSEIF ll_result > 0 Then
		MessageBox("Benefit Calculation Module - Validation Error","You cannot delete a Benefit Calculation that has Benefit Entitlement",Exclamation!)
		cb_close.SetFocus()
		Return
	END IF


/*	Can not delete a benefit calculation if there are Cost Analysis Options for it.*/
	ll_result	=	in_bencalcs.f_retrieve_cost_analysis_option(il_claim_no,il_benefit_calculation_no)
	IF ll_result = -1 Then
		Close(parent)
		Return
	ELSEIF ll_result > 0 Then
		MessageBox("Benefit Calculation Module - Validation Error","You cannot delete a Benefit Calculation that is used in~r~n" + &
		           "calculating benefit costs for a Cost Analysis option",Exclamation!)
		cb_close.SetFocus()
		Return
	END IF

/*	Cannot delete a regular LTD benefit calculation if it was copied to create another bencalc
*/
	SELECT	Count(*)
	INTO		:ll_result
	FROM		BENEFIT_CALCULATION
	WHERE	claim_no = :il_claim_no
	AND		copied_from_benefit_calc_no = :il_benefit_calculation_no
	USING SQLCA;
	
	SQLCA.nf_handle_error("w_benefits","select count from BENEFIT_CALCULATION","clicked for cb_delete")

	IF ll_result > 0 Then
		MessageBox("Benefit Calculation Module - Validation Error","You cannot delete a Benefit Calculation that was copied~r~n" + &
		           "to create another benefit calculation.",Exclamation!)
		cb_close.SetFocus()
		Return
	END IF

/*	Delete the appropriate records from the datawindows, then update the table.*/


/*	Delete the benefit calculation details.*/

SQLCA.nf_begin_transaction()

	SetPointer(Hourglass!)
	
	dw_benefit_calculation_Details.DeleteRow(1)
	dw_benefit_calculation_Details.Update()
	IF SQLCA.nf_handle_error("w_benefits","dw_benefit_calculation_details","clicked for cb_delete") < 0 THEN
		Close(Parent)
		Return 
	END IF

/*	Delete Frozen Data (if it exists).*/
	ll_result = dw_benefit_calculation_frozen_data.RowCount()
	IF ll_result = 1 THEN
		dw_benefit_calculation_frozen_data.DeleteRow(1)
		dw_benefit_calculation_frozen_data.Update()
		IF SQLCA.nf_handle_error("w_benefits","dw_benefit_calculation_frozen_data","clicked for cb_delete") < 0 THEN
			Close(Parent)
			Return 
		END IF
	END IF

/*	Delete Capable Data (if it exists).*/
	ll_result = dw_benefit_calculation_capable_data.RowCount()
	IF ll_result = 1 THEN
		dw_benefit_calculation_capable_data.DeleteRow(1)
		dw_benefit_calculation_capable_data.Update()
		IF SQLCA.nf_handle_error("w_benefits","dw_benefit_calculation_capable_data","clicked for cb_delete") < 0 THEN
			Close(Parent)
			Return 
		END IF
	END IF

/*	Delete Remunerations (if it exists).*/
	ll_result = dw_benefit_calculation_remunerations.RowCount()
	IF ll_result > 0 THEN
		ll_cntr = ll_result
		Do While ll_cntr > 0
			dw_benefit_calculation_remunerations.DeleteRow(ll_cntr)
			ll_cntr --
		Loop
		dw_benefit_calculation_remunerations.Update()
		IF SQLCA.nf_handle_error("w_benefits","dw_benefit_calculation_remunerations","clicked for cb_delete") < 0 THEN
			Close(Parent)
			Return 	
		END IF
	END IF

/*	Delete Deductions (if it exists).*/
	ll_result = dw_benefit_calculation_deductions.RowCount()
	IF ll_result > 0 THEN
		ll_cntr = ll_result
		Do While ll_cntr > 0 
			dw_benefit_calculation_deductions.DeleteRow(ll_cntr)
			ll_cntr --
		Loop
		dw_benefit_calculation_deductions.Update()
		IF SQLCA.nf_handle_error("w_benefits","dw_benefit_calculation_deductions","clicked for cb_delete") < 0 THEN
			Close(Parent)
			Return 
		END IF
	END IF

	
SetPointer(Arrow!)

/*	Clear the calculations screen.*/
	dw_benefit_calculation_display_computed.Reset()


SQLCA.nf_commit_transaction()


/*	Go back to the benefit calculation list.*/
	cb_list_bencalcs.TriggerEvent(Clicked!)
	
	SetPointer(Arrow!)


end event

type st_window_title from statictext within w_calculation_details
boolean visible = false
integer y = 8
integer width = 2665
integer height = 76
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "     "
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_benefit_calculation_details from u_dw_online within w_calculation_details
integer x = 23
integer y = 80
integer width = 2606
integer height = 700
integer taborder = 20
boolean enabled = false
string dataobject = "d_benefit_calculation_details"
boolean border = false
end type

event itemchanged;call super::itemchanged;//	When ever a value changes, we do a set item so that other scripts will 
//	be able to obtain the value. (Otherwise, we would have to post the event).

//	--------------------------------------------------------------------------------------------------------	|
//	The following table lists valid values for flags according to the accident/recurrence Date:					|
//																																				|
//	Before 1993-01-01					|	Between 1993-01-01 and 1994-12-31	|	On or after 1995-01-01				|
//	-----------------------------	|	-----------------------------------	|	------------------------------	|
//	Transitional	= No or Yes		|	Transitional	= Inapplicable			|	Transitional	= Inapplicable		|
//	Top Up			= No or Yes		|	Top Up			= No or Yes				|	Top Up			= No					|
//											|													|												|
//	--------------------------------------------------------------------------------------------------------	|
// The Top Up flag will be validated against the three day paid flag and the accident/recurrence date in the
// payment module.

//***********************************************************
//	Benefit Calculation Parameters stored in s_window_message
//
//	Claim No								al_doubleparm[1]
//	Opening No							al_doubleparm[2]
//	Benefit Calculation No			al_doubleparm[3]
//	Copy Benefit Calculation No	al_doubleparm[4]
//
//	Transitional Claim Flag			as_stringparm[1]
//	Top-up Flag							as_stringparm[2]
//	Opening Type Code					as_stringparm[3]
//
//	Accident Recurrence Date		adtm_datetimeparm[1]
//	Benefit Start Date				adtm_datetimeparm[2]
//	Effective Date						adtm_datetimeparm[3]
//
//	Read/Update Mode					as_mode 
//
//**********************************************************
INTEGER		li_tax_index_year, li_period_position, li_decimal_number, li_index
INTEGER		li_return, li_found_row, li_year, li_copied_from_effective_year, li_ltd_benefit_calc_no, li_prior_copied_bencalc_no, li_copied_remun_no
LONG			ll_rownum, ll_result, ll_null

STRING		ls_gettext, ls_mod_string, ls_temp_string, ls_calculation_type_code, ls_column, ls_exemption_mismatch
STRING		ls_transitional_claim_flag, ls_top_up_flag, ls_opening_type_code, ls_prev_yrs_capable_type_code
CHAR			lc_check_pen

DATE			ldt_effective_date, ldt_index_date, ldt_sys_date,ldt_null_date
DATETIME	ldt_yearly_factor_date

DECIMAL{2}	ldec_td1_married_exemption, ldec_td1_basic_exemption, ldec_old_td1_married_exemption
DECIMAL{2}	ldec_td1_exemption, ldec_old_td1_basic_exemption
DECIMAL{2}	ldec_prov_basic_exemption, ldec_prov_married_exemption, ldec_prov_exemption
DECIMAL	{2}	ldec_benefit_level_percentage, ldec_prev_year_capable

BOOLEAN	lb_recalculate


ls_opening_type_code = istr_bencalc_parameters.as_stringparm[3]
setnull(ldt_null_date)
lc_check_pen = "N"
ldt_sys_date = DATE(f_server_datetime())

ll_rownum = This.GetRow()
ls_transitional_claim_flag 		= GetItemString(1,"transitional_claim_flag")
ls_calculation_type_code			= GetItemString(1,"calculation_type_code")
ldec_benefit_level_percentage 	= GetItemDecimal(1,"benefit_level_percentage") 
ldt_effective_date 					= Date(GetItemDateTime(1, "effective_from_date"))
SetNull(ll_null)

ls_column = GetColumnName()


CHOOSE CASE GetColumnName()
CASE "effective_from_date" // EFFECTIVE FROM DATE - Validations and Screen Operations
	ls_gettext = GetText()
	ls_gettext = Left(ls_gettext,10)
	ldt_effective_date = Date(ls_gettext)
 
	IF ldt_effective_date < date(istr_bencalc_parameters.adtm_datetimeparm[1]) THEN
		MessageBox("Benefit Calculation Module - Validation Error","The effective date cannot be before the accident/recurrence date",Exclamation!)
		Return 1
	END IF

	ldec_benefit_level_percentage = GetItemDecimal(1,"benefit_level_percentage")
	IF ldec_benefit_level_percentage = .85 and ls_transitional_claim_flag <> "Y" and ls_opening_type_code <> "LTD" and &
	   date(istr_bencalc_parameters.adtm_datetimeparm[1]) < Date("1998/01/01") THEN
		IF ldt_effective_date < RelativeDate(date(istr_bencalc_parameters.adtm_datetimeparm[2]),273) THEN
			MessageBox("Benefit Calculation Module - Validation Error","85% payments can not start until 40 weeks from the benefit start date~r~n" + &
							"The effective date can not be before " + string(RelativeDate(date(istr_bencalc_parameters.adtm_datetimeparm[2]),273),"yyyy-mm-dd"),Exclamation!)
			Return 1
		END IF
	END IF

	// Determine the Taxation Year based on the effective date and the anniversary date of the claim
	wf_determine_tax_index_year(date(istr_bencalc_parameters.adtm_datetimeparm[1]),ldt_effective_date,li_tax_index_year)
 	SetItem(ll_rownum,"index_taxation_year",li_tax_index_year)

	/* If the effective date is prior to 1982-01-01, the calculation type is Manual
	   Periods after this are now calculated automatically  for project P-10105
	*/
	IF ldt_effective_date < Date("1982/01/01") and ls_calculation_type_code <> "M" THEN
		SetItem(ll_rownum,"calculation_type_code","M")
		lb_recalculate = False 
		MessageBox("Benefit Calculation Module - Warning","Calculation Type must be manual when the effective date is before 1982-01-01.~r~n" + &
						"The Calculation Type will be set accordingly.",Information!)
		Return
	END IF

	// Check to see if the Tax Information is available from the Taxation Tables for Automatic Calculations
	// Ed Lenarczyk  - Feb 04, 2000   ***   BEGIN  CHANGES   ***
	//                 Changes to make sure the latest information for the taxation year is used
	//                 Original line commented out	
	If li_tax_index_year = year(ldt_effective_date) THEN
		ldt_index_date = Date(year(ldt_effective_date),month(ldt_effective_date),day(ldt_effective_date))
	ELSE
		ldt_index_date = Date(li_tax_index_year,12,31)
	End If	
	// Ed Lenarczyk  - Feb 04, 2000  ***  END  CHANGES  ***

   // Routine added 1997/12/01 to check to see if claiment is a 'PEN' or 'SV'.... */
  	IF ls_opening_type_code = 'PEN' OR ls_opening_type_code = 'SV'  OR ls_opening_type_code = 'S1' OR ls_opening_type_code = 'S2' THEN
		lc_check_pen = "Y"
	ELSE
		lc_check_pen = "N"
	END IF
	
	IF not in_bencalcs.f_isvalid_tax_year(ldt_index_date) and ls_calculation_type_code <> "M" THEN
   	IF lc_check_pen = "Y" THEN
				/* do nothing.... */
		ELSE
			setitem(1, "effective_from_date",ldt_null_date)
			MessageBox("Benefit Calculation Module - Validation Error","Tax information is not available for the index year " + string(li_tax_index_year),Exclamation!)
			
			Return 1
		END IF
	END IF

	IF lc_check_pen = "Y" AND li_tax_index_year = YEAR(ldt_sys_date) + 1 THEN
		li_tax_index_year = li_tax_index_year - 1
	END IF 
	
		/* for project 10105 "A transitional claim is a claim with an accident/recurrence date prior to Janurary 01,1993
	   The transitional claim flag indicates for claims with an accident/recurrence date prior to January 01, 1993
		whether the benefit level is at 90% or 85% . In the Benefit Calculation module, 
		when the accident/recurrence date prior to 1993-01-01, the transaitional claim flag is set to NO
		when the benefit at 85% passes the frozen benefits. We will modify the benefit calculation to set
		the transitional claim flag to Inapplicable if the transitional claim's benefit effective date 
		is prior to January 01,1993
	*/
	
	/* The Frozen Earnings  are not applicable (and therefore not entered) when
	   (a) The accident/Recurrence Date is prior to Janurary 01, 1993 and
		(b) The benefit effective date is prior to 1993
	*/
	
		in_bencalcs.f_set_index_date(ldt_index_date)
		in_bencalcs.f_populate_work_variables(ldt_effective_date,'')
		dw_benefit_calculation_details.setitem(1,"yearly_factor_date",in_bencalcs.idt_from_yearly_factor)
	
		//grab the static text value - set depending on the accident/effective date combination
		// PR1486 Stephanie Manzer  Sept. 21, 2000
		IF date(idtm_accident_recurrence_date) < Date("1993/01/01") AND ldt_effective_date < Date("1993/01/01") THEN
			IF ib_copy_ben_calc =  true THEN
			ELSE
			dw_benefit_calculation_frozen_data.reset()
			dw_benefit_calculation_frozen_data.SetItem(1,'frozen_cpp_flag','Y')
			dw_benefit_calculation_frozen_data.SetItem(1,'frozen_uic_flag','Y')
			setitem(ll_rownum,"transitional_claim_flag","I")
			SetItem(1, "benefit_level_percentage", .90)
			END IF
			setitem(ll_rownum,"transitional_claim_flag","I")
			dw_benefit_calculation_frozen_data.visible = FALSE
			st_frozen_92_taxyear.visible = False
			st_taxation_period.visible = False
		END IF	
		IF date(idtm_accident_recurrence_date) < Date("1993/01/01") AND ldt_effective_date >= Date("1993/01/01") THEN
			IF ib_detail = false THEN
				IF ib_copy_ben_calc = FALSE THEN
					dw_benefit_calculation_frozen_data.insertrow(0)
					dw_benefit_calculation_frozen_data.SetItem(1,'frozen_cpp_flag','Y')
					dw_benefit_calculation_frozen_data.SetItem(1,'frozen_uic_flag','Y')
				END IF
		  		SetItem(1, "benefit_level_percentage", .90)
			END IF	  
	  	   setitem(1,"transitional_claim_flag","Y")
		   IF uo_tabs.ii_index = 1 THEN
		  		dw_benefit_calculation_frozen_data.visible = TRUE
				st_frozen_92_taxyear.visible = False
				st_taxation_period.visible = False
				IF dw_benefit_calculation_frozen_data.RowCount() = 0 THEN
					dw_benefit_calculation_frozen_data.insertrow(0)
					dw_benefit_calculation_frozen_data.SetItem(1,'frozen_cpp_flag','Y')
					dw_benefit_calculation_frozen_data.SetItem(1,'frozen_uic_flag','Y')
				END IF	
		   END IF
		END IF	
		IF date(idtm_accident_recurrence_date) >= Date("1993/01/01") THEN
		   setitem(ll_rownum,"transitional_claim_flag","I")
			IF ldt_effective_date >= Date("1998/01/01") OR date(idtm_accident_recurrence_date) >= Date("1998/01/01") THEN
		   	SetItem(1, "benefit_level_percentage", .85)
			ELSEIF ldt_effective_date >= Date("1993/01/01") THEN
				SetItem(1, "benefit_level_percentage", '')
			END IF
		   dw_benefit_calculation_frozen_data.reset()
			dw_benefit_calculation_frozen_data.SetItem(1,'frozen_cpp_flag','Y')
				dw_benefit_calculation_frozen_data.SetItem(1,'frozen_uic_flag','Y')
		   dw_benefit_calculation_frozen_data.visible = FALSE	
			st_frozen_92_taxyear.visible = False
			st_taxation_period.visible = False
		END IF


	// If PEN, SV, S1, S2 - set percentage to 100% and calc type to M
	IF lc_check_pen = "Y" THEN
		SetItem(1, "benefit_level_percentage", 1.0)
		SetItem(1, "calculation_type_code", 'M')
	END IF

		
	//	Get the td1 basic and married exemption amount for the index year and build the drop down list box for the user to select
	//	a corresponding td1 exemption amount
	ll_result = in_bencalcs.f_get_exemption_amounts(Date(in_bencalcs.idt_from_yearly_factor),ldec_td1_basic_exemption,ldec_td1_married_exemption,ldec_prov_basic_exemption,ldec_prov_married_exemption)	//sr64

	IF ll_result = -1 THEN
		Close(parent)
		Return
	END IF
	
	ls_mod_string = "td1_exemption_amount.Values='" + string(ldec_td1_basic_exemption,'$#,##0.00') + "~t" + &
							string(ldec_td1_basic_exemption) + "/" + string(ldec_td1_married_exemption,'$#,##0.00') + &
							"~t" + string(ldec_td1_married_exemption) + "'"
	Modify(ls_mod_string)

/*	SR64 - Jan. 2001 Tax Changes - new listbox control added to display new provincial TD1 exemption levels
*/
	ls_mod_string = "prov_td1_exemption_amount.Values='" + string(ldec_prov_basic_exemption,'$#,##0.00') + "~t" + &
							string(ldec_prov_basic_exemption) + "/" + string(ldec_prov_married_exemption,'$#,##0.00') + &
							"~t" + string(ldec_prov_married_exemption) + "'"
	Modify(ls_mod_string)

	//	Determine whether or not to set the TD1 Exemption Amount
	//	1) ADD option 
	//			Set the exemption amount to the basic exemption determined from the tax index year
	//	2)	COPY option 
	//		   If the TD1 Exemption amount field still equals the copied TD1 Exemption amount
	//				If the copied exemption amount was NOT a basic exemption or was NOT a married exemption for that tax year
	//					Display a warning message 
	//		   		Leave the exemption amount that was copied in 
	//				Otherwise (it was either a basic or a married exemption for that Tax Year)
	//					If the current taxation year is NOT the same as the copied taxation year
	//						If the copied exemption is a BASIC exemption
	//							Set the exemption amount to the new basic exemption amount for the new Tax Year
	//						Otherwise
	//							Set the exemption amount to the new married exemption amount for the new Tax Year


	// *****************************************
	// *****************************************
	// *****************************************
	// *****************************************
	// THERE IS A LOT OF QUESTIONABLE CODE BELOW
	// The local variables ldec_old_td1_basic_exemption & ldec_old_td1_married_exemption
	// are never assigned a value, but are used in comparisons.
	// SO, I have commented out the code that should never happen.
	// If changes are being made to this script and no errors have resulted from my changes,
	// then please remove the commmented code.
	// Kevin MacLeod 2007-01-08
	
	IF ii_copy_tax_index_year = 0 THEN
		SetItem(ll_rownum,"td1_exemption_amount",ldec_td1_basic_exemption)
		SetItem(ll_rownum, "prov_td1_exemption_amount", ldec_prov_basic_exemption)	//Added for SR64
	ELSE
		If ii_copy_tax_index_year = year(ldt_effective_date) THEN
			ldt_index_date = Date(year(ldt_effective_date),month(ldt_effective_date),day(ldt_effective_date))
		ELSE
			ldt_index_date = Date(ii_copy_tax_index_year,12,31)
		End If
		
		IF ll_result = -1 THEN
			Close(parent)
			Return
		END IF

		// If the copied exemption amount was a basic exemption
		IF idec_copy_td1_exemption_amount = ldec_old_td1_basic_exemption   THEN
			// SHOULD NEVER HAPPEN because ldec_old_td1_basic_exemption is never set
//				IF ii_copy_tax_index_year <> li_tax_index_year THEN
//					SetItem(ll_rownum,"td1_exemption_amount",ldec_td1_basic_exemption)
//					SetItem(ll_rownum, "prov_td1_exemption_amount", ldec_prov_basic_exemption)	//Added for SR64
//				END IF
		ELSE
			// If the copied exemption amount was a married exemption
			IF idec_copy_td1_exemption_amount = ldec_old_td1_married_exemption THEN
				// SHOULD NEVER HAPPEN because ldec_old_td1_married_exemption is never set
//					IF ii_copy_tax_index_year <> li_tax_index_year THEN
//						SetItem(ll_rownum,"td1_exemption_amount",ldec_td1_married_exemption)
//						SetItem(ll_rownum,"prov_td1_exemption_amount", ldec_prov_married_exemption)	//Added for SR64
//					END IF
			ELSE
//Ed Lenarczyk - begin changes  Feb 07, 2000
//					  Checking the Personal Exemption for more conditions and forcing the user to decide
				IF ldec_old_td1_basic_exemption > 0 and idec_copy_td1_exemption_amount <> GetItemDecimal(1,"td1_exemption_amount") THEN
					// WILL NEVER HAPPEN: ldec_old_td1_basic_exemption = 0
//						IF idec_copy_td1_exemption_amount < ldec_old_td1_basic_exemption THEN
//							If MessageBox("Benefit Calculation Module - Warning","The copied TD1 Exemption amount is smaller than a basic exemption ~r~n" + &
//											  " for the Taxation Year.  Do you want to correct ?",INFORMATION!,YESNO!) = 1 Then
//								dw_benefit_calculation_details.SetColumn("td1_exemption_amount")
//								SetItem(ll_rownum,"td1_exemption_amount",ldec_old_td1_basic_exemption)
//						// Ed Lenarczyk - PR #1325  July 04, 2000  - Return statement deleted from here...
//							End If
//						ELSE
//							If MessageBox("Benefit Calculation Module - Warning","The copied TD1 Exemption amount is not a basic exemption or~r~n" + &
//											  " a married exemption for the Taxation Year.  Do you want to correct ?",INFORMATION!,YESNO!) = 1 Then
//								dw_benefit_calculation_details.SetColumn("td1_exemption_amount")							
//								SetItem(ll_rownum,"td1_exemption_amount",ldec_old_td1_basic_exemption)
//						// Ed Lenarczyk - PR #1325  July 04, 2000  - Return statement deleted from here....
//							End If
//						END IF
				END IF
				IF ib_copy_first = TRUE and idec_copy_td1_exemption_amount <> ldec_td1_basic_exemption THEN
					MessageBox("Warning","The Federal & Provincial TD1 Exemption Amounts have been re-set to the " + &
									+ "~r~nFederal & Provincial TD1 BASIC Exemption Amounts.  Some of the values may " + &
									+ "~r~nhave been revised since the original Benefit Calculation was created.", INFORMATION!, OK!)
					ib_copy_first = FALSE
				END IF					
				dw_benefit_calculation_details.SetColumn("td1_exemption_amount")
				SetItem(ll_rownum,"td1_exemption_amount",ldec_td1_basic_exemption)
				SetItem(ll_rownum,"prov_td1_exemption_amount",ldec_prov_basic_exemption)
			END IF
		END IF
	END IF

	// Always set the gross pay base year and the cppd base year to the tax/index year
	SetItem(ll_rownum,"effective_from_date",ldt_effective_date)
	SetItem(ll_rownum,"index_taxation_year",li_tax_index_year)

	IF GetItemNumber(ll_rownum,"preacc_gross_pay_base_year") <= 0 THEN
		SetItem(ll_rownum,"preacc_gross_pay_base_year",li_tax_index_year)
	END IF
	IF GetItemNumber(ll_rownum,"cppd_base_year") <= 0 THEN
		SetItem(ll_rownum,"cppd_base_year",li_tax_index_year)
	END IF
	IF DATE(st_frozen_92_taxyear.text) <> Date('1992-01-01') AND DATE(st_frozen_92_taxyear.text) <> Date('1992-07-01') THEN
		st_frozen_92_taxyear.Text = "1992-01-01"
		idt_frozen_92_taxyear = DATE('1992-01-01')
	END IF
	
/* *** code added for p10105 
*/
	IF li_tax_index_year = year(ldt_effective_date) THEN
		ldt_index_date = Date(year(ldt_effective_date),month(ldt_effective_date),day(ldt_effective_date))
	ELSE
		ldt_index_date = Date(li_tax_index_year,12,31)
	END IF
	
	in_bencalcs.f_set_index_date(ldt_index_date)
	in_bencalcs.f_populate_work_variables(ldt_effective_date,'')
	dw_benefit_calculation_details.setitem(1,"yearly_factor_date",in_bencalcs.idt_from_yearly_factor)
	dw_benefit_calculation_details.SetItem(1,'release_no',in_bencalcs.ii_release)
	
/* code end added for project p10105
*/

	lb_recalculate = True

CASE "td1_exemption_amount"  // TD1 EXEMPTION AMOUNT - Validations and Screen Operations
	ib_suppress_exemption_msg = FALSE
	ls_gettext = gettext()
	ldec_td1_exemption = Dec(ls_gettext)
//	Get value of federal td1 basic exemption
	ls_temp_string = Describe("td1_exemption_amount.Values")
	ls_temp_string = mid(ls_temp_string,pos(ls_temp_string,"~t") + 1,len(ls_temp_string))
	ls_temp_string = left(ls_temp_string,pos(ls_temp_string,"/") - 1)
	ldec_td1_basic_exemption = Dec(ls_temp_string)
//	Get value of federal td1 married exemption - Added for SR 64: Jan. 2001 tax changes
	ls_temp_string = Describe("td1_exemption_amount.Values")
	ls_temp_string = Right( ls_temp_string, pos(ls_temp_string,"~t") - 1 )
	ls_temp_string = Left( ls_temp_string, pos(ls_temp_string,"/") - 1 )
	ldec_td1_married_exemption = DEC(ls_temp_string)
//	Get value of provincial td1 basic exemption - Added for SR 64: Jan. 2001 tax changes
	ls_temp_string = Describe("prov_td1_exemption_amount.Values")
	ls_temp_string = mid(ls_temp_string,pos(ls_temp_string,"~t") + 1,len(ls_temp_string))
	ls_temp_string = left(ls_temp_string,pos(ls_temp_string,"/") - 1)
	ldec_prov_basic_exemption = Dec(ls_temp_string)
//	Get value of provincial td1 married exemption - Added for SR 64: Jan. 2001 tax changes
	ls_temp_string = Describe("prov_td1_exemption_amount.Values")
	ls_temp_string = Right( ls_temp_string, pos(ls_temp_string,"~t") - 1 )
	ls_temp_string = Left( ls_temp_string, pos(ls_temp_string,"/") - 1 )
	ldec_prov_married_exemption = DEC(ls_temp_string)

/*	Get value of current provincial td1 exemption - Added for SR 64: Jan. 2001 tax changes
*/
	ldec_prov_exemption = GetItemDecimal(ll_rownum, "prov_td1_exemption_amount")
	
/*	Check to make sure that if default basic or married exemptions are being used, then provincial and federal
	exemptions are of the same type (one can't be married while the other is basic) - SR 64: Jan. 2001 tax changes
*/
	IF (ldec_td1_exemption = ldec_td1_basic_exemption) AND (ldec_prov_exemption = ldec_prov_married_exemption) THEN
	// set provincial exemption to be provincial basic exemption (to match federal basic exemption type)
		li_return = dw_benefit_calculation_details.SetItem(ll_rownum, "prov_td1_exemption_amount", ldec_prov_basic_exemption)
	ELSEIF (ldec_td1_exemption = ldec_td1_married_exemption) AND (ldec_prov_exemption = ldec_prov_basic_exemption) THEN
	// set provincial exemption to be provincial married exemption (to match federal married exemption type)
		li_return = dw_benefit_calculation_details.SetItem(ll_rownum, "prov_td1_exemption_amount", ldec_prov_married_exemption)
	END IF	

/*	Check to make sure that federal exemption is at least equal to the federal basic personal exemption ( SR 64 )
	NOTE:	In very odd cases zero amount for the personal exemption may be allowed, therefore must not trigger error
			message if Fed. TD1 = $0
*/
	IF ( ldec_td1_exemption < ldec_td1_basic_exemption ) AND (ldec_td1_exemption <> 0 ) THEN
		ldt_yearly_factor_date = GetItemDateTime(1, "yearly_factor_date")
		li_return = Messagebox('Invalid Federal TD1 Exemption', 'The Federal TD1 Exemption must be equal to ' + &
		+ 'or greater than the federal basic personal exemption.~r~nThe federal basic personal exemption ' + &
		+ 'for the taxation period ' + String(Date(ldt_yearly_factor_date)) + ' is $' + String(ldec_td1_basic_exemption) )
	END IF

/*	[PR 2508, June 2002] If a manual TD1 amount has been entered & the tax year is 2001 (the year that
	 the provincial TD1 came into effect) or later, then:

		a)	If the federal TD1 amount is greater than the federal basic amount but less than the
			federal married amount, then the prov. TD1 amount must also be greater than the prov.
			basic amount and less than the prov. married amount.  Set prov_td1_exemption_amount to
			the prov. basic amount.
			
		b)	If the federal TD1 amount is greater than the federal married amount, then the prov. TD1
			must be greater than the prov. married amount.  Set prov_td1_exemption_amount to
			the prov. married amount.
*/
	IF Year( ldt_effective_date ) >= 2001 THEN
		IF ldec_td1_exemption > ldec_td1_basic_exemption AND ldec_td1_exemption < ldec_td1_married_exemption THEN
			IF ldec_prov_exemption >= ldec_prov_married_exemption THEN
				dw_benefit_calculation_details.SetItem(ll_rownum, "prov_td1_exemption_amount", ldec_prov_basic_exemption)				
			END IF
		ELSEIF ldec_td1_exemption > ldec_td1_married_exemption THEN
			IF ldec_prov_exemption < ldec_prov_married_exemption THEN
				dw_benefit_calculation_details.SetItem(ll_rownum, "prov_td1_exemption_amount", ldec_prov_married_exemption)
			END IF
		END IF
	END IF

// Ed Lenarczyk - begin changes  Feb 07, 2000
//					   Checking the Personal Exemption for more conditions and forcing the user to decide
	IF ldec_old_td1_basic_exemption > 0 THEN
		IF idec_copy_td1_exemption_amount < ldec_old_td1_basic_exemption THEN
			If MessageBox("Benefit Calculation Module - Warning","The TD1 Exemption amount is smaller than a basic exemption ~r~n" + &
						" for the Taxation Year.  Do you want to correct ?",INFORMATION!,YESNO!) = 1 Then
				dw_benefit_calculation_details.setColumn("td1_exemption_amount")
				SetItem(ll_rownum,"td1_exemption_amount",ldec_old_td1_basic_exemption)
			// Ed Lenarczyk - PR #1325  July 04, 2000  - Return statement deleted from here....
			End If
		ELSE
			If MessageBox("Benefit Calculation Module - Warning","The copied TD1 Exemption amount is not a basic exemption or~r~n" + &
						" a married exemption for the Taxation Year.  Do you want to correct ?",INFORMATION!,YESNO!) = 1 Then
				dw_benefit_calculation_details.SetColumn("td1_exemption_amount")							
				SetItem(ll_rownum,"td1_exemption_amount",ldec_old_td1_basic_exemption)
			// Ed Lenarczyk - PR #1325  July 04, 2000  - Return statement deleted from here....
			End If
		END IF
	END IF

/*	Check to make sure that federal exemption is at least equal to the federal basic personal exemption ( SR 64 )
*/
	IF ( ldec_td1_exemption < ldec_td1_basic_exemption ) THEN
		ldt_yearly_factor_date = GetItemDateTime(1, "yearly_factor_date")
		li_return = Messagebox('Invalid Federal TD1 Exemption', 'The Federal TD1 Exemption must be equal to ' + &
		+ 'or greater than the federal basic personal exemption.~r~nThe federal basic personal exemption ' + &
		+ 'for the taxation period ' + String(Date(ldt_yearly_factor_date)) + ' is $' + String(ldec_td1_basic_exemption) )
	END IF
	
CASE "prov_td1_exemption_amount"  // Provincial TD1 Exemption - Added for SR 64: Jan. 2001 tax changes
	ib_suppress_exemption_msg = FALSE
	ls_gettext = gettext()
	ldec_prov_exemption = DEC(ls_gettext)
//	Get value of federal td1 basic exemption
	ls_temp_string = Describe("td1_exemption_amount.Values")
	ls_temp_string = mid(ls_temp_string,pos(ls_temp_string,"~t") + 1,len(ls_temp_string))
	ls_temp_string = left(ls_temp_string,pos(ls_temp_string,"/") - 1)
	ldec_td1_basic_exemption = Dec(ls_temp_string)
//	Get value of federal td1 married exemption
	ls_temp_string = Describe("td1_exemption_amount.Values")
	ls_temp_string = Right( ls_temp_string, pos(ls_temp_string,"~t") - 1 )
	ls_temp_string = Left( ls_temp_string, pos(ls_temp_string,"/") - 1 )
	ldec_td1_married_exemption = DEC(ls_temp_string)
//	Get value of provincial td1 basic exemption
	ls_temp_string = Describe("prov_td1_exemption_amount.Values")
	ls_temp_string = mid(ls_temp_string,pos(ls_temp_string,"~t") + 1,len(ls_temp_string))
	ls_temp_string = left(ls_temp_string,pos(ls_temp_string,"/") - 1)
	ldec_prov_basic_exemption = Dec(ls_temp_string)
//	Get value of provincial td1 married exemption
	ls_temp_string = Describe("prov_td1_exemption_amount.Values")
	ls_temp_string = Right( ls_temp_string, pos(ls_temp_string,"~t") - 1 )
	ls_temp_string = Left( ls_temp_string, pos(ls_temp_string,"/") - 1 )
	ldec_prov_married_exemption = DEC(ls_temp_string)

/*	Get value of current federal td1 exemption - Added for SR 64: Jan. 2001 tax changes
*/
	ldec_td1_exemption = GetItemDecimal(ll_rownum, "td1_exemption_amount")

/*	Check to make sure that if default basic or married exemptions are being used, then provincial and federal
	exemptions are of the same type (one can't be married while the other is basic) - SR 64: Jan. 2001 tax changes
*/
	IF (ldec_prov_exemption = ldec_prov_basic_exemption) AND (ldec_td1_exemption = ldec_td1_married_exemption) THEN
	// set federal exemption to be federal basic exemption (to match provincial basic exemption type)
		li_return = dw_benefit_calculation_details.SetItem (ll_rownum, "td1_exemption_amount", ldec_td1_basic_exemption)
	ELSEIF (ldec_prov_exemption = ldec_prov_married_exemption) AND (ldec_td1_exemption = ldec_td1_basic_exemption) THEN
	// set federal exemption to be federal married exemption (to match provincial married exemption type)
		li_return = dw_benefit_calculation_details.SetItem (ll_rownum, "td1_exemption_amount", ldec_td1_married_exemption)
	END IF

/*	Check to make sure that provincial exemption is at least equal to the provincial basic personal exemption
	( SR 64: Jan. 2001 tax changes ) - NOTE:  For years prior to 2001, $0 is default value for provincial exemption
															(defaulted since provincial exemptions didn't exist prior to Jan. 2001).
*/
	ldt_yearly_factor_date = GetItemDateTime(1, "yearly_factor_date")
	
	IF ldec_prov_exemption < ldec_prov_basic_exemption THEN
		IF ldec_prov_exemption = 0 AND Year(Date(ldt_yearly_factor_date)) < 2001 THEN // $0 is value for prov. exemption prior to Jan. 2001
		ELSE 
			li_return = Messagebox('Invalid Provincial TD1 Exemption', 'The Provincial TD1 Exemption must be equal to ' + &
			+ 'or greater than the provincial basic personal exemption.~r~r~nThe provincial basic personal exemption ' + &
			+ 'for the taxation period ' + String(Date(ldt_yearly_factor_date)) + ' is $' + String(ldec_prov_basic_exemption))
			li_return = dw_benefit_calculation_details.SetItem(ll_rownum, "prov_td1_exemption_amount", ldec_prov_basic_exemption)
		END IF
	END IF	

/*	[PR 2508, June 2002] If a manual TD1 amount has been entered & the tax year is 2001 or later, then:
   
	PR4097 - J. Hawker - Removed code that validates the exemption amounts - replaced with a call to the 
	function wf_exemption_mismatch. 
*/

	ls_exemption_mismatch = wf_exemption_mismatch (ldec_td1_exemption,ldec_td1_basic_exemption, ldec_td1_married_exemption, &
		ldec_prov_exemption, ldec_prov_basic_exemption, ldec_prov_married_exemption, DATE(ldt_yearly_factor_date))
	
	IF ls_exemption_mismatch <> 'NONE' THEN
		RETURN -1
	END IF

	
CASE "benefit_level_percentage"   // BENEFIT LEVEL PERCENTAGE - Validations and Screen Operations

	ls_gettext = gettext()
	ldec_benefit_level_percentage  = Dec(ls_gettext)

	IF ls_transitional_claim_flag <> "Y" and ldec_benefit_level_percentage = .85 and ls_opening_type_code <> "LTD" and &
	   date(istr_bencalc_parameters.adtm_datetimeparm[1]) < Date("1998/01/01") THEN  
		IF ldt_effective_date < RelativeDate(date(istr_bencalc_parameters.adtm_datetimeparm[2]),273) THEN
			MessageBox("Benefit Calculation Module - Validation Error","85% payments can not start until 40 weeks from the benefit start date~r~n" + &
							"The benefit level can not be set to 85% without changing the effective date to " + string(RelativeDate(date(istr_bencalc_parameters.adtm_datetimeparm[2]),273),"yyyy-mm-dd"),Exclamation!)
			Return 1
		END IF	
	END IF

	// Prior to Janaury 01, 1993 and after January 01, 1998,the benefit level of 80% is not valid for RLOE and LTD benefits
	IF (date(istr_bencalc_parameters.adtm_datetimeparm[1]) < Date("1993/01/01") or date(istr_bencalc_parameters.adtm_datetimeparm[1]) >= Date("1998/01/01")) &
		AND ldec_benefit_level_percentage = .80  AND (ls_opening_type_code = "LTD" or ls_opening_type_code = "RLOE") THEN
		MessageBox("Benefit Calculation Module - Validation Error","80% payments of RLOE or LTD benefits are not valid for Claims with an  ~r~n" + &
					 "Accident/Recurrence Date prior to January 01, 1993 or after December 31, 1997",Exclamation!)
		Return 1
	END IF	

	lb_recalculate = True

CASE "calculation_type_code" // CALCULATION TYPE CODE - Validations and Screen Operations
	ls_calculation_type_code = gettext()
	/* the following code has been altered in order to allow for calculation of benefits
	   for pre 93 accidents P10105
	*/
	IF ldt_effective_date < Date("1982/01/01") and ls_calculation_type_code <> "M" THEN
		SetItem(ll_rownum,"calculation_type_code","M")
		lb_recalculate = False 
		MessageBox("Benefit Calculation Module - Warning","Calculation Type must be manual when the effective date is before 1993-01-01.~r~n" + &
						"The Calculation Type will be set accordingly.",Information!)
		Return
	ELSEIF ls_calculation_type_code = "A" THEN
		SetItem(ll_rownum,"calculation_type_code",ls_gettext)
		lb_recalculate = True
	END IF
	//end of code rem the following is code added from above
	IF ls_calculation_type_code = "A" THEN
		SetItem(ll_rownum,"calculation_type_code",ls_gettext)
		lb_recalculate = True
	END IF

CASE "preacc_work_days_per_week"  // PAID_DAYS_LOST - Validations and Screen Operations

	// If a decimal value is entered, it must be of .25, .5, or .75 (i.e. should be 
	//	divisable by 5 as we should not pay for .33 days or something stupid like that)
	li_period_position = Pos(GetText(),".")
	IF li_period_position > 0 THEN
		li_decimal_number = Integer(Right(GetText(),Len(GetText()) - li_period_position))
		IF li_decimal_number <> 0 and li_decimal_number <> 25 and li_decimal_number <> 5 and li_decimal_number <> 50 and li_decimal_number <> 75 THEN
			MessageBox("Benefit Calculation Module - Validation Error","Days Lost cannot be broken down in units less than .25, .5, or .75",Exclamation!) 
         return 1
		END IF
	END IF

CASE "paid_hours_lost" // PAID_HOURS_LOST - Validations and Screen Operations

	// If a decimal value is entered, it must be of .25, .5, or .75 (i.e a multiple of 5 as we should not pay 
	//	for .33 hours or something stupid like that)
	li_period_position = Pos(GetText(),".")
	IF li_period_position > 0 THEN
		li_decimal_number = Integer(Right(GetText(),Len(GetText()) - li_period_position))
		IF li_decimal_number <> 0 AND li_decimal_number <> 25 AND li_decimal_number <> 5 AND li_decimal_number <> 50 AND li_decimal_number <> 75 THEN
			MessageBox("Benefit Calculation Module - Validation Error","Hours Lost cannot be broken down in units less than .25, .5, or .75") 
         return 1
		END IF
	END IF

CASE "top_up_flag"
	//	Top-Up flag can be either YES or NO depending on the Accident/Recurrence Date of the opening
	//
	//  	Acc/Recurrence Date		TOP-UP
	//	-------------------		----------
	// 	>=  1995-01-01				NO
	//	< 1995-01-01				YES or NO
	//
	ls_top_up_flag = GetText()
	
	IF date(istr_bencalc_parameters.adtm_datetimeparm[1])  >= Date("1995-01-01") THEN
		IF ls_top_up_flag <> "N" THEN
			MessageBox("Benefit Calculation Module - Validation Error","The top-up flag must be 'No' if the accident/recurrence Date" + &
							" is on or after 1995-01-01",Exclamation!)
			Return 1
		END IF
	END IF

CASE "transitional_claim_flag"
	// TRANSITIONAL CLAIM FLAG - Validations and Screen Operations                                          */
	// 	If the Transitional Claim Flag is set to YES, the frozen data window should be displayed. If it   */
	//    is set to NO, the Frozen Data should be cleared and the window not displayed.                     */
	//
	//  	Acc/Recurrence Date		TRANSITIONAL
	//	-------------------		--------------
	// 	>=  1993-01-01				INAPPLICABLE
	//	< 1993-01-01				YES or NO
	ls_transitional_claim_flag = GetText()
		
	IF date(istr_bencalc_parameters.adtm_datetimeparm[1]) >= Date("1993-01-01") THEN
		IF ls_transitional_claim_flag <> "I" THEN
			MessageBox("Benefit Calculation Module - Validation Error","The transitional claim flag must be 'Inapplicable' if the Accident/Recurrence Date~r~n" + &
							" is on or after 1993-01-01",Exclamation!)
			setitem(1,"transitional_claim_flag","I")
			Return 1
		END IF
	END IF
	
	/* for project 10105 "A transitional claim is a claim with an accident/recurrence date prior to Janurary 01,1993
	   The transitional claim flag indicates for claims with an accident/recurrence date prior to January 01, 1993
		whether the benefit level is at 90% or 85% . In the Benefit Calculation module, 
		when the accident/recurrence date prior to 1993-01-01, the transaitional claim flag is set to NO
		when the benefit at 85% passes the frozen benefits. We will modify the benefit calculation to set
		the transitional claim flag to Inapplicable if the transaitional claim's benefit effective date 
		is prior to January 01,1993
	*/
	
	/* The Frozen Earnings  are not applicable (and therefore not entered) when
	   (a) The accident/Recurrence Date is prior to Janurary 01, 1993 and
		(b) The benefit effective date is prior to 1993
	*/
	IF ldt_effective_date <= Date("1993/01/01") AND in_bencalcs.id_index_date <= Date("1993/01/01") THEN
		IF ls_transitional_claim_flag <> "I" THEN
			MessageBox("Benefit Calculation Module - Validation Error","The transitional claim flag must be 'Inapplicable' if both the Accident/Recurrence Date~r~n" + &
							" and the effective date are before 1993-01-01",Exclamation!)
			Return 1
		END IF
	END IF
	
	IF ldt_effective_date < Date("1993/01/01") THEN
		//grab the static text value
		IF in_bencalcs.id_index_date < Date("1993/01/01") THEN
			IF uo_tabs.ii_index = 1 THEN
				dw_benefit_calculation_frozen_data.reset()
				dw_benefit_calculation_frozen_data.SetItem(1,'frozen_cpp_flag','Y')
				dw_benefit_calculation_frozen_data.SetItem(1,'frozen_uic_flag','Y')
				dw_benefit_calculation_frozen_data.visible = FALSE
				st_frozen_92_taxyear.visible = False
				st_taxation_period.visible = False
			END IF
		END IF	
	END IF
	
	// Determine if Frozen Data should be displayed. It is only displayed if the benefit calculation has
	//	the transitional claim flag set on.
	IF ls_transitional_claim_flag = "Y" THEN
		IF ls_opening_type_code = 'PEN' OR ls_opening_type_code = 'SV'  OR ls_opening_type_code = 'S1' OR ls_opening_type_code = 'S2' THEN
			dw_benefit_calculation_details.SetItem(1, 'benefit_level_percentage',1.0)
		ELSE
			IF ldt_effective_date >= Date("1993/01/01") AND date(istr_bencalc_parameters.adtm_datetimeparm[1]) < Date('1993-01-01') THEN
				dw_benefit_calculation_details.SetItem(1, 'benefit_level_percentage',.90)
			END IF
		END IF
		IF uo_tabs.ii_index = 1 THEN
			dw_benefit_calculation_frozen_data.visible = True
				st_frozen_92_taxyear.visible = False
				st_taxation_period.visible = False
		END IF
		dw_benefit_calculation_frozen_data.InsertRow(0)
		dw_benefit_calculation_frozen_data.SetItem(1,'frozen_cpp_flag','Y')
		dw_benefit_calculation_frozen_data.SetItem(1,'frozen_uic_flag','Y')
		dw_benefit_calculation_frozen_data.Enabled = True
	ELSE
		IF ls_opening_type_code = 'PEN' OR ls_opening_type_code = 'SV'  OR ls_opening_type_code = 'S1' OR ls_opening_type_code = 'S2' THEN
			dw_benefit_calculation_details.SetItem(1, 'benefit_level_percentage',1.0)
		ELSE
			IF ls_transitional_claim_flag = 'N' THEN
				IF ldt_effective_date >= Date("1993/01/01") AND date(istr_bencalc_parameters.adtm_datetimeparm[1]) < Date('1993-01-01') THEN
					dw_benefit_calculation_details.SetItem(1, 'benefit_level_percentage',.85)
				END IF
			END IF
		END IF
		dw_benefit_calculation_frozen_data.DeleteRow(0)
		
		
      dw_benefit_calculation_frozen_data.UPDATE()
		IF SQLCA.nf_handle_error("w_calculation_details","dw_benefit_calculation_frozen_data","itemchanged for dw") < 0 THEN
			Close(Parent)
			Return
		END IF

				
		IF uo_tabs.ii_index = 1 THEN
			dw_benefit_calculation_display_computed.visible = False
			dw_benefit_calculation_remunerations.visible 	= False
			dw_benefit_calculation_deductions.visible 		= False
			cb_add_earnings.visible									= False
			cb_prev.visible											= False
			cb_next.visible											= False
			dw_benefit_calculation_frozen_data.visible 		= False
			st_frozen_92_taxyear.visible = False
			st_taxation_period.visible = False
		END IF
	END IF
	
CASE 'rtw_incentive_flag'
	// P10261 RTW Incentive project
	
	IF String(data) = 'Y' THEN
		
		IF IsNull(ldt_effective_date) OR ldt_effective_date = Date('1900-01-01') THEN
			wf_set_effective_date(dwo)

		ELSE
			IF ldt_effective_date <> Date(istr_bencalc_parameters.adtm_datetimeparm[3]) THEN
				IF MessageBox('RTW Incentive Error','The effective date for a RTW Incentive benefit calculation must match' +&
															'~nthe effective date for the copied regular LTD benefit calculation.' +&
															'~nWould you like to have the date set appropriately?',Question!,YesNo!,1) = 2 THEN
					RETURN 2
				ELSE
					wf_set_effective_date(dwo)

				END IF
			END IF
		END IF
		
		// check BRs
		ll_result = wf_check_RTW_bus_rules()
		IF ll_result <> 0 THEN RETURN ll_result
		
		// IF copied LTD bencalc only has inserted deduction row
		// then populate as RTW-LTD column
		
		// otherwise, add another deduction row where the
		// LTD deduction matches on amount.
		
		PARENT.SetRedraw(FALSE)
		// we are potentially changing tabs below, so the redraw was disabled
		IF istr_bencalc_parameters.al_doubleparm[6] = 0 THEN // count of copied deductions was zero
			wf_set_rtw_deduction(1)
		ELSE
			uo_tabs.uf_getindex(li_index)
			uo_tabs.uf_setindex(3)
			cb_add_earnings.TriggerEvent(Clicked!)
			ll_rownum = dw_benefit_calculation_deductions.GetRow()
			dw_benefit_calculation_deductions.SetItem(ll_rownum,'added_for_rtw','yes')
			wf_set_rtw_deduction(ll_rownum)
			uo_tabs.uf_setindex(li_index)
		END IF
		dw_benefit_calculation_deductions.Enabled = FALSE
		
		// set the RTW Incentive capable earnings data to match previous years LTD bencalc
		wf_set_rtw_capable()
		dw_benefit_calculation_capable_data.Enabled = FALSE
	
		
		IF istr_bencalc_parameters.al_doubleparm[7] = 0 THEN // count of copied remunerations was zero
			// one inserted anyway, so set the values
			wf_set_rtw_remuneration(1)
		ELSE
			SELECT	remun_no
			INTO		:li_copied_remun_no
			FROM		REMUNERATION
			WHERE	claim_no = :il_claim_no
			AND		benefit_calculation_no = :istr_bencalc_parameters.al_doubleparm[4]
			AND		remun_type_code = 'WAGE'
			USING SQLCA;
			
			SQLCA.nf_handle_error("w_calculation_details","dw_benefit_calculation_details - itemchanged","select remun_gross_pay from REMUNERATIONS")
			
			// look for one that matches value in copied reg LTD
			// if found, then change amount to match capable value that was set above.
			li_found_row = dw_benefit_calculation_remunerations.Find('remun_no = ' + String(li_copied_remun_no) , 1, dw_benefit_calculation_remunerations.RowCount())
			IF li_found_row > 0 THEN
				wf_set_rtw_remuneration (li_found_row)
			ELSE
				// if not found, then add a record and set the values.
				uo_tabs.uf_getindex(li_index)
				uo_tabs.uf_setindex(4)
				cb_add_earnings.TriggerEvent(Clicked!)
				ll_rownum = dw_benefit_calculation_remunerations.GetRow()
				wf_set_rtw_remuneration (ll_rownum)
				uo_tabs.uf_setindex(li_index)
			END IF
			dw_benefit_calculation_remunerations.Enabled = FALSE
		END IF
		
		// disable other datawindows
		dw_benefit_calculation_preaccident_data.Enabled = FALSE
		dw_benefit_calculation_frozen_data.Enabled = FALSE
		dw_benefit_calculation_cppd.Enabled = FALSE
		
		cb_add_earnings.Enabled = FALSE
		
		PARENT.SetRedraw(TRUE)

	ELSE
		MessageBox('RTW Incentive Checkbox', 'The RTW Incentive box was unchecked. If you want to proceed with a RTW Incentive ' &
													+	'~nbenefit calculation, you must check the RTW Incentive box ON and then SAVE.' &
													+	'~n~nIf not, you must CANCEL the benefit calculation and start again as Capable Earnings,' &
													+	'~nRemunerations & Deductions have been specifically set for RTW Incentive benefits.' ,Exclamation!)
		// unchecking check box
		dw_benefit_calculation_preaccident_data.Enabled = TRUE
		dw_benefit_calculation_frozen_data.Enabled = TRUE
		dw_benefit_calculation_capable_data.Enabled = TRUE
		dw_benefit_calculation_cppd.Enabled = TRUE
		dw_benefit_calculation_deductions.Enabled = TRUE
		dw_benefit_calculation_remunerations.Enabled = TRUE
		ids_rtw_incentive_linked_bencalcs.Reset()
		
		cb_add_earnings.Enabled = TRUE
		
	END IF
	
END CHOOSE

dw_benefit_calculation_details.AcceptText()

// If the Benefit Calculation was copied in, re-calculate the Pre-Accident Net Earnings since the
//	calculation detail changes can affect the pre-acc net 
IF ib_copy_ben_calc = True THEN
	wf_compute_earnings("PreAccident")
END IF

wf_recalculate_all()

end event

event ue_print;LONG ll_job

	ll_job = PrintOpen()
	PrintDataWindow(ll_job,dw_benefit_calculation_details)
	IF dw_benefit_calculation_preaccident_data.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_preaccident_data) 
	END IF
	IF dw_benefit_calculation_frozen_data.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_frozen_data)
	END IF
	IF dw_benefit_calculation_capable_data.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_capable_data) 
	END IF
	IF dw_benefit_calculation_deductions.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_deductions)
	END IF
	IF dw_benefit_calculation_cppd.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_cppd)
	END IF
	IF dw_benefit_calculation_remunerations.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_remunerations) 
	END IF
	IF dw_benefit_calculation_display_computed.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_display_computed)
	END IF

	PrintClose(ll_job)

end event

event editchanged;call super::editchanged;CHOOSE CASE GetColumnName()
CASE "td1_exemption_amount","prov_td1_exemption_amount"  
	ib_suppress_exemption_msg = FALSE
END CHOOSE

end event

type cb_next from commandbutton within w_calculation_details
integer x = 2322
integer y = 1604
integer width = 247
integer height = 96
integer taborder = 150
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Next >"
end type

event clicked;INTEGER	li_index


// If the tab index is 3, we are processing deductions, otherwise, we are processing remunerations

li_index	=	uo_tabs.ii_index

IF li_index = 3 THEN		
	dw_benefit_calculation_deductions.ScrollNextRow()

ELSEIF li_index = 4 THEN
	dw_benefit_calculation_remunerations.ScrollNextRow()
	
END IF

end event

type cb_prev from commandbutton within w_calculation_details
integer x = 1166
integer y = 1604
integer width = 247
integer height = 96
integer taborder = 130
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "< &Prev"
end type

event clicked;INTEGER	li_index


// If the tab index is 3, we are processing deductions, otherwise, we are processing remunerations

li_index	=	uo_tabs.ii_index

IF li_index = 3 THEN
	dw_benefit_calculation_deductions.ScrollPriorRow()

ELSEIF li_index = 4 THEN
	dw_benefit_calculation_remunerations.ScrollPriorRow()

END IF

end event

type st_taxation_period from statictext within w_calculation_details
integer x = 1829
integer y = 968
integer width = 439
integer height = 64
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Taxation Period:"
boolean focusrectangle = false
end type

type st_frozen_92_taxyear from statictext within w_calculation_details
integer x = 2254
integer y = 968
integer width = 302
integer height = 64
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 79741120
boolean enabled = false
string text = "none"
alignment alignment = right!
boolean focusrectangle = false
end type

type cb_add_earnings from commandbutton within w_calculation_details
integer x = 1408
integer y = 1604
integer width = 457
integer height = 96
integer taborder = 140
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Add Deduction"
end type

event clicked;/*	Declare Variables  
	
*/

INTEGER	li_index
LONG		ll_currow,		ll_rowcount


li_index	=	uo_tabs.ii_index


// If the tab index is 3, we are processing deductions, otherwise, we are processing remunerations


	IF li_index = 3 THEN

		// Before creating a new row, check to see if the current record is ok

			IF dw_benefit_calculation_deductions.AcceptText() < 0 THEN
				dw_benefit_calculation_deductions.SetFocus()
				return
			END IF


		// Add a new row and move to it

			ll_currow	=	dw_benefit_calculation_deductions.InsertRow(0)
			IF ll_currow = -1 THEN
				Error.Text = "Error inserting record into dw_benefit_calculation_deductions"
				Error.WindowMenu="w_benefits"
				Error.Object="cb_add_earnings"
				Error.ObjectEvent="clicked"
				SignalError()
			END IF

			dw_benefit_calculation_deductions.SetItem(ll_currow,"deduction_no",ll_currow)
			dw_benefit_calculation_deductions.ScrollToRow(ll_currow)


	ELSEIF li_index = 4 THEN

		// Before creating a new row, check to see IF the current record is ok

			IF dw_benefit_calculation_remunerations.AcceptText() < 0 THEN
				dw_benefit_calculation_remunerations.SetFocus()
				return
			END IF


		// Add a new row and move to it

			ll_currow	=	dw_benefit_calculation_remunerations.InsertRow(0)
			IF ll_currow = -1 THEN
				Error.Text = "Error inserting record into dw_benefit_calculation_remunerations"
				Error.WindowMenu="w_benefits"
				Error.Object="cb_add_earnings"
				Error.ObjectEvent="clicked"
				SignalError()
			END IF

			dw_benefit_calculation_remunerations.SetItem(ll_currow,"remun_no",ll_currow)
			dw_benefit_calculation_remunerations.ScrollToRow(ll_currow)


	END IF




end event

type dw_benefit_calculation_capable_data from u_dw_online within w_calculation_details
integer x = 69
integer y = 876
integer width = 2496
integer height = 828
integer taborder = 50
boolean enabled = false
string dataobject = "d_benefit_calculation_capable_data"
boolean border = false
end type

event itemchanged;call super::itemchanged;//	When ever a value changes, we do a set item so that other scripts will 
//	be able to obtain the value. (Otherwise, we would have to post the event).


STRING	ls_column

ls_column = this.GetcolumnName()

CHOOSE CASE ls_column
	CASE "pre93_capable_gross_pay"
		IF LONG(data) < 0 THEN
			MessageBox("Benefit Calculation Module - Validation Error","The Earnings Amount must be positive.~r~nPlease re-enter",Exclamation!)
			Return 1
		ELSE
			SetItem(1,ls_column,dec(gettext()))
		END IF
	CASE ELSE
		SetItem(1,ls_column,gettext())
END CHOOSE


wf_compute_earnings("Capable")
wf_calculate_award()

end event

event ue_print;LONG ll_job

	ll_job = PrintOpen()
	PrintDataWindow(ll_job,dw_benefit_calculation_details)
	IF dw_benefit_calculation_preaccident_data.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_preaccident_data) 
	END IF
	IF dw_benefit_calculation_frozen_data.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_frozen_data)
	END IF
	IF dw_benefit_calculation_capable_data.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_capable_data) 
	END IF
	IF dw_benefit_calculation_deductions.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_deductions)
	END IF
	IF dw_benefit_calculation_cppd.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_cppd)
	END IF
	IF dw_benefit_calculation_remunerations.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_remunerations) 
	END IF
	IF dw_benefit_calculation_display_computed.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_display_computed)
	END IF

	PrintClose(ll_job)

end event

type dw_benefit_calculation_display_computed from u_dw_online within w_calculation_details
integer x = 69
integer y = 876
integer width = 2505
integer height = 828
integer taborder = 110
string dataobject = "d_benefit_calculation_display_computed"
boolean border = false
end type

event ue_print;LONG ll_job

	ll_job = PrintOpen()
	PrintDataWindow(ll_job,dw_benefit_calculation_details)
	IF dw_benefit_calculation_preaccident_data.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_preaccident_data) 
	END IF
	IF dw_benefit_calculation_frozen_data.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_frozen_data)
	END IF
	IF dw_benefit_calculation_capable_data.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_capable_data) 
	END IF
	IF dw_benefit_calculation_deductions.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_deductions)
	END IF
	IF dw_benefit_calculation_cppd.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_cppd)
	END IF
	IF dw_benefit_calculation_remunerations.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_remunerations) 
	END IF
	IF dw_benefit_calculation_display_computed.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_display_computed)
	END IF

	PrintClose(ll_job)

end event

on rowfocuschanged;//	This is a place holder to override the ancestor script
end on

on itemerror;//	This is a place holder to override the ancestor script
end on

event buttonclicked;call super::buttonclicked;S_WINDOW_MESSAGE    lstr_bencalc


IF dwo.name = 'b_legend' THEN
	lstr_bencalc.al_doubleparm[1] = il_claim_no
	lstr_bencalc.al_doubleparm[2] = il_opening_no
	lstr_bencalc.al_doubleparm[3] = il_benefit_calculation_no
	
	lstr_bencalc.adtm_datetimeparm[1] = dw_benefit_calculation_details.GetItemDatetime(1,'effective_from_date')	
	lstr_bencalc.as_stringparm[1]     = dw_benefit_calculation_details.GetItemString(1,'calculation_type_code')
	IF cb_save.Enabled = TRUE THEN
		lstr_bencalc.as_stringparm[2] = 'Y'                                                                                   // draft_flag = Y
	ELSE
		lstr_bencalc.as_stringparm[2] = 'N'                                                                                   // draft_flag = N
	END IF
	
	lstr_bencalc.adec_decimalparm[1] = dw_benefit_calculation_preaccident_data.GetItemDecimal(1,'avg_net_pay')               // average_net_earnings
	
	IF dw_benefit_calculation_capable_data.RowCount() = 1 THEN
		lstr_bencalc.adec_decimalparm[2] = dw_benefit_calculation_capable_data.GetItemDecimal(1,'pre93_capable_net_pay')      // capable_net
	ELSE
		lstr_bencalc.adec_decimalparm[2] = 0
	END IF
	
	lstr_bencalc.adec_decimalparm[3] = dw_benefit_calculation_details.GetItemDecimal(1,'benefit_level_percentage')        // benefit_level_percentage
	
	IF dw_benefit_calculation_remunerations.RowCount() > 0 THEN
		lstr_bencalc.adec_decimalparm[4] = dw_benefit_calculation_remunerations.GetItemDecimal(1,'compute_net_remunerations') // total_net_remuneration
	ELSE
		lstr_bencalc.adec_decimalparm[4] = 0
	END IF
	
	lstr_bencalc.adec_decimalparm[5] = dw_benefit_calculation_preaccident_data.GetItemDecimal(1,'preacc_net_pay')            // pre_accident_earnings
	
	IF dw_benefit_calculation_cppd.RowCount() > 0 THEN
		lstr_bencalc.adec_decimalparm[6] = dw_benefit_calculation_cppd.GetItemDecimal(1,'cppd_indexed_monthly_amount')        // Calculate CPP Disability deduction
	ELSE
		lstr_bencalc.adec_decimalparm[6] = 0
	END IF
	
	IF dw_benefit_calculation_deductions.RowCount() > 0 THEN
		lstr_bencalc.adec_decimalparm[7] = dw_benefit_calculation_deductions.GetItemDecimal(1,"sum_deduction_freq_pay")       // total_regular_deductions
	ELSE
		lstr_bencalc.adec_decimalparm[7] = 0
	END IF
	
	lstr_bencalc.as_stringparm[3]    = String(dw_benefit_calculation_details.GetItemDecimal(1,'award_amount'),'$###,##0.00') +' / '+ + dw_benefit_calculation_details.Object.st_award_frequency.Text  // award_string
	lstr_bencalc.as_stringparm[4]    = dw_benefit_calculation_details.GetItemString(1,'award_freq_code')                     // award_freq_code
	lstr_bencalc.as_stringparm[5]    = dw_benefit_calculation_details.GetItemString(1,'top_up_flag')                         // top_up_flag
	lstr_bencalc.as_stringparm[6]    = iw_active_sheet.dw_basic_claim.GetItemString(1,'given_names') +' '+ iw_active_sheet.dw_basic_claim.GetItemString(1,'last_name')
	lstr_bencalc.as_stringparm[7]    = dw_benefit_calculation_details.GetItemString(1,"transitional_claim_flag")             // transitional_claim_flag
	
	OpenWithParm(w_bencalc_legend,lstr_bencalc)
END IF
end event

type dw_benefit_calculation_deductions from u_dw_online within w_calculation_details
boolean visible = false
integer x = 1239
integer y = 952
integer width = 1262
integer height = 632
integer taborder = 60
boolean enabled = false
string dataobject = "d_benefit_calculation_deductions"
boolean border = false
end type

event itemchanged;//	When ever a value changes, we do a set item so that other scripts will 
//	be able to obtain the value. (Otherwise, we would have to post the event).

STRING	ls_column
LONG		ll_rownum

ll_rownum	= this.Getrow()
ls_column = this.GetColumnName()


Choose Case ls_column
	Case "deduction_amount"
		SetItem(ll_rownum,ls_column,dec(gettext()))
		IF Dec(this.GetText()) < 0 THEN
			MessageBox('Entry Error','The Deduction amount must not be negative. Please re-enter the value.',Information!)
			RETURN 1
		END IF
	Case Else
		SetItem(ll_rownum,ls_column,gettext())
End Choose


wf_compute_earnings("Deductions")
wf_calculate_award()

end event

event ue_print;LONG ll_job

	ll_job = PrintOpen()
	PrintDataWindow(ll_job,dw_benefit_calculation_details)
	IF dw_benefit_calculation_preaccident_data.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_preaccident_data) 
	END IF
	IF dw_benefit_calculation_frozen_data.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_frozen_data)
	END IF
	IF dw_benefit_calculation_capable_data.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_capable_data) 
	END IF
	IF dw_benefit_calculation_deductions.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_deductions)
	END IF
	IF dw_benefit_calculation_cppd.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_cppd)
	END IF
	IF dw_benefit_calculation_remunerations.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_remunerations) 
	END IF
	IF dw_benefit_calculation_display_computed.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_display_computed)
	END IF

	PrintClose(ll_job)

end event

type dw_benefit_calculation_preaccident_data from u_dw_online within w_calculation_details
event ue_post_frozen ( )
boolean visible = false
integer x = 69
integer y = 876
integer width = 2482
integer height = 828
integer taborder = 30
boolean enabled = false
string dataobject = "d_benefit_calculation_preaccident_data"
boolean border = false
end type

event itemchanged;call super::itemchanged;//	When ever a value changes, we do a set item so that other scripts will 
//	be able to obtain the value. (Otherwise, we would have to post the event).
Long		ll_base_year, ll_index_year, ll_rownum, ll_acc_dt
Decimal	ldec_preacc_gross_pay, ldec_avg_gross_pay, ldec_old_value
String	ls_gettext, ls_old_value, ls_avg_earn_freq_code, ls_preacc_earn_freq_code
String   ls_preacc_paid_uic_flag, ls_preacc_paid_cpp_flag, ls_avg_paid_uic_flag, ls_avg_paid_cpp_flag

ll_rownum					=	GetRow()
ls_gettext					=	GetText()

ll_acc_dt					= YEAR(DATE(dw_benefit_calculation_details.Object.st_accident_recurrence_date.Text))

CHOOSE CASE GetColumnName()

// Gross Pay Base Year - Validations and Screen Operations
CASE "preacc_gross_pay_base_year"
	ll_base_year = Long(ls_gettext)

	//	The base year cannot be greater than the tax year
	ll_index_year = dw_benefit_calculation_details.GetItemNumber(dw_benefit_calculation_details.GetRow(),"index_taxation_year")

	IF ll_base_year > ll_index_year THEN
		MessageBox("Benefit Calculation Module - Validation Error","The base year cannot be greater than the index/taxation year",Exclamation!)
		Return 1
	END IF

	SetItem(1,"preacc_gross_pay_base_year",ll_base_year)
	
	//The base year cannot be prior to 1982
	IF ll_base_year < 1982 THEN
		MessageBox("Benefit Calculation Module - Validation Error","The Year of Earnings cannot be prior to 1982",Exclamation!)
		Return 1
	END IF
	
	//The base year cannot be prior to the accident/recurrence date
	IF ll_base_year < ll_acc_dt THEN
		MessageBox("Benefit Calculation Module - Validation Error","The Year of Earnings cannot be prior to the accident/recurrence date",Exclamation!)
		Return 1
	END IF
	
// PreAccident Earnings Frequency Code - Validations and Screen Operations
CASE "preacc_earn_freq_code"
	//	Get the old values for comparison later
	ls_old_value				=	GetItemString(ll_rownum,"preacc_earn_freq_code")
	ldec_preacc_gross_pay		=	GetItemDecimal(ll_rownum,"preacc_gross_pay")
	ls_preacc_paid_uic_flag=	GetItemString(ll_rownum,"preacc_paid_uic_flag")
	ls_preacc_paid_cpp_flag=	GetItemString(ll_rownum,"preacc_paid_cpp_flag")
	ls_avg_earn_freq_code	=	GetItemString(ll_rownum,"avg_earn_freq_code")
	ldec_avg_gross_pay			=	GetItemDecimal(ll_rownum,"avg_gross_pay")
	ls_avg_paid_uic_flag	=	GetItemString(ll_rownum,"avg_paid_uic_flag")
	ls_avg_paid_cpp_flag	=	GetItemString(ll_rownum,"avg_paid_cpp_flag")

	//	If the average earnings column was not dIFferent from the preaccident earnings column before,
	//	then change the average earnings column to match the change that was just made here.
	IF ls_old_value = ls_avg_earn_freq_code and &
		ldec_preacc_gross_pay = ldec_avg_gross_pay and &
		ls_preacc_paid_uic_flag = ls_avg_paid_uic_flag and &
		ls_preacc_paid_cpp_flag = ls_avg_paid_cpp_flag THEN
			SetItem(ll_rownum,"avg_earn_freq_code",ls_gettext)
	END IF

	//	If there is a frozen earnings column, change the frequency of frozen earnings
	IF dw_benefit_calculation_frozen_data.RowCount() > 0 THEN
		dw_benefit_calculation_frozen_data.SetItem(1,"frozen_earn_freq_code",ls_gettext)
	END IF

	SetItem(1,"preacc_earn_freq_code",ls_gettext)

// Gross Pay - Validations and Screen Operations
CASE "preacc_gross_pay"
	// Is value negative? - SR 220
	IF IsNumber(data) THEN
		IF Dec(data) < 0 THEN
			MessageBox('Entry Error','The Pre-Accident Gross Pay must not be negative. Please re-enter the value.',Information!)
			RETURN 1
		END IF
	END IF
	
	//	Get the old values for comparison later
	ls_preacc_earn_freq_code	=	GetItemString(ll_rownum,"preacc_earn_freq_code")
	ldec_old_value					=	GetItemDecimal(ll_rownum,"preacc_gross_pay")
	ls_preacc_paid_uic_flag	=	GetItemString(ll_rownum,"preacc_paid_uic_flag")
	ls_preacc_paid_cpp_flag	=	GetItemString(ll_rownum,"preacc_paid_cpp_flag")
	ls_avg_earn_freq_code		=	GetItemString(ll_rownum,"avg_earn_freq_code")
	ldec_avg_gross_pay				=	GetItemDecimal(ll_rownum,"avg_gross_pay")
	ls_avg_paid_uic_flag		=	GetItemString(ll_rownum,"avg_paid_uic_flag")
	ls_avg_paid_cpp_flag		=	GetItemString(ll_rownum,"avg_paid_cpp_flag")

	//	IF the average earnings column was not dIFferent from the preaccident earnings column before,
	//	THEN change the average earnings column to match the change that was just made here.
	IF ls_preacc_earn_freq_code = ls_avg_earn_freq_code and &
		ldec_old_value = ldec_avg_gross_pay and &
		ls_preacc_paid_uic_flag = ls_avg_paid_uic_flag and &
		ls_preacc_paid_cpp_flag = ls_avg_paid_cpp_flag THEN
			SetItem(ll_rownum,"avg_gross_pay",Dec(ls_gettext))
	END IF

	SetItem(1,"preacc_gross_pay",Dec(ls_gettext))

// PREACC PAID UIC FLAG - Validations and Screen Operations
CASE "preacc_paid_uic_flag"
	//	Get the old values for comparison later
	ls_preacc_earn_freq_code	=	GetItemString(ll_rownum,"preacc_earn_freq_code")
	ldec_preacc_gross_pay			=	GetItemDecimal(ll_rownum,"preacc_gross_pay")
	ls_old_value 					=	GetItemString(ll_rownum,"preacc_paid_uic_flag")
	ls_preacc_paid_cpp_flag	=	GetItemString(ll_rownum,"preacc_paid_cpp_flag")
	ls_avg_earn_freq_code		=	GetItemString(ll_rownum,"avg_earn_freq_code")
	ldec_avg_gross_pay				=	GetItemDecimal(ll_rownum,"avg_gross_pay")
	ls_avg_paid_uic_flag		=	GetItemString(ll_rownum,"avg_paid_uic_flag")
	ls_avg_paid_cpp_flag		=	GetItemString(ll_rownum,"avg_paid_cpp_flag")

	//	IF the average earnings column was not dIFferent from the preaccident earnings column before,
	//	THEN change the average earnings column to match the change that was just made here.

	IF ls_preacc_earn_freq_code = ls_avg_earn_freq_code and &
		ldec_preacc_gross_pay = ldec_avg_gross_pay and &
		ls_old_value = ls_avg_paid_uic_flag and &
		ls_preacc_paid_cpp_flag = ls_avg_paid_cpp_flag THEN
			SetItem(ll_rownum,"avg_paid_uic_flag",ls_gettext)
	END IF

	SetItem(1,"preacc_paid_uic_flag",ls_gettext)
	dw_benefit_calculation_frozen_data.SetItem(1,"frozen_uic_flag",ls_gettext)	// SR72

// PREACC PAID CPP FLAG - Validations and Screen Operations
CASE "preacc_paid_cpp_flag"
	//	Get the old values for comparison later
	ls_preacc_earn_freq_code	=	GetItemString(ll_rownum,"preacc_earn_freq_code")
	ldec_preacc_gross_pay			=	GetItemDecimal(ll_rownum,"preacc_gross_pay")
	ls_preacc_paid_uic_flag	=	GetItemString(ll_rownum,"preacc_paid_uic_flag")
	ls_old_value					=	GetItemString(ll_rownum,"preacc_paid_cpp_flag")
	ls_avg_earn_freq_code		=	GetItemString(ll_rownum,"avg_earn_freq_code")
	ldec_avg_gross_pay				=	GetItemDecimal(ll_rownum,"avg_gross_pay")
	ls_avg_paid_uic_flag		=	GetItemString(ll_rownum,"avg_paid_uic_flag")
	ls_avg_paid_cpp_flag		=	GetItemString(ll_rownum,"avg_paid_cpp_flag")

	//	IF the average earnings column was not dIFferent from the preaccident earnings column before,
	//	THEN change the average earnings column to match the change that was just made here.
	IF ls_preacc_earn_freq_code = ls_avg_earn_freq_code and &
		ldec_preacc_gross_pay = ldec_avg_gross_pay and &
		ls_preacc_paid_uic_flag = ls_avg_paid_uic_flag and &
		ls_old_value = ls_avg_paid_cpp_flag THEN
			SetItem(ll_rownum,"avg_paid_cpp_flag",ls_gettext)
	END IF

	SetItem(1,"preacc_paid_cpp_flag",ls_gettext)
	dw_benefit_calculation_frozen_data.SetItem(1,"frozen_cpp_flag",ls_gettext)	// SR72

CASE "avg_gross_pay"
	// Is value negative? - SR 220
	IF IsNumber(data) THEN
		IF Dec(data) < 0 THEN
			MessageBox('Entry Error','The Average Earnings Gross Pay must not be negative. Please re-enter the value.',Information!)
			RETURN 1
		END IF
	END IF
	
	SetItem(1,"avg_gross_pay",dec(ls_gettext))

CASE "avg_earn_freq_code"
	SetItem(1,"avg_earn_freq_code",ls_gettext)

CASE "avg_paid_uic_flag"
	SetItem(1,"avg_paid_uic_flag",ls_gettext)
	SetItem(1,"preacc_paid_uic_flag",ls_gettext)
	dw_benefit_calculation_frozen_data.SetItem(1,"frozen_uic_flag",ls_gettext)
CASE "avg_paid_cpp_flag"
	SetItem(1,"avg_paid_cpp_flag",ls_gettext)
	SetItem(1,"preacc_paid_cpp_flag",ls_gettext)
	dw_benefit_calculation_frozen_data.SetItem(1,"frozen_cpp_flag",ls_gettext)
END CHOOSE


	wf_compute_earnings("PreAccident")
	wf_compute_earnings("Average")
	
	/* added for project 10105 june 1/2000
	*/
	wf_compute_earnings("Frozen")
	wf_calculate_award()

end event

event ue_print;LONG ll_job

	ll_job = PrintOpen()
	PrintDataWindow(ll_job,dw_benefit_calculation_details)
	IF dw_benefit_calculation_preaccident_data.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_preaccident_data) 
	END IF
	IF dw_benefit_calculation_frozen_data.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_frozen_data)
	END IF
	IF dw_benefit_calculation_capable_data.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_capable_data) 
	END IF
	IF dw_benefit_calculation_deductions.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_deductions)
	END IF
	IF dw_benefit_calculation_cppd.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_cppd)
	END IF
	IF dw_benefit_calculation_remunerations.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_remunerations) 
	END IF
	IF dw_benefit_calculation_display_computed.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_display_computed)
	END IF

	PrintClose(ll_job)

end event

type dw_benefit_calculation_remunerations from u_dw_online within w_calculation_details
integer x = 69
integer y = 876
integer width = 2501
integer height = 828
integer taborder = 100
boolean enabled = false
string dataobject = "d_benefit_calculation_remunerations"
boolean border = false
end type

event rowfocuschanged;
STRING	ls_earn_freq_code
DECIMAL	ldec_gross_pay,	ldec_annual_gross

IF CurrentRow > 0 THEN
	ls_earn_freq_code	=  dw_benefit_calculation_remunerations.GetItemString(CurrentRow,"remun_earn_freq_code")
	ldec_gross_pay		=	dw_benefit_calculation_remunerations.GetItemDecimal(CurrentRow,"remun_gross_pay")
	ldec_annual_gross	=	f_convert_frequency(ls_earn_freq_code,ldec_gross_pay,"A")
	dw_benefit_calculation_remunerations.SetItem(CurrentRow,"annual_gross",ldec_annual_gross)
END IF 


end event

event itemchanged;call super::itemchanged;//	When ever a value changes, we do a set item so that other scripts will 
//	be able to obtain the value. (Otherwise, we would have to post the event).


STRING ls_column

ls_column = this.GetColumnName()

CHOOSE CASE ls_column
CASE "remun_gross_pay"
	this.SetItem(this.GetRow(),ls_column,Dec(this.GetText()))
	IF Dec(this.GetText()) < 0 THEN
		MessageBox('Entry Error','The Remuneration amount must not be negative. Please re-enter the value.',Information!)
		RETURN 1
	END IF

CASE ELSE
	this.SetItem(this.GetRow(),ls_column,this.GetText())

END CHOOSE


wf_compute_earnings("Remunerations")
wf_calculate_award()

end event

event ue_print;LONG ll_job

	ll_job = PrintOpen()
	PrintDataWindow(ll_job,dw_benefit_calculation_details)
	IF dw_benefit_calculation_preaccident_data.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_preaccident_data) 
	END IF
	IF dw_benefit_calculation_frozen_data.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_frozen_data)
	END IF
	IF dw_benefit_calculation_capable_data.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_capable_data) 
	END IF
	IF dw_benefit_calculation_deductions.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_deductions)
	END IF
	IF dw_benefit_calculation_cppd.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_cppd)
	END IF
	IF dw_benefit_calculation_remunerations.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_remunerations) 
	END IF
	IF dw_benefit_calculation_display_computed.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_display_computed)
	END IF

	PrintClose(ll_job)

end event

type cb_delete_remuneration from commandbutton within w_calculation_details
integer x = 1861
integer y = 1604
integer width = 466
integer height = 96
integer taborder = 70
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Delete Remun"
end type

event clicked;
/*	Declare Variables  
	
*/

INTEGER	    li_index
INTEGER      li_current_row


li_index	=	uo_tabs.ii_index


// If the tab index is 3, we are processing deductions, otherwise, we are processing remunerations
	IF li_index = 3 THEN
		li_current_row = dw_benefit_calculation_deductions.GetRow()
		IF li_current_row > 0 THEN
			dw_benefit_calculation_deductions.DeleteRow(0)			
		END IF

	ELSEIF li_index = 4 THEN
		li_current_row = dw_benefit_calculation_remunerations.GetRow()
		IF li_current_row > 0 THEN
			dw_benefit_calculation_remunerations.DeleteRow(0)
		END IF

	END IF

// calculate award after deletion
wf_recalculate_all()
end event

type gb_regular_deductions from groupbox within w_calculation_details
integer x = 1166
integer y = 896
integer width = 1399
integer height = 804
integer taborder = 70
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Regular Deductions"
end type

type dw_benefit_calculation_frozen_data from u_dw_online within w_calculation_details
integer x = 1993
integer y = 876
integer width = 581
integer height = 832
integer taborder = 40
boolean enabled = false
string dataobject = "d_benefit_calculation_frozen_data"
boolean border = false
end type

event itemchanged;call super::itemchanged;//	When ever a value changes, we do a set item so that other scripts will 
//	be able to obtain the value. (Otherwise, we would have to post the event).


String	ls_column

ls_column = this.GetColumnName()

CHOOSE CASE ls_column
	CASE "frozen_gross_pay"
		SetItem(1,ls_column,dec(gettext()))
		st_frozen_92_taxyear.Visible = True
		st_taxation_period.Visible = True
		st_taxation_period.Text = 'Taxation Period:'
		st_taxation_period.BringToTop = True
		
	CASE "frozen_annual_net_pay"
		SetItem(1,ls_column,dec(gettext()))
	CASE "frozen_uic_flag"
		SetItem(1,ls_column,gettext())
		dw_benefit_calculation_preaccident_data.SetItem(1, "preacc_paid_uic_flag", gettext())
		dw_benefit_calculation_preaccident_data.SetItem(1, "avg_paid_uic_flag", gettext())
	CASE "frozen_cpp_flag"
		SetItem(1,ls_column,gettext())
		dw_benefit_calculation_preaccident_data.SetItem(1, "preacc_paid_cpp_flag", gettext())
		dw_benefit_calculation_preaccident_data.SetItem(1, "avg_paid_cpp_flag", gettext())
	CASE ELSE
		SetItem(1,ls_column,gettext())

END CHOOSE

wf_compute_earnings("PreAccident")
wf_compute_earnings("Average")

wf_compute_earnings("Frozen")
wf_calculate_award()

end event

event ue_print;LONG ll_job

	ll_job = PrintOpen()
	PrintDataWindow(ll_job,dw_benefit_calculation_details)
	IF dw_benefit_calculation_preaccident_data.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_preaccident_data) 
	END IF
	IF dw_benefit_calculation_frozen_data.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_frozen_data)
	END IF
	IF dw_benefit_calculation_capable_data.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_capable_data) 
	END IF
	IF dw_benefit_calculation_deductions.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_deductions)
	END IF
	IF dw_benefit_calculation_cppd.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_cppd)
	END IF
	IF dw_benefit_calculation_remunerations.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_remunerations) 
	END IF
	IF dw_benefit_calculation_display_computed.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_display_computed)
	END IF

	PrintClose(ll_job)

end event

type uo_tabs from u_top_bar within w_calculation_details
event recalculate pbm_custom02
integer x = 46
integer y = 764
integer width = 2551
integer height = 984
integer taborder = 120
boolean border = false
end type

event ue_bar_clicked;call super::ue_bar_clicked;//	extended event for ue_bar_clicked

	Integer	li_index,	li_old_index , li_row, li_rtn
	Long		ll_rownum

LONG 		ll_getrow
STRING	ls_earn_freq_code
DECIMAL	ldec_gross_pay,	ldec_annual_gross
dwitemstatus ldw_status
datawindowChild ldw_child

//	make sure a new tab was clicked

	li_index	=	this.ii_index
	li_old_index	=	this.ii_lastindex

	If li_index < 1 then li_index = 1
	If li_old_index < 1 then li_old_index = 1

	If li_old_index = li_index then Return


//	show the new tab folder!
	dw_benefit_calculation_preaccident_data.visible = FALSE
	dw_benefit_calculation_frozen_data.visible      = FALSE	
	dw_benefit_calculation_capable_data.visible 		= FALSE
	// S. Manzer temporary fix. 
	//ddlb_frozen_92_taxyear.visible = False 
	//Commented out nov 24 by N Irwin
	st_frozen_92_taxyear.visible = False
	st_taxation_period.visible = False
	dw_benefit_calculation_deductions.visible			= FALSE
	dw_benefit_calculation_remunerations.visible		= FALSE
	dw_benefit_calculation_display_computed.visible	= FALSE
	
	cb_delete_remuneration.Enabled = FALSE
	
	Choose case li_index

		/* Pre-Accident Data */
		Case 1
			dw_benefit_calculation_preaccident_data.visible =	True
			dw_benefit_calculation_frozen_data.visible = True
			// S.Manzer Temporary Change
			li_row = dw_benefit_calculation_frozen_data.GetRow()
			IF (ib_detail = False and li_row <> 0) THEN
				//ddlb_frozen_92_taxyear.visible = True
				//Commented out nov 24 by N Irwin
				st_frozen_92_taxyear.visible = False
				st_taxation_period.visible = False
			END IF	
			dw_benefit_calculation_preaccident_data.SetFocus()

		/* Capable Data  */
		Case 2

			dw_benefit_calculation_capable_data.visible 		=	TRUE
			dw_benefit_calculation_capable_data.SetFocus()

		/* Deductions   */
		Case 3
			gb_regular_deductions.visible							=	TRUE
			gb_cppd_deductions.visible								=	TRUE
			dw_benefit_calculation_deductions.visible			=	TRUE
			dw_benefit_calculation_deductions.SetFocus()

			If dw_benefit_calculation_cppd.GetItemDecimal(1,"cppd_monthly_amount") > 0 or &
				dw_benefit_calculation_deductions.Enabled 	=  TRUE Then
				dw_benefit_calculation_cppd.visible				=	TRUE
			End If

			If dw_benefit_calculation_deductions.RowCount() > 0 Then
				cb_add_earnings.Text									=	"Add Deduction"
				cb_delete_remuneration.Text                  =  'Delete Deduction'
			End If
			
			IF cb_save.Enabled THEN
				cb_add_earnings.Enabled        = TRUE
				cb_delete_remuneration.Enabled = TRUE
			ELSE
				cb_add_earnings.Enabled        = FALSE
				cb_delete_remuneration.Enabled = FALSE
			END IF
			
			cb_add_earnings.Visible        = TRUE
			cb_delete_remuneration.Visible = TRUE
			cb_prev.Visible                = TRUE
			cb_next.Visible                = TRUE
			
			// Only allow deduction deletion if the row is new/newmodified
			ll_getrow = dw_benefit_calculation_deductions.GetRow()
			IF ll_getrow > 0 THEN
				ldw_status = dw_benefit_calculation_deductions.getItemStatus(ll_getrow, 0, Primary!)
				IF ldw_status = New! or ldw_status = NewModified! THEN
					cb_delete_remuneration.Enabled = TRUE
				END IF
			END IF


		/* Remunerations  */
		Case 4

			dw_benefit_calculation_remunerations.visible		=	True
			If dw_benefit_calculation_remunerations.RowCount() > 0 Then
				cb_add_earnings.Text									=	"Add Remun"
				cb_delete_remuneration.Text                  =  'Delete Remun'
			End If
			
			IF cb_save.Enabled THEN
				cb_add_earnings.Enabled        = TRUE
				cb_delete_remuneration.Enabled = TRUE
			ELSE
				cb_add_earnings.Enabled        = FALSE
				cb_delete_remuneration.Enabled = FALSE
			END IF
			
			cb_add_earnings.Visible        = TRUE
			cb_delete_remuneration.Visible = TRUE
			cb_prev.Visible                = TRUE
			cb_next.Visible                = TRUE
			
			
			dw_benefit_calculation_remunerations.SetFocus()
			/* If there are any remunerations, calculate the annual gross remuneration amount 
			*/
			ll_getrow = dw_benefit_calculation_remunerations.GetRow()
			IF ll_getrow > 0 THEN
				//T021844 on 'Add', filter out any non-active remuneration types, (keep only the ones with active-flag = 'Y')
				ldw_status = dw_benefit_calculation_remunerations.getItemStatus(ll_getrow, 0, Primary!)
				IF ldw_status = New! or ldw_status = NewModified! THEN
					// Only allow remuneration deletion if the row is new/newmodified
					li_rtn = dw_benefit_calculation_remunerations.getChild('remun_type_code', ldw_child)
					IF li_rtn = -1 THEN 
						MESSAGEBOX("Not a child", "remun_type_code is not a datawindowchild. Cancelling this operation")
						RETURN -1
					END IF
					ldw_child.setFilter("active_flag = 'Y'")
					ldw_child.filter()
					
					cb_delete_remuneration.Enabled = TRUE
				ELSE
					ldw_child.setFilter("")
					ldw_child.filter()
				END IF
				ls_earn_freq_code	=  dw_benefit_calculation_remunerations.GetItemString(ll_getrow,"remun_earn_freq_code")
				ldec_gross_pay		=	dw_benefit_calculation_remunerations.GetItemDecimal(ll_getrow,"remun_gross_pay")
				ldec_annual_gross	=	f_convert_frequency(ls_earn_freq_code,ldec_gross_pay,"A")
				dw_benefit_calculation_remunerations.SetItem(ll_getrow,"annual_gross",ldec_annual_gross)
			END IF
			

		/* Display Computations  */			
		Case 5

			dw_benefit_calculation_display_computed.visible	=	True


	End Choose


end event

on uo_tabs.destroy
call u_top_bar::destroy
end on

type dw_benefit_calculation_cppd from u_dw_online within w_calculation_details
integer x = 96
integer y = 952
integer width = 1006
integer height = 604
integer taborder = 80
boolean bringtotop = true
boolean enabled = false
string dataobject = "d_benefit_calculation_cppd"
boolean border = false
end type

event itemchanged;call super::itemchanged;//	When ever a value changes, we do a set item so that other scripts will 
//	be able to obtain the value. (Otherwise, we would have to post the event).
Long	ll_base_year,	ll_index_year

THIS.AcceptText()
	
CHOOSE CASE GetColumnName()

CASE "cppd_base_year"  // Gross Pay Base Year - Validations and Screen Operations
	ll_base_year = Long(gettext())

	//	The base year cannot be greater than the tax year
	ll_index_year = dw_benefit_calculation_details.GetItemNumber(dw_benefit_calculation_details.GetRow(),"index_taxation_year")

	IF ll_base_year > ll_index_year THEN
		MessageBox("Benefit Calculation Module - Validation Error","The cppd base year cannot be greater than the index/taxation year",Exclamation!)
		Return 1
	END IF

// Sept.28, 2000 S.Manzer PR1486
//	The cppd_base_year cannot be before 1982
	IF ll_base_year < 1982 THEN
		MessageBox("Benefit Calculation Module - Validation Error","The cppd base year cannot be before 1982",Exclamation!)
		Return 1
	END IF

	SetItem(1,"cppd_base_year",ll_base_year)
CASE "cppd_monthly_amount"
//	SetItem(1,"cppd_monthly_amount",Dec(this.GetText()))
	IF Dec(this.GetText()) < 0 THEN
		MessageBox('Entry Error','The CPPD monthly amount must not be negative. Please re-enter the value.',Information!)
		RETURN 1
	END IF
END CHOOSE

wf_compute_earnings("CPPD")
wf_calculate_award()

end event

event ue_print;LONG ll_job

	ll_job = PrintOpen()
	PrintDataWindow(ll_job,dw_benefit_calculation_details)
	IF dw_benefit_calculation_preaccident_data.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_preaccident_data) 
	END IF
	IF dw_benefit_calculation_frozen_data.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_frozen_data)
	END IF
	IF dw_benefit_calculation_capable_data.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_capable_data) 
	END IF
	IF dw_benefit_calculation_deductions.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_deductions)
	END IF
	IF dw_benefit_calculation_cppd.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_cppd)
	END IF
	IF dw_benefit_calculation_remunerations.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_remunerations) 
	END IF
	IF dw_benefit_calculation_display_computed.RowCount() > 0 THEN
		PrintDataWindow(ll_job,dw_benefit_calculation_display_computed)
	END IF

	PrintClose(ll_job)

end event

