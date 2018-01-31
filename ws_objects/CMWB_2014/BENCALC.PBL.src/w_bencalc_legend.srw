$PBExportHeader$w_bencalc_legend.srw
forward
global type w_bencalc_legend from window
end type
type cb_print from commandbutton within w_bencalc_legend
end type
type dw_legend from u_dw_online within w_bencalc_legend
end type
type cb_close from commandbutton within w_bencalc_legend
end type
end forward

global type w_bencalc_legend from window
integer width = 4686
integer height = 2728
boolean titlebar = true
string title = "Benefit Calculation Legend"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
cb_print cb_print
dw_legend dw_legend
cb_close cb_close
end type
global w_bencalc_legend w_bencalc_legend

on w_bencalc_legend.create
this.cb_print=create cb_print
this.dw_legend=create dw_legend
this.cb_close=create cb_close
this.Control[]={this.cb_print,&
this.dw_legend,&
this.cb_close}
end on

on w_bencalc_legend.destroy
destroy(this.cb_print)
destroy(this.dw_legend)
destroy(this.cb_close)
end on

event open;DATETIME           ldtm_effective_from_date
DECIMAL{2}         ldec_average_net_earnings, ldec_capable_net, ldec_100_pct_LOE
DECIMAL{2}         ldec_benefit_level_percentage, ldec_adj_benefit_level_percentage, ldec_LOE_reduced
DECIMAL{2}         ldec_total_net_remunerations, ldec_combined_earnings
DECIMAL{2}         ldec_pre_accident_net_earnings, ldec_preacc_earnings_reduced
DECIMAL{2}         ldec_excess_amount, ldec_cppd_indexed_monthly, ldec_cppd_amount
DECIMAL{2}         ldec_other_deduction_amount, ldec_total_deductions, ldec_regular_award_amount
INTEGER            li_inserted_row, li_opening_no, li_benefit_calculation_no
LONG               ll_claim_no
STRING             ls_calculation_type_code, ls_draft_flag, ls_ben_pct_text_string
STRING             ls_award_string, ls_award_freq_code, ls_top_up_flag, ls_claimant_name, ls_transitional_claim_flag
S_WINDOW_MESSAGE   lstr_bencalc

lstr_bencalc = Message.PowerObjectParm




/* populate variables for setitems, perform calculations */
// header
ll_claim_no                               = lstr_bencalc.al_doubleparm[1]
li_opening_no                             = lstr_bencalc.al_doubleparm[2]
li_benefit_calculation_no                 = lstr_bencalc.al_doubleparm[3]

ldtm_effective_from_date                  = lstr_bencalc.adtm_datetimeparm[1]
ls_calculation_type_code                  = lstr_bencalc.as_stringparm[1]
ls_draft_flag                             = lstr_bencalc.as_stringparm[2]
ls_award_string                           = lstr_bencalc.as_stringparm[3]
ls_award_freq_code                        = lstr_bencalc.as_stringparm[4]
ls_claimant_name                          = lstr_bencalc.as_stringparm[6]
ls_transitional_claim_flag                = lstr_bencalc.as_stringparm[7]

// details
ldec_average_net_earnings                 = lstr_bencalc.adec_decimalparm[1]
ldec_capable_net                          = lstr_bencalc.adec_decimalparm[2]
ldec_100_pct_LOE                          = ldec_average_net_earnings - ldec_capable_net
IF ldec_100_pct_LOE < 0 THEN ldec_100_pct_LOE = 0

ldec_benefit_level_percentage             = lstr_bencalc.adec_decimalparm[3]
IF ls_transitional_claim_flag = 'Y' THEN
	ldec_adj_benefit_level_percentage = 0.85
ELSE
	ldec_adj_benefit_level_percentage = ldec_benefit_level_percentage
END IF
ldec_LOE_reduced                          = ldec_100_pct_LOE * ldec_adj_benefit_level_percentage

ldec_total_net_remunerations              = lstr_bencalc.adec_decimalparm[4]

// set combined earnings
IF DATE(ldtm_effective_from_date) < Date('1993-01-01') THEN
	ldec_combined_earnings = 0
ELSE
	ldec_combined_earnings = ldec_LOE_reduced + ldec_total_net_remunerations
END IF

ldec_pre_accident_net_earnings            = lstr_bencalc.adec_decimalparm[5]
ldec_preacc_earnings_reduced              = ldec_pre_accident_net_earnings * ldec_adj_benefit_level_percentage

// set excess amount
ls_top_up_flag                            = lstr_bencalc.as_stringparm[5]
ldec_excess_amount                        = ldec_combined_earnings - ldec_preacc_earnings_reduced  	//	Calculate Excess Amount
IF ldec_excess_amount < 0 THEN
	ldec_excess_amount = 0
ELSE
	IF ls_top_up_flag = 'Y' THEN
		ldec_excess_amount = 0
	ELSE
		IF ldec_benefit_level_percentage = .90 AND DATE(ldtm_effective_from_date) < Date('1993-01-01') THEN
			ldec_excess_amount = 0
		END IF
	End If
END IF

// set cppd amount
ldec_cppd_indexed_monthly                 = lstr_bencalc.adec_decimalparm[6]
// convert CPPD deduction to award's frequency
ldec_cppd_indexed_monthly = f_convert_frequency('M',ldec_cppd_indexed_monthly,ls_award_freq_code)

IF ldec_average_net_earnings > 0  THEN
	IF DATE(ldtm_effective_from_date) >= Date('1990-01-01') THEN
		ldec_cppd_amount                       = ( ldec_100_pct_LOE/ldec_average_net_earnings ) * ldec_cppd_indexed_monthly
	ELSE
		// deduction is not reduced by factor above ( ldec_100_pct_LOE/ldec_average_net_earnings )
		ldec_cppd_amount                       = ldec_cppd_indexed_monthly
		dw_legend.Object.t_CPPD_amount.Text    = '= CPPD Indexed Monthly'
	END IF
END IF

ldec_other_deduction_amount               = lstr_bencalc.adec_decimalparm[7]

ldec_total_deductions                     = ldec_excess_amount + ldec_cppd_amount + ldec_other_deduction_amount

ldec_regular_award_amount                 = ldec_LOE_reduced - ldec_total_deductions


// populate columns
li_inserted_row = dw_legend.InsertRow(0)

dw_legend.SetItem(li_inserted_row,'claim_no',                             ll_claim_no )
dw_legend.SetItem(li_inserted_row,'opening_no',                           li_opening_no )
dw_legend.SetItem(li_inserted_row,'benefit_calculation_no',               li_benefit_calculation_no )
dw_legend.SetItem(li_inserted_row,'effective_from_date',                  ldtm_effective_from_date )
dw_legend.SetItem(li_inserted_row,'calculation_type_code',                ls_calculation_type_code )
dw_legend.SetItem(li_inserted_row,'draft_flag',                           ls_draft_flag )
dw_legend.SetItem(li_inserted_row,'claimant_name',                        ls_claimant_name )

dw_legend.SetItem(li_inserted_row,'average_net_earnings',                 ldec_average_net_earnings )
dw_legend.SetItem(li_inserted_row,'capable_net',                          ldec_capable_net )
dw_legend.SetItem(li_inserted_row,'one_hundred_pct_LOE',                  ldec_100_pct_LOE)

dw_legend.SetItem(li_inserted_row,'benefit_level_percentage',             ldec_benefit_level_percentage )
dw_legend.SetItem(li_inserted_row,'adj_benefit_level_percentage',         ldec_adj_benefit_level_percentage )
dw_legend.SetItem(li_inserted_row,'LOE_reduced',                          ldec_LOE_reduced )

dw_legend.SetItem(li_inserted_row,'total_net_remuneration',               ldec_total_net_remunerations )
dw_legend.SetItem(li_inserted_row,'combined_earnings',                    ldec_combined_earnings )

dw_legend.SetItem(li_inserted_row,'pre_accident_earnings',                ldec_pre_accident_net_earnings )
dw_legend.SetItem(li_inserted_row,'preacc_earnings_reduced',              ldec_preacc_earnings_reduced)

dw_legend.SetItem(li_inserted_row,'excess_amount',                        ldec_excess_amount)

dw_legend.SetItem(li_inserted_row,'cppd_deductions',                      ldec_cppd_indexed_monthly )
dw_legend.SetItem(li_inserted_row,'cppd_amount',                          ldec_cppd_amount )

dw_legend.SetItem(li_inserted_row,'total_regular_deductions',             ldec_other_deduction_amount )

dw_legend.SetItem(li_inserted_row,'total_deductions',                     ldec_total_deductions )

dw_legend.SetItem(li_inserted_row,'regular_award_amount',                 ldec_regular_award_amount )

dw_legend.SetItem(li_inserted_row,'award_string',                         ls_award_string )


// set legend texts for non-85% benefit level
IF ldec_adj_benefit_level_percentage <> 0.85 THEN
	ls_ben_pct_text_string = String(ldec_adj_benefit_level_percentage,'##0%')
	
	dw_legend.Object.t_reduced_loe_calc.Text = ls_ben_pct_text_string + ' Loss of Earnings'
	IF ldec_adj_benefit_level_percentage = 1.00 THEN
		dw_legend.Object.t_reduced_loe_legend_calc.Text = '= 100% Loss of Earnings'
	ELSE
		dw_legend.Object.t_reduced_loe_legend_calc.Text = '= 100% Loss of Earnings * ' + ls_ben_pct_text_string
	END IF
	
	IF Date(ldtm_effective_from_date) >= 1993-01-01 THEN
		dw_legend.Object.t_combined_earnings_legend_calc.Text = '= '+ls_ben_pct_text_string+' Loss of Earnings + Total Net Remunerations'
	END IF
	
	dw_legend.Object.t_reduced_preacc_earnings_calc.Text = ls_ben_pct_text_string + ' Pre-Accident Net Earnings'
	dw_legend.Object.t_reduced_preacc_earnings_legend_calc.Text = '= Pre-Accident Net Earnings * ' + ls_ben_pct_text_string	
	
	dw_legend.Object.t_excess_amount_calc.Text = '= Combined Earnings - '+ls_ben_pct_text_string+' Pre-Accident Net Earnings'
	
	dw_legend.Object.t_regular_award_calc.Text = '= '+ls_ben_pct_text_string+' Loss of Earnings - Total Deduction'	
END IF

// set top-up provision text, where applicable
IF ls_top_up_flag = 'Y' AND ldec_excess_amount = 0 THEN
	dw_legend.Object.t_excess_amount_calc.Text = dw_legend.Object.t_excess_amount_calc.Text + ' (TopUp Prov)'
END IF



end event

type cb_print from commandbutton within w_bencalc_legend
integer x = 3685
integer y = 2508
integer width = 402
integer height = 104
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Print"
end type

event clicked;dw_legend.Object.DataWindow.Print.Orientation = 1
dw_legend.Print()

end event

type dw_legend from u_dw_online within w_bencalc_legend
integer x = 37
integer y = 32
integer width = 4599
integer height = 2440
integer taborder = 10
string dataobject = "d_benefit_calculation_legend"
borderstyle borderstyle = stylelowered!
end type

type cb_close from commandbutton within w_bencalc_legend
integer x = 4229
integer y = 2508
integer width = 402
integer height = 104
integer taborder = 10
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Close"
end type

event clicked;Close(PARENT)
end event

