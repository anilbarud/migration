$PBExportHeader$w_benefits.srw
forward
global type w_benefits from w_a_tool
end type
type cb_add from commandbutton within w_benefits
end type
type cb_switch from commandbutton within w_benefits
end type
type dw_opening_list from u_dw_online within w_benefits
end type
type dw_benefit_calculation_list from u_dw_online within w_benefits
end type
type gb_benefit_calcs from groupbox within w_benefits
end type
type gb_select_opening from groupbox within w_benefits
end type
type cb_copy from commandbutton within w_benefits
end type
type sle_overpayment from singlelineedit within w_benefits
end type
end forward

global type w_benefits from w_a_tool
boolean resizable = false
cb_add cb_add
cb_switch cb_switch
dw_opening_list dw_opening_list
dw_benefit_calculation_list dw_benefit_calculation_list
gb_benefit_calcs gb_benefit_calcs
gb_select_opening gb_select_opening
cb_copy cb_copy
sle_overpayment sle_overpayment
end type
global w_benefits w_benefits

type variables
//
// Variables required by benefits module
//

DATE		              idt_server_date
STRING		              is_triggered_event
n_benefit_calculation	in_bencalcs
s_window_message 	istr_bencalc_parameters


//
// Received from dw_basic_claim
//

LONG                 il_claim_no
DATE                 idt_accident_date

// Required for benefit calculation validations 
// and defaults

DECIMAL            idec_cpi_index_factor

WINDOW            iw_parent_window

end variables

forward prototypes
public function integer wf_read_only ()
public subroutine wf_initialize_structure ()
end prototypes

public function integer wf_read_only ();/* Function to Display the Benefit Calculation in Read-Only mode
 	Disable the ADD button and the COPY button. Set the data window column colours to read-only display
*/

cb_copy.enabled = FALSE

dw_benefit_calculation_list.uf_protect_allattributes(TRUE)
dw_benefit_calculation_list.uf_set_backcolor()

return 0
 
end function

public subroutine wf_initialize_structure ();LONG							ll_doubleparm_upperbound , ll_stringparm_upperbound , ll_datetimeparm_upperbound
DATETIME					ldtm_null_datetime

IF IsValid(istr_bencalc_parameters) THEN
	ll_doubleparm_upperbound = UpperBound(istr_bencalc_parameters.al_doubleparm)
	ll_stringparm_upperbound = UpperBound(istr_bencalc_parameters.as_stringparm)
	ll_datetimeparm_upperbound = UpperBound(istr_bencalc_parameters.adtm_datetimeparm)
END IF
	
/* Initialize variables
*/
	SETNULL(ldtm_null_datetime)

IF ll_doubleparm_upperbound = 0 THEN
	istr_bencalc_parameters.al_doubleparm[1] = 0
	istr_bencalc_parameters.al_doubleparm[2] = 0
	istr_bencalc_parameters.al_doubleparm[3] = 0
	istr_bencalc_parameters.al_doubleparm[4] = 0
	istr_bencalc_parameters.al_doubleparm[5] = 0
END IF

IF ll_stringparm_upperbound = 0 THEN
	istr_bencalc_parameters.as_stringparm[1] = ""
	istr_bencalc_parameters.as_stringparm[2] = ""
	istr_bencalc_parameters.as_stringparm[3] = ""
	istr_bencalc_parameters.as_stringparm[4] = ""
END IF

IF ll_datetimeparm_upperbound = 0 THEN
	istr_bencalc_parameters.adtm_datetimeparm[1] = ldtm_null_datetime
	istr_bencalc_parameters.adtm_datetimeparm[2] = ldtm_null_datetime
	istr_bencalc_parameters.adtm_datetimeparm[3] = ldtm_null_datetime
END IF

end subroutine

event open;call super::open;/* ---------------------------------------------------------------------------------------------------- */
/* Declare  Variables                                                                    					  */
/* ---------------------------------------------------------------------------------------------------- */

	INTEGER                 li_rtn, li_rows
	LONG							ll_nmbr_openings,	ll_find_opening, ll_find_bencalc, ll_rownum , ll_nmbr_bencalcs, ll_individual_no
	LONG							ll_doubleparm_upperbound , ll_stringparm_upperbound , ll_datetimeparm_upperbound
	W_SHEET						lwi_active_sheet
	DECIMAL						ldec_balance_amount
	S_WINDOW_MESSAGE			lstr_message
	U_DS              lds_overpayment_warning_sum
	
		

/* ***********************************************************
	Benefit Calculation Parameters stored in s_window_message

	Claim No								al_doubleparm[1]
	Opening No							al_doubleparm[2]
	Benefit Calculation No			al_doubleparm[3]
	Copy Benefit Calculation No	al_doubleparm[4]
	Copy BenCalc Opening No			al_doubleparm[5]

	Transitional Claim Flag			as_stringparm[1]
	Top-up Flag							as_stringparm[2]
	Opening Type Code					as_stringparm[3]
	Window Opened By 					as_stringparm[4]

	Accident Recurrence Date		adtm_datetimeparm[1]
	Benefit Start Date				adtm_datetimeparm[2]
	Effective Date						adtm_datetimeparm[3]

	Read/Update Mode					as_mode 

	**********************************************************
*/
	istr_bencalc_parameters	=	Message.PowerObjectParm
	
/* Initialize variables
*/
	wf_initialize_structure()
	
/* ---------------------------------------------------------------------------------------------------- */
/* Database Connections                                                                                 */
/* ---------------------------------------------------------------------------------------------------- */

	dw_opening_list.SetTransObject						(sqlca)
	dw_opening_list.uf_setselect(1)
	dw_benefit_calculation_list.SetTransObject			(sqlca)
	dw_benefit_calculation_list.uf_setselect(1)



/* Determine if Benefit Calculation should be opened Read-Only. When opened "read-only", the user can 
	still select the ADD option to allow them to enter benefit calculation detail without saving it
*/
	IF upper(istr_bencalc_parameters.as_mode) = "READ" THEN
		wf_read_only()
	END IF


/* ---------------------------------------------------------------------------------------------------- */
/* Receive the basic claim information                                                                  */
/* ---------------------------------------------------------------------------------------------------- */

	lwi_active_sheet	= w_frame.GetActiveSheet()
	il_claim_no		   = lwi_active_sheet.dw_basic_claim.GetItemNumber(1,"claim_no")
	ll_individual_no	= lwi_active_sheet.dw_basic_claim.GetItemNumber(1,"individual_no")
	idt_accident_date	= date(lwi_active_sheet.dw_basic_claim.GetItemDateTime(1,"accident_date"))


/* ---------------------------------------------------------------------------------------------------- */
/* Retrieve the opening list. If this window has been opened from the w_calculation_details	window  */
/*	then try to select the opening and benefit calculation they were working on in that window.       */
/*	Otherwise, select the last opening and the last benefit calculation in the list.       			  */
/* ---------------------------------------------------------------------------------------------------- */

	ll_nmbr_openings = dw_opening_list.Retrieve(il_claim_no)
	IF SQLCA.nf_handle_error("w_benefits","dw_opening_list","rowfocuschanged for dw_opening_list") < 0 THEN
		Close(This)
		Return 
	END IF

	IF ll_nmbr_openings > 0 THEN
	
		IF istr_bencalc_parameters.al_doubleparm[1] = il_claim_no and istr_bencalc_parameters.al_doubleparm[2] > 0 THEN
			ll_find_opening = dw_opening_list.Find("opening_no = " + string(istr_bencalc_parameters.al_doubleparm[2]),1,ll_nmbr_openings)
		END IF
		
		IF ll_find_opening = 0 THEN 
			ll_find_opening = dw_opening_list.RowCount()
		END IF
		dw_opening_list.ScrollToRow(ll_find_opening)

		/* Add a new Cost Analysis Opening to the Opening List to allow the user to create/copy and view
			Cost Analysis Benefit Calculations (Opening will have an opening number of zero and is just a placeholder
		*/
		ll_rownum =	dw_opening_list.InsertRow(0)
		IF ll_rownum = -1 THEN
			Error.Text = "Error inserting record into dw_opening_list"
			Error.WindowMenu="w_benefits"
			Error.Object="window"
			Error.ObjectEvent="open"
			SignalError()
		END IF
		dw_opening_list.SetItem(ll_rownum,"opening_no", 0 )
		dw_opening_list.SetItem(ll_rownum,"claim_no", il_claim_no )
		dw_opening_list.SetItem(ll_rownum,"opening_type_code","n/a")
		dw_opening_list.SetItem(ll_rownum,"comment","COST/ANALYSIS CALC")
		ll_nmbr_bencalcs = dw_benefit_calculation_list.RowCount()			//	Ben calcs are retrieved in rowfocuschanged of opening dw
																								// and the last ben calc is automatically selected.
		IF ll_nmbr_bencalcs > 0 THEN
			IF istr_bencalc_parameters.al_doubleparm[3] > 0 THEN
				ll_find_bencalc = dw_benefit_calculation_list.Find("benefit_calculation_no = " + string(istr_bencalc_parameters.al_doubleparm[3]),1,ll_nmbr_bencalcs)
			ELSEIF istr_bencalc_parameters.al_doubleparm[4] > 0 THEN
				// if the creation of new bencalc was not completed, then return to the one that was being copied
				ll_find_bencalc = dw_benefit_calculation_list.Find("benefit_calculation_no = " + string(istr_bencalc_parameters.al_doubleparm[4]),1,ll_nmbr_bencalcs)
			END IF
			
			IF ll_find_bencalc > 0 and ll_find_bencalc <> ll_nmbr_bencalcs THEN
				dw_benefit_calculation_list.ScrollToRow(ll_find_bencalc)
			END IF
		END IF

	ELSE	
		/* If no other openings exists:
			Add a new Cost Analysis Opening to the Opening List to allow the user to create/display
			Cost Analysis Benefit Calculations (will have an opening number of zero and is just a placeholder
		*/
		ll_rownum =	dw_opening_list.InsertRow(0)
		IF ll_rownum = -1 THEN
			Error.Text = "Error inserting record into dw_opening_list"
			Error.WindowMenu="w_benefits"
			Error.Object="window"
			Error.ObjectEvent="open"
			SignalError()
		END IF
		dw_opening_list.SetItem(ll_rownum,"opening_no", 0 )
		dw_opening_list.SetItem(ll_rownum,"claim_no", il_claim_no )
		dw_opening_list.SetItem(ll_rownum,"opening_type_code","n/a")
		dw_opening_list.SetItem(ll_rownum,"comment","COST/ANALYSIS CALC")
		dw_opening_list.ScrollToRow(ll_rownum)
		dw_opening_list.TriggerEvent(rowfocuschanged!)

		cb_copy.Enabled = False
		
	END IF

/*	check for overpayment to the claimant
*/
	lds_overpayment_warning_sum = Create U_DS
	lds_overpayment_warning_sum.DataObject = 'ds_overpayment_warning_sum'
	lds_overpayment_warning_sum.SetTransObject(SQLCA)
	li_rows = lds_overpayment_warning_sum.Retrieve(il_claim_no)
	SQLCA.nf_handle_error('w_account_payment','ds_overpayment_warning_sum','lds_overpayment_warning_sum.Retrieve')
	   
	IF li_rows > 0 AND istr_bencalc_parameters.as_stringparm[4] <> 'w_calculation_details' THEN
		sle_overpayment.Text = 'Overpayment'
		lstr_message.as_stringparm[1] = 'OVERPAYMENT situation exists.'
		lstr_message.as_stringparm[2] = 'I'
		lstr_message.al_doubleparm[1] = ll_individual_no
		lstr_message.al_doubleparm[2] = il_claim_no
		
		OpenWithParm(w_payment_message, lstr_message)
	END IF
	


end event

on w_benefits.create
int iCurrent
call super::create
this.cb_add=create cb_add
this.cb_switch=create cb_switch
this.dw_opening_list=create dw_opening_list
this.dw_benefit_calculation_list=create dw_benefit_calculation_list
this.gb_benefit_calcs=create gb_benefit_calcs
this.gb_select_opening=create gb_select_opening
this.cb_copy=create cb_copy
this.sle_overpayment=create sle_overpayment
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_add
this.Control[iCurrent+2]=this.cb_switch
this.Control[iCurrent+3]=this.dw_opening_list
this.Control[iCurrent+4]=this.dw_benefit_calculation_list
this.Control[iCurrent+5]=this.gb_benefit_calcs
this.Control[iCurrent+6]=this.gb_select_opening
this.Control[iCurrent+7]=this.cb_copy
this.Control[iCurrent+8]=this.sle_overpayment
end on

on w_benefits.destroy
call super::destroy
destroy(this.cb_add)
destroy(this.cb_switch)
destroy(this.dw_opening_list)
destroy(this.dw_benefit_calculation_list)
destroy(this.gb_benefit_calcs)
destroy(this.gb_select_opening)
destroy(this.cb_copy)
destroy(this.sle_overpayment)
end on

type st_title from w_a_tool`st_title within w_benefits
integer x = 18
integer width = 2651
integer height = 76
string text = "List Benefit Calculations"
end type

type cb_close from w_a_tool`cb_close within w_benefits
integer y = 1684
integer width = 357
integer height = 96
integer taborder = 10
end type

type cb_add from commandbutton within w_benefits
integer x = 37
integer y = 1684
integer width = 357
integer height = 96
integer taborder = 80
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Add"
end type

event clicked;// Declare variables

W_SHEET					lwi_active_sheet
LONG						ll_opening_row
DATETIME					ldtm_accident_date, ldtm_nulldate, ldtm_benefit_start
INTEGER					li_error_cd , li_count

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


SetPointer (hourglass!)

ll_opening_row = dw_opening_list.GetRow()
IF ll_opening_row <= 0 THEN
	Setpointer (Arrow!)
	Return
END IF


/* Set up Benefit Calculation Parameter defaults for Adding a New Benefit Calculation.
	Transitional Claims are only valid prior to accidents/recurrences in 1993-01-01. The read-only flag was
	set up when the Benefit Calculation option was selected from the Menu options by the user
*/
istr_bencalc_parameters.al_doubleparm[1]			= il_claim_no
istr_bencalc_parameters.al_doubleparm[3]	  		= 0
istr_bencalc_parameters.al_doubleparm[4] 			= 0
istr_bencalc_parameters.al_doubleparm[2]		   = dw_opening_list.GetItemNumber(ll_opening_row,"opening_no")


/* If Adding a Cost Analysis, get the default Opening Parameters and display the Opening
	Parameters Window
*/
IF istr_bencalc_parameters.al_doubleparm[2]	= 0 THEN 
	SetNull (ldtm_nulldate)
	istr_bencalc_parameters.adtm_datetimeparm[2] = ldtm_nulldate							// Benefit Start Date of the opening	
	istr_bencalc_parameters.as_stringparm[3]		= " "											// Opening Type Code
	istr_bencalc_parameters.adtm_datetimeparm[1]	= datetime(idt_accident_date)			// Accident/Recurrence Date	

	/* 
		See documentation for project p10105 this will be defaulted "I" for all types (dates)
	*/
		istr_bencalc_parameters.as_stringparm[1] = "I"  		

 	istr_bencalc_parameters.as_stringparm[2]   = "N"  	 

 	OpenWithParm(w_cost_analysis_parameters,istr_bencalc_parameters)	

ELSE

	istr_bencalc_parameters.as_stringparm[3]			= dw_opening_list.GetItemString(ll_opening_row,"opening_type_code")
	istr_bencalc_parameters.adtm_datetimeparm[1]		= dw_opening_list.GetItemDateTime(ll_opening_row,"accident_recurrence_date")
	istr_bencalc_parameters.adtm_datetimeparm[2]  	= dw_opening_list.GetItemDateTime(ll_opening_row,"benefit_start_date")
	IF IsNull(istr_bencalc_parameters.adtm_datetimeparm[2]) THEN
	/*		must find the matching recurrence in order to get the benefit start date
*/
		SELECT Max(benefit_start_date)
   	  INTO :ldtm_benefit_start
        FROM OPENING
	    WHERE claim_no = :il_claim_no 
			AND opening_type_code = :istr_bencalc_parameters.as_stringparm[3]
			AND recurrence_type_code = 'R'
			AND accident_recurrence_date = :istr_bencalc_parameters.adtm_datetimeparm[1]
		 USING SQLCA;
   	li_error_cd = SQLCA.nf_handle_error("w_benefitsr","Embedded SQL: read OPENING","cb_add")
		IF li_error_cd < 0 THEN
			Close(Parent)
			Return
		ELSEIF li_error_cd = 100 THEN
			istr_bencalc_parameters.adtm_datetimeparm[2]  	= dw_opening_list.GetItemDateTime(ll_opening_row,"benefit_start_date")
		END IF
		istr_bencalc_parameters.adtm_datetimeparm[2]  	= ldtm_benefit_start
	END IF

	/* 
		see documentation for project p10105 defaults to inapplicable
	*/
		istr_bencalc_parameters.as_stringparm[1] = "I"  		

	/* Set the default top-up flag value. Dependeny of the Top-Up flag to the Accident/Recurrence date and the
		three day paid flag will be dealt with when a payment is processed.
	*/
 	istr_bencalc_parameters.as_stringparm[2]   = "N"  


	lwi_active_sheet = w_frame.GetActiveSheet()
	
	// P10261 RTW Incentive changes
	
	SELECT	Count(*)
	INTO		:li_count
	FROM		BENEFIT_CALCULATION	a
	JOIN		OPENING					b	ON	a.claim_no		= b.claim_no
													AND	a.opening_no	= b.opening_no
	WHERE	a.claim_no = :il_claim_no
	AND		a.calculation_type_code = 'A'
	AND		b.opening_no = :istr_bencalc_parameters.al_doubleparm[2]
	USING SQLCA;
	
	SQLCA.nf_handle_error("w_benefits","SELECT Count from BENEFIT_CALCULATION...","cb_add")
	
	IF li_count > 0 THEN
		IF MessageBox('Add Benefit Calculation Warning',		'A benefit calculation should be copied from an existing benefit calculation for the same opening.'+&
																			'~nDo you want to continue?', Exclamation!, YesNo!, 2) = 2 THEN
			RETURN
		END IF
	END IF
	
	
	IF IsValid (lwi_active_sheet.iw_calculation_details) THEN
		lwi_active_sheet.iw_calculation_details.Show()
	ELSE
		OpenWithParm (lwi_active_sheet.iw_calculation_details,istr_bencalc_parameters,lwi_active_sheet)
	END IF
	Close(Parent)
	Return

END IF




end event

type cb_switch from commandbutton within w_benefits
integer x = 1838
integer y = 1684
integer width = 357
integer height = 96
integer taborder = 70
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Details"
end type

event clicked;// Declare variables

W_SHEET					lwi_active_sheet
LONG						ll_rownum,					ll_opening_row, 	ll_opening_no

/* ***********************************************************
	Benefit Calculation Parameters stored in s_window_message

	Claim No								al_doubleparm[1]
	Opening No							al_doubleparm[2]
	Benefit Calculation No			al_doubleparm[3]
	Copy Benefit Calculation No	al_doubleparm[4]
	Copy BenCalc Opening No			al_doubleparm[5]

	Transitional Claim Flag			as_stringparm[1]
	Top-up Flag							as_stringparm[2]
	Opening Type Code					as_stringparm[3]

	Accident Recurrence Date		adtm_datetimeparm[1]
	Benefit Start Date				adtm_datetimeparm[2]

	Read/Update Mode					as_mode 

	**********************************************************
*/

SetPointer (hourglass!)

ll_opening_row = dw_opening_list.GetRow()
IF ll_opening_row <= 0 THEN
	Setpointer (Arrow!)
	Return
END IF

ll_rownum = dw_benefit_calculation_list.GetRow()
IF ll_rownum <= 0 THEN
	Setpointer (Arrow!)
	Return
END IF

ll_opening_no = dw_opening_list.GetItemNumber(ll_opening_row,"opening_no")

/* Set up Benefit Calculaton parameters for displaying details of the benefit calculation selected
*/
istr_bencalc_parameters.al_doubleparm[4]			= 0
istr_bencalc_parameters.al_doubleparm[1]			= il_claim_no
istr_bencalc_parameters.al_doubleparm[2]			= ll_opening_no
istr_bencalc_parameters.al_doubleparm[3]   		= dw_benefit_calculation_list.GetItemNumber(ll_rownum,"benefit_calculation_no")
istr_bencalc_parameters.as_stringparm[1]   		= dw_benefit_calculation_list.GetItemString(ll_rownum,"transitional_claim_flag")
istr_bencalc_parameters.as_stringparm[2]			= dw_benefit_calculation_list.GetItemString(ll_rownum,"top_up_flag")


/* If a Cost Analysis opening, the accident/recurrence date defaults to the Claim Accident Date and the
	opening Type code is determined from the frequency of the Benefit Calculation
*/
IF ll_opening_no = 0 THEN
	istr_bencalc_parameters.adtm_datetimeparm[1]	= datetime(idt_accident_date)		
	IF dw_benefit_calculation_list.GetItemString(ll_rownum,"award_freq_code") = "M" then		
		istr_bencalc_parameters.as_stringparm[3]	 = "LOE"
	ELSE
		istr_bencalc_parameters.as_stringparm[3]	 = "RLOE"
	END IF
ELSE
	istr_bencalc_parameters.adtm_datetimeparm[1]		= dw_opening_list.GetItemDateTime(ll_opening_row,"accident_recurrence_date")
	istr_bencalc_parameters.as_stringparm[3]	   	= dw_opening_list.GetItemString(ll_opening_row,"opening_type_code")
END IF


lwi_active_sheet = w_frame.GetActiveSheet()

OpenWithParm (lwi_active_sheet.iw_calculation_details,istr_bencalc_parameters,lwi_active_sheet)

IF IsValid (lwi_active_sheet.iw_calculation_details) THEN
	lwi_active_sheet.iw_calculation_details.Show()
END IF

Close(parent)
Return


end event

type dw_opening_list from u_dw_online within w_benefits
integer x = 46
integer y = 212
integer width = 2583
integer height = 480
integer taborder = 40
string dataobject = "d_opening_list"
boolean vscrollbar = true
end type

event rowfocuschanged;call super::rowfocuschanged;LONG				ll_opening_no,		ll_nmbr_bencalcs
STRING			ls_rtw_incentive_flag

//	Automatically retrieve the benefit calculations when a new row has been selected

IF CurrentRow > 0 THEN

	ll_opening_no = GetItemNumber(CurrentRow,"opening_no")
	IF IsNull(ll_opening_no) THEN ll_opening_no = 0

	ll_nmbr_bencalcs = dw_benefit_calculation_list.Retrieve(il_claim_no,ll_opening_no)
	IF SQLCA.nf_handle_error("w_benefits","dw_benefit_calculation_list","rowfocuschanged for dw_opening_list") < 0 THEN
		Close(parent)
		Return 
	END IF

	IF ll_nmbr_bencalcs > 0 THEN
			
		dw_benefit_calculation_list.ScrollToRow(ll_nmbr_bencalcs)
		ls_rtw_incentive_flag = dw_benefit_calculation_list.GetItemString(ll_nmbr_bencalcs,'rtw_incentive_flag')
		cb_switch.Enabled = True
		IF upper(istr_bencalc_parameters.as_mode) <> "READ" AND ls_rtw_incentive_flag = 'N' THEN
			cb_copy.Enabled	= True
		END IF
	ELSE
		cb_switch.Enabled = False
		cb_copy.Enabled	= False
	END IF

 	uf_ProcessSelect(CurrentRow,"Keyboard")
	 
END IF

end event

type dw_benefit_calculation_list from u_dw_online within w_benefits
integer x = 46
integer y = 800
integer width = 2583
integer height = 780
integer taborder = 30
string dataobject = "d_benefit_calculation_list"
boolean vscrollbar = true
end type

event rowfocuschanged;call super::rowfocuschanged;STRING		ls_rtw_incentive_flag

uf_ProcessSelect(CurrentRow,"Keyboard")

IF CurrentRow > 0 THEN
	ls_rtw_incentive_flag = THIS.GetItemString(currentrow,'rtw_incentive_flag')
	IF ls_rtw_incentive_flag = 'Y' THEN
		cb_copy.Enabled = FALSE
	ELSE
		cb_copy.Enabled = TRUE
	END IF
END IF
end event

on doubleclicked;call u_dw_online::doubleclicked;cb_switch.TriggerEvent(Clicked!)
end on

type gb_benefit_calcs from groupbox within w_benefits
integer x = 18
integer y = 728
integer width = 2633
integer height = 876
integer taborder = 50
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Benefit Calculations for Opening"
end type

type gb_select_opening from groupbox within w_benefits
integer x = 18
integer y = 152
integer width = 2633
integer height = 564
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Select Opening"
end type

type cb_copy from commandbutton within w_benefits
integer x = 416
integer y = 1684
integer width = 357
integer height = 96
integer taborder = 60
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "C&opy..."
end type

event clicked;Long     ll_current_opening, ll_cntr, ll_count_opening_list, ll_openings_found, ll_opening_no, ll_rownum
Datetime ldtm_accident_recurrence_date, ldtm_nulldate 
String   ls_opening_type_code, ls_copy_cost_dw, ls_copy_bencalc_dw
Integer  li_rtn
w_sheet  lwi_active_sheet

//	Benefit Calculation Parameters stored in s_window_message
//
//	Claim No                       al_doubleparm[1]
//	Opening No                     al_doubleparm[2]
//	Benefit Calculation No         al_doubleparm[3]
//	Copy Benefit Calculation No    al_doubleparm[4]
//	Copy BenCalc Opening No        al_doubleparm[5]
//	number of deduction records    al_doubleparm[6]
//	number of remuneration records al_doubleparm[7]
//	Transitional Claim Flag        as_stringparm[1]
//	Top-up Flag                    as_stringparm[2]
//	Opening Type Code              as_stringparm[3]
//	Accident Recurrence Date       adtm_datetimeparm[1]
//	Benefit Start Date             adtm_datetimeparm[2]
//	Effective Date                 adtm_datetimeparm[3]
//	Read/Update Mode               as_mode 

SetPointer(Hourglass!)

ls_copy_cost_dw = "d_copy_cost_analysis_list"
ls_copy_bencalc_dw = "d_copy_opening_list"

// Get the Benefit Calculation to be copied.  Manual calculations cannot be copied.
ll_rownum = dw_benefit_calculation_list.GetRow()
istr_bencalc_parameters.al_doubleparm[4] = dw_benefit_calculation_list.GetItemNumber(ll_rownum,"benefit_calculation_no")

IF ll_rownum <= 0 THEN
	MessageBox("Benefit Calculation Module", "There are no benefit calculations for this opening", Exclamation!)
	Setpointer (Arrow!)
	Return
END IF

IF dw_benefit_calculation_list.GetItemString(ll_rownum, "calculation_type_code") = "M" THEN
	MessageBox("Benefit Calculation Module", "You cannot copy a manual calculation", Exclamation!)
	Setpointer(Arrow!)
	RETURN
END IF


// Get the current opening
ll_current_opening = dw_opening_list.GetRow()
ll_opening_no = dw_opening_list.GetItemNumber(ll_current_opening, "opening_no")

// If copying from a Cost Analysis to an existing Opening
IF ll_opening_no = 0 THEN
	// Set up the following Benefit Calculation Parameters for copying a benefit calculation (the access mode flag was set 
	//	when the user selected the Benefit Calculaton item from the Menu Option):
	//	1) Claim Number							(the claim number of the current claim)
	//	2) Benefit Calculation Number 		(initialized to zero: this will be determined when the new benefit calculation is created)
	//	3) Opening Type Code						(determined from the award frequency of the current benefit calculation)
	//	4) Accident/Recurrence Date			(to be determined from the opening selected)
	//	5) Transitional Claim Flag				(defaults to the same as the benefit being copied)
	//	6) Top-Up Flag								(defaults to the same as the benefit being copied)
	//	7) Copy Benefit Calculation Mumber	(the benefit calculation number of the benefit being copied)
	//	8) Copy Ben Calc opening No			(the opening number of the benefit calculation being copied)
	//	9) Effective Date							(the effective date of the benefit calculation being copied)

	istr_bencalc_parameters.al_doubleparm[1] = il_claim_no	
	istr_bencalc_parameters.al_doubleparm[3] = 0										
	istr_bencalc_parameters.as_stringparm[1] = dw_benefit_calculation_list.GetItemString(ll_rownum,"transitional_claim_flag")
	istr_bencalc_parameters.as_stringparm[2] = dw_benefit_calculation_list.GetItemString(ll_rownum,"top_up_flag")

	istr_bencalc_parameters.al_doubleparm[5] = ll_opening_no
	istr_bencalc_parameters.adtm_datetimeparm[3]	= dw_benefit_calculation_list.GetItemdatetime(ll_rownum, "effective_from_date")
	IF dw_benefit_calculation_list.GetItemString(ll_rownum, "award_freq_code") = "M" then		
		ls_opening_type_code = "LTD"
	ELSE
		ls_opening_type_code = "RLOE"
	END IF
	istr_bencalc_parameters.as_stringparm[3]	= ls_opening_type_code

	// Check the current opening list to see if there is more than one of the same opening type as the opening type determined
	//	from the Benefit Calculation's award frequency code. Get the opening number, benefit start date and the
	//	accident/recurrence date of any qualifying Openings as you loop  through the list looking for openings to copy
	//	to in case there is only one that meets the criteria (this way,  we won't have to go back and get the Opening information 
	//	all over again when we determine there was only one qualifying Opening). 

	ll_openings_found = 0
	ll_cntr = 1
	ll_count_opening_list = dw_opening_list.RowCount()
	DO WHILE ll_cntr <= ll_count_opening_list
		IF dw_opening_list.GetItemNumber(ll_cntr, "opening_no") <> 0 THEN
			IF dw_opening_list.GetItemString(ll_cntr, "opening_type_code") = ls_opening_type_code THEN
				ll_openings_found = ll_openings_found + 1
				istr_bencalc_parameters.al_doubleparm[2]		= dw_opening_list.GetItemNumber(ll_cntr,"opening_no")
				istr_bencalc_parameters.adtm_datetimeparm[1] = dw_opening_list.GetItemDateTime(ll_cntr,"accident_recurrence_date")	
				istr_bencalc_parameters.adtm_datetimeparm[2] = dw_opening_list.GetItemDateTime(ll_cntr,"benefit_start_date")	
			END IF
		END IF
		ll_cntr ++
	LOOP

	IF ll_openings_found = 0  THEN
		MessageBox("Benefit Calculation", "There are no openings of the same Opening Type to copy this Cost Analysis to", Exclamation!)
		Setpointer(Arrow!)
		RETURN
	END IF

	// If there is more than one opening that meets the criteria, then set up defaults for the parameters and display a window listing the openings.
	IF ll_openings_found > 1 THEN
		// The remaining parameter fields will be set once the user selects an opening from the list of openings of the same opening type 
		SetNull(ldtm_nulldate)
		istr_bencalc_parameters.al_doubleparm[2] = 0								// Opening Number 
		istr_bencalc_parameters.adtm_datetimeparm[2] = ldtm_nulldate		// Benefit Start Date of the opening								 
		OpenWithParm(w_copy_opening_list, istr_bencalc_parameters)
	ELSE
		// If there is only one opening that meets the criteria, then do not display a window listing the
		// openings. Default the opening to the opening found and set up the remaining parameters.
		lwi_active_sheet = w_frame.GetActiveSheet()
		IF IsValid (lwi_active_sheet.iw_calculation_details) THEN
			lwi_active_sheet.iw_calculation_details.Show()
		ELSE
			OpenWithParm(lwi_active_sheet.iw_calculation_details, istr_bencalc_parameters, lwi_active_sheet)
		END IF
		Close(Parent)
		RETURN 
	END IF
ELSE
	// If copying from a Benefit Calculation having an Opening to another Opening of the same accident/recurrence date and the same Opening Type
	//	Set up the following Benefit Calculation Parameters for copying a benefit calculation (the access mode flag was set 
	//	when the user selected the Benefit Calculaton item from the Menu Option):
	//	1) Claim Number							(the claim number of the current claim)
	//	2) Benefit Calculation Number 		(initialized to zero: this will be determined when the new benefit calculation is created)
	//	3) Opening Type Code						(always the same as the current opening)
	//	4) Accident/Recurrence Date			(always the same as the current opening)
	//	5) Transitional Claim Flag				(defaults to the same as the benefit being copied)
	//	6) Top-Up Flag								(defaults to the same as the benefit being copied)
	//	7) Copy Benefit Calculation Mumber	(the benefit calculation number of the benefit being copied)
	//	8) Effective Date							(the effective date of the benefit calculation being copied)

	istr_bencalc_parameters.al_doubleparm[1] = il_claim_no	
	istr_bencalc_parameters.al_doubleparm[3] = 0									
	istr_bencalc_parameters.as_stringparm[3] = dw_opening_list.GetItemString(ll_current_opening, "opening_type_code")
	istr_bencalc_parameters.adtm_datetimeparm[1]	= dw_opening_list.GetItemDateTime(ll_current_opening, "accident_recurrence_date")				
	istr_bencalc_parameters.as_stringparm[1] = dw_benefit_calculation_list.GetItemString(ll_rownum, "transitional_claim_flag")
	istr_bencalc_parameters.as_stringparm[2] = dw_benefit_calculation_list.GetItemString(ll_rownum, "top_up_flag")
	istr_bencalc_parameters.al_doubleparm[5] = ll_opening_no
	istr_bencalc_parameters.adtm_datetimeparm[3]	= dw_benefit_calculation_list.GetItemDateTime(ll_rownum, "effective_from_date")

	// Get the Accident Date and the Opening Type Code of the current opening. This will be used as the criteria for listing the Openings that the user can select from.
	ldtm_accident_recurrence_date = dw_opening_list.GetItemDateTime(ll_current_opening, "accident_recurrence_date")
	ls_opening_type_code = dw_opening_list.GetItemString(ll_current_opening, "opening_type_code")

	// Check the current opening list to see if there is more than one of the same opening type and same accident/recurrence date as the current opening. 
	ll_openings_found = 0
	ll_cntr = 1
	ll_count_opening_list = dw_opening_list.RowCount()
	DO WHILE ll_cntr <= ll_count_opening_list
		IF dw_opening_list.GetItemNumber(ll_cntr,"opening_no") <> 0 THEN
			IF dw_opening_list.GetItemDateTime(ll_cntr,"accident_recurrence_date") = ldtm_accident_recurrence_date and &
				dw_opening_list.GetItemString(ll_cntr,"opening_type_code") = ls_opening_type_code THEN
				ll_openings_found = ll_openings_found + 1
			END IF
		END IF
		ll_cntr ++
	LOOP 

	// Set structure variable to count of deductions from copied bencalc
	SELECT Count(*)
	  INTO :istr_bencalc_parameters.al_doubleparm[6] 
	  FROM BENEFIT_CALC_DEDUCTION
	 WHERE claim_no = :il_claim_no
	   AND benefit_calculation_no = :istr_bencalc_parameters.al_doubleparm[4] 
	 USING SQLCA ;

	li_rtn = SQLCA.nf_handle_error("w_benefits", "", "cb_copy - SELECT Count(*) INTO :istr_bencalc_parameters.al_doubleparm[6] FROM BENEFIT_CALC_DEDUCTION WHERE claim_no = :il_claim_no")

	// set structure variable to count of remunerations from copied bencalc
	SELECT Count(*)
	  INTO :istr_bencalc_parameters.al_doubleparm[7] 
	  FROM REMUNERATION
	 WHERE claim_no = :il_claim_no
	   AND benefit_calculation_no = :istr_bencalc_parameters.al_doubleparm[4] 
	   AND remun_type_code = 'WAGE' 
	 USING SQLCA ;

	li_rtn = SQLCA.nf_handle_error("w_benefits", "", "cb_copy - SELECT Count(*) INTO :istr_bencalc_parameters.al_doubleparm[7] FROM REMUNERATION WHERE claim_no = :il_claim_no")

	// If there is more than one opening that meets the criteria, then display a window listing the openings.
	IF ll_openings_found > 1 THEN
		// The remaining parameter fields will be set once the user selects an opening from the list of
		// openings of the same accident/recurrence date and same opening type as the current opening. 
		SetNull(ldtm_nulldate)
		istr_bencalc_parameters.al_doubleparm[2] = 0							// Opening Number 
		istr_bencalc_parameters.adtm_datetimeparm[2] = ldtm_nulldate	// Benefit Start Date of the opening								 
		OpenWithParm(w_copy_opening_list, istr_bencalc_parameters)
		Close(Parent)
		RETURN
	ELSE
		// If there is only one opening that meets the criteria, then do not display a window listing the
		// openings. Default the opening to the current opening and set up the remaining parameters.
		istr_bencalc_parameters.al_doubleparm[2] = dw_opening_list.GetItemNumber(ll_current_opening,"opening_no")
		istr_bencalc_parameters.adtm_datetimeparm[2] = dw_opening_list.GetItemDateTime(ll_current_opening,"benefit_start_date")	
		lwi_active_sheet = w_frame.GetActiveSheet()

		IF IsValid (lwi_active_sheet.iw_calculation_details) THEN
			lwi_active_sheet.iw_calculation_details.Show()
		ELSE
			OpenWithParm(lwi_active_sheet.iw_calculation_details, istr_bencalc_parameters, lwi_active_sheet)
		END IF
		Close(Parent)
		RETURN
	END IF
END IF
end event

type sle_overpayment from singlelineedit within w_benefits
integer x = 2295
integer y = 12
integer width = 366
integer height = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 255
long backcolor = 67108864
boolean border = false
boolean autohscroll = false
end type

