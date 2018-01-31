$PBExportHeader$nvo_payment_cheque_register.sru
$PBExportComments$Non-visual user object containing functions used to produce payment and/or cheque registers.
forward
global type nvo_payment_cheque_register from nonvisualobject
end type
end forward

global type nvo_payment_cheque_register from nonvisualobject
end type
global nvo_payment_cheque_register nvo_payment_cheque_register

type variables
STRING		is_register_type
STRING		is_benefit_class_code
DATETIME	idt_processed_date
DATETIME	idt_payment_processed_date1
DATETIME	idt_payment_processed_date2
DATETIME              idt_start_date
DATETIME              idt_end_date
U_DW_ONLINE	idw_criteria
U_DW_ONLINE	idw_register
end variables

forward prototypes
public function integer of_payment_cheque_register_process ()
public function integer of_retrieve ()
public subroutine of_print_register ()
public subroutine of_set_processed_date_filter (string as_data, string as_column_name, ref u_dw_online adw_criteria_datawindow)
public function integer of_validate_criteria (ref u_dw_online adw_criteria, ref u_dw_online adw_register)
public subroutine of_set_register_d_object ()
end prototypes

public function integer of_payment_cheque_register_process ();/*	Perform the process of creating the appropriate register.
*/
	INTEGER	li_result
	
/*	Call function to retrieve the information.
*/
	li_result = of_retrieve()
	
	IF li_result < 0 THEN
		Return -1
	END IF

	Return 0

end function

public function integer of_retrieve ();Long ll_rows_returned

// This function is used to retrieve the information for the register.
//	If is_benefit_class_code is NULL, then the user has choosen to produce a register for all
// benefit class codes.

IF is_register_type = 'E' THEN
	idt_end_date = Datetime(RelativeDate(Date(idt_end_date), 1))
	ll_rows_returned = idw_register.Retrieve(idt_start_date, idt_end_date)
ELSE
	IF is_benefit_class_code = 'ALL' THEN
		IF is_register_type <> 'C' THEN
			ll_rows_returned = idw_register.Retrieve(idt_processed_date)
		ELSE
			ll_rows_returned = idw_register.Retrieve(idt_payment_processed_date1,idt_payment_processed_date2)
		END IF
	ELSE
		IF is_register_type <> 'C' THEN
			ll_rows_returned = idw_register.Retrieve(is_benefit_class_code,idt_processed_date)
		ELSE
			ll_rows_returned = idw_register.Retrieve(is_benefit_class_code,idt_payment_processed_date1,idt_payment_processed_date2)
		END IF
	END IF
END IF

IF SQLCA.nf_handle_error("nvo_payment_cheque_register","of_retrieve()","idw_register.Retrieve()") < 0 THEN
	Return -1
END IF

IF ll_rows_returned = 0 THEN
	MessageBox("Register Report","No data was found for the entered criteria.",Exclamation!)
END IF

Return 0

end function

public subroutine of_print_register ();/*	This function is used to print the current register.
*/
	INTEGER	li_result
	
	li_result = idw_register.Print()
	
	IF li_result < 0 THEN
		MessageBox("Case Management Workbench","An error occurred trying to print the register.",Exclamation!)
	END IF
	
	Return

end subroutine

public subroutine of_set_processed_date_filter (string as_data, string as_column_name, ref u_dw_online adw_criteria_datawindow);/*	This function is used to filter out the non-appropriate dates for a certain
	benefit class code.
	
	Arguments:	as_benefit_class_code	- The selected benefit type code.
					as_column_name				- The name of the column being changed.
					adw_criteria_datawindow	- The criteria datawindow.
*/
DATAWINDOWCHILD	ldwc_child
INTEGER				li_result
STRING				ls_column_name, ls_register_type, ls_filter, ls_benefit_class_code, ls_child_column
DATETIME				ldtm_null

SetNull(ldtm_null)

adw_criteria_datawindow.AcceptText()

CHOOSE CASE	as_column_name
	CASE	'register_type'
		ls_register_type = as_data
		ls_benefit_class_code = adw_criteria_datawindow.GetItemString(1,'benefit_class_code')
	CASE	'benefit_class_code'
		ls_register_type = adw_criteria_datawindow.GetItemString(1,'register_type')
		ls_benefit_class_code = as_data
END CHOOSE

CHOOSE CASE	ls_register_type
	CASE	'A'		// Cheque registers, filter out all process dates that are not for the selected benefit class code.
		ls_column_name = 'cbh_processed_date'
		ls_child_column = 'processed_date'
		IF ls_benefit_class_code = 'ALL' THEN
			ls_filter = "benefit_class_code <> ''"
		ELSE
			ls_filter = "benefit_class_code = '" + ls_benefit_class_code + "'"
		END IF
	CASE  'D'  // There is no benefit class code for Employer Refund cheques.
		ls_column_name = 'cbh_processed_date'
		ls_child_column = 'processed_date'
		ls_filter = "benefit_class_code = ''"
	CASE  'E'  // Dates are entered for other type cheques 
		Return
	CASE	'B'			// Direct Deposit registers, filter out all process dates that are not for the selected benefit class code.
		ls_column_name = 'processed_xmit_date'
		ls_child_column = 'processed_xmit_date'
		IF ls_benefit_class_code = 'ALL' THEN
			ls_filter = ''
		ELSE
			ls_filter = "benefit_class_code = '" + ls_benefit_class_code + "'"
		END IF
	CASE	'C'
		Return					//Nothing to be done for Payment registers as date is enters, so return.
END CHOOSE

li_result = adw_criteria_datawindow.GetChild(ls_column_name,ldwc_child)

IF li_result < 0 THEN
	MessageBox("Register Error","An error occured filtering the processed dates for register you wish to run.",Exclamation!)
ELSE
	li_result = ldwc_child.SetFilter(ls_filter)
	IF li_result < 0 THEN
		MessageBox("Register Error","An error occured filtering the processed dates for register you wish to run.",Exclamation!)
	ELSE
		li_result = ldwc_child.Filter()
		IF li_result < 0 THEN
			MessageBox("Register Error","An error occured filtering the processed dates for register you wish to run.",Exclamation!)
		END IF
	END IF
END IF

IF ldwc_child.RowCount() > 0 THEN
	adw_criteria_datawindow.SetItem(1,ls_column_name,ldwc_child.GetItemDateTime(1, ls_child_column))
ELSE
	adw_criteria_datawindow.SetItem(1,ls_column_name,ldtm_null)
END IF

Return

end subroutine

public function integer of_validate_criteria (ref u_dw_online adw_criteria, ref u_dw_online adw_register);/*	This function is used to validate that all the values inputted by the user are
	valid.
	
	Argument:	adw_criteria	- The datawindow containing the entered values.
					adw_register	- The datawindow containing the register.
*/
	DATETIME		ldt_current_date

/*	Assign the arguments to instance variables so they can be used later in other functions.
*/
	idw_criteria = adw_criteria
	idw_register = adw_register

	idw_criteria.AcceptText()

/*	Grab the entered values.
*/
	ldt_current_date = f_server_datetime()
	
	is_register_type = idw_criteria.GetItemString(1,'register_type')
	is_benefit_class_code = idw_criteria.GetItemString(1,'benefit_class_code')
	CHOOSE CASE	is_register_type
		CASE	'A'
			idt_processed_date = idw_criteria.GetItemDatetime(1,'cbh_processed_date')
		CASE	'B'
			idt_processed_date = idw_criteria.GetItemDatetime(1,'processed_xmit_date')
		CASE	'C'
			idt_processed_date = idw_criteria.GetItemDatetime(1,'processed_date')
		CASE	'D'
			idt_processed_date = idw_criteria.GetItemDatetime(1,'cbh_processed_date')
		CASE	'E'
			idt_start_date = idw_criteria.GetItemDatetime(1,'start_date')
			idt_end_date = idw_criteria.GetItemDatetime(1,'end_date')
			IF IsNull(idt_start_date) THEN
				MessageBox("Case Management Workbench","The Start date is required and must be entered, please enter one.",Exclamation!)
				RETURN -1
			END IF
			IF IsNull(idt_end_date) THEN
				MessageBox("Case Management Workbench","The End date is required and must be entered, please enter one.",Exclamation!)
				RETURN -1
			END IF
			IF idt_start_date > idt_end_date THEN
				MessageBox("Case Management Workbench","The Start date must be before the End date, please re-enter.",Exclamation!)
				RETURN -1
			END IF
	END CHOOSE

/*	The columns register type, and processed date are required. Check to see if entered.
*/
	IF IsNull(is_register_type) THEN
		MessageBox("Case Management Workbench","The register type is required and must be selected, please select one.",Exclamation!)
		Return -1
	END IF

	IF IsNull(idt_processed_date) THEN
		MessageBox("Case Management Workbench","The processed date is required and must be entered, please enter one.",Exclamation!)
		Return -1
	END IF
	
/*	The benefit class is required if the register type is a direct deposit.
*/
	IF is_register_type = 'B' AND is_benefit_class_code = 'ALL' THEN
		MessageBox("Case Management Workbench","The benefit class is required when the type of register to create is Direct Deposit, please select one.",Exclamation!)
		Return -1
	END IF
	
/*	The benefit class must be 'All' if the register type is a employer refund.
*/
	IF (is_register_type = 'D' OR is_register_type = 'E') AND is_benefit_class_code <> 'ALL' THEN
		is_benefit_class_code = 'ALL'
		idw_criteria.SetItem(1,'benefit_class_code', 'ALL')
	END IF

/*	Processed date can not be in the future.
*/
	IF idt_processed_date > ldt_current_date THEN
		MessageBox("Case Management Workbench","The entered processed date can not be in the future, please re-enter.",Exclamation!)
		Return -1
	END IF

/*	Set the values for idt_payment_processed_date1, idt_payment_processed_date2 if the register is
	a payment register.
*/
	IF is_register_type = 'C' THEN
		idt_payment_processed_date1 = DateTime(Date(idt_processed_date))
		idt_payment_processed_date2 = DateTime(RelativeDate(Date(idt_processed_date),1))
	END IF

/*	Call function to set the appropriate register datawindow based on the entered choices.
*/
	of_set_register_d_object()

	Return 0

end function

public subroutine of_set_register_d_object ();/*	This function is used to set the appropriate register datawindow based on the values
	of is_benefit_class_code and is_register_type.
*/
	CHOOSE CASE	is_register_type
		CASE	'A'											// Cheque register for Benefit Payments.
			CHOOSE CASE	is_benefit_class_code
				CASE	'LOE', 'MA', 'P','SP'
					idw_register.DataObject = 'd_bp_chq_register_one_ben_class'
				CASE ELSE
					idw_register.DataObject = 'd_bp_chq_register_all_ben_class'
			END CHOOSE
			
		CASE	'B'											// Direct Deposit register for Benefit Payments.
			idw_register.DataObject = 'd_bp_dd_register_one_ben_class'
			
		CASE	'C'											// General Payment register for Benefit Payments.
			CHOOSE CASE	is_benefit_class_code
				CASE	'LOE', 'MA', 'P'
					idw_register.DataObject = 'd_bp_p_register_one_ben_class'
				CASE ELSE
					idw_register.DataObject = 'd_bp_p_register_all_ben_class'
			END CHOOSE
			
		CASE	'D'											// Cheque register for Employer Refunds.
			idw_register.DataObject = 'd_er_chq_register_all_ben_class'

		CASE	'E'											// Cheque register for Other
			idw_register.DataObject = 'd_other_chq_register_all_ben_class'
	END CHOOSE

	idw_register.SetTransObject(SQLCA)
	
	Return

end subroutine

on nvo_payment_cheque_register.create
call super::create
TriggerEvent( this, "constructor" )
end on

on nvo_payment_cheque_register.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

