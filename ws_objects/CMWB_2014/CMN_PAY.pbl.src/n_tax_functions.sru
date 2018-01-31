$PBExportHeader$n_tax_functions.sru
$PBExportComments$Object used to hold basic tax functions - related functions to for the TAX_RATE table.
forward
global type n_tax_functions from nonvisualobject
end type
end forward

global type n_tax_functions from nonvisualobject
end type
global n_tax_functions n_tax_functions

forward prototypes
public function string nf_check_provider_tax_flag (long al_provider_no, string as_provider_type)
public function integer nf_check_tax_flag (string as_payment_type, string as_sub_type, string as_recipient_type, long al_provider_no, ref boolean ab_protect)
public function string nf_check_recipient_type_tax (string as_pymt_recpt_type)
public function string nf_check_payment_type_tax (string as_payment_type, string as_payment_sub_type)
public function integer nf_check_valid_from_date (date ad_paid_from, date ad_paid_to, ref decimal adc_rate)
public function integer nf_get_valid_tax_rate (date ad_paid_from, date ad_paid_to, ref decimal adc_rate)
public function integer nf_get_max_tax_amount (datetime adtm_paid_from, datetime adtm_paid_to, decimal adc_payment_amount, ref decimal adc_max_tax_allowed)
public subroutine nf_get_max_tax_amount (decimal adec_txn_amount, decimal adec_adjustment_amount, decimal adec_tax_rate, ref decimal adec_max_tax_amount)
public subroutine nf_get_max_adj_tax_amount (decimal adec_adjustment_amount, decimal adec_tax_rate, ref decimal adec_max_adjustment_amount)
public subroutine nf_get_min_adj_tax_amount (decimal adec_tax_amount, decimal adec_txn_amount, decimal adec_txn_adjustment_amount, decimal adec_tax_rate, ref decimal adec_min_adjustment_amount)
end prototypes

public function string nf_check_provider_tax_flag (long al_provider_no, string as_provider_type);/* function to check the tax flag for Provider as individual providers have
   tax flags
   passes in provider_no, provider_type_code
	returns the tax flag
*/
STRING ls_tax_flag

ls_tax_flag = "N"

SELECT tax_flag INTO :ls_tax_flag FROM PROVIDER 
 WHERE provider_no        = :al_provider_no
   AND provider_type_code = :as_provider_type
 USING SQLCA;
				 
 SQLCA.nf_handle_error("SELECT tax_flag INTO :ls_tax_flag FROM PROVIDER ","n_tax_functions","nf_check_provider_tax_flag()")
				
IF ISNULL(ls_tax_flag) THEN ls_tax_flag = "N"				

RETURN ls_tax_flag
			
end function

public function integer nf_check_tax_flag (string as_payment_type, string as_sub_type, string as_recipient_type, long al_provider_no, ref boolean ab_protect);/* function to check the tax flag for recipient and payment types 
   passes in payment_type and sub type for one check and recipient type 
	for the other - if you dont want one returned pass an empty string
	the true value of the passed by reference boolean is what you want to check
	the return is to ensure the function ends correctly
	
	the boolean is defaulted to true
*/

/*
Only service provider payments will have a tax amount > o
Recipient_Type.tax_flag = "Y"
if this isnt the case reset the tax amount to 0 and protect the column
else unprotect the column
*/

STRING ls_tax_flag

SELECT tax_flag INTO :ls_tax_flag FROM Payment_Sub_Type
 WHERE payment_type_code     = :as_payment_type
	AND payment_sub_type_code = :as_sub_type
 USING SQLCA;
				 
SQLCA.nf_handle_error("SELECT tax_flag INTO :ls_tax_flag FROM Payment_Sub_Type ","n_tax_functions","nf_check_tax_flag()")
				
CHOOSE CASE ls_tax_flag			
	CASE "N"
		ab_protect = TRUE
		RETURN 1		
	CASE ELSE//keep going
END CHOOSE

/*
Only service provider payments will have a tax amount > o
Recipient_Type.tax_flag = "Y"
if this isnt the case reset the tax amount to 0 and protect the column
else unprotect the column
*/
SELECT tax_flag INTO :ls_tax_flag FROM Recipient_Type 
 WHERE recipient_type_code = :as_recipient_type
 USING SQLCA;
				 
 SQLCA.nf_handle_error("SELECT tax_flag INTO :ls_tax_flag FROM Recipient_Type ","n_tax_functions","nf_check_tax_flag()")
				
CHOOSE CASE ls_tax_flag
  CASE "N"
	   ab_protect = TRUE
		RETURN 1
  CASE ELSE
 END CHOOSE
 
 /* now check the provider tax flag */
 SELECT tax_flag INTO :ls_tax_flag FROM PROVIDER 
 WHERE provider_no        = :al_provider_no
   AND provider_type_code = :as_recipient_type
 USING SQLCA;
				 
 SQLCA.nf_handle_error("SELECT tax_flag INTO :ls_tax_flag FROM PROVIDER ","n_tax_functions","nf_check_tax_flag()")
				
CHOOSE CASE ls_tax_flag
  CASE "N"
	   ab_protect = TRUE
		RETURN 1
  CASE ELSE
 END CHOOSE
 
				  
ab_protect = FALSE
RETURN 1
			
end function

public function string nf_check_recipient_type_tax (string as_pymt_recpt_type);//	Get recipient type HST allowed tax flag - this will be validated 
// on the code call side - you could just as easily return a 1 or -1
// for the return of this function.

STRING  ls_recpt_tax_flag

ls_recpt_tax_flag = "N"

SELECT tax_flag	
  INTO :ls_recpt_tax_flag
  FROM Recipient_Type
 WHERE recipient_type_code = :as_pymt_recpt_type
 USING SQLCA;
		 
SQLCA.nf_handle_error("SELECT tax_flag FROM Recipient_Type","n_tax_functions","nf_check_recipient_type_tax()") 				

IF ISNULL(ls_recpt_tax_flag) THEN ls_recpt_tax_flag = "N"				

RETURN ls_recpt_tax_flag
			
end function

public function string nf_check_payment_type_tax (string as_payment_type, string as_payment_sub_type);//	Get payment type HST allowed tax flag - this will be validated 
// on the code call side - you could just as easily return a 1 or -1
// for the return of this function.

STRING  ls_pymt_tax_flag

ls_pymt_tax_flag = "N"

//	Get payment type & sub type combo HST allowed tax flag
SELECT tax_flag	
  INTO :ls_pymt_tax_flag
  FROM Payment_Sub_Type
 WHERE payment_type_code     = :as_payment_type
   AND payment_sub_type_code = :as_payment_sub_type
 USING SQLCA;
		 
SQLCA.nf_handle_error("SELECT tax_flag	INTO :ls_pymt_tax_flag","n_tax_functions","nf_check_payment_type_tax()") 				

IF ISNULL(ls_pymt_tax_flag) THEN ls_pymt_tax_flag = "N"				

RETURN ls_pymt_tax_flag
			
end function

public function integer nf_check_valid_from_date (date ad_paid_from, date ad_paid_to, ref decimal adc_rate);/*
This function will grab the valid tax rate for a given paid from and to date
'Only one tax rate is possible per payment, therfore if the payment period 
spans over a tax change a warning message should not allow the payment to 
be created' 
*/
INTEGER li_count_1
STRING  ls_test

//set the effective tax rate to 0
adc_rate = 0

SELECT count(*) INTO :li_count_1 FROM Tax_Rate 
 WHERE effective_date <= :ad_paid_from
   AND active_flag = "Y"
 USING SQLCA;
 SQLCA.nf_handle_error("SELECT count(*) INTO :li_count_1","n_tax_functions","nf_check_valid_from_date()") 
 
IF ISNULL(li_count_1) THEN li_count_1 = 0
IF li_count_1 = 0 THEN RETURN 0

RETURN 1






end function

public function integer nf_get_valid_tax_rate (date ad_paid_from, date ad_paid_to, ref decimal adc_rate);/*
This function will grab the valid tax rate for a given paid from and to date
'Only one tax rate is possible per payment, therfore if the payment period 
spans over a tax change a warning message should not allow the payment to 
be created' 
*/
INTEGER li_count_1, li_count_2
STRING  ls_test

//set the effective tax rate to 0
adc_rate = 0

IF ad_paid_from >= Date('1997-04-01') THEN
	
	SELECT	tax_rate
	INTO		:adc_rate
	FROM		Tax_Rate 
	WHERE	effective_date = (	SELECT	max(effective_date)
										FROM		Tax_Rate 
										WHERE 	effective_date <= :ad_paid_to
										AND		active_flag = 'Y')
	 USING SQLCA;
			 
	SQLCA.nf_handle_error("SELECT tax_rate...","n_tax_functions","nf_get_valid_tax_rate()") 
	
	IF ISNULL(adc_rate) THEN adc_rate = 0
	
	IF adc_rate <= 0 THEN
		//We have no Tax rate - I would assume we message out as there is no effective tax rate
	//	messagebox("Invalid Tax Rate","This Payment does not have an effective Tax rate. Please Correct.")
		RETURN -1
	END IF
END IF

RETURN 1






end function

public function integer nf_get_max_tax_amount (datetime adtm_paid_from, datetime adtm_paid_to, decimal adc_payment_amount, ref decimal adc_max_tax_allowed); /* If the Tax amount id greater than 0 it must be less than or equal to 
    the maximum tax amount permitted
    If PAYMENT.tax_amount >= 0: THEN PAYMENT.tax_amount must be <=
    (PAYMENT.total_payment_amount * TAX_RATE.tax_rate) / 1 + TAX_RATE.tax_rate)
	 
	 adtm_paid_from: paid from date on the payment used to get effective
	 tax rate
	 adtm_paid_to: paid to date on the payment used to get effective tax rate
	 adc_payment_amount: The amount of the payment
	 adc_max_tax_allowed: passed back by reference - the maximum tax allowed
	 
	 returns max tax value by reference and -1 if it fails
	 
*/
DECIMAL ldec_tax_rate
INTEGER li_error

//call the function to get the tax rate
li_error = nf_get_valid_tax_rate(date(adtm_paid_from),date(adtm_paid_to),ldec_tax_rate)

IF li_error = -1 OR ISNULL(ldec_tax_rate) THEN RETURN -1
	
adc_max_tax_allowed = round(((adc_payment_amount * ldec_tax_rate) / (1 + ldec_tax_rate)),2)

IF ISNULL(adc_max_tax_allowed) THEN
	//messagebox out
	adc_max_tax_allowed = 0
	RETURN -1
END IF 

RETURN 1 

end function

public subroutine nf_get_max_tax_amount (decimal adec_txn_amount, decimal adec_adjustment_amount, decimal adec_tax_rate, ref decimal adec_max_tax_amount);
adec_max_tax_amount = (adec_txn_amount * adec_tax_rate ) /  (1 + adec_tax_rate)


end subroutine

public subroutine nf_get_max_adj_tax_amount (decimal adec_adjustment_amount, decimal adec_tax_rate, ref decimal adec_max_adjustment_amount);adec_max_adjustment_amount = ((adec_adjustment_amount * -1) * adec_tax_rate) / (1 + adec_tax_rate)

adec_max_adjustment_amount = ABS(adec_max_adjustment_amount)
end subroutine

public subroutine nf_get_min_adj_tax_amount (decimal adec_tax_amount, decimal adec_txn_amount, decimal adec_txn_adjustment_amount, decimal adec_tax_rate, ref decimal adec_min_adjustment_amount);
adec_min_adjustment_amount = adec_tax_amount - (((adec_txn_amount + adec_txn_adjustment_amount) * adec_tax_rate) / ( 1 + adec_tax_rate ))

if adec_min_adjustment_amount < 0 then
	adec_min_adjustment_amount = 0
else
	adec_min_adjustment_amount = ABS(adec_min_adjustment_amount)
end if


end subroutine

on n_tax_functions.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_tax_functions.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

