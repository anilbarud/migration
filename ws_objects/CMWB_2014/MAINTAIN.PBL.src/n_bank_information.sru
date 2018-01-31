$PBExportHeader$n_bank_information.sru
forward
global type n_bank_information from nonvisualobject
end type
end forward

global type n_bank_information from nonvisualobject
end type
global n_bank_information n_bank_information

type variables
DATAWINDOW idw_dw
LONG il_recipient_no
STRING is_recipient_type_code, is_recipient_sub_type_code
end variables

forward prototypes
public subroutine nf_init (datawindow adw_dw)
public function integer nf_save ()
public function integer nf_check_bus_rule ()
public function integer nf_set_recipient_info (long al_recipient_no, string as_recipient_type_code, string as_recipient_sub_type_code)
public function integer nf_retrieve ()
public function integer nf_set_defaults ()
public function integer nf_insert ()
public function long nf_get_next_identifier ()
public function long nf_get_next_bank_info_no ()
public function integer nf_set_identifier ()
public function integer nf_update ()
public function integer nf_delete (long al_row)
public function integer nf_check_payments (long al_recipient_no, string as_recipient_type_code)
end prototypes

public subroutine nf_init (datawindow adw_dw);idw_dw = adw_dw
end subroutine

public function integer nf_save ();

IF nf_check_bus_rule() < 0 THEN
	RETURN -1
END IF

IF nf_set_identifier() < 0 THEN
	RETURN -1
END IF

IF nf_update() < 0 THEN
	RETURN -1
END IF

RETURN 0
end function

public function integer nf_check_bus_rule ();STRING ls_bank_no, ls_bank_account_no, ls_bank_transit_no
STRING ls_old_bank_no, ls_old_bank_account_no, ls_old_bank_transit_no
LONG ll_row

idw_dw.AcceptText()

ll_row = idw_dw.GetRow()

IF ll_row > 0 THEN
	ls_bank_no = idw_dw.GetItemString(ll_row, 'bank_no')
	ls_bank_account_no = TRIM(idw_dw.GetItemString(ll_row,'bank_account_no'))
	ls_bank_transit_no = idw_dw.GetItemString(ll_row, 'bank_transit_no')
END IF

IF (IsNull(ls_bank_no) OR ls_bank_no = '' OR LONG(ls_bank_no) = 0) AND (IsNull(ls_bank_transit_no) OR ls_bank_transit_no = ''  OR LONG(ls_bank_transit_no) = 0) AND (IsNull(ls_bank_account_no) OR ls_bank_account_no = '' OR LONG(ls_bank_account_no) = 0) THEN
	MessageBox('Missing Information','You must enter the Bank Information prior to saving a record.', Information!)
	RETURN -1
END IF

/*Bank number must be entered if other info is entered.*/
IF IsNull(ls_bank_no) OR ls_bank_no = '' OR LONG(ls_bank_no) = 0 THEN
	IF ls_bank_account_no > '' OR ls_bank_transit_no > '' THEN
		MessageBox('Bank Number','If any of the bank information is entered, then all of the bank information must be entered. Please enter the Bank Number.', Information!)
		RETURN -1
	END IF
ELSE
	/*Bank number must be 3 digits*/
	IF LEN(TRIM(ls_bank_no)) <> 3 THEN
		MessageBox('Bank Number','The Bank Number must be 3 digits in length. Please re-enter.', Information!)
		RETURN -1
	END IF
END IF

/*Bank transit number must be entered if other info is entered.*/
IF IsNull(ls_bank_transit_no) OR ls_bank_transit_no = ''  OR LONG(ls_bank_transit_no) = 0 THEN
	IF ls_bank_account_no > '' OR ls_bank_no > '' THEN
		MessageBox('Bank Transit Number','If any of the bank information is entered, then all of the bank information must be entered. Please enter the Bank Transit Number.', Information!)
		RETURN -1
	END IF
ELSE
	/*Bank transit number must be 5 digits*/
	IF LEN(TRIM(ls_bank_transit_no)) <> 5 THEN
		MessageBox('Bank Transit Number','The Bank Transit Number must be 5 digits in length. Please re-enter.', Information!)
		RETURN -1
	END IF
END IF	

/*Bank account number must be entered if other info is entered.*/
IF IsNull(ls_bank_account_no) OR TRIM(ls_bank_account_no) = '' THEN
	IF ls_bank_no > '' OR ls_bank_transit_no > '' THEN
		MessageBox('Bank Account Number','If any of the bank information is entered, then all of the bank information must be entered. Please enter the Bank Account Number.', Information!)
		RETURN -1
	END IF
ELSE
	/*Bank account number must be atleast 2 digits.*/
	IF LEN(TRIM(ls_bank_account_no)) < 2 THEN
		MessageBox('Bank Account Number','The Bank Account Number must be atleast 2 digits. Please re-enter.', Information!)
		RETURN -1
	END IF
END IF


RETURN 0
end function

public function integer nf_set_recipient_info (long al_recipient_no, string as_recipient_type_code, string as_recipient_sub_type_code);il_recipient_no = al_recipient_no
is_recipient_type_code = as_recipient_type_code
is_recipient_sub_type_code = as_recipient_sub_type_code

RETURN 0
end function

public function integer nf_retrieve ();
RETURN idw_dw.Retrieve(il_recipient_no, is_recipient_type_code, is_recipient_sub_type_code)
end function

public function integer nf_set_defaults ();
idw_dw.SetItem(idw_dw.GetRow(), 'bank_info_purpose_code', '001')
idw_dw.SetItem(idw_dw.GetRow(), 'recipient_no', il_recipient_no)
idw_dw.SetItem(idw_dw.GetRow(), 'recipient_type_code', is_recipient_type_code)
idw_dw.SetItem(idw_dw.GetRow(), 'recipient_sub_type_code',is_recipient_sub_type_code)
idw_dw.SetItem(idw_dw.GetRow(), 'bank_no','')
idw_dw.SetItem(idw_dw.GetRow(), 'bank_transit_no','')
idw_dw.SetItem(idw_dw.GetRow(), 'bank_account_no','')

RETURN 0
end function

public function integer nf_insert ();LONG ll_row, ll_getrow

ll_getrow = idw_dw.GetRow()

IF ll_getrow = 0 THEN
	
	ll_row = idw_dw.Insertrow(0)
	
	IF ll_row > 0 THEN
		idw_dw.ScrollToRow(ll_row)
	END IF
	
	IF nf_set_defaults() <> 0 THEN
		MessageBox('Problem','There was a problem setting the default values in the bank information table for the selected provider.', Information!)
		RETURN -1
	END IF
END IF

RETURN 0
end function

public function long nf_get_next_identifier ();LONG  ll_bank_info_no, ll_row

ll_row = idw_dw.GetRow()

IF ll_row > 0 THEN
	IF idw_dw.GetItemStatus(ll_row,0,Primary!) = NewModified! THEN 
		ll_bank_info_no = nf_get_next_bank_info_no()
	ELSE
		ll_bank_info_no = idw_dw.GetItemNumber(ll_row,'bank_info_no')
	END IF
ELSE
	RETURN 0
END IF

RETURN ll_bank_info_no

end function

public function long nf_get_next_bank_info_no ();LONG ll_bank_info_no, ll_error

SQLCA.nf_begin_transaction()

UPDATE Last_Bank_Info_No
SET	       last_bank_info_no = last_bank_info_no +1
USING    SQLCA;

ll_error = SQLCA.nf_handle_error("Embedded SQL","n_bank_information","nf_get_next_bank_info_no")

SELECT last_bank_info_no
INTO       :ll_bank_info_no
FROM      Last_Bank_Info_No
USING    SQLCA;

ll_error = SQLCA.nf_handle_error("Embedded SQL","n_bank_information","nf_get_next_bank_info_no")

IF ll_error < 0 THEN
	SQLCA.nf_rollback_transaction()
	RETURN - 1
ELSE
	SQLCA.nf_commit_transaction()
END IF

RETURN ll_bank_info_no
end function

public function integer nf_set_identifier ();LONG ll_next_bank_info_no

ll_next_bank_info_no = nf_get_next_identifier()

IF ll_next_bank_info_no <= 0 THEN
	MessageBox('Error','An error was encountered while trying to determing the Bank Information Number.', Information!)
	RETURN -1
END IF

idw_dw.SetItem(idw_dw.GetRow(), 'bank_info_no',ll_next_bank_info_no)

RETURN 0
end function

public function integer nf_update ();LONG ll_error

idw_dw.Update() 

ll_error = SQLCA.nf_handle_error('n_bank_information','nf_update','idw_dw.Update()')
IF ll_error <> 0 THEN
	RETURN -1
END IF

RETURN 0
end function

public function integer nf_delete (long al_row);LONG ll_return

/*Bank information must not be removed if there are scheduled payments or awards for the recipient and the payment method is direct deposit*/
IF nf_check_payments(il_recipient_no, is_recipient_type_code) = 0 THEN
	idw_dw.DeleteRow(al_row)
	
	IF SQLCA.nf_handle_error('n_bank_information','nf_delete','idw_dw.DeleteRow(al_row)') < 0 THEN
		RETURN -1
	END IF
	
	idw_dw.Update()
	IF SQLCA.nf_handle_error('n_bank_information','nf_delete','idw_dw.Update()') < 0 THEN
		RETURN -1
	END IF
	
END IF

RETURN 0



end function

public function integer nf_check_payments (long al_recipient_no, string as_recipient_type_code);LONG		ll_count, ll_return, ll_cntr, ll_claim, ll_claim_temp
DATETIME	ldtm_today
STRING	ls_award_type, ls_temp, ls_awards
DATASTORE lds_awards

ldtm_today = f_server_datetime()

IF IsNull(al_recipient_no) OR as_recipient_type_code = '' THEN
	MessageBox('Recipient Information','There was a problem determining the recipient information', Information!)
	RETURN -1
END IF

lds_awards = CREATE DATASTORE
lds_awards.DataObject = 'd_award_types_all_recipients'
lds_awards.SetTransObject(SQLCA)


SELECT Count(*)
INTO       :ll_count
FROM     UNAPPLIED_CLAIM_TXN
WHERE  recipient_no = :al_recipient_no
AND         recipient_type_code = :as_recipient_type_code
AND         payment_method_code = 'D'
USING    SQLCA;

IF SQLCA.nf_handle_error('Embedded SQL: select from UNAPPLIED_CLAIM_TXN','n_bank_information','nf_check_payments') < 0 THEN
	RETURN -1
END IF

IF ll_count > 0 THEN
	MessageBox('Scheduled Payments Exist','Scheduled direct deposit PAYMENTS exist for this recipient.  The banking information~r~n cannot be removed until the payment method has been changed to Automated Cheque.')
	RETURN -1
END IF

ll_count = lds_awards.Retrieve(al_recipient_no, as_recipient_type_code)

IF ll_count > 0 THEN
	MessageBox('Scheduled Awards Exist','Scheduled direct deposit AWARDS exist for this recipient.  The banking information ~r~ncannot be removed until the payment method has been changed.~r~n~r~n' &
	+ 'To replace the payment method for Awards, terminate~r~nthe award and enter a new Award record for the remaining period with a payment~r~nmethod of Automated Cheque.' &
	+ '~r~n~r~n' + ls_awards)
	RETURN -1
END IF

RETURN 0
end function

on n_bank_information.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_bank_information.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

