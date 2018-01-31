$PBExportHeader$n_reissue.sru
forward
global type n_reissue from nonvisualobject
end type
end forward

global type n_reissue from nonvisualobject
end type
global n_reissue n_reissue

type variables
n_transaction itr_trans_object
DATAWINDOW idw_dw[]
w_ancestor iwi_window_parent
LONG il_row1, il_row2, il_row3, il_row4
LONG il_reissued_txn[]

end variables

forward prototypes
public function integer nf_save ()
public function integer nf_check_bus_rules ()
public function integer nf_update ()
public function long nf_get_max_seq_no (long al_txn_no)
public function integer nf_reissue (string as_type)
public function integer nf_set_defaults ()
public function integer nf_get_benefit_class (long al_payment_no, ref string as_benefit_class)
public function integer nf_rollback ()
public subroutine nf_set_window_parent (w_ancestor awi_window_parent)
public function integer nf_insert (string as_type)
public function integer nf_is_reconciled (long al_cheque_no)
public subroutine nf_set_datawindow (datawindow adw_dw[], n_transaction anv_transobj)
end prototypes

public function integer nf_save ();//Check Business Rules
IF nf_check_bus_rules() < 0 THEN RETURN -1
//Update datawindows
IF nf_update() < 0 THEN	RETURN -1


RETURN 0
end function

public function integer nf_check_bus_rules ();DATE ldt_null
LONG ll_cntr, ll_check, ll_checked, ll_upper, ll_cntr2, ll_reset[]
STRING ls_type, ls_reissued_txns

SetNull(ldt_null)

//BR 1.00
FOR ll_cntr = 1 to idw_dw[1].RowCount()
	ll_check = idw_dw[1].GetItemNumber(ll_cntr,'checkbox_group')
	
	IF ll_check = 0 THEN
		IF ll_cntr = idw_dw[1].RowCount() AND ll_checked = 0 THEN
			MessageBox('No Chq/Deposit Selected','Please select a transaction to Re-Issue', Information!)
			RETURN -1
		END IF
		CONTINUE
	END IF
	
	ll_checked++
NEXT

//BR 1.30
ll_upper = Upperbound(il_reissued_txn[])
IF ll_upper > 0 THEN
	FOR ll_cntr2 = 1 to ll_upper
		IF ll_cntr2 = ll_upper THEN
			ls_reissued_txns = ls_reissued_txns + STRING(il_reissued_txn[ll_cntr2])
		ELSE
			ls_reissued_txns = ls_reissued_txns + STRING(il_reissued_txn[ll_cntr2]) + '~r~n'
		END IF
	NEXT	
	IF MessageBox('Warning','The following transaction(s) has/have previously been Re-Issued: ~r~n~r~n' + ls_reissued_txns + '~r~n~r~nWould you like to Continue?', Question!, YesNo!) = 2 THEN
		il_reissued_txn[] = ll_reset[]
		RETURN -1
	END IF
END IF

RETURN  0 
end function

public function integer nf_update ();LONG ll_error, ll_cntr = 1, ll_upper

ll_upper = UpperBound(idw_dw)
DO WHILE ll_cntr <= ll_upper
	idw_dw[ll_cntr].Update()
	ll_error = itr_trans_object.nf_handle_error("n_reissue","nf_update","Updating idw_dw[" + STRING(ll_cntr) + "]")
	IF ll_error < 0 THEN
		RETURN ll_error
	END IF
	ll_cntr ++
LOOP

RETURN 0
end function

public function long nf_get_max_seq_no (long al_txn_no);LONG ll_seq_no

SELECT MAX(seq_no)
INTO       :ll_seq_no
FROM     CHEQUE_DEPOSIT_REPLACEMENT (TABLOCKX)
WHERE  txn_no = :al_txn_no
USING    itr_trans_object;

itr_trans_object.nf_handle_error('n_reissue','nf_get_max_seq_no','SELECT MAX(seq_no)')

IF IsNull(ll_seq_no) THEN
	ll_seq_no = 0
END IF

RETURN ll_seq_no
end function

public function integer nf_reissue (string as_type);LONG ll_cntr, ll_check, ll_checked, ll_chq_no, ll_xmit_no, ll_old_dd_detail_no, ll_recipient_no
LONG ll_claim_no, ll_txn_no, ll_payment_no, ll_seq_no, ll_row, ll_row2, ll_row3, ll_row4, ll_upper, ll_reset[]
DATE ldt_deposit_date, ldt_null
STRING ls_recipient_type, ls_benefit_class

SetNull(ldt_null)

il_reissued_txn[] = ll_reset[]

FOR ll_cntr = 1 to idw_dw[1].RowCount()
	ll_seq_no = 0
	ll_check = idw_dw[1].GetItemNumber(ll_cntr,'checkbox_group')
	
	IF ll_check = 0 THEN
		CONTINUE
	END IF
	
	IF as_type = 'Cheque' THEN
		ll_chq_no = idw_dw[1].GetItemNumber(ll_cntr, 'cheque_no')
		ll_xmit_no = 0
		ll_old_dd_detail_no = 0
		idw_dw[1].SetItem(ll_cntr, 'cheque_no', 0)
	ELSE
		ll_xmit_no = idw_dw[1].GetItemNumber(ll_cntr, 'direct_deposit_xmit_no')
		ll_chq_no = 0
		ll_old_dd_detail_no = idw_dw[1].GetItemNumber(ll_cntr,'direct_deposit_detail_no')
		IF IsNull(ll_old_dd_detail_no) THEN ll_old_dd_detail_no = 0
		idw_dw[1].SetItem(ll_cntr, 'direct_deposit_xmit_no', 0)
	END IF
	ll_recipient_no = idw_dw[1].GetItemNumber(ll_cntr,'recipient_no')
	ls_recipient_type = idw_dw[1].GetItemString(ll_cntr,'recipient_type_code')
	
	ldt_deposit_date = DATE(idw_dw[1].GetItemDateTime(ll_cntr,'cheque_deposit_date'))
	idw_dw[1].Setitem(ll_cntr, 'cheque_deposit_date', ldt_null)
	
	ll_txn_no = idw_dw[1].GetItemNumber(ll_cntr, 'txn_no')
	ll_payment_no = idw_dw[1].GetItemNumber(ll_cntr, 'payment_no')
	ll_claim_no = idw_dw[1].GetItemNumber(ll_cntr, 'claim_no')

	ll_seq_no = nf_get_max_seq_no(ll_txn_no)
	
	IF ll_seq_no > 0 THEN
		ll_upper = UpperBound(il_reissued_txn[]) + 1
		il_reissued_txn[ll_upper] = ll_txn_no
	END IF
	
	ll_seq_no = ll_seq_no + 1
	
	//Insert rows in to CHEQUE_DEPOSIT_REPLACEMENT, DAILY_CHEQUE or DAILY_DIRECT_DEPOSIT
	ll_row = nf_insert(as_type)

	IF ll_row < 0 THEN
		MessageBox('Error','An error occurred while inserting records.', Information!)
		RETURN -1
	END IF
	
	ll_row2 = il_row2
	il_row2 = 0
	//Set values in CHEQUE_DEPOSIT_REPLACEMENT
	IF ll_row2 > 0 THEN
		idw_dw[2].SetItem(ll_row2,'txn_no',ll_txn_no)
		idw_dw[2].SetItem(ll_row2,'seq_no',ll_seq_no)
		idw_dw[2].SetItem(ll_row2,'payment_no',ll_payment_no)
		idw_dw[2].SetItem(ll_row2,'claim_no',ll_claim_no)
		idw_dw[2].SetItem(ll_row2,'old_cheque_no', ll_chq_no)
		idw_dw[2].SetItem(ll_row2,'old_cheque_deposit_date', ldt_deposit_date)
		idw_dw[2].SetItem(ll_row2,'new_cheque_deposit_date',ldt_null )	
		idw_dw[2].SetItem(ll_row2,'old_direct_deposit_xmit_no', ll_xmit_no)
		idw_dw[2].SetItem(ll_row2,'old_direct_deposit_detail_no', ll_old_dd_detail_no)
	END IF
	
	IF nf_get_benefit_class(ll_payment_no, ls_benefit_class) < 0 THEN
		MessageBox('Error','An error occurred trying to obtain the benefit class code for the payment.', Information!)
		RETURN -1
	END IF

	ll_row3 = il_row3
	il_row3 = 0
	//Set values in DAILY_CHEQUE
	IF ll_row3 > 0 THEN
		idw_dw[3].SetItem(ll_row3, 'txn_no', ll_txn_no)
		idw_dw[3].SetItem(ll_row3, 'benefit_class_code', ls_benefit_class)
	END IF
	
	ll_row4 = il_row4
	il_row4 = 0
	//Set values in DAILY_DIRECT_DEPOSIT
	IF ll_row4 > 0 THEN
		idw_dw[4].SetItem(ll_row4, 'txn_no', ll_txn_no)
		idw_dw[4].SetItem(ll_row4, 'benefit_class_code', ls_benefit_class)
	END IF
	
NEXT

RETURN  0 
end function

public function integer nf_set_defaults ();DATE ldt_null

SetNull(ldt_null)

idw_dw[2].SetItem(il_row2,'txn_no',0)
idw_dw[2].SetItem(il_row2,'seq_no',0)
idw_dw[2].SetItem(il_row2,'payment_no',0)
idw_dw[2].SetItem(il_row2,'claim_no',0)
idw_dw[2].SetItem(il_row2,'old_cheque_no', 0)
idw_dw[2].SetItem(il_row2,'new_cheque_no', 0)
idw_dw[2].SetItem(il_row2,'new_cheque_deposit_date',ldt_null )	
idw_dw[2].SetItem(il_row2,'old_direct_deposit_xmit_no', 0)
idw_dw[2].SetItem(il_row2,'new_direct_deposit_xmit_no', 0)
idw_dw[2].SetItem(il_row2,'old_direct_deposit_detail_no', 0)
idw_dw[2].SetItem(il_row2,'new_direct_deposit_detail_no', 0)

IF idw_dw[3].GetRow() > 0 THEN
	idw_dw[3].SetItem(il_row3, 'txn_no', 0)
	idw_dw[3].SetItem(il_row3, 'benefit_class_code', '')
END IF

IF idw_dw[4].GetRow() > 0 THEN
	idw_dw[4].SetItem(il_row4, 'txn_no', 0)
	idw_dw[4].SetItem(il_row4, 'benefit_class_code', '')
END IF

RETURN 0


end function

public function integer nf_get_benefit_class (long al_payment_no, ref string as_benefit_class);
SELECT  benefit_class_code
INTO        :as_benefit_class
FROM      PAYMENT a, Payment_Type b
WHERE  a.payment_type_code  = b.payment_type_code
AND         a.payment_no                = :al_payment_no
USING     itr_trans_object;

itr_trans_object.nf_handle_error('n_reissue','nf_get_benefit_class','SELECT benefit_class_code')

IF IsNull(as_benefit_class) THEN
	RETURN -1
END IF

RETURN 0
end function

public function integer nf_rollback ();
itr_trans_object.nf_rollback_transaction()

RETURN 0
end function

public subroutine nf_set_window_parent (w_ancestor awi_window_parent);
iwi_window_parent = awi_window_parent
end subroutine

public function integer nf_insert (string as_type);LONG ll_loop, ll_rtn

ll_loop = 2

DO UNTIL ll_loop = UpperBound(idw_dw[]) +1
	IF as_type = 'Direct Deposit'  AND ll_loop = 3 THEN ll_loop = 4
	IF as_type = 'Cheque' AND ll_loop = 4 THEN 
		ll_loop = ll_loop + 1
		CONTINUE
	END IF
	ll_rtn = idw_dw[ll_loop].InsertRow(0) 
	IF ll_rtn < 0 THEN 
		RETURN -1
	ELSE
		CHOOSE CASE ll_loop
			CASE 2
				il_row2 = ll_rtn
			CASE 3
				il_row3 = ll_rtn
			CASE 4
				il_row4 = ll_rtn
		END CHOOSE
	END IF
   ll_loop = ll_loop + 1
LOOP

IF nf_set_defaults() < 0 THEN Return -1

RETURN 0
end function

public function integer nf_is_reconciled (long al_cheque_no);STRING ls_reconciled

SELECT reconciled_code
INTO      :ls_reconciled
FROM    CHEQUE_HEADER
WHERE cheque_no = :al_cheque_no
USING   SQLCA;

SQLCA.nf_handle_error('n_reissue','nf_is_reconciled','SELECT reconciled_code')

IF TRIM(ls_reconciled) > '' THEN
	RETURN -1
END IF

RETURN 0
end function

public subroutine nf_set_datawindow (datawindow adw_dw[], n_transaction anv_transobj);INTEGER li_cntr = 1

DO WHILE li_cntr <=	UpperBound(adw_dw)
	idw_dw[li_cntr]	=	adw_dw[li_cntr]
	idw_dw[li_cntr].SetTransObject(anv_transobj)
	li_cntr ++
LOOP

itr_trans_object 	=	anv_transobj


end subroutine

on n_reissue.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_reissue.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

