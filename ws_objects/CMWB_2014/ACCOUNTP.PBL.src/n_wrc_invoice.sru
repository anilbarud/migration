$PBExportHeader$n_wrc_invoice.sru
forward
global type n_wrc_invoice from nonvisualobject
end type
end forward

global type n_wrc_invoice from nonvisualobject
end type
global n_wrc_invoice n_wrc_invoice

forward prototypes
public function long nf_get_invoice_number (long al_docid)
public function decimal nf_get_balance_owing (long al_wrc_invoice_no)
public function long nf_get_service_provider_no ()
public function decimal nf_get_invoice_amount (long al_wrc_invoice_no)
end prototypes

public function long nf_get_invoice_number (long al_docid);//*******************************************************************
//RETURN 		0 (The docid is not associated with a wrc_invoice)
//					wrc_invoice_no
//*******************************************************************

Long			ll_wrc_invoice_no

SELECT wrc_invoice_no INTO :ll_wrc_invoice_no
FROM WRC_INVOICE_HEADER
WHERE docid = :al_docid
USING SQLCA;

SQLCA.nf_handle_error('n_wrc_invocie','nf_get_invoice_number','SELECT wrc_invoice_no INTO :ll_wrc_invoice_no')

RETURN ll_wrc_invoice_no
end function

public function decimal nf_get_balance_owing (long al_wrc_invoice_no);// nf_get_balance_owing - Returns balance owing on a WRC Invoice.  
// 
Integer    li_rtn 
Decimal{2} ld_balance_owing, ld_sum_txns 

SELECT ISNULL(outstanding_balance, 0.00) 
  INTO :ld_balance_owing 
  FROM WRC_INVOICE_HEADER 
 WHERE wrc_invoice_no = :al_wrc_invoice_no 
 USING SQLCA ; 

li_rtn = SQLCA.nf_handle_error("n_wrc_invoice", "", "nf_get_balance_owing - SELECT outstanding_balance FROM WRC_INVOICE_HEADER WHERE wrc_invoice_no = :al_wrc_invoice_no")

// Check UNAPPLIED_CLAIM_TXN to see if there are any txns waiting to be applied against the invoice.
SELECT SUM(ISNULL(UCT.txn_amount, 0.00)) 
  INTO :ld_sum_txns 
  FROM            WRC_INVOICE_HEADER WIH  
            INNER JOIN PAYMENT_DOCUMENT PD     ON WIH.docid = PD.doc_id
       LEFT OUTER JOIN UNAPPLIED_CLAIM_TXN UCT ON PD.payment_no = UCT.payment_no
 WHERE WIH.wrc_invoice_no = :al_wrc_invoice_no
 USING SQLCA ; 

li_rtn = SQLCA.nf_handle_error("n_wrc_invoice", "", "nf_get_balance_owing - SELECT SUM(ISNULL(UCT.txn_amount, 0.00)) FROM WRC_INVOICE_HEADER, PAYMENT_DOCUMENT, UNAPPLIED_CLAIM_TXN ... WHERE WIH.wrc_invoice_no = :al_wrc_invoice_no")

IF IsNull(ld_sum_txns) = TRUE THEN
	ld_sum_txns = 0.00 
END IF

ld_balance_owing = ld_balance_owing - ld_sum_txns 

RETURN ld_balance_owing 

end function

public function long nf_get_service_provider_no ();LONG		ll_service_provider_no

SELECT service_provider_no into :ll_service_provider_no
FROM App_Document_Index_Parameter
WHERE app_function_code = 'WRC_INV'
USING SQLCA;

SQLCA.nf_handle_error("n_wrc_invoice","nf_service_provider_no","select service_provider_no")

return ll_service_provider_no
end function

public function decimal nf_get_invoice_amount (long al_wrc_invoice_no);// nf_get_invoice_amount 
// 
Integer    li_rtn 
Decimal{2} ld_wrc_invoice_amount

SELECT total_invoice_amount 
  INTO :ld_wrc_invoice_amount 
  FROM WRC_INVOICE_HEADER  
 WHERE wrc_invoice_no = :al_wrc_invoice_no 
 USING SQLCA ; 

li_rtn = SQLCA.nf_handle_error("n_wrc_invoice", "", "nf_get_invoice_amount - SELECT total_invoice_amount FROM WRC_INVOICE_HEADER WHERE wrc_invoice_no = :al_wrc_invoice_no")

IF SQLCA.SQLNRows = 0 THEN ld_wrc_invoice_amount = -1

RETURN ld_wrc_invoice_amount

end function

on n_wrc_invoice.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_wrc_invoice.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

