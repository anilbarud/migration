$PBExportHeader$n_annuity_interest.sru
$PBExportComments$Non-Visual Object that contains functions to allow the posting of quartely interest.
forward
global type n_annuity_interest from n_pdc
end type
end forward

global type n_annuity_interest from n_pdc
end type
global n_annuity_interest n_annuity_interest

type variables
STRING is_receiving_salary_flag
LONG il_claim_no, il_list_row_count
U_DW_ONLINE idw_basic_claim
DECIMAL ic_authorization_limit
DATE idt_max_paid_to_date

end variables

forward prototypes
public function integer nf_retrieve_interest ()
public function integer nf_insert (long al_row)
public function integer nf_retrieve ()
public function integer nf_retrieve_annuity_interest ()
public function integer nf_change_item (long al_datawindow)
end prototypes

public function integer nf_retrieve_interest ();LONG   ll_row
	
ll_row = idw_dw[2].Retrieve()

Return ll_row






end function

public function integer nf_insert (long al_row);LONG ll_row

ll_row = idw_dw[1].InsertRow(0)

Return ll_row
end function

public function integer nf_retrieve ();LONG		ll_rows

ll_rows = idw_dw[1].Retrieve()

return ll_rows
end function

public function integer nf_retrieve_annuity_interest ();DATAWINDOWCHILD  ldwc_child
LONG             ll_row
	
	
idw_dw[1].GetChild('year', ldwc_child)
	
ldwc_child.SetTransObject(SQLCA)

ll_row =  ldwc_child.Retrieve()

Return ll_row













end function

public function integer nf_change_item (long al_datawindow);//messagebox("nf_change_item","test")

return 1
end function

on n_annuity_interest.create
call super::create
end on

on n_annuity_interest.destroy
call super::destroy
end on

