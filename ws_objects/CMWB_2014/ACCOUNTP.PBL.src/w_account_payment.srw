$PBExportHeader$w_account_payment.srw
$PBExportComments$Account Payments - Main window used to display accounts and related options
forward
global type w_account_payment from w_a_tool
end type
type dw_select_working_set from u_dw_online within w_account_payment
end type
type dw_select_taxing_category from u_dw_online within w_account_payment
end type
type dw_retrieve_ref_entries from u_dw_online within w_account_payment
end type
type cb_send from commandbutton within w_account_payment
end type
type cb_delete from commandbutton within w_account_payment
end type
type cb_view from commandbutton within w_account_payment
end type
type cb_remove from commandbutton within w_account_payment
end type
type cb_link from commandbutton within w_account_payment
end type
type dw_enter_claim_no from u_dw_online within w_account_payment
end type
type st_1 from statictext within w_account_payment
end type
type cbx_imaged_view from checkbox within w_account_payment
end type
type cbx_remove from checkbox within w_account_payment
end type
type cb_refresh from commandbutton within w_account_payment
end type
type dw_document_path from u_dw_document_path within w_account_payment
end type
type st_advance_notice from statictext within w_account_payment
end type
type st_overpayment from statictext within w_account_payment
end type
type uo_image_append from u_image_append within w_account_payment
end type
type dw_claimsmaster_ref_count from u_dw_online within w_account_payment
end type
type cb_add from commandbutton within w_account_payment
end type
type dw_account_list from u_dw_online within w_account_payment
end type
type gb_account_list from groupbox within w_account_payment
end type
type dw_account_payment_list from u_dw_online within w_account_payment
end type
type cb_reject from commandbutton within w_account_payment
end type
type cb_authorization from commandbutton within w_account_payment
end type
type cb_advance from commandbutton within w_account_payment
end type
type gb_noaccount_payment_list from groupbox within w_account_payment
end type
type dw_account_type_payments_list from u_dw_online within w_account_payment
end type
type dw_display_claim_info from u_dw_online within w_account_payment
end type
type cb_pay_ephysio from commandbutton within w_account_payment
end type
type cb_delete_ephysio from commandbutton within w_account_payment
end type
type gb_account_payment_list from groupbox within w_account_payment
end type
end forward

global type w_account_payment from w_a_tool
boolean resizable = false
event ue_postopen pbm_custom01
event ue_childactivate pbm_custom02
dw_select_working_set dw_select_working_set
dw_select_taxing_category dw_select_taxing_category
dw_retrieve_ref_entries dw_retrieve_ref_entries
cb_send cb_send
cb_delete cb_delete
cb_view cb_view
cb_remove cb_remove
cb_link cb_link
dw_enter_claim_no dw_enter_claim_no
st_1 st_1
cbx_imaged_view cbx_imaged_view
cbx_remove cbx_remove
cb_refresh cb_refresh
dw_document_path dw_document_path
st_advance_notice st_advance_notice
st_overpayment st_overpayment
uo_image_append uo_image_append
dw_claimsmaster_ref_count dw_claimsmaster_ref_count
cb_add cb_add
dw_account_list dw_account_list
gb_account_list gb_account_list
dw_account_payment_list dw_account_payment_list
cb_reject cb_reject
cb_authorization cb_authorization
cb_advance cb_advance
gb_noaccount_payment_list gb_noaccount_payment_list
dw_account_type_payments_list dw_account_type_payments_list
dw_display_claim_info dw_display_claim_info
cb_pay_ephysio cb_pay_ephysio
cb_delete_ephysio cb_delete_ephysio
gb_account_payment_list gb_account_payment_list
end type
global w_account_payment w_account_payment

type variables
DATAWINDOWCHILD						idwc_taxing_categories, idwc_working_sets
S_WINDOW_MESSAGE 					istr_message
U_DW_DOCUMENT_PATH 				iu_dw_document_path
N_PAYMENT					 				inv_payment
w_inbasket            						iwi_parent_window
w_account_payment_maintenance iw_account_payment_maintenance
w_account_link_maintenance 		iw_account_link_maintenance
w_memotab                                iw_memotab
w_physio_reimbursements				 iw_physio_reimbursements
LONG			il_setid, il_catid, il_inbasket_catid, il_inbasket_claim_no
BOOLEAN 		ib_remove_after_payment = false,		ib_imaged_view, 	ib_authorizations_found = false
STRING			is_module_name,	is_sort_name = "by Document ID", 	is_filter_name = "All Documents"
STRING			is_category_select,  is_as_mode, is_imaged_flag = "Y",  is_call,  is_from_clause
DATETIME	 	idt_server_datetime
U_DS			ids_admin_act_document_restriction

w_sheet     iw_sheet
end variables

forward prototypes
public function boolean wf_isaccount (string as_type)
public subroutine wf_set_parent_window (window awi_parent_window)
public function string wf_validate_list_data ()
public function integer wf_validate_imaged ()
public function integer wf_validate_nonimaged ()
public subroutine wf_set_redraw (boolean ab_redraw, string as_call)
public subroutine wf_apply_doc_list_option (string as_option, long al_option_number, string as_option_name)
public subroutine wf_screen_reset (string as_screen_change)
public subroutine wf_open_maintenance ()
public function integer wf_retrieve_claim (long al_claim_no)
public function integer wf_retrieve_doclist ()
public function string wf_get_module_source_code (long ll_docid)
public function boolean wf_can_line_item_be_deleted (long al_payment_no, long al_docid)
end prototypes

on ue_postopen;call w_a_tool::ue_postopen;LONG	ll_nmbr_categories

//	Obtain a handle to the drop down datawindow and retrieve the list of pc taxing categories for the work areas

	dw_select_working_set.GetChild("setid",idwc_working_sets)
	idwc_working_sets.SetTransObject(ImageTrans)

	ll_nmbr_categories = idwc_working_sets.Retrieve()
	IF ImageTrans.nf_handle_error("vidwc_working_sets","w_account_payment","ue_postopen") < 0 THEN
		Close(this)
		Return
	END IF

//	Obtain a handle to the drop down datawindow and retrieve the list of pc taxing categories for the region

	dw_select_taxing_category.GetChild("catid",idwc_taxing_categories)
	idwc_taxing_categories.SetTransObject(ImageTrans)

	ll_nmbr_categories = idwc_taxing_categories.Retrieve(il_setid)
	IF ImageTrans.nf_handle_error("vidwc_taxing_categories","w_account_payment","ue_postopen") < 0 THEN
		Close(this)
		Return
	END IF


//	Set up the external datawindows so the user can enter/select from them

	dw_select_working_set.InsertRow(0)
	dw_select_taxing_category.InsertRow(0)
	dw_enter_claim_no.InsertRow(0)
	dw_display_claim_info.InsertRow(0)

// Set the working set to the users default work area
	dw_select_working_set.SetItem(1, "setid", il_setid)
end on

event ue_childactivate;//	Check to see if there were any child windows open, and if so
//	bring them to the front in the appropriate order

IF IsValid(iw_account_payment_maintenance) THEN
	iw_account_payment_maintenance.Show()
END IF

IF IsValid(iw_account_link_maintenance) THEN
	iw_account_link_maintenance.Show()
END IF

IF IsValid(iw_physio_reimbursements) THEN
	iw_physio_reimbursements.Show()
END IF
end event

public function boolean wf_isaccount (string as_type);Return (Left(as_type,1) = "A" OR as_type = "MPC" OR as_type = "MPD" OR as_type = "SDC" OR as_type = "SDD")

end function

public subroutine wf_set_parent_window (window awi_parent_window);iwi_parent_window = awi_parent_window
end subroutine

public function string wf_validate_list_data ();long ll_rtn
string ls_rtn
IF ib_imaged_view THEN
	ll_rtn = this.trigger function wf_validate_imaged()
	IF ll_rtn < 0 THEN 
		Choose Case ll_rtn 
			Case -1
				ls_rtn =  "Error retrieving claim information."
			Case -2
				ls_rtn =  "There has been an addtion to this document. Please refresh data."
			Case -3 
				ls_rtn =  "There has been a deletion to this document. Please refresh data."
			Case -4
				ls_rtn =  "There has been a change to this documents data. Please refresh data."
		End Choose
	End IF
ELSE
	ll_rtn = this.trigger function wf_validate_nonimaged()
	IF ll_rtn < 0 THEN 
		Choose Case ll_rtn 
			Case -1
				ls_rtn =  "Error retrieving claim information on non imaged Claim."
			Case -2
				ls_rtn =  "There has been an addtion to this claim. Please refresh data."
			Case -3 
				ls_rtn =  "There has been a deletion to this claim. Please refresh data."
			Case -4
				ls_rtn =  "There has been a change to this claim data. Please refresh data."
		End Choose		
	End IF
END IF

RETURN ls_rtn

end function

public function integer wf_validate_imaged ();long ll_docid
string ls_temp,ls_sort,ls_table,ls_col,ls_cols,ls_filter
datastore lds_data
u_dw_online ldw_data //Holds values from dw_account_payment_list
/*Variable names that start with lower case d are database values 
Variable names that start with lower case w are window values from w_account_payment
*/
long ll_dCount,ll_wCount,ll_ColCount,ll_RowCntr,ll_ColCntr
String ls_dwsyntax,ls_errors,ls_oldSyntax
ldw_data = dw_account_payment_list
ll_docid = dw_account_list.getitemnumber(dw_account_list.getrow(),'docid')
lds_data = create datastore
lds_data.dataobject = ldw_data.dataobject
lds_data.settransobject(SQLCA)
lds_data.retrieve(ll_docid)

ls_sort = ldw_data.Describe("DataWindow.Table.Sort")
lds_data.setSort(ls_sort)
lds_data.sort()

ldw_data.setSort(ls_sort)
ldw_data.sort()

//Compare rowcount
ll_wCount = ldw_data.rowcount() 
ll_dCount = lds_data.rowcount()
if ll_wCount = ll_dCount then
	ll_ColCount = long(lds_data.Object.DataWindow.Column.Count)
	FOR ll_RowCntr = 1 to ll_wCount
		FOR ll_ColCntr = 1 to ll_ColCount
			IF lds_data.Object.Data.Primary.Current[ll_RowCntr,ll_ColCntr] = ldw_data.Object.Data.Primary.Original[ll_RowCntr,ll_ColCntr] THEN
				//Records match do nothing
			ELSE
				IF isNull(lds_data.Object.Data.Primary.Current[ll_RowCntr,ll_ColCntr]) and isNull(ldw_data.Object.Data.Primary.Original[ll_RowCntr,ll_ColCntr]) THEN
				ELSE
					//Records do not match return  error
					return  -4
				END IF
			END IF
		NEXT
	NEXT
else
	if ll_wCount < ll_dCount then
		return  -2
	else
		return  -3
	end if	
end if
return  0
end function

public function integer wf_validate_nonimaged ();long ll_docid
string ls_temp,ls_sort,ls_table,ls_col,ls_cols,ls_filter
datastore lds_data
u_dw_online ldw_data //Holds values from dw_account_payment_list
/*Variable names that start with lower case d are database values 
Variable names that start with lower case w are window values from w_account_payment
*/
long ll_dCount,ll_wCount,ll_ColCount,ll_RowCntr,ll_ColCntr
String ls_dwsyntax,ls_errors,ls_oldSyntax
ldw_data = dw_account_type_payments_list
ll_docid = dw_enter_claim_no.getitemnumber(dw_enter_claim_no.getrow(),'claim_no')
lds_data = create datastore
lds_data.dataobject = ldw_data.dataobject
lds_data.settransobject(SQLCA)
lds_data.retrieve(ll_docid)

ls_sort = ldw_data.Describe("DataWindow.Table.Sort")
lds_data.setSort("")
lds_data.sort()
ldw_data.setSort("")
ldw_data.sort()

lds_data.setSort("payment_no DESC")
lds_data.sort()
ldw_data.setSort("payment_no DESC")
ldw_data.sort()

//Compare rowcount
ll_wCount = ldw_data.rowcount() 
ll_dCount = lds_data.rowcount()
if ll_wCount = ll_dCount then
	ll_ColCount = long(lds_data.Object.DataWindow.Column.Count)
	FOR ll_RowCntr = 1 to ll_wCount
		FOR ll_ColCntr = 1 to ll_ColCount
			IF lds_data.Object.Data.Primary.Current[ll_RowCntr,ll_ColCntr] = ldw_data.Object.Data.Primary.Original[ll_RowCntr,ll_ColCntr] THEN
				//Records match do nothing
			ELSE
				IF isNull(lds_data.Object.Data.Primary.Current[ll_RowCntr,ll_ColCntr]) and isNull(ldw_data.Object.Data.Primary.Original[ll_RowCntr,ll_ColCntr]) THEN
				ELSE
					//Records do not match return  error
					ldw_data.setSort(ls_sort)
					ldw_data.sort()
					return  -4
				END IF
			END IF
		NEXT
	NEXT
else
	if ll_wCount < ll_dCount then
		ldw_data.setSort(ls_sort)
		ldw_data.sort()
		return  -2
	else
		ldw_data.setSort(ls_sort)
		ldw_data.sort()
		return  -3
	end if	
end if
ldw_data.setSort(ls_sort)
ldw_data.sort()
return  0
end function

public subroutine wf_set_redraw (boolean ab_redraw, string as_call);IF as_call = is_call OR is_call = "" THEN
	THIS.setredraw(ab_redraw) 
	IF is_call = "" THEN  is_call = as_call
	
	IF ab_redraw = TRUE THEN 
		is_call = ""
	ELSE
	END IF
END IF

end subroutine

public subroutine wf_apply_doc_list_option (string as_option, long al_option_number, string as_option_name);LONG	ll_list_rownum, ll_claim_no


CHOOSE CASE as_option
CASE "filter"

	CHOOSE CASE al_option_number
		CASE	1
			dw_account_list.SetFilter("")
		CASE	2
			dw_account_list.SetFilter("left( type , 1 ) = 'A' or type = 'MPC' or type = 'MPD' or type = 'SDC' or type = 'SDD'")
		CASE	3
			dw_account_list.SetFilter("(left( type , 1 ) = 'A' or type = 'MPC' or type = 'MPD' or type = 'SDC' or type = 'SDD') and paid_count = 0")
		CASE	4
			dw_account_list.SetFilter("type = 'AC'")
		CASE	5
			dw_account_list.SetFilter("type = 'AD' or type = 'MPD' or type = 'SDD'")
		CASE	6
			dw_account_list.SetFilter("type = 'AH'")
		CASE	7
			dw_account_list.SetFilter("type = 'AI'")
		CASE	8
			dw_account_list.SetFilter("type = 'AP'")
		CASE	9
			dw_account_list.SetFilter("type = 'AR'")
		CASE	10
			dw_account_list.SetFilter("type = 'AT'")
	END CHOOSE

	dw_account_list.Filter()
	is_filter_name = as_option_name
	gb_account_list.Text = "Document View: " + is_filter_name + " / " + is_sort_name

	dw_account_list.TriggerEvent(RowFocusChanged!)
	ll_list_rownum = dw_account_list.GetRow()
	IF ll_list_rownum > 0 THEN
		dw_account_list.uf_ProcessSelect(ll_list_rownum,"Mouse")
	END IF

CASE "sort"

	CHOOSE CASE al_option_number

	CASE 1	
		dw_account_list.SetSort("claim A, type A, date A, comment A, docid A")
	CASE 2
		dw_account_list.SetSort("date A, claim A, type A, comment A, docid A")
	CASE 3
		dw_account_list.SetSort("docid A")
	CASE 4
		dw_account_list.SetSort("type A, date A")
	CASE 5
		dw_account_list.SetSort("sender A, date A") 
	End Choose

	dw_account_list.Sort()

	is_sort_name = as_option_name
	gb_account_list.Text = "Document View: " + is_filter_name + " / " + is_sort_name
	dw_account_list.TriggerEvent(RowFocusChanged!)
	ll_list_rownum = dw_account_list.GetRow()
	IF ll_list_rownum > 0 THEN
		dw_account_list.uf_ProcessSelect(ll_list_rownum,"Mouse")
	END IF

CASE "remove after payment"

	CHOOSE CASE al_option_number
	CASE 1
		ib_remove_after_payment = True
	CASE 2
		ib_remove_after_payment = False
	End Choose

CASE "imaged view"
	CHOOSE CASE al_option_number
	CASE 1
		IF not ib_imaged_view THEN
			ib_imaged_view = True
			wf_retrieve_doclist() 
			wf_screen_reset("display imaged/nonimaged")
			st_advance_notice.BringToTop = True
		END IF
	CASE 2
		IF ib_imaged_view THEN
			ll_claim_no = dw_display_claim_info.GetItemNumber(1,"claim_no")
			ib_imaged_view = False
			dw_account_type_payments_list.Reset()
			dw_account_type_payments_list.Retrieve(ll_claim_no)
			SQLCA.nf_handle_error("dw_account_type_payments_list",is_module_name,"wf_apply_doclist_option")
			wf_screen_reset("display imaged/nonimaged")
		END IF
	END CHOOSE

	wf_screen_reset("after refresh")
END CHOOSE
	
end subroutine

public subroutine wf_screen_reset (string as_screen_change);CHAR  			lc_screen
STRING 		ls_hist_flag1, ls_hist_flag2, ls_type_code, ls_doc_subtype_code
INTEGER		li_row

IF dw_account_type_payments_list.Visible = TRUE THEN
	lc_screen = 'Y'
ELSE
	lc_screen = 'N'
END IF

/* Get history_flag values */
IF dw_display_claim_info.GetRow() > 0 THEN
	ls_hist_flag1 = dw_display_claim_info.GetItemString(1,'claim_history_flag')
	ls_hist_flag2 = dw_display_claim_info.GetItemString(1, 'individual_history_flag')
END IF

li_row = dw_account_list.getRow() 
IF li_row > 0 Then
	ls_type_code = dw_account_list.getItemString(li_row, 'type')
End If

CHOOSE CASE as_screen_change

	CASE "disable all", "update on"
		cb_advance.Enabled									= False
		cb_add.Enabled											= False
		cb_pay_ephysio.enabled 							= False
		cb_reject.Enabled										= False
		cb_close.Enabled										= False
		cb_delete.Enabled										= False
		cb_link.Enabled											= False
		cb_refresh.Enabled 									= False
		cb_remove.Enabled									= False
		cb_send.Enabled										= False
		dw_account_list.Enabled 							= False
		dw_account_payment_list.Enabled				= False
		dw_account_type_payments_list.Enabled 	= False
			
		IF as_screen_change = "disable all" THEN
			cb_view.Enabled						= False
		END IF

	CASE "not authorized"
		cb_advance.Enabled						=	False
		cb_add.Enabled								=	False
		cb_pay_ephysio.enabled 				= False
		cb_reject.Enabled							=	False
		cb_delete.Enabled							=	False
		cb_link.Enabled								=	False
	
	CASE "authorized"
		cb_advance.Enabled						=	True
		cb_add.Enabled								=	True
		cb_pay_ephysio.enabled					=	True
		cb_reject.Enabled							=	True
		cb_delete.Enabled							=	True
		cb_link.Enabled								=	True
		
	CASE "display imaged/nonimaged"
		
		IF ib_imaged_view THEN
			IF not gb_account_list.Visible = True THEN
				gb_account_list.Visible					= True
				gb_account_payment_list.Visible		= True
				cb_add.Visible								= True
				cb_pay_ephysio.visible				= TRUE
				cb_reject.Visible							= True
				cb_view.Visible							= True
				cb_send.Visible							= True
				cb_remove.Visible							= True
				cb_link.Visible							= True
				dw_account_list.Visible					= True
				dw_account_payment_list.Visible		= True
				cb_delete.Visible							= True		// to bring it to the front
				cb_authorization.Visible				= True		// to bring it to the front
				gb_noaccount_payment_list.Visible	= False
				cb_advance.Visible						= False
				dw_account_type_payments_list.Visible = False
				
				IF st_advance_notice.Visible = True THEN st_advance_notice.bringtotop = true
			END IF
		ELSE
			IF gb_account_list.Visible = True THEN
				cb_add.Visible								= False
				cb_pay_ephysio.visible				= FALSE
				cb_reject.Visible							= False
				cb_view.Visible							= False
				cb_send.Visible							= False
				cb_remove.Visible							= False
				cb_link.Visible							= False
				gb_account_list.Visible					= False
				dw_account_list.Visible					= False
				gb_account_payment_list.Visible		= False
				dw_account_payment_list.Visible		= False
				gb_noaccount_payment_list.Visible	= True
				dw_account_type_payments_list.Visible = True
				cb_advance.Visible						= True
				cb_delete.Visible							= True 		// to bring it to the front
				cb_authorization.Visible				= True 		// to bring it to the front
			END IF
		END IF

CASE "after refresh", "update off"
		cb_close.Enabled							= True
		cb_refresh.Enabled						= True
		dw_select_taxing_category.Enabled	= True

		IF ib_imaged_view THEN
			dw_account_list.Enabled				= True
			dw_account_payment_list.Enabled	= True
		ELSE
			dw_account_type_payments_list.Enabled = True
		END IF
		
		IF lc_screen = 'N' THEN dw_account_list.Enabled	= TRUE

		IF il_catid = 2 THEN
			dw_enter_claim_no.Enabled			= True
			IF inv_payment.nf_is_valid_status_act_pymt() THEN
				IF ls_hist_flag1 = 'N' AND ls_hist_flag2 = 'N' THEN
					cb_advance.Enabled = True
					IF lc_screen = 'Y' THEN
     					cb_authorization.Enabled =	TRUE
					END IF
				ELSE 
					cb_advance.enabled 			= FALSE
					cb_add.enabled 				= FALSE
					cb_pay_ephysio.enabled 	= FALSE
					cb_pay_ephysio.enabled		= FALSE
					cb_delete.enabled 			= FALSE
					cb_reject.enabled 				= FALSE
	  	    		cb_authorization.Enabled 	= FALSE
					
					MessageBox('Invalid Claim','History Claim does not allow for account payments.')
				END IF			
			ELSE
				cb_advance.enabled 				= FALSE
				cb_add.enabled 					= FALSE
				cb_pay_ephysio.enabled 		= FALSE
				cb_delete.enabled 				= FALSE
				cb_reject.enabled 					= FALSE
      			cb_authorization.Enabled 		= FALSE
				
				MessageBox('Invalid Status','The claim status does not allow for account payments.')
			END IF
		END IF
		
	CASE "account list"
		IF dw_account_list.GetRow() <= 0 THEN
			cb_add.Enabled							= False
			cb_pay_ephysio.enabled 			= False
			cb_delete.Enabled						= False
			cb_view.Enabled 						= False
			cb_send.Enabled						= False
			cb_remove.Enabled					= False
			cb_link.Enabled						    = False	
		ELSE
			cb_view.Enabled						= True
			cb_send.Enabled						= True
			IF not il_catid = 2 THEN
				cb_remove.Enabled					= True
			ELSE
				cb_remove.Enabled					= False
			END IF

			// cb_add, cb_reject, and cb_link are taken care of within the rowfocuschanged event of dw_account_list
			// cb_delete is taken care of within  the rowfocuschanged event of dw_account_payment_list
		END IF	
		
END CHOOSE


/* If InBasket Unpaid Accounts called the Account Payment module, the documents are displayed
	for a specific claim and are removed once the user has paid or rejected the document. The user
	therefore cannot enter a claim number or taxing category or working set. The Remove after Payment check 
	box is always set on.
*/
IF is_as_mode = "inbasketunpaid" THEN
	dw_enter_claim_no.Enabled				= False
	dw_select_taxing_category.Enabled 	= False
	dw_select_working_set.Enabled 		= False
	cbx_remove.checked 						= True
	cbx_remove.enabled 						= False
	cbx_imaged_view.checked 				= True
	cbx_imaged_view.enabled 				= False
	ib_remove_after_payment 				= true
END IF

end subroutine

public subroutine wf_open_maintenance ();// wf_open_maintenance
// 
Long    ll_row, ll_claim_no 
Integer li_rtn 
String  ls_payment_type,ls_payment_sub_type,ls_admin_region, ls_administering_act_code

//	Make sure the user is authorized
ls_admin_region = iw_active_sheet.dw_basic_claim.GetItemString(1, 'admin_region_code')
IF gnv_user_authorizations.nf_authorizations_exist(ls_admin_region, "act") = FALSE THEN
	MessageBox("Account Payment Maintenance", "You have no authorization for this claim's region", Exclamation!) 
	RETURN 
END IF

IF ib_imaged_view = FALSE AND (istr_message.as_stringparm[2] <> "ADVANCE" AND istr_message.as_stringparm[2] <> "Authorization" ) THEN
	ll_row = dw_account_type_payments_list.GetRow()
	IF ll_row <> 0 THEN
		IF NOT ISNULL(dw_account_type_payments_list.getitemnumber(ll_row,"rehab_invoice_no")) Then
			MessageBox('Rehab Invoice Error','This payment is linked to a Rehab Invoice line item and can not be modified or deleted.', Exclamation!) 
			RETURN
		END IF
	END IF
END IF

// for project NBMS the following is true when attempting to delete a scheduled payment for a rejected claim,
// sometimes get the msg that empl# not valid - this msg should'nt come up for those payments. The number is always 0 
ls_payment_type = ""
IF dw_account_payment_list.rowcount() > 0 THEN
	ll_row = dw_account_payment_list.getrow()
	ls_payment_type = dw_account_payment_list.GetItemString(ll_row, "payment_payment_type_code")
	ls_payment_sub_type = dw_account_payment_list.GetItemString(ll_row, "payment_payment_sub_type_code") 
END IF	

IF ls_payment_type = '21' AND &
	(  ls_payment_sub_type = '03' &
	OR ls_payment_sub_type = '04' &
	OR ls_payment_sub_type = '05' &
	OR ls_payment_sub_type = '06' &
	OR ls_payment_sub_type = '13' &
	OR ls_payment_sub_type = '14' &
	OR ls_payment_sub_type = '15' &
	OR ls_payment_sub_type = '16' &
	OR ls_payment_sub_type = '17' &
	OR ls_payment_sub_type = '18' ) THEN
	// do not validate for cost allocation
ELSE	
	IF inv_payment.nf_validate_cost_alloc() < 0 THEN
		RETURN 
	END IF
END IF

ll_claim_no = dw_display_claim_info.GetItemNumber(1, "claim_no") 
istr_message.al_doubleparm[2] = ll_claim_no 

// Get the admin act code for the claim to pass into the Account Payment Maintenance window
SELECT administering_act_code 
  INTO :ls_administering_act_code 
  FROM CLAIM 
 WHERE claim_no = :ll_claim_no 
 USING SQLCA ; 

li_rtn = SQLCA.nf_handle_error('w_account_payment', '', 'wf_open_maintenance - SELECT administering_act_code FROM CLAIM') 

istr_message.as_stringparm[3] = ls_administering_act_code 

// Try to open the window and initialize.  If something happens in the initialization ensure the window is closed.
OpenWithParm(iw_account_payment_maintenance, istr_message, iw_active_sheet)
IF IsValid(iw_account_payment_maintenance) THEN
	iw_account_payment_maintenance.wf_set_payment_object(inv_payment)
	iw_account_payment_maintenance.wf_set_parent_window(This) 
	wf_screen_reset("update on")
END IF 
end subroutine

public function integer wf_retrieve_claim (long al_claim_no);LONG					ll_advances, ll_individual_no
INTEGER				li_return_value, li_rtn, li_rows
STRING				ls_status, ls_display_message
DECIMAL				ldec_balance_amount
S_WINDOW_MESSAGE	lstr_message
U_DS              lds_overpayment_warning_sum


	wf_set_redraw(FALSE, 'wf_retrieve_claim')
	
	li_return_value = dw_display_claim_info.Retrieve(al_claim_no)
	SQLCA.nf_handle_error('w_account_payment', '', 'wf_retrieve_claim - dw_display_claim_info.Retrieve(al_claim_no)')
	IF li_return_value < 1 THEN 
		wf_set_redraw(TRUE, 'wf_retrieve_claim')
		Return li_return_value
	END IF

	iw_active_sheet.wf_set_claim(dw_display_claim_info.GetItemNumber(1,"claim_no"))
	inv_payment.nf_set_claim_no(al_claim_no)

/*	If the claim is an imaged claim, check to see if there were advances issued
*/
	ll_advances = inv_payment.nf_get_advances()
	IF ll_advances < 0 THEN
		wf_set_redraw(TRUE, 'wf_retrieve_claim')
		Return -1
	END IF

	ls_status = dw_display_claim_info.GetItemString(1,'imaged_flag')
	IF ls_status = "N" THEN
		ib_imaged_view = FALSE
		cbx_imaged_view.checked = FALSE
	ELSE
		ib_imaged_view = TRUE
		cbx_imaged_view.checked = TRUE
	END IF

	IF ll_advances > 0 THEN
		st_advance_notice.Visible = TRUE
	ELSE
		st_advance_notice.Visible = FALSE
	END If

	ll_individual_no = dw_display_claim_info.GetItemNumber(1,'individual_no')
	st_overpayment.visible = FALSE

/*	check for overpayment to any claim participant
*/

	lds_overpayment_warning_sum = Create U_DS
	lds_overpayment_warning_sum.DataObject = 'ds_overpayment_warning_sum'
	lds_overpayment_warning_sum.SetTransObject(SQLCA)
	li_rows = lds_overpayment_warning_sum.Retrieve(al_claim_no)
	SQLCA.nf_handle_error('w_account_payment','ds_overpayment_warning_sum','lds_overpayment_warning_sum.Retrieve')
	
	IF li_rows > 0 THEN
		ls_display_message = ls_display_message + '~r~n~r~n OVERPAYMENT situation exists.'
		st_overpayment.Text = 'Overpayment'
		st_overpayment.visible = TRUE
	END IF

	IF ls_display_message <> '' THEN
		lstr_message.as_stringparm[1] = ls_display_message
		lstr_message.as_stringparm[2] = 'I'
		lstr_message.al_doubleparm[1] = ll_individual_no
		lstr_message.al_doubleparm[2] = al_claim_no
		
		wf_set_redraw(TRUE, 'wf_retrieve_claim')
		OpenWithParm(w_payment_message, lstr_message)
	END IF
	
	wf_set_redraw(TRUE, 'wf_retrieve_claim')
	
Return 1
end function

public function integer wf_retrieve_doclist ();Long    ll_retrieval_arg, ll_docid, ll_retrieval_arg2, ll_find, ll_num_rows, n, ll_count
Integer li_return
String  ls_syntax, ls_type

//	Check to see if the where clause on the datawindow has to be modified as this 
//	datawindow is used for retrieving by claim and by category.  If so, make the modifications.
wf_set_redraw(FALSE, 'wf_retrieve_doclist')
IF is_as_mode = "inbasketunpaid" THEN
	ll_retrieval_arg = il_inbasket_claim_no
	ll_retrieval_arg2 = 0	
ELSE
	IF il_catid = 2 THEN
		ll_retrieval_arg = dw_enter_claim_no.GetItemNumber(1, "claim_no")
		ll_retrieval_arg2 = 0
	ELSE
		ll_retrieval_arg2 = il_catid
		ll_retrieval_arg = 0
	END IF
END IF

//	Retrieve and display the account list
dw_account_list.Reset()


li_return = dw_account_list.Retrieve(ll_retrieval_arg,ll_retrieval_arg2,is_imaged_flag)
li_return = SQLCA.nf_handle_error("dw_account_list",'w_account_payment',"wf_retrieve_doclist")


IF li_return < 0 THEN
	wf_set_redraw(TRUE, 'wf_retrieve_doclist')
	Return li_return  // This will keep everything disabled and allow the user to try to refresh again
END IF

IF is_as_mode = "inbasketunpaid" THEN
	wf_apply_doc_list_option("filter",3,"")
END IF

// Reapply any filters and sorts that may have been overridden
ls_syntax = String(dw_account_list.Object.DataWindow.Table.Sort)
IF ls_syntax <> '?' THEN
	dw_account_list.Sort()
END IF

ls_syntax = String(dw_account_list.Object.DataWindow.Table.Filter)
IF ls_syntax <> '?' THEN
	dw_account_list.Filter()
END IF

// PR 2871 - the RowFocusChanged does not fire when a sort & filter
li_return = dw_account_list.Retrieve(ll_retrieval_arg,ll_retrieval_arg2,is_imaged_flag)

IF li_return < 0 THEN
	Return li_return  // This will keep everything disabled and allow the user to try to refresh again
END IF

IF dw_account_payment_list.GetRow() <> 0 THEN
	ll_docid = dw_account_payment_list.GetItemNumber(dw_account_payment_list.GetRow(),'doc_id')
	ll_find = dw_account_list.Find('docid = ' + String(ll_docid),1,dw_account_list.RowCount())
	
	IF ll_find <> 0 THEN
		dw_account_list.SelectRow(ll_find, TRUE)
	END IF
END IF

// Display RB beside doc types "ARX" that have a claim reimbursement.
ll_num_rows = dw_account_list.RowCount()
FOR n = 1 TO ll_num_rows
	ls_type = dw_account_list.GetItemString(n, "type")
	IF ls_type = "ARX" THEN
		ll_docid = dw_account_list.GetItemNumber(n, "docid")

		SELECT COUNT(*) 
		  INTO :ll_count 
		  FROM PAYMENT_DOCUMENT PD, 
				 PAYMENT_PRESCRIPTION PP 
		 WHERE PD.doc_id = :ll_docid
			AND PD.payment_no = PP.payment_no 
			AND PP.carrier_code = "WB" ; 

		li_return = SQLCA.nf_handle_error("w_account_payment", '', "wf_retrieve_doclist - SELECT COUNT(*) FROM PAYMENT_DOCUMENT PD, PAYMENT_PRESCRIPTION PP")

		IF ll_count > 0 THEN
			dw_account_list.SetItem(n, "rb_symbol", "RB")
		END IF
	END IF
NEXT

wf_set_redraw(TRUE, 'wf_retrieve_doclist')

return 0
end function

public function string wf_get_module_source_code (long ll_docid);STRING		ls_module_source_code


SELECT module_source_code
INTO :ls_module_source_code
FROM DOC
WHERE docid = :ll_docid
USING ImageTrans;

ImageTrans.nf_handle_error("w_account_payment", "", "cb_remove - SELECT module_source_code") 

return ls_module_source_code

end function

public function boolean wf_can_line_item_be_deleted (long al_payment_no, long al_docid);/*
SELECT PAYMENT.payment_type_code ,PAYMENT.paid_from_date ,PAYMENT.paid_to_date ,
           PAYMENT.processed_date ,PAYMENT.submitted_amount ,PAYMENT.total_payment_amount ,
           PAYMENT_DOCUMENT.paid_status_code , PAYMENT_DOCUMENT.doc_id ,
           PAYMENT_DOCUMENT.paid_status_explanation_code ,PAYMENT.payment_no ,
           Payment_Combination.authorization_type_code  , 0 as related_document, UNAPPLIED_CLAIM_TXN.recipient_type_code 'recipient_type_code',
           REHAB_INVOICE_LINE_ITEM.rehab_invoice_no
 FROM PAYMENT 
     INNER JOIN Payment_Type            ON PAYMENT.payment_type_code 	= Payment_Type.payment_type_code
     INNER JOIN Payment_Combination     ON Payment_Type.payment_type_code = Payment_Combination.payment_type_code
LEFT OUTER JOIN PAYMENT_DOCUMENT        ON PAYMENT.payment_no 	    = PAYMENT_DOCUMENT.payment_no 
     INNER JOIN UNAPPLIED_CLAIM_TXN     ON PAYMENT.payment_no 	    = UNAPPLIED_CLAIM_TXN.payment_no
LEFT OUTER JOIN REHAB_INVOICE_LINE_ITEM ON PAYMENT.payment_no   = REHAB_INVOICE_LINE_ITEM.payment_no
WHERE  PAYMENT.claim_no 		           =  :val_claim_no
  and  Payment_Combination.authorization_type_code = 'act'
  and  Payment_Combination.opening_type_code 	   = 'I'
  and  UNAPPLIED_CLAIM_TXN.txn_type_code 	   = '1'
  
  
  SELECT count(*)
INTO :li_count
FROM REHAB_TASK  a
	JOIN REHAB_TASK_AUTHORIZATION b ON a.claim_no = b.claim_no AND a.task_no = b.task_no
    JOIN Billable_Item_Rehab_Task_Xref 	c 
		on a.rehab_service_code		  	= c.rehab_service_code
			AND a.rehab_program_code  	= c.rehab_program_code
			AND a.task_type_code      	= c.task_type_code
			AND a.task_sub_type_code  	= c.task_sub_type_code
			AND a.task_specific_code  		= c.task_specific_code
			AND b.billable_xref_no    		= c.billable_xref_no
	join Billable_Item 		d 	on c.billable_item_no 			= d.billable_item_no 
    join Rehab_Program 	e 	on a.rehab_program_code	= e.rehab_program_code 
WHERE a.claim_no							= :ll_claim_no
AND   a.rehab_service_code			= 'S022'
AND   a.task_status_code				NOT	IN ('03')
AND   c.payment_type_code			<> ''
GROUP BY a.rehab_program_code, a.planned_start_date, e.rehab_program_desc_e, actual_completion_date, a.task_no
USING SQLCA;
SQLCA.nf_handle_error("w_account_payment","clicked","SELECT count(*) INTO :li_count") 

/* if no authorizations they cant really do anything unless there is something already saved in the lineitems - will have to worry about that later */
IF isnull(li_count) OR li_count < 1 THEN 
	messagebox('No Rehab Authorizations', 'No Rehab Authorizations are currently available.' )
	RETURN -1
END IF 
   
*/
INTEGER			li_count

SELECT  	count(*)
INTO			:li_count
FROM  		REHAB_INVOICE_LINE_ITEM 	a
	join 		UNAPPLIED_CLAIM_TXN 			b	ON a.claim_no	= b.claim_no AND a.payment_no = b.payment_no
WHERE 	a.web_create_id 	<> 0 
AND   		a.payment_no 				= :al_payment_no
USING 		SQLCA;
SQLCA.nf_handle_error("w_account_payment", "wf_can_line_item_be_deleted()", "SELECT  count(*)")

//something to delete
IF li_count > 0 THEN RETURN TRUE

//nothing to delete
RETURN FALSE
end function

on closequery;call w_a_tool::closequery;
//	Destroy the instance of the payment user object

	IF IsValid(inv_payment) THEN
		Destroy(inv_payment)
	END IF


end on

event open;call super::open;INTEGER  							li_return_status
S_WINDOW_MESSAGE		lstr_window_message


	idt_server_datetime	= f_server_datetime()
	is_module_name		= st_title.text

   lstr_window_message= Message.PowerObjectParm
	
	
	
// Get the name of the active work sheet
iw_sheet = w_frame.GetActiveSheet()
If Not IsValid(iw_sheet) Then
	MessageBox("InBasket","Could not determine active sheet.  You may have to reboot.")
	Close(This)
	Return
End If

/* Init value for ADVANCE SCREEN for authorization_no. */

	istr_message.al_doubleparm[5] = 0

	is_as_mode 	= lstr_window_message.as_mode 
	
	IF is_as_mode = "inbasketunpaid" THEN
		il_inbasket_claim_no = lstr_window_message.al_doubleparm[2] 
	END IF

	iw_account_payment_maintenance 	= w_account_payment_maintenance
	iw_account_link_maintenance 			= iw_account_link_maintenance
	iw_physio_reimbursements 				= w_physio_reimbursements

/*	Figure out what the setid (imaging work area) is for the user
*/
	SetNull(il_setid)

   SELECT User_Profile.default_image_working_setid  
    INTO :il_setid  
    FROM User_Profile
	 WHERE User_Profile.user_id = :vgst_user_profile.user_id using SQLCA;

	IF SQLCA.nf_handle_error("Embedded SQL: Retrieve from User_Profile",is_module_name,"open event") < 0 THEN
		Close(this)
		Return
	END IF

	IF IsNull(il_setid) OR il_setid = 0 THEN
		MessageBox(is_module_name,"Could not find your default work area for account payment ~r~n" + &
			"...Please call the Help Desk",StopSign!)
		Close(this)
		Return
	END IF


/*	Set transaction objects on the main datawindows
*/
	dw_account_list.SetTransObject(SQLCA)
	dw_display_claim_info.SetTransObject(SQLCA)
	dw_account_payment_list.SetTransObject(SQLCA)
	dw_account_type_payments_list.SetTransObject(SQLCA)
	dw_claimsmaster_ref_count.SetTransObject(ImageTrans)
	dw_retrieve_ref_entries.SetTransObject(ImageTrans)


/*	Set the selection mode on the list datawindows (Single Select)
*/
	dw_account_list.uf_setselect(1)
	dw_account_payment_list.uf_setselect(1)
	dw_account_type_payments_list.uf_setselect(1)


/*	Create an instance of the user object for the view function
*/

	iu_dw_document_path = dw_document_path


/*	Create an instance of the payment user object
*/
	inv_payment = Create n_payment
	inv_payment.nf_set_basic_claim(dw_display_claim_info)

/*	Set the focus to the appropriate place
*/
	PostEvent("ue_postopen")


/* If the Account Payment was called from the InBasket module, the user
	will see only the documents for that claim from the Claim Master category
*/
	IF is_as_mode = "inbasketunpaid" THEN
		il_inbasket_catid = 2
		dw_select_taxing_category.Reset()
		dw_select_taxing_category.InsertRow(0)
		dw_enter_claim_no.Reset()
		dw_enter_claim_no.InsertRow(0)

 		dw_select_taxing_category.SetItem(1,"catid", il_inbasket_catid)
		dw_select_taxing_category.SetText(string(il_inbasket_catid))
		dw_select_taxing_category.AcceptText()
		dw_enter_claim_no.SetItem(1, "claim_no", il_inbasket_claim_no)
		dw_enter_claim_no.SetText(string(il_inbasket_claim_no))
		dw_enter_claim_no.AcceptText()
        dw_select_taxing_category.Enabled = false
		dw_enter_claim_no.Enabled = false
		/* Retrieve the claim info for the claim */
		li_return_status = wf_retrieve_claim(il_inbasket_claim_no)
		IF li_return_status < 0  THEN
			Return
		END IF
		cb_refresh.default = false
		cb_refresh.TriggerEvent(clicked!)

   END IF
end event

on w_account_payment.create
int iCurrent
call super::create
this.dw_select_working_set=create dw_select_working_set
this.dw_select_taxing_category=create dw_select_taxing_category
this.dw_retrieve_ref_entries=create dw_retrieve_ref_entries
this.cb_send=create cb_send
this.cb_delete=create cb_delete
this.cb_view=create cb_view
this.cb_remove=create cb_remove
this.cb_link=create cb_link
this.dw_enter_claim_no=create dw_enter_claim_no
this.st_1=create st_1
this.cbx_imaged_view=create cbx_imaged_view
this.cbx_remove=create cbx_remove
this.cb_refresh=create cb_refresh
this.dw_document_path=create dw_document_path
this.st_advance_notice=create st_advance_notice
this.st_overpayment=create st_overpayment
this.uo_image_append=create uo_image_append
this.dw_claimsmaster_ref_count=create dw_claimsmaster_ref_count
this.cb_add=create cb_add
this.dw_account_list=create dw_account_list
this.gb_account_list=create gb_account_list
this.dw_account_payment_list=create dw_account_payment_list
this.cb_reject=create cb_reject
this.cb_authorization=create cb_authorization
this.cb_advance=create cb_advance
this.gb_noaccount_payment_list=create gb_noaccount_payment_list
this.dw_account_type_payments_list=create dw_account_type_payments_list
this.dw_display_claim_info=create dw_display_claim_info
this.cb_pay_ephysio=create cb_pay_ephysio
this.cb_delete_ephysio=create cb_delete_ephysio
this.gb_account_payment_list=create gb_account_payment_list
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_select_working_set
this.Control[iCurrent+2]=this.dw_select_taxing_category
this.Control[iCurrent+3]=this.dw_retrieve_ref_entries
this.Control[iCurrent+4]=this.cb_send
this.Control[iCurrent+5]=this.cb_delete
this.Control[iCurrent+6]=this.cb_view
this.Control[iCurrent+7]=this.cb_remove
this.Control[iCurrent+8]=this.cb_link
this.Control[iCurrent+9]=this.dw_enter_claim_no
this.Control[iCurrent+10]=this.st_1
this.Control[iCurrent+11]=this.cbx_imaged_view
this.Control[iCurrent+12]=this.cbx_remove
this.Control[iCurrent+13]=this.cb_refresh
this.Control[iCurrent+14]=this.dw_document_path
this.Control[iCurrent+15]=this.st_advance_notice
this.Control[iCurrent+16]=this.st_overpayment
this.Control[iCurrent+17]=this.uo_image_append
this.Control[iCurrent+18]=this.dw_claimsmaster_ref_count
this.Control[iCurrent+19]=this.cb_add
this.Control[iCurrent+20]=this.dw_account_list
this.Control[iCurrent+21]=this.gb_account_list
this.Control[iCurrent+22]=this.dw_account_payment_list
this.Control[iCurrent+23]=this.cb_reject
this.Control[iCurrent+24]=this.cb_authorization
this.Control[iCurrent+25]=this.cb_advance
this.Control[iCurrent+26]=this.gb_noaccount_payment_list
this.Control[iCurrent+27]=this.dw_account_type_payments_list
this.Control[iCurrent+28]=this.dw_display_claim_info
this.Control[iCurrent+29]=this.cb_pay_ephysio
this.Control[iCurrent+30]=this.cb_delete_ephysio
this.Control[iCurrent+31]=this.gb_account_payment_list
end on

on w_account_payment.destroy
call super::destroy
destroy(this.dw_select_working_set)
destroy(this.dw_select_taxing_category)
destroy(this.dw_retrieve_ref_entries)
destroy(this.cb_send)
destroy(this.cb_delete)
destroy(this.cb_view)
destroy(this.cb_remove)
destroy(this.cb_link)
destroy(this.dw_enter_claim_no)
destroy(this.st_1)
destroy(this.cbx_imaged_view)
destroy(this.cbx_remove)
destroy(this.cb_refresh)
destroy(this.dw_document_path)
destroy(this.st_advance_notice)
destroy(this.st_overpayment)
destroy(this.uo_image_append)
destroy(this.dw_claimsmaster_ref_count)
destroy(this.cb_add)
destroy(this.dw_account_list)
destroy(this.gb_account_list)
destroy(this.dw_account_payment_list)
destroy(this.cb_reject)
destroy(this.cb_authorization)
destroy(this.cb_advance)
destroy(this.gb_noaccount_payment_list)
destroy(this.dw_account_type_payments_list)
destroy(this.dw_display_claim_info)
destroy(this.cb_pay_ephysio)
destroy(this.cb_delete_ephysio)
destroy(this.gb_account_payment_list)
end on

event close;call super::close;
IF IsValid(iw_sheet.iw_inbasket)  OR IsValid(iw_sheet.iw_inbasket_old) THEN
	// do not close sheet
ELSE
	post close(iw_sheet)
END IF
end event

type st_title from w_a_tool`st_title within w_account_payment
integer width = 3173
string text = "Maintain Account Payments"
end type

type cb_close from w_a_tool`cb_close within w_account_payment
integer y = 1704
integer taborder = 20
end type

on cb_close::clicked;IF is_as_mode = "inbasketunpaid" THEN
	parent.iwi_parent_window.triggerEvent("ue_enable_inbasket_menu")
	parent.iwi_parent_window.triggerEvent("ue_post_refreshdoc")
	SetPointer(HourGlass!)
	Close(parent)
ELSE
	SetPointer(HourGlass!)
	Close(parent)
END IF
end on

type dw_select_working_set from u_dw_online within w_account_payment
integer x = 9
integer y = 116
integer width = 1326
integer height = 88
integer taborder = 40
string dataobject = "d_work_areas"
boolean border = false
end type

on itemchanged;call u_dw_online::itemchanged;// Get the taxing buckets for the selected work area
	il_setid = Long(GetText())

	dw_select_taxing_category.Reset()

	idwc_taxing_categories.Retrieve(il_setid)
	IF ImageTrans.nf_handle_error("idwc_taxing_categories","w_account_payment","itemchanged for dw_select_working_set") < 0 THEN
		Close(Parent)
		Return
	END IF

	dw_select_taxing_category.InsertRow(0)
end on

type dw_select_taxing_category from u_dw_online within w_account_payment
integer x = 18
integer y = 216
integer width = 1120
integer height = 88
integer taborder = 50
string dataobject = "d_select_taxing_category"
boolean border = false
end type

event itemchanged;call super::itemchanged;LONG  ll_catid, ll_claim_no

IF is_as_mode <> "inbasketunpaid" THEN
	ll_catid = Long(This.GetText())

	IF ll_catid = 2 THEN
		dw_enter_claim_no.Enabled = True
		dw_enter_claim_no.SetFocus()
		is_imaged_flag = 'Y'
	ELSE
		SetNull(ll_claim_no)
		dw_enter_claim_no.SetItem(1,"claim_no",ll_claim_no)
		dw_enter_claim_no.Enabled = False
		is_imaged_flag = 'N'
	END IF


	cb_refresh.default = True

END IF
end event

type dw_retrieve_ref_entries from u_dw_online within w_account_payment
boolean visible = false
integer x = 3195
integer y = 48
integer width = 786
integer height = 392
integer taborder = 160
string dataobject = "d_retrieve_ref_entries"
end type

type cb_send from commandbutton within w_account_payment
integer x = 1047
integer y = 876
integer width = 425
integer height = 100
integer taborder = 110
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Send Doc ..."
end type

event clicked;S_SEND_DOC_PARAMETERS	ls_send_doc_parameters
string ls_admin_region

//	Make sure the user is authorized

IF dw_display_claim_info.RowCount() > 0 THEN
		
	ls_admin_region = iw_active_sheet.dw_basic_claim.GetItemString(1,'admin_region_code')
	
	IF gnv_user_authorizations.nf_authorizations_exist(ls_admin_region,"act") = FALSE THEN
		MessageBox("Account Payment Maintenance","You have no authorization for this claim's region",Exclamation!)
		Return
	END IF
	
	ls_send_doc_parameters.msg_mode = FALSE
	ls_send_doc_parameters.claim_no = dw_display_claim_info.GetItemNumber(1,"claim_no")
	ls_send_doc_parameters.document_list = dw_account_list
	OpenWithParm(w_send_folder,ls_send_doc_parameters)
ELSE
	MessageBox('Error','Unable to determine claim number.')
END IF
end event

type cb_delete from commandbutton within w_account_payment
integer x = 1477
integer y = 1512
integer width = 425
integer height = 100
integer taborder = 190
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Delete"
end type

event clicked;LONG  			ll_rownum
INTEGER		li_count
s_window_message lstr_message

//	Load the structure and open the payment maintenance window

IF ib_imaged_view THEN
	ll_rownum 				= dw_account_payment_list.GetRow()
	istr_message.al_doubleparm[1]	= dw_account_payment_list.GetItemNumber(ll_rownum,"payment_no")
	istr_message.al_doubleparm[3]	= dw_account_payment_list.GetItemNumber(ll_rownum,"doc_id")
ELSE
	ll_rownum 				= dw_account_type_payments_list.GetRow()
	istr_message.al_doubleparm[1] 	= dw_account_type_payments_list.GetItemNumber(ll_rownum,"payment_no")
	istr_message.al_doubleparm[3]		= dw_account_type_payments_list.GetItemNumber(ll_rownum,"payment_document_doc_id")
	IF IsNull(istr_message.al_doubleparm[3]) then istr_message.al_doubleparm[3] = 0
END IF

istr_message.al_doubleparm[4] = 0
istr_message.as_stringparm[1] = ""
SetNull(istr_message.adtm_datetimeparm[1])
istr_message.as_mode = "delete"

IF ib_imaged_view = False THEN
	istr_message.as_stringparm[2] = "ADVANCE"
ELSE
	istr_message.as_stringparm[2] = ""
END IF

// dont allow them to delete if a rehab_invoice line item exists for this docid 
SELECT 	count(*) 
INTO 		:li_count
FROM 		REHAB_INVOICE_LINE_ITEM a
	join		PAYMENT b ON a.payment_no = b.payment_no	  
WHERE 	a.payment_no = :istr_message.al_doubleparm[1]
USING		SQLCA;
SQLCA.nf_handle_error("w_account_payment", "cb_delete", "SELECT count(*)... ")

IF li_count > 0 THEN 
	MessageBox("Rehab Invoice Payment","This Payment is associated with a Rehab Invoice. Please Delete through the *Pay Ephysio* Invoice screen.",Exclamation!)
	RETURN
END IF 

wf_open_maintenance()
end event

type cb_view from commandbutton within w_account_payment
integer x = 617
integer y = 876
integer width = 425
integer height = 100
integer taborder = 100
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&View ..."
end type

event clicked;LONG	ll_rownum,ll_doc_id
string ls_doc_type
integer li_rtn

//	Get the document id of the selected row

	ll_rownum = dw_account_list.GetRow()
	IF ll_rownum <= 0 THEN
		MessageBox("Medical Aid Payment Module","You must select a document before you can view",Exclamation!)
		Return
	END IF





ll_doc_id =dw_account_list.GetItemNumber(ll_rownum,"docid")
	
if isvalid(uo_image_append) then
	if uo_image_append.of_init(ll_doc_id)	<= 0 then
		RETURN
	end if
		
		
	ls_doc_type =  uo_image_append.of_get_file_type()
		
	
	CHOOSE CASE ls_doc_type
		/*  Imaged document */ 
		CASE 'IMA', 'TIF'
			li_rtn = uo_image_append.of_append_image(ll_doc_id)
			if li_rtn < 0 then
				RETURN
			end if
		case else
	/*		 Get value of document id field on selected row */
			iu_dw_document_path.f_manage_document(dw_account_list.GetItemNumber(ll_rownum,"docid"),"V","NORMAL")
	end choose
			
end if
		
		
		
	
	
	
	
end event

type cb_remove from commandbutton within w_account_payment
integer x = 1477
integer y = 876
integer width = 425
integer height = 100
integer taborder = 120
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Re&move"
end type

event clicked;LONG		ll_docfldid, ll_docid, ll_doccount, ll_account_row, ll_cntr, ll_nmbr_folders
LONG		ll_nmbr_rows_deleted = 0
STRING	ls_select,ls_admin_region
LONG		ll_count
STRING	ls_module_source_code

	ll_account_row = dw_account_list.GetRow()
	IF ll_account_row <= 0 THEN
		Return
	END IF

	/* If the Account Payment module was called from the InBasket for viewing unpaid accounts for
		a claim, the user is always pointing to the Claim Master folder but the account documents
		are to be deleted from ALL other folders in which they exist. The deletion of the paid documents
		will be automatic as the user pays or rejects the document (i.e. the user does not choose the
		REMOVE button). The document id of the paid/rejected document is passed as an instance variable.
	*/
	IF is_as_mode <> "inbasketunpaid" THEN
		IF il_catid = 2 THEN
			MessageBox(is_module_name,"You can't remove a document from the master folder",Exclamation!)
			Return
		END IF

		/*	Make sure the user is authorized
		*/
		IF dw_display_claim_info.RowCount() < 1 THEN
			MessageBox('Error','Unable to determine claim.')
			Return
		END IF

		ls_admin_region = iw_active_sheet.dw_basic_claim.GetItemString(1,'admin_region_code')
	
		IF gnv_user_authorizations.nf_authorizations_exist(ls_admin_region,"act") = FALSE THEN
			MessageBox("Account Payment Maintenance","You have no authorization for this claim's region",Exclamation!)
			Return
		END IF
		
		ll_docid = dw_account_list.GetItemNumber(ll_account_row,"docid")

		/*	If the document is an account, and there have been no entries made,
			check to ensure the user really wants to remove this document
		*/
		IF wf_IsAccount(dw_account_list.GetItemString(ll_account_row,"type")) THEN
			IF dw_account_payment_list.RowCount() = 0 AND dw_account_list.GetItemNumber(ll_account_row,"paid_count") = 0 THEN
				IF MessageBox(is_module_name, "This document has no entries.~r~nAre you sure you want to delete it",Question!,YesNo!) = 2 THEN Return
			END IF
		END IF
	ELSE
		ll_docid = dw_account_list.GetItemNumber(ll_account_row,"docid")
	END IF
	
	
/* Get the source of the doucument
*/
	ls_module_source_code = wf_get_module_source_code(ll_docid)
	
	IF ls_module_source_code = '12' Then
	
	/* 3.20  An imported medical e-invoice must not be removed unless one of the following is true:
				·	Invoice is paid
				·	Invoice is scheduled to be paid
				·	Invoice is rejected for payment
	*/
		SELECT count(*)
		INTO :ll_count
		FROM PAYMENT_DOCUMENT
		WHERE doc_id = :ll_docid
		  AND paid_status_code in('P','S','R')
		USING SQLCA;
		
		SQLCA.nf_handle_error("w_account_payment", "", "cb_remove - SELECT paid_status_code") 
	
		//IF the count is greater than zero, the document is paid, scheduled to be paid or 
		//is rejected.
		IF ll_count = 0 Then
			MessageBox(is_module_name,'This document cannot be removed until it is "Paid", "Scheduled to be Paid" or "Rejected".')
			RETURN
		END IF
	End if
	
	
	
/* 
	If called from Inbasket Unpaid accounts - Change the SQL in the datawindow to look for
	all REF entries for the current document , excluding folders from the ClaimsMaster category as the 
	documents are never deleted from this category
*/
	ls_select = dw_retrieve_ref_entries.Describe("DataWindow.Table.Select")

	IF is_as_mode = "inbasketunpaid" THEN
		f_replace_text(ls_select,"REF.doccatid = :val_catid","REF.doccatid <> :val_catid")
		dw_retrieve_ref_entries.Modify("DataWindow.Table.Select='" + ls_select + "'")		
	END IF

/*	Remove all references to the document/category from Imaging Database (Note: It is possible that
	nothing gets deleted here as another user may have deleted this document from this category
	already.  However, continue down through and remove it from view.
*/
	ll_nmbr_folders = dw_retrieve_ref_entries.Retrieve(ll_docid, il_catid)

	IF ImageTrans.nf_handle_error("w_account_payment", "", "cb_remove - dw_retrieve_ref_entries.Retrieve(ll_docid, il_catid)") < 0 THEN 
		Return
	END IF
	
	ImageTrans.nf_begin_transaction()
	
	ll_cntr = 1
	DO WHILE ll_cntr <= ll_nmbr_folders
		ll_docfldid = dw_retrieve_ref_entries.GetItemNumber(ll_cntr,"docfldid")
/*		Delete the corresponding REF entries
*/
		IF is_as_mode <> "inbasketunpaid" THEN
			DELETE REF
		 		WHERE docfldid  = :ll_docfldid 
 		   		AND docid    = :ll_docid
		   		AND doccatid = :il_catid
		 		USING ImageTrans;
		ELSE
			DELETE REF
		 		WHERE docfldid   = :ll_docfldid 
 		   		AND docid     = :ll_docid
		   		AND doccatid <> :il_catid
		 		USING ImageTrans;
		END IF
		IF ImageTrans.nf_handle_error("w_account_payment", "", "cb_remove - DELETE REF") < 0 THEN 
			Return
		END IF
		ll_nmbr_rows_deleted = ll_nmbr_rows_deleted + ImageTrans.SQLNRows

/*		Check to see if the folder is now empty.  If so, remove the corresponding entried from 
		FLD, PRV_FLD and claimsworking
*/
		SELECT count(*)
		  INTO :ll_doccount FROM REF
		 	WHERE docfldid = :ll_docfldid
		 	USING ImageTrans;
		IF ImageTrans.nf_handle_error("w_account_payment", "", "cb_remove - SELECT count(*) FROM REF") < 0 THEN 
			Return
		END IF
		IF ll_doccount = 0 THEN
			DELETE 	FLD
			 WHERE 	fldid = :ll_docfldid
			 USING 	ImageTrans;
			IF ImageTrans.nf_handle_error("w_account_payment", "", "cb_remove - DELETE FLD") < 0 THEN
				 Return
			END IF
			
			DELETE	CLAIM_WORKING
		 	 WHERE 	folderid = :ll_docfldid
			 USING 	ImageTrans;
			IF ImageTrans.nf_handle_error("w_account_payment", "", "cb_remove - DELETE CLAIM_WORKING") < 0 THEN
				 Return
			END IF
		END IF
		ll_cntr ++
	LOOP

//	Now, if we have deleted documents, update the document reference count in the DOC table

	IF ll_nmbr_rows_deleted > 0 THEN
		SELECT 	docrefcount
	  	  INTO	:ll_doccount
		  FROM	DOC
		 WHERE 	docid = :ll_docid
		 USING	ImageTrans;
		IF ImageTrans.nf_handle_error("w_account_payment", "", "cb_remove - SELECT docrefcount FROM DOC") < 0 THEN 
			Return
		END IF
		IF ll_doccount <= ll_nmbr_rows_deleted THEN
			ImageTrans.nf_rollback_transaction()
			
			MessageBox(is_module_name,"Cannot remove document~r~nThis is the last reference to this document...Please call the helpdesk",Exclamation!)
			Return
		END IF

		UPDATE	DOC
		   SET 	docrefcount = docrefcount - :ll_nmbr_rows_deleted
		 WHERE 	docid = :ll_docid 
		 USING	ImageTrans;
		IF ImageTrans.nf_handle_error("w_account_payment", "", "cb_remove - UDPATE DOC") < 0 THEN Return
	END IF

	ImageTrans.nf_commit_transaction()

/*	Now, remove the document from the doclist so it looks gone and make sure
	we get a rowfocuschanged to redisplay stuff
*/
	dw_account_list.DeleteRow(ll_account_row)
	IF ll_account_row = dw_account_list.GetRow() THEN
		dw_account_list.TriggerEvent(rowfocuschanged!)
	END IF
Return
end event

type cb_link from commandbutton within w_account_payment
integer x = 1906
integer y = 876
integer width = 425
integer height = 100
integer taborder = 130
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Link"
end type

event clicked;LONG  	ll_rownum
LONG		ll_docid
string 	ls_admin_region
STRING	ls_module_souce_code, ls_administering_act_code



//	Make sure the user is authorized	
ls_admin_region = iw_active_sheet.dw_basic_claim.GetItemString(1,'admin_region_code')

IF gnv_user_authorizations.nf_authorizations_exist(ls_admin_region,"act") = FALSE THEN
	MessageBox("Account Payment Maintenance","You have no authorization for this claim's region",Exclamation!)
	Return
END IF

ll_rownum 							    = dw_account_list.GetRow()

ll_docid = dw_account_list.GetItemNumber(ll_rownum,"docid")

ls_module_souce_code = wf_get_module_source_code(ll_docid)

IF ls_module_souce_code = '12' Then
	MessageBox(is_module_name,'An imported medical e-invoice must not be linked to an existing or scheduled payment.')
	RETURN
END IF

//	If there are already entries for this account, ensure that the user really wants to continue
IF dw_account_payment_list.RowCount() > 0 THEN
	IF MessageBox(is_module_name,"There are already entries for this document, are you sure you want to continue?",Question!,YesNo!) = 2 THEN
		Return
	END IF
END IF



//	Load the structure and open the account link maintenance window

istr_message.al_doubleparm[1]		 = 0									// payment no
istr_message.al_doubleparm[2]		 = dw_display_claim_info.GetItemNumber(1,"claim_no")
istr_message.al_doubleparm[3]		 = ll_docid
istr_message.al_doubleparm[4] 	 = dw_account_list.GetItemNumber(ll_rownum,"sender")
istr_message.as_stringparm[1] 	 = dw_account_list.GetItemString(ll_rownum,"type")
istr_message.adtm_datetimeparm[1] = dw_account_list.GetItemDateTime(ll_rownum,"date")
istr_message.as_mode					 = "link"

// get the admin act code for paying NBMS FCA docs
SELECT	administering_act_code
INTO		:ls_administering_act_code
FROM		CLAIM
WHERE		claim_no = :istr_message.al_doubleparm[2]
USING SQLCA;
SQLCA.nf_handle_error('w_account_payment', 'dw_account_list', 'rowfocuschanged - select administering_act_code from CLAIM')

// pass to acct link maintenance
istr_message.as_stringparm[2] = ls_administering_act_code

OpenWithParm(iw_account_link_maintenance,istr_message,iw_active_sheet)
IF IsValid(iw_account_link_maintenance) THEN
	iw_account_link_maintenance.wf_set_payment_object(inv_payment)
	iw_account_link_maintenance.wf_set_parent_window(parent)
	wf_screen_reset("update on")
END IF

end event

type dw_enter_claim_no from u_dw_online within w_account_payment
integer x = 1541
integer y = 224
integer width = 366
integer height = 88
integer taborder = 60
string dataobject = "d_enter_claim_no"
boolean border = false
end type

on editchanged;call u_dw_online::editchanged;IF cb_refresh.default = FALSE THEN
	cb_refresh.default = TRUE
END IF
end on

type st_1 from statictext within w_account_payment
integer x = 1280
integer y = 228
integer width = 256
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Claim No:"
alignment alignment = center!
boolean focusrectangle = false
end type

type cbx_imaged_view from checkbox within w_account_payment
integer x = 1541
integer y = 152
integer width = 416
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Imaged View"
boolean checked = true
end type

event clicked;LONG	ll_claim_no,ll_rows

	IF This.checked THEN
		ib_imaged_view = TRUE
		wf_retrieve_doclist() 
		wf_screen_reset("display imaged/nonimaged")
		st_advance_notice.BringToTop = True
	ELSE
		IF dw_display_claim_info.RowCount() > 0 THEN
			ib_imaged_view = FALSE
			ll_claim_no = dw_display_claim_info.GetItemNumber(1,"claim_no")
			dw_account_type_payments_list.Reset()
			ll_rows = dw_account_type_payments_list.Retrieve(ll_claim_no)
			IF ll_rows > 0 THEN
				dw_account_type_payments_list.SetRow( 1 )
				dw_account_type_payments_list.SelectRow( 1 , TRUE )
				dw_account_type_payments_list.TriggerEvent(RowFocusChanged!)
			END IF
			SQLCA.nf_handle_error("dw_account_type_payments_list",'w_account_payments',"clicked for cbx_imaged_view")
			wf_screen_reset("display imaged/nonimaged")
		END IF
	END IF

	wf_screen_reset("after refresh")

end event

type cbx_remove from checkbox within w_account_payment
integer x = 1541
integer y = 88
integer width = 677
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Remove After Payment"
end type

on clicked;	IF This.checked THEN
		ib_remove_after_payment = TRUE
	ELSE
		ib_remove_after_payment = FALSE
	END IF
end on

type cb_refresh from commandbutton within w_account_payment
event ue_update_complete pbm_custom04
event ue_update_cancelled pbm_custom06
integer x = 2121
integer y = 184
integer width = 370
integer height = 100
integer taborder = 70
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Re&fresh"
end type

event ue_update_complete;LONG     ll_rownum, ll_docid,	ll_claim_no, ll_count, ll_paid_count
BOOLEAN  lb_remove_documents
STRING	ls_paid_status_explan

lb_remove_documents = false

wf_screen_reset("update off")
ll_claim_no = dw_display_claim_info.GetItemNumber(1,"claim_no")

IF ib_imaged_view THEN
	ll_rownum = dw_account_list.GetRow()
	IF istr_message.as_mode = "delete" THEN
		dw_account_list.TriggerEvent(rowfocuschanged!)
		IF dw_account_payment_list.RowCount() <= 0 THEN
			dw_account_list.SetItem(ll_rownum,"paid_count",0)
		END IF
		IF ll_rownum > 0 THEN
			cb_refresh.TriggerEvent(Clicked!)
			dw_account_list.ScrollToRow(ll_rownum)
			dw_account_list.SetRow(ll_rownum)
			dw_account_list.SelectRow(ll_rownum, TRUE)
		END IF		
	ELSE
		IF istr_message.as_mode = "link" THEN//***just hook it up to the PAYMENT_DOCUMENT table
			//	Check to see if the claim still has advances
			IF st_advance_notice.Visible = True THEN
				ll_count = 0
								
				SELECT count(*) 
				  INTO :ll_count
              FROM PAYMENT a,PAYMENT_DOCUMENT b
             WHERE a.claim_no   = :ll_claim_no
               AND a.payment_no = b.payment_no
				 USING SQLCA;
				
				SQLCA.nf_handle_error("w_account_payment", "", "cb_refresh - ue_update_complete - SELECT count(*) FROM PAYMENT a, PAYMENT_DOCUMENT b")
				IF ll_count = 0 THEN st_advance_notice.Visible = False
			END IF
		END IF
		/* Set the Paid Symbol on the Screen to indicate something was done with the payment (it uses
			a character set that displays an "O" as anoter symbol
		*/
		// SR193 - NBMS Redesign - Set to 1 to show indicator
		ll_paid_count = dw_account_list.GetItemNumber(ll_rownum,"paid_count")
		dw_account_list.SetItem(ll_rownum,"paid_count",ll_paid_count + 1)
		
	   ls_paid_status_explan = dw_account_list.GetItemString(ll_rownum,"paid_status_explanation_code")

		/*	Check to see if autoremove is true, but don't autoremove if the document was rejected unless
			the calling module is the Inbasket Unpaid Accounts
		*/
		IF is_as_mode <> "inbasketunpaid" THEN 
			IF ib_remove_after_payment and not istr_message.as_mode = "reject" and not istr_message.as_mode = "display" THEN
				lb_remove_documents = true
			END IF
			IF ib_remove_after_payment and (istr_message.as_mode = "display" and ls_paid_status_explan = "16") THEN
				lb_remove_documents = true
			END IF	
		ELSE
			IF ib_remove_after_payment and not istr_message.as_mode = "display" THEN
				lb_remove_documents = true
			END IF
		END IF
		IF lb_remove_documents THEN
			ll_docid = dw_account_list.GetItemNumber(ll_rownum,"docid")
			cb_remove.TriggerEvent(clicked!)
			// IF the remove didn't work successfully, trigger the rowfocuschanged to 
			// clean up the screen
			ll_rownum = dw_account_list.GetRow()
			IF ll_rownum > 0 then
				IF ll_docid = dw_account_list.GetItemNumber(ll_rownum,"docid") THEN
					dw_account_list.TriggerEvent(rowfocuschanged!)
				END IF
			END IF
		ELSE
//			IF ll_rownum < dw_account_list.RowCount() THEN
//				ll_rownum = ll_rownum + 1
//				dw_account_list.ScrollToRow(ll_rownum)
//				dw_account_list.uf_processselect(ll_rownum,"Mouse")
//			ELSE
//				dw_account_list.TriggerEvent(rowfocuschanged!)
			IF ll_rownum > 0 THEN
				cb_refresh.TriggerEvent(Clicked!)
				dw_account_list.ScrollToRow(ll_rownum)
				dw_account_list.SetRow(ll_rownum)
				dw_account_list.SelectRow(ll_rownum, TRUE)
			END IF
		END IF
	END IF
ELSE
	dw_account_type_payments_list.Reset()
	dw_account_type_payments_list.Retrieve(ll_claim_no)
	SQLCA.nf_handle_error("w_account_payment", "", "cb_refresh - ue_update_complete - dw_account_type_payments_list.Retrieve(ll_claim_no)")
END IF
		
istr_message.al_doubleparm[5] = 0
end event

event ue_update_cancelled;LONG 	ll_claim_no, ll_payment_no, ll_rownum,	ll_account_rownum


wf_screen_reset("update off")
IF ib_imaged_view THEN
	dw_account_list.TriggerEvent(rowfocuschanged!)
ELSE
	ll_claim_no = dw_display_claim_info.GetItemNumber(1,"claim_no")
	ll_account_rownum = dw_account_type_payments_list.GetRow()
	IF ll_account_rownum > 0 THEN
		ll_payment_no = dw_account_type_payments_list.GetItemNumber(dw_account_type_payments_list.GetRow(),"payment_no")
	ELSE
		ll_payment_no = 0
	END IF
	dw_account_type_payments_list.Reset()
	dw_account_type_payments_list.Retrieve(ll_claim_no)
	SQLCA.nf_handle_error("w_account_payment", "", "cb_refresh - ue_update_cancelled - dw_account_type_payments_list.Retrieve(ll_claim_no)")
	IF ll_payment_no > 0 THEN
		ll_rownum = dw_account_type_payments_list.Find("payment_no = " + string(ll_payment_no) ,1,dw_account_type_payments_list.RowCount())
		IF ll_rownum > 0 THEN
			dw_account_type_payments_list.ScrollToRow(ll_rownum)
			dw_account_type_payments_list.uf_processselect(ll_rownum,"Mouse")
		END IF
	END IF
END IF

end event

event clicked;LONG    ll_catid, ll_claim_no, ll_docid
INTEGER li_return_status

w_sheet lw_sheet

/*	Validate before making any changes to the screen.  If any problems, display message
	and leave the screen as is
	Ensure that a valid category has been selected
*/
	IF is_as_mode = "inbasketunpaid" THEN
		ll_catid = il_inbasket_catid
	ELSE
		ll_catid = dw_select_taxing_category.GetItemNumber(1,"catid")
		IF IsNull(ll_catid) or ll_catid <= 0 THEN
			MessageBox(is_module_name,"You must select a category before you can retrieve",Exclamation!)
			dw_select_taxing_category.SetFocus()
			Return
		END IF
	END IF

	SetPointer(HourGlass!)

	cbx_imaged_view.enabled = FALSE

/*	If claimsmaster (catid=2) was selected, validate the claim number.  IF the claim does not exist on imaging, 
	ensure that it exists on the Workbench db before allowing the user to continue. Do Not do this validation if 
	the Account Payment was called from the InBasket module.
*/
	IF is_as_mode <> "inbasketunpaid" THEN
		IF ll_catid = 2 THEN
			
			/* make sure we have something in this datawindow */
			IF len(trim(dw_enter_claim_no.gettext())) < 1 THEN
				MessageBox(is_module_name,"You must provide a claim number before you can retrieve",Exclamation!)
				dw_enter_claim_no.SetFocus()
				dw_enter_claim_no.SetColumn("claim_no")
				Return
			END IF
			
			IF dw_enter_claim_no.AcceptText() < 0 THEN
				dw_enter_claim_no.SetFocus()
				dw_enter_claim_no.SetColumn("claim_no")
				Return
			END IF
			
			ll_claim_no = dw_enter_claim_no.GetItemNumber(1,"claim_no")
			IF IsNull(ll_claim_no) or ll_claim_no <= 0 then
				MessageBox(is_module_name,"You must provide a claim number before you can retrieve",Exclamation!)
				dw_enter_claim_no.SetFocus()
				dw_enter_claim_no.SetColumn("claim_no")
				Return
			END IF
			li_return_status = wf_retrieve_claim(ll_claim_no)
			IF li_return_status < 0  THEN
				dw_enter_claim_no.SetFocus()
				dw_enter_claim_no.SetColumn("claim_no")
				Return
			END IF
			IF li_return_status = 0 THEN	
				MessageBox(is_module_name,"The claim number entered does not exist.  You must select a valid claim.",Exclamation!)
				dw_enter_claim_no.SetFocus()
				dw_enter_claim_no.SetColumn("claim_no")
				Return
			END IF
			
		ELSE
			dw_display_claim_info.Reset()
			dw_display_claim_info.InsertRow(0)
		END IF
	END IF
			

/*	Clean up a bit before retrieves
*/
	This.Default = False
	wf_screen_reset("disable all")


	IF is_as_mode = "inbasketunpaid" THEN
		il_catid = il_inbasket_catid
	ELSE
		il_catid = ll_catid
	END IF

	ib_imaged_view = True
	cbx_imaged_view.checked = TRUE
	
	/* this is where the action is in order to get the other screen up the 
	ib_imaged view must be = false */
	IF il_catid = 2 THEN
		
		cbx_remove.enabled = FALSE
		cbx_remove.checked = FALSE
		cbx_imaged_view.enabled = TRUE
		IF dw_display_claim_info.GetItemString(1,"imaged_flag") = "N" THEN
			
			ib_imaged_view = False
			cbx_imaged_view.checked = FALSE
			cbx_imaged_view.enabled = FALSE
		ELSE
			gb_account_payment_list.Text = "Claim: " + dw_display_claim_info.GetItemString(1,"display_claim_info")
			
		END IF
	ELSE
		cbx_remove.enabled = TRUE
		cbx_imaged_view.enabled = FALSE	
	END IF
	
	wf_screen_reset("display imaged/nonimaged")
	
	
/*	Retrieve the appropriate data
*/
	IF ib_imaged_view THEN
		
		Parent.wf_set_redraw(FALSE, 'clicked_cb_refresh')
		IF is_as_mode = "inbasketunpaid" THEN
			wf_apply_doc_list_option("filter",3,"")
		END IF
		IF wf_retrieve_doclist() < 0 THEN
			Parent.wf_set_redraw(TRUE, 'clicked_cb_refresh')
			Return
		END IF
		/*	PR 2994: use the first row to populate in dw_account_list
			to populate tombstone, doclist, acct payment list 
			and the background claim dw (dw_display_claim_info) */
		
		IF dw_account_list.GetRow() > 0 THEN
			dw_account_list.SelectRow( 0 , FALSE )
			dw_account_list.SetRow( 1 )
			dw_account_list.SelectRow( 1, TRUE )
			ll_claim_no = dw_account_list.GetItemNumber(1, 'claim')
			ll_docid = dw_account_list.GetItemNumber(1, 'docid')
			lw_sheet  = Parent.ParentWindow()
			lw_sheet.wf_set_claim(ll_claim_no)
//			Parent.ParentWindow().function dynamic trigger wf_set_claim(ll_claim_no)
			dw_account_payment_list.Retrieve(ll_docid)
			dw_display_claim_info.Retrieve(ll_claim_no)
		END IF
		Parent.wf_set_redraw(TRUE, 'clicked_cb_refresh')
	ELSE
		
		dw_account_type_payments_list.Reset()
		dw_account_type_payments_list.Retrieve(ll_claim_no)
		SQLCA.nf_handle_error("w_account_payment", "", "cb_refresh - dw_account_type_payments_list.Retrieve(ll_claim_no)")
	END IF

/*	Reset the enabled/disabled attributes
*/
	wf_screen_reset("after refresh")

/*	IF we are here by claim, set up the group box header
*/
	IF il_catid = 2 THEN
		IF dw_display_claim_info.GetRow() > 0 THEN
			gb_noaccount_payment_list.Text = "Payments for Claim: " + dw_display_claim_info.GetItemString(1,"display_claim_info")
		END IF
	ELSE
		gb_noaccount_payment_list.Text = ""
		gb_account_payment_list.Text = "Claim: " + dw_display_claim_info.GetItemString(1,"display_claim_info")
	END IF

end event

type dw_document_path from u_dw_document_path within w_account_payment
boolean visible = false
integer x = 155
integer y = 1676
integer taborder = 10
end type

type st_advance_notice from statictext within w_account_payment
boolean visible = false
integer x = 1961
integer y = 1000
integer width = 1070
integer height = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16711680
long backcolor = 67108864
boolean enabled = false
string text = "Claim has advances"
alignment alignment = center!
boolean focusrectangle = false
end type

type st_overpayment from statictext within w_account_payment
boolean visible = false
integer x = 2043
integer y = 308
integer width = 544
integer height = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 255
long backcolor = 67108864
boolean enabled = false
string text = "Overpayment"
alignment alignment = center!
boolean focusrectangle = false
end type

type uo_image_append from u_image_append within w_account_payment
boolean visible = false
integer x = 768
integer y = 1672
integer taborder = 30
boolean bringtotop = true
end type

on uo_image_append.destroy
call u_image_append::destroy
end on

type dw_claimsmaster_ref_count from u_dw_online within w_account_payment
boolean visible = false
integer x = 3195
integer y = 540
integer width = 795
integer height = 360
integer taborder = 80
boolean bringtotop = true
string dataobject = "d_claimsmaster_ref_count"
end type

type cb_add from commandbutton within w_account_payment
integer x = 201
integer y = 1512
integer width = 425
integer height = 100
integer taborder = 170
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Pay Acct..."
end type

event clicked;Long     ll_account_rownum, ll_payment_rownum, ll_docid, ll_service_provider_no, ll_claim_no, ll_cost_alloc_no
Long     ll_num_payments, n, ll_payment_no, ll_num_applied_claim_txn, ll_num_unapplied_claim_txn, ll_wrc_invoice_no
Integer  li_count, li_rtn
String   ls_paid_status_code, ls_type_code, ls_payment_type_code, ls_payment_sub_type_code, ls_carrier_code
String   ls_country_code, ls_prov_state_code, ls_service_provider_type_code, ls_service_provider_sub_type_code
Boolean  lb_claim_reimbursement_correction_already_made, lb_claim_reimbursement_payment_exists, lb_non_allocated_types
Datetime ldt_date_on_document, ldt_epay_implementation_date
Decimal  ld_outstanding_balance 
STRING ls_claim_status_code, ls_claim_status_type_code

ll_account_rownum = dw_account_list.GetRow()
ll_payment_rownum = dw_account_payment_list.GetRow()
IF ll_account_rownum <= 0 THEN
	RETURN
END IF

ll_docid = dw_account_list.GetItemNumber(ll_account_rownum, 'docid')

SELECT Count(*)
  INTO :li_count
  FROM WRC_INVOICE_HEADER b
 WHERE docid = :ll_docid
   AND total_invoice_amount < 0
 USING SQLCA ;

li_rtn = SQLCA.nf_handle_error("w_account_payment", "", "cb_add - SELECT Count(*) FROM WRC_INVOICE_HEADER b")

IF li_count > 0 THEN
	MessageBox('WRC Credit Invoice', 'You cannot pay a credit invoice.')
	RETURN
END IF

// BR 1.16 A WRC Invoice with an outstanding balance greater than zero may be paid if an entered or scheduled payment for the invoice does not exist.
SELECT ISNULL(wrc_invoice_no, 0), ISNULL(outstanding_balance, 0.00) 
  INTO :ll_wrc_invoice_no, :ld_outstanding_balance 
  FROM WRC_INVOICE_HEADER 
 WHERE docid = :ll_docid ;

li_rtn = SQLCA.nf_handle_error("w_account_payment", "", "cb_add - SELECT ISNULL(wrc_invoice_no, 0), ISNULL(outstanding_balance, 0.00) FROM WRC_INVOICE_HEADER WHERE docid = :ll_docid")

IF ll_wrc_invoice_no > 0 AND ld_outstanding_balance <= 0.00 THEN
	Messagebox('Payment not allowed', 'The outstanding balance on WRC Invoice (document # ' + String(ll_docid) + ', wrc invoice # ' +&
				  String(ll_wrc_invoice_no) + ') is ' + String(ld_outstanding_balance, "$#,##0.00") + '.~rNo more payments can be made against this invoice.') 
	RETURN
END IF

istr_message.al_doubleparm[3] = dw_account_list.GetItemNumber(ll_account_rownum, "docid")
ll_service_provider_no = dw_account_list.GetItemNumber(ll_account_rownum, "sender")
istr_message.al_doubleparm[4] = ll_service_provider_no

ls_type_code = dw_account_list.GetItemString(ll_account_rownum, "type")
istr_message.as_stringparm[1] = ls_type_code

istr_message.adtm_datetimeparm[1] = dw_account_list.GetItemDateTime(ll_account_rownum, "date")
ldt_date_on_document = istr_message.adtm_datetimeparm[1]

// If there are payments in the payment datawindow then check to see if the paid_status_code = "E"
If ll_payment_rownum > 0 THEN
	ls_paid_status_code = dw_account_payment_list.GetItemString(ll_payment_rownum, "paid_status_code")
END IF

// Give error when document_type = "Prescription Accounts" and service_provider = 0
IF ls_type_code = "AR" AND ll_service_provider_no = 0 THEN
	MessageBox("Error", "A prescription invoice submitted by a claimant must be rejected and paid via ABCC system.", Exclamation!)
	RETURN
END IF

// Give error when document_type = "Prescription Accounts" and service_provider > 0 and ldt_date_on_document > ldt_epay_implementation_date
ldt_epay_implementation_date = Datetime(Date(ProfileString(vgs_ini_filename, "EPay", "EpayImplementationDate", "")))
IF ls_type_code = "AR" AND ll_service_provider_no > 0 AND ldt_date_on_document >= ldt_epay_implementation_date THEN
	SELECT service_provider_type_code
	INTO :ls_service_provider_type_code
	FROM DOCUMENT_INDEX
	WHERE docid = :ll_docid
	using imagetrans;
	
	imagetrans.nf_handle_error('w_account_payment','cb_add.clicked','SELECT service_provider_type_code')
	
	IF ls_service_provider_type_code = '' Then
		SignalError(-666,'Error occured retrieving service_provider_type_code for document #' + String(ll_docid))
	End if
	
	SELECT country_code, prov_state_code, provider_sub_type_code
	INTO :ls_country_code, :ls_prov_state_code, :ls_service_provider_sub_type_code
	FROM PROVIDER
	WHERE provider_no = :ll_service_provider_no
	and   provider_type_code = :ls_service_provider_type_code;
	
	sqlca.nf_handle_error('w_account_payment','cb_add.clicked','SELECT country_code, prov_state_code')
	
	// IF doc_type = 'AR', the service provider is a Drug Store, the date on document is on or after the Epay implementation 
	// date and the Drug Store is locate outside of the atlantic provinces then allow drug payments.
	IF Not(ls_service_provider_type_code = 'M' and ls_service_provider_sub_type_code = '07') Then
		MessageBox('Error',"Prescription invoices submitted by provider, other than a pharmacy, cannot be paid when the service date is on or after the ABCC Epay implementation date.", Exclamation!)
		RETURN
	END IF
		
	//We know we are dealing with Pharmacy's at this point.
	IF ls_country_code = 'CAN' Then
		CHOOSE CASE ls_prov_state_code
			CASE 'NB','NS','NL','PE'					
				MessageBox("Error", "Prescription invoices submitted by a pharmacy, located in the Atlantic Provinces, cannot be paid when the service date is on or after the ABCC Epay implementation date.", Exclamation!)
				RETURN
		END CHOOSE
	End if
END IF

// Check payments when document type is ARX = Prescription Drugs via ABCC
IF ls_type_code = "ARX" THEN
	lb_claim_reimbursement_correction_already_made = FALSE
	lb_claim_reimbursement_payment_exists = FALSE
	
	ll_num_payments = dw_account_payment_list.RowCount()
	FOR n = 1 TO ll_num_payments
		ls_payment_type_code = dw_account_payment_list.GetItemString(n, "payment_payment_type_code")
		ls_payment_sub_type_code = dw_account_payment_list.GetItemString(n, "payment_payment_sub_type_code")
		ll_payment_no = dw_account_payment_list.GetItemNumber(n, "payment_no")
		
		// 22 = Drugs, RC = ABCC Claim Reimbursement Correction
		IF ls_payment_type_code = "22" AND ls_payment_sub_type_code = "RC" THEN
			lb_claim_reimbursement_correction_already_made = TRUE
		END IF
		
		// 22 = Drugs, BC = Prescription Drugs via ABCC
		IF ls_payment_type_code = "22" AND ls_payment_sub_type_code = "BC" THEN	
			SELECT ISNULL(carrier_code, "") 
			  INTO :ls_carrier_code 
			  FROM PAYMENT_PRESCRIPTION 
			 WHERE payment_no = :ll_payment_no ;
	
			li_rtn = SQLCA.nf_handle_error("w_account_payment", "", "cb_add - SELECT carrier_code FROM PAYMENT_PRESCRIPTION ")
	
			IF ls_carrier_code = "WB" THEN
				lb_claim_reimbursement_payment_exists = TRUE
			END IF
		END IF
	NEXT

	// Display warning if a Claim Reimbursement Correction already exists
	IF lb_claim_reimbursement_correction_already_made = TRUE THEN
		Messagebox("Warning", "A Claim Reimbursement Correction was already made.")
	END IF

	// Display an error message when no associated Claim Reimbusement payment exists
	IF lb_claim_reimbursement_payment_exists = FALSE THEN
		Messagebox("Error", "No Claim Reimbursement payments exist.  A Claim Reimbursement Correction payment " +&
					  "can only be created when a Claim Reimbursemnt payment exists.", Exclamation!)
		RETURN
	END IF

	// If Claim Reimbusement payment exists, check if payment has been transferred, adjusted or cancelled
	IF lb_claim_reimbursement_payment_exists = TRUE THEN
		SELECT COUNT(*) 
		  INTO :ll_num_applied_claim_txn 
		  FROM APPLIED_CLAIM_TXN 
		 WHERE payment_no = :ll_payment_no 
		   AND txn_type_code <> "1" ; 

		li_rtn = SQLCA.nf_handle_error("w_account_payment", "", "cb_add - SELECT COUNT(*) FROM APPLIED_CLAIM_TXN")

		SELECT COUNT(*) 
		  INTO :ll_num_unapplied_claim_txn 
		  FROM UNAPPLIED_CLAIM_TXN 
		 WHERE payment_no = :ll_payment_no 
		   AND txn_type_code <> "1" ; 

		li_rtn = SQLCA.nf_handle_error("w_account_payment", "", "cb_add - SELECT COUNT(*) FROM UNAPPLIED_CLAIM_TXN")

		IF ll_num_applied_claim_txn > 0 OR ll_num_unapplied_claim_txn > 0 THEN
			Messagebox("Error", "Claim Reimbursement Correction cannot be created as the reimbursement payment has been adjusted, canceled or transferred.", Exclamation!)
			RETURN
		END IF
	END IF
END IF

// If the paid_status_code = "E" then display the record instead of creating a new payment when the user hits this button
IF ls_paid_status_code = "E" THEN	
	istr_message.al_doubleparm[1] = dw_account_payment_list.GetItemNumber(ll_payment_rownum, "payment_no")
	istr_message.as_mode = "display"
Else
	IF ls_type_code = "ARX" THEN
		istr_message.al_doubleparm[1] = dw_account_payment_list.GetItemNumber(ll_payment_rownum, "payment_no")
	ELSE
		istr_message.al_doubleparm[1] = 0
	END IF
	istr_message.as_mode = "add"
END IF

// If WRC Invoice and there is already an Entered or Scheduled payment put the next window in display mode
IF ll_wrc_invoice_no > 0 THEN
	SELECT COUNT(*) 
	  INTO :li_count 
	  FROM PAYMENT_DOCUMENT 
	 WHERE doc_id = :ll_docid 
	   AND paid_status_code IN ('E', 'S') ; 

	li_rtn = SQLCA.nf_handle_error("w_account_payment", "", "cb_add - SELECT COUNT(*) FROM PAYMENT_DOCUMENT WHERE docid = :ll_docid AND paid_status_code IN ('E', 'S') ")

	IF li_count > 0 THEN
		istr_message.al_doubleparm[1] = dw_account_payment_list.GetItemNumber(ll_payment_rownum, "payment_no")
		istr_message.as_mode = "display"
	END IF
END IF

/*BR 1.140 : T012790 Account Payments  must not be made on the following Claim Status/Types when the claim is not cost allocated to the All Assessed Non-allocated Claim Cost Account (employer 8000, operation 1)
•	Rejected – Disallowed Claim
•	Rejected – Insufficient Information
•	Claims at Pre-Adjudication 
•	Claims at Adjudication 
*/

ls_claim_status_code = dw_display_claim_info.getItemString(1,'claim_status_code')
ls_claim_status_type_code = dw_display_claim_info.getItemString(1,'claim_status_type_code')
ll_cost_alloc_no = dw_display_claim_info.getItemNumber(1,'cost_alloc_no')

IF ls_claim_status_code = 'P'  OR      ls_claim_status_code = 'J' OR   &  	
   (ls_claim_status_code = 'R'  AND  (ls_claim_status_type_code = '07' OR ls_claim_status_type_code =  '18') )  THEN
	IF ll_cost_alloc_no <> 8000 THEN
		MESSAGEBOX("Invalid Cost Allocation Number",  "Payments cannot be made on claims with a status of Pre-Adjudication, Adjudication, Rejected – Claim Disallowed, or Rejected – Insufficient Information if the claim is not allocated to Cost Allocation No. 8000.", Information!)                                                                   
		RETURN
	END IF
	
	//1.150	Document types being paid on claims with a status of Adjudication, PreAdjudication, Rejected – Claim  Disallowed, and Rejected – Insufficient information 
	//         must be limited to the following: AC, AD, AH, AI, AP, AT, MPC, MPD, SDC, and SDD
	IF  inv_payment.nf_is_valid_doctype_act_pymt(ls_type_code)  = FALSE THEN 
		MESSAGEBOX("Invlaid Document Type",  "Only document types of 'AC', 'AD', 'AH','AI', 'AP','AT','MPC', 'MPD','SDC', and 'SDD' can have a payment made for claims with this status.", Information!)
		RETURN
	END IF
END IF

istr_message.as_stringparm[2] = ""

wf_open_maintenance()

end event

type dw_account_list from u_dw_online within w_account_payment
integer x = 27
integer y = 372
integer width = 3095
integer height = 492
integer taborder = 90
string dataobject = "d_account_list"
boolean vscrollbar = true
boolean livescroll = false
borderstyle borderstyle = stylelowered!
end type

event rowfocuschanged;call super::rowfocuschanged;Long     ll_rownum, ll_docid,	ll_claim_no, ll_rowcount, ll_cntr, ll_account_payment_list_rownum,ll_docid2
Long     ll_wrc_invoice_no, ll_service_provider_no
String   ls_folder_list, ls_type_code, ls_auto_created_flag, ls_subtype_code
String   ls_administering_act_code, ls_error_flag, ls_treatment_fee_flag, ls_report_fee_flag
Integer  li_return_value, li_rtn, li_rows
Boolean  lb_payment_allowed = TRUE, lb_link_allowed = TRUE, lb_send_allowed = TRUE, lb_test
Datetime ldt_date_on_document, ldt_epay_implementation_date

//	Get the document id of the selected document
SetPointer(HourGlass!)
dw_account_payment_list.Reset()

ll_rownum = GetRow()
IF ll_rownum <= 0 THEN
	wf_screen_reset("account list")
	dw_account_payment_list.Retrieve(0)	// To clear the display
	SQLCA.nf_handle_error("w_account_payment", "dw_account_list", "rowfocuschanged - dw_account_payment_list.Retrieve(0)") 
	IF NOT il_catid = 2 THEN
		dw_display_claim_info.Reset()
		dw_display_claim_info.InsertRow(0)
		gb_account_payment_list.Text = ""
	END IF
	RETURN
ELSE
	uf_ProcessSelect(ll_rownum,"Mouse")
END IF

ls_type_code = this.GetItemString(ll_rownum,"type")

//	Verify that the document was properly indexed - If there is no docid, there is no reason to continue
// through the code as we can't do anything anyway.
ll_docid = dw_account_list.GetItemNumber(ll_rownum,"docid")
IF IsNull(ll_docid)  THEN
	MessageBox(is_module_name, "Document not indexed ... Please fix before making payment",Exclamation!)
	lb_payment_allowed = FALSE
	lb_link_allowed = FALSE
	IF NOT il_catid = 2 THEN
		dw_display_claim_info.Reset()
		dw_display_claim_info.InsertRow(0)
		gb_account_payment_list.Text = ""
	END IF
	GOTO Common_Exit
END IF

// Don't allow payments to be linked to the wrc invoices	
SELECT wrc_invoice_no
  INTO :ll_wrc_invoice_no
  FROM WRC_INVOICE_HEADER
 WHERE docid = :ll_docid
 USING SQLCA ;

SQLCA.nf_handle_error('w_account_payment', 'dw_account_list', 'rowfocuschanged - select wrc_invoice_no FROM WRC_INVOICE_HEADER')

IF ll_wrc_invoice_no > 0 THEN
	lb_link_allowed = FALSE
END IF

// Retrieve payment and claim information.  Retrieve any existing payments for this document 
dw_account_payment_list.retrieve(ll_docid)
SQLCA.nf_handle_error("w_account_payment", "dw_account_list", "rowfocuschanged - dw_account_payment_list.retrieve(ll_docid)") 
	
//	If we're not in the master folder, check to see if we have a new claim number.
//	If so, retrieve the claim information, check the claim status and clean up accordingly
ll_claim_no = dw_account_list.GetItemNumber(ll_rownum,"claim")
IF IsNull(ll_claim_no) THEN ll_claim_no = 0

IF NOT il_catid = 2 THEN
	li_return_value = wf_retrieve_claim(ll_claim_no)
	IF li_return_value = 0 THEN
		MessageBox(is_module_name,"Error Validating Claim Number. Please verify indexing on this document",Exclamation!)
		lb_payment_allowed = FALSE
		lb_link_allowed = FALSE
		cb_send.enabled = FALSE
		GOTO Common_Exit
	END IF
	IF li_return_value < 1 THEN
		gb_account_payment_list.Text = ""
		lb_payment_allowed = FALSE
		lb_link_allowed = FALSE
	ELSE
		IF NOT inv_payment.nf_is_valid_status_act_pymt() THEN
			MessageBox(is_module_name,"Claim status does not permit account payments",Exclamation!)
			lb_payment_allowed = FALSE
			lb_link_allowed = FALSE
		END IF
		
		//test for allowable doctypes to be paid on rejected, adjudication and preadjudication claims
		IF NOT inv_payment.nf_is_valid_doctype_act_pymt(ls_type_code ) THEN
			lb_payment_allowed = FALSE
			lb_link_allowed = FALSE
		END IF
		gb_account_payment_list.Text = "Claim: " + dw_display_claim_info.GetItemString(1,"display_claim_info")
	END IF
ELSE
	IF NOT inv_payment.nf_is_valid_status_act_pymt() THEN
		lb_payment_allowed = FALSE
		lb_link_allowed = FALSE
	END IF
	
	//test for allowable doctypes to be paid on rejected, adjudication and preadjudication claims
	IF NOT inv_payment.nf_is_valid_doctype_act_pymt(ls_type_code ) THEN
		lb_payment_allowed = FALSE
		lb_link_allowed = FALSE
	END IF
	
END IF

// get the admin act code for paying NBMS FCA docs
SELECT	administering_act_code
INTO		:ls_administering_act_code
FROM		CLAIM
WHERE		claim_no = :ll_claim_no
USING SQLCA;
SQLCA.nf_handle_error('w_account_payment', 'dw_account_list', 'rowfocuschanged - select administering_act_code from CLAIM')

// pass to acct payment maintenance
istr_message.as_stringparm[3] = ls_administering_act_code


//	Do validations on the document
//	Ensure that the document is an account
IF NOT wf_IsAccount(ls_type_code) THEN
	lb_payment_allowed = FALSE
	lb_link_allowed = FALSE
ELSE
	// it is an account document
	IF ls_administering_act_code = 'FCA' THEN
		IF IsValid(ids_admin_act_document_restriction) THEN
		ELSE
			ids_admin_act_document_restriction = Create U_DS
			ids_admin_act_document_restriction.DataObject = 'ds_admin_act_document_restriction'
			ids_admin_act_document_restriction.SetTransObject(SQLCA)
		END IF
		li_rows = ids_admin_act_document_restriction.Retrieve(ls_administering_act_code,ls_type_code)
		SQLCA.nf_handle_error('w_account_payment', 'ids_admin_act_document_restriction', 'retrieve')
		
		IF li_rows = 1 THEN
			ls_error_flag = ids_admin_act_document_restriction.GetItemString(1,'error_flag')
			ls_treatment_fee_flag = ids_admin_act_document_restriction.GetItemString(1,'treatment_fee_flag')
			ls_report_fee_flag = ids_admin_act_document_restriction.GetItemString(1,'report_fee_flag')			
		ELSE
			// no rows or this would be a primary key violation
			// no rows indicates no restriction, i.e., the doc can be paid for the FCA claim
		END IF
		
		IF ls_error_flag = 'Y' AND ls_treatment_fee_flag = 'Y' AND ls_report_fee_flag = 'Y' THEN
			lb_payment_allowed = FALSE
			lb_link_allowed = FALSE
		END IF
	END IF
END IF

//	Check to ensure data integrity of document (ie. It must exist in only one master folder (it's own))
ll_rowcount = dw_claimsmaster_ref_count.Retrieve(ll_docid)
SQLCA.nf_handle_error("w_account_payment", "dw_account_list", "rowfocuschanged - dw_claimsmaster_ref_count.Retrieve(ll_docid)")
	
IF ll_rowcount <= 0 THEN
	MessageBox(is_module_name,"This document does not exist in a master folder...Please call the helpdesk",Exclamation!)
	lb_payment_allowed = FALSE
	lb_link_allowed = FALSE
ELSEIF ll_rowcount > 1 THEN
	ll_cntr = ll_rowcount
	DO WHILE ll_cntr <= ll_rowcount
		ls_folder_list = "Claim Number: " + string(dw_claimsmaster_ref_count.GetItemNumber(ll_cntr,"claimsmaster_claim")) + "~r~n"
		ll_cntr ++
	LOOP
	MessageBox(is_module_name,"This document exists in the following master folders:~r~n" + &
					ls_folder_list + "Please fix before making payment",Exclamation!)
	lb_payment_allowed = FALSE
	lb_link_allowed = FALSE
ELSE
	IF NOT dw_claimsmaster_ref_count.GetItemNumber(1,"claimsmaster_claim") = ll_claim_no THEN
		MessageBox(is_module_name,"This document is in the wrong master folder. ~r~n" + &
						"The claim number on the index form is " + string(ll_claim_no) + ".~r~n" + &
						"But it resides in the master folder for claim number " + string(dw_claimsmaster_ref_count.GetItemNumber(1,"claimsmaster_claim")) + ".~r~n" +&
						"Do not attempt to send this document (document #" + String(ll_docid) + " ). "+ &
						"Please fix before making payment and call the help desk immediately.",Exclamation!)
		lb_send_allowed = FALSE
		lb_payment_allowed = FALSE
		lb_link_allowed = FALSE
		ll_docid2 = dw_account_list.GetItemNumber(ll_rownum,"ref_docid")
	END IF
END IF

//	If there is already an entry of type "O" or "R" or "S", don't allow the user to add another entry
// SR193 NBMS REdesign - Note, paid_count excludes payment status of "entered" in its total
IF This.GetItemNumber(ll_rownum,"paid_count") > 0 THEN
	ll_rowcount = dw_account_payment_list.RowCount()
	IF ll_rowcount > 0 THEN
		This.SetItem(ll_rownum,"paid_count",dw_account_payment_list.RowCount())
		ll_account_payment_list_rownum = dw_account_payment_list.Find("paid_status_code='R'",1,ll_rowcount)
		IF ll_account_payment_list_rownum > 0 THEN
			lb_payment_allowed = FALSE
			lb_link_allowed = FALSE
		END IF
		ll_account_payment_list_rownum = dw_account_payment_list.Find("paid_status_code='O'",1,ll_rowcount)
		IF ll_account_payment_list_rownum > 0 THEN
			lb_payment_allowed = FALSE
			lb_link_allowed = FALSE
		END IF
		ll_account_payment_list_rownum = dw_account_payment_list.Find("paid_status_code='H'",1,ll_rowcount)
		IF ll_account_payment_list_rownum > 0 THEN
			lb_payment_allowed = FALSE
			lb_link_allowed = FALSE
		END IF
	ELSE
		This.SetItem(ll_rownum,"paid_count",0)
	END IF
END IF

//// Disallow payment if service_provider > 0 and date_on_document > Epay Implementation date
//ldt_epay_implementation_date = Datetime(Date(ProfileString(vgs_ini_filename, "EPay", "EpayImplementationDate", "")))
//ldt_date_on_document = This.GetItemDatetime(ll_rownum, "date")
//ll_service_provider_no = This.GetItemNumber(ll_rownum, "sender")
//IF ll_service_provider_no > 0 AND ldt_date_on_document > ldt_epay_implementation_date THEN
//	lb_payment_allowed = FALSE
//	lb_link_allowed = FALSE
//END IF

// Disable Reject Acct... & Link buttons if document created automatically
ls_type_code 		= This.GetItemString(ll_rownum, "type")
ls_subtype_code	= This.GetItemString(ll_rownum, "doc_subtype_code")

SELECT auto_created_flag 
  INTO :ls_auto_created_flag
  FROM Document_Type_Code
 WHERE type_code = :ls_type_code 
 USING imagetrans ;

li_rtn = imagetrans.nf_handle_error("w_account_payment", "dw_account_list", "rowfocuschanged - SELECT auto_created_flag FROM Document_Type_Code")

IF ls_auto_created_flag = "Y" THEN
	cb_reject.Enabled = FALSE
	lb_link_allowed = FALSE
END IF

// Disable Link button when it's AR type and date on document >= Epay Implementation date
ldt_epay_implementation_date = Datetime(Date(ProfileString(vgs_ini_filename, "EPay", "EpayImplementationDate", "")))
ldt_date_on_document = This.GetItemDatetime(ll_rownum, "date")

IF ls_type_code = "AR" AND ldt_date_on_document >= ldt_epay_implementation_date THEN
	lb_link_allowed = FALSE
END IF

Common_Exit:
	wf_screen_reset("account list")

	IF lb_payment_allowed THEN
		cb_add.Enabled 				= TRUE	
		cb_pay_ephysio.Enabled 	= TRUE
		cb_authorization.enabled = TRUE
		IF dw_account_payment_list.rowcount() > 0 THEN
			cb_reject.Enabled = FALSE
		ELSE
			cb_reject.Enabled = TRUE
		END IF
		
//		/* check for the payment type */
//		IF ls_type_code = 'AC' AND ls_subtype_code = 'A2'  THEN 
//			cb_pay_ephysio.Enabled 	= TRUE
//		ELSE
//			cb_pay_ephysio.Enabled 	= FALSE
//		END IF 
		
	ELSE
		cb_add.Enabled 				= FALSE
		cb_reject.Enabled 				= FALSE
		cb_authorization.enabled 	= FALSE
		cb_pay_ephysio.Enabled 	= FALSE
	END IF

	IF lb_link_allowed THEN
		cb_link.Enabled = TRUE
	ELSE
		cb_link.Enabled = FALSE
	END IF

	IF lb_send_allowed THEN
		cb_send.Enabled = TRUE
	ELSE
		cb_send.Enabled = FALSE
	END IF

// reset the authorization number in case the user had selected one previously
	istr_message.al_doubleparm[5] = 0
	RETURN
end event

event ue_filter;call super::ue_filter;LONG					ll_row
S_WINDOW_MESSAGE	lstr_message

	Open(w_filter_account_pay_list)
	lstr_message = Message.PowerObjectParm	
	
	IF lstr_message.as_stringparm[1] <> 'Cancel' THEN
		This.SetFilter(lstr_message.as_stringparm[1])
		This.Filter()

		gb_account_list.Text = "Document View: " + lstr_message.as_stringparm[2] 
		This.TriggerEvent(RowFocusChanged!)
		ll_row = This.GetRow()
		IF ll_row > 0 THEN
			This.uf_ProcessSelect(ll_row,"Mouse")
		END IF
	END IF
	
	This.SetFocus( )
end event

on ue_more_details;call u_dw_online::ue_more_details;LONG		ll_rownum, ll_docid        
STRING	ls_document

	ll_rownum = This.GetRow()
	IF ll_rownum = 0 THEN
		MessageBox("Document","Could not determine selected document. Please try again.")
		Return
	END IF

	ll_docid = This.GetItemNumber(ll_rownum,"docid")

	OpenWithParm(w_memotab,ll_docid, Parent)

end on

on rbuttondown;M_DW_ONLINE_RMB_POPUP lm_popup
/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/
	lm_popup = Create m_dw_online_rmb_popup
	lm_popup.mf_set_datawindow(This)
	lm_popup.m_options.m_sort.visible = TRUE
	lm_popup.m_options.m_moredetails.visible = TRUE
	lm_popup.m_options.m_filterlist.visible = TRUE
	lm_popup.m_options.PopMenu(w_frame.PointerX( ), w_frame.PointerY( ))

	Destroy lm_popup
end on

on doubleclicked;call u_dw_online::doubleclicked;cb_view.TriggerEvent(clicked!)
end on

event clicked;call super::clicked;LONG ll_row

ll_row = THIS.GetRow()

IF ll_row = 1 THEN
	THIS.SetRow(1)
	THIS.SelectRow(1,TRUE)
END IF

end event

type gb_account_list from groupbox within w_account_payment
integer x = 9
integer y = 312
integer width = 3150
integer height = 684
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Document View: All Documents / by Document Id"
end type

type dw_account_payment_list from u_dw_online within w_account_payment
integer x = 27
integer y = 1056
integer width = 3095
integer height = 440
integer taborder = 150
boolean bringtotop = true
string dataobject = "d_account_payment_list"
boolean vscrollbar = true
boolean livescroll = false
borderstyle borderstyle = stylelowered!
end type

event doubleclicked;call super::doubleclicked;Long       ll_account_rownum, ll_payment_rownum, ll_docid, ll_wrc_invoice_no 
Integer    li_rtn, li_count 
String     ls_payment_type_code, ls_payment_sub_type_code 
Decimal{2} ld_outstanding_balance 
Datetime ldtm_today
ldtm_today = f_server_datetime()

s_window_message lstr_message

//	Load the structure and open the payment maintenance window
ll_account_rownum = dw_account_list.GetRow()
ll_payment_rownum = This.GetRow()
IF ll_account_rownum <= 0 OR ll_payment_rownum <= 0 THEN
	RETURN
END IF

istr_message.al_doubleparm[1] = This.GetItemNumber(ll_payment_rownum, "payment_no")
istr_message.al_doubleparm[3] = This.GetItemNumber(ll_payment_rownum, "doc_id")
istr_message.al_doubleparm[4] = dw_account_list.GetItemNumber(ll_account_rownum, "sender")
istr_message.as_stringparm[1] = dw_account_list.GetItemString(ll_account_rownum, "type")
istr_message.adtm_datetimeparm[1] = dw_account_list.GetItemDatetime(ll_account_rownum, "date")
istr_message.as_mode = "display"
istr_message.as_stringparm[2] = ''

ll_docid = This.GetItemNumber(ll_payment_rownum, "doc_id") 

SELECT Count(*)
  INTO :li_count
  FROM WRC_INVOICE_HEADER b
 WHERE docid = :ll_docid
   AND total_invoice_amount < 0
 USING SQLCA ; 

li_rtn = SQLCA.nf_handle_error("w_account_payment", "dw_account_payment_list", "doubleclicked - SELECT Count(*) FROM WRC_INVOICE_HEADER b")

IF li_count > 0 THEN
	MessageBox('WRC Credit Invoice', 'You cannot pay a credit invoice.')
	RETURN
END IF

// BR 1.16 A WRC Invoice with an outstanding balance greater than zero may be paid if an entered or scheduled payment for the invoice does not exist.
SELECT ISNULL(wrc_invoice_no, 0), ISNULL(outstanding_balance, 0.00) 
  INTO :ll_wrc_invoice_no, :ld_outstanding_balance 
  FROM WRC_INVOICE_HEADER 
 WHERE docid = :ll_docid ;

li_rtn = SQLCA.nf_handle_error("w_account_payment", "dw_account_payment_list", "doubleclicked - SELECT ISNULL(wrc_invoice_no, 0), ISNULL(outstanding_balance, 0.00) FROM WRC_INVOICE_HEADER WHERE docid = :ll_docid")

// The user should also get a message if the outstanding balance on the wrc invoice <= 0
IF ll_wrc_invoice_no > 0 AND ld_outstanding_balance <= 0.00 THEN
	Messagebox('Payment not allowed', 'The outstanding balance on WRC Invoice (document # ' + String(ll_docid) + ', wrc invoice # ' +&
				  String(ll_wrc_invoice_no) + ') is ' + String(ld_outstanding_balance, "$#,##0.00") + '.~rNo more payments can be made against this invoice.') 
	RETURN
END IF

wf_open_maintenance()

IF dw_account_payment_list.GetItemString(dw_account_payment_list.getrow(), "payment_payment_type_code") = "23" and Date(ldtm_today) >= Date(gdtm_ephysio_implementation_date) THEN
	lstr_message.al_doubleparm[1] = istr_message.al_doubleparm[2] // claim_no
	lstr_message.al_doubleparm[2] = istr_message.al_doubleparm[1]  // payment_no
	OpenWithParm (w_travel_expense,lstr_message)
END IF


end event

event rowfocuschanged;call super::rowfocuschanged;Long       ll_rownum, ll_wrc_invoice_no, ll_docid
Integer    li_rtn 
Decimal{2} ld_outstanding_balance 
String     ls_paid_status_code, ls_paid_status_explanation_code, ls_payment_type_code, ls_payment_sub_type_code

ll_rownum = This.GetRow()

IF ll_rownum > 0 THEN
	uf_processselect(ll_rownum, "Mouse")

	// Disable Delete button if paid_status = Paid and explanation_code = "From WRC Invoicing System"
	ls_paid_status_code = This.GetItemString(ll_rownum, "paid_status_code")
	ls_paid_status_explanation_code = This.GetItemString(ll_rownum, "paid_status_explanation")
	IF ls_paid_status_code  = "P" OR ls_paid_status_explanation_code = '16' THEN
		cb_delete.Enabled = FALSE
	ELSE
		cb_delete.Enabled = TRUE
	END IF

	// BR 1.14 An entered or scheduled payment for a WRC Invoice may be deleted if the outstanding balance on the WRC Invoice is less than or equal to zero.	
	ll_docid = This.GetItemNumber(ll_rownum, "doc_id")

	SELECT ISNULL(wrc_invoice_no, 0), ISNULL(outstanding_balance, 0.00) 
	  INTO :ll_wrc_invoice_no, :ld_outstanding_balance 
	  FROM WRC_INVOICE_HEADER 
	 WHERE docid = :ll_docid 
	 USING SQLCA ; 

	li_rtn = SQLCA.nf_handle_error('w_account_payment', 'dw_account_payment_list', 'rowfocuschanged - select wrc_invoice_no FROM WRC_INVOICE_HEADER')

	IF ll_wrc_invoice_no > 0 AND (ls_paid_status_code = "E" OR ls_paid_status_code = "S") AND ls_paid_status_explanation_code = '16' AND ld_outstanding_balance <= 0.00 THEN
		cb_delete.Enabled = TRUE
	END IF

	// Disable Delete button when payment_type = "Drugs" and payment_sub_type = "ABCC Prescript. Payment or Adj."
	ls_payment_type_code = This.GetItemString(ll_rownum, "payment_payment_type_code")
	ls_payment_sub_type_code = This.GetItemString(ll_rownum, "payment_payment_sub_type_code")
	IF ls_payment_type_code  = "22" AND ls_payment_sub_type_code = 'BC' THEN
		cb_delete.Enabled = FALSE
	END IF

	// Disable Authorization button when payment_type = "Drugs" and payment_sub_type = "BC" or "RC"
	IF ls_payment_type_code  = "22" AND (ls_payment_sub_type_code = 'BC' OR ls_payment_sub_type_code = 'RC') THEN
		cb_authorization.Enabled = FALSE
	END IF

	cb_reject.Enabled = FALSE
ELSE
	cb_delete.Enabled = FALSE
	cb_reject.Enabled = TRUE
END IF

end event

type cb_reject from commandbutton within w_account_payment
integer x = 1051
integer y = 1512
integer width = 425
integer height = 100
integer taborder = 180
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Reject Acct..."
end type

event clicked;LONG  ll_rowcount, ll_cntr, ll_rec_count,	ll_docid, ll_payment_no, ll_account_rownum

//	Check to ensure that all payments for this document have been cancelled.
//	We didn't do this in rowfocuschanged, since we want the user to know why
//	they can't reject and we didn't want to give them the message unless they
//	actually tried to reject

	ll_rowcount = dw_account_payment_list.RowCount()
	
	IF ll_rowcount > 0 THEN
		ll_cntr = 1
		DO WHILE ll_cntr <= ll_rowcount
			ll_docid		= dw_account_payment_list.GetItemNumber(ll_cntr,"doc_id")
			ll_payment_no = dw_account_payment_list.GetItemNumber(ll_cntr,"payment_no")
			IF IsNull(ll_payment_no) THEN
				MessageBox(is_module_name + " - Data Integrity Error","Payment Number not found for document # " + string(ll_docid) + &
								"~r~nReject not allowed.  Please call the help desk",Exclamation!)
				Return
			END IF

			SELECT 	count(*) 
			INTO		:ll_rec_count
			FROM		APPLIED_CLAIM_TXN 
			WHERE		payment_no = :ll_payment_no and 
						(canceled_txn_flag = "N" and
						 related_txn_no = 0) using SQLCA;

			IF ll_rec_count > 0 THEN
				MessageBox(is_module_name,"You must cancel all payments for this document before you can reject the document",Exclamation!)
				Return
			END IF
			ll_cntr ++
		LOOP
	END IF

//	Load the structure and open the payment maintenance window

	ll_account_rownum = dw_account_list.GetRow()

	istr_message.al_doubleparm[1]		= 0
	istr_message.al_doubleparm[3] 	= dw_account_list.GetItemNumber(ll_account_rownum,"docid")
	istr_message.al_doubleparm[4] 	= dw_account_list.GetItemNumber(ll_account_rownum,"sender")
	istr_message.as_stringparm[1]		= dw_account_list.GetItemString(ll_account_rownum,"type")
	istr_message.adtm_datetimeparm[1]	= dw_account_list.GetItemDateTime(ll_account_rownum,"date")
	istr_message.as_mode					= "reject"

	wf_open_maintenance()
end event

type cb_authorization from commandbutton within w_account_payment
integer x = 2327
integer y = 1512
integer width = 425
integer height = 100
integer taborder = 200
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Authorization"
end type

event clicked;S_WINDOW_MESSAGE	lstr_message


	lstr_message.al_doubleparm[1] = dw_display_claim_info.GetItemNumber(1,"claim_no")
	OpenWithParm(w_account_authorizations,lstr_message)
	
	lstr_message = Message.PowerObjectParm
	
	IF lstr_message.as_mode = 'OK' THEN
		istr_message.al_doubleparm[5] = lstr_message.al_doubleparm[1]
	ELSE
		istr_message.al_doubleparm[5] = 0
	END IF
end event

type cb_advance from commandbutton within w_account_payment
boolean visible = false
integer x = 201
integer y = 1512
integer width = 425
integer height = 100
integer taborder = 220
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Ad&vance"
end type

event clicked;LONG hold_value

//	Load the structure and open the payment maintenance window

	istr_message.al_doubleparm[1] = 0 
	istr_message.al_doubleparm[3] = 0
	istr_message.al_doubleparm[4] = 0
	istr_message.as_stringparm[1] = ""
/*	istr_message.al_doubleparm[5] = 0 */

	SetNull(istr_message.adtm_datetimeparm[1])
	istr_message.as_mode = "advance"
	istr_message.as_stringparm[2] = "ADVANCE"

	wf_open_maintenance()
end event

type gb_noaccount_payment_list from groupbox within w_account_payment
boolean visible = false
integer x = 14
integer y = 408
integer width = 3145
integer height = 1236
integer taborder = 230
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
end type

type dw_account_type_payments_list from u_dw_online within w_account_payment
boolean visible = false
integer x = 192
integer y = 480
integer width = 2679
integer height = 1008
integer taborder = 210
string dataobject = "d_account_type_payments_list"
boolean vscrollbar = true
end type

event ue_more_details;call super::ue_more_details;LONG		ll_rownum, ll_docid        
STRING	ls_document

	ll_rownum = This.GetRow()
	IF ll_rownum <= 0 THEN
		MessageBox("Document","Could not determine selected document. Please try again.")
		Return
	END IF

	ll_docid = This.GetItemNumber(ll_rownum,"payment_document_doc_id")

	OpenWithParm(w_memotab,ll_docid, Parent)

end event

event doubleclicked;call super::doubleclicked;LONG		ll_rownum, ll_docid
DATETIME	ldtm_service_date


//	Load the structure and open the payment maintenance window

	ll_rownum = this.GetRow()
	IF ll_rownum <= 0 THEN
		Return
	END IF
	istr_message.al_doubleparm[5] = 0

	ll_docid = this.GetItemNumber(ll_rownum,"payment_document_doc_id")
	IF not IsNull(ll_docid) THEN
		istr_message.al_doubleparm[3] = ll_docid

		SetNull(istr_message.al_doubleparm[4])

		SELECT 	service_provider_no, type_code, date_on_document
		INTO		:istr_message.al_doubleparm[4], :istr_message.as_stringparm[1], :ldtm_service_date
		FROM		DOCUMENT_INDEX
		WHERE		docid = :ll_docid using ImageTrans;

		IF SQLCA.nf_handle_error("Embedded SQL: Retrieve from docindex",is_module_name,"doubleclicked for dw_account_type_payments_list") < 0 THEN
			Return
		END IF

		IF IsNull(istr_message.al_doubleparm[4]) THEN
			MessageBox(is_module_name + " - Data Integrity Error","Error reading document index information for document # " + string(ll_docid) + &
							"~r~nPlease call the help desk!",Exclamation!)
			Return
		END IF
	ELSE
		istr_message.al_doubleparm[3] = 0
		istr_message.al_doubleparm[4] = 0
		istr_message.as_stringparm[1] = " "
		SetNull(ldtm_service_date)
	END IF
	
	istr_message.adtm_datetimeparm[1] = ldtm_service_date
	istr_message.al_doubleparm[1] = this.GetItemNumber(ll_rownum,"payment_no")
	istr_message.al_doubleparm[6] = this.GetItemNumber(ll_rownum,"rehab_invoice_no")
	istr_message.as_mode = "display"
	istr_message.as_stringparm[2] = "ADVANCE"
	wf_open_maintenance()
end event

event rowfocuschanged;call super::rowfocuschanged;LONG 		ll_rownum, ll_payment_no, ll_docid_no, ll_rehab_invoice_no
STRING 	ls_paid_status_code, ls_paid_status_explanation_code

ll_rownum = This.GetRow()

cb_delete_ephysio.enabled 	= FALSE
cb_delete_ephysio.visible 		= FALSE

IF ll_rownum > 0 THEN
	
	ls_paid_status_code = This.GetItemString(ll_rownum,"payment_document_paid_status_code")
	// PR 3240 - Note that payment_document_paid_status_explanation = 16 only for WRC Invoices
	ls_paid_status_explanation_code = This.GetItemString(ll_rownum,"payment_document_paid_status_explanation")
	IF ls_paid_status_code  = "P" OR ls_paid_status_explanation_code = '16' &
		OR not IsNull(this.GetItemDateTime(ll_rownum,"processed_date"))THEN
		cb_delete.Enabled = False
	ELSE
		cb_delete.Enabled = True
	END IF


ll_rehab_invoice_no = this.getitemnumber(ll_rownum, "rehab_invoice_no")
	// a payment associated with a rehab invoice line item can be deleted - under certain conditions.
	IF NOT ISNULL(this.getitemnumber(ll_rownum, "rehab_invoice_no")) AND  (ls_paid_status_code <> 'P' OR isnull(ls_paid_status_code)) THEN
		//check if it has unprocessed web based line items that can be deleted
		cb_delete.enabled 				= False
		
		ll_payment_no 	= THIS.getitemnumber(ll_rownum, "payment_no")
		ll_docid_no 		= THIS.getitemnumber(ll_rownum, "payment_document_doc_id")
		
		IF wf_can_line_item_be_deleted(ll_payment_no, ll_docid_no) = TRUE THEN 
			cb_delete_ephysio.enabled 	= TRUE
			cb_delete_ephysio.visible 		= TRUE
		END IF 
		
	END IF	
ELSE
	cb_delete.Enabled = False
END IF

end event

event rbuttondown;M_DW_ONLINE_RMB_POPUP lm_popup
/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/
	lm_popup = Create m_dw_online_rmb_popup
	lm_popup.mf_set_datawindow(This)
	lm_popup.m_options.m_sort.visible = TRUE
	lm_popup.m_options.m_moredetails.visible = TRUE
	lm_popup.m_options.PopMenu(w_frame.PointerX( ), w_frame.PointerY( ))

	Destroy lm_popup
end event

type dw_display_claim_info from u_dw_online within w_account_payment
boolean visible = false
integer x = 174
integer y = 640
integer width = 2871
integer height = 644
integer taborder = 140
string dataobject = "d_basic_claim_everything"
end type

type cb_pay_ephysio from commandbutton within w_account_payment
integer x = 626
integer y = 1512
integer width = 425
integer height = 100
integer taborder = 180
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Pay P&hysio..."
end type

event clicked;/*
Unlike the Account Payment module, this module will require that the claim have a rehab authorization for the billable item that is being submitted. 
The other main difference between this module and the Account Payment module is that the payment clerk will pay 
each invoice line item rather than the total of the submitted invoice. 

In order to reimburse the recipient,  the payment clerk will work from an imaged invoice or receipt that was submitted to WorkSafeNB and then
scanned & indexed to a claim. Since the Account Payment module already provides this ability to view a imaged invoice/receipt from an inbasket or 
from the claim master folder, the Account Payment module will be used as the access point into the new Rehab Invoice window.

In the Account Payment module, the payment clerk will select the document(s) in one of two ways:
Claim:		this displays all of the scanned account payable invoices for the selected claim	
In basket:	this displays all of the scanned account payable invoices for the selected In Basket
	
Once the document is selected, an image of the document is opened and the payment clerk must determine if the document is to be paid through the 
Account Payment payment details window or through the rehab invoice window. A new filter on the document image datawindow will assist the user 
by filtering the list to include only Physio Clinic Accounts (i.e. document type ‘AC’ document subtype ‘A1’)

If the document is a reimbursement for physio-related sercices and supplies, the payment clerk must click the rehab Invoice button to pay the invoice . 
Note: new business rules have been added to the Account Payment payment details window to prevent the payment clerk from reimbursing the 
injured worker or a provider for physio-related services and supplies through the account payment ‘payment’ window.

The claim that received the service or supply will be derived from the document indexing data for the image that is being viewed
(i.e. DOCUMENT_INDEX.claim_no).

The provider that provided the physio service may be derived from the document indexing data for the image that is being viewed, 
if this information was entered at the time that the document was indexed 
(i.e. DOCUMENT_INDEX.service_provider_type_code & DOCUMENT_INDEX.service_provider_no).
*/

LONG						ll_claim_no, ll_account_rownum, ll_payment_rownum, ll_docid, ll_service_provider_no
INTEGER					li_count
STRING						ls_service_provider_type_code, ls_type_code, ls_doc_subtype_code, ls_admin_region
DATE						ldt_date_on_document, ldt_date_received  
W_SHEET					lw_active_sheet
s_window_message 	lstr_message 

ls_admin_region = iw_active_sheet.dw_basic_claim.GetItemString(1,'admin_region_code')

IF gnv_user_authorizations.nf_authorizations_exist(ls_admin_region,"act") = FALSE THEN
	MessageBox("Account Payment Maintenance","You have no authorization for this claim's region",Exclamation!)
	RETURN
END IF

ll_account_rownum 		= dw_account_list.GetRow()
ll_payment_rownum 		= dw_account_payment_list.GetRow()

IF ll_account_rownum <= 0 THEN	RETURN

lw_active_sheet = w_frame.GetActiveSheet()
IF NOT IsValid(lw_active_sheet) THEN RETURN

ll_docid 			= dw_account_list.GetItemNumber(ll_account_rownum, 'docid')

SELECT service_provider_type_code, service_provider_no, type_code, doc_subtype_code, claim_no, date_on_document, date_received  
INTO 	:ls_service_provider_type_code, :ll_service_provider_no, :ls_type_code, :ls_doc_subtype_code, :ll_claim_no, :ldt_date_on_document, :ldt_date_received  
FROM 	DOCUMENT_INDEX
WHERE docid = :ll_docid
USING 	imagetrans;
imagetrans.nf_handle_error('w_account_payment', 'cb_pay_ephysio', 'SELECT service_provider_type_code')

/*
Once the document is selected, an image of the document is opened and the payment clerk must determine if the document is to be paid through the 
Account Payment payment details window or through the rehab invoice window. A new filter on the document image datawindow will assist the user 
by filtering the list to include only Physio Clinic Accounts (i.e. document type ‘AC’ document subtype ‘A1’)
NOTE: No longer applicable
*/


/*
Unlike the Account Payment module, this module will require that the claim have a rehab authorization for the billable item that is being submitted. 
The other main difference between this module and the Account Payment module is that the payment clerk will pay 
each invoice line item rather than the total of the submitted invoice. 
*/
SELECT count(*)
INTO :li_count
FROM REHAB_TASK  a
	JOIN REHAB_TASK_AUTHORIZATION b ON a.claim_no = b.claim_no AND a.task_no = b.task_no
    JOIN Billable_Item_Rehab_Task_Xref 	c 
		on a.rehab_service_code		  	= c.rehab_service_code
			AND a.rehab_program_code  	= c.rehab_program_code
			AND a.task_type_code      	= c.task_type_code
			AND a.task_sub_type_code  	= c.task_sub_type_code
			AND a.task_specific_code  		= c.task_specific_code
			AND b.billable_xref_no    		= c.billable_xref_no
	join Billable_Item 		d 	on c.billable_item_no 			= d.billable_item_no 
    join Rehab_Program 	e 	on a.rehab_program_code	= e.rehab_program_code 
WHERE a.claim_no							= :ll_claim_no
AND   a.rehab_service_code			= 'S022'
AND   a.task_status_code				NOT	IN ('03')
AND   c.payment_type_code			<> ''
GROUP BY a.rehab_program_code, a.planned_start_date, e.rehab_program_desc_e, actual_completion_date, a.task_no
USING SQLCA;
SQLCA.nf_handle_error("w_account_payment","clicked","SELECT count(*) INTO :li_count") 

/* if no authorizations they cant really do anything unless there is something already saved in the lineitems - will have to worry about that later */
IF isnull(li_count) OR li_count < 1 THEN 
	messagebox('No Rehab Authorizations', 'No Rehab Authorizations are currently available.' )
	RETURN -1
END IF 

/* need to grab the values from the powerobject and populate  the rest */
lstr_message.al_doubleparm[1] 		= ll_claim_no
lstr_message.al_doubleparm[2]  		= ll_docid
lstr_message.adt_dateparm[1] 		= ldt_date_on_document	
lstr_message.as_stringparm[1] 		= ls_service_provider_type_code
lstr_message.al_doubleparm[3]  		= ll_service_provider_no
lstr_message.adt_dateparm[2] 		= ldt_date_received
lstr_message.al_doubleparm[4]  		= 1 // [ Normal ]
lstr_message.al_doubleparm[5]  		= 0 //used for web based stuff
	
openwithparm(w_physio_reimbursements, lstr_message, lw_active_sheet)




end event

type cb_delete_ephysio from commandbutton within w_account_payment
boolean visible = false
integer x = 1902
integer y = 1512
integer width = 425
integer height = 100
integer taborder = 190
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Delete p&hysio..."
end type

event clicked;/*
Unlike the Account Payment module, this module will require that the claim have a rehab authorization for the billable item that is being submitted. 
The other main difference between this module and the Account Payment module is that the payment clerk will pay 
each invoice line item rather than the total of the submitted invoice. 

In order to reimburse the recipient,  the payment clerk will work from an imaged invoice or receipt that was submitted to WorkSafeNB and then
scanned & indexed to a claim. Since the Account Payment module already provides this ability to view a imaged invoice/receipt from an inbasket or 
from the claim master folder, the Account Payment module will be used as the access point into the new Rehab Invoice window.

In the Account Payment module, the payment clerk will select the document(s) in one of two ways:
Claim:		this displays all of the scanned account payable invoices for the selected claim	
In basket:	this displays all of the scanned account payable invoices for the selected In Basket
	
Once the document is selected, an image of the document is opened and the payment clerk must determine if the document is to be paid through the 
Account Payment payment details window or through the rehab invoice window. A new filter on the document image datawindow will assist the user 
by filtering the list to include only Physio Clinic Accounts (i.e. document type ‘AC’ document subtype ‘A1’)

If the document is a reimbursement for physio-related sercices and supplies, the payment clerk must click the rehab Invoice button to pay the invoice . 
Note: new business rules have been added to the Account Payment payment details window to prevent the payment clerk from reimbursing the 
injured worker or a provider for physio-related services and supplies through the account payment ‘payment’ window.

The claim that received the service or supply will be derived from the document indexing data for the image that is being viewed
(i.e. DOCUMENT_INDEX.claim_no).

The provider that provided the physio service may be derived from the document indexing data for the image that is being viewed, 
if this information was entered at the time that the document was indexed 
(i.e. DOCUMENT_INDEX.service_provider_type_code & DOCUMENT_INDEX.service_provider_no).
*/

LONG						ll_claim_no, ll_account_rownum, ll_payment_rownum, ll_docid, ll_service_provider_no, ll_rehab_invoice_no
STRING						ls_service_provider_type_code, ls_type_code, ls_doc_subtype_code, ls_admin_region
DATE						ldt_date_on_document, ldt_date_received  
W_SHEET					lw_active_sheet
s_window_message 	lstr_message 

ls_admin_region = iw_active_sheet.dw_basic_claim.GetItemString(1,'admin_region_code')

IF gnv_user_authorizations.nf_authorizations_exist(ls_admin_region,"act") = FALSE THEN
	MessageBox("Account Payment Maintenance","You have no authorization for this claim's region",Exclamation!)
	RETURN
END IF

//ll_account_rownum 		= dw_account_list.GetRow()
ll_payment_rownum 		= dw_account_payment_list.GetRow()
ll_account_rownum 		=	dw_account_type_payments_list.GetRow()

IF ll_account_rownum <= 0 THEN	RETURN

lw_active_sheet = w_frame.GetActiveSheet()
IF NOT IsValid(lw_active_sheet) THEN RETURN

ll_claim_no 				= dw_enter_claim_no.GetItemNumber(1,"claim_no")
ll_docid 						= dw_account_type_payments_list.GetItemNumber(ll_account_rownum, 'payment_document_doc_id')
ll_rehab_invoice_no 	= dw_account_type_payments_list.GetItemNumber(ll_account_rownum, 'rehab_invoice_no')

SELECT service_provider_type_code, service_provider_no, type_code, doc_subtype_code, claim_no, date_on_document, date_received  
INTO 	:ls_service_provider_type_code, :ll_service_provider_no, :ls_type_code, :ls_doc_subtype_code, :ll_claim_no, :ldt_date_on_document, :ldt_date_received  
FROM 	DOCUMENT_INDEX
WHERE docid = :ll_docid
USING 	imagetrans;
imagetrans.nf_handle_error('w_account_payment', 'cb_delete_ephysio', 'SELECT service_provider_type_code')

/* need to grab the values from the powerobject and populate  the rest */
lstr_message.al_doubleparm[1] 		= ll_claim_no
lstr_message.al_doubleparm[2]  		= ll_docid
lstr_message.adt_dateparm[1] 		= ldt_date_on_document	
lstr_message.as_stringparm[1] 		= ls_service_provider_type_code
lstr_message.al_doubleparm[3]  		= ll_service_provider_no
lstr_message.adt_dateparm[2] 		= ldt_date_received
lstr_message.al_doubleparm[4]  		= 2 // [ Delete Only ]
lstr_message.al_doubleparm[5]  		= ll_rehab_invoice_no
	
openwithparm(w_physio_reimbursements, lstr_message, lw_active_sheet)




end event

type gb_account_payment_list from groupbox within w_account_payment
integer x = 9
integer y = 1000
integer width = 3150
integer height = 624
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
end type

