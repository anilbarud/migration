$PBExportHeader$w_maintain_reporting_fees.srw
forward
global type w_maintain_reporting_fees from w_a_tool
end type
type dw_retrieve_ref_entries from u_dw_online within w_maintain_reporting_fees
end type
type dw_claimsmaster_ref_count from u_dw_online within w_maintain_reporting_fees
end type
type dw_account_type_payments_list from u_dw_online within w_maintain_reporting_fees
end type
type gb_account_list from groupbox within w_maintain_reporting_fees
end type
type gb_noaccount_payment_list from groupbox within w_maintain_reporting_fees
end type
type cb_refresh from commandbutton within w_maintain_reporting_fees
end type
type dw_reporting_fees from u_dw_online within w_maintain_reporting_fees
end type
type dw_document_path from u_dw_document_path within w_maintain_reporting_fees
end type
type dw_display_claim_info from u_dw_online within w_maintain_reporting_fees
end type
type uo_image_append from u_image_append within w_maintain_reporting_fees
end type
type cb_save from commandbutton within w_maintain_reporting_fees
end type
type dw_payment_document from u_dw_online within w_maintain_reporting_fees
end type
type cb_accept from commandbutton within w_maintain_reporting_fees
end type
type dw_payment_find from u_dw_online within w_maintain_reporting_fees
end type
end forward

global type w_maintain_reporting_fees from w_a_tool
boolean resizable = false
event ue_postopen pbm_custom01
event ue_childactivate pbm_custom02
dw_retrieve_ref_entries dw_retrieve_ref_entries
dw_claimsmaster_ref_count dw_claimsmaster_ref_count
dw_account_type_payments_list dw_account_type_payments_list
gb_account_list gb_account_list
gb_noaccount_payment_list gb_noaccount_payment_list
cb_refresh cb_refresh
dw_reporting_fees dw_reporting_fees
dw_document_path dw_document_path
dw_display_claim_info dw_display_claim_info
uo_image_append uo_image_append
cb_save cb_save
dw_payment_document dw_payment_document
cb_accept cb_accept
dw_payment_find dw_payment_find
end type
global w_maintain_reporting_fees w_maintain_reporting_fees

type variables
DATAWINDOWCHILD	idwc_taxing_categories
DATAWINDOWCHILD 	idwc_working_sets
DATETIME	 	idt_server_datetime
S_WINDOW_MESSAGE 	istr_message
U_DW_DOCUMENT_PATH 	iu_dw_document_path
N_PAYMENT 	inv_payment
w_inbasket            iwi_parent_window
w_account_payment_maintenance iw_account_payment_maintenance
w_account_link_maintenance 	iw_account_link_maintenance
w_memotab                                        iw_memotab
LONG	       il_setid, il_catid,il_rowerror = 0
LONG             il_inbasket_claim_no
BOOLEAN      ib_remove_after_payment = false
BOOLEAN      ib_imaged_view,ib_check = false

STRING	is_module_name
STRING   is_sort_name = "by Document ID"
STRING  is_filter_name = "All Documents"
STRING  is_as_mode

w_sheet    iw_sheet
end variables

forward prototypes
public function boolean wf_isaccount (string as_type)
public subroutine wf_apply_doc_list_option (string as_option, long al_option_number, string as_option_name)
public subroutine wf_open_maintenance ()
public subroutine wf_screen_reset (string as_screen_change)
public function integer wf_retrieve_doclist ()
public function integer wf_retrieve_claim (long al_claim_no)
public subroutine wf_set_parent_window (window awi_parent_window)
end prototypes

event ue_childactivate;//	Check to see if there were any child windows open, and if so
//	bring them to the front in the appropriate order

IF IsValid(iw_account_payment_maintenance) THEN
	iw_account_payment_maintenance.Show()
END IF

IF IsValid(iw_account_link_maintenance) THEN
	iw_account_link_maintenance.Show()
END IF
end event

public function boolean wf_isaccount (string as_type);Return (Left(as_type,1) = "A" OR as_type = "MPC" OR as_type = "MPD" OR as_type = "SDC" OR as_type = "SDD")

end function

public subroutine wf_apply_doc_list_option (string as_option, long al_option_number, string as_option_name);LONG	ll_list_rownum, ll_claim_no


CHOOSE CASE as_option
CASE "filter"

	CHOOSE CASE al_option_number
		CASE	1
			dw_reporting_fees.SetFilter("")
		CASE	2
			dw_reporting_fees.SetFilter("left( type , 1 ) = 'A' or type = 'MPC' or type = 'MPD' or type = 'SDC' or type = 'SDD'")
		CASE	3
			dw_reporting_fees.SetFilter("(left( type , 1 ) = 'A' or type = 'MPC' or type = 'MPD' or type = 'SDC' or type = 'SDD') and cpaid_status = ''")
		CASE	4
			dw_reporting_fees.SetFilter("type = 'AC'")
		CASE	5
			dw_reporting_fees.SetFilter("type = 'AD' or type = 'MPD' or type = 'SDD'")
		CASE	6
			dw_reporting_fees.SetFilter("type = 'AH'")
		CASE	7
			dw_reporting_fees.SetFilter("type = 'AI'")
		CASE	8
			dw_reporting_fees.SetFilter("type = 'AP'")
		CASE	9
			dw_reporting_fees.SetFilter("type = 'AR'")
		CASE	10
			dw_reporting_fees.SetFilter("type = 'AT'")
	END CHOOSE

	dw_reporting_fees.Filter()
	is_filter_name = as_option_name
	gb_account_list.Text = "Document View: " + is_filter_name + " / " + is_sort_name

	dw_reporting_fees.TriggerEvent(RowFocusChanged!)
	ll_list_rownum = dw_reporting_fees.GetRow()
	IF ll_list_rownum > 0 THEN
		dw_reporting_fees.uf_ProcessSelect(ll_list_rownum,"Mouse")
	END IF

CASE "sort"

	CHOOSE CASE al_option_number

	CASE 1	
		dw_reporting_fees.SetSort("claim A, type A, date A, comment A, docid A")
	CASE 2
		dw_reporting_fees.SetSort("date A, claim A, type A, comment A, docid A")
	CASE 3
		dw_reporting_fees.SetSort("docid A")
	CASE 4
		dw_reporting_fees.SetSort("type A, date A")
	CASE 5
		dw_reporting_fees.SetSort("sender A, date A") 
	End Choose

	dw_reporting_fees.Sort()

	is_sort_name = as_option_name
	gb_account_list.Text = "Document View: " + is_filter_name + " / " + is_sort_name
	dw_reporting_fees.TriggerEvent(RowFocusChanged!)
	ll_list_rownum = dw_reporting_fees.GetRow()
	IF ll_list_rownum > 0 THEN
		dw_reporting_fees.uf_ProcessSelect(ll_list_rownum,"Mouse")
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

public subroutine wf_open_maintenance ();//	Make sure the user is authorized

	IF inv_payment.nf_get_authorization_limit_act() < 0 THEN
		MessageBox("Account Payment Maintenance","You have no authorization for this claim's region",Exclamation!)
		Return
	END IF

	IF inv_payment.nf_validate_cost_alloc() < 0 THEN
		Return
	END IF

	istr_message.al_doubleparm[2] = dw_display_claim_info.GetItemNumber(1,"claim_no")
/*	Try to open the window and initialize.  If something happens in the initialization
	ensure the window is closed.
*/
	OpenWithParm(iw_account_payment_maintenance,istr_message,iw_active_sheet)
	IF IsValid(iw_account_payment_maintenance) THEN
		iw_account_payment_maintenance.wf_set_payment_object(inv_payment)
		iw_account_payment_maintenance.wf_set_parent_window(this)
		wf_screen_reset("update on")
	END IF

end subroutine

public subroutine wf_screen_reset (string as_screen_change);//CHAR  lc_screen
//STRING ls_hist_flag1, ls_hist_flag2
//
//IF dw_account_type_payments_list.Visible = TRUE THEN
//	lc_screen = 'Y'
//ELSE
//	lc_screen = 'N'
//END IF
//
///* Get history_flag values */
//
////ls_hist_flag1 = dw_display_claim_info.GetItemString(1,'claim_history_flag')
////ls_hist_flag2 = dw_display_claim_info.GetItemString(1, 'individual_history_flag')
//
//CHOOSE CASE as_screen_change
//
//	CASE "disable all", "update on"
//		cb_close.Enabled							  = False
//		dw_reporting_fees.Enabled 				  = False
//		dw_account_type_payments_list.Enabled = False
//	
//	CASE "not authorized"
//	
//	CASE "authorized"
//	
//	CASE "display imaged/nonimaged"
//		IF ib_imaged_view THEN
//			IF not gb_account_list.Visible           = True THEN
//				gb_account_list.Visible					  = True
//				dw_reporting_fees.Visible					  = True
//				gb_noaccount_payment_list.Visible	  = False
//				dw_account_type_payments_list.Visible = False
//			
//			END IF
//		ELSE
//			IF gb_account_list.Visible = True THEN
//				gb_account_list.Visible					  = False
//				dw_reporting_fees.Visible					  = False
//				gb_noaccount_payment_list.Visible	  = True
//				dw_account_type_payments_list.Visible = True
//			END IF
//		END IF
//
//	CASE "after refresh", "update off"
//		cb_close.Enabled							= True
//		cb_refresh.Enabled						= True
//		
//		IF ib_imaged_view THEN
//			dw_reporting_fees.Enabled				= True
//		ELSE
//			dw_account_type_payments_list.Enabled = True
//		END IF
//
//		IF il_catid = 2 THEN
//		
//			IF inv_payment.nf_is_valid_status_act() THEN		
//			ELSE
//				MessageBox('Invalid Status','The claim status does not allow for account payments.')
//			END IF
//		END IF
//		
//	CASE "account list"
//		
//END CHOOSE
//
//
///* If InBasket Unpaid Accounts called the Account Payment module, the documents are displayed
//	for a specific claim and are removed once the user has paid or rejected the document. The user
//	therefore cannot enter a claim number or taxing category or working set. The Remove after Payment check 
//	box is always set on.
//*/
//IF is_as_mode = "inbasketunpaid" THEN
//	ib_remove_after_payment 				= true
//END IF
//
end subroutine

public function integer wf_retrieve_doclist ();LONG		ll_retrieval_arg,	ll_nmbr_entries, ll_cntr, ll_docid, ll_rowcount,ll_retrieval_arg2
INTEGER	li_return

//	Check to see if the where clause on the datawindow has to be modified as this 
//	datawindow is used for retrieving by claim and by category.  If so, make the modifications.
	
	IF is_as_mode = "inbasketunpaid" THEN
		ll_retrieval_arg = il_inbasket_claim_no
		ll_retrieval_arg2 = 0	
	ELSE
		IF il_catid = 2 THEN
			ll_retrieval_arg2 = 0
		ELSE
			ll_retrieval_arg2 = il_catid
			ll_retrieval_arg = 0
		END IF
	END IF


//	Retrieve and display the account list

	dw_reporting_fees.Reset()
	dw_reporting_fees.Retrieve(ll_retrieval_arg,ll_retrieval_arg2)
	li_return = SQLCA.nf_handle_error("dw_reporting_fees",'w_account_payment',"wf_retrieve_doclist")
	IF li_return < 0 THEN
		Return li_return				//	This will keep everything disabled and allow the user to try to refresh again
	END IF

	IF is_as_mode = "inbasketunpaid" THEN
		wf_apply_doc_list_option("filter",3,"")
   END IF


// Reapply any filters and sorts that may have been overridden
dw_reporting_fees.Filter()
dw_reporting_fees.Sort()

return 0
end function

public function integer wf_retrieve_claim (long al_claim_no);LONG					ll_advances, ll_individual_no
INTEGER				li_return_value
STRING				ls_status, ls_display_message
DECIMAL				ldec_balance_amount
S_WINDOW_MESSAGE	lstr_message


	li_return_value = dw_display_claim_info.Retrieve(al_claim_no)
	SQLCA.nf_handle_error('Retrieve of dw_dispaly_claim_info','w_account_payment', 'wf_retrieve_claim')
	IF li_return_value < 1 THEN Return li_return_value

	iw_active_sheet.wf_set_claim(dw_display_claim_info.GetItemNumber(1,"claim_no"))
  IF isvalid(inv_payment) THEN
		inv_payment.nf_set_claim_no(al_claim_no)
	ELSE
		inv_payment = Create n_payment
		inv_payment.nf_set_claim_no(al_claim_no)
	END IF

	ls_status = dw_display_claim_info.GetItemString(1,'imaged_flag')
	IF ls_status = "N" THEN
		ib_imaged_view = FALSE
	ELSE
		ib_imaged_view = TRUE
	END IF

	ll_individual_no = dw_display_claim_info.GetItemNumber(1,'individual_no')

Return 1
end function

public subroutine wf_set_parent_window (window awi_parent_window);iwi_parent_window = awi_parent_window
end subroutine

event closequery;call super::closequery;
//	Destroy the instance of the payment user object

	IF IsValid(inv_payment) THEN
		Destroy(inv_payment)
	END IF


end event

event open;call super::open;INTEGER  				li_return_status,li_rows
STRING					ls_filter
S_WINDOW_MESSAGE		lstr_window_message


lstr_window_message= Message.PowerObjectParm

iw_sheet = lstr_window_message.apo_powerobjectparm[1]
If IsValid(iw_sheet) = False Then
	MessageBox("Maintain Reporting Fees","Error determining active sheet. You may have to close WorkBench and try again.")
	Close(This)
	Return
End If


	idt_server_datetime 		= f_server_datetime()
	is_module_name				= st_title.text


	iw_account_payment_maintenance = w_account_payment_maintenance
	iw_account_link_maintenance    = iw_account_link_maintenance

/*	Set transaction objects on the main datawindows
*/
	dw_reporting_fees.SetTransObject(SQLCA)
	dw_display_claim_info.SetTransObject(SQLCA)
	dw_account_type_payments_list.SetTransObject(SQLCA)
	dw_claimsmaster_ref_count.SetTransObject(ImageTrans)
	dw_retrieve_ref_entries.SetTransObject(ImageTrans)
   dw_payment_find.SetTransObject(SQLCA)

/*	Set the selection mode on the list datawindows (Single Select)
*/
	dw_reporting_fees.uf_setselect(1)
	dw_account_type_payments_list.uf_setselect(1)
	
	dw_payment_find.retrieve()
	IF SQLCA.nf_handle_error("dw_payment_find.retrieve()",is_module_name,"open event") < 0 THEN
		Close(this)
		Return
	END IF

   li_rows = dw_reporting_fees.retrieve()
	IF SQLCA.nf_handle_error(" dw_reporting_fees.retrieve()",is_module_name,"open event") < 0 THEN
		Close(this)
		Return
	END IF
	
	IF li_rows < 1 THEN
		messagebox("Reporting Fee","There are no documents waiting to be approved for the NBMS/NBCA Reporting Fee." ,information!)
	END IF


/*	Create an instance of the user object for the view function
*/
	iu_dw_document_path = dw_document_path


/*	Create an instance of the payment user object
*/
	inv_payment = Create n_payment
	inv_payment.nf_set_basic_claim(dw_display_claim_info)


end event

on w_maintain_reporting_fees.create
int iCurrent
call super::create
this.dw_retrieve_ref_entries=create dw_retrieve_ref_entries
this.dw_claimsmaster_ref_count=create dw_claimsmaster_ref_count
this.dw_account_type_payments_list=create dw_account_type_payments_list
this.gb_account_list=create gb_account_list
this.gb_noaccount_payment_list=create gb_noaccount_payment_list
this.cb_refresh=create cb_refresh
this.dw_reporting_fees=create dw_reporting_fees
this.dw_document_path=create dw_document_path
this.dw_display_claim_info=create dw_display_claim_info
this.uo_image_append=create uo_image_append
this.cb_save=create cb_save
this.dw_payment_document=create dw_payment_document
this.cb_accept=create cb_accept
this.dw_payment_find=create dw_payment_find
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_retrieve_ref_entries
this.Control[iCurrent+2]=this.dw_claimsmaster_ref_count
this.Control[iCurrent+3]=this.dw_account_type_payments_list
this.Control[iCurrent+4]=this.gb_account_list
this.Control[iCurrent+5]=this.gb_noaccount_payment_list
this.Control[iCurrent+6]=this.cb_refresh
this.Control[iCurrent+7]=this.dw_reporting_fees
this.Control[iCurrent+8]=this.dw_document_path
this.Control[iCurrent+9]=this.dw_display_claim_info
this.Control[iCurrent+10]=this.uo_image_append
this.Control[iCurrent+11]=this.cb_save
this.Control[iCurrent+12]=this.dw_payment_document
this.Control[iCurrent+13]=this.cb_accept
this.Control[iCurrent+14]=this.dw_payment_find
end on

on w_maintain_reporting_fees.destroy
call super::destroy
destroy(this.dw_retrieve_ref_entries)
destroy(this.dw_claimsmaster_ref_count)
destroy(this.dw_account_type_payments_list)
destroy(this.gb_account_list)
destroy(this.gb_noaccount_payment_list)
destroy(this.cb_refresh)
destroy(this.dw_reporting_fees)
destroy(this.dw_document_path)
destroy(this.dw_display_claim_info)
destroy(this.uo_image_append)
destroy(this.cb_save)
destroy(this.dw_payment_document)
destroy(this.cb_accept)
destroy(this.dw_payment_find)
end on

event close;call super::close;
post close(iw_sheet)

end event

type st_title from w_a_tool`st_title within w_maintain_reporting_fees
integer width = 3163
fontcharset fontcharset = ansi!
long textcolor = 33554432
string text = "Maintain Documents Eligible for a Provider Reporting Fee"
end type

type cb_close from w_a_tool`cb_close within w_maintain_reporting_fees
integer x = 2258
integer y = 1704
integer width = 343
integer taborder = 20
boolean default = true
end type

event cb_close::clicked;/* Check to see it there are any modified records 
   if so ask if they want to continue to close the window
*/
LONG		ll_found
INT		li_message


ll_found = dw_reporting_fees.find("accepted = 0 or eligibility_code <> 'ELG'",1,dw_reporting_fees.rowcount())
IF ll_found  > 0 THEN
	li_message = messagebox("Rows Modified","Data not Saved. Save now?",QUESTION!,YESNO!,2)
	IF li_message = 1 THEN
		RETURN
		
	ELSE	
	END IF
END IF

Close(parent)

end event

type dw_retrieve_ref_entries from u_dw_online within w_maintain_reporting_fees
boolean visible = false
integer x = 3003
integer y = 48
integer width = 786
integer height = 392
integer taborder = 110
string dataobject = "d_retrieve_ref_entries"
end type

type dw_claimsmaster_ref_count from u_dw_online within w_maintain_reporting_fees
boolean visible = false
integer x = 3003
integer y = 540
integer width = 795
integer height = 360
integer taborder = 80
string dataobject = "d_claimsmaster_ref_count"
end type

type dw_account_type_payments_list from u_dw_online within w_maintain_reporting_fees
boolean visible = false
integer x = 119
integer y = 564
integer width = 503
integer height = 1008
integer taborder = 120
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
	istr_message.as_mode = "display"
	wf_open_maintenance()
end event

event rowfocuschanged;call super::rowfocuschanged;LONG ll_rownum

ll_rownum = This.GetRow()

IF ll_rownum > 0 THEN
	uf_processselect(ll_rownum,"Mouse")
	IF not IsNull(this.GetItemDateTime(ll_rownum,"processed_date")) THEN

	ELSE
		
	END IF
ELSE
	
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

event ue_filter;call super::ue_filter;//LONG					ll_row
//S_WINDOW_MESSAGE	lstr_message
//
//	Open(w_filter_account_pay_list)
//	lstr_message = Message.PowerObjectParm	
//
//	If lstr_message.as_stringparm[1] <> 'Cancel' THEN
//		This.SetFilter(lstr_message.as_stringparm[1])
//		This.Filter()
//	
//		gb_account_list.Text = "Document View: " + lstr_message.as_stringparm[2] 
//		This.TriggerEvent(RowFocusChanged!)
//		ll_row = This.GetRow()
//		IF ll_row > 0 THEN
//			This.uf_ProcessSelect(ll_row,"Mouse")
//		END IF
//	END IF

//MessageBox("Warning", "Filtering is temporarily disabled on this datawindow")
end event

type gb_account_list from groupbox within w_maintain_reporting_fees
integer x = 69
integer y = 324
integer width = 2469
integer height = 128
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Document View: All Documents / by Document Id"
end type

type gb_noaccount_payment_list from groupbox within w_maintain_reporting_fees
boolean visible = false
integer x = 1582
integer y = 344
integer width = 809
integer height = 1236
integer taborder = 130
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
end type

type cb_refresh from commandbutton within w_maintain_reporting_fees
event ue_update_complete pbm_custom04
event ue_update_cancelled pbm_custom06
integer x = 32
integer y = 1704
integer width = 343
integer height = 100
integer taborder = 70
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Refresh"
end type

event ue_update_complete;LONG     ll_rownum, ll_docid,	ll_claim_no, ll_count
BOOLEAN  lb_remove_documents

lb_remove_documents = false

wf_screen_reset("update off")
ll_claim_no = dw_display_claim_info.GetItemNumber(1,"claim_no")

IF ib_imaged_view THEN
	ll_rownum = dw_reporting_fees.GetRow()
	IF istr_message.as_mode = "delete" THEN
		dw_reporting_fees.TriggerEvent(rowfocuschanged!)
	
	ELSE
		IF istr_message.as_mode = "link" THEN
			//	Check to see if the claim still has advances
		END IF
	
		/*	Check to see if autoremove is true, but don't autoremove if the document was rejected unless
			the calling module is the Inbasket Unpaid Accounts
		*/
		IF is_as_mode <> "inbasketunpaid" THEN 
			IF ib_remove_after_payment and not istr_message.as_mode = "reject" and not istr_message.as_mode = "display" THEN
				lb_remove_documents = true
			END IF
		ELSE
			IF ib_remove_after_payment and not istr_message.as_mode = "display" THEN
				lb_remove_documents = true
			END IF
		END IF
		IF lb_remove_documents THEN
			ll_docid = dw_reporting_fees.GetItemNumber(ll_rownum,"docid")
		
			// IF the remove didn't work successfully, trigger the rowfocuschanged to 
			// clean up the screen
			ll_rownum = dw_reporting_fees.GetRow()
			IF ll_rownum > 0 then
				IF ll_docid = dw_reporting_fees.GetItemNumber(ll_rownum,"docid") THEN
					dw_reporting_fees.TriggerEvent(rowfocuschanged!)
				END IF
			END IF
		ELSE
			IF ll_rownum < dw_reporting_fees.RowCount() THEN
				ll_rownum = ll_rownum + 1
				dw_reporting_fees.ScrollToRow(ll_rownum)
				dw_reporting_fees.uf_processselect(ll_rownum,"Mouse")
			ELSE
				dw_reporting_fees.TriggerEvent(rowfocuschanged!)
			END IF
		END IF
	END IF
ELSE
	dw_account_type_payments_list.Reset()
	dw_account_type_payments_list.Retrieve(ll_claim_no)
	SQLCA.nf_handle_error("dw_account_type_payment_list",is_module_name,"ue_update_complete for db_refresh")
END IF
		
istr_message.al_doubleparm[5] = 0


end event

event ue_update_cancelled;LONG 	ll_claim_no, ll_payment_no, ll_rownum,	ll_account_rownum

wf_screen_reset("update off")
IF ib_imaged_view THEN
	dw_reporting_fees.TriggerEvent(rowfocuschanged!)
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
	IF ll_payment_no > 0 THEN
		ll_rownum = dw_account_type_payments_list.Find("payment_no = " + string(ll_payment_no) ,1,dw_account_type_payments_list.RowCount())
		IF ll_rownum > 0 THEN
			dw_account_type_payments_list.ScrollToRow(ll_rownum)
			dw_account_type_payments_list.uf_processselect(ll_rownum,"Mouse")
		END IF
	END IF
END IF
		

end event

event clicked;LONG    ll_catid, ll_claim_no
INTEGER li_return_status,li_rows

SetPointer(HourGlass!)

ib_check = false
dw_reporting_fees.reset()

 li_rows = dw_reporting_fees.retrieve()
IF SQLCA.nf_handle_error("dw_reporting_fees.retrieve()",is_module_name,"cb_refresh") < 0 THEN
	Return -1
END IF
	
IF li_rows < 1 THEN
	Return -1
END IF

dw_reporting_fees.object.eligibility_code.protect = 0
dw_reporting_fees.object.accepted.protect         = 0	
cb_save.enabled   = true
cb_accept.enabled = true



end event

type dw_reporting_fees from u_dw_online within w_maintain_reporting_fees
event ue_view ( )
event ue_post_setitem ( )
integer x = 32
integer y = 104
integer width = 3177
integer height = 1572
integer taborder = 90
boolean bringtotop = true
string dataobject = "d_maintain_reporting_fees"
boolean vscrollbar = true
boolean livescroll = false
borderstyle borderstyle = stylelowered!
end type

event ue_view;LONG	ll_rownum,ll_doc_id
string ls_doc_type
integer li_rtn

//	Get the document id of the selected row

	ll_rownum = dw_reporting_fees.GetRow()
	IF ll_rownum <= 0 THEN
		MessageBox("Medical Society Agreement","You must select a document before you can view it",Exclamation!)
		Return
	END IF

ll_doc_id = dw_reporting_fees.GetItemNumber(ll_rownum,"docid")
	
IF isvalid(uo_image_append) THEN
	IF uo_image_append.of_init(ll_doc_id)	<= 0 THEN
		RETURN
	END IF
		
		
	ls_doc_type =  uo_image_append.of_get_file_type()
		
	
	CHOOSE CASE ls_doc_type
		/*  Imaged document */ 
		CASE 'IMA', 'TIF'
			li_rtn = uo_image_append.of_append_image(ll_doc_id)
			IF li_rtn < 0 THEN
				RETURN
			END IF
		CASE ELSE
	/*		 Get value of document id field on selected row */
			iu_dw_document_path.f_manage_document(dw_reporting_fees.GetItemNumber(ll_rownum,"docid"),"V","NORMAL")
	END CHOOSE
			
END IF
		
		
end event

event ue_post_setitem;this.setitem(this.getrow(),"accepted",0)
end event

event rowfocuschanged;call super::rowfocuschanged;STRING   ls_eligibility_code,ls_expression,ls_expression_two
INTEGER	li_find,li_end,li_return_value,li_viewer_return
LONG		ll_rownum,ll_docid,ll_claim_no

/*	Get the document id of the selected document
*/
	SetPointer(HourGlass!)
	IF this.rowcount() < 1 THEN
		return
	END IF
	
	/*	Verify that the document was properly indexed - If there is no docid, there is no reason to continue
	through the code as we can't do anything anyway.
*/
	ll_docid = dw_reporting_fees.GetItemNumber(currentrow,"docid")
	IF IsNull(ll_docid)  THEN
		RETURN
	END IF
	
/* check to see if the viewer is open - if it is close it.
*/
	DO 
		li_viewer_return = f_close_viewer()
	LOOP until li_viewer_return = 0
	
/*	Retrieve payment and claim information.
	Retrieve any existing payments for this document 
*/

/* If the user has removed the check in the checkbox column "accepted" then
   the column eligibility_code must be changed to reflect this
	Aswell if the eligibity code has been changed and the column is type accepted then 
	it must be reset back to eligible
*/
ls_expression     = 'accepted = 0 and eligibility_code = "ELG"'
ls_expression_two = 'accepted = 1 and eligibility_code <> "ELG"'
li_end = this.rowcount()
il_rowerror = 0

li_find = this.find(ls_expression,0,li_end)
IF li_find = currentrow THEN
	ib_check = FALSE
END IF

IF ib_check = TRUE AND li_find > 0 THEN
	IF li_find > 0 THEN
		this.setrow(li_find)
		ll_rownum = li_find	
		il_rowerror = ll_rownum
		messagebox("Maintain Reporting Fees","Please set the document's status if the document is not to be approved for NBMS/NBCA Reporting Fee payment")	
		return
	END IF
ELSE
	ll_rownum = currentrow
	ib_check = true
END IF
	
li_find = this.find(ls_expression_two,0,li_end)
IF li_find = currentrow THEN
	ib_check = FALSE
END IF

IF ib_check = TRUE AND li_find > 0 THEN
	IF li_find > 0 THEN
		this.setrow(li_find)
		ll_rownum = li_find
		il_rowerror = ll_rownum
		messagebox("Maintain Reporting Fees","Please set the document's status if the document is not to be approved for NBMS/NBCA Reporting Fee payment")	
		return
	END IF
ELSE
	ll_rownum = currentrow
	ib_check = true
END IF

//	If we're not in the master folder, check to see if we have a new claim number.
//	If so, retrieve the claim information, check the claim status and clean up accordingly
IF il_rowerror = 0 THEN
	this.SelectRow(0, false)
	this.setrow(ll_rownum)
	this.SelectRow(ll_rownum, true)
	ll_claim_no = dw_reporting_fees.GetItemNumber(ll_rownum,"claim_no")
	
	li_return_value = wf_retrieve_claim(ll_claim_no)
	IF li_return_value = 0 THEN
		MessageBox(is_module_name,"Error validating claim. Please verify indexing on this document",Exclamation!)
		RETURN
	END IF
END IF

RETURN
	
	

end event

event ue_filter;call super::ue_filter;STRING 	ls_filter,ls_check

INTEGER	li_return_value
LONG		ll_claim_no

	Open(w_filter_document_list)

//	Apply the filter that was selected 
	ls_filter = Message.StringParm
	IF ls_filter = "Cancel" THEN
		Return
	END IF
//retrieve the origional sort of the datawindow.
   dw_reporting_fees.Sort()
	
//set the filter to the one specified.
	dw_reporting_fees.SetFilter(ls_filter)
	dw_reporting_fees.Filter()
	
	IF dw_reporting_fees.rowcount() > 0 THEN
		ls_check = this.getitemstring(1,"eligibility_code")
		CHOOSE CASE ls_check
			CASE "ELG"
				this.object.eligibility_code.protect = 0
			   this.object.accepted.protect         = 0	
				cb_save.enabled   = true
				cb_accept.enabled = true
			CASE "DNL","DNC"
				this.object.eligibility_code.protect = 1
			   this.object.accepted.protect         = 0
				cb_save.enabled   = false
				cb_accept.enabled = false
			CASE ELSE
				this.object.eligibility_code.protect = 1
			   this.object.accepted.protect         = 1	
				cb_save.enabled   = false
				cb_accept.enabled = false
		END CHOOSE
	END IF
	
IF this.rowcount() > 0 THEN
	this.SelectRow(0, false)
	this.setrow(1)
	this.SelectRow(1, true)
	ll_claim_no = dw_reporting_fees.GetItemNumber(1,"claim_no")
	
	li_return_value = wf_retrieve_claim(ll_claim_no)
	IF li_return_value = 0 THEN
		MessageBox(is_module_name,"Error validating claim. Please verify indexing on this document",Exclamation!)
		RETURN
	END IF
END IF


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

event clicked;LONG							ll_claim_no
Long    ll_selected_row, ll_doc_id

string ls_doc_type,ls_return,ls_code
integer li_rtn,li_accepted
s_reporting_fee_parameters	ls_sdp
/* all we want to do he is scroll to the row that has been highlighted
   the datawindow will do the rest.
*/

IF il_rowerror = 0 THEN
	this.setrow(row)
END IF

il_rowerror = 0

CHOOSE CASE dwo.name
	CASE "accepted"
		// Get the first highlighted document
		ll_selected_row = dw_reporting_fees.GetSelectedRow(0)
		If ll_selected_row = 0 Then
			MessageBox("Issue Reporting Fee", "You must select a claim and document to receive the Reporting Fee Payment",StopSign!)
			Return
		End If
		ls_code     = dw_reporting_fees.getitemstring(row,"eligibility_code")	
		li_accepted = dw_reporting_fees.getitemnumber(row,"accepted")
		IF li_accepted = 1 THEN
			return
		END IF
		
		IF dw_reporting_fees.rowcount() > 0 AND (ls_code = "DNL" OR ls_code = "DNC" )THEN
			// Open the send window
			ls_sdp.doc_id   = dw_reporting_fees.GetItemNumber(row,"docid")
			ls_sdp.claim_no = dw_reporting_fees.GetItemNumber(row,"claim_no")
			OpenWithParm(w_issue_reporting_fee_lite,ls_sdp)
				
			ls_return = message.stringparm
				
			IF NOT ISNULL(ls_return) AND trim(ls_return) <> "" THEN
				dw_reporting_fees.setItem(row,"eligibility_code","APP")
				dw_reporting_fees.setItem(row,"accepted","1")
				dw_reporting_fees.setItem(row,"comment",ls_return)
				cb_save.enabled = TRUE
			ELSE
				dw_reporting_fees.setItem(row,"accepted","0")
				this.postevent("ue_post_setitem")
				
			END IF
		END IF
	CASE ELSE
END CHOOSE



	


end event

event retrieveend;call super::retrieveend;INT				li_counter
STRING			ls_filter,ls_document_status
datawindowchild ldw_child

/* for some odd reason the default on this column is not performing as expected
   this should default the values of the accepted flags to 1
*/
setpointer(hourglass!)

ib_check = false

dw_reporting_fees.SetRedraw(false)


this.getChild('provider_sub_type_code',ldw_child)
ldw_child.setFilter("provider_type_code = 'M'")
ldw_child.filter()

/* On the open we want to filter by "ELG"
*/
ls_document_status = "ELG"
ls_filter = "eligibility_code = '" + ls_document_status + "'"
dw_reporting_fees.SetFilter(ls_filter)
dw_reporting_fees.Filter()


FOR li_counter = 1 TO this.rowcount()
	this.setitem(li_counter,"accepted",1)
NEXT

IF this.rowcount() <= 0 THEN
	cb_save.enabled   = FALSE
	cb_accept.enabled = FALSE
END IF

dw_reporting_fees.SetRedraw(true)
dw_reporting_fees.setfocus()
dw_reporting_fees.setrow(1)
dw_reporting_fees.selectrow(1,TRUE)


end event

event losefocus;call super::losefocus;this.accepttext()
end event

event dberror;call super::dberror;
sqlca.SQLDBCode = sqldbcode
sqlca.SQLErrText = sqlerrtext
return 1


end event

event doubleclicked;call super::doubleclicked;this.triggerevent("ue_view")
end event

type dw_document_path from u_dw_document_path within w_maintain_reporting_fees
boolean visible = false
integer x = 155
integer y = 1676
integer taborder = 10
end type

type dw_display_claim_info from u_dw_online within w_maintain_reporting_fees
boolean visible = false
integer x = 1038
integer y = 640
integer height = 528
integer taborder = 100
string dataobject = "d_basic_claim_everything"
end type

type uo_image_append from u_image_append within w_maintain_reporting_fees
boolean visible = false
integer x = 768
integer y = 1672
integer taborder = 30
boolean bringtotop = true
end type

on uo_image_append.destroy
call u_image_append::destroy
end on

type cb_save from commandbutton within w_maintain_reporting_fees
integer x = 1865
integer y = 1704
integer width = 343
integer height = 100
integer taborder = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Save"
end type

event clicked;/* "The maintain Documents window should have an 'Accept' button, which will set the
    eligibility status to approvedand save any statuses for documents that are not
	 accepted And a "SAVE" button. The SAVE button will only save the status of
	 the documents that have manually changed.
	  
	 *** Fields included - doc_id, payment_no, paid_status_code, paid_status_explanation_code ***
	  
	- If the document is not accepted, then a rejected reason must be selected.
	  Rejected reasons are 'DNC - Document Not Complete' and 
	  'DNL - Document Not Legible' 
*/

LONG			ll_rowcount,ll_counter,ll_doc_id,ll_find_rowcount

INT			li_accepted_value,li_row,li_error,li_find,li_check,li_approved, li_rtn

STRING		ls_eligibility_code
DATETIME		ldt_current

N_PROCESS_RUN_STATUS ln_process_run_status

/******************************************************************************************
P10275 - Daytime Payment Processing
- added new function call to prevent updating of tables used by PRODSVCS Payment Processing
- new object N_PROCESS_RUN_STATUS is used to determine status of processes (in progress = Y/N)
- '016' refers to the Maintain NBMS/NBCA Reporting Fees module, '044' refers to the Payment Processing module
******************************************************************************************/
ln_process_run_status = Create N_PROCESS_RUN_STATUS

li_rtn = ln_process_run_status.nf_in_progress('016','044','save',SQLCA)

IF li_rtn = 1 THEN
	// module is blocked
	return
END IF
/******************************************************************************************/

/* set the pointer
*/
setpointer(hourglass!)

/* do some quick checks before we proceed
*/
IF dw_reporting_fees.rowcount() <= 0 THEN
	RETURN
END IF

ll_find_rowcount = dw_payment_find.rowcount()

ldt_current = f_server_datetime()

/* go through each of the rows in dw_reporting_fees and 
   if the row is accepted we will add it too our datastore based 
	on then Set the ELIGIBLE_REPORT_FEE_DOCUMENTS.reporting_fee_eligibility_code = "APP"
ELSE
	we will write a record to our datastore based on PAYMENT_DOCUMENT as illustrated in
	the header information.
*/
ll_rowcount = dw_reporting_fees.rowcount()
li_approved = 0
FOR ll_counter = 1 TO ll_rowcount
	ls_eligibility_code  = dw_reporting_fees.getitemstring(ll_counter,"eligibility_code")
	IF ls_eligibility_code = "APP" THEN
		li_approved ++
	END IF
NEXT

IF li_approved > 0 THEN
	
	SQLCA.nf_begin_transaction()
	
	FOR ll_counter = 1 TO ll_rowcount
		ls_eligibility_code  = dw_reporting_fees.getitemstring(ll_counter,"eligibility_code")
		li_accepted_value    = dw_reporting_fees.getitemnumber(ll_counter,"accepted")
			
		IF ls_eligibility_code = "APP" AND li_accepted_value = 1 THEN
			ll_doc_id            = dw_reporting_fees.getitemnumber(ll_counter,"docid")
			IF ll_find_rowcount > 0 THEN
				li_find = dw_payment_find.find("doc_id = "+ string(ll_doc_id),0,ll_find_rowcount)
			END IF
				
			dw_reporting_fees.setitem(ll_counter,"approved_date",ldt_current)
			dw_reporting_fees.setitem(ll_counter,"approved_user_id",vgst_user_profile.user_id)
					
			/* this document may already be in the PAYMENT_DOCUMENT table therfore we
				need to get rid of it
			*/
			IF li_find > 0 THEN
				//do the delete
				DELETE PAYMENT_DOCUMENT
				 WHERE doc_id     = :ll_doc_id
					AND payment_no = 0
				 USING SQLCA;
					
				IF SQLCA.nf_handle_error("w_maintain_reporting_fees","cb_accept_click","dw_reporting_fees.update()") < 0 THEN
					Return -1
				END IF
			END IF
		END IF
	NEXT
				
	/* now lets do our updates on the datawindows
	*/
	li_error = dw_reporting_fees.update()
	IF SQLCA.nf_handle_error("w_maintain_reporting_fees","cb_save_click","dw_reporting_fees.update()") < 0 THEN
		Return -1
	END IF
		
	/* If everything goes alright we should be able to commit the transactions
	*/
	SQLCA.nf_commit_transaction()
			
	/* now trigger the refresh event
	*/
	ib_check = false
	cb_refresh.triggerevent("clicked")
			
	/* If there are no rows then disable the save button
	*/
	IF dw_reporting_fees.rowcount() <= 0 THEN
		cb_save.enabled    = FALSE
		cb_refresh.enabled = FALSE
		cb_accept.enabled  = FALSE
	END IF
return
END IF



FOR ll_counter = 1 TO ll_rowcount
	li_accepted_value    = dw_reporting_fees.getitemnumber(ll_counter,"accepted")
	ll_doc_id            = dw_reporting_fees.getitemnumber(ll_counter,"docid")
	ls_eligibility_code  = dw_reporting_fees.getitemstring(ll_counter,"eligibility_code")

	/* do a quick validation on everything to make sure that we don't
		have any validation problems.
	*/
	IF li_accepted_value = 1 AND ls_eligibility_code <> "ELG" THEN
		messagebox("Validation Error","If the document is accepted then the eligibility code must be 'ELG'",stopsign!)
		dw_reporting_fees.setrow(ll_counter)
		dw_reporting_fees.setcolumn("eligibility_code")
		dw_reporting_fees.setfocus()
		RETURN
	ELSEIF li_accepted_value = 0 AND  (ls_eligibility_code <> "DNC" AND ls_eligibility_code <> "DNL") THEN
		messagebox("Validation Error","If the document is not accepted then the eligibility code must be either 'DNC' or 'DNL'",stopsign!)
		dw_reporting_fees.setrow(ll_counter)
		dw_reporting_fees.setcolumn("eligibility_code")
		dw_reporting_fees.setfocus()
		RETURN
	END IF
			
	IF li_accepted_value = 1 THEN
		IF ls_eligibility_code <> "ELG" THEN
			messagebox("Validation Error","If the document is accepted then the eligibility code must be 'ELG'",stopsign!)
			dw_reporting_fees.setrow(ll_counter)
			dw_reporting_fees.setcolumn("eligibility_code")
			dw_reporting_fees.setfocus()
			RETURN
		END IF
	ELSE
				
		IF ls_eligibility_code <> "DNC" AND ls_eligibility_code <> "DNL" THEN
			messagebox("Validation Error","If the document is not accepted then the eligibility code must be either 'DNC' or 'DNL'",stopsign!)
			dw_reporting_fees.setrow(ll_counter)
			dw_reporting_fees.setcolumn("eligibility_code")
			dw_reporting_fees.setfocus()
			RETURN
		END IF
	END IF
NEXT



FOR ll_counter = 1 TO ll_rowcount
	li_accepted_value    = dw_reporting_fees.getitemnumber(ll_counter,"accepted")
	ll_doc_id            = dw_reporting_fees.getitemnumber(ll_counter,"docid")
	ls_eligibility_code  = dw_reporting_fees.getitemstring(ll_counter,"eligibility_code")
	
	IF li_accepted_value <> 1 THEN
		/* this document may already be in the PAYMENT_DOCUMENT table therfore we
			need to get rid of it
		*/
		li_check = 0
		IF ll_find_rowcount > 0 THEN
			li_find = dw_payment_find.find("doc_id = "+ string(ll_doc_id),0,ll_find_rowcount)
			IF li_find > 0 THEN
				li_check = 1
			END IF
		END IF	
		
		IF li_check <> 1 THEN
			li_row = dw_payment_document.insertrow(0)
			dw_payment_document.setitem(li_row,"doc_id",ll_doc_id)
			dw_payment_document.setitem(li_row,"payment_no",0)
			dw_payment_document.setitem(li_row,"paid_status_code","R")
			
			/*If the rejected reason is 'Document not complete' 
			(ELIGIBLE_REPORT_FEE_DOCUMENTS.reporting_fee_eligibility_code = 'DNC'), then use '12'. 
			If the rejected reason is 'Document not legible' 
			(ELIGIBLE_REPORT_FEE_DOCUMENTS.reporting_fee_eligibility_code = 'DNL'), then use '13'.
			*/
			IF ls_eligibility_code = "DNC" THEN
				dw_payment_document.setitem(li_row,"paid_status_explanation_code","12")
			ELSEIF ls_eligibility_code = "DNL" THEN
				dw_payment_document.setitem(li_row,"paid_status_explanation_code","13")
			ELSE
			END IF	
		END IF
	END IF
NEXT


SQLCA.nf_begin_transaction()	

/* now lets do our updates on the datawindows
*/
li_error = dw_reporting_fees.update()
IF SQLCA.nf_handle_error("w_maintain_reporting_fees","cb_save_click","dw_reporting_fees.update()") < 0 THEN
	Return -1
END IF

/* remember to check the update 
*/
li_error = dw_payment_document.update()
IF SQLCA.nf_handle_error("w_maintain_reporting_fees","cb_save_click","dw_payment_document.update()")  < 0 THEN
	Return -1
END IF

/* If everything goes alright we should be able to commit the transactions
*/
SQLCA.nf_commit_transaction()



/* now trigger the refresh event
*/
ib_check = false
cb_refresh.triggerevent("clicked")

/* If there are no rows then disable the save button
*/
IF dw_reporting_fees.rowcount() <= 0 THEN
	cb_save.enabled    = FALSE
	cb_refresh.enabled = FALSE
	cb_accept.enabled  = FALSE
END IF

end event

type dw_payment_document from u_dw_online within w_maintain_reporting_fees
boolean visible = false
integer x = 421
integer y = 1436
integer height = 360
integer taborder = 50
boolean bringtotop = true
string dataobject = "ds_payment_document"
borderstyle borderstyle = stylelowered!
end type

event constructor;this.settransobject(sqlca)
end event

event dberror;
sqlca.SQLDBCode = sqldbcode
sqlca.SQLErrText = sqlerrtext
return 1
end event

type cb_accept from commandbutton within w_maintain_reporting_fees
integer x = 1472
integer y = 1704
integer width = 343
integer height = 100
integer taborder = 40
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Accept"
end type

event clicked;/* On the click of this button, all documents clicked 'ON', will be marked as approved.
   (Set the ELIGIBLE_REPORT_FEE_DOCUMENTS.reporting_fee_eligibility_code = "APP"). 
	All documents clicked off must have their status set based on 2.5 above).
	- All documents set to a rejeted status  must have a record written  to 
	  PAYMENT_DOCUMENT. see chart F of Project 10127 - Medical Society Agreement
	  Detail dasign - Phase 2 version 2
	  
	 *** Fields included - doc_id, payment_no, paid_status_code, paid_status_explanation_code ***
	  
	- If the document is not accepted, then a rejected reason must be selected.
	  Rejected reasons are 'DNC - Document Not Complete' and 
	  'DNL - Document Not Legible' 
*/

LONG			ll_rowcount,ll_counter,ll_doc_id,ll_find_rowcount

INT			li_accepted_value,li_row,li_error,li_find, li_rtn

STRING		ls_eligibility_code
DATETIME		ldt_current

N_PROCESS_RUN_STATUS ln_process_run_status

/******************************************************************************************
P10275 - Daytime Payment Processing
- added new function call to prevent updating of tables used by PRODSVCS Payment Processing
- new object N_PROCESS_RUN_STATUS is used to determine status of processes (in progress = Y/N)
- '016' refers to the Maintain NBMS/NBCA Reporting Fees module, '044' refers to the Payment Processing module
******************************************************************************************/
ln_process_run_status = Create N_PROCESS_RUN_STATUS

li_rtn = ln_process_run_status.nf_in_progress('016','044','acceptance of fees',SQLCA)

IF li_rtn = 1 THEN
	// module is blocked
	return
END IF
/******************************************************************************************/


/* set the pointer
*/
setpointer(hourglass!)

/* do some quick checks before we proceed
*/
IF dw_reporting_fees.rowcount() <= 0 THEN
	RETURN
END IF

ldt_current = f_server_datetime()

/* go through each of the rows in dw_reporting_fees and 
   if the row is accepted we will add it too our datastore based 
	on then Set the ELIGIBLE_REPORT_FEE_DOCUMENTS.reporting_fee_eligibility_code = "APP"
ELSE
	we will write a record to our datastore based on PAYMENT_DOCUMENT as illustrated in
	the header information.
*/
ll_rowcount      = dw_reporting_fees.rowcount()
ll_find_rowcount = dw_payment_find.rowcount()


FOR ll_counter = 1 TO ll_rowcount
	li_accepted_value    = dw_reporting_fees.getitemnumber(ll_counter,"accepted")
	ll_doc_id            = dw_reporting_fees.getitemnumber(ll_counter,"docid")
	ls_eligibility_code  = dw_reporting_fees.getitemstring(ll_counter,"eligibility_code")
	
	/* do a quick validation on everything to make sure that we don't
	   have any validation problems.
	*/
	IF li_accepted_value = 1 AND ls_eligibility_code <> "ELG" THEN
			messagebox("Validation Error","If the document is accepted then the eligibility code must be 'ELG'",stopsign!)
			dw_reporting_fees.setrow(ll_counter)
			dw_reporting_fees.setcolumn("eligibility_code")
			dw_reporting_fees.setfocus()
			RETURN
	ELSEIF li_accepted_value = 0 AND  (ls_eligibility_code <> "DNC" AND ls_eligibility_code <> "DNL") THEN
			messagebox("Validation Error","If the document is not accepted then the eligibility code must be either 'DNC' or 'DNL'",stopsign!)
			dw_reporting_fees.setrow(ll_counter)
			dw_reporting_fees.setcolumn("eligibility_code")
			dw_reporting_fees.setfocus()
			RETURN
	END IF
	
	IF li_accepted_value = 1 THEN//setitem to the eligibility code "APP"
		IF ls_eligibility_code <> "ELG" THEN
			messagebox("Validation Error","If the document is accepted then the eligibility code must be 'ELG'",stopsign!)
			dw_reporting_fees.setrow(ll_counter)
			dw_reporting_fees.setcolumn("eligibility_code")
			dw_reporting_fees.setfocus()
			RETURN
		END IF
	ELSE
		IF ls_eligibility_code <> "DNC" AND ls_eligibility_code <> "DNL" THEN
			messagebox("Validation Error","If the document is not accepted then the eligibility code must be either 'DNC' or 'DNL'",stopsign!)
			dw_reporting_fees.setrow(ll_counter)
			dw_reporting_fees.setcolumn("eligibility_code")
			dw_reporting_fees.setfocus()
			RETURN
		END IF
	END IF
NEXT

FOR ll_counter = 1 TO ll_rowcount
	li_accepted_value    = dw_reporting_fees.getitemnumber(ll_counter,"accepted")
	ll_doc_id            = dw_reporting_fees.getitemnumber(ll_counter,"docid")
	ls_eligibility_code  = dw_reporting_fees.getitemstring(ll_counter,"eligibility_code")
	
	IF ll_find_rowcount > 0 THEN
		li_find = dw_payment_find.find("doc_id = "+ string(ll_doc_id),0,ll_find_rowcount)
	END IF
	
	IF li_accepted_value = 1 THEN//setitem to the eligibility code "APP"
		dw_reporting_fees.setitem(ll_counter,"eligibility_code","APP")	
		dw_reporting_fees.setitem(ll_counter,"approved_date",ldt_current)
		dw_reporting_fees.setitem(ll_counter,"approved_user_id",vgst_user_profile.user_id)
	ELSE
		IF li_find > 0 THEN
			//do nothing
		ELSE
			li_row = dw_payment_document.insertrow(0)
			dw_payment_document.setitem(li_row,"doc_id",ll_doc_id)
			dw_payment_document.setitem(li_row,"payment_no",0)
			dw_payment_document.setitem(li_row,"paid_status_code","R")
			
			/*If the rejected reason is 'Document not complete' 
			(ELIGIBLE_REPORT_FEE_DOCUMENTS.reporting_fee_eligibility_code = 'DNC'), then use '12'. 
			If the rejected reason is 'Document not legible' 
			(ELIGIBLE_REPORT_FEE_DOCUMENTS.reporting_fee_eligibility_code = 'DNL'), then use '13'.
			*/
			IF ls_eligibility_code = "DNC" THEN
				dw_payment_document.setitem(li_row,"paid_status_explanation_code","12")
			ELSEIF ls_eligibility_code = "DNL" THEN
				dw_payment_document.setitem(li_row,"paid_status_explanation_code","13")
			ELSE
			END IF	
		END IF
	END IF
NEXT


SQLCA.nf_begin_transaction()

/* now lets do our updates on the datawindows
*/
li_error = dw_reporting_fees.update()
IF SQLCA.nf_handle_error("w_maintain_reporting_fees","cb_accept_click","dw_reporting_fees.update()") < 0 THEN
	Return -1
END IF

/* remember to check the update 
*/
li_error = dw_payment_document.update()
IF SQLCA.nf_handle_error("w_maintain_reporting_fees","cb_accept_click","dw_payment_document.update()")  < 0 THEN
	Return -1
END IF

/* If everything goes alright we should be able to commit the transactions
*/
SQLCA.nf_commit_transaction()


/* now trigger the refresh event
*/
ib_check = false
cb_refresh.triggerevent("clicked")

/* If there are no rows then disable the save button
*/
IF dw_reporting_fees.rowcount() <= 0 THEN
	cb_save.enabled    = FALSE
	cb_refresh.enabled = FALSE
	THIS.enabled       = FALSE
END IF

end event

type dw_payment_find from u_dw_online within w_maintain_reporting_fees
boolean visible = false
integer x = 50
integer y = 1676
integer height = 184
integer taborder = 11
boolean bringtotop = true
string dataobject = "d_payment_document_find"
end type

