$PBExportHeader$w_med_aid_vouchers.srw
$PBExportComments$Medical Aid Vouchers
forward
global type w_med_aid_vouchers from window
end type
type dw_cheque_batch_history_invisible from u_dw_online within w_med_aid_vouchers
end type
type cb_searchprovider from commandbutton within w_med_aid_vouchers
end type
type st_1 from statictext within w_med_aid_vouchers
end type
type cb_search from commandbutton within w_med_aid_vouchers
end type
type rb_service_provider from radiobutton within w_med_aid_vouchers
end type
type rb_cheque_number from radiobutton within w_med_aid_vouchers
end type
type rb_claim_number from radiobutton within w_med_aid_vouchers
end type
type cb_print_batch from commandbutton within w_med_aid_vouchers
end type
type cb_preview from commandbutton within w_med_aid_vouchers
end type
type cb_next from commandbutton within w_med_aid_vouchers
end type
type cb_prior from commandbutton within w_med_aid_vouchers
end type
type dw_processed_date from u_dw_online within w_med_aid_vouchers
end type
type cb_print_voucher from commandbutton within w_med_aid_vouchers
end type
type dw_med_aid_voucher from u_dw_online within w_med_aid_vouchers
end type
type dw_ma_cheque_batch_history from u_dw_online within w_med_aid_vouchers
end type
type cb_close from commandbutton within w_med_aid_vouchers
end type
type gb_search_by from groupbox within w_med_aid_vouchers
end type
end forward

global type w_med_aid_vouchers from window
integer x = 1851
integer width = 2738
integer height = 2092
boolean titlebar = true
string title = "Medical Aid Vouchers"
string menuname = "m_cmwb_notools"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 67108864
event ue_open ( )
dw_cheque_batch_history_invisible dw_cheque_batch_history_invisible
cb_searchprovider cb_searchprovider
st_1 st_1
cb_search cb_search
rb_service_provider rb_service_provider
rb_cheque_number rb_cheque_number
rb_claim_number rb_claim_number
cb_print_batch cb_print_batch
cb_preview cb_preview
cb_next cb_next
cb_prior cb_prior
dw_processed_date dw_processed_date
cb_print_voucher cb_print_voucher
dw_med_aid_voucher dw_med_aid_voucher
dw_ma_cheque_batch_history dw_ma_cheque_batch_history
cb_close cb_close
gb_search_by gb_search_by
end type
global w_med_aid_vouchers w_med_aid_vouchers

forward prototypes
public function integer wf_get_voucher (long al_cheque_no)
public function integer wf_search_vouchers ()
end prototypes

event ue_open();Long     ll_num_rows, ll_cheque_no
Integer  li_rtn
Datetime ldt_processed_date
DatawindowChild ldwc_processed_date

SetPointer(HourGlass!)
dw_ma_cheque_batch_history.SetTransObject(SQLCA)
dw_med_aid_voucher.SetTransObject(SQLCA)
dw_cheque_batch_history_invisible.SetTransObject(SQLCA)

// Put DW in PrintPreview Mode and Zoom Out
dw_med_aid_voucher.Modify("Datawindow.Print.Preview=Yes")
dw_med_aid_voucher.Modify("Datawindow.Print.Preview.Zoom=75")

// Get all the Medical Aid Processed Dates
dw_processed_date.InsertRow(0)
li_rtn = dw_processed_date.GetChild("processed_date", ldwc_processed_date)
ldwc_processed_date.SetTransObject(SQLCA)
ll_num_rows = ldwc_processed_date.Retrieve()
li_rtn = SQLCA.nf_handle_error("w_med_aid_vouchers","","open - ldwc_processed_date.Retrieve()")
IF ll_num_rows > 0 THEN
	ldt_processed_date = ldwc_processed_date.GetItemDateTime(1, "processed_date")
	dw_processed_date.SetItem(1, "processed_date", ldt_processed_date)
END IF

end event

public function integer wf_get_voucher (long al_cheque_no);Long    ll_num_rows
Integer li_rtn

ll_num_rows = dw_med_aid_voucher.Retrieve(al_cheque_no)
li_rtn = SQLCA.nf_handle_error("w_med_aid_vouchers","","wf_get_voucher - dw_voucher_header.Retrieve(al_cheque_no)")
IF ll_num_rows > 0 THEN
	dw_med_aid_voucher.Visible = TRUE
	cb_print_voucher.Enabled = TRUE
	cb_print_batch.Enabled = TRUE
	cb_preview.Enabled = TRUE
	
	IF	cb_preview.Text = "Un-P&review" THEN
		cb_next.Enabled = TRUE
		cb_prior.Enabled = TRUE
	END IF	
ELSE
	dw_med_aid_voucher.Visible = FALSE
	cb_print_voucher.Enabled = FALSE
	cb_print_batch.Enabled = FALSE
	cb_preview.Enabled = FALSE
	MessageBox("Medical Aid Voucher Not Found", "Could not find Medical Aid Voucher for cheque number " +&
				  String(al_cheque_no) + ".~rTry another cheque or call the Helpdesk for assistance.", Information!)
	IF	cb_preview.Text = "Un-P&review" THEN
		cb_next.Enabled = FALSE
		cb_prior.Enabled = FALSE
	END IF
	RETURN -1
END IF

RETURN 1

end function

public function integer wf_search_vouchers ();// wf_search_vouchers
Long     ll_cheque_no, ll_claim_no, ll_recipient_no, ll_num_rows, ll_row
Long     ll_cheque_batch_no, ll_beg, ll_end, ll_temp
Integer  li_rtn
String   ls_select, ls_sql, ls_message, ls_select2, ls_cheque_type_desc, ls_find
Datetime ldt_processed_date
Boolean  lb_created_temp_table
DataWindowChild ldwc_cheque_batch

SetPointer(HourGlass!)

lb_created_temp_table = FALSE
dw_processed_date.AcceptText()
dw_ma_cheque_batch_history.Reset()
dw_med_aid_voucher.Reset()
dw_med_aid_voucher.Visible = FALSE

// Get the Cheque Batch Number from the DropDown
li_rtn = dw_processed_date.GetChild("processed_date", ldwc_cheque_batch)
ls_find = "processed_date = DateTime('" + String(dw_processed_date.GetItemDateTime(1, 'processed_date')) + "')"
ll_row = ldwc_cheque_batch.Find(ls_find, 1, ldwc_cheque_batch.RowCount())
IF ll_row > 0 THEN
	ll_cheque_batch_no = ldwc_cheque_batch.GetItemNumber(ll_row, "cheque_batch_no")
	ldt_processed_date = ldwc_cheque_batch.GetItemDatetime(ll_row, "processed_date")
END IF

ls_select = "SELECT CH.cheque_batch_no, CH.cheque_no, CH.cheque_amount, CH.name_on_cheque, CH.cheque_date " +& 
				"  FROM CHEQUE_BATCH_HISTORY CBH, CHEQUE_HEADER CH, CHEQUE_TXN_XREF CTX, APPLIED_CLAIM_TXN ACT " 

IF rb_cheque_number.Checked = TRUE THEN
	ll_cheque_no = dw_processed_date.GetItemNumber(1, "number")
	IF ll_cheque_no = 0 OR IsNull(ll_cheque_no) THEN
		MessageBox("Invalid Cheque Number", "Enter a valid number and try again.")
		dw_processed_date.SetColumn("number")
		dw_processed_date.SetFocus()
		RETURN -1
	END IF
	
	// See if Cheque Number exists
	SELECT cheque_no 
	  INTO :ll_temp 
	  FROM CHEQUE_HEADER 
	 WHERE cheque_no = :ll_cheque_no ;
	
	li_rtn = SQLCA.nf_handle_error("w_med_aid_vouchers","","wf_search_vouchers - SELECT cheque_no FROM CHEQUE_HEADER")
	IF li_rtn = 100 THEN
		MessageBox("Invalid Cheque Number", "Cheque Number " + String(ll_cheque_no) + " is not a valid cheque number.~r" +&
					  "Enter a valid number and try again.", Information!)
		dw_processed_date.SetColumn("number")
		dw_processed_date.SetFocus()
		RETURN -1
	END IF

	ls_select = ls_select + " WHERE CH.cheque_no = " + String(ll_cheque_no) + " " +&
									"   AND CBH.cheque_batch_no = CH.cheque_batch_no " +& 
									"   AND CBH.benefit_class_code = CH.benefit_class_code " +&
									"   AND CH.cheque_no = CTX.cheque_no " +&
									"   AND CTX.txn_no = ACT.txn_no " +&
									"   AND CH.benefit_class_code = ~"MA~" " +&
									"   AND (( CH.printed_date IS NOT NULL " +&
									"   AND CH.payment_method_code = ~"A~" ) " +&
									"   OR CH.payment_method_code = ~"H~" ) " +&
									"   AND CH.replacement_cheque_no = 0 " +&
									" GROUP BY CH.cheque_batch_no, CH.cheque_no, CH.cheque_amount, CH.name_on_cheque, " +&
									"          CH.cheque_date, ACT.cheque_print_group_code " +&
									" ORDER BY ACT.cheque_print_group_code, CH.cheque_no ASC  "
ELSEIF rb_claim_number.Checked = TRUE THEN
	ll_claim_no = dw_processed_date.GetItemNumber(1, "number")

	IF ll_claim_no = 0 OR IsNull(ll_claim_no) THEN
		MessageBox("Invalid Claim Number", "Enter a valid number and try again.")
		dw_processed_date.SetColumn("number")
		dw_processed_date.SetFocus()
		RETURN -1
	END IF

	// See if Claim Number Exists
	SELECT claim_no 
	  INTO :ll_temp 
	  FROM CLAIM 
	 WHERE claim_no = :ll_claim_no ;
	
	li_rtn = SQLCA.nf_handle_error("w_med_aid_vouchers","","wf_search_vouchers - SELECT claim_no FROM CLAIM")
	IF li_rtn = 100 THEN
		MessageBox("Invalid Claim Number", "Claim Number " + String(ll_claim_no) + " is not a valid claim number.~r" +&
					  "Enter a valid number and try again.", Information!)
		dw_processed_date.SetColumn("number")
		dw_processed_date.SetFocus()
		RETURN -1
	END IF

	// Create a Temporary table to cheque numbers	
	ls_sql = "CREATE TABLE #CHEQUE_NOS(cheque_no int NOT NULL)" 

	EXECUTE IMMEDIATE :ls_sql ;
	li_rtn = SQLCA.nf_handle_error("w_med_aid_vouchers","","wf_search_vouchers - EXECUTE IMMEDIATE CREATE TABLE #CHEQUE_NOS")

	lb_created_temp_table = TRUE

	INSERT #CHEQUE_NOS 
	SELECT CH.cheque_no
	  FROM CHEQUE_BATCH_HISTORY CBH,   
	       CHEQUE_HEADER CH,   
          CHEQUE_TXN_XREF CTX,  
          APPLIED_CLAIM_TXN ACT 
	 WHERE CBH.cheque_batch_no = :ll_cheque_batch_no 
	   AND ACT.claim_no = :ll_claim_no 
	   AND CBH.cheque_batch_no = CH.cheque_batch_no 
	   AND CBH.benefit_class_code = CH.benefit_class_code 
      AND CH.cheque_no = CTX.cheque_no 
      AND CTX.txn_no = ACT.txn_no 
	   AND CH.benefit_class_code = "MA"   
	   AND (( CH.printed_date IS NOT NULL 
	   AND CH.payment_method_code = "A" ) 
		OR  CH.payment_method_code = "H" ) 
	   AND CH.replacement_cheque_no = 0 
	ORDER BY CH.cheque_batch_no ASC, CH.cheque_no ASC ;

	li_rtn = SQLCA.nf_handle_error("w_med_aid_vouchers","","wf_search_vouchers - INSERT #CHEQUE_NOS")

	ls_select = ls_select + " WHERE CH.cheque_no IN (SELECT CN.cheque_no FROM #CHEQUE_NOS CN) " +&
									"   and CBH.cheque_batch_no = CH.cheque_batch_no " +& 
									"   AND CBH.benefit_class_code = CH.benefit_class_code " +&
									"   AND CH.cheque_no = CTX.cheque_no " +&
									"   AND CTX.txn_no = ACT.txn_no " +&
									"   AND CH.benefit_class_code = ~"MA~" " +&
									"   AND (( CH.printed_date IS NOT NULL " +&
									"   AND CH.payment_method_code = ~"A~" ) " +&
									"   OR CH.payment_method_code = ~"H~" ) " +&
									"   AND CH.replacement_cheque_no = 0 " +& 
									" GROUP BY CH.cheque_batch_no, CH.cheque_no, CH.cheque_amount, CH.name_on_cheque, " +&
									"          CH.cheque_date, ACT.cheque_print_group_code " +&
									" ORDER BY ACT.cheque_print_group_code, CH.cheque_no ASC  "
ELSEIF rb_service_provider.Checked = TRUE THEN
	ll_recipient_no = dw_processed_date.GetItemNumber(1, "number")
	IF ll_recipient_no = 0 OR IsNull(ll_recipient_no) THEN
		MessageBox("Invalid Recipient Number", "Enter a valid number and try again.")
		dw_processed_date.SetColumn("number")
		dw_processed_date.SetFocus()
		RETURN -1
	END IF

	ls_select = ls_select + " WHERE CBH.cheque_batch_no = " + String(ll_cheque_batch_no) + " " +&
									"   AND CBH.cheque_batch_no = CH.cheque_batch_no " +& 
									"   AND CBH.benefit_class_code = CH.benefit_class_code " +&
									"   AND CH.cheque_no = CTX.cheque_no " +& 
									"   AND CTX.txn_no = ACT.txn_no " +&
									"   AND ACT.recipient_no = " + String(ll_recipient_no) + " " +&
									"   AND CH.benefit_class_code = ~"MA~" " +&
									"   AND (( CH.printed_date IS NOT NULL " +&
									"   AND CH.payment_method_code = ~"A~" ) " +&
									"   OR CH.payment_method_code = ~"H~" ) " +&
									"   AND CH.replacement_cheque_no = 0 " +&
									" GROUP BY CH.cheque_batch_no, CH.cheque_no, CH.cheque_amount, CH.name_on_cheque, " +&
												  "CH.cheque_date, ACT.cheque_print_group_code " +&
									" ORDER BY ACT.cheque_print_group_code, CH.cheque_no ASC "
END IF

dw_ma_cheque_batch_history.Modify("Datawindow.Table.Select='" + ls_select + "'")
//MessageBox("debug", ls_select)
li_rtn = dw_ma_cheque_batch_history.SetTransObject(SQLCA)
ll_num_rows = dw_ma_cheque_batch_history.Retrieve()
li_rtn = SQLCA.nf_handle_error("w_med_aid_vouchers","","wf_search_vouchers - dw_ma_cheque_batch_history.Retrieve()")
IF ll_num_rows > 0 THEN
	dw_ma_cheque_batch_history.SelectRow(0, FALSE)
	dw_ma_cheque_batch_history.SelectRow(1, TRUE)
	dw_ma_cheque_batch_history.SetFocus()
ELSEIF ll_num_rows = 0 THEN
	// See if the reason no vouchers were found is because the cheques haven't been printed yet	
	ll_beg = Pos(ls_select, "CH.printed_date", 1)
	ll_end = Pos(ls_select, "AND", ll_beg)
	ls_select2 = Left(ls_select, ll_beg - 1) + " " + Mid(ls_select, (ll_end + 3), (Len(ls_select) - ll_end - 3))

	dw_cheque_batch_history_invisible.Modify("Datawindow.Table.Select='" + ls_select2 + "'")
	li_rtn = dw_cheque_batch_history_invisible.SetTransObject(SQLCA)
	ll_num_rows = dw_cheque_batch_history_invisible.Retrieve()
	li_rtn = SQLCA.nf_handle_error("w_med_aid_vouchers","","wf_search_vouchers - dw_cheque_batch_history_invisible.Retrieve()")
	IF rb_cheque_number.Checked = TRUE THEN
		IF ll_num_rows > 0 THEN
			MessageBox("Cheque Not Printed", "Cheque Number " + String(ll_cheque_no) + " has been processed but has " +&
						  "not been printed yet.~rMedical Aid Vouchers can Not be displayed/printed until the cheques have been printed.", Information!)
		ELSE
			SELECT CT.cheque_type_desc
			  INTO :ls_cheque_type_desc
			  FROM CHEQUE_HEADER CH, Cheque_Type CT 
			 WHERE CH.cheque_no = :ll_cheque_no 
			   AND CH.cheque_type_code = CT.cheque_type_code ;

			li_rtn = SQLCA.nf_handle_error("w_med_aid_vouchers","","wf_search_vouchers - SELECT cheque_type_desc FROM CHEQUE_HEADER CH, Cheque_Type CT ")

			ls_message = "No Medical Aid Voucher(s) found where Cheque Number = " + String(ll_cheque_no) + ".~r" +&
							 "Cheque Number " + String(ll_cheque_no) + " is a " + ls_cheque_type_desc + " type cheque."
			MessageBox("None Found", ls_message)
		END IF
	ELSEIF rb_claim_number.Checked = TRUE THEN
		IF ll_num_rows > 0 THEN
			MessageBox("Cheques Not Printed", "The cheques with the following:~r~tClaim Number = " + String(ll_claim_no) +&
						  "~r~tProcessed Date = " + String(ldt_processed_date, "mmm dd, yyyy") +&
						  "have been processed but have not been printed yet.~r" +&
						  "Medical Aid Vouchers can Not be displayed/printed until the cheques have been printed.", Information!)
		ELSE
			ls_message = "No Medical Aid Voucher(s) found with the following:~r~tClaim Number = " + String(ll_claim_no) +&
							 "~r~tProcessed Date = " + String(ldt_processed_date, "mmm dd, yyyy") +&
							 "~r~tCheque Batch Number = " + String(ll_cheque_batch_no)
			MessageBox("None Found", ls_message)
		END IF
	ELSEIF rb_service_provider.Checked = TRUE THEN
		IF ll_num_rows > 0 THEN
			MessageBox("Cheques Not Printed", "The cheques with the following:~r~tService Provider " +&
						  String(ll_recipient_no) + ".~r~tProcessed Date = " + String(ldt_processed_date, "mmm dd, yyyy") +&
						  "have been processed but have not been printed yet.~rMedical Aid Vouchers can Not be " +&
						  "displayed/printed until the cheques have been printed.", Information!)
		ELSE
			ls_message = "No Medical Aid Voucher(s) found with the following:~r~tService Provider " + String(ll_recipient_no) + "." +&
						 "~r~tProcessed Date = " + String(ldt_processed_date, "mmm dd, yyyy") +&
						 "~r~tCheque Batch Number = " + String(ll_cheque_batch_no) + "."
			MessageBox("None Found", ls_message)
		END IF
	END IF
END IF

// Get rid of Temp table if one was created
IF lb_created_temp_table = TRUE THEN
	ls_sql = "DROP TABLE #CHEQUE_NOS" 

	EXECUTE IMMEDIATE :ls_sql ;
	li_rtn = SQLCA.nf_handle_error("w_med_aid_vouchers","","wf_search_vouchers - EXECUTE IMMEDIATE DROP TABLE #CHEQUE_NOS")
	
END IF

RETURN 1
end function

on w_med_aid_vouchers.create
if this.MenuName = "m_cmwb_notools" then this.MenuID = create m_cmwb_notools
this.dw_cheque_batch_history_invisible=create dw_cheque_batch_history_invisible
this.cb_searchprovider=create cb_searchprovider
this.st_1=create st_1
this.cb_search=create cb_search
this.rb_service_provider=create rb_service_provider
this.rb_cheque_number=create rb_cheque_number
this.rb_claim_number=create rb_claim_number
this.cb_print_batch=create cb_print_batch
this.cb_preview=create cb_preview
this.cb_next=create cb_next
this.cb_prior=create cb_prior
this.dw_processed_date=create dw_processed_date
this.cb_print_voucher=create cb_print_voucher
this.dw_med_aid_voucher=create dw_med_aid_voucher
this.dw_ma_cheque_batch_history=create dw_ma_cheque_batch_history
this.cb_close=create cb_close
this.gb_search_by=create gb_search_by
this.Control[]={this.dw_cheque_batch_history_invisible,&
this.cb_searchprovider,&
this.st_1,&
this.cb_search,&
this.rb_service_provider,&
this.rb_cheque_number,&
this.rb_claim_number,&
this.cb_print_batch,&
this.cb_preview,&
this.cb_next,&
this.cb_prior,&
this.dw_processed_date,&
this.cb_print_voucher,&
this.dw_med_aid_voucher,&
this.dw_ma_cheque_batch_history,&
this.cb_close,&
this.gb_search_by}
end on

on w_med_aid_vouchers.destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.dw_cheque_batch_history_invisible)
destroy(this.cb_searchprovider)
destroy(this.st_1)
destroy(this.cb_search)
destroy(this.rb_service_provider)
destroy(this.rb_cheque_number)
destroy(this.rb_claim_number)
destroy(this.cb_print_batch)
destroy(this.cb_preview)
destroy(this.cb_next)
destroy(this.cb_prior)
destroy(this.dw_processed_date)
destroy(this.cb_print_voucher)
destroy(this.dw_med_aid_voucher)
destroy(this.dw_ma_cheque_batch_history)
destroy(this.cb_close)
destroy(this.gb_search_by)
end on

event resize;dw_med_aid_voucher.height = This.height - 810
dw_med_aid_voucher.width = This.width - 50

cb_next.y = This.height - 400
cb_prior.y = This.height - 400
cb_close.y = This.height - 400
cb_preview.y = This.height - 400
cb_print_voucher.y = This.height - 400
cb_print_batch.y = This.height - 400

end event

event open;INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


PostEvent("ue_open")
end event

type dw_cheque_batch_history_invisible from u_dw_online within w_med_aid_vouchers
boolean visible = false
integer x = 338
integer y = 1736
integer width = 110
integer height = 124
integer taborder = 81
boolean enabled = false
string dataobject = "d_ma_cheque_batch_history"
borderstyle borderstyle = stylelowered!
end type

type cb_searchprovider from commandbutton within w_med_aid_vouchers
boolean visible = false
integer x = 416
integer y = 272
integer width = 87
integer height = 72
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "?"
end type

event clicked;S_WINDOW_MESSAGE	lstr_message
STRING 				ls_type, ls_service_provider_name, ls_comment
BOOLEAN				lb_account_visible
LONG					ll_row
/*	get the type to search for
*/

ls_type = 'M'
OpenWithParm(w_service_provider_search, ls_type)
lstr_message = Message.PowerObjectParm
dw_processed_date.SetColumn('number')
dw_processed_date.SetItem(1,'number', lstr_message.al_doubleparm[1])

		
end event

type st_1 from statictext within w_med_aid_vouchers
integer x = 91
integer y = 276
integer width = 247
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Provider"
boolean focusrectangle = false
end type

type cb_search from commandbutton within w_med_aid_vouchers
integer x = 663
integer y = 48
integer width = 283
integer height = 96
integer taborder = 100
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Search"
boolean default = true
end type

event clicked;wf_search_vouchers()

end event

type rb_service_provider from radiobutton within w_med_aid_vouchers
integer x = 23
integer y = 216
integer width = 462
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
string text = "Medical Aid"
end type

event clicked;dw_processed_date.Modify("processed_date.background.color='16777215' processed_date.protect='0'")
dw_processed_date.Modify("number.background.color='16777215'         number.protect='0'")
dw_processed_date.Modify("number_text.text='Recipient Number:'")
IF dw_processed_date.RowCount() > 0 THEN
	dw_processed_date.SetItem(1, "number", 0)
END IF

dw_processed_date.SetColumn("number")
dw_processed_date.SetFocus()

dw_ma_cheque_batch_history.Reset()
dw_med_aid_voucher.Reset()
dw_med_aid_voucher.Visible = FALSE

cb_searchprovider.Visible = TRUE
end event

type rb_cheque_number from radiobutton within w_med_aid_vouchers
integer x = 23
integer y = 144
integer width = 507
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
string text = "Cheque Number"
end type

event clicked;dw_processed_date.Modify("processed_date.background.color='67108864' processed_date.protect='1'")
dw_processed_date.Modify("number.background.color='16777215'         number.protect='0'")
dw_processed_date.Modify("number_text.text='Cheque Number:'")
IF dw_processed_date.RowCount() > 0 THEN
	dw_processed_date.SetItem(1, "number", 0)
END IF

dw_processed_date.SetColumn("number")
dw_processed_date.SetFocus()

dw_ma_cheque_batch_history.Reset()
dw_med_aid_voucher.Reset()
dw_med_aid_voucher.Visible = FALSE

cb_searchprovider.Visible = FALSE


end event

type rb_claim_number from radiobutton within w_med_aid_vouchers
integer x = 23
integer y = 72
integer width = 507
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
string text = "Claim Number"
end type

event clicked;dw_processed_date.Modify("processed_date.background.color='16777215' processed_date.protect='0'")
dw_processed_date.Modify("number.background.color='16777215'         number.protect='0'")
dw_processed_date.Modify("number_text.text='Claim Number:'")
IF dw_processed_date.RowCount() > 0 THEN
	dw_processed_date.SetItem(1, "number", 0)
END IF

dw_processed_date.SetColumn("number")
dw_processed_date.SetFocus()

dw_ma_cheque_batch_history.Reset()
dw_med_aid_voucher.Reset()
dw_med_aid_voucher.Visible = FALSE

cb_searchprovider.Visible = FALSE

end event

type cb_print_batch from commandbutton within w_med_aid_vouchers
boolean visible = false
integer x = 1349
integer y = 1808
integer width = 398
integer height = 92
integer taborder = 80
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Print &Batch"
end type

event clicked;Long    n, ll_num_rows, ll_row, ll_cur_cheque_no, ll_cheque_no
Integer li_rtn

// Get the current Cheque being displayed
ll_row = dw_ma_cheque_batch_history.GetRow()
IF ll_row > 0 THEN
	ll_cur_cheque_no = dw_ma_cheque_batch_history.GetItemNumber(ll_row, "cheque_no")
END IF

// Loop through and print all the vouchers
ll_num_rows = dw_ma_cheque_batch_history.RowCount()
FOR n = 1 TO ll_num_rows
	ll_cheque_no = dw_ma_cheque_batch_history.GetItemNumber(n, "cheque_no")
	li_rtn = wf_get_voucher(ll_cheque_no)
	IF li_rtn = 1 THEN
		dw_med_aid_voucher.Print()
	END IF
NEXT

// Display the voucher that was originally being displayed
IF ll_cur_cheque_no > 0 THEN
	wf_get_voucher(ll_cur_cheque_no)
END IF
end event

type cb_preview from commandbutton within w_med_aid_vouchers
integer x = 1774
integer y = 1704
integer width = 398
integer height = 92
integer taborder = 90
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Un-P&review"
end type

event clicked;// Put DW in PrintPreview Mode and Zoom Out

IF cb_preview.Text = "P&review" THEN
	dw_med_aid_voucher.Modify("Datawindow.Print.Preview=Yes")
	dw_med_aid_voucher.Modify("Datawindow.Print.Preview.Zoom=75")
	cb_preview.Text = "Un-P&review"
	cb_next.Enabled = TRUE
	cb_prior.Enabled = TRUE
ELSE
	dw_med_aid_voucher.Modify("Datawindow.Print.Preview=No")
	dw_med_aid_voucher.Modify("Datawindow.Print.Preview.Zoom=75")
	cb_preview.Text = "P&review"
	cb_next.Enabled = FALSE
	cb_prior.Enabled = FALSE
END IF

end event

type cb_next from commandbutton within w_med_aid_vouchers
integer x = 1024
integer y = 1704
integer width = 87
integer height = 92
integer taborder = 60
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = ">"
end type

event clicked;dw_med_aid_voucher.ScrollNextPage()
end event

type cb_prior from commandbutton within w_med_aid_vouchers
integer x = 887
integer y = 1704
integer width = 87
integer height = 92
integer taborder = 70
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "<"
end type

event clicked;dw_med_aid_voucher.ScrollPriorPage()
end event

type dw_processed_date from u_dw_online within w_med_aid_vouchers
integer x = 539
integer y = 156
integer width = 512
integer height = 208
integer taborder = 110
string dataobject = "d_processed_date"
boolean border = false
end type

type cb_print_voucher from commandbutton within w_med_aid_vouchers
integer x = 1335
integer y = 1704
integer width = 398
integer height = 92
integer taborder = 120
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Print Voucher"
end type

event clicked;dw_med_aid_voucher.Print()
end event

type dw_med_aid_voucher from u_dw_online within w_med_aid_vouchers
boolean visible = false
integer x = 14
integer y = 392
integer width = 2624
integer height = 1292
integer taborder = 10
string dataobject = "d_med_aid_voucher"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

type dw_ma_cheque_batch_history from u_dw_online within w_med_aid_vouchers
integer x = 1093
integer width = 1545
integer height = 380
integer taborder = 40
string dataobject = "d_ma_cheque_batch_history"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event rowfocuschanged;call super::rowfocuschanged;Long    ll_num_rows, ll_cheque_no
Integer li_rtn

IF currentrow = 0 OR IsNull(currentrow) = TRUE OR This.RowCount() = 0 THEN
	RETURN
END IF

This.SelectRow(0, FALSE)
This.SelectRow(currentrow, TRUE)
This.SetFocus()

ll_cheque_no = This.GetItemNumber(currentrow, "cheque_no")
IF ll_cheque_no > 0 THEN
	wf_get_voucher(ll_cheque_no)
END IF

end event

type cb_close from commandbutton within w_med_aid_vouchers
integer x = 2213
integer y = 1704
integer width = 398
integer height = 92
integer taborder = 50
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Close"
end type

event clicked;Close(Parent)
end event

type gb_search_by from groupbox within w_med_aid_vouchers
integer width = 1065
integer height = 372
integer taborder = 30
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
string text = "Search By:"
end type

