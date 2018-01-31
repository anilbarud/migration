$PBExportHeader$w_inbasket_filter.srw
forward
global type w_inbasket_filter from window
end type
type dw_inbasket_preadj_67_account from u_dw_online within w_inbasket_filter
end type
type dw_inbasket_preadj_ready from u_dw_online within w_inbasket_filter
end type
type dw_inbasket_document_flag from u_dw_online within w_inbasket_filter
end type
type cb_ok from commandbutton within w_inbasket_filter
end type
type cb_cancel from commandbutton within w_inbasket_filter
end type
type dw_inbasket_filter_list from u_dw_online within w_inbasket_filter
end type
end forward

global type w_inbasket_filter from window
integer x = 667
integer y = 320
integer width = 2226
integer height = 1272
boolean titlebar = true
string title = "Filter Folder List"
windowtype windowtype = response!
long backcolor = 67108864
dw_inbasket_preadj_67_account dw_inbasket_preadj_67_account
dw_inbasket_preadj_ready dw_inbasket_preadj_ready
dw_inbasket_document_flag dw_inbasket_document_flag
cb_ok cb_ok
cb_cancel cb_cancel
dw_inbasket_filter_list dw_inbasket_filter_list
end type
global w_inbasket_filter w_inbasket_filter

type variables
INTEGER			il_category_id_nmbr
LONG			il_inbasket_filter_cnt
LONG			il_days_after
STRING			is_row_filter
STRING			is_filter_choice
DATAWINDOWCHILD	idw_action_child,idwc_claim_status
DATAWINDOWCHILD	idwc_claim_status_type

end variables

forward prototypes
private function integer wf_inbasket_filter_folder_rows ()
end prototypes

private function integer wf_inbasket_filter_folder_rows ();//	This function sets up and filters rows in the datawindow dw_inbasket_folder_list.
//	Returns:		an integer.

LONG		ll_results, ll_counter, ll_fldid, ll_claim_no
DATE		ld_from, ld_to, ld_bringfwddate
BOOLEAN	lb_first
STRING	ls_string, ls_string2

SetPointer(HourGlass!)

IF w_inbasket_filter.dw_inbasket_filter_list.AcceptText() = -1 THEN
	RETURN -1
END IF

//	Set up search on datawindow
is_filter_choice = w_inbasket_filter.dw_inbasket_filter_list.GetItemString(1,"filter_choice")
is_row_filter    = ""

CHOOSE CASE is_filter_choice
	CASE "A"							// "ACTION_CODE"
		IF IsNull(w_inbasket_filter.dw_inbasket_filter_list.GetItemString(1,"action_codes")) OR w_inbasket_filter.dw_inbasket_filter_list.GetItemString(1,"action_codes") = "" THEN
			is_row_filter = ""
		ELSE
			is_row_filter = "action_code = '" + w_inbasket_filter.dw_inbasket_filter_list.GetItemString(1,"action_codes") + "'"
		END IF

	CASE "B"							// "CLAIM_NO"
		IF IsNull(w_inbasket_filter.dw_inbasket_filter_list.GetItemNumber(1,"start_claim_no")) OR IsNull(w_inbasket_filter.dw_inbasket_filter_list.GetItemNumber(1,"end_claim_no")) THEN
			is_row_filter = ""
		ELSE
			is_row_filter = 'claim_no >= ' + String(w_inbasket_filter.dw_inbasket_filter_list.GetItemNumber(1,"start_claim_no")) + ' AND claim_no <= ' &
							  + String(w_inbasket_filter.dw_inbasket_filter_list.GetItemNumber(1,"end_claim_no"))
		END IF

	CASE "C"							// "ACTION_DATE"
		IF IsNull(w_inbasket_filter.dw_inbasket_filter_list.GetItemDate(1,"start_action_date")) OR IsNull(String(w_inbasket_filter.dw_inbasket_filter_list.GetItemDate(1,"end_action_date"))) THEN
			is_row_filter = ""
		ELSE
			ld_from = w_inbasket_filter.dw_inbasket_filter_list.GetItemDate(1,"start_action_date")
			ld_to   = w_inbasket_filter.dw_inbasket_filter_list.GetItemDate(1,"end_action_date")

			IF (IsNull(ld_from) OR string(ld_from) = "0000 01 01") AND &
				(IsNull(ld_to)   OR string(ld_to)   = "0000 01 01") THEN
				is_row_filter = ""
			ELSEIF IsNull(ld_from) OR string(ld_from) = "0000 01 01" THEN
				ld_to = RelativeDate(ld_to,1)
				is_row_filter = "(action_date < " + string(ld_to,'yyyy-mm-dd') + ")"
			ELSEIF IsNull(ld_to) OR string(ld_to) = "0000 01 01"  THEN
				is_row_filter = "(action_date >= " + string(ld_from,'yyyy-mm-dd') + ")"
			ELSE
				IF ld_from > ld_to THEN
					MessageBox("Validation Error","The start action date cannot be after the end action date")
					RETURN -1
				END IF
				ld_to = RelativeDate(ld_to,1)
				is_row_filter = "(action_date >= " + string(ld_from,'yyyy-mm-dd') + " and action_date < " + string(ld_to,'yyyy-mm-dd') + ")"
			END IF
		END IF

	CASE "D"   // "KEYWORD"
		IF IsNull(w_inbasket_filter.dw_inbasket_filter_list.GetItemString(1,"action_note")) OR Trim(w_inbasket_filter.dw_inbasket_filter_list.GetItemString(1,"action_note")) = "" THEN
			is_row_filter = ""
		ELSE
			is_row_filter = "Match(Upper(action_note),'" + Trim(w_inbasket_filter.dw_inbasket_filter_list.GetItemString(1,"action_note")) + "')"
		END IF

	CASE "E"  // "ENGLISH ONLY"
		dw_inbasket_document_flag.Retrieve(il_category_id_nmbr)
		IF ImageTrans.nf_handle_error("dw_inbasket_document_flag","w_inbasket","wf_inbasket_filter_folder_rows") < 0 THEN
			RETURN -1
		END IF

		// If they only want to see folders that have documents marked as english then remove the
		// ones where the english_flag is 'N'.
		IF w_inbasket_filter.dw_inbasket_filter_list.GetItemString(1,"english_flag") = "Y" THEN
			ll_counter = 1
			lb_first   = TRUE
			DO WHILE ll_counter <= dw_inbasket_document_flag.RowCount()
				ll_fldid = dw_inbasket_document_flag.GetItemNumber(ll_counter,"fldid")
				IF dw_inbasket_document_flag.GetItemString(ll_counter,"english_flag") = "N" THEN
					IF lb_first THEN
						is_row_filter = "folderid <> " + String(ll_fldid)
						lb_first = FALSE
					ELSE
						is_row_filter = is_row_filter + " And " + "folderid <> " + String(ll_fldid)
					END IF
				END IF
				ll_counter++
			LOOP
		ELSE
			// If they only want to see folders that have documents not marked as english then remove the
			// ones where the english_flag is 'Y'.
			ll_counter = 1
			lb_first   = TRUE
			DO WHILE ll_counter <= dw_inbasket_document_flag.RowCount()
				ll_fldid = dw_inbasket_document_flag.GetItemNumber(ll_counter,"fldid")
				IF dw_inbasket_document_flag.GetItemString(ll_counter,"english_flag") = "Y" THEN
					IF lb_first THEN
						is_row_filter = "folderid <> " + String(ll_fldid)
						lb_first = FALSE
					ELSE
						is_row_filter = is_row_filter + " And folderid <> " + String(ll_fldid)
					END IF
				END IF
				ll_counter++
			LOOP
		END IF

	CASE "F" // PRE-ADJUDICATION READY
		// This one requires that only the claims at the pre-adjudication stage be shown and further only
		// the ones that their file contains a Form 67 (SE) and Medical ('SD','SDC','SDD','MP','MPC','MPD','MH').
		//
		// First get rid of the folders that aren't at pre-adjudication stage.
		is_row_filter = "(claim_status_code = 'P')"

		// Now only keep the claims that have the required documents.
		dw_inbasket_preadj_ready.Retrieve(il_category_id_nmbr)
		IF ImageTrans.nf_handle_error("dw_inbasket_preadj_ready","w_inbasket","wf_inbasket_filter_folder_rows") < 0 THEN
			RETURN -1
		END IF

		// If no rows returned then none are ready so empty the folder list.
		IF dw_inbasket_preadj_ready.RowCount() = 0 THEN
			is_row_filter = "claim_status_code = 'ZZ'"
		END IF
		ll_counter = 1
		lb_first = TRUE
		DO WHILE ll_counter <= dw_inbasket_preadj_ready.RowCount()
			ll_claim_no = dw_inbasket_preadj_ready.GetItemNumber(ll_counter,"claim_no")
			IF lb_first THEN
				is_row_filter = is_row_filter + "And (claim_no = " + String(ll_claim_no)
				lb_first = FALSE
			ELSE
				is_row_filter = is_row_filter + " Or claim_no = " + String(ll_claim_no)
			END IF
			ll_counter++
		LOOP
		IF NOT lb_first THEN
			is_row_filter = is_row_filter + ")"
		END IF

	CASE "G" // PRE-ADJUDICATION NOT READY
		// This one requires that only the claims at the pre-adjudication stage be shown and further only
		// the ones that their file is missing either a Form 67 (SE) or Medical ('SD','SDC','SDD','MP','MPC','MPD','MH').
		//
		// First get rid of the folders that aren't at pre-adjudication stage.
		is_row_filter = "claim_status_code = 'P'"

		// Now only keep the claims that don't have the required documents.
		dw_inbasket_preadj_ready.Retrieve(il_category_id_nmbr)
		IF ImageTrans.nf_handle_error("dw_inbasket_preadj_ready","w_inbasket","wf_inbasket_filter_folder_rows") < 0 THEN
			RETURN -1
		END IF
		ll_counter = 1
		DO WHILE ll_counter <= dw_inbasket_preadj_ready.RowCount()
			ll_claim_no = dw_inbasket_preadj_ready.GetItemNumber(ll_counter,"claim_no")
			is_row_filter = is_row_filter + " And claim_no <> " + String(ll_claim_no)
			ll_counter++
		LOOP

		// And remove the folders that are under the number of days old the user has specified.
		il_days_after = w_inbasket_filter.dw_inbasket_filter_list.GetItemNumber(1,"not_ready_days_old")

	CASE "H" // PRE-ADJUDICATION WITH FORM 67 AND ACCOUNT 

		// This one requires that only the claims at the pre-adjudication stage be shown and further only
		// the ones that their file contains a Form 67 (SE) and Medical ('SD','SDC','SDD','MP','MPC','MPD','MH')
		// or Account ('A%','SDC','SDD','MPC','MPD').
		//
		// First get rid of the folders that aren't at pre-adjudication stage.
		is_row_filter = "(claim_status_code = 'P')"

		//  Now only keep the claims that have the required documents.
		dw_inbasket_preadj_67_account.Retrieve(il_category_id_nmbr)
		IF ImageTrans.nf_handle_error("dw_inbasket_preadj_67_account","w_inbasket","wf_inbasket_filter_folder_rows") < 0 THEN
			RETURN -1
		END IF

		// If no rows returned then none are ready so empty the folder list.
		IF dw_inbasket_preadj_67_account.RowCount() = 0 THEN
			is_row_filter = "claim_status_code = 'ZZ'"
		END IF
		ll_counter = 1
		lb_first = TRUE
		DO WHILE ll_counter <= dw_inbasket_preadj_67_account.RowCount()
			ll_claim_no = dw_inbasket_preadj_67_account.GetItemNumber(ll_counter,"claim_no")
			IF lb_first THEN
				is_row_filter = is_row_filter + "And (claim_no = " + String(ll_claim_no)
				lb_first = FALSE
			ELSE
				is_row_filter = is_row_filter + " Or claim_no = " + String(ll_claim_no)
			END IF
			ll_counter++
		LOOP
		IF NOT lb_first THEN
			is_row_filter = is_row_filter + ")"
		END IF

		// And remove the folders that are under the number of days old the user has specified.
		il_days_after = w_inbasket_filter.dw_inbasket_filter_list.GetItemNumber(1,"not_ready_days_old")

	CASE 'I'
		ls_string2 = dw_inbasket_filter_list.GetItemString(1,'claim_status_type_code')
		ls_string = dw_inbasket_filter_list.GetItemString(1,'claim_status_code')
		IF ls_string = '' OR IsNull(ls_string) THEN
			MessageBox('No Status','The claim status for the filter is not specified.')	
			RETURN -1
		ELSE
			is_row_filter = 'claim_status_code = "' + ls_string + '"'
			IF ls_string2 <> '' AND NOT IsNull(ls_string2) THEN
				is_row_filter = is_row_filter + ' and claim_status_type_code = "' + ls_string2 + '"'
			END IF
		END IF

	CASE 'J'
		IF IsNull(w_inbasket_filter.dw_inbasket_filter_list.GetItemDate(1,"bring_forward_date")) THEN
			is_row_filter = ""
		ELSE
			ld_bringfwddate = w_inbasket_filter.dw_inbasket_filter_list.GetItemDate(1,"bring_forward_date")
			IF (IsNull(ld_bringfwddate) or string(ld_bringfwddate) = "0000 01 01") THEN
				is_row_filter = ""
			ELSE
				is_row_filter = "(bring_fwd_date <= " + string(ld_bringfwddate,'yyyy-mm-dd') + ")"
			END IF
		END IF

	CASE ELSE
		MessageBox("","Currently unavailable.")
		is_row_filter = ""
END CHOOSE

//	Set filter on data window and filter
il_inbasket_filter_cnt = 0

RETURN 1

end function

event open;LONG	ll_rows_loaded

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


SetPointer(HourGlass!)

il_category_id_nmbr = Message.DoubleParm

w_inbasket_filter.dw_inbasket_filter_list.Visible = TRUE

/* Set up default search by option.
*/
	w_inbasket_filter.dw_inbasket_filter_list.InsertRow(0)
	w_inbasket_filter.dw_inbasket_filter_list.SetItem(1,"start_action_date",Today())
	w_inbasket_filter.dw_inbasket_filter_list.SetItem(1,"end_action_date",Today())
	w_inbasket_filter.dw_inbasket_filter_list.SetItem(1,"bring_forward_date",Today())
	w_inbasket_filter.dw_inbasket_filter_list.SetItem(1,'claim_status_code','')
	w_inbasket_filter.dw_inbasket_filter_list.SetItem(1,'claim_status_type_code','')

/* Setup the action codes.
*/
	w_inbasket_filter.dw_inbasket_filter_list.GetChild("action_codes",idw_action_child)
	idw_action_child.SetTransObject(ImageTrans)
	ll_rows_loaded = idw_action_child.Retrieve()
	IF ImageTrans.nf_handle_error("viw_action_child","w_inbasket","Open") < 0 THEN
		cb_cancel.TriggerEvent(Clicked!)
		RETURN
	END IF

/* If no rows found then problem!
*/
	IF ll_rows_loaded <= 0 THEN
	   MessageBox("In-Basket", "No action codes found for folders. Please call the Helpdesk with this message", StopSign!, OK!)
		cb_cancel.TriggerEvent(Clicked!)
		Return
	END IF

	w_inbasket_filter.dw_inbasket_filter_list.GetChild("claim_status_code",idwc_claim_status)
	idwc_claim_status.SetTransObject(SQLCA)

	ll_rows_loaded = idwc_claim_status.Retrieve()
	IF SQLCA.nf_handle_error("claim status retrieve","w_inbasket","Open") < 0 THEN
		cb_cancel.TriggerEvent(Clicked!)
		RETURN
	END IF

/* If no rows found then problem!
*/
	IF ll_rows_loaded <= 0 THEN
	   MessageBox("In-Basket", "No claim status codes found. Please call the Helpdesk with this message",StopSign!,OK!)
		cb_cancel.TriggerEvent(Clicked!)
		Return
	END IF

	w_inbasket_filter.dw_inbasket_filter_list.GetChild("claim_status_type_code",idwc_claim_status_type)
	idwc_claim_status_type.SetTransObject(SQLCA)

	ll_rows_loaded = idwc_claim_status_type.Retrieve()
	IF SQLCA.nf_handle_error("claim status type retrieve","w_inbasket","Open") < 0 THEN
		cb_cancel.TriggerEvent(Clicked!)
		RETURN
	END IF

/* If no rows found then problem!
*/
	IF ll_rows_loaded <= 0 THEN
	   MessageBox("In-Basket", "No claim status type codes found. Please call the Helpdesk with this message",StopSign!,OK!)
		cb_cancel.TriggerEvent(Clicked!)
		Return
	END IF
	idwc_claim_status_type.SetFilter('claim_status_code = ""')
	idwc_claim_status_type.Filter()

/* Initialize data windows.
*/
	dw_inbasket_document_flag.SetTransObject(ImageTrans)
	dw_inbasket_preadj_ready.SetTransObject(ImageTrans)
	dw_inbasket_preadj_67_account.SetTransObject(ImageTrans)

end event

on w_inbasket_filter.create
this.dw_inbasket_preadj_67_account=create dw_inbasket_preadj_67_account
this.dw_inbasket_preadj_ready=create dw_inbasket_preadj_ready
this.dw_inbasket_document_flag=create dw_inbasket_document_flag
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
this.dw_inbasket_filter_list=create dw_inbasket_filter_list
this.Control[]={this.dw_inbasket_preadj_67_account,&
this.dw_inbasket_preadj_ready,&
this.dw_inbasket_document_flag,&
this.cb_ok,&
this.cb_cancel,&
this.dw_inbasket_filter_list}
end on

on w_inbasket_filter.destroy
destroy(this.dw_inbasket_preadj_67_account)
destroy(this.dw_inbasket_preadj_ready)
destroy(this.dw_inbasket_document_flag)
destroy(this.cb_ok)
destroy(this.cb_cancel)
destroy(this.dw_inbasket_filter_list)
end on

type dw_inbasket_preadj_67_account from u_dw_online within w_inbasket_filter
boolean visible = false
integer x = 1737
integer y = 956
integer width = 137
integer height = 108
integer taborder = 40
string dataobject = "d_inbasket_preadj_67_account"
end type

type dw_inbasket_preadj_ready from u_dw_online within w_inbasket_filter
boolean visible = false
integer x = 1591
integer y = 956
integer width = 119
integer height = 104
integer taborder = 30
string dataobject = "d_inbasket_preadj_ready"
end type

type dw_inbasket_document_flag from u_dw_online within w_inbasket_filter
boolean visible = false
integer x = 1399
integer y = 940
integer width = 142
integer height = 120
integer taborder = 20
string dataobject = "d_inbasket_document_flag"
end type

type cb_ok from commandbutton within w_inbasket_filter
integer x = 791
integer y = 1024
integer width = 334
integer height = 96
integer taborder = 60
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
end type

event clicked;S_WINDOW_MESSAGE ls_message

SetPointer(HourGlass!)

wf_inbasket_filter_folder_rows()

ls_message.al_doubleparm[1] = il_inbasket_filter_cnt
ls_message.as_stringparm[1] = is_row_filter
ls_message.al_doubleparm[2] = il_days_after
ls_message.as_stringparm[2] = is_filter_choice
CloseWithReturn(w_inbasket_filter,ls_message)
end event

type cb_cancel from commandbutton within w_inbasket_filter
integer x = 1157
integer y = 1024
integer width = 334
integer height = 100
integer taborder = 50
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cance&l"
end type

on clicked;S_WINDOW_MESSAGE ls_message

ls_message.al_doubleparm[1] = 0
ls_message.as_stringparm[1] = ""
ls_message.al_doubleparm[2] = 0
ls_message.as_stringparm[2] = ""
CloseWithReturn(w_inbasket_filter,ls_message)
end on

type dw_inbasket_filter_list from u_dw_online within w_inbasket_filter
integer x = 27
integer y = 32
integer width = 2135
integer height = 972
integer taborder = 10
string dataobject = "d_inbasket_filter_list"
boolean border = false
end type

on itemchanged;STRING	ls_string

CHOOSE CASE dw_inbasket_filter_list.GetColumnName()
	CASE 'claim_status_code'

/*	Filter the data in the claim status type list.
*/
		ls_string = dw_inbasket_filter_list.GetText()
		idwc_claim_status_type.SetFilter('claim_status_code = "' + ls_string +'"')
		idwc_claim_status_type.Filter()
		dw_inbasket_filter_list.SetItem(1, 'claim_status_type_code', '')
END CHOOSE

end on

