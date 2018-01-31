$PBExportHeader$w_document_details.srw
$PBExportComments$Claim Retrieve - Window used to display more details when option selected from the document list at the bottom of the screen
forward
global type w_document_details from window
end type
type dw_file_path_name from u_dw_online within w_document_details
end type
type dw_reviewed_document from u_dw_online within w_document_details
end type
type cb_close from commandbutton within w_document_details
end type
type dw_account_details from u_dw_online within w_document_details
end type
type dw_document_indexed_date from u_dw_online within w_document_details
end type
end forward

global type w_document_details from window
integer x = 265
integer y = 352
integer width = 4187
integer height = 1844
boolean titlebar = true
string title = "Document Details"
windowtype windowtype = response!
long backcolor = 67108864
dw_file_path_name dw_file_path_name
dw_reviewed_document dw_reviewed_document
cb_close cb_close
dw_account_details dw_account_details
dw_document_indexed_date dw_document_indexed_date
end type
global w_document_details w_document_details

event open;Long    ll_docid, ll_num_rows, n, ll_payment_no
Integer li_rtn
String  ls_type_code, ls_carrier_code, ls_first_name, ls_last_name

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// Get the docid passed in
ll_docid = Message.DoubleParm

// write to the application log
f_populate_app_log(gs_appname_and_handle ,100, this.ClassName(), 'Open Event - Doc id: ' + string(ll_docid))

dw_account_details.SetTransObject(SQLCA)
dw_document_indexed_date.SetTransObject(SQLCA)
dw_reviewed_document.SetTransObject(ImageTrans)
dw_file_path_name.SetTransObject(ImageTrans)

// Populate the datawindows
ll_num_rows = dw_document_indexed_date.Retrieve(ll_docid)
li_rtn = SQLCA.nf_handle_error('w_document_details', '', 'open - dw_document_indexed_date.Retrieve(ll_docid)')
IF li_rtn < 0 THEN
	Close(This)
	RETURN
END IF

ll_num_rows = dw_reviewed_document.Retrieve(ll_docid)
li_rtn = ImageTrans.nf_handle_error('w_document_details', '', 'open - dw_reviewed_document.Retrieve(ll_docid)')
IF li_rtn < 0 THEN
	Close(This)
	RETURN
END IF

ll_num_rows = dw_file_path_name.Retrieve(ll_docid)
li_rtn = ImageTrans.nf_handle_error('w_document_details', '', 'open - dw_file_path_name.Retrieve(ll_docid)')
IF li_rtn < 0 THEN
	Close(This)
	RETURN
END IF

IF IsNull(dw_document_indexed_date.GetItemDateTime(1,"doccreated")) THEN
	dw_document_indexed_date.Modify("doccreated.Visible=0")
	dw_document_indexed_date.Modify("t_note.Visible=1")
ELSE
	dw_document_indexed_date.Modify("doccreated.Visible=1")
	dw_document_indexed_date.Modify("t_note.Visible=0")
END IF

ll_num_rows = dw_account_details.Retrieve(ll_docid)
li_rtn = SQLCA.nf_handle_error('w_document_details', '', 'open - dw_account_details.Retrieve(ll_docid)')
IF li_rtn < 0 THEN
	Close(This)
	Return
END IF

IF ll_num_rows = 0 THEN
	dw_account_details.InsertRow(0)
	dw_account_details.Modify("recipient_name.Visible=0")
	dw_account_details.Modify("txn_amount.Visible=0")
	dw_account_details.Modify("processed_date.Visible=0")
	dw_account_details.Modify("t_payments.Visible=1")
ELSE
	dw_account_details.Modify("recipient_name.Visible=1")
	dw_account_details.Modify("txn_amount.Visible=1")
	dw_account_details.Modify("processed_date.Visible=1")
	dw_account_details.Modify("t_payments.Visible=0")
END IF

// Change the 'Created By' field if the document type is 'ARX'
SELECT type_code 
  INTO :ls_type_code 
  FROM DOCUMENT_INDEX 
 WHERE docid = :ll_docid ;

li_rtn = SQLCA.nf_handle_error('w_document_details', '', 'open - SELECT type_code FROM DOCUMENT_INDEX ')

IF ls_type_code = "ARX" THEN
	FOR n = 1 TO ll_num_rows
		ll_payment_no = dw_account_details.GetItemNumber(n, "payment_document_payment_no")
		SELECT carrier_code 
		  INTO :ls_carrier_code 
		  FROM PAYMENT_PRESCRIPTION 
		 WHERE payment_no = :ll_payment_no ;

		li_rtn = SQLCA.nf_handle_error('w_document_details', '', 'open - SELECT carrier_code FROM PAYMENT_PRESCRIPTION')
		 
		IF ls_carrier_code = "AT" OR ls_carrier_code = "BT" THEN
			// Imported prescription payment from the pharmacy
			SELECT UP.user_first_name, UP.user_last_name 
			  INTO :ls_first_name, :ls_last_name 
			  FROM PAYMENT P, 
			       User_Profile UP 
			 WHERE P.payment_no = :ll_payment_no 
			   AND P.create_user_id = UP.user_id ;

			li_rtn = SQLCA.nf_handle_error('w_document_details', '', 'open - SELECT UP.user_first_name, UP.user_last_name FROM PAYMENT P, User_Profile UP ')
			
			dw_account_details.SetItem(n, "user_profile_user_first_name", ls_first_name)
			dw_account_details.SetItem(n, "user_profile_user_last_name", ls_last_name)
		ELSEIF ls_carrier_code = "WB" THEN
			// Imported prescription payment from a claim reimbursement
			SELECT claim_reimbursement_user_id 
			  INTO :ls_first_name  
			  FROM PAYMENT_PRESCRIPTION 
			 WHERE payment_no = :ll_payment_no ;

			li_rtn = SQLCA.nf_handle_error('w_document_details', '', 'open - SELECT claim_reimbursement_user_id FROM PAYMENT_PRESCRIPTION')
			
			dw_account_details.SetItem(n, "user_profile_user_first_name", ls_first_name)
			dw_account_details.SetItem(n, "user_profile_user_last_name", "from ABCC")
		END IF
	NEXT
END IF

end event

on w_document_details.create
this.dw_file_path_name=create dw_file_path_name
this.dw_reviewed_document=create dw_reviewed_document
this.cb_close=create cb_close
this.dw_account_details=create dw_account_details
this.dw_document_indexed_date=create dw_document_indexed_date
this.Control[]={this.dw_file_path_name,&
this.dw_reviewed_document,&
this.cb_close,&
this.dw_account_details,&
this.dw_document_indexed_date}
end on

on w_document_details.destroy
destroy(this.dw_file_path_name)
destroy(this.dw_reviewed_document)
destroy(this.cb_close)
destroy(this.dw_account_details)
destroy(this.dw_document_indexed_date)
end on

event close;f_populate_app_log(gs_appname_and_handle ,100, this.ClassName(), 'Close Event ' )
end event

type dw_file_path_name from u_dw_online within w_document_details
integer x = 23
integer y = 1148
integer width = 928
integer height = 416
integer taborder = 30
string dataobject = "d_file_path_name"
boolean vscrollbar = true
borderstyle borderstyle = styleraised!
end type

type dw_reviewed_document from u_dw_online within w_document_details
integer x = 3296
integer y = 28
integer width = 850
integer height = 304
integer taborder = 30
string dataobject = "d_reviewed_document"
borderstyle borderstyle = styleraised!
end type

type cb_close from commandbutton within w_document_details
integer x = 1874
integer y = 1644
integer width = 293
integer height = 96
integer taborder = 40
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
boolean default = true
end type

on clicked;Close(Parent)
end on

type dw_account_details from u_dw_online within w_document_details
integer x = 27
integer y = 344
integer width = 4114
integer height = 788
integer taborder = 20
string dataobject = "d_account_details"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

type dw_document_indexed_date from u_dw_online within w_document_details
integer x = 27
integer y = 28
integer width = 3250
integer height = 304
integer taborder = 10
string dataobject = "d_document_indexed_date"
borderstyle borderstyle = styleraised!
end type

