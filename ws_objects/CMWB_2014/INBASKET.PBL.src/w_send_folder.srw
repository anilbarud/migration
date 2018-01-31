$PBExportHeader$w_send_folder.srw
$PBExportComments$Window to allow user to send selected documents to a specified category
forward
global type w_send_folder from window
end type
type st_pending from statictext within w_send_folder
end type
type st_2 from statictext within w_send_folder
end type
type st_1 from statictext within w_send_folder
end type
type dw_sendto from u_dw_online within w_send_folder
end type
type dw_send_documents from u_dw_online within w_send_folder
end type
type cb_send_ok from commandbutton within w_send_folder
end type
type cb_send_cancel from commandbutton within w_send_folder
end type
type lstr_wmsg from structure within w_send_folder
end type
end forward

type lstr_WMsg from structure
	unsignedlong		hwnd
	unsignedlong		unmessage
	unsignedlong		unwparm
	long		llparm
	long		ltime
	integer		npt
end type

global type w_send_folder from window
integer x = 2199
integer y = 688
integer width = 1723
integer height = 1364
boolean titlebar = true
string title = "Send Documents"
windowtype windowtype = response!
long backcolor = 67108864
st_pending st_pending
st_2 st_2
st_1 st_1
dw_sendto dw_sendto
dw_send_documents dw_send_documents
cb_send_ok cb_send_ok
cb_send_cancel cb_send_cancel
end type
global w_send_folder w_send_folder

type prototypes
//2014-07-18 David Worboys - Added and lstr_Wmsg to support it
Function Boolean PeekMessageA(REF lstr_Wmsg lpmsg, ulong hwnd, uint uMsgFilterMin, uint uMsgFilterMax, uint uRemoveMsg) library "user32.dll"
end prototypes

type variables
BOOLEAN			I_Authorized_Access
W_SHEET			iw_sheet
S_SEND_DOC_PARAMETERS	is_send_doc_parameters
N_IMAGING			inv_imaging
LONG           il_claim_no
end variables

event open;/* APPLICATION SECURITY CODE.
*/
	G_PFSecurity.UOF_Check_Access(THIS)
	THIS.I_Authorized_Access = TRUE				//declared as an instance variable.

LONG					ll_rowcount, ll_results
STRING				ls_imaged_claim_flag, ls_event_category_code, ls_msg
DATAWINDOWCHILD	ldwc_dropdown

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


/* Create an instance of the user object for the imaging functions.
*/
	inv_imaging = CREATE n_imaging

/* Get the name of the active work sheet.
*/
	iw_sheet = w_frame.GetActiveSheet()

	IF IsValid(iw_sheet) = FALSE THEN
   	MessageBox("Document Indexing","Error determining active sheet. You may have to reboot and try again.")
		Close(THIS)
		Return
	END IF

	is_send_doc_parameters = Message.PowerObjectParm
	IF IsNull(is_send_doc_parameters.claim_no) OR is_send_doc_parameters.claim_no = 0 THEN
		MessageBox("Send Documents","Cannot Determine claim number",Exclamation!)
		Close(THIS)
		RETURN
	ELSE
		il_claim_no = is_send_doc_parameters.claim_no
	END IF

	IF is_send_doc_parameters.document_list.GetSelectedRow(0) = 0 AND is_send_doc_parameters.msg_mode = FALSE THEN
		MessageBox("Send Documents","You must select one or more documents first",Exclamation!)
		Close(THIS)
		RETURN
	END IF

	dw_send_documents.SetTransObject(ImageTrans)
	dw_send_documents.GetChild("catid",ldwc_dropdown)
	ldwc_dropdown.SetTransObject(ImageTrans)
	dw_sendto.settransobject(ImageTrans)

/* Retrieve the list of categories the user can send to.
*/

	ll_rowcount = dw_sendto.Retrieve(vgst_user_profile.user_id)
	IF ImageTrans.nf_handle_error("dw_sendto.","w_send_folder","Open") < 0 THEN
		Close(THIS)
		RETURN
	END IF

/* If no rows found then user has not set up any buckets to send to.
*/
	IF ll_rowcount <= 0 THEN
	   MessageBox("Send Documents", "No 'Send To:' categories found for sending documents.~r~n" + &
					  "You must select 'Send To' categories using the Options - Maintain In/Out Buckets menu option.",StopSign!,OK!)
	   Close(THIS)
		RETURN
	END IF

/* Insert blank row into the datawindow.
*/
	dw_send_documents.InsertRow(0)
	dw_send_documents.SetItem(1,"claim_no",il_claim_no)

/* Set defaults for phone message notification usage.
*/
	IF is_send_doc_parameters.msg_mode THEN
		w_send_folder.Title = 'Send Log Entry'
		
		ls_event_category_code = is_send_doc_parameters.document_list.GetItemString(is_send_doc_parameters.document_list.GetSelectedRow(0),"event_category_code")
		IF ls_event_category_code = 'C' THEN
			ls_msg = "See Claim Event " + String(is_send_doc_parameters.document_list.GetItemNumber(is_send_doc_parameters.document_list.GetSelectedRow(0),"event_no"))
		ELSE
			ls_msg = "See Ind. Event " + String(is_send_doc_parameters.document_list.GetItemNumber(is_send_doc_parameters.document_list.GetSelectedRow(0),"event_no"))
		END IF
		dw_send_documents.SetItem(1,"action_note",ls_msg)
		dw_send_documents.SetColumn("action_code")
		dw_send_documents.SetItem(1,"action_date",f_server_datetime())
	END IF
	
	/* set up the column select which is a multi select (2) */
   dw_sendto.uf_setselect(3)



end event

event closequery;IF IsValid(inv_imaging) THEN Destroy inv_imaging


IF gb_additional_logging = TRUE THEN	
	N_OBJECTHELPER lnv_object_helper
	// write to the application log
	f_populate_app_log(gs_appname_and_handle ,lnv_object_helper.get_app_component_type(THIS), lnv_object_helper.getpath(THIS), 'closequery' )
END IF 


end event

on w_send_folder.create
this.st_pending=create st_pending
this.st_2=create st_2
this.st_1=create st_1
this.dw_sendto=create dw_sendto
this.dw_send_documents=create dw_send_documents
this.cb_send_ok=create cb_send_ok
this.cb_send_cancel=create cb_send_cancel
this.Control[]={this.st_pending,&
this.st_2,&
this.st_1,&
this.dw_sendto,&
this.dw_send_documents,&
this.cb_send_ok,&
this.cb_send_cancel}
end on

on w_send_folder.destroy
destroy(this.st_pending)
destroy(this.st_2)
destroy(this.st_1)
destroy(this.dw_sendto)
destroy(this.dw_send_documents)
destroy(this.cb_send_ok)
destroy(this.cb_send_cancel)
end on

type st_pending from statictext within w_send_folder
integer x = 389
integer y = 740
integer width = 1056
integer height = 388
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_2 from statictext within w_send_folder
integer x = 155
integer y = 740
integer width = 229
integer height = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Pending:"
boolean focusrectangle = false
end type

type st_1 from statictext within w_send_folder
integer x = 155
integer y = 324
integer width = 233
integer height = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Send To:"
boolean focusrectangle = false
end type

type dw_sendto from u_dw_online within w_send_folder
integer x = 389
integer y = 340
integer width = 1051
integer height = 372
integer taborder = 20
string dataobject = "d_inbasket_forward_category_list"
boolean vscrollbar = true
boolean livescroll = false
borderstyle borderstyle = stylelowered!
end type

event clicked;call super::clicked;INTEGER li_counter
STRING  ls_temp, ls_cat

// set it to nothing this fixes the case where there is only name highlighted
st_pending.text = ""

/* for each selected row create the pending section of names */
FOR li_counter = 1  TO dw_sendto.rowcount()
	IF THIS.isselected(li_counter) THEN
		ls_cat  = THIS.getitemstring(li_counter,"cat_catname")
		IF TRIM(ls_temp) = "" THEN
			ls_temp = ls_cat 
		ELSE
			ls_temp = ls_temp + " ; " + ls_cat 
		END IF 
	END IF 
NEXT

IF trim(ls_temp) > ""  THEN st_pending.text = ls_temp
end event

event constructor;call super::constructor;this.settransobject(imagetrans)
end event

event rbuttondown;call super::rbuttondown;triggerevent(clicked!)
end event

event keydown;call super::keydown;STRING ls_search, ls_char
LONG   ll_end, ll_find
lstr_WMsg lvstr_Msg //2014-07-18 David Worboys - Used in removing O from the  windows message queue

IF NOT KeyDown(KeyAlt!) THEN //2014-07-18 David Worboys.  Start do not process search if the Alt key is held down.

	IF KeyDown(KeyA!) THEN
		ls_char = 'A'
	ELSEIF KeyDown(KeyA!) THEN
		ls_char = 'A'
	ELSEIF KeyDown(KeyB!) THEN
		ls_char = 'B'
	ELSEIF KeyDown(KeyC!) THEN
		ls_char = 'C'
	ELSEIF KeyDown(KeyD!) THEN
		ls_char = 'D'
	ELSEIF KeyDown(KeyE!) THEN
		ls_char = 'E'
	ELSEIF KeyDown(KeyF!) THEN
		ls_char = 'F'
	ELSEIF KeyDown(KeyG!) THEN
		ls_char = 'G'
	ELSEIF KeyDown(KeyH!) THEN
		ls_char = 'H'
	ELSEIF KeyDown(KeyI!) THEN
		ls_char = 'I'
	ELSEIF KeyDown(KeyJ!) THEN
		ls_char = 'J'
	ELSEIF KeyDown(KeyK!) THEN
		ls_char = 'K'
	ELSEIF KeyDown(KeyL!) THEN
		ls_char = 'L'
		
		 //2014-07-18 David Worboys - Start
        PeekMessageA( lvstr_Msg, 0, 256, 264, 1 )   //This removes the keyed L from the windows message queue.
         message.processed=TRUE 
         message.returnvalue=1
		//2014-07-18 David Worboys - End	
	ELSEIF KeyDown(KeyM!) THEN
		ls_char = 'M'
	ELSEIF KeyDown(KeyN!) THEN
		ls_char = 'N'
	ELSEIF KeyDown(KeyO!) THEN
		ls_char = 'O'
		
        //2014-07-18 David Worboys - Start
        PeekMessageA( lvstr_Msg, 0, 256, 264, 1 )   //This removes the keyed O from the windows message queue.
         message.processed=TRUE 
         message.returnvalue=1
		//2014-07-18 David Worboys - End	
		
	ELSEIF KeyDown(KeyP!) THEN
		ls_char = 'P'
	ELSEIF KeyDown(KeyQ!) THEN
		ls_char = 'Q'
	ELSEIF KeyDown(KeyR!) THEN
		ls_char = 'R'
	ELSEIF KeyDown(KeyS!) THEN
		ls_char = 'S'
	ELSEIF KeyDown(KeyT!) THEN
		ls_char = 'T'
	ELSEIF KeyDown(KeyU!) THEN
		ls_char = 'U'
	ELSEIF KeyDown(KeyV!) THEN
		ls_char = 'V'
	ELSEIF KeyDown(KeyW!) THEN
		ls_char = 'W'
	ELSEIF KeyDown(KeyX!) THEN
		ls_char = 'X'
	ELSEIF KeyDown(KeyY!) THEN
		ls_char = 'Y'
	ELSEIF KeyDown(KeyZ!) THEN
		ls_char = 'Z'	
	ELSE
		RETURN
	END IF 

	// grab the first char of the value
	ls_search = this.getitemstring(this.getrow(),'cat_catname')
	IF ISNULL(ls_search) THEN ls_search = ""

	// The end value is one greater than the row count
	ll_end  = THIS.RowCount() + 1
	ll_find = 1

	IF TRIM(ls_char) > "" THEN
	
		IF left(ls_search,1) = ls_char THEN
			ls_char = ls_char + '%'
			ll_find = this.getrow() + 1
			ll_find = THIS.Find("cat_catname LIKE '" + ls_char + "'",ll_find, ll_end)
		
	   ELSE
	
			ls_char = ls_char + '%'
			ll_find = THIS.Find("cat_catname LIKE '" + ls_char + "'",ll_find, ll_end)
		
		END IF 
	END IF 

	IF ll_find > 0 AND ll_find <= this.rowcount() AND TRIM(ls_char) > "" THEN 
		THIS.scrolltorow(ll_find)
		THIS.selectrow(ll_find,true)
		THIS.triggerevent(clicked!)
	END IF
END IF //2014-07-18 David Worboys.  End do not process search if the Alt key is held down.


end event

event rowfocuschanged;call super::rowfocuschanged;triggerevent(clicked!)
end event

type dw_send_documents from u_dw_online within w_send_folder
event ue_post_losefocus pbm_custom08
integer x = 5
integer y = 32
integer width = 1253
integer height = 300
integer taborder = 10
string dataobject = "d_send_specs_formulary"
boolean border = false
end type

event itemchanged;call super::itemchanged;// This script added 1998/10/19 Ian L. Ross in response to HelpDesk Log No 27502 / Problem Report Number 231
// Reasable edit check on the date field.

Int					li_msgbox

Date					ld_today
Date					ld_chk_date

Choose Case dwo.name 
	Case "action_date" 

		ld_today		= Date(f_server_datetime())
		ld_chk_date = Date(Left(Data,10))
		
		// Reasonable Edit Checks...
		// Chek to see if the date is in the future > 365 days or in the past then reject the data
		If ld_chk_date > ld_today Then
			
			IF DaysAfter(ld_today, ld_chk_date) > 365 Then
				li_msgbox = MessageBox("Warning","The Action Date, "  + trim(String(ld_chk_date)) + ", Cannot Be A Year Or More In The Future!")
				Return 1
			End If

		ELSEIF (ld_chk_date) < (ld_today) THEN

			li_msgbox = MessageBox("Warning","The Action Date, "  + trim(String(ld_chk_date)) + ", Cannot Be In The Past!")
			Return 1

		END IF

End Choose


end event

type cb_send_ok from commandbutton within w_send_folder
integer x = 562
integer y = 1164
integer width = 334
integer height = 88
integer taborder = 30
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
boolean default = true
end type

event clicked;LONG		ll_fldid, ll_selected_row,	ll_catid, ll_setid, ll_docid, ll_results, ll_claim_no, ll_event_no
STRING   ls_action, ls_action_date, ls_keyword, ls_claimant_name
DATETIME	ldt_action_date
LONG     ll_total_items, ll_row
INTEGER  li_counter, ll_insert, li_inserted_row, li_trancount

SetPointer(HourGlass!)
Enabled = FALSE
cb_send_cancel.Enabled = FALSE


IF gb_additional_logging = TRUE THEN	
	N_OBJECTHELPER lnv_object_helper
	// write to the application log
	f_populate_app_log(gs_appname_and_handle ,lnv_object_helper.get_app_component_type(THIS), lnv_object_helper.getpath(THIS), 'cb_send_ok-clicked' )
END IF 

/* If there is no row, then there was a problem so get out.
*/
IF dw_send_documents.RowCount() <> 1 THEN
	Close(PARENT)
	RETURN
END IF

IF dw_send_documents.AcceptText() = -1 THEN
	THIS.Enabled = TRUE
	cb_send_cancel.Enabled = TRUE
	RETURN
END IF


/* Get variables, and validate. 1 message will be sent to each of the selected individuals
*/
ll_total_items = dw_sendto.rowcount()
IF ll_total_items < 1 OR dw_sendto.GetSelectedRow(0) = 0 THEN 
	MessageBox("Send Documents","You must select a destination category before forwarding documents.",Exclamation!)
	GOTO Normal_Exit
END IF 

SELECT a.given_names + ' ' + a.last_name
INTO   :ls_claimant_name
FROM   INDIVIDUAL a
JOIN   CLAIM b ON a.individual_no = b.individual_no
WHERE  b.claim_no = :il_claim_no
USING SQLCA;
SQLCA.nf_handle_error('w_send_folder', 'cb_send_ok.clicked', 'embedded SQL: SELECT given_names + last_name FROM INDIVIDUAL JOIN CLAIM...')

ll_claim_no		  = dw_send_documents.GetItemNumber(1,"claim_no")
ls_action        = dw_send_documents.GetItemString(1,"action_code")
ldt_action_date  = dw_send_documents.GetItemDateTime(1,"action_date")
ls_keyword       = dw_send_documents.GetItemString(1,"action_note")

IF IsNull(ls_action) THEN
	MessageBox("Send Documents","You must select an action code before forwarding documents.",Exclamation!)
	GOTO Normal_Exit
END IF

IF Date(ldt_action_date) < Date("1900,01,01") THEN
	MessageBox("Send Documents","You must provide an action date greater than or equal to Jan 1, 1900",Exclamation!)
	GOTO Normal_Exit
END IF

ll_row    = 1//firstrow in main datawindow we may have to add other rows
ll_insert = 1//counter for insert

FOR li_counter = 1 TO ll_total_items
	
	//If the record is not selected than continue onwards
	IF dw_sendto.isselected(li_counter) = FALSE THEN CONTINUE

	/* GRAB THE CATEGORY id */
	ll_catid = dw_sendto.GetItemNumber(li_counter,"cat_catid")
	IF ISNULL(ll_catid) OR ll_catid < 1  THEN RETURN -1
	
	IF ll_insert > 1 THEN
		li_inserted_row = dw_send_documents.insertrow(0)
		dw_send_documents.setitem(li_inserted_row,"claim_no",ll_claim_no)
		dw_send_documents.SetItem(li_inserted_row,"action_code",ls_action)
		dw_send_documents.setitem(li_inserted_row,"action_date",ldt_action_date)
		dw_send_documents.setitem(li_inserted_row,"action_note",ls_keyword)
		ll_row = li_inserted_row
	END IF 

	/* Create a work folder.*/
	
	ImageTrans.nf_begin_transaction()
	
	// this function begins & commits its own transaction, then inserts data outside of txn,
	// so it must be enclosed within its own txn
	
	ll_fldid = inv_imaging.nf_create_workfolder("w_send_folder",ll_catid)
	IF ll_fldid = -1 THEN
		MessageBox("Send Folder","Unable to create work folder. Please try again.")
		GOTO Normal_Exit
	END IF
	
	ImageTrans.nf_transaction_count(li_trancount,1,'','','',FALSE)
	IF li_trancount > 0 THEN
		ImageTrans.nf_commit_transaction()
	END IF


	dw_send_documents.SetItem(ll_row,"folderid",ll_fldid)

/* Get the setid for the Category.
*/
	SELECT setid
	  INTO :ll_setid
	  FROM CAT
	 WHERE catid = :ll_catid
	 USING ImageTrans;

	ImageTrans.nf_handle_error("w_send_folder","clicked for cb_send_ok","SELECT setid") 


	ImageTrans.nf_begin_transaction()

/* Add selected documents to the work folder.
*/
	ll_selected_row = 0
	DO WHILE TRUE AND is_send_doc_parameters.msg_mode = FALSE

/* Get the next selected row.
*/
	   ll_selected_row = is_send_doc_parameters.document_list.GetSelectedRow(ll_selected_row)
	   IF ll_selected_row = 0 THEN EXIT

/* Get the document id of the selcted row.
*/
	   ll_docid = is_send_doc_parameters.document_list.GetItemNumber(ll_selected_row,"ref_docid")
	   IF ll_docid = 0 THEN
	      MessageBox("Send Documents", "Error determining docid for selected document.")
			GOTO Normal_Exit
	   END IF
		

/* Add the document to the folder.
*/
		INSERT INTO REF (docid, docfldid, doccatid, docsetid )
		VALUES (:ll_docid, :ll_fldid, :ll_catid, :ll_setid )
		 USING ImageTrans;

		ImageTrans.nf_handle_error("w_send_folder","clicked for cb_send_ok","INSERT INTO REF") 
			
/* Increment the document reference counter.
*/
		UPDATE DOC
			SET docrefcount = docrefcount + 1
		 WHERE docid = :ll_docid
		 USING ImageTrans;

		ImageTrans.nf_handle_error("w_send_folder","clicked for cb_send_ok","UPDATE DOC") 
		
	LOOP

/* Index the work folder.
*/
	dw_send_documents.Update()
	ImageTrans.nf_handle_error("w_send_folder","clicked for cb_send_ok","dw_send_documents.Update()") 

/* Update the folder with the new name.
*/
	UPDATE FLD
		SET fldname = Upper(:ls_action) + CONVERT(varchar(10),:ll_claim_no) + :ls_claimant_name
	 WHERE fldid = :ll_fldid
	 USING ImageTrans;

	ImageTrans.nf_handle_error("w_send_folder","clicked for cb_send_ok","UPDATE FLD") 
	
	ImageTrans.nf_commit_transaction()

	ll_insert ++
NEXT




Close(PARENT)
RETURN

Normal_Exit:
	
	ImageTrans.nf_transaction_count(li_trancount,1,'','','',FALSE)
	IF li_trancount > 0 THEN
		ImageTrans.nf_rollback_transaction()
	END IF
	
	SetPointer(Arrow!)
	Enabled                = TRUE
	cb_send_cancel.Enabled = TRUE
   RETURN

end event

type cb_send_cancel from commandbutton within w_send_folder
event lbuttondown pbm_lbuttondown
integer x = 910
integer y = 1164
integer width = 334
integer height = 88
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cance&l"
end type

event lbuttondown;Close(PARENT)
end event

event clicked;Close(PARENT)

end event

