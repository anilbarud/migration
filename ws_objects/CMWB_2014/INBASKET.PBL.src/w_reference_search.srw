$PBExportHeader$w_reference_search.srw
forward
global type w_reference_search from w_a_tool
end type
type cb_search from commandbutton within w_reference_search
end type
type dw_ref_search_doc_list from u_dw_online within w_reference_search
end type
type cb_send from commandbutton within w_reference_search
end type
type cb_view from commandbutton within w_reference_search
end type
type rb_docid from radiobutton within w_reference_search
end type
type rb_refno from radiobutton within w_reference_search
end type
type dw_ref_search_criteria from u_dw_online within w_reference_search
end type
type gb_2 from groupbox within w_reference_search
end type
type gb_1 from groupbox within w_reference_search
end type
type uo_image_append from u_image_append within w_reference_search
end type
type rb_batch from radiobutton within w_reference_search
end type
end forward

global type w_reference_search from w_a_tool
boolean resizable = false
cb_search cb_search
dw_ref_search_doc_list dw_ref_search_doc_list
cb_send cb_send
cb_view cb_view
rb_docid rb_docid
rb_refno rb_refno
dw_ref_search_criteria dw_ref_search_criteria
gb_2 gb_2
gb_1 gb_1
uo_image_append uo_image_append
rb_batch rb_batch
end type
global w_reference_search w_reference_search

type variables
STRING		is_SQLSyntax
N_IMAGING	inv_imaging

BOOLEAN               ib_retrieved

w_sheet   iw_sheet
end variables

forward prototypes
public function integer wf_set_claim (long al_claim_no)
end prototypes

public function integer wf_set_claim (long al_claim_no);LONG		ll_result
W_SHEET	lw_sheet

lw_sheet = w_frame.GetActiveSheet()
IF NOT IsValid(lw_sheet) THEN
	RETURN -1
END IF

/* Validate the claim number the user has entered and get the claim's master folder.
*/
	ll_result = lw_sheet.wf_set_claim(al_claim_no)

	RETURN 1

end function

event open;call super::open;s_window_message lstr_message


lstr_message = Message.PowerObjectParm


// Get the name of the active work sheet
iw_sheet = lstr_message.apo_powerobjectparm[1]
If IsValid(iw_sheet) = False Then
   MessageBox("Document Search","Error determining active sheet. You may have to close WorkBench and try again.")
	Close(This)
	Return
End If

/* Set the environment.
*/
	dw_ref_search_criteria.SetTransObject(SQLCA)
	dw_ref_search_doc_list.SetTransObject(ImageTrans)

/* Set up the basic SQL Syntax that will be modified for the requested search.
*/
	is_SQLSyntax = dw_ref_search_doc_list.Describe("DataWindow.Table.Select")

/* Set up search criteria area.
*/
	dw_ref_search_criteria.Reset()
	dw_ref_search_criteria.InsertRow(0)

/* Create an instance of the user object for the imaging functions.
*/
	inv_imaging = CREATE n_imaging

/* Set up enabling/disabling.
*/
	rb_refno.Enabled = TRUE
	rb_docid.Enabled = TRUE
	rb_batch.Enabled = TRUE
	//rb_refno.Checked = TRUE
	//rb_refno.EVENT POST clicked()
	rb_docid.Checked = TRUE
	rb_docid.EVENT POST clicked()
	
	dw_ref_search_criteria.Enabled = FALSE
	dw_ref_search_doc_list.Enabled = FALSE
	cb_search.Enabled = FALSE
	cb_send.Enabled = FALSE
	cb_view.Enabled = FALSE

	dw_ref_search_criteria.SetFocus()

end event

on w_reference_search.create
int iCurrent
call super::create
this.cb_search=create cb_search
this.dw_ref_search_doc_list=create dw_ref_search_doc_list
this.cb_send=create cb_send
this.cb_view=create cb_view
this.rb_docid=create rb_docid
this.rb_refno=create rb_refno
this.dw_ref_search_criteria=create dw_ref_search_criteria
this.gb_2=create gb_2
this.gb_1=create gb_1
this.uo_image_append=create uo_image_append
this.rb_batch=create rb_batch
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_search
this.Control[iCurrent+2]=this.dw_ref_search_doc_list
this.Control[iCurrent+3]=this.cb_send
this.Control[iCurrent+4]=this.cb_view
this.Control[iCurrent+5]=this.rb_docid
this.Control[iCurrent+6]=this.rb_refno
this.Control[iCurrent+7]=this.dw_ref_search_criteria
this.Control[iCurrent+8]=this.gb_2
this.Control[iCurrent+9]=this.gb_1
this.Control[iCurrent+10]=this.uo_image_append
this.Control[iCurrent+11]=this.rb_batch
end on

on w_reference_search.destroy
call super::destroy
destroy(this.cb_search)
destroy(this.dw_ref_search_doc_list)
destroy(this.cb_send)
destroy(this.cb_view)
destroy(this.rb_docid)
destroy(this.rb_refno)
destroy(this.dw_ref_search_criteria)
destroy(this.gb_2)
destroy(this.gb_1)
destroy(this.uo_image_append)
destroy(this.rb_batch)
end on

event close;call super::close;
post close(iw_sheet)

end event

type st_title from w_a_tool`st_title within w_reference_search
string text = "Document Search"
end type

type cb_close from w_a_tool`cb_close within w_reference_search
integer x = 2816
integer y = 1688
integer width = 247
integer height = 108
integer taborder = 10
end type

type cb_search from commandbutton within w_reference_search
integer x = 2789
integer y = 184
integer width = 256
integer height = 108
integer taborder = 80
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Sea&rch"
boolean default = true
end type

event clicked;LONG		     ll_refno_lo, ll_refno_hi, ll_docid_lo, ll_docid_hi
STRING	     ls_additional_whereclause,	ls_ModString
W_SHEET     lw_sheet

/* make sure something is checked */
IF rb_refno.Checked = FALSE AND rb_docid.Checked = FALSE AND rb_batch.checked = FALSE THEN
	MessageBox("Document Search","Search criteria must be specified. Please try again.")
	RETURN
END IF

/*	Get current values for variables.
*/
	dw_ref_search_criteria.AcceptText()
	ll_refno_lo = dw_ref_search_criteria.GetItemNumber(1,"refno_lo") 
	ll_refno_hi = dw_ref_search_criteria.GetItemNumber(1,"refno_hi") 
	ll_docid_lo = dw_ref_search_criteria.GetItemNumber(1,"docid_lo") 
	ll_docid_hi = dw_ref_search_criteria.GetItemNumber(1,"docid_hi") 

/* Validate search criteria.
*/
	IF rb_refno.Checked = TRUE THEN
		IF IsNull(ll_refno_lo) OR IsNull(ll_refno_hi) THEN
			MessageBox("Document Search","Both a 'FROM' and 'TO' value must be provided. Please try again.")
			RETURN
		END IF
		IF ll_refno_lo > ll_refno_hi THEN
			MessageBox("Document Search","The 'FROM' value may not be larger than the 'TO' value. Please try again.")
			RETURN
		END IF
	END IF

	IF rb_docid.Checked = TRUE THEN
		IF IsNull(ll_docid_lo) OR IsNull(ll_docid_hi) THEN
			MessageBox("Document Search","Both a 'FROM' and 'TO' value must be provided. Please try again.")
			RETURN
		END IF
		IF ll_docid_lo > ll_docid_hi THEN
			MessageBox("Document Search","The 'FROM' value may not be larger than the 'TO' value. Please try again.")
			RETURN
		END IF
	END IF
	
	IF rb_batch.Checked = TRUE THEN
		ll_refno_lo = dw_ref_search_criteria.GetItemNumber(1,"batch_lo") 
		ll_refno_hi = dw_ref_search_criteria.GetItemNumber(1,"batch_hi") 
		
		IF IsNull(ll_refno_lo) OR IsNull(ll_refno_hi) THEN
			MessageBox("Document Search","Both a 'FROM' and 'TO' value must be provided. Please try again.")
			RETURN
		END IF
		IF ll_refno_lo > ll_refno_hi THEN
			MessageBox("Document Search","The 'FROM' value may not be larger than the 'TO' value. Please try again.")
			RETURN
		END IF
	END IF

/*	Build the WHERE clause for the basic claim search.
*/
	IF rb_refno.Checked = TRUE THEN
		ls_Additional_WhereClause = ls_Additional_WhereClause + " and (DOCUMENT_INDEX.reference_no >= " + string(ll_refno_lo) + ")"
		ls_Additional_WhereClause = ls_Additional_WhereClause + " and (DOCUMENT_INDEX.reference_no <= " + string(ll_refno_hi) + ")"
	END IF

	IF rb_docid.Checked = TRUE THEN
		ls_Additional_WhereClause = ls_Additional_WhereClause + " and (DOCUMENT_INDEX.docid >= " + string(ll_docid_lo) + ")"
		ls_Additional_WhereClause = ls_Additional_WhereClause + " and (DOCUMENT_INDEX.docid <= " + string(ll_docid_hi) + ")"
	END IF
	
	IF rb_batch.Checked = TRUE THEN
		ls_Additional_WhereClause = ls_Additional_WhereClause + " and (DOCUMENT_INDEX.reference_no >= " + string(ll_refno_lo) + ")"
		ls_Additional_WhereClause = ls_Additional_WhereClause + " and (DOCUMENT_INDEX.reference_no <= " + string(ll_refno_hi) + ")"
	END IF

/*	Modify the data window syntax.
*/
	ls_ModString = "DataWindow.Table.Select=~"" + is_SQLSyntax + ls_Additional_WhereClause + "~""
	dw_ref_search_doc_list.ModIfy(ls_ModString)

/*	Retrive data and handle enabling accordingly.
*/
	IF dw_ref_search_doc_list.Retrieve() > 0 THEN
		rb_refno.Enabled = TRUE
		rb_docid.Enabled = TRUE
		rb_batch.enabled = TRUE
		dw_ref_search_criteria.Enabled = TRUE
		dw_ref_search_doc_list.Enabled = TRUE
		cb_search.Enabled = TRUE
		cb_send.Enabled = TRUE
		cb_view.Enabled = TRUE
		dw_ref_search_doc_list.uf_setselect(3)  // normal windows shift/ctl click selection functionality
		dw_ref_search_doc_list.SetFocus()
		
		IF rb_docid.Checked THEN
			dw_ref_search_doc_list.SetSort('document_index_docid A') // sort ascending by doc id
		ELSE // rb_refno.Checked
			dw_ref_search_doc_list.SetSort('document_index_reference_no A') // sort ascending by ref number
		END IF
		dw_ref_search_doc_list.Sort()
		
		// clear tombstone, clear doc list and disable view button
		lw_sheet = w_frame.GetActiveSheet()
		IF NOT IsValid(lw_sheet) THEN
			RETURN
		END IF
		lw_sheet.wf_clear_worksheet()
		lw_sheet.wf_initialize_basic_claim()
	ELSE
		ImageTrans.nf_handle_error("dw_ref_search_doc_list","w_reference_search","clicked for cb_search")
		rb_refno.Enabled = TRUE
		rb_docid.Enabled = TRUE
		rb_batch.enabled = TRUE
		dw_ref_search_criteria.Enabled = TRUE
		dw_ref_search_doc_list.Enabled = FALSE
		cb_search.Enabled = TRUE
		cb_send.Enabled = FALSE
		cb_view.Enabled = FALSE
	END IF

	ib_retrieved = TRUE  // avoid running rowfocuschanged on dw_ref_search_criteria except after full retrieval
end event

type dw_ref_search_doc_list from u_dw_online within w_reference_search
integer x = 59
integer y = 616
integer width = 3072
integer height = 904
integer taborder = 20
string dataobject = "d_ref_search_doc_list"
boolean vscrollbar = true
end type

event rowfocuschanged;call super::rowfocuschanged;LONG            ll_claim_no,ll_row
INTEGER       li_selected, li_selected_old
W_SHEET      lw_sheet

/*
Find out if more than one row is selected. If true, clear the tombstone, the titlebar and the document list and disable the view and send buttons.
Otherwise, set the claim information in the tombstone, populate the claim document list and the titlebar.
*/

IF ib_retrieved THEN
	li_selected = This.GetSelectedRow(0)
	IF li_selected > 0 THEN
		li_selected_old = li_selected
		
		li_selected = This.GetSelectedRow(li_selected_old)

		IF li_selected > 0 THEN
			lw_sheet = w_frame.GetActiveSheet()
			IF NOT IsValid(lw_sheet) THEN
				RETURN
			END IF
			lw_sheet.wf_clear_worksheet()
			lw_sheet.wf_initialize_basic_claim()
			cb_view.Enabled = FALSE
			RETURN
		END IF
	END IF

	cb_view.Enabled = TRUE
	ll_row = this.getrow()
	IF ll_row > 0 THEN
		ll_claim_no = This.Object.document_index_claim_no[ll_row]
		wf_set_claim(ll_claim_no)
	END IF 
END IF
end event

event clicked;call super::clicked;LONG   ll_row

ll_row = This.GetRow()

IF ll_row > 0 THEN
	This.EVENT TRIGGER RowFocusChanged(ll_row)
END IF




end event

type cb_send from commandbutton within w_reference_search
integer x = 59
integer y = 1544
integer width = 247
integer height = 108
integer taborder = 40
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Send"
end type

event clicked;LONG		ll_fldid, ll_selected_row, ll_catid_parm, ll_setid_parm, ll_docid, ll_results, ll_claim_no, &
			ll_event_no, ll_catid, ll_doc_count
STRING	ls_action_parm, ls_action_parm_date, ls_keyword_parm, ls_claimant_name, ls_given_names, ls_last_name
DATETIME	ldt_action_date_parm
BOOLEAN	lb_warning_1_given, lb_warning_2_given, lb_commit

SetPointer(HourGlass!)

/* If there is no row retrieved then there was a problem so get out.
*/
	IF dw_ref_search_doc_list.RowCount() < 1 THEN
		MessageBox("Document Search","No rows have been retrieved based on given search criteria.")
		RETURN
	END IF

/* Check for a highlighted row.
*/
	ll_selected_row = dw_ref_search_doc_list.GetSelectedRow(ll_selected_row)
	IF ll_selected_row = 0 THEN
	   MessageBox("Document Search", "A row must be highlighted in order to be sent.")
   	RETURN
	END IF

	IF dw_ref_search_doc_list.AcceptText() = -1 THEN
		RETURN
	END IF

	ls_action_parm        = "ZZ"
	ll_catid_parm         = 864
	ll_setid_parm         = 6
	ldt_action_date_parm  = f_server_datetime()
	ls_keyword_parm       = "CORRECTION"

	lb_warning_1_given = FALSE
	lb_warning_2_given = FALSE
	lb_commit          = FALSE

/* Set up an infinite loop.
*/
	ll_selected_row = 0
	DO WHILE TRUE

/* Get the next selected row.
*/
	   ll_selected_row = dw_ref_search_doc_list.GetSelectedRow(ll_selected_row)
   	IF ll_selected_row = 0 THEN
			EXIT
		END IF

/* Get the neccessary data from the selcted row.
*/
		ll_claim_no = dw_ref_search_doc_list.GetItemNumber(ll_selected_row,"document_index_claim_no")
		ll_catid	   = dw_ref_search_doc_list.GetItemNumber(ll_selected_row,"ref_doccatid")
	   ll_docid    = dw_ref_search_doc_list.GetItemNumber(ll_selected_row,"document_index_docid")
   	IF ll_docid = 0 THEN
      	MessageBox("Document Search", "Error determining docid for selected document. Operation aborted.")
			RETURN
		END IF

/* If user has chosen a 'correction' row to send to 'corrections', notify of problem.
*/
		IF ll_catid = ll_catid_parm THEN
			IF lb_warning_1_given = FALSE THEN
				MessageBox("Document Search","Attempt to send a 'correction' to 'corrections'. Item ignored.")
				lb_warning_1_given = TRUE
			END IF
			CONTINUE
		END IF

/* If user has chosen a 'master' row that has already been sent to 'corrections', notify of problem.
*/
		SELECT count(*)
		  INTO :ll_doc_count
		  FROM REF
		 WHERE docid = :ll_docid
			AND doccatid = :ll_catid_parm
		 USING ImageTrans;
		IF ImageTrans.nf_handle_error("Embedded SQL: Select from REF","w_reference_search","clicked for cb_send") < 0 THEN
   		Close(PARENT)
			RETURN
		END IF
		IF ll_doc_count >= 1 THEN
			IF lb_warning_2_given = FALSE THEN
				MessageBox("Document Search","Attempt to sent a corrected master to 'corrections'. Item ignored.")
				lb_warning_2_given = TRUE
			END IF
			CONTINUE
		END IF

/* Create a work folder.
*/
		ImageTrans.nf_begin_transaction()
		
		// this function begins & commits its own transaction, then inserts data outside of txn,
		// so it must be enclosed within its own txn
		
		ll_fldid = inv_imaging.nf_create_workfolder("w_reference_search",ll_catid_parm)
		IF ll_fldid = -1 THEN
			MessageBox("Document Search","Unable to create work folder. Operation aborted.")
			RETURN
		END IF

		ImageTrans.nf_commit_transaction()


/* Get the claimant name for the claim number.
*/
		SELECT given_names, last_name
		  INTO :ls_given_names, :ls_last_name
 		  FROM INDIVIDUAL, CLAIM
		 WHERE INDIVIDUAL.individual_no = CLAIM.individual_no
			AND CLAIM.claim_no = :ll_claim_no	
		 USING SQLCA;

		IF SQLCA.nf_handle_error("Embedded SQL: Select from CLAIM, INDIVIDUAL","w_reference_search","clicked for cb_send") < 0 THEN
   		Close(PARENT)
			RETURN
		END IF

		ls_claimant_name = ls_given_names + " " + ls_last_name
		
		
		ImageTrans.nf_begin_transaction()

/* Add the document to the folder.
*/
		INSERT INTO REF (docid, docfldid, doccatid, docsetid )
		VALUES (:ll_docid, :ll_fldid, :ll_catid_parm, :ll_setid_parm )
		USING ImageTrans;

		IF ImageTrans.nf_handle_error("Embedded SQL: Insert into REF","w_reference_search","clicked for cb_send") < 0 THEN
			Close(PARENT)
			RETURN
	   END IF

/* Increment the document reference counter.
*/
	   UPDATE DOC 
			SET docrefcount = docrefcount + 1
		 WHERE docid = :ll_docid
		 USING ImageTrans;

		IF ImageTrans.nf_handle_error("Embedded SQL: Update DOC","w_reference_search","clicked for cb_send") < 0 THEN
			Close(PARENT)
			RETURN
		END IF

/* Index the work folder.
*/

/* First create the claimsworking entry.
*/
		INSERT INTO CLAIM_WORKING (folderid, action_code, claim_no, action_note, action_date)
		VALUES (:ll_fldid, :ls_action_parm, :ll_claim_no, :ls_keyword_parm, :ldt_action_date_parm)
		USING ImageTrans;

		IF ImageTrans.nf_handle_error("Embedded SQL: Insert CLAIM_WORKING","w_reference_search","clicked for cb_send") < 0 THEN
			Close(PARENT)
			RETURN
		END IF

/* Update the folder with the new name.
*/
		UPDATE FLD
			SET fldname = Upper(:ls_action_parm) + CONVERT(varchar(10),:ll_claim_no) + :ls_claimant_name
		 WHERE fldid = :ll_fldid
		 USING ImageTrans;

		IF ImageTrans.nf_handle_error("Embedded SQL: Update FLD","w_reference_search","clicked for cb_send") < 0 THEN
			Close(PARENT)
			RETURN
		END IF

		ImageTrans.nf_commit_transaction()
		
		lb_commit = TRUE
	LOOP

	IF lb_commit THEN
		MessageBox("Document Search","Document(s) successfully sent to CORRECTIONS.")
	END IF

	IF dw_ref_search_doc_list.Retrieve() <= 0 THEN
		ImageTrans.nf_handle_error("dw_ref_search_doc_list","w_reference_search","clicked for cb_send")
	END IF

end event

type cb_view from commandbutton within w_reference_search
integer x = 347
integer y = 1544
integer width = 247
integer height = 108
integer taborder = 50
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&View"
end type

event clicked;LONG	ll_rownum, ll_ref, ll_result, ll_docid
string ls_doc_type
integer li_rowsreturned

ll_rownum = dw_ref_search_doc_list.GetSelectedRow(0)
IF ll_rownum <= 0 THEN
	MessageBox("Document Search","A row must be highlighted in order to be viewed.",Exclamation!)
	RETURN
END IF

ll_docid = dw_ref_search_doc_list.GetItemNumber(ll_rownum,"document_index_docid")

ll_result = f_close_viewer()


if isvalid(uo_image_append) then
	
	/* check to see if new viewe should be called */
		if uo_image_append.of_init(ll_docid)	<= 0 then
			RETURN 
		end if
			
			
		ls_doc_type =  uo_image_append.of_get_file_type()
			
		
		CHOOSE CASE ls_doc_type
			/*  Imaged document */ 
			CASE 'IMA', 'TIF'
				li_rowsreturned = uo_image_append.of_append_image(ll_docid) 
				if li_rowsreturned = 0 then 
					MessageBox("Document Search","There are no pages in this document.",Exclamation!)
					return
				elseif li_rowsreturned < 0 then
					MessageBox("Document Search","Unable to open document. Please try again.",Exclamation!)
					return
				end if
			case else
				/* execel and word */
				li_rowsreturned = iw_active_sheet.iu_dw_document_path.f_manage_document(ll_docid,"V","NORMAL")
		end choose
	
		
		IF li_rowsreturned = 0 THEN
			MessageBox("Document Search","There are no pages in this document.",Exclamation!)
		elseif li_rowsreturned < 0 then
			MessageBox("Document Search","Unable to open document. Please try again.",Exclamation!)		
		END IF


end if

	





end event

type rb_docid from radiobutton within w_reference_search
integer x = 96
integer y = 312
integer width = 73
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
end type

event clicked;Long	ll_null

SetNull(ll_null)

dw_ref_search_criteria.Enabled = TRUE
IF rb_docid.Checked = TRUE Then
	// Enable 'docid' fields, disable and blank 'refno' fields
	dw_ref_search_criteria.SetTabOrder("refno_lo",0)
	dw_ref_search_criteria.SetTabOrder("refno_hi",0)
	dw_ref_search_criteria.SetTabOrder("batch_lo",0)
	dw_ref_search_criteria.SetTabOrder("batch_hi",0)
	dw_ref_search_criteria.SetItem(1,"refno_lo",ll_null)
	dw_ref_search_criteria.SetItem(1,"refno_hi",ll_null)
	dw_ref_search_criteria.SetItem(1,"batch_lo",ll_null)
	dw_ref_search_criteria.SetItem(1,"batch_hi",ll_null)
	dw_ref_search_criteria.SetTabOrder("docid_lo",30)
	dw_ref_search_criteria.SetTabOrder("docid_hi",40)
END IF
cb_search.Enabled = TRUE

dw_ref_search_criteria.SetFocus()
dw_ref_search_criteria.SetColumn("docid_lo")
end event

type rb_refno from radiobutton within w_reference_search
integer x = 96
integer y = 236
integer width = 64
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
end type

event clicked;Long	ll_null

SetNull(ll_null)

dw_ref_search_criteria.Enabled = TRUE
IF rb_refno.Checked = TRUE Then
	// Enable 'refno' fields, disable and blank 'docid' fields
	dw_ref_search_criteria.SetTabOrder("refno_lo",10)
	dw_ref_search_criteria.SetTabOrder("refno_hi",20)
	dw_ref_search_criteria.SetTabOrder("docid_lo",0)
	dw_ref_search_criteria.SetTabOrder("docid_hi",0)
	dw_ref_search_criteria.SetTabOrder("batch_lo",0)
	dw_ref_search_criteria.SetTabOrder("batch_hi",0)
	dw_ref_search_criteria.SetItem(1,"docid_lo",ll_null)
	dw_ref_search_criteria.SetItem(1,"docid_hi",ll_null)
	dw_ref_search_criteria.SetItem(1,"batch_lo",ll_null)
	dw_ref_search_criteria.SetItem(1,"batch_hi",ll_null)
END IF
cb_search.Enabled = TRUE

dw_ref_search_criteria.SetFocus()
dw_ref_search_criteria.SetColumn("refno_lo")
end event

type dw_ref_search_criteria from u_dw_online within w_reference_search
integer x = 55
integer y = 188
integer width = 2670
integer height = 300
integer taborder = 60
string dataobject = "d_ref_search_criteria"
end type

event itemchanged;call super::itemchanged;LONG	ll_refno_lo, ll_refno_hi, ll_docid_lo, ll_docid_hi,ll_batch_hi,ll_batch_lo

ll_refno_lo = dw_ref_search_criteria.GetItemNumber(1,"refno_lo")
ll_refno_hi = dw_ref_search_criteria.GetItemNumber(1,"refno_hi")
ll_docid_lo = dw_ref_search_criteria.GetItemNumber(1,"docid_lo")
ll_docid_hi = dw_ref_search_criteria.GetItemNumber(1,"docid_hi")
ll_batch_hi = dw_ref_search_criteria.GetItemNumber(1,"batch_hi")
ll_batch_lo = dw_ref_search_criteria.GetItemNumber(1,"batch_lo")

/* Default 'hi' val to same as 'lo' val in case of single value search.
*/
	CHOOSE CASE GetColumnName()
		CASE "refno_lo"
			ll_refno_lo = Long(dw_ref_search_criteria.GetText())
			IF IsNull(ll_refno_hi) THEN
				dw_ref_search_criteria.SetItem(1,"refno_hi",ll_refno_lo)
			END IF

		CASE "docid_lo"
			ll_docid_lo = Long(dw_ref_search_criteria.GetText())
			IF IsNull(ll_docid_hi) THEN
				dw_ref_search_criteria.SetItem(1,"docid_hi",ll_docid_lo)
			END IF
			
		CASE "batch_lo"
			ll_batch_lo = Long(dw_ref_search_criteria.GetText())
			IF IsNull(ll_batch_hi) THEN
				dw_ref_search_criteria.SetItem(1,"batch_hi",ll_batch_lo)
			END IF
			
	END CHOOSE

end event

type gb_2 from groupbox within w_reference_search
integer x = 32
integer y = 532
integer width = 3127
integer height = 1144
integer taborder = 90
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Documents"
end type

type gb_1 from groupbox within w_reference_search
integer x = 27
integer y = 104
integer width = 3127
integer height = 400
integer taborder = 70
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Search Criteria"
borderstyle borderstyle = styleraised!
end type

type uo_image_append from u_image_append within w_reference_search
event destroy ( )
boolean visible = false
integer x = 1381
integer y = 1732
integer taborder = 30
boolean bringtotop = true
end type

on uo_image_append.destroy
call u_image_append::destroy
end on

type rb_batch from radiobutton within w_reference_search
integer x = 96
integer y = 388
integer width = 78
integer height = 76
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
end type

event clicked;Long	ll_null

SetNull(ll_null)

dw_ref_search_criteria.Enabled = TRUE
IF rb_batch.Checked = TRUE Then
	// Enable 'refno' fields, disable and blank 'docid' fields
	dw_ref_search_criteria.SetTabOrder("refno_lo",0)
	dw_ref_search_criteria.SetTabOrder("refno_hi",0)
	dw_ref_search_criteria.SetTabOrder("docid_lo",0)
	dw_ref_search_criteria.SetTabOrder("docid_hi",0)
	dw_ref_search_criteria.SetTabOrder("batch_lo",10)
	dw_ref_search_criteria.SetTabOrder("batch_hi",20)
	dw_ref_search_criteria.SetItem(1,"docid_lo",ll_null)
	dw_ref_search_criteria.SetItem(1,"docid_hi",ll_null)
	dw_ref_search_criteria.SetItem(1,"refno_lo",ll_null)
	dw_ref_search_criteria.SetItem(1,"refno_hi",ll_null)
END IF
cb_search.Enabled = TRUE

dw_ref_search_criteria.SetFocus()
dw_ref_search_criteria.SetColumn("batch_lo")
end event

