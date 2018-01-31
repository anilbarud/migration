$PBExportHeader$w_inbasket_document_index.srw
forward
global type w_inbasket_document_index from window
end type
type cb_cancel from commandbutton within w_inbasket_document_index
end type
type cb_ok from commandbutton within w_inbasket_document_index
end type
type uo_document_index from u_document_index within w_inbasket_document_index
end type
end forward

global type w_inbasket_document_index from window
integer x = 1335
integer y = 688
integer width = 2423
integer height = 1148
boolean titlebar = true
string title = "Document Index"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 67108864
cb_cancel cb_cancel
cb_ok cb_ok
uo_document_index uo_document_index
end type
global w_inbasket_document_index w_inbasket_document_index

type variables
S_WINDOW_MESSAGE	istr_window_message
W_SHEET		iw_sheet
N_REFERRAL inv_referral

end variables

forward prototypes
public function integer wf_set_claim (long al_claim_no)
public function integer wf_payments_exist_check (long al_doc_id)
end prototypes

public function integer wf_set_claim (long al_claim_no);LONG	ll_result

ll_result = iw_sheet.wf_set_claim(al_claim_no)

/* Anything but 1 means the the claim can't be touched.
*/
	IF ll_result <> 1 THEN
		RETURN -1
	END IF
	
	RETURN 1

end function

public function integer wf_payments_exist_check (long al_doc_id);INTEGER	li_recordsfound

/*	This function reads the PAYMENT_DOCUMENT table on the CLAIM database
	to see if there exists any records (payments) for the given document id.

	Arguments:	al_doc_id	- The document id to look for in PAYMENT_DOCUMENT.
*/

	SELECT count(*)
	  INTO :li_recordsfound
	  FROM PAYMENT_DOCUMENT
	 WHERE doc_id = :al_doc_id
	 USING SQLCA;

	IF SQLCA.nf_handle_error('Embedded SQL: PAYMENT_DOCUMENT', 'wf_payments_exist_check()','SELECT count(*) .....') < 0 THEN
		li_recordsfound = -1
	END IF

	RETURN li_recordsfound

end function

event open;LONG		ll_docid
BOOLEAN	lb_indexed = False
STRING	ls_doctype, ls_doc_sub_type, ls_doc_type_desc_e, ls_doc_subtype_desc_e, ls_doctype_desc
LONG     ll_count , ll_ref , ll_doc_check
STRING	ls_module_source_code
INTEGER	li_rtn

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


/* Get the document number.
*/
	istr_window_message = Message.PowerObjectParm
	ll_docid = istr_window_message.al_doubleparm[1]

/* Get the name of the active work sheet.
*/
	iw_sheet = w_frame.GetActiveSheet()
	IF NOT IsValid(iw_sheet) THEN
		MessageBox("InBasket","Could not determine active sheet. You may have to reboot.")
		Close(THIS)
		RETURN
	END IF
	
	


/* Get document Indexing information.
*/
	IF uo_document_index.uf_retrieve(ll_docid,lb_indexed) = -1 THEN
		Close(THIS)
		RETURN
	END IF

/* If the document is not already indexed then set it up with some defaults.
*/
	IF NOT lb_indexed THEN
		uo_document_index.dw_document_indexing.InsertRow(0)
		uo_document_index.dw_document_indexing.SetItem(1,"docid",ll_docid)
		uo_document_index.dw_document_indexing.SetItem(1,"imaged_document_flag","Y")
		uo_document_index.dw_document_indexing.SetItem(1,"source_code","I")
		uo_document_index.dw_document_indexing.SetItem(1,"sent_flag","N")
		
		
		/* check to see if the document does not exist in the Docid_Reference_Xref table
   		if it does exist DO NOT allow them to enter a reference_no if IT DOESN't
			exist then enable the reference_no field
		*/
		ll_count = 0
	
		SELECT count(*), reference_no, docid
		  INTO :ll_count , :ll_ref , :ll_doc_check
		  FROM Docid_Reference_Xref 
		 WHERE docid = :ll_docid
		 group by reference_no,docid
		  USING ImageTrans;
		 
		ImageTrans.nf_handle_error("w_inbasket_document_indexing","open event","SELECT count(*) INTO :ll_count")

		IF ll_count > 0 THEN
			uo_document_index.dw_document_indexing.Modify("reference_no.protect=1")
			uo_document_index.dw_document_indexing.Modify("reference_no.Background.mode=1")
			uo_document_index.dw_document_indexing.Modify("reference_no.Background.Color='553648127'")
			
			/* set the reference_no to that found */
			uo_document_index.dw_document_indexing.SetItem(1,"reference_no",ll_ref)
		ELSE
			uo_document_index.dw_document_indexing.Modify("reference_no.protect=0")
			uo_document_index.dw_document_indexing.Modify("reference_no.Background.mode=0")
			uo_document_index.dw_document_indexing.Modify("reference_no.Background.Color='16777215'")
		END IF
			
	ELSE
		
		
		/* retrieve module source code for document, to be used below
		*/
		select module_source_code 
		INTO :ls_module_source_code
		FROM DOC
		where docid = :ll_docid
		USING ImageTrans;
		
		ImageTrans.nf_handle_error("w_inbasket_document_indexing","open event","SELECT count(*) INTO :ll_count")
		
		
		
		IF ls_module_source_code = "12" THEN
			/* If this is a Mecial Aid E-invoice document, only allow the doc type to change
			*/
			//Filter the document type dropdown
			li_rtn = uo_document_index.idwc_document_type.SetFilter("type_code in('AC','AH')")
			IF li_rtn = -1 Then SignalError(-666,'Error Setting filter')	
				
			li_rtn = uo_document_index.idwc_document_type.Filter()
			IF li_rtn = -1 Then SignalError(-666,'Error filtering document type drop down.')
				
			uo_document_index.dw_document_indexing.Modify("date_on_document.protect=1")
			uo_document_index.dw_document_indexing.Modify("date_received.protect=1")
			uo_document_index.dw_document_indexing.Modify("comment.protect=1")
			uo_document_index.dw_document_indexing.Modify("english_flag.protect=1")
			uo_document_index.dw_document_indexing.Modify("service_provider_type_code.protect=1")
			uo_document_index.dw_document_indexing.Modify("service_provider_no.protect=1")
			uo_document_index.dw_document_indexing.Modify("reference_no.protect=1")
			uo_document_index.dw_document_indexing.Modify("source_code.protect=1")
			uo_document_index.dw_document_indexing.Modify("paid_flag.protect=1")
			uo_document_index.dw_document_indexing.Modify("sent_flag.protect=1")
			uo_document_index.dw_document_indexing.Modify("claim_no.Protect=1")
			uo_document_index.dw_document_indexing.Modify("document_desc.Protect=1")
			uo_document_index.dw_document_indexing.Modify("referral_flag.Protect=1")			
			
			uo_document_index.cb_provider_search.Enabled = False
			
		ELSEIF ls_module_source_code = "13" OR ls_module_source_code = "15" THEN//webform67 (15)
			/* If this is an SNB Imported Form 67 document, only allow the comment to change
			*/ 
			uo_document_index.dw_document_indexing.uf_protectcolumn ('claim_no', TRUE)
			uo_document_index.dw_document_indexing.uf_protectcolumn ('doc_class_code', TRUE)
			uo_document_index.dw_document_indexing.uf_protectcolumn ('type_code', TRUE)
			uo_document_index.dw_document_indexing.uf_protectcolumn ('doc_subtype_code', TRUE)
			uo_document_index.dw_document_indexing.uf_protectcolumn ('document_desc', TRUE)
			uo_document_index.dw_document_indexing.uf_protectcolumn ('date_on_document', TRUE)
			uo_document_index.dw_document_indexing.uf_protectcolumn ('date_received', TRUE)
			uo_document_index.dw_document_indexing.uf_protectcolumn ('comment', FALSE) // remains unprotected
			uo_document_index.dw_document_indexing.uf_protectcolumn ('english_flag', TRUE)
			uo_document_index.dw_document_indexing.uf_protectcolumn ('service_provider_type_code', TRUE)
			uo_document_index.dw_document_indexing.uf_protectcolumn ('service_provider_no', TRUE)
			uo_document_index.dw_document_indexing.uf_protectcolumn ('reference_no', TRUE)
			uo_document_index.dw_document_indexing.uf_protectcolumn ('source_code', TRUE)
			uo_document_index.dw_document_indexing.uf_protectcolumn ('paid_flag', TRUE)
			uo_document_index.dw_document_indexing.uf_protectcolumn ('sent_flag', TRUE)
			uo_document_index.dw_document_indexing.uf_protectcolumn ('referral_flag', TRUE)
			
			uo_document_index.cb_provider_search.Enabled = FALSE
			
		ELSE
				
			IF wf_payments_exist_check(ll_docid) <> 0 THEN
				uo_document_index.dw_document_indexing.Modify("doc_class_code.Protect=1")
				uo_document_index.dw_document_indexing.Modify("type_code.Protect=1")
				uo_document_index.dw_document_indexing.Modify("doc_subtype_code.Protect=1")				
			END IF
			uo_document_index.dw_document_indexing.Modify("claim_no.Protect=1")
			uo_document_index.dw_document_indexing.Modify("document_desc.Protect=1")
			uo_document_index.dw_document_indexing.Modify("source_code.Protect=1")
			uo_document_index.dw_document_indexing.Modify("sent_flag.Protect=1")
			uo_document_index.dw_document_indexing.Modify("reference_no.Protect=1")
		End if

		ls_doctype = uo_document_index.dw_document_indexing.GetItemString(1,"type_code") 
		ls_doc_sub_type =  uo_document_index.dw_document_indexing.GetItemString(1,"doc_subtype_code")
		
		select doc_type_desc_e
		into   :ls_doc_type_desc_e
		from  Doc_Type
		where doc_type_code = :ls_doctype
		using ImageTrans;
		
		ImageTrans.nf_handle_error("w_inbasket_document_index","open","Select from Document_Type")
		
		IF ls_doc_sub_type = '' or IsNull(ls_doc_sub_type) THEN
			uo_document_index.dw_document_indexing.Modify("doc_subtype_code.Protect=1")
		ELSE
			select doc_subtype_desc_e
			into   :ls_doc_subtype_desc_e
			from Doc_Subtype
			where doc_subtype_code = :ls_doc_sub_type
			using ImageTrans;
			
			ImageTrans.nf_handle_error("w_inbasket_document_index","open","Select from Document_Sub_Type")		
		END IF
		
		ls_doctype_desc = ls_doc_type_desc_e + " " + ls_doc_subtype_desc_e
		
		uo_document_index.dw_document_indexing.SetItem(1,"document_desc",ls_doctype_desc)
		
	END IF

	uo_document_index.dw_document_indexing.Modify("paid_flag.Protect=1")
	uo_document_index.uf_date_received(True)
	uo_document_index.cb_provider_search.Visible = True

end event

on w_inbasket_document_index.create
this.cb_cancel=create cb_cancel
this.cb_ok=create cb_ok
this.uo_document_index=create uo_document_index
this.Control[]={this.cb_cancel,&
this.cb_ok,&
this.uo_document_index}
end on

on w_inbasket_document_index.destroy
destroy(this.cb_cancel)
destroy(this.cb_ok)
destroy(this.uo_document_index)
end on

type cb_cancel from commandbutton within w_inbasket_document_index
integer x = 1198
integer y = 900
integer width = 274
integer height = 108
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cance&l"
end type

on clicked;Close(PARENT)
end on

type cb_ok from commandbutton within w_inbasket_document_index
integer x = 901
integer y = 900
integer width = 274
integer height = 108
integer taborder = 30
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
end type

event clicked;STRING	ls_type, ls_referral_flag, ls_referring_provider_type_code
DATETIME ldt_date, ldtm_referred_on_date
LONG     ll_docid, ll_ref_count, ll_claim_no, ll_sender, ll_referral_no
INTEGER  li_rtn
Boolean lb_commit = false

inv_referral = CREATE n_referral

/*	This script updates the document indexing information.
*/

/* Disable button to prevent second click.
*/
	Enabled = FALSE

/* Check that you should be here.
*/
	IF uo_document_index.uf_check_required(ls_type,ldt_date) = - 1 THEN
		GOTO Normal_Exit
	END IF
	
	
	ll_docid = uo_document_index.dw_document_indexing.GetItemNumber(1,'docid')

	//******************************************************************
	// P10151-47 NB Chiro Agreement, fixes for NBMS Agreement

	li_rtn = uo_document_index.uf_validate_NBMS_EFB(ll_docid)
	IF li_rtn < 0 THEN
		RETURN // do not allow change
		Enabled = TRUE
	END IF
	
	li_rtn = uo_document_index.uf_validate_Chiro_EFB(ll_docid)
	IF li_rtn < 0 THEN
		RETURN // do not allow change
		Enabled = TRUE
	END IF

	// end Chiro changes
	//*********************************************************************


	ImageTrans.nf_begin_transaction()
	
/* Update the table.
*/
	uo_document_index.dw_document_indexing.Update()
	IF ImageTrans.nf_handle_error("dw_document_indexing","w_inbasket","clicked for cb_inbasket_doc_index_ok") < 0 THEN
		Close(PARENT)
		RETURN
	END IF
	
/* update the Docid_Reference_Xref table so that the docid has it's indexed flag
	set to "Y" there may not be a record here in which case we don't really care.
*/
	IF ll_docid > 0 THEN
		UPDATE Docid_Reference_Xref  SET indexed_flag = "Y"
		 WHERE docid = :ll_docid
		 USING ImageTrans;
	
		ImageTrans.nf_handle_error("w_inbasket_document_indexing","clicked","UPDATE Docid_Reference_Xref")
	END IF	

/* Check the referral flag indicator
*/

	ls_referral_flag                          = uo_document_index.dw_document_indexing.getitemstring(1,'referral_flag')
    ll_claim_no                               = uo_document_index.dw_document_indexing.getitemnumber(1,'claim_no')
    ll_sender                                 = 	uo_document_index.dw_document_indexing.getitemnumber(1,'service_provider_no') 
    ls_referring_provider_type_code = uo_document_index.dw_document_indexing.getitemstring(1,'service_provider_type_code')
	ldtm_referred_on_date              = DateTime(Date(uo_document_index.dw_document_indexing.GetItemDatetime(1,"date_on_document")))
	
		
	Select count(*)
     Into    :ll_ref_count
     From  REHAB_REFERRAL
     Where doc_id = :ll_docid  
     Using  SQLCA;

    SQLCA.nf_handle_error('w_inbasket_document_index','cb_ok','Select From REHAB_REFERRAL')
	
	// If the referral flag is selected then insert or update the REHAB_REFERRAL table.
	IF ls_referral_flag = 'Y' Then
		
		SQLCA.nf_begin_transaction()
		
		// Check to see if it's an update or an insert
		IF ll_ref_count = 0 THEN
	 		IF inv_referral.nf_insert_referral(ll_claim_no, ll_sender,ls_referring_provider_type_code, ll_docid, ldtm_referred_on_date, 'S022',0,ll_referral_no) < 0 THEN   //ll_referral_no passed into function by reference
				SQLCA.nf_rollback_transaction()
				RETURN
			ELSE
				lb_commit = true
				
				//T015677 - create a direct referral task for 'SP' documents if business rules allow
				IF ls_type = 'SP' THEN					
					IF inv_referral.nf_create_direct_referral(ll_claim_no, ll_sender,ls_referring_provider_type_code, ll_docid, ldtm_referred_on_date, 'S022',0, ll_referral_no) < 0 THEN
						SQLCA.nf_rollback_transaction()
			  		 	Return
					END IF
				END IF
			END IF
		ELSE
    		IF inv_referral.nf_update_referral(ll_claim_no, ll_sender,ls_referring_provider_type_code, ll_docid, ldtm_referred_on_date, 'S022',0) < 0 THEN
				SQLCA.nf_rollback_transaction()
				RETURN 
			ELSE
				lb_commit = true
			END IF
		END IF	
	ELSE
		IF ll_ref_count > 0 THEN
			
			SQLCA.nf_begin_transaction()
		//this is a correction - there was a referral indicator and now it's been removed.
			IF inv_referral.nf_delete_referral(ll_claim_no, ll_docid)  < 0 THEN
				SQLCA.nf_rollback_transaction()
				RETURN 
			ELSE
				lb_commit = true
			END IF
		END IF
	END IF
		
		
	IF lb_commit = true THEN
		SQLCA.nf_commit_transaction()   
	END IF


/* Commit the database.
*/
   ImageTrans.nf_commit_transaction()

	Close(PARENT)
	Return
/* End of script.
*/
Normal_Exit:
   Enabled = TRUE

	

end event

type uo_document_index from u_document_index within w_inbasket_document_index
integer x = 55
integer y = 80
integer taborder = 10
end type

on uo_document_index.destroy
call u_document_index::destroy
end on

