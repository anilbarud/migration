$PBExportHeader$w_indexing_search.srw
$PBExportComments$Response window used by indexing module. It contains the basic search functions (u_claim_search), but also additional functionality required for indexing.
forward
global type w_indexing_search from w_ancestor
end type
type dw_document_path from u_dw_document_path within w_indexing_search
end type
type cb_resize_doclist from commandbutton within w_indexing_search
end type
type cb_refresh_document_list from commandbutton within w_indexing_search
end type
type dw_documents from u_dw_online within w_indexing_search
end type
type uo_claim_search from u_claim_search within w_indexing_search
end type
type cb_cancel from commandbutton within w_indexing_search
end type
type cb_index_to_claim from commandbutton within w_indexing_search
end type
type cb_create_claim from commandbutton within w_indexing_search
end type
type cb_image_claim from commandbutton within w_indexing_search
end type
type uo_image_append from u_image_append within w_indexing_search
end type
end forward

global type w_indexing_search from w_ancestor
integer x = 1335
integer y = 300
integer width = 3191
integer height = 2448
string menuname = ""
boolean controlmenu = false
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
long backcolor = 67108864
event type long ue_rowfocuschanged ( long al_row )
dw_document_path dw_document_path
cb_resize_doclist cb_resize_doclist
cb_refresh_document_list cb_refresh_document_list
dw_documents dw_documents
uo_claim_search uo_claim_search
cb_cancel cb_cancel
cb_index_to_claim cb_index_to_claim
cb_create_claim cb_create_claim
cb_image_claim cb_image_claim
uo_image_append uo_image_append
end type
global w_indexing_search w_indexing_search

type variables
w_document_indexing	iwi_window_parent
s_window_message		istr_window_message
long			il_document_row_number
string                    		is_image_status
u_dw_document_path 	iu_dw_document_path
w_document_details   	iw_document_details
end variables

forward prototypes
public subroutine wf_set_parent (window window_parent)
public function integer wf_set_claim (long claim_no)
public subroutine wf_initalize_basic_claim ()
end prototypes

event type long ue_rowfocuschanged(long al_row);String ls_imaged_flag
long ll_claim_no, ll_result

/*	get the claim number from the selected row
*/	
	ll_result = uo_claim_search.dw_search_list.GetSelectedRow(0)
	IF ll_result < 1 THEN
//		MessageBox('No Claims','There is no claim selected')
		dw_documents.Reset()
		Return -1
	END IF

	ll_claim_no = uo_claim_search.dw_search_list.GetItemNumber(ll_result,'claim_no')

	SELECT imaged_flag
	  INTO :ls_imaged_flag
	  FROM CLAIM
	 WHERE claim_no = :ll_claim_no
	USING SQLCA;

	IF SQLCA.nf_handle_error('Embedded SQL: CLAIM','w_indexing_search','cb_image_claim') < 0 THEN
		Return -1
	END IF
	
//	iwi_view_documents.wf_refresh_list(ll_claim_no, ls_imaged_flag)
	dw_documents.Retrieve(ll_claim_no, ls_imaged_flag, 'N')
	SQLCA.nf_handle_error('w_indexing_search', 'dw_documents', 'ue_rowFocusChanged')

Return 0
end event

public subroutine wf_set_parent (window window_parent);/*------------------------------------------------------------------------*/
/*  This function sets a pointer to the parent window.                    */
/*------------------------------------------------------------------------*/

iwi_window_parent = window_parent
end subroutine

public function integer wf_set_claim (long claim_no);/*---------------------------------------------------------------------*/
/*  This function triggers the parent windows 'wf_set_claim', passing  */
/*  the selected claim number.                                         */
/*---------------------------------------------------------------------*/

Integer	li_ReturnCode
//li_ReturnCode = iwi_window_parent.wf_set_claim(claim_no)
Return li_ReturnCode
end function

public subroutine wf_initalize_basic_claim ();this.getparent().dynamic wf_initialize_basic_claim()
end subroutine

event open;call super::open;/*------------------------------------------------------------------*/
/*  Set the default to an 'or' type of search for indexing.         */
/*------------------------------------------------------------------*/
	STRING				ls_module_source_code
	s_window_message	lstr_window_message
	s_claim_search_defaults		lstr_claim_search_defaults

	lstr_window_message = Message.PowerObjectParm
	
		
	wf_set_parent(lstr_window_message.awi_parent_window)
	uo_claim_search.uf_set_parent(This)
	uo_claim_search.uf_set_search_type(lstr_window_message.as_StringParm[1])
	uo_claim_search.uf_protect_searchtype("DISABLE")
/*	This is only temporary until preferrences can be added to the system
*/
		
	uo_claim_search.dw_search_list.SetSort('accident_date D')
	uo_claim_search.dw_search_list.Sort()
	
//	Open(iwi_view_documents, 'w_view_documents')

	IF SQLCA.ServiceAvailable() THEN
		dw_documents.SetTransObject(SQLCA)
	END IF
	
	IF imagetrans.ServiceAvailable() THEN
		dw_document_path.SetTransObject(imagetrans)
	END IF

	dw_documents.uf_SetSelect(3)
	
/*	Create an instance of the user object for the view/print function
*/
	iu_dw_document_path = dw_document_path
	iu_dw_document_path.Hide()
	iu_dw_document_path.uf_set_window_handle(Handle(This))
	
	//due to the multiple viewer configuarations we want to set this close to the frame location
	this.x = w_frame.x + 300
	
	IF UpperBound(lstr_window_message.apo_PowerObjectParm) = 1 Then		
		lstr_claim_search_defaults = lstr_window_message.apo_PowerObjectParm[1]
		uo_claim_search.dw_search_criteria.SetITem(1,'medicare',String(lstr_claim_search_defaults.medicare_no))
		uo_claim_search.dw_search_criteria.SetITem(1,'sin',String(lstr_claim_search_defaults.sin_no))		
		uo_claim_search.dw_search_criteria.SetITem(1,'first_name',lstr_claim_search_defaults.given_names)			
		uo_claim_search.dw_search_criteria.SetITem(1,'last_name',lstr_claim_search_defaults.last_name)			
		uo_claim_search.dw_search_criteria.SetITem(1,'birth_date',lstr_claim_search_defaults.birth_date)			

		uo_claim_search.cb_search.triggerevent('clicked')
		
		if uo_claim_search.dw_search_list.RowCount() = 1 Then
			cb_index_to_claim.SetFocus()
		End if
		
	End if
	
end event

on w_indexing_search.create
int iCurrent
call super::create
this.dw_document_path=create dw_document_path
this.cb_resize_doclist=create cb_resize_doclist
this.cb_refresh_document_list=create cb_refresh_document_list
this.dw_documents=create dw_documents
this.uo_claim_search=create uo_claim_search
this.cb_cancel=create cb_cancel
this.cb_index_to_claim=create cb_index_to_claim
this.cb_create_claim=create cb_create_claim
this.cb_image_claim=create cb_image_claim
this.uo_image_append=create uo_image_append
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_document_path
this.Control[iCurrent+2]=this.cb_resize_doclist
this.Control[iCurrent+3]=this.cb_refresh_document_list
this.Control[iCurrent+4]=this.dw_documents
this.Control[iCurrent+5]=this.uo_claim_search
this.Control[iCurrent+6]=this.cb_cancel
this.Control[iCurrent+7]=this.cb_index_to_claim
this.Control[iCurrent+8]=this.cb_create_claim
this.Control[iCurrent+9]=this.cb_image_claim
this.Control[iCurrent+10]=this.uo_image_append
end on

on w_indexing_search.destroy
call super::destroy
destroy(this.dw_document_path)
destroy(this.cb_resize_doclist)
destroy(this.cb_refresh_document_list)
destroy(this.dw_documents)
destroy(this.uo_claim_search)
destroy(this.cb_cancel)
destroy(this.cb_index_to_claim)
destroy(this.cb_create_claim)
destroy(this.cb_image_claim)
destroy(this.uo_image_append)
end on

type dw_document_path from u_dw_document_path within w_indexing_search
boolean visible = false
integer x = 672
integer y = 1820
integer width = 613
integer taborder = 10
boolean enabled = true
end type

type cb_resize_doclist from commandbutton within w_indexing_search
integer x = 2482
integer y = 1952
integer width = 101
integer height = 80
integer taborder = 20
integer textsize = -10
integer weight = 700
fontcharset fontcharset = symbol!
fontpitch fontpitch = variable!
string facename = "Monotype Sorts"
string text = " s"
end type

event clicked;//	Note: the picture on this command button is accomplished by using the
//	monotype sorts font.  A maximize arrow is the letter 's' and a
//	minimize arrow is the letter 't'


//	Hide the buttons

	cb_refresh_document_list.visible = False
	cb_resize_doclist.visible = False

If this.text = " s" Then

//	Move and resize the height of the Document List/Index

	dw_documents.Y = 533
	dw_documents.Height=1805

//	Move the controls that reside on the Document List/Index

	cb_refresh_document_list.Y=533
	cb_resize_doclist.Y=533


//	Scroll the current row into view

	IF il_document_row_number > 0 THEN
		dw_documents.ScrollToRow(il_document_row_number)
	END IF

	This.text = " t"

Else

	//	Move and resize the height of the Document List/Index  

	dw_documents.Y=1953
	dw_documents.Height=389

	//	Move the controls that reside on the Document List/Index 

	cb_refresh_document_list.Y=1953
	cb_resize_doclist.Y=1953

	//	Scroll the current row into view

	IF il_document_row_number > 0 THEN
		dw_documents.ScrollToRow(il_document_row_number)
	END IF

	This.Text = " s"
End If

//	Show the buttons

cb_refresh_document_list.visible = True
cb_resize_doclist.visible = True

end event

type cb_refresh_document_list from commandbutton within w_indexing_search
integer x = 2377
integer y = 1952
integer width = 101
integer height = 80
integer taborder = 30
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "R"
end type

event clicked;//
//	Refresh the document list

Parent.Event ue_rowfocuschanged(0)

end event

type dw_documents from u_dw_online within w_indexing_search
integer x = 5
integer y = 1952
integer width = 3163
integer height = 388
integer taborder = 40
string dataobject = "d_documents"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event doubleclicked;call super::doubleclicked;LONG    ll_doc_id
string ls_doc_type
integer li_rtn

/*	Get the number of the row that was selected
	Only continue if a row was selected
*/

	il_document_row_number = row
	IF il_document_row_number <= 0 THEN
		RETURN
	END IF

/*	Get the document id for selected row
	View the document
*/
	ll_doc_id =dw_documents.GetItemNumber (il_document_row_number,"ref_docid")
	
	
		
	if uo_image_append.of_init(ll_doc_id)	<= 0 then
		RETURN
	end if
		
		
	ls_doc_type =  uo_image_append.of_get_file_type()
		
	
	CHOOSE CASE ls_doc_type
		/*  Imaged document */ 
		CASE 'IMA', 'TIF'
			uo_image_append.of_set_option()
			li_rtn = uo_image_append.of_append_image(ll_doc_id)
			if li_rtn < 0 then
				RETURN
			end if
		case else
			iu_dw_document_path.f_manage_document(ll_doc_id,"V","NORMAL")
	end choose
		
	Parent.Enabled = TRUE

end event

event rbuttondown;Long    				ll_rownum
m_document_popup_indexing	lm_document_popup_indexing

If This.RowCount() > 0 Then
	//	Create an instance of the popup menu
	lm_document_popup_indexing = Create m_document_popup_indexing

	//	Call the menu function to register the datawindow
	lm_document_popup_indexing.mf_set_datawindow(This)

	//	Popup the menu
	lm_document_popup_indexing.m_docdetails.popmenu(parent.PointerX(),parent.PointerY())
	Destroy lm_document_popup_indexing
End If

end event

type uo_claim_search from u_claim_search within w_indexing_search
integer taborder = 50
end type

on ue_doubleclicked;call u_claim_search::ue_doubleclicked;cb_index_to_claim.TriggerEvent(Clicked!)

end on

on uo_claim_search.destroy
call u_claim_search::destroy
end on

type cb_cancel from commandbutton within w_indexing_search
integer x = 2249
integer y = 1824
integer width = 411
integer height = 96
integer taborder = 90
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cance&l"
end type

on clicked;close(Parent)
end on

type cb_index_to_claim from commandbutton within w_indexing_search
integer x = 1787
integer y = 1824
integer width = 421
integer height = 96
integer taborder = 80
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Index to Claim"
end type

on clicked;LONG		ll_claim_no
INTEGER	li_Row
/*--------------------------------------------------------------------------*/
/*  The document the user has belongs to the current selected document, so  */
/*  retrieve the claim and pass control back to the indexing application.   */
/*--------------------------------------------------------------------------*/


	li_Row = uo_claim_search.dw_search_list.GetRow()
	IF li_Row < 1 THEN
		MessageBox("Missing Claim","A claim must be selected to index the document.",Exclamation!)
		Return
	END IF

	ll_claim_no = uo_claim_search.dw_search_list.GetItemNumber(li_Row,"claim_no")
	iwi_window_parent.wf_set_claim(ll_claim_no)
	Close(Parent)
Return

end on

type cb_create_claim from commandbutton within w_indexing_search
integer x = 1330
integer y = 1824
integer width = 434
integer height = 96
integer taborder = 70
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Create Claim"
end type

event clicked;STRING            ls_last_name, ls_first_name
INTEGER	         li_ReturnCode, li_row
S_WINDOW_MESSAGE  lstr_window_message

/*-----------------------------------------------------------------*/
/*  If any claims were found matching the search criteria, check   */
/*  to see if any are for the individual you want to create a claim*/
/*  for.                                                           */
/*-----------------------------------------------------------------*/

//*******************************************************************
// determine which search list is visible
// the claim list or the individual list
//*******************************************************************
IF uo_claim_search.dw_search_list_individual.visible THEN
// the double clicking was on the individual list
   IF uo_claim_search.dw_search_list_individual.RowCount() > 0 THEN
	   li_Row = uo_claim_search.dw_search_list_individual.GetRow()
   	ls_last_name = uo_claim_search.dw_search_list_individual.GetItemString(li_Row,"last_name")
	   ls_first_name = uo_claim_search.dw_search_list_individual.GetItemString(li_Row,"given_names")

   	li_ReturnCode = MessageBox("Create Claim","Do you wish to use this individual (" + ls_first_name + " " + ls_last_name + ") for the new claim?",Question!,YesNoCancel!)

	   IF li_ReturnCode = 3 THEN			
    		Return
   	/*--------------------------------------------------------------*/
	   /*  Chose the 'No' button, so you must create      individual   */
	   /*--------------------------------------------------------------*/
   	ELSEIF li_ReturnCode = 2 THEN
	   	lstr_window_message.awi_parent_window = iwi_window_parent
		   lstr_window_message.al_DoubleParm[1] = 0
			Close(Parent)
			SetPointer(HourGlass!)
			OpenWithParm(w_create_claim,lstr_window_message)
   	/*--------------------------------------------------------------*/
	   /*  Chose the 'Yes' button     */
   	/*--------------------------------------------------------------*/
	   ELSE	
   		lstr_window_message.awi_parent_window = iwi_window_parent
		   lstr_window_message.al_DoubleParm[1] = uo_claim_search.dw_search_list_individual.GetItemNumber(li_Row,"individual_no")
			Close(Parent)
			SetPointer(HourGlass!)
			OpenWithParm(w_create_claim,lstr_window_message)
	   END IF
   ELSE
   	lstr_window_message.awi_parent_window = iwi_window_parent
	   lstr_window_message.al_DoubleParm[1] = 0
		Close(Parent)
		SetPointer(HourGlass!)
		OpenWithParm(w_create_claim,lstr_window_message)
   END IF
ELSE
   IF uo_claim_search.dw_search_list.RowCount() > 0 THEN
	   li_Row = uo_claim_search.dw_search_list.GetRow()
   	ls_last_name = uo_claim_search.dw_search_list.GetItemString(li_Row,"last_name")
	   ls_first_name = uo_claim_search.dw_search_list.GetItemString(li_Row,"given_names")

   	li_ReturnCode = MessageBox("Create Claim","Do you wish to use this individual (" + ls_first_name + " " + ls_last_name + ") for the new claim?",Question!,YesNoCancel!)

	   IF li_ReturnCode = 3 THEN			
    		Return
   	/*--------------------------------------------------------------*/
	   /*  Chose the 'No' button, so you must send back 0 for          */
   	/*  individual no                                               */
	   /*--------------------------------------------------------------*/
   	ELSEIF li_ReturnCode = 2 THEN
	   	lstr_window_message.awi_parent_window = iwi_window_parent
		   lstr_window_message.al_DoubleParm[1] = 0
			Close(Parent)
			SetPointer(HourGlass!)
			OpenWithParm(w_create_claim,lstr_window_message)
   	/*--------------------------------------------------------------*/
	   /*  Chose the 'Yes' button, so go and create the claim.         */
   	/*--------------------------------------------------------------*/
	   ELSE	
   		lstr_window_message.awi_parent_window = iwi_window_parent
		   lstr_window_message.al_DoubleParm[1] = uo_claim_search.dw_search_list.GetItemNumber(li_Row,"individual_no")
			Close(Parent)
			SetPointer(HourGlass!)
			OpenWithParm(w_create_claim,lstr_window_message)
	   END IF
   ELSE
   	lstr_window_message.awi_parent_window = iwi_window_parent
	   lstr_window_message.al_DoubleParm[1] = 0
		Close(Parent)
		SetPointer(HourGlass!)
		OpenWithParm(w_create_claim,lstr_window_message)
   END IF
END IF
end event

type cb_image_claim from commandbutton within w_indexing_search
integer x = 197
integer y = 1824
integer width = 434
integer height = 96
integer taborder = 60
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "I&mage Claim"
end type

event clicked;INTEGER		li_rtn
LONG			ll_result, ll_claim_no
N_IMAGING	lnv_imaging
STRING		ls_imaged_flag
N_PROCESS_RUN_STATUS ln_process_run_status

/******************************************************************************************
P10275 - Daytime Payment Processing
- added new function call to prevent updating of tables used by PRODSVCS Payment Processing
- new object N_PROCESS_RUN_STATUS is used to determine status of processes (in progress = Y/N)
- '019' refers to the Document Indexing module, '044' refers to the Payment Processing module
******************************************************************************************/
ln_process_run_status = Create N_PROCESS_RUN_STATUS

li_rtn = ln_process_run_status.nf_in_progress('019','044','imaging of the claim',SQLCA)

IF li_rtn = 1 THEN
	// module is blocked
	return
END IF
/******************************************************************************************/


/*	this option was removed from uf_set_claim - the users thought this was to easy to make a mistake
	it is now an option by itself
*/

	IF uo_claim_search.dw_search_list_individual.visible THEN
		MessageBox('Warning','You must select the claimant before imaging the claim.')
		Return
	END IF
/*	this should not happen since you must have a valid claim before you get here
*/

/*	get the claim number from the selected row
*/	
	ll_result = uo_claim_search.dw_search_list.GetSelectedRow(0)
	IF ll_result < 1 THEN
		MessageBox('No Claims','There is no claim selected')
		Return
	END IF

	ll_claim_no = uo_claim_search.dw_search_list.GetItemNumber(ll_result,'claim_no')
	IF uo_claim_search.dw_search_list.GetItemString(ll_result,'history_flag') = 'Y' THEN
		MessageBox('Warning','This claim is a history claim and cannot be imaged.')
		Return
	END IF

	SELECT imaged_flag
	  INTO :ls_imaged_flag
	  FROM CLAIM
	 WHERE claim_no = :ll_claim_no
	USING SQLCA;

	IF SQLCA.nf_handle_error('Embedded SQL: CLAIM','w_indexing_search','cb_image_claim') < 0 THEN
		Return
	END IF
	
	IF ls_imaged_flag = 'Y' THEN
		MessageBox('Claim Imaged','The claim is already imaged.')
		Return
	END IF

	lnv_imaging = Create n_imaging
/*	error check ???
*/	

	SQLCA.nf_begin_transaction()

/*	always try and update the CLAIM database first to avoid dead lock situations between the two
	databases
*/
	UPDATE CLAIM
  	   SET imaged_flag = 'Y'
	 WHERE claim_no = :ll_claim_no
	 USING SQLCA;
	
	IF SQLCA.nf_handle_error("Embedded SQL: Update CLAIM","w_indexing_search","cb_image_claim") < 0 THEN
/*		the error routine should rollback the data and the other trans has not been started so exit
*/
		Return 
	ELSE
/*		update to CLAIM was successful
		no update ImageTrans
*/
		ImageTrans.nf_begin_transaction()
		
		ll_result = lnv_imaging.nf_create_claimsmaster("w_indexing_search",ll_claim_no,"Y")
		IF ll_result <> 1 THEN
			IF SQLCA.nf_rollback_transaction() < 0 THEN
				Return
			END IF
			Return
		ELSE
/*			check the document index table for entries that may be marked as non imaged and sent.
			if correspondence has been created for the non-imaged claim and has been sent, it will not
			show up in the document list now that the claim is imaged.  We need to set the document_imaged_flag to Y
*/
			UPDATE DOCUMENT_INDEX
				SET imaged_document_flag = 'Y'
			 WHERE claim_no = :ll_claim_no
				AND sent_flag = 'Y'
				AND imaged_document_flag = 'N'
			 USING ImageTrans;

			IF ImageTrans.nf_handle_error('Embedded SQL: Update Document Index','w_indexing_search','cb_image_claim') < 0 THEN
				SQLCA.nf_rollback_transaction()
				Return
			END IF
			
			IF SQLCA.nf_commit_transaction() < 0 THEN
				ImageTrans.nf_rollback_transaction()
				Return 
			END IF
			
			IF ImageTrans.nf_commit_transaction() < 0 THEN
/*				if this fails then nothing can be done about the CLAIM transaction since it is already
				committed
*/
				Return
			END IF
			MessageBox('Completed','An entry has now been created in imaging for this claim.')
		END IF
	END IF
Return 
end event

type uo_image_append from u_image_append within w_indexing_search
boolean visible = false
integer x = 690
integer y = 1732
integer taborder = 100
boolean bringtotop = true
end type

on uo_image_append.destroy
call u_image_append::destroy
end on

