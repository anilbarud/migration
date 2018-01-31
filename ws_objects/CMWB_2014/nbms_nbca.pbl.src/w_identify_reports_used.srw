$PBExportHeader$w_identify_reports_used.srw
$PBExportComments$window (popup- ancestor) used for project 10127 Phase2 Medical Society agreement
forward
global type w_identify_reports_used from w_ancestor
end type
type cb_save from commandbutton within w_identify_reports_used
end type
type cb_cancel from commandbutton within w_identify_reports_used
end type
type dw_identify_reports from u_dw_online within w_identify_reports_used
end type
type uo_image_append from u_image_append within w_identify_reports_used
end type
end forward

global type w_identify_reports_used from w_ancestor
integer x = 1289
integer y = 316
integer width = 2359
integer height = 1036
string title = "Documents Eligible for NBMS/NBCA Reporting Fee"
string menuname = ""
boolean controlmenu = false
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
long backcolor = 67108864
cb_save cb_save
cb_cancel cb_cancel
dw_identify_reports dw_identify_reports
uo_image_append uo_image_append
end type
global w_identify_reports_used w_identify_reports_used

type variables
U_DW_DOCUMENT_PATH 	iu_dw_document_path
BLOB			ablb_data
gstr_report_fee_documents istr_report_fee_documents
end variables

on w_identify_reports_used.create
int iCurrent
call super::create
this.cb_save=create cb_save
this.cb_cancel=create cb_cancel
this.dw_identify_reports=create dw_identify_reports
this.uo_image_append=create uo_image_append
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_save
this.Control[iCurrent+2]=this.cb_cancel
this.Control[iCurrent+3]=this.dw_identify_reports
this.Control[iCurrent+4]=this.uo_image_append
end on

on w_identify_reports_used.destroy
call super::destroy
destroy(this.cb_save)
destroy(this.cb_cancel)
destroy(this.dw_identify_reports)
destroy(this.uo_image_append)
end on

event open;call super::open;/* at this point we will grab the message object and do a retrieve for our datawindow
  
*/
LONG		ll_claim
DATETIME	ldt_accident_date

/* grab the claim number passed from the calling window - in this case w_claim/d_claim_maintain
*/
ll_claim = Message.DoubleParm

/* now do the claim status validations - accident date > 1999-06-01
*/
SELECT accident_date
  INTO :ldt_accident_date
  FROM CLAIM
 WHERE claim_no = :ll_claim
 USING SQLCA;
 
 sqlca.nf_handle_error("SELECT accident_date","w_identify_reports_used","Open") 

/* see if the accident date > 1999-06-01
*/
IF date(ldt_accident_date) <  date("1999-06-01") THEN
	RETURN
END IF

/* do a basic check
*/
IF ll_claim > 0 THEN
	dw_identify_reports.settransobject(sqlca)
	dw_identify_reports.retrieve(ll_claim)
	
	SQLCA.nf_handle_error("dw_identify_reports.retrieve(ll_claim)","w_identify_reports_used","open") 
	
ELSE
	RETURN
END IF

/*	Create an instance of the user object for the view function*/
iu_dw_document_path = u_dw_document_path
This.OpenUserObject(iu_dw_document_path)
iu_dw_document_path.Hide()





end event

type cb_save from commandbutton within w_identify_reports_used
integer x = 1627
integer y = 816
integer width = 274
integer height = 108
integer taborder = 10
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Ok"
boolean default = true
end type

event clicked;/* Information for this POPUP window for project 10127
   1. The list will include all SDD, MPD, and AD documents for the current claim
	   (DOCUMENT_INDEX.type_code in ('SDD','MPD','AD')
		
		AMMENDED: T017023 Sept 2015 - R.S.  the document types (in the datawindow's datasource) 
		have been revised to include just 'SDD' type for non-chiro providers and 'SDC type documents for 
		chiro providers (provider_sub_type-code = '04')
		
		
	2. Show the document type, physician's name, date of treatment and date received
	3. Documents to be listed by treatment date, received date.
	4. Have a check box for each row (document)displayed
	5. the check boxes are checked off by default
	6. there should be scroll bars on the datawindow
	7. When checked on, document is to be considered as used in the decision making process
	   and is to be inserted into ELIGIBLE_REPORT_FEE_DOCUMENTS when the save button is clicked
	8. Double clicking on a row will invoke the document viewer and display the document.
	9. Allow for right button click on datawindow to change the sort order
	10. There is to be two buttons - Save and Cancel. The save button is disabled by default
	11. At least one document must be flagged or checked as identified for reporting fee
	    to enable the Save Button
	12. The Cancel button closes the pop-up window and returns control to w_claim
	    without identifying any documents. The save button inserts the appropriate
		 records into ELIGIBILITY_REPORT_FEE_DOCUMENTS (see chart b), closes the pop-up
		 widow and returns control to w_claim
		 
--All saves will be made from the following rules
Click  on Save  button (on w_claim) saves changes to both the CLAIM and 
ELIGIBLE_REPORT_FEE_DOCUMENTS tables.
Click on cancel button (on w_claim) cancels  changes to both  the claim and 
ELIGIBLE_REPORT_FEE_DOCUMENTS tables.	

--new rule added nov 15 - "Forcing the adjudicator to select at least one document from the list
  of documents. The adjudicators have found a number of situations where they should'nt pay the
  reporting fee on medical reports, but they are forced to select them. Therefore, Sandra would 
  like it changed so that selecting a medical is optional. There should be an OK button instead
  of a save and cancel button. If they have NOT selected a medical document when they hit the 
  OK button, amessage box should be displayed confirming that they have not selected any
  medical documents for the reporting fee. If they hit the YES button then allow them to save 
  the claim status change. If they hit the NO button then go back to the POP up window."
*/

INT			li_counter,li_check_box,li_rowcount,li_accepted_count,li_messagebox
LONG			ll_docid,ll_claim_no,ll_provider
DATETIME		ldt_current
STRING		ls_option,ls_sub_type


li_rowcount =  dw_identify_reports.rowcount() 
IF li_rowcount < 1 THEN
	CLOSEWITHRETURN(PARENT,istr_report_fee_documents)
	RETURN
END IF 

ldt_current = f_server_datetime()

li_accepted_count = 0

FOR li_counter = 1 TO li_rowcount
	li_check_box = dw_identify_reports.getitemnumber(li_counter,"accepted")
	IF li_check_box = 1 THEN
		li_accepted_count ++
	END IF
NEXT

IF li_accepted_count = 0 THEN
	li_messagebox = messagebox("Medical Reporting Fee","No Medical Documents have been selected " +&
										"for the Reporting Fee, Is this correct?",question!,yesno!,2)
										
	IF li_messagebox = 2 THEN
		return
	ELSE
		CLOSEWITHRETURN(PARENT,istr_report_fee_documents)	
		return
	END IF
END IF

FOR li_counter = 1 TO li_rowcount
	//grab the value contained in the check box
	li_check_box = dw_identify_reports.getitemnumber(li_counter,"accepted")
		
	//grab the values from the datawindow that we will insert into our datastore
	ll_docid       = dw_identify_reports.getitemnumber(li_counter,"docid")
	ll_claim_no    = dw_identify_reports.getitemnumber(li_counter,"claim_no")
	ll_provider    = dw_identify_reports.getitemnumber(li_counter,"provider_no")
	ls_sub_type    = dw_identify_reports.getitemstring(li_counter,"provider_sub_type_code")
			
	/* put the values into a structure
	*/
	istr_report_fee_documents.docid[li_counter]                          = ll_docid
	istr_report_fee_documents.claim_no[li_counter]                       = ll_claim_no
	istr_report_fee_documents.reporting_fee_type_code[li_counter]        = "DC"
	istr_report_fee_documents.generated_method_code[li_counter]          = "M"
	istr_report_fee_documents.approved_date[li_counter]                  = ldt_current
	istr_report_fee_documents.approved_by_user_id[li_counter]            = vgst_user_profile.user_id
	istr_report_fee_documents.comment[li_counter]                        = "Created through claim maintenance when claim rejected"
	istr_report_fee_documents.reporting_fee_eligibility_code[li_counter] = "APP"
	istr_report_fee_documents.accepted[li_counter]                       = li_check_box										
	istr_report_fee_documents.provider_no[li_counter]                    = ll_provider
	istr_report_fee_documents.provider_sub_type[li_counter]              = ls_sub_type
NEXT

CLOSEWITHRETURN(PARENT,istr_report_fee_documents)	

	
end event

type cb_cancel from commandbutton within w_identify_reports_used
boolean visible = false
integer x = 1326
integer y = 816
integer width = 274
integer height = 108
integer taborder = 30
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Cancel"
boolean default = true
end type

event clicked;CLOSEWITHRETURN(PARENT,istr_report_fee_documents)	
end event

type dw_identify_reports from u_dw_online within w_identify_reports_used
event ue_view ( )
integer x = 32
integer y = 24
integer width = 2286
integer height = 780
integer taborder = 20
boolean bringtotop = true
string dataobject = "d_identify_reports_used"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event ue_view;LONG	ll_rownum,ll_doc_id
string ls_doc_type
integer li_rtn

//	Get the document id of the selected row

	ll_rownum = dw_identify_reports.GetRow()
	IF ll_rownum <= 0 THEN
		MessageBox("Medical Society Agreement","You must select a document before you can view it",Exclamation!)
		Return
	END IF

ll_doc_id = dw_identify_reports.GetItemNumber(ll_rownum,"docid")
	
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
			iu_dw_document_path.f_manage_document(dw_identify_reports.GetItemNumber(ll_rownum,"docid"),"V","NORMAL")
	END CHOOSE
			
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
	//lm_popup.m_options.m_moredetails.visible = TRUE
	//lm_popup.m_options.m_filterlist.visible = TRUE
	lm_popup.m_options.PopMenu(w_frame.PointerX( ), w_frame.PointerY( ))

	Destroy lm_popup
end event

event doubleclicked;call super::doubleclicked;this.TriggerEvent("ue_view")
end event

event itemchanged;call super::itemchanged;/* At least on button one document must be flagged or checked as 
   identified for reporting fee to enable the save button
*/

INT			li_check,li_counter
string 		ls_text

/* first we check if they are setting it on 
   if this is the case then we enable the check box
	If however they are turning it off then we must check to see if there are 
	any still left on if there are none then we turn the save button off
*/
//CHOOSE CASE  dwo.name
//	CASE "accepted"
//		IF data = "1" THEN
//			cb_save.enabled = TRUE
//			cb_save.default = TRUE
//		ELSE
//			FOR li_counter = 1 TO THIS.rowcount()
//				li_check = THIS.setrow(li_counter)//getitemnumber(li_counter,"accepted")
//				ls_text = this.gettext()
//				IF ls_text        = "1" THEN
//					cb_save.enabled = TRUE
//					cb_save.default = TRUE
//					RETURN 0
//				END IF
//			NEXT
//			cb_save.enabled   = FALSE
//			cb_cancel.default = TRUE
//		END IF
//	CASE ELSE
//END CHOOSE
end event

event losefocus;call super::losefocus;this.AcceptText ( )
end event

event clicked;call super::clicked;
/* all we want to do he is scroll to the row that has been highlighted
   the datawindow will do the rest.
*/
dw_identify_reports.selectrow(0,false)
dw_identify_reports.ScrollToRow(row)
dw_identify_reports.selectrow(row,TRUE)
end event

event rowfocuschanged;call super::rowfocuschanged;dw_identify_reports.selectrow(0,false)
dw_identify_reports.selectrow(currentrow,TRUE)
end event

type uo_image_append from u_image_append within w_identify_reports_used
boolean visible = false
integer x = 416
integer y = 904
integer taborder = 20
boolean bringtotop = true
end type

on uo_image_append.destroy
call u_image_append::destroy
end on

