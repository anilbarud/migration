$PBExportHeader$w_payment_inquiry.srw
forward
global type w_payment_inquiry from w_a_tool
end type
type dw_docs_for_payment_no from u_dw_online within w_payment_inquiry
end type
type st_receiving_salary_note from statictext within w_payment_inquiry
end type
type sle_receiving_salary_indicator from singlelineedit within w_payment_inquiry
end type
type uo_image_append from u_image_append within w_payment_inquiry
end type
type cb_view from commandbutton within w_payment_inquiry
end type
type cb_prev_transaction from commandbutton within w_payment_inquiry
end type
type cb_next_transaction from commandbutton within w_payment_inquiry
end type
type cb_legend from commandbutton within w_payment_inquiry
end type
type dw_list_payments from u_dw_online within w_payment_inquiry
end type
type dw_display_transaction_details from u_dw_online within w_payment_inquiry
end type
type dw_payment_details_display_only from u_dw_online within w_payment_inquiry
end type
type cb_more from commandbutton within w_payment_inquiry
end type
type cb_extract from commandbutton within w_payment_inquiry
end type
type uo_filter from u_filter_control within w_payment_inquiry
end type
type cb_switch from commandbutton within w_payment_inquiry
end type
end forward

global type w_payment_inquiry from w_a_tool
integer width = 2688
integer height = 1904
boolean resizable = false
long il_design_time_height = 1836
long il_design_time_width = 2688
dw_docs_for_payment_no dw_docs_for_payment_no
st_receiving_salary_note st_receiving_salary_note
sle_receiving_salary_indicator sle_receiving_salary_indicator
uo_image_append uo_image_append
cb_view cb_view
cb_prev_transaction cb_prev_transaction
cb_next_transaction cb_next_transaction
cb_legend cb_legend
dw_list_payments dw_list_payments
dw_display_transaction_details dw_display_transaction_details
dw_payment_details_display_only dw_payment_details_display_only
cb_more cb_more
cb_extract cb_extract
uo_filter uo_filter
cb_switch cb_switch
end type
global w_payment_inquiry w_payment_inquiry

type prototypes

end prototypes

type variables
string 		is_display_message
long	il_claim_no
string	is_receiving_salary_flag
u_dw_document_path iu_dw_document_path
M_DW_ONLINE_RMB_POPUP im_popup

end variables

forward prototypes
public function integer wf_setup_address (string as_recipient_type_code, long al_recipient_no)
public function integer wf_get_recipient_name (string as_recipient_type_code, long al_recipient_no)
public function integer wf_receiving_salary_note ()
end prototypes

public function integer wf_setup_address (string as_recipient_type_code, long al_recipient_no);LONG             ll_result, ll_tranrow
STRING           ls_recipient_care_of, ls_recipient_street, ls_recipient_city, ls_recipient_province
STRING           ls_recipient_country, ls_recipient_postal_code, ls_recipient_name
DATAWINDOWCHILD  ldwc_child
DATAwindow	  lds_basic_claim

/* Function Name: wf_setup_address                                                                      

	Purpose:       The purpose of this function is to load the current transaction record with default   
		            address information                                                                   

	Arguments:     as_recipient_type_code - identifies the recipient type so we know where to go to get 
						the address                                                 
						al_recipient_no - if the recipient is not the claimant or employer, this contains    
						the payee number                                                   

	Return Values: n/a                                                                                   
*/

/*MA000810 commented out */
//	ll_tranrow = dw_display_transaction_details.GetRow()
//	lds_basic_claim = CREATE DATASTORE
//	lds_basic_claim.dataobject = 'd_basic_claim'
//	lds_basic_claim.SetTransObject(SQLCA)
//	lds_basic_claim.Retrieve(il_claim_no)
/*MA000810 */


lds_basic_claim = iw_active_sheet.dw_basic_claim  														/*PR 1407*/


	CHOOSE CASE as_recipient_type_code
/* If recipient type code empty, we just want to blank out the address and get out
*/
	   CASE ""
		   dw_display_transaction_details.SetItem(ll_tranrow,"address_line1","")
   		dw_display_transaction_details.SetItem(ll_tranrow,"address_line2","")
		   dw_display_transaction_details.SetItem(ll_tranrow,"city","")
   	 	dw_display_transaction_details.SetItem(ll_tranrow,"prov_state_code","")
    		dw_display_transaction_details.SetItem(ll_tranrow,"country","")
	      dw_display_transaction_details.SetItem(ll_tranrow,"postal_code","")
/* 		If recipient is claimant, get the address from the basic claim info            
*/
	   CASE "I"
			IF al_recipient_no = 0 THEN
	   	   dw_display_transaction_details.SetItem(ll_tranrow,"address_line1",lds_basic_claim.GetItemString(1,'address_line1'))
   	   	dw_display_transaction_details.SetItem(ll_tranrow,"address_line2",lds_basic_claim.GetItemString(1,'address_line2'))
	   	   dw_display_transaction_details.SetItem(ll_tranrow,"city",lds_basic_claim.GetItemString(1,'city'))
   	   	dw_display_transaction_details.SetItem(ll_tranrow,"prov_state_code",lds_basic_claim.GetItemString(1,'prov_state_code'))
	      	ls_recipient_country = lds_basic_claim.GetItemString(1,'country_code')
/*				need to get the description
*/

  			   lds_basic_claim.GetChild('country_code', ldwc_child)
   	   	ll_result = ldwc_child.Find('location_code = "' + ls_recipient_country + '"', 1, ldwc_child.RowCount())
		      IF ll_result > 0 THEN
   	      	ls_recipient_country = ldwc_child.GetItemString(ll_result, 'location_desc1')
      		END IF
			   dw_display_transaction_details.SetItem(ll_tranrow,"country",ls_recipient_country)
   		   dw_display_transaction_details.SetItem(ll_tranrow,"postal_code",lds_basic_claim.GetItemString(1,'postal_code'))
			ELSE
				SELECT given_names + ' ' + last_name, address_line1, address_line2,   
		             city, prov_state_code, country_code, postal_code  
	   		  INTO :ls_recipient_name, :ls_recipient_care_of, :ls_recipient_street, :ls_recipient_city, 
   	   	       :ls_recipient_province, :ls_recipient_country, :ls_recipient_postal_code  
			     FROM INDIVIDUAL
			    WHERE ( individual_no = :al_recipient_no ) 
	   	    USING SQLCA ;
			
			   ll_result = SQLCA.nf_handle_error("Embedded SQL: Retrieve on INDIVIDUAL","w_payment_inquiry","wf_setup_address")
			   IF ll_result < 0 THEN
					//DESTROY lds_basic_claim
	   			return -1
		   	ELSEIF ll_result = 100 THEN
					//DESTROY lds_basic_claim
			   	Return 100
		   	END IF
		   	dw_display_transaction_details.SetItem(ll_tranrow,"recipient_name",ls_recipient_name)
			   ls_recipient_care_of = Left(ls_recipient_care_of,29)
   			dw_display_transaction_details.SetItem(ll_tranrow,"address_line1",ls_recipient_care_of)
			   dw_display_transaction_details.SetItem(ll_tranrow,"address_line2",ls_recipient_street)
	   		dw_display_transaction_details.SetItem(ll_tranrow,"city",ls_recipient_city)
		   	dw_display_transaction_details.SetItem(ll_tranrow,"prov_state_code",ls_recipient_province)
				SELECT location_desc1
				  INTO :ls_recipient_country
				  FROM Location
				 WHERE location_code = :ls_recipient_country
				   AND location_type_code = 'C';

				IF SQLCA.nf_handle_error('Embedded SQL: Select from Location','w_payment_inquiry','wf_setup_address') < 0 THEN
					//DESTROY lds_basic_claim
					Return -1
				END IF
   			dw_display_transaction_details.SetItem(ll_tranrow,"country",ls_recipient_country)
		      dw_display_transaction_details.SetItem(ll_tranrow,"postal_code",ls_recipient_postal_code)

			END IF

	   CASE ELSE
/*		If recipient is a payee, read the address from Service Providers table          
*/
			IF IsNull(as_recipient_type_code) OR IsNull(al_recipient_no) THEN
				MessageBox('Warning','Recipient number or type is missing')
				//DESTROY lds_basic_claim
				Return -1
			END IF
		   SELECT PROVIDER.name, PROVIDER.address_line1, PROVIDER.address_line2,   
	             PROVIDER.city, PROVIDER.prov_state_code, PROVIDER.country_code, PROVIDER.postal_code  
	   	  INTO :ls_recipient_name, :ls_recipient_care_of, :ls_recipient_street, :ls_recipient_city, 
   	          :ls_recipient_province, :ls_recipient_country, :ls_recipient_postal_code  
		     FROM PROVIDER  
		    WHERE ( PROVIDER.provider_no = :al_recipient_no ) AND  
			   	 ( PROVIDER.provider_type_code = :as_recipient_type_code) AND
      	       ( PROVIDER.active_flag = 'Y' )  
	       USING SQLCA ;
		   ll_result = SQLCA.nf_handle_error("Embedded SQL: Retrieve on SERVICE_PROVIDER","w_payment_inquiry","wf_setup_address")
		   IF ll_result < 0 THEN
				//DESTROY lds_basic_claim
	   		return -1
		   ELSEIF ll_result = 100 THEN
				//DESTROY lds_basic_claim
			   Return 100
	   	END IF

	   	dw_display_transaction_details.SetItem(ll_tranrow,"recipient_name",ls_recipient_name)
			IF Trim(ls_recipient_street) = "" THEN
				ls_recipient_street = ls_recipient_care_of
				ls_recipient_care_of = ''
			END IF
   		IF ls_recipient_care_of = "" or ls_recipient_care_of = " " THEN
	   		ls_recipient_care_of = "RE: " + lds_basic_claim.GetItemString(1,'given_names') + ' '+ lds_basic_claim.GetItemString(1,'last_name') 
	   	END IF
		   ls_recipient_care_of = Left(ls_recipient_care_of,29)
   		dw_display_transaction_details.SetItem(ll_tranrow,"address_line1",ls_recipient_care_of)
		   dw_display_transaction_details.SetItem(ll_tranrow,"address_line2",ls_recipient_street)
	   	dw_display_transaction_details.SetItem(ll_tranrow,"city",ls_recipient_city)
	   	dw_display_transaction_details.SetItem(ll_tranrow,"prov_state_code",ls_recipient_province)
			SELECT location_desc1
			  INTO :ls_recipient_country
			  FROM Location
			 WHERE location_code = :ls_recipient_country
			   AND location_type_code = 'C';

			IF SQLCA.nf_handle_error('Embedded SQL: Select from Location','w_payment_inquiry','wf_setup_address') < 0 THEN
				//DESTROY lds_basic_claim
				Return -1
			END IF
   		dw_display_transaction_details.SetItem(ll_tranrow,"country",ls_recipient_country)
	      dw_display_transaction_details.SetItem(ll_tranrow,"postal_code",ls_recipient_postal_code)
	END CHOOSE	
	//DESTROY lds_basic_claim
Return 0
end function

public function integer wf_get_recipient_name (string as_recipient_type_code, long al_recipient_no);STRING	ls_recipient_name, ls_recipient_care_of, ls_recipient_street, ls_recipient_city, ls_recipient_province, &
			ls_recipient_country, ls_recipient_postal_code
LONG		ll_result

IF as_recipient_type_code = 'I' THEN

	SELECT	given_names + ' ' + last_name	//, address_line1, address_line2, city, prov_state_code, country_code, postal_code  
	  INTO	:ls_recipient_name	//, :ls_recipient_care_of, :ls_recipient_street, :ls_recipient_city, :ls_recipient_province, :ls_recipient_country, :ls_recipient_postal_code  
	  FROM	INDIVIDUAL
	 WHERE	( individual_no = :al_recipient_no )
	 USING SQLCA ;
			
	ll_result = SQLCA.nf_handle_error("Embedded SQL: Retrieve on INDIVIDUAL","w_payment_inquiry","wf_get_newest_recipient_info")
	IF ll_result < 0 THEN
	   Return -1
	END IF

ELSE

	SELECT	name	//, address_line1, address_line2, city, prov_state_code, country_code, postal_code
	  INTO	:ls_recipient_name	//, :ls_recipient_care_of, :ls_recipient_street, :ls_recipient_city, :ls_recipient_province, :ls_recipient_country, :ls_recipient_postal_code  
	  FROM	PROVIDER  
	 WHERE	( provider_no = :al_recipient_no ) AND
	 			( provider_type_code = :as_recipient_type_code) AND
				( active_flag = 'Y' )
	 USING SQLCA ;
	
	ll_result = SQLCA.nf_handle_error("Embedded SQL: Retrieve on PROVIDER","w_payment_inquiry","wf_get_newest_recipient_info")
	IF ll_result < 0 THEN
		Return -1
	END IF

END IF

dw_display_transaction_details.SetItem(dw_display_transaction_details.GetRow(),"recipient_name",ls_recipient_name)

Return 1
end function

public function integer wf_receiving_salary_note ();string ls_rs, ls_not_rs

	/*	We use these variables in determining whether or not the claim has a mix of
	receiving salary payments and issued payments
	*/

	IF is_receiving_salary_flag = "Y" THEN
		ls_rs 	  = "Y"
		ls_not_rs = "N"
	ELSE
		ls_rs     = "N"
		ls_not_rs = "Y"
	END IF
	
	IF ls_rs = "Y" AND ls_not_rs = "Y" THEN
		IF is_receiving_salary_flag = "Y" THEN
			st_receiving_salary_note.text = "Some payments have been issued when the claim was not marked as a Receiving Salary Claim"
		ELSE
			st_receiving_salary_note.text = "Some payments have been recorded with the claim marked as a Receiving Salary Claim"
		END IF
		st_receiving_salary_note.Show()
	END IF

	IF dw_list_payments.RowCount() > 0 THEN
		dw_list_payments.SelectRow(0,FALSE)
		dw_list_payments.SelectRow(1,True)
	END IF
	

	Return 0
	
end function

event open;call super::open;LONG		ll_result
INTEGER	li_rtn
STRING	ls_select, ls_sql
W_SHEET  lw_active_sheet

	dw_list_payments.settransobject (sqlca)
	dw_payment_details_display_only.settransobject (sqlca)
	dw_display_transaction_details.settransobject (sqlca)
	dw_docs_for_payment_no.settransobject (sqlca)

	
	// this function call, along with setting values in column, 
	// computed field or text datawindow objects will allow for display of 'tooltips'

	dw_list_payments.uf_setselect(1)
	dw_list_payments.uf_setfilter(True)
	uo_filter.uf_set_Requestor(dw_list_payments)
	
	
//	dw_list_payments.uf_settooltip(True)
	dw_list_payments.setFocus()
	dw_list_payments.Object.DataWindow.HideGrayLine=true	
	dw_list_payments.uf_SetSort(True)
	
	
	THIS.wf_setresize(TRUE)
	
	THIS.inv_resize.of_Register(dw_list_payments,'ScaleToRight&Bottom')
	THIS.inv_resize.of_Register(st_title,'ScaleToRight')
	THIS.inv_resize.of_Register(cb_extract,'FixedToRight&Bottom')
	THIS.inv_resize.of_Register(cb_view,'FixedToRight&Bottom')
	THIS.inv_resize.of_Register(cb_switch,'FixedToRight&Bottom')
	THIS.inv_resize.of_Register(cb_close,'FixedToRight&Bottom')
	THIS.inv_resize.of_Register(uo_filter,'FixedToBottom')
	THIS.inv_resize.of_Register(cb_legend,'FixedToBottom')
	THIS.inv_resize.of_Register(sle_receiving_salary_indicator,50,0,0,0)
		
		
	iu_dw_document_path = u_dw_document_path
	This.OpenUserObject(iu_dw_document_path)
	iu_dw_document_path.Hide()

	lw_active_sheet = w_frame.GetActiveSheet()
	il_claim_no = lw_active_sheet.dw_basic_claim.GetItemNumber(1,"claim_no")
	is_receiving_salary_flag = lw_active_sheet.dw_basic_claim.GetItemString(1,"receiving_salary_flag")


	IF is_receiving_salary_flag = "Y" THEN
		sle_receiving_salary_indicator.text = "Receiving Salary"
	ELSE
		sle_receiving_salary_indicator.text = ""
	END IF
	
	ll_result = dw_list_payments.retrieve(il_claim_no)
	SQLCA.nf_handle_error("dw_list_payments","w_payment_inquiry","open for w_payment_inquiry")
	

	





end event

on w_payment_inquiry.create
int iCurrent
call super::create
this.dw_docs_for_payment_no=create dw_docs_for_payment_no
this.st_receiving_salary_note=create st_receiving_salary_note
this.sle_receiving_salary_indicator=create sle_receiving_salary_indicator
this.uo_image_append=create uo_image_append
this.cb_view=create cb_view
this.cb_prev_transaction=create cb_prev_transaction
this.cb_next_transaction=create cb_next_transaction
this.cb_legend=create cb_legend
this.dw_list_payments=create dw_list_payments
this.dw_display_transaction_details=create dw_display_transaction_details
this.dw_payment_details_display_only=create dw_payment_details_display_only
this.cb_more=create cb_more
this.cb_extract=create cb_extract
this.uo_filter=create uo_filter
this.cb_switch=create cb_switch
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_docs_for_payment_no
this.Control[iCurrent+2]=this.st_receiving_salary_note
this.Control[iCurrent+3]=this.sle_receiving_salary_indicator
this.Control[iCurrent+4]=this.uo_image_append
this.Control[iCurrent+5]=this.cb_view
this.Control[iCurrent+6]=this.cb_prev_transaction
this.Control[iCurrent+7]=this.cb_next_transaction
this.Control[iCurrent+8]=this.cb_legend
this.Control[iCurrent+9]=this.dw_list_payments
this.Control[iCurrent+10]=this.dw_display_transaction_details
this.Control[iCurrent+11]=this.dw_payment_details_display_only
this.Control[iCurrent+12]=this.cb_more
this.Control[iCurrent+13]=this.cb_extract
this.Control[iCurrent+14]=this.uo_filter
this.Control[iCurrent+15]=this.cb_switch
end on

on w_payment_inquiry.destroy
call super::destroy
destroy(this.dw_docs_for_payment_no)
destroy(this.st_receiving_salary_note)
destroy(this.sle_receiving_salary_indicator)
destroy(this.uo_image_append)
destroy(this.cb_view)
destroy(this.cb_prev_transaction)
destroy(this.cb_next_transaction)
destroy(this.cb_legend)
destroy(this.dw_list_payments)
destroy(this.dw_display_transaction_details)
destroy(this.dw_payment_details_display_only)
destroy(this.cb_more)
destroy(this.cb_extract)
destroy(this.uo_filter)
destroy(this.cb_switch)
end on

type st_title from w_a_tool`st_title within w_payment_inquiry
integer x = 23
integer y = 16
integer width = 2729
string text = "Payment Inquiry"
end type

type cb_close from w_a_tool`cb_close within w_payment_inquiry
integer x = 2363
integer y = 1736
integer width = 302
integer height = 96
integer taborder = 20
end type

type dw_docs_for_payment_no from u_dw_online within w_payment_inquiry
boolean visible = false
integer x = 818
integer y = 1728
integer width = 425
integer height = 360
integer taborder = 40
string dataobject = "d_docs_for_payment_no"
end type

type st_receiving_salary_note from statictext within w_payment_inquiry
boolean visible = false
integer x = 27
integer y = 1692
integer width = 1440
integer height = 140
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
boolean focusrectangle = false
end type

type sle_receiving_salary_indicator from singlelineedit within w_payment_inquiry
integer x = 1728
integer y = 20
integer width = 814
integer height = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean border = false
boolean autohscroll = false
boolean displayonly = true
end type

type uo_image_append from u_image_append within w_payment_inquiry
boolean visible = false
integer x = 841
integer y = 1688
integer width = 133
integer height = 104
integer taborder = 120
boolean bringtotop = true
end type

on uo_image_append.destroy
call u_image_append::destroy
end on

type cb_view from commandbutton within w_payment_inquiry
boolean visible = false
integer x = 1577
integer y = 1736
integer width = 398
integer height = 92
integer taborder = 70
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "View Account"
end type

event clicked;LONG ll_cntr, ll_rowcount, ll_docid
string ls_doc_type
integer li_rtn


ll_cntr = 1
ll_rowcount = dw_docs_for_payment_no.RowCount()
DO WHILE ll_cntr <= ll_rowcount
	ll_docid = dw_docs_for_payment_no.GetItemNumber(ll_cntr,"doc_id")
	
					
		if uo_image_append.of_init(ll_docid)	<= 0 then
			RETURN
		end if
			
			
		ls_doc_type =  uo_image_append.of_get_file_type()
			
		
		CHOOSE CASE ls_doc_type
			/*  Imaged document */ 
			CASE 'IMA', 'TIF'
				li_rtn = uo_image_append.of_append_image(ll_docid)
				if li_rtn < 0 then
					RETURN
				end if
			case else
				iu_dw_document_path.f_manage_document(ll_docid,"V","NORMAL")
		end choose

	
	
	
	ll_cntr ++
LOOP
end event

type cb_prev_transaction from commandbutton within w_payment_inquiry
boolean visible = false
integer x = 2112
integer y = 1644
integer width = 251
integer height = 64
integer taborder = 80
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "<< Prev"
end type

event clicked;LONG   ll_tranrow

// Check to see if we are already on the first row
ll_tranrow=dw_display_transaction_details.GetRow()
IF ll_tranrow = 1 THEN
	Return
END IF
dw_display_transaction_details.ScrollToRow(ll_tranrow - 1)

//IF dw_display_transaction_details.GetItemString(ll_tranrow - 1,"canceled_txn_flag") = "Y" THEN
//	dw_display_transaction_details.SetItem(ll_tranrow - 1,"cancelled_txn_note","CANCELED")
//ELSE
//	dw_display_transaction_details.SetItem(ll_tranrow - 1,"cancelled_txn_note","")
//END IF

// Need to default address for unapplied with default_address = 'Y'
IF dw_display_transaction_details.GetItemString(ll_tranrow - 1,"use_default_address_flag") = "Y" AND &
	IsNull(dw_payment_details_display_only.GetItemDateTime(1,"processed_date")) THEN
	wf_setup_address(dw_display_transaction_details.GetItemString(ll_tranrow - 1,"recipient_type_code"),dw_display_transaction_details.GetItemNumber(ll_tranrow - 1,"recipient_no"))
END IF
end event

type cb_next_transaction from commandbutton within w_payment_inquiry
boolean visible = false
integer x = 2373
integer y = 1644
integer width = 251
integer height = 64
integer taborder = 90
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Next >>"
end type

event clicked;LONG		ll_tranrow

// Check to see if we are already on the last row
ll_tranrow = dw_display_transaction_details.GetRow()
IF ll_tranrow = dw_display_transaction_details.RowCount() THEN
	Return
END IF

dw_display_transaction_details.ScrollToRow(ll_tranrow + 1)

//IF dw_display_transaction_details.GetItemString(ll_tranrow + 1,"canceled_txn_flag") = "Y" THEN
//	dw_display_transaction_details.SetItem(ll_tranrow + 1,"cancelled_txn_note","CANCELED")
//ELSE
//	dw_display_transaction_details.SetItem(ll_tranrow + 1,"cancelled_txn_note","")
//END IF

// Need to default address for unapplied with default_address = 'Y'
IF dw_display_transaction_details.GetItemString(ll_tranrow + 1,"use_default_address_flag") = "Y" AND &
	IsNull(dw_payment_details_display_only.GetItemDateTime(1,"processed_date")) THEN
	wf_setup_address(dw_display_transaction_details.GetItemString(ll_tranrow + 1,"recipient_type_code"),dw_display_transaction_details.GetItemNumber(ll_tranrow + 1,"recipient_no"))
END IF
end event

type cb_legend from commandbutton within w_payment_inquiry
integer x = 777
integer y = 1732
integer width = 325
integer height = 100
integer taborder = 60
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Legend..."
end type

event clicked;open(w_payment_inquiry_legend)
end event

type dw_list_payments from u_dw_online within w_payment_inquiry
event ue_manualtxn ( )
event ue_show_transfer_details ( string as_detail_type )
integer x = 14
integer y = 96
integer width = 2647
integer height = 1608
integer taborder = 100
string dataobject = "d_payment_list"
boolean hscrollbar = true
boolean vscrollbar = true
boolean hsplitscroll = true
borderstyle borderstyle = stylelowered!
end type

event ue_manualtxn();//This function will report to the user whether the selected transaction
//is blocked by a manual transaction

LONG				ll_txn_no
LONG				ll_row
INTEGER			li_rtn
DECIMAL{2}		ldec_sum_payment_type_amount
DECIMAL{2} 		ldec_sum_cost_alloc_amount
n_claim_cost_maintenance	lnv_claim_cost_maintenance
STRING			ls_message


ll_row = this.GetRow()


IF ll_row <= 0 THEN
	MessageBox("Error","A payment must be selected", Exclamation!)
	Return 
END IF

ll_txn_no = GetItemNumber(ll_row,"txn_no")

lnv_claim_cost_maintenance = CREATE n_claim_cost_maintenance
li_rtn = lnv_claim_cost_maintenance.of_check_for_over_adjustments(ll_txn_no,ldec_sum_payment_type_amount,ldec_sum_cost_alloc_amount,ls_message)
	
MessageBox("Result",ls_message)

end event

event ue_show_transfer_details(string as_detail_type);/*

Any changes made to this script should also be made in 
w_claim_cost_maintenance.dw_transaction_list.ue_show_transfer_details and
w_claim_cost_maintenance_inquiry.dw_txn_list.ue_show_transfer_details

*/

LONG			ll_selected_txn_no
LONG			ll_selected_payment_no
LONG			ll_payment_no, ll_max_payment_no
LONG			ll_claim_no
LONG			ll_max_applied_payment_no, ll_max_applied_txn_no
LONG			ll_max_unapplied_payment_no , ll_max_unapplied_txn_no
LONG			ll_cost_alloc_no , ll_cost_alloc_operation_no
STRING		ls_message
STRING		ls_txn_amount_desc
STRING		ls_txn_sub_type_code
STRING		ls_payment_type_code
STRING		ls_payment_sub_type_code
DECIMAL{2}	ldec_txn_amount
DECIMAL{2}  ldec_selected_txn_amount
LONG			ll_rows, ll_rows_unapplied
u_ds			lds_max_payment_applied, lds_max_payment_unapplied
u_ds			lds_transfer_details_applied, lds_transfer_details_unapplied

lds_transfer_details_applied	= CREATE u_ds
lds_transfer_details_unapplied	= CREATE u_ds
lds_max_payment_applied		= CREATE u_ds
lds_max_payment_unapplied	= CREATE u_ds

ll_selected_txn_no = GetItemNumber(GetRow(),'txn_no')
ll_selected_payment_no = GetItemNumber(GetRow(),'payment_no')
ldec_selected_txn_amount = GetItemDecimal(GetRow(),'txn_amount')

CHOOSE CASE as_detail_type
	CASE 'TO'
		//The user wants to know where this transaction was transfered "to"
		//Get the maximum payment_no that is related to the selected transaction 
		
		lds_max_payment_applied.DataObject = 'd_xfer_to_applied_max_payment'
		lds_max_payment_applied.SetTransObject(SQLCA)
		ll_rows = lds_max_payment_applied.Retrieve(ll_selected_txn_no)
		IF ll_rows < 0 Then
			SignalError(-666,'Error retrieving applied transfer details.')
		End if
		SQLCA.nf_handle_error('w_payment_inquiry','ue_show_transfer_details','retrieve')
		
		lds_max_payment_unapplied.DataObject = 'd_xfer_to_unapplied_max_payment'
		lds_max_payment_unapplied.SetTransObject(SQLCA)
		ll_rows_unapplied = lds_max_payment_unapplied.Retrieve(ll_selected_txn_no)
		IF ll_rows_unapplied < 0 Then
			SignalError(-666,'Error retrieving unapplied transfer details.')
		End if
		SQLCA.nf_handle_error('w_payment_inquiry','ue_show_transfer_details','retrieve')
				
		IF ll_rows = 0 AND ll_rows_unapplied = 0 Then
			MessageBox('Not transferred','This transaction has not been transferred.')
			
		ELSEIF ll_rows <> 0 AND ll_rows_unapplied = 0 THEN // only applied txns transferred
			ll_max_applied_payment_no = lds_max_payment_applied.GetItemNumber(1,'max_transfer_payment_no')
			
			//Gather the information and create the message string to pass into the message window.
			
			// the txn sub type will determine the data passed to the messagebox
			SELECT	txn_sub_type_code , Max(txn_no)
			INTO		:ls_txn_sub_type_code , :ll_max_applied_txn_no
			FROM		APPLIED_CLAIM_TXN
			WHERE	payment_no	= :ll_max_applied_payment_no
			AND		txn_type_code = 'T'
			GROUP BY txn_sub_type_code
			USING	SQLCA;
			SQLCA.nf_handle_error('w_payment_inquiry','ue_show_transfer_details','select max txn from APPLIED_CLAIM_TXN')
			
			CHOOSE CASE ls_txn_sub_type_code
				CASE '7','8'
					SELECT	b.cost_alloc_no , b.cost_alloc_operation_no , a.txn_amount
					INTO		:ll_cost_alloc_no , :ll_cost_alloc_operation_no, :ldec_txn_amount
					FROM		APPLIED_CLAIM_TXN					a,
								COST_OF_CLAIMS_ALLOCATED	b
					WHERE	a.txn_no = b.txn_no
					AND		a.txn_no	= :ll_max_applied_txn_no
					USING	SQLCA;
					SQLCA.nf_handle_error('w_payment_inquiry','ue_show_transfer_details','select data for max txn from APPLIED_CLAIM_TXN')
					
					ls_message = ' (' + String(ldec_txn_amount,'$#,##0.00') + ') of the selected transaction was transferred to '&
						+ 'cost alloc ' + String(ll_cost_alloc_no) +'/'+ String(ll_cost_alloc_operation_no) + '.  See payment #' + String(ll_max_applied_payment_no) + '.'
						
				CASE '6'
					SELECT	claim_no , txn_amount
					INTO		:ll_claim_no , :ldec_txn_amount
					FROM		APPLIED_CLAIM_TXN
					WHERE	txn_no	= :ll_max_applied_txn_no
					USING	SQLCA;
					SQLCA.nf_handle_error('w_payment_inquiry','ue_show_transfer_details','select data for max txn from APPLIED_CLAIM_TXN')
					
					ls_message = ' (' + String(ldec_txn_amount,'$#,##0.00') + ') of the selected transaction was transferred to '&
						+ 'claim #' + String(ll_claim_no) + '.  See payment #' + String(ll_max_applied_payment_no) + '.'
						
				CASE '9'
					SELECT	b.payment_type_code , b.payment_sub_type_code , a.txn_amount
					INTO		:ls_payment_type_code , :ls_payment_sub_type_code , :ldec_txn_amount
					FROM		APPLIED_CLAIM_TXN	a ,
								PAYMENT				b
					WHERE	a.payment_no	= b.payment_no
					AND		a.txn_no			= :ll_max_applied_txn_no
					USING	SQLCA;
					SQLCA.nf_handle_error('w_payment_inquiry','ue_show_transfer_details','select data for max txn from APPLIED_CLAIM_TXN')
					
					IF ls_payment_sub_type_code <> '' THEN
						ls_message = ' (' + String(ldec_txn_amount,'$#,##0.00') + ') of the selected transaction was transferred to '&
							+ 'payment type/sub type ' + ls_payment_type_code +'/'+ ls_payment_sub_type_code  + '.  See payment #' + String(ll_max_applied_payment_no) + '.'
					ELSE
						ls_message = ' (' + String(ldec_txn_amount,'$#,##0.00') + ') of the selected transaction was transferred to '&
							+ 'payment type ' + ls_payment_type_code + '.  See payment #' + String(ll_max_applied_payment_no) + '.'
					END IF
					
			END CHOOSE
					
			IF ldec_selected_txn_amount = ldec_txn_amount Then
				ls_message = 'The full amount' + ls_message
			Else
				ls_message = 'A portion' + ls_message
			End if
			
			MessageBox('Transfer',ls_message)
			
		ELSEIF ll_rows = 0 AND ll_rows_unapplied <> 0 THEN // only unapplied txns transferred
			ll_max_unapplied_payment_no = lds_max_payment_unapplied.GetItemNumber(1,'max_transfer_payment_no')
			
			//Gather the information and create the message string to pass into the message window.
			
			// the txn sub type will determine the data passed to the messagebox
			SELECT	txn_sub_type_code , Max(txn_no)
			INTO		:ls_txn_sub_type_code , :ll_max_unapplied_txn_no
			FROM		UNAPPLIED_CLAIM_TXN
			WHERE	payment_no	= :ll_max_unapplied_payment_no
			AND		txn_type_code = 'T'
			GROUP BY txn_sub_type_code
			USING	SQLCA;
			SQLCA.nf_handle_error('w_payment_inquiry','ue_show_transfer_details','select max txn from UNAPPLIED_CLAIM_TXN')
			
			CHOOSE CASE ls_txn_sub_type_code
				CASE '7','8'
					SELECT	b.cost_alloc_no , b.cost_alloc_operation_no , a.txn_amount
					INTO		:ll_cost_alloc_no , :ll_cost_alloc_operation_no, :ldec_txn_amount
					FROM		UNAPPLIED_CLAIM_TXN				a,
								COST_OF_CLAIMS_ALLOCATED	b
					WHERE	a.txn_no = b.txn_no
					AND		a.txn_no	= :ll_max_unapplied_txn_no
					USING	SQLCA;
					SQLCA.nf_handle_error('w_payment_inquiry','ue_show_transfer_details','select data for max txn from UNAPPLIED_CLAIM_TXN')
					
					ls_message = ' (' + String(ldec_txn_amount,'$#,##0.00') + ') of the selected transaction is scheduled to be transferred to '&
						+ 'cost alloc ' + String(ll_cost_alloc_no) +'/'+ String(ll_cost_alloc_operation_no) + '.  See payment #' + String(ll_max_unapplied_payment_no) + '.'
						
				CASE '6'
					SELECT	claim_no , txn_amount
					INTO		:ll_claim_no , :ldec_txn_amount
					FROM		UNAPPLIED_CLAIM_TXN
					WHERE	txn_no	= :ll_max_unapplied_txn_no
					USING	SQLCA;
					SQLCA.nf_handle_error('w_payment_inquiry','ue_show_transfer_details','select data for max txn from UNAPPLIED_CLAIM_TXN')
					
					ls_message = ' (' + String(ldec_txn_amount,'$#,##0.00') + ') of the selected transaction is scheduled to be transferred to '&
						+ 'claim #' + String(ll_claim_no) + '.  See payment #' + String(ll_max_unapplied_payment_no) + '.'
						
				CASE '9'
					SELECT	b.payment_type_code , b.payment_sub_type_code , a.txn_amount
					INTO		:ls_payment_type_code , :ls_payment_sub_type_code , :ldec_txn_amount
					FROM		UNAPPLIED_CLAIM_TXN	a ,
								PAYMENT					b
					WHERE	a.payment_no	= b.payment_no
					AND		a.txn_no			= :ll_max_unapplied_txn_no
					USING	SQLCA;
					SQLCA.nf_handle_error('w_payment_inquiry','ue_show_transfer_details','select data for max txn from UNAPPLIED_CLAIM_TXN')
					
					IF ls_payment_sub_type_code <> '' THEN
						ls_message = ' (' + String(ldec_txn_amount,'$#,##0.00') + ') of the selected transaction is scheduled to be transferred to '&
							+ 'payment type/sub type ' + ls_payment_type_code +'/'+ ls_payment_sub_type_code  + '.  See payment #' + String(ll_max_unapplied_payment_no) + '.'
					ELSE
						ls_message = ' (' + String(ldec_txn_amount,'$#,##0.00') + ') of the selected transaction was transferred to '&
							+ 'payment type ' + ls_payment_type_code + '.  See payment #' + String(ll_max_unapplied_payment_no) + '.'
					END IF
					
			END CHOOSE
			IF ldec_selected_txn_amount = ldec_txn_amount Then
				ls_message = 'The full amount' + ls_message
			Else
				ls_message = 'A portion' + ls_message
			End if
			
			MessageBox('Transfer', ls_message)
			
		End if
		
	CASE 'FROM'
		//The user wants to know where this transactions was transfered "from"
		lds_transfer_details_applied.DataObject = 'd_transfer_from'
		lds_transfer_details_applied.SetTransObject(SQLCA)
		ll_rows = lds_transfer_details_applied.Retrieve(ll_selected_txn_no)
		IF ll_rows < 0 Then
			SignalError(-666,'Error retrieving transfer details.')
		End if
		SQLCA.nf_handle_error('w_payment_inquiry','ue_show_transfer_details','retrieve')
		
		lds_transfer_details_unapplied.DataObject = 'd_transfer_from_unapplied'
		lds_transfer_details_unapplied.SetTransObject(SQLCA)
		ll_rows_unapplied = lds_transfer_details_unapplied.Retrieve(ll_selected_txn_no)
		IF ll_rows_unapplied < 0 Then
			SignalError(-666,'Error retrieving unapplied transfer details.')
		End if
		SQLCA.nf_handle_error('w_payment_inquiry','ue_show_transfer_details','retrieve')
		
		IF ll_rows = 0 and ll_rows_unapplied = 0 Then
			MessageBox('Not transferred','This transaction has not been transferred.')
			
		ELSEIF ll_rows <> 0 AND ll_rows_unapplied = 0 THEN // only applied txns transferred
			//Gather the information and create the message string to pass into 
			//the message window.
			ll_payment_no  			= lds_transfer_details_applied.GetItemNumber(1,'payment_no')
			ll_max_applied_txn_no	= lds_transfer_details_applied.GetItemNumber(1,'max_transfer_txn_no')
			ls_txn_sub_type_code	= lds_transfer_details_applied.GetItemString(1,'txn_sub_type_code')

			CHOOSE CASE ls_txn_sub_type_code
				CASE '7','8'
					ll_cost_alloc_no = lds_transfer_details_applied.GetItemNumber(1,'cost_alloc_no')
					ll_cost_alloc_operation_no = lds_transfer_details_applied.GetItemNumber(1,'cost_alloc_operation_no')
					ls_message =  'The selected transaction was transferred from ' &
						+ 'cost alloc ' + String(ll_cost_alloc_no) +'/'+ String(ll_cost_alloc_operation_no) + '.  See payment #' + String(ll_payment_no) + '.'
						
				CASE '6'
					ll_claim_no = lds_transfer_details_applied.GetItemNumber(1,'claim_no')
					ls_message = 'The selected transaction was transferred from '&
						+ 'claim #' + String(ll_claim_no) + '.  See payment #' + String(ll_payment_no) + '.'
					
				CASE '9'
					ls_payment_type_code = lds_transfer_details_applied.GetItemString(1,'payment_type_code')
					ls_payment_sub_type_code = lds_transfer_details_applied.GetItemString(1,'payment_sub_type_code')
					IF ls_payment_sub_type_code <> '' THEN
						ls_message = 'The selected transaction was transferred from '&
							+ 'payment type/sub type ' + ls_payment_type_code +'/'+ ls_payment_sub_type_code  + '.  See payment #' + String(ll_payment_no) + '.'
					ELSE
						ls_message = 'The selected transaction was transferred from '&
						+ 'payment type ' + ls_payment_type_code + '.  See payment #' + String(ll_payment_no) + '.'
					END IF
					
			END CHOOSE
					
			IF ll_max_applied_txn_no = ll_selected_txn_no THEN
				MessageBox('Transfer',ls_message)
			ELSE
				MessageBox('Not transferred','This transaction has not been transferred.')
			END IF
			
		ELSEIF ll_rows = 0 AND ll_rows_unapplied <> 0 THEN // only unapplied txns transferred
			
			//Gather the information and create the message string to pass into 
			//the message window.
			ll_payment_no  				= lds_transfer_details_unapplied.GetItemNumber(1,'payment_no')
			ll_max_unapplied_txn_no	= lds_transfer_details_unapplied.GetItemNumber(1,'max_transfer_txn_no')
			ls_txn_sub_type_code		= lds_transfer_details_unapplied.GetItemString(1,'txn_sub_type_code')
			
			CHOOSE CASE ls_txn_sub_type_code
				CASE '7','8'
					ll_cost_alloc_no = lds_transfer_details_unapplied.GetItemNumber(1,'cost_alloc_no')
					ll_cost_alloc_operation_no = lds_transfer_details_unapplied.GetItemNumber(1,'cost_alloc_operation_no')
					ls_message =  'The selected transaction is scheduled to be transferred from ' &
						+ 'cost alloc ' + String(ll_cost_alloc_no) +'/'+ String(ll_cost_alloc_operation_no) + '.  See payment #' + String(ll_payment_no) + '.'
						
				CASE '6'
					ll_claim_no = lds_transfer_details_unapplied.GetItemNumber(1,'claim_no')
					ls_message = 'The selected transaction was transferred from '&
						+ 'claim #' + String(ll_claim_no) + '.  See payment #' + String(ll_payment_no) + '.'
					
				CASE '9'
					ls_payment_type_code = lds_transfer_details_unapplied.GetItemString(1,'payment_type_code')
					ls_payment_sub_type_code = lds_transfer_details_unapplied.GetItemString(1,'payment_sub_type_code')
					IF ls_payment_sub_type_code <> '' THEN
						ls_message = 'The selected transaction is scheduled to be transferred from '&
							+ 'payment type/sub type ' + ls_payment_type_code +'/'+ ls_payment_sub_type_code  + '.  See payment #' + String(ll_payment_no) + '.'
					ELSE
						ls_message = 'The selected transaction is scheduled to be transferred from '&
						+ 'payment type ' + ls_payment_type_code + '.  See payment #' + String(ll_payment_no) + '.'
					END IF
					
			END CHOOSE
			
			IF ll_max_unapplied_txn_no	= ll_selected_txn_no THEN
				MessageBox('Transfer',ls_message)
			ELSE
				MessageBox('Not transferred','This transaction has not been transferred.')
			END IF
			
		End if
END CHOOSE
end event

event ue_filter;call super::ue_filter;STRING 	ls_filter
LONG 		ll_payment_no, ll_last_payment_no, ll_nmbr_rows, ll_rownum
DECIMAL 	ldec_days_lost, ldec_total_days_lost

	Open(w_filter_transaction_list)

//	Apply the filter that was selected and recalculate total days
	ls_filter = Message.StringParm
	IF ls_filter = "Cancel" THEN
		Return
	END IF	
	
	This.object.st_filter.text = this.inv_filter.of_SetFilter(ls_filter)

	
	dw_list_payments.GroupCalc()
	IF dw_list_payments.RowCount() > 0 THEN
		dw_list_payments.SelectRow(0,FALSE)
		dw_list_payments.SelectRow(1,True)
	END IF



end event

event rowfocuschanged;call super::rowfocuschanged;LONG ll_row

	ll_row = GetRow()
	IF ll_row > 0 THEN
		uf_processselect(ll_row,"Mouse")
	END IF

end event

event rbuttondown;
/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/

	If not isvalid(im_popup) Then

		im_popup = Create m_dw_online_rmb_popup
		im_popup.mf_set_datawindow(This)
		im_popup.m_options.m_filterlist.visible = TRUE	
		im_popup.m_options.m_sort.visible = TRUE
		im_popup.m_options.m_manualtxn.visible = TRUE
		im_popup.m_options.m_transferdetails.visible = TRUE
		im_popup.m_options.m_tooltips.visible = TRUE	
	End if

	im_popup.m_options.PopMenu(w_frame.PointerX( ), w_frame.PointerY( ))

end event

event retrievestart;call super::retrievestart;RETURN 2
end event

on doubleclicked;call u_dw_online::doubleclicked;cb_switch.TriggerEvent(Clicked!)
end on

event clicked;call super::clicked;
setrow(row)


end event

event retrieveend;call super::retrieveend;this.groupcalc()
end event

event buttonclicked;call super::buttonclicked;string		ls_filter
LONG			ll_begin_rowcount
LONG			ll_end_rowcount

ll_begin_rowcount = this.rowcount()

ll_end_rowcount = this.RowCount()

IF ll_end_rowcount <> ll_begin_rowcount THen
	this.SelectRow(0,False)
	this.SelectRow(1,True)
End if
end event

event itemfocuschanged;call super::itemfocuschanged;STRING		ls_microhelp


If this.RowCount() > 0 Then
	CHOOSE CASE dwo.name
		CASE "payment_type_code"
			ls_microhelp = this.GetItemString(row,"payment_type_desc")
		CASE "payment_sub_type_code"
			ls_microhelp = GetItemString(row,"payment_sub_type_desc")
		CASE "txn_type_code"
			ls_microhelp = GetItemString(row,"txn_type_desc")
		CASE "txn_sub_type_code"
			ls_microhelp = GetItemString(row,"txn_sub_type_desc")
		CASE ELSE
			ls_microhelp = ''
	END CHOOSE
	w_frame.setmicrohelp(ls_microhelp)
END IF
end event

event ue_print;string ls_rtn

PARENT.SetRedraw(FALSE)

ls_rtn = THIS.Modify("DataWindow.Zoom='85'")

THIS.Print()

ls_rtn = THIS.Modify("DataWindow.Zoom='100'")

PARENT.SetRedraw(TRUE)
end event

type dw_display_transaction_details from u_dw_online within w_payment_inquiry
boolean visible = false
integer y = 896
integer width = 2729
integer height = 844
integer taborder = 60
string dataobject = "d_display_transaction_details"
boolean border = false
end type

event rowfocuschanged;call super::rowfocuschanged;LONG		ll_result , ll_recipient_no
STRING	ls_source_table, ls_bank_no, ls_bank_transit_no, ls_bank_account_no , ls_payment_method_code, ls_recipient_type_code

IF currentrow > 0 THEN
	ll_recipient_no = THIS.GetItemNumber(currentrow, 'recipient_no' )
	ls_payment_method_code = THIS.GetItemString(currentrow, 'payment_method_code' )
	ls_recipient_type_code = THIS.GetItemString(currentrow,'recipient_type_code')
	
	IF NOT IsNull(ll_recipient_no) and ls_payment_method_code = 'D' THEN
			
		ls_source_table = THIS.GetItemString(currentrow, 'applied_flag' )
		
		IF IsNull(ls_source_table) THEN
			ls_source_table = 'U'
		END IF
		
		IF ls_source_table = 'U' THEN
	
			IF ls_recipient_type_code = 'I' THEN
				SELECT bank_no, bank_transit_no, bank_account_no
				 INTO      :ls_bank_no, :ls_bank_transit_no, :ls_bank_account_no
				FROM     INDIVIDUAL
				WHERE individual_no = :ll_recipient_no
				USING   SQLCA;
			ELSE
				SELECT bank_no, bank_transit_no, bank_account_no
				INTO       :ls_bank_no, :ls_bank_transit_no, :ls_bank_account_no
				FROM     BANK_INFO a 
				RIGHT OUTER JOIN PROVIDER b ON a.recipient_no = b.provider_no 
														           AND a.recipient_type_code = b.provider_type_code
				WHERE  b.provider_no = :ll_recipient_no
				AND         b.provider_type_code = :ls_recipient_type_code
				USING     SQLCA;
			END IF
		
			ll_result = SQLCA.nf_handle_error('SELECT bank_no, bank_transit_no, bank_account_no','dw_display_transaction_details','rowfocuschanged')
			IF ll_result  < 0 THEN Return -1
		
			IF ll_result = 100 THEN Return -1
					
			IF IsNull(ls_bank_no) THEN ls_bank_no = ''
			IF IsNull(ls_bank_transit_no) THEN ls_bank_transit_no = ''
			IF IsNull(ls_bank_account_no) THEN ls_bank_account_no = ''
		
			THIS.SetItem(currentrow,'bank_no',ls_bank_no)
			THIS.SetItem(currentrow,'bank_transit_no', ls_bank_transit_no)
			THIS.SetItem(currentrow,'bank_account_no', ls_bank_account_no)
		
		ELSE
			RETURN -1
		END IF
	
	END IF
END IF

RETURN 0
			

			

end event

type dw_payment_details_display_only from u_dw_online within w_payment_inquiry
boolean visible = false
integer y = 92
integer width = 2729
integer height = 836
integer taborder = 110
string dataobject = "d_payment_details_display_only"
boolean border = false
end type

type cb_more from commandbutton within w_payment_inquiry
boolean visible = false
integer x = 2313
integer y = 776
integer width = 288
integer height = 72
integer taborder = 120
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&More..."
end type

on clicked;LONG	ll_payment_no

ll_payment_no = dw_payment_details_display_only.GetItemNumber(1,"payment_no")
OpenWithParm(w_adjustments, ll_payment_no)
end on

type cb_extract from commandbutton within w_payment_inquiry
integer x = 1673
integer y = 1740
integer width = 302
integer height = 92
integer taborder = 80
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Extract"
end type

event clicked;
IF dw_list_payments.RowCount() = 0 Then
	MessageBox('No rows','There are no payments to extract.')
	return
end if

dw_list_payments.Saveas('',Excel!,True)
end event

type uo_filter from u_filter_control within w_payment_inquiry
event destroy ( )
integer x = 32
integer y = 1728
integer taborder = 130
boolean bringtotop = true
end type

on uo_filter.destroy
call u_filter_control::destroy
end on

event ue_filter_changed;call super::ue_filter_changed;dw_list_payments.object.st_filter.text = ls_new_filter
end event

type cb_switch from commandbutton within w_payment_inquiry
integer x = 2043
integer y = 1740
integer width = 302
integer height = 92
integer taborder = 50
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Details"
end type

event clicked;LONG		ll_payment_no,	ll_rownum, ll_txn_no
LONG		ll_txn_rows
LONG		ll_jrnl_rows
LONG		ll_found_row
INTEGER	li_x
datawindowchild ldwc_jrnl_num

// Check the current mode - if the button says Show Details, that means we currently
// see a list but want to see the details of the payment
IF This.text = "&Details" THEN
	ll_rownum = dw_list_payments.GetRow()

	IF ll_rownum <= 0 THEN
		Return
	END IF

	ll_payment_no = dw_list_payments.GetItemNumber(ll_rownum,"payment_no")	

	dw_payment_details_display_only.Retrieve(ll_payment_no)
	
	SQLCA.nf_handle_error("dw_display_payment_details","w_payment_inquiry","clicked for cb_switch")

	IF dw_display_transaction_details.GetChild('journal_control_no', ldwc_jrnl_num) < 0 THEN
		MessageBox('Error', 'Not a datawindow child')
	END IF
	ldwc_jrnl_num.SetTransObject(SQLCA)
	ldwc_jrnl_num.InsertRow(0)

	ll_txn_rows = dw_display_transaction_details.Retrieve(ll_payment_no)
	IF SQLCA.nf_handle_error("dw_display_transaction_details","w_payment_inquiry","clicked for cb_switch") < 0 THEN
		Close(parent)
		Return
	END IF
	
	ldwc_jrnl_num.SetFilter('gl_journal_control_no <> ""')
	ldwc_jrnl_num.Filter()
	
	//Loop through the transaction details and set the journal control numbers
	//for revenue and expense entries
	
	For li_x = 1 To ll_txn_rows		
	
		ll_txn_no = dw_display_transaction_details.GetItemNumber(li_x, "txn_no")
		ll_jrnl_rows = ldwc_jrnl_num.Retrieve(ll_txn_no)			
		
		//Set the gl_journal_control numbers if some exist
		If ll_jrnl_rows > 0 Then
			
			ll_found_row = ldwc_jrnl_num.Find("gl_txn_type_code = 'E'",1,2)
			IF ll_found_row <= 0 Then SignalError(-666,'Error finding expense entry')
			dw_display_transaction_details.SetItem(li_x, 'journal_control_no', ldwc_jrnl_num.GetItemString(ll_found_row, 'gl_journal_control_no'))
			
			ll_found_row = ldwc_jrnl_num.Find("gl_txn_type_code = 'R'",1,2)
			IF ll_found_row > 0 Then
				dw_display_transaction_details.SetItem(li_x, 'revenue_journal_control_no', ldwc_jrnl_num.GetItemString(ll_found_row, 'gl_journal_control_no'))
			End if
		End if
	Next
	
// Need to default address for unapplied with default_address = 'Y'
	IF dw_display_transaction_details.GetItemString(1,"use_default_address_flag") = "Y" AND &
		IsNull(dw_payment_details_display_only.GetItemDateTime(1,"processed_date")) THEN
		wf_setup_address(dw_display_transaction_details.GetItemString(1,"recipient_type_code"),dw_display_transaction_details.GetItemNumber(1,"recipient_no"))
	END IF

	IF dw_display_transaction_details.GetItemString(1,"use_default_address_flag") = "N" AND &
		IsNull(dw_payment_details_display_only.GetItemDateTime(1,"processed_date")) THEN
		wf_get_recipient_name(dw_display_transaction_details.GetItemString(1,"recipient_type_code"), dw_display_transaction_details.GetItemNumber(1,"recipient_no"))		//pr2458
	END IF

	dw_payment_details_display_only.SetItem(1,"order_date",Date(dw_display_transaction_details.GetItemDateTime(1,"scheduled_processing_date")))

	dw_list_payments.Visible = False
	dw_payment_details_display_only.Visible = True
	dw_display_transaction_details.Visible = True
	cb_next_transaction.Visible = True
	cb_prev_transaction.Visible = True
	cb_more.Visible = True

	IF dw_payment_details_display_only.GetItemString(1,"authorization_type_code") = "act" THEN
		dw_docs_for_payment_no.Retrieve(ll_payment_no)
		IF SQLCA.nf_handle_error("dw_docs_for_payment_no","w_payment_inquiry","clicked for cb_switch") < 0 THEN
			Close(parent)
			Return
		END IF

		IF dw_docs_for_payment_no.RowCount() > 0 THEN
			cb_view.Visible = True
		ELSE
			cb_view.Visible = False
		END IF
	ELSE
		cb_view.Visible = False
	END IF
	uo_filter.visible = False
	
	cb_legend.visible = False
	cb_switch.text = "&List"


// IF the button says Show List, that means we currently see the details but want to
// go back to the list
ELSE
	dw_payment_details_display_only.Visible = False
	dw_display_transaction_details.Visible = False
	dw_payment_details_display_only.Reset()
	dw_display_transaction_details.Reset()
	dw_list_payments.Visible = True
	cb_next_transaction.Visible = False
	cb_prev_transaction.Visible = False
	cb_more.Visible = False
	cb_view.Visible = False
	uo_filter.visible = True
	cb_legend.visible = True
	cb_switch.text = "&Details"
END IF

end event

