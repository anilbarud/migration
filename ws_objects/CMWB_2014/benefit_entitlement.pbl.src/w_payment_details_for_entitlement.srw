$PBExportHeader$w_payment_details_for_entitlement.srw
$PBExportComments$used as resonse to show user payment details on selected payment
forward
global type w_payment_details_for_entitlement from window
end type
type cb_more from commandbutton within w_payment_details_for_entitlement
end type
type cb_prev_transaction from commandbutton within w_payment_details_for_entitlement
end type
type cb_next_transaction from commandbutton within w_payment_details_for_entitlement
end type
type dw_display_transaction_details from u_dw_online within w_payment_details_for_entitlement
end type
type dw_payment_details_display_only from u_dw_online within w_payment_details_for_entitlement
end type
type cb_ok from commandbutton within w_payment_details_for_entitlement
end type
end forward

global type w_payment_details_for_entitlement from window
integer x = 1335
integer y = 688
integer width = 2702
integer height = 1888
boolean titlebar = true
string title = "Payment Details"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 67108864
boolean center = true
cb_more cb_more
cb_prev_transaction cb_prev_transaction
cb_next_transaction cb_next_transaction
dw_display_transaction_details dw_display_transaction_details
dw_payment_details_display_only dw_payment_details_display_only
cb_ok cb_ok
end type
global w_payment_details_for_entitlement w_payment_details_for_entitlement

type variables

end variables

forward prototypes
public function integer wf_get_recipient_name (string as_recipient_type_code, long al_recipient_no)
public function integer wf_setup_address (string as_recipient_type_code, long al_recipient_no, long al_claim_no)
end prototypes

public function integer wf_get_recipient_name (string as_recipient_type_code, long al_recipient_no);STRING	ls_recipient_name

IF as_recipient_type_code = 'I' THEN

	SELECT	given_names + ' ' + last_name	
	  INTO	:ls_recipient_name	
	  FROM	INDIVIDUAL
	 WHERE	( individual_no = :al_recipient_no )
	 USING SQLCA ;
			
	SQLCA.nf_handle_error("w_payment_details_for_entitlement","wf_get_recipient_name()","SELECT given_names + ' ' + last_name")
	
ELSE

	SELECT	name	
	INTO		:ls_recipient_name	
	FROM		PROVIDER  
	WHERE	provider_no 			= :al_recipient_no  
	AND      	provider_type_code 	= :as_recipient_type_code 
	AND       active_flag 				= 'Y' 
	USING 	SQLCA ;
	
	SQLCA.nf_handle_error("w_payment_details_for_entitlement","wf_get_recipient_name()","SELECT name")
	
END IF

dw_display_transaction_details.SetItem(dw_display_transaction_details.GetRow(),"recipient_name",ls_recipient_name)

RETURN 1
end function

public function integer wf_setup_address (string as_recipient_type_code, long al_recipient_no, long al_claim_no);LONG             ll_result, ll_tranrow
STRING           ls_recipient_care_of, ls_recipient_street, ls_recipient_city, ls_recipient_province
STRING           ls_recipient_country, ls_recipient_postal_code, ls_recipient_name
DATAWINDOWCHILD  ldwc_child
DATASTORE	  lds_basic_claim

/* Function Name: wf_setup_address                                                                      

	Purpose:       The purpose of this function is to load the current transaction record with default   
		            address information                                                                   

	Arguments:     as_recipient_type_code - identifies the recipient type so we know where to go to get 
						the address                                                 
						al_recipient_no - if the recipient is not the claimant or employer, this contains    
						the payee number                                                   

	Return Values: n/a                                                                                   
*/

IF isnull(al_claim_no) OR al_claim_no = 0 THEN RETURN -1

//setup the dtatstore
lds_basic_claim = create datastore

lds_basic_claim.dataobject = 'd_basic_claim'
lds_basic_claim.settransobject(sqlca)
lds_basic_claim.retrieve(al_claim_no)

SQLCA.nf_handle_error("w_payment_details_for_entitlement","wf_setup_address()","lds_basic_claim.retrieve(al_claim_no)")

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
					
				/*need to get the description */
  			   	lds_basic_claim.GetChild('country_code', ldwc_child)
   	   			ll_result = ldwc_child.Find('location_code = "' + ls_recipient_country + '"', 1, ldwc_child.RowCount())
		      	
				IF ll_result > 0 THEN
   	      			ls_recipient_country = ldwc_child.GetItemString(ll_result, 'location_desc1')
      			END IF
			   
				dw_display_transaction_details.SetItem(ll_tranrow,"country",ls_recipient_country)
   		   		dw_display_transaction_details.SetItem(ll_tranrow,"postal_code",lds_basic_claim.GetItemString(1,'postal_code'))
			ELSE
				
				SELECT 	given_names + ' ' + last_name, address_line1, address_line2,   
		             			city, prov_state_code, country_code, postal_code  
	   		  	INTO 		:ls_recipient_name, :ls_recipient_care_of, :ls_recipient_street, :ls_recipient_city, 
   	   	       				:ls_recipient_province, :ls_recipient_country, :ls_recipient_postal_code  
			     FROM 	INDIVIDUAL
			     WHERE 	individual_no = :al_recipient_no  
	   	    		USING SQLCA ;
			
				IF SQLCA.nf_handle_error("W_payment_details_for_entitlement","wf_setup_address()","SELECT 	given_names + ' ' + last_name, address_line1") = 100 THEN RETURN 100
				 
		   		dw_display_transaction_details.SetItem(ll_tranrow,"recipient_name",ls_recipient_name)
			  	ls_recipient_care_of = Left(ls_recipient_care_of,29)
   				dw_display_transaction_details.SetItem(ll_tranrow,"address_line1",ls_recipient_care_of)
			   	dw_display_transaction_details.SetItem(ll_tranrow,"address_line2",ls_recipient_street)
	   			dw_display_transaction_details.SetItem(ll_tranrow,"city",ls_recipient_city)
		   		dw_display_transaction_details.SetItem(ll_tranrow,"prov_state_code",ls_recipient_province)
				
				SELECT 	location_desc1
				INTO 		:ls_recipient_country
				FROM 	Location
				WHERE 	location_code 			= :ls_recipient_country
				AND 		location_type_code 	= 'C';

				IF SQLCA.nf_handle_error("W_payment_details_for_entitlement","wf_setup_address()","SELECT 	location_desc1") = 100 THEN RETURN 100
				
   				dw_display_transaction_details.SetItem(ll_tranrow,"country",ls_recipient_country)
		      	dw_display_transaction_details.SetItem(ll_tranrow,"postal_code",ls_recipient_postal_code)

			END IF

	   CASE ELSE
			
			/*If recipient is a payee, read the address from Service Providers table */
			IF IsNull(as_recipient_type_code) OR IsNull(al_recipient_no) THEN
				MessageBox('Warning','Recipient number or type is missing')
				RETURN -1
			END IF
			
		   	SELECT 	PROVIDER.name, PROVIDER.address_line1, PROVIDER.address_line2,   
	             			PROVIDER.city, PROVIDER.prov_state_code, PROVIDER.country_code, PROVIDER.postal_code  
	   	  	INTO 		:ls_recipient_name, :ls_recipient_care_of, :ls_recipient_street, :ls_recipient_city, 
   	          			:ls_recipient_province, :ls_recipient_country, :ls_recipient_postal_code  
		     FROM 	PROVIDER  
		    	WHERE 	PROVIDER.provider_no 			= :al_recipient_no
			AND  		PROVIDER.provider_type_code = :as_recipient_type_code 
			AND		PROVIDER.active_flag 			= 'Y'   
	       	USING 	SQLCA ;
		
			IF SQLCA.nf_handle_error("W_payment_details_for_entitlement","wf_setup_address()","SELECT PROVIDER.name, PROVIDER.address_line1") = 100 THEN RETURN 100
			
	   		dw_display_transaction_details.SetItem(ll_tranrow,"recipient_name",ls_recipient_name)
			
			IF Trim(ls_recipient_street) = "" THEN
				ls_recipient_street 		= ls_recipient_care_of
				ls_recipient_care_of 		= ''
			END IF
			
   			IF ls_recipient_care_of = "" or ls_recipient_care_of = " " THEN
	   			ls_recipient_care_of = "RE: " + lds_basic_claim.GetItemString(1,'given_names') + ' '+ lds_basic_claim.GetItemString(1,'last_name') 
	   		END IF
				
		   	ls_recipient_care_of = Left(ls_recipient_care_of,29)
   			dw_display_transaction_details.SetItem(ll_tranrow,"address_line1",ls_recipient_care_of)
		   	dw_display_transaction_details.SetItem(ll_tranrow,"address_line2",ls_recipient_street)
	   		dw_display_transaction_details.SetItem(ll_tranrow,"city",ls_recipient_city)
	   		dw_display_transaction_details.SetItem(ll_tranrow,"prov_state_code",ls_recipient_province)
			
			SELECT 	location_desc1
			INTO 		:ls_recipient_country
			FROM 	Location
			WHERE 	location_code 			= :ls_recipient_country
			AND 		location_type_code 	= 'C';

			IF SQLCA.nf_handle_error("W_payment_details_for_entitlement","wf_setup_address()","SELECT location_desc1") = 100 THEN	RETURN 100
				
   			dw_display_transaction_details.SetItem(ll_tranrow,"country",ls_recipient_country)
	      	dw_display_transaction_details.SetItem(ll_tranrow,"postal_code",ls_recipient_postal_code)
				
	END CHOOSE	
	
RETURN 0
end function

event open;INTEGER         			li_x, li_rowcount
LONG						ll_payment_no, ll_txn_rows, ll_txn_no, ll_jrnl_rows, ll_found_row, ll_claim_no
DATAWINDOWCHILD	ldwc_jrnl_num

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


/*
	9) When you double click on a row on the payment tab (or may be also right mouse click) 
	can we display the details like the payment inquiry module does.  This could just be a popup 
	with the same dws as in payment inquiry.  I find that I always have to go to payment Inquiry to 
	find out more details of the transactions.
*/

/* grab the claim_no from the calling menu */
ll_payment_no = Message.DoubleParm	

/* make sure we have a valid claim no */
IF isnull(ll_payment_no) THEN
	//problem - return out
	CLOSE(THIS)
	RETURN 
END IF 

/*	Set up the DW */
dw_payment_details_display_only.SetTransObject(SQLCA)
dw_display_transaction_details.SetTransObject(SQLCA)

/* do the retrieve */
li_rowcount = dw_payment_details_display_only.Retrieve(ll_payment_no)
SQLCA.nf_handle_error("w_payment_details_for_entitlement","open","dw_payment_details_display_only.Retrieve(ll_payment_no)")

IF li_rowcount <= 0 THEN 
	//RETURN OUT
	CLOSE(THIS)
	RETURN 
END IF 

IF dw_display_transaction_details.GetChild('journal_control_no', ldwc_jrnl_num) < 0 THEN
	MessageBox('Error', 'Not a datawindow child')
END IF
ldwc_jrnl_num.SetTransObject(SQLCA)
ldwc_jrnl_num.InsertRow(0)

ll_txn_rows = dw_display_transaction_details.Retrieve(ll_payment_no)
SQLCA.nf_handle_error("w_payment_details_for_entitlement","open"," dw_display_transaction_details.Retrieve(ll_payment_no)") 
	
ldwc_jrnl_num.SetFilter('gl_journal_control_no <> ""')
ldwc_jrnl_num.Filter()

//Loop through the transaction details and set the journal control numbers
//for revenue and expense entries
	
FOR li_x = 1 TO ll_txn_rows		
	
	ll_txn_no = dw_display_transaction_details.GetItemNumber(li_x, "txn_no")
	ll_jrnl_rows = ldwc_jrnl_num.Retrieve(ll_txn_no)	
	SQLCA.nf_handle_error("w_payment_details_for_entitlement","open","ll_jrnl_rows = ldwc_jrnl_num.Retrieve(ll_txn_no)")
		
	//Set the gl_journal_control numbers if some exist
	IF ll_jrnl_rows > 0 THEN
			
		ll_found_row = ldwc_jrnl_num.Find("gl_txn_type_code = 'E'",1,2)
		IF ll_found_row <= 0 THEN SignalError(-666,'Error finding expense entry')
		dw_display_transaction_details.SetItem(li_x, 'journal_control_no', ldwc_jrnl_num.GetItemString(ll_found_row, 'gl_journal_control_no'))
			
		ll_found_row = ldwc_jrnl_num.Find("gl_txn_type_code = 'R'",1,2)
		IF ll_found_row > 0 THEN
			dw_display_transaction_details.SetItem(li_x, 'revenue_journal_control_no', ldwc_jrnl_num.GetItemString(ll_found_row, 'gl_journal_control_no'))
		END IF
	END IF
NEXT

//grab the claim-no from the payment
ll_claim_no = dw_payment_details_display_only.GetItemnumber(1,"claim_no")
IF ISNULL(ll_claim_no) THEN ll_claim_no = 0
	
// Need to default address for unapplied with default_address = 'Y'
IF dw_display_transaction_details.GetItemString(1,"use_default_address_flag") = "Y" AND &
	IsNull(dw_payment_details_display_only.GetItemDateTime(1,"processed_date")) THEN
	wf_setup_address(dw_display_transaction_details.GetItemString(1,"recipient_type_code"),dw_display_transaction_details.GetItemNumber(1,"recipient_no"), ll_claim_no)
END IF

IF dw_display_transaction_details.GetItemString(1,"use_default_address_flag") = "N" AND &
	IsNull(dw_payment_details_display_only.GetItemDateTime(1,"processed_date")) THEN
	wf_get_recipient_name(dw_display_transaction_details.GetItemString(1,"recipient_type_code"), dw_display_transaction_details.GetItemNumber(1,"recipient_no"))		//pr2458
END IF

dw_payment_details_display_only.SetItem(1,"order_date",Date(dw_display_transaction_details.GetItemDateTime(1,"scheduled_processing_date")))

IF dw_payment_details_display_only.GetItemString(1,"authorization_type_code") = "act" THEN
//	dw_docs_for_payment_no.Retrieve(ll_payment_no)
	SQLCA.nf_handle_error("dw_docs_for_payment_no","w_payment_inquiry","clicked for cb_switch") 
END IF

	


end event

on w_payment_details_for_entitlement.create
this.cb_more=create cb_more
this.cb_prev_transaction=create cb_prev_transaction
this.cb_next_transaction=create cb_next_transaction
this.dw_display_transaction_details=create dw_display_transaction_details
this.dw_payment_details_display_only=create dw_payment_details_display_only
this.cb_ok=create cb_ok
this.Control[]={this.cb_more,&
this.cb_prev_transaction,&
this.cb_next_transaction,&
this.dw_display_transaction_details,&
this.dw_payment_details_display_only,&
this.cb_ok}
end on

on w_payment_details_for_entitlement.destroy
destroy(this.cb_more)
destroy(this.cb_prev_transaction)
destroy(this.cb_next_transaction)
destroy(this.dw_display_transaction_details)
destroy(this.dw_payment_details_display_only)
destroy(this.cb_ok)
end on

type cb_more from commandbutton within w_payment_details_for_entitlement
integer x = 1865
integer y = 1652
integer width = 251
integer height = 64
integer taborder = 70
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "More"
end type

event clicked;LONG	ll_payment_no

ll_payment_no = dw_payment_details_display_only.GetItemNumber(1,"payment_no")
OpenWithParm(w_adjustments, ll_payment_no)
end event

type cb_prev_transaction from commandbutton within w_payment_details_for_entitlement
integer x = 2167
integer y = 1652
integer width = 251
integer height = 64
integer taborder = 70
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "<< Prev"
end type

event clicked;LONG   ll_tranrow, ll_claim_no

// Check to see if we are already on the first row
ll_tranrow=dw_display_transaction_details.GetRow()
IF ll_tranrow = 1 THEN RETURN

//grab the claim-no from the payment
ll_claim_no = dw_payment_details_display_only.GetItemnumber(1,"claim_no")
IF ISNULL(ll_claim_no) THEN ll_claim_no = 0

dw_display_transaction_details.ScrollToRow(ll_tranrow - 1)

// Need to default address for unapplied with default_address = 'Y'
IF dw_display_transaction_details.GetItemString(ll_tranrow - 1,"use_default_address_flag") = "Y" AND &
	IsNull(dw_payment_details_display_only.GetItemDateTime(1,"processed_date")) THEN
	wf_setup_address(dw_display_transaction_details.GetItemString(ll_tranrow - 1,"recipient_type_code"),dw_display_transaction_details.GetItemNumber(ll_tranrow - 1,"recipient_no"),ll_claim_no)
END IF
end event

type cb_next_transaction from commandbutton within w_payment_details_for_entitlement
integer x = 2423
integer y = 1652
integer width = 251
integer height = 64
integer taborder = 70
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Next >>"
end type

event clicked;LONG		ll_tranrow, ll_claim_no

// Check to see if we are already on the last row
ll_tranrow = dw_display_transaction_details.GetRow()
IF ll_tranrow = dw_display_transaction_details.RowCount() THEN RETURN

//grab the claim-no from the payment
ll_claim_no = dw_payment_details_display_only.GetItemnumber(1,"claim_no")
IF ISNULL(ll_claim_no) THEN ll_claim_no = 0

dw_display_transaction_details.ScrollToRow(ll_tranrow + 1)

// Need to default address for unapplied with default_address = 'Y'
IF dw_display_transaction_details.GetItemString(ll_tranrow + 1,"use_default_address_flag") = "Y" AND &
	IsNull(dw_payment_details_display_only.GetItemDateTime(1,"processed_date")) THEN
	wf_setup_address(dw_display_transaction_details.GetItemString(ll_tranrow + 1,"recipient_type_code"),dw_display_transaction_details.GetItemNumber(ll_tranrow + 1,"recipient_no"),ll_claim_no)
END IF
end event

type dw_display_transaction_details from u_dw_online within w_payment_details_for_entitlement
integer x = 14
integer y = 800
integer width = 2670
integer height = 864
integer taborder = 20
string dataobject = "d_display_transaction_details"
boolean border = false
end type

type dw_payment_details_display_only from u_dw_online within w_payment_details_for_entitlement
integer x = 5
integer y = 16
integer width = 2738
integer height = 808
integer taborder = 10
string dataobject = "d_payment_details_display_only"
boolean border = false
end type

event constructor;call super::constructor;//THIS.uf_setselect(1)
end event

type cb_ok from commandbutton within w_payment_details_for_entitlement
integer x = 1207
integer y = 1684
integer width = 379
integer height = 96
integer taborder = 60
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
end type

event clicked;
CLOSE(PARENT)


end event

