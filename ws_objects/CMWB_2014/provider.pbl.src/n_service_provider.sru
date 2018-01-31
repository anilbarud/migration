$PBExportHeader$n_service_provider.sru
$PBExportComments$business rules for validating providers
forward
global type n_service_provider from n_pdc
end type
end forward

global type n_service_provider from n_pdc
end type
global n_service_provider n_service_provider

type variables
PUBLIC BOOLEAN ib_blow_entity_members_away = FALSE //2014-10-01 David Worboys

Protected:



n_validate_address		inv_address



end variables

forward prototypes
public function integer nf_check_mandatory ()
public function long nf_get_next_identifier ()
public function long nf_set_identifiers ()
public subroutine nf_init (u_dwa adw_dw[], n_transaction anv_transobj, w_ancestor awi_window_parent)
public function integer nf_retrieve (long al_service_provider_no, string al_provider_type_code)
public function long nf_check_address (string as_string)
public function integer nf_insert (long al_row)
public function integer nf_set_unused_fields ()
public function integer nf_set_defaults ()
public function integer nf_set_provider_sub_type_filter (string as_provider_type_code)
public subroutine nf_check_for_payments (long al_provider_no, string as_provider_type_code, ref long al_processed_count, ref long al_unprocessed_count)
public function integer nf_check_bus_rule ()
public subroutine nf_check_for_awards (long al_recipient_no, string as_recipient_type_code, ref long al_awards)
public function integer nf_change_item (long al_datawindow)
public function integer nf_check_rehab_tasks ()
public function integer nf_log_events ()
public function integer nf_check_provider_ephysio ()
end prototypes

public function integer nf_check_mandatory ();LONG  ll_row

/*
	Accept the data windows
*/
	IF idw_dw[1].AcceptText() < 0 THEN Return -1
	IF idw_dw[2].AcceptText() < 0 THEN Return -1

	ll_row = idw_dw[1].GetRow()
	IF ll_row > 0 THEN
   	IF IsNull(idw_dw[1].GetItemString(ll_row,'name')) OR Trim(idw_dw[1].GetItemString(ll_row,'name')) = '' THEN
      	MessageBox("Missing Name", "The name of the provider must be supplied.  Please correct.")
	      Return -1
   	END IF
		
	   IF IsNull(idw_dw[1].GetItemString(ll_row,'provider_type_code')) OR Trim(idw_dw[1].GetItemString(ll_row,'provider_type_code')) = ''THEN
   	   MessageBox("Missing Provider Type", "The type of the provider must be supplied.  Please correct.")
      	Return -1
	   END IF
		
/*		Now, call the nf_check_mandatory function in n_validate_address for common address validations
*/
		IF inv_address.nf_check_mandatory() < 0 THEN
			Return -1
		END IF
	
	ELSE
   	MessageBox("ERROR", 'Missing all data.  Contact sytem administrator.')
	   Return -1
	END IF

Return 0
end function

public function long nf_get_next_identifier ();LONG  ll_no, ll_row 

	ll_row = idw_dw[1].GetRow()
	IF ll_row > 0 THEN
	   IF idw_dw[1].GetItemStatus(ll_row,0,Primary!) = NewModified! THEN 
   	   ll_no = idw_dw[2].Retrieve()
			IF SQLCA.nf_handle_error('retrieving next provider no', 'nf_get_next_identifier', 'n_service_provider') < 0 THEN
				Return -1
			END IF
      	IF ll_no = 1 THEN
         	ll_no = idw_dw[2].GetItemNumber(1,'provider_no')
	         idw_dw[2].DeleteRow(1)
   	   ELSE
/*			this should only happen if there are no more provider numbers in the Unused_Provider_No table
			this table contains all the gaps in the system prior to this program - July/96
*/
      	  MessageBox('No Provider numbers', 'There are no provider numbers available to choose from.~r~nPlease see system administrator.')
	        Return -1
   	   END IF
	   ELSE
   	   ll_no = idw_dw[1].GetItemNumber(ll_row,'provider_no')
	   END IF
	ELSE
   	Return 0
	END IF
Return ll_no


end function

public function long nf_set_identifiers ();LONG  ll_provider_no, ll_row

	ll_provider_no = nf_get_next_identifier()

	IF ll_provider_no > 0 THEN
/*
	need to check if new row
*/
	   ll_row = idw_dw[1].GetRow()
	   IF ll_row > 0 THEN
   	   idw_dw[1].SetItem(ll_row, 'provider_no', ll_provider_no)
	   ELSE
   	   MessageBox("Error", "Error setting provider number.")
      	Return -1
	   END IF
	ELSE
   	MessageBox("Error","Unable to generate an new provider number.~r~nSee system administrator.")
		Return -1
	END IF


Return ll_provider_no
end function

public subroutine nf_init (u_dwa adw_dw[], n_transaction anv_transobj, w_ancestor awi_window_parent);/*	The following datawindows are passed:
	adw_dw[1] = dw_service_provider
	adw_dw[2] = dw_next_provider_no
*/

	nf_set_datawindow(adw_dw[],anv_transobj)
	nf_set_window_parent(awi_window_parent)

/* Create and initialize all the objects that are used by this object
*/
	inv_address = Create n_validate_address
	inv_address.nf_set_datawindow(adw_dw[1])
end subroutine

public function integer nf_retrieve (long al_service_provider_no, string al_provider_type_code);LONG					ll_rows, ll_child_row, ll_rows2
DATAWINDOWCHILD	ldwc_child
STRING				ls_type_code,ls_sub_type_code, ls_location_code, ls_location_type_code, ls_find_expression

/*
	dw 1 = the main service provider 
	dw 2 = next provider no
	dw 3 = rehab tasks associated with that provider
	dw 4 = rehab_program_xref_provider
*/

/*
	filter out invalid type codes
*/

	ll_rows = idw_dw[1].Retrieve(al_service_provider_no,al_provider_type_code)
	IF SQLCA.nf_handle_error('Retrieve of providers', 'n_service_provider', 'nf_retrieve' ) < 0 THEN
		Return -1
	END IF

	ll_rows2 = idw_dw[3].Retrieve(al_service_provider_no,al_provider_type_code)
	IF SQLCA.nf_handle_error('Retrieve of tasks', 'n_service_provider', 'nf_retrieve' ) < 0 THEN
		Return -1
	END IF
	
	// holds associated rehab programs for a provider of type M/35 - physio clinic
	idw_dw[4].Retrieve(al_service_provider_no)
	SQLCA.nf_handle_error('Retrieve of tasks', 'n_service_provider', 'idw_dw[4].Retrieve(al_service_provider_no)' ) 

	nf_set_provider_sub_type_filter(al_provider_type_code)
	
/* Get the location type code to set the location description field so that the appropriate municipality code
	is displayed.
*/
	ls_location_code = idw_dw[1].GetItemString(ll_rows,'location_code')
	ls_location_type_code = idw_dw[1].GetItemString(ll_rows,'location_type_code')
	IF ls_location_type_code = 'M' THEN
	/*	Get a reference to the city drop down list
	*/
		IF idw_dw[1].GetChild("location_desc2",ldwc_child) < 0 THEN
			MessageBox("Error","Could not reference list of city codes.  Please call the Help Desk.")
			Return -1
		END IF
		ls_find_expression = 'location_code= "' + ls_location_code + '"'
		ll_child_row = ldwc_child.Find(ls_find_expression,1,ldwc_child.RowCount())
		/* April 3, 1998 - check to see if the PROVIDER location code exists in the Location table */
		IF ll_child_row > 0 THEN
			idw_dw[1].SetItem(ll_rows,'location_desc2',ldwc_child.GetItemString(ll_child_row,"location_desc2"))
		ELSE
			idw_dw[1].SetItem(ll_rows,'location_desc2',idw_dw[1].GetItemString(ll_rows,'city'))
		END IF
	ELSE
		idw_dw[1].SetItem(ll_rows,'location_desc2',idw_dw[1].GetItemString(ll_rows,'city'))
	END IF
	
//ls_physio_flag = idw_dw[3].GetItemString(ll_rows,'ephysio_flag')
//	ls_physio_contract_flag = idw_dw[3].GetItemString(ll_rows,'physio_contract_flag')
	

Return ll_rows

end function

public function long nf_check_address (string as_string);STRING		ls_address_line1_3,ls_address_line2_3,ls_address_line1_4,ls_address_line2_4, ls_msg, ls_column
LONG			ll_line
/*  	If users enter RE;, RE:, RE , RE-, or RE -, give them a warning message
*/
		ls_address_line1_3 = TRIM(Left(idw_dw[1].GetItemString(idw_dw[1].GetRow(), 'address_line1'),3))
		ls_address_line1_4 = TRIM(Left(idw_dw[1].GetItemString(idw_dw[1].GetRow(), 'address_line1'),4))
		
		ls_address_line2_3 = TRIM(Left(idw_dw[1].GetItemString(idw_dw[1].GetRow(), 'address_line2'),3))
		ls_address_line2_4 = TRIM(Left(idw_dw[1].GetItemString(idw_dw[1].GetRow(), 'address_line2'),4))
		
		IF ls_address_line1_4 = 'RE ;' OR ls_address_line1_4 = 'RE :' OR ls_address_line1_4 = 'RE -' THEN
			ls_msg    = ls_address_line1_4
			ls_column = "address_line1"
			ll_line 	 = 1
		ELSEIF ls_address_line1_3 = 'RE;' OR ls_address_line1_3 = 'RE:' OR ls_address_line1_3 = 'RE' OR ls_address_line1_3 = 'RE-' THEN
			ls_msg = ls_address_line1_3
			ls_column = "address_line1"
			ll_line 	 = 1
		ELSEIF ls_address_line2_4 = 'RE ;' OR ls_address_line2_4 = 'RE :' OR ls_address_line2_4 = 'RE -' THEN
			ls_msg = ls_address_line2_4
			ls_column = "address_line2"
			ll_line 	 = 2
		ELSEIF ls_address_line2_3 ='RE;' OR ls_address_line2_3 = 'RE:' OR ls_address_line2_3 = 'RE' OR ls_address_line2_3 = 'RE-' THEN
			ls_msg = ls_address_line2_3
			ls_column = "address_line2"
			ll_line 	 = 2
		END IF
		
		IF ls_msg > '' THEN
			IF as_string = 'M' THEN
				IF MessageBox('Warning - Address Information',"You have started address line " + STRING(ll_line) + " with " + ls_msg + ".~r~n" + ls_msg + " 'claimant name' is not valid for Medical Aid providers.~r~n~r~nWould you like to continue?",question!,yesno!) = 2 THEN
					idw_dw[1].SetColumn(ls_column)
					idw_dw[1].SetFocus()
					RETURN -1
				END IF
			ELSE
				IF MessageBox('Warning - Address Information',"You have started address line " + STRING(ll_line) + " with " + ls_msg + ".~r~n" + ls_msg + " 'claimant name' is automatically filled in on the cheque~r~nwhen only one line of the address is entered here.~r~n~r~nWould you like to continue?",question!,yesno!) = 2 THEN
					idw_dw[1].SetColumn(ls_column)
					idw_dw[1].SetFocus()
					RETURN -1
				END IF
			END IF
		END IF

RETURN 0
end function

public function integer nf_insert (long al_row);LONG             ll_row
DATAWINDOWCHILD  ldwc_child

	idw_dw[1].Reset()

/*	Insert a row, and then filter all values out of the sub_type_code drop down
	until a valid type code is selected
*/
	ll_row = idw_dw[1].InsertRow(al_row)
	idw_dw[1].GetChild('provider_sub_type_code', ldwc_child)
	ldwc_child.SetFilter("provider_type_code = ''")
	ldwc_child.Filter()

/*	Setup some defaults for the user
*/
	nf_set_defaults()

	Return 0

end function

public function integer nf_set_unused_fields ();LONG  ll_row

/*	determine which fields are null and fill them in with a default value
	at some point in time any of the following fields may be unused
*/
	ll_row = idw_dw[1].GetRow()
	IF ll_row > 0 THEN
   	IF IsNull(idw_dw[1].GetItemString(ll_row, 'sort_name')) OR idw_dw[1].GetItemString(ll_row, 'sort_name') = '' THEN
/*		if unused default to the main name
*/
  	    idw_dw[1].SetItem(ll_row, 'sort_name', idw_dw[1].GetItemString(ll_row,'name'))
	   END IF
   	IF IsNull(idw_dw[1].GetItemString(ll_row, 'contact_name'))  OR idw_dw[1].GetItemString(ll_row, 'contact_name') = '' THEN
      	idw_dw[1].SetItem(ll_row, 'contact_name', '')
	    END IF
   	IF IsNull(idw_dw[1].GetItemString(ll_row, 'fax_no')) OR idw_dw[1].GetItemString(ll_row, 'fax_no') = '' THEN
      	idw_dw[1].SetItem(ll_row, 'fax_no', '')
	   END IF
   	IF IsNull(idw_dw[1].GetItemString(ll_row, 'active_flag'))  OR idw_dw[1].GetItemString(ll_row, 'active_flag') = '' THEN
	      idw_dw[1].SetItem(ll_row, 'active_flag', 'Y')
		END IF
   	IF IsNull(idw_dw[1].GetItemString(ll_row, 'nbms_early_filing_bonus_flag'))  OR idw_dw[1].GetItemString(ll_row, 'nbms_early_filing_bonus_flag' ) = '' THEN
	      idw_dw[1].SetItem(ll_row, 'nbms_early_filing_bonus_flag', 'N')
		END IF

/*		Now, set the unused address fields using the nf_set_unused_fields function in 
		n_validate_address
*/
		inv_address.nf_set_unused_fields()

	END IF

Return 0
end function

public function integer nf_set_defaults ();	LONG  ll_row
	Date   ldt_date
	
	ldt_date = Date(f_server_datetime())

	ll_row = idw_dw[1].GetRow()
	IF ll_row > 0 THEN

/*   	Set the default values for the provider insert
*/
     	idw_dw[1].SetItem(ll_row,'active_flag', 'Y')
		idw_dw[1].SetItem(ll_row,'nbms_early_filing_bonus_flag','N')
		idw_dw[1].SetItem(ll_row,'provider_sub_type_code','.')
		idw_dw[1].SetItem(ll_row,'active_start_date',ldt_date)
		idw_dw[1].Setitem(ll_row,'ephysio_flag','N')
		idw_dw[1].Setitem(ll_row,'physio_contract_flag','N')		
		idw_dw[1].Setitem(ll_row,'inactive_reason_desc','')
		idw_dw[1].Setitem(ll_row,'provider_inactive_reason_code','')
		idw_dw[1].Setitem(ll_row,'service_offered_language_code','U')
		idw_dw[1].Setitem(ll_row,'preferred_correspond_language_code','U')
		idw_dw[1].Setitem(ll_row,'hours_of_operation','')		
		idw_dw[1].Setitem(ll_row,'email_address','')
		idw_dw[1].Setitem(ll_row,'cellphone_no','')		
		
/*		Now call the common address function for setting address defaults
*/
		inv_address.nf_set_defaults()

	END IF

	Return 0
end function

public function integer nf_set_provider_sub_type_filter (string as_provider_type_code);Long    ll_row, ll_num_rows, ll_rtn, ll_provider_no
String  ls_filter
DataWindowChild ldwc_child

ll_row = idw_dw[1].GetChild('provider_sub_type_code', ldwc_child)
	
IF ll_row > 0 THEN
	ll_rtn = ldwc_child.SetTransObject(SQLCA)
	ll_num_rows = ldwc_child.Retrieve()
	ll_rtn = SQLCA.nf_handle_error("n_service_provider", "", "nf_set_provider_sub_type_filter - ldwc_child.Retrieve()")
	
	ls_filter = "provider_type_code = '" + as_provider_type_code + " '"

	ldwc_child.SetFilter(ls_filter)
	ldwc_child.SetSort("provider_sub_type_desc A")
	ldwc_child.Filter()
	ldwc_child.Sort()
	
	ll_provider_no = idw_dw[1].GetItemNumber(1,'provider_no')
	IF ll_provider_no = 0 THEN
		idw_dw[1].SetItem(1,'provider_sub_type_code','.')
	END IF
ELSE
	RETURN -1
END IF

RETURN 0


end function

public subroutine nf_check_for_payments (long al_provider_no, string as_provider_type_code, ref long al_processed_count, ref long al_unprocessed_count);SELECT  count(*)
INTO    :al_processed_count
FROM    PAYMENT           a ,
        APPLIED_CLAIM_TXN b
WHERE   a.payment_no          = b.payment_no
AND     b.recipient_type_code = :as_provider_type_code
AND     b.recipient_no        = :al_provider_no
USING SQLCA;

SQLCA.nf_handle_error("n_service_provider", "", "nf_check_bus_rule - SELECT COUNT(*) FROM Provider_Sub_Type")

SELECT  count(*)
INTO    :al_unprocessed_count
FROM    PAYMENT             a ,
		  UNAPPLIED_CLAIM_TXN b
WHERE   a.payment_no          = b.payment_no
AND     b.recipient_type_code = :as_provider_type_code
AND     b.recipient_no        = :al_provider_no
USING SQLCA;

SQLCA.nf_handle_error("n_service_provider", "", "nf_check_bus_rule - SELECT COUNT(*) FROM Provider_Sub_Type")

end subroutine

public function integer nf_check_bus_rule ();BOOLEAN lb_match
Long    ll_row, ll_count, ll_provider_no, ll_processed_count, ll_unprocessed_count, ll_count_space, ll_count_at, ll_len , x
Integer li_rtn, li_msg_rtn
String  ls_provider_type_code, ls_provider_sub_type_code, ls_active_flag, ls_inactive_reason
String  ls_nbms_flag, ls_chiro_flag, ls_prov_state_code, ls_nbms_eligible_flag, ls_cadre_flag, ls_email 
String  ls_provider_inactive_reason_code, ls_ephysio_flag, ls_prov_code, ls_orginal_flag, ls_physio_contract_flag, ls_string, ls_prov
String  ls_name, ls_contact_name,ls_cellphone_no, ls_hours_of_operation, ls_provider_sub_type_code_org
STRING ls_fax_no = "" //2014-09-05 David Worboys
STRING  ls_address_line1, ls_address_line2, ls_sort_name
Date   ldt_active_start_date, ldt_active_end_date
DataWindowChild ldwc_child

ll_row = idw_dw[1].GetRow()
IF ll_row <> 1 THEN 
	RETURN 0
END IF

ls_provider_type_code = idw_dw[1].GetItemString(ll_row, "provider_type_code")
ls_provider_sub_type_code = idw_dw[1].GetItemString(ll_row, "provider_sub_type_code")
ls_nbms_flag = idw_dw[1].GetItemString(ll_row, "nbms_early_filing_bonus_flag")
ls_chiro_flag = idw_dw[1].GetItemString(ll_row, "chiro_early_filing_bonus_flag")
ls_cadre_flag = idw_dw[1].GetItemString(ll_row, "cadre_flag")
ls_prov_state_code = idw_dw[1].GetItemString(ll_row, "prov_state_code")
ll_provider_no = idw_dw[1].GetItemNumber(ll_row, "provider_no")
ls_active_flag = idw_dw[1].GetItemString(ll_row, "active_flag")
ls_ephysio_flag = idw_dw[1].GetItemString(ll_row, "ephysio_flag")
ls_provider_inactive_reason_code = idw_dw[1].GetItemString(ll_row,"provider_inactive_reason_code")
ls_inactive_reason =  idw_dw[1].GetItemString(ll_row,"inactive_reason_desc")
ls_prov_code =  idw_dw[1].GetItemString(ll_row,"prov_state_code")
ldt_active_start_date = Date(idw_dw[1].GetItemDatetime(ll_row,"active_start_date"))
ldt_active_end_date = Date(idw_dw[1].GetItemDatetime(ll_row,"active_end_date"))
ls_physio_contract_flag = idw_dw[1].GetItemString(ll_row, "physio_contract_flag")
ls_email = idw_dw[1].GetItemString(ll_row,"email_address")
ls_prov = idw_dw[1].GetItemString(ll_row,"prov_state_code")
ls_name =  idw_dw[1].GetItemString(ll_row,"name")
ls_contact_name =  idw_dw[1].GetItemString(ll_row,"contact_name")
ls_cellphone_no =  idw_dw[1].GetItemString(ll_row,"cellphone_no")
ls_fax_no =  idw_dw[1].GetItemString(ll_row,"fax_no") //2014-09-05 David Worboys
ls_hours_of_operation =  idw_dw[1].GetItemString(ll_row,"hours_of_operation")
ls_provider_sub_type_code_org = idw_dw[1].GetItemString(ll_row, "provider_sub_type_code",primary!, true)

ls_address_line1       = idw_dw[1].GetItemString(ll_row,'address_line1')
ls_address_line2       = idw_dw[1].GetItemString(ll_row,'address_line2')
ls_sort_name           = idw_dw[1].GetItemString(ll_row,'sort_name')

IF LEFT(ls_name,1) = ' ' OR RIGHT(ls_name,1) = ' ' then 	idw_dw[1].setitem(1,'name',Trim(idw_dw[1].GetItemString(ll_row,'name')))	
IF LEFT(ls_inactive_reason,1) = ' ' OR RIGHT(ls_inactive_reason,1) = ' ' then 	idw_dw[1].setitem(1,'inactive_reason_desc',Trim(idw_dw[1].GetItemString(ll_row,'inactive_reason_desc')))	
IF LEFT(ls_contact_name,1) = ' ' OR RIGHT(ls_contact_name,1) = ' ' then 	idw_dw[1].setitem(1,'contact_name',Trim(idw_dw[1].GetItemString(ll_row,'contact_name')))	
IF LEFT(ls_cellphone_no,1) = ' ' OR RIGHT(ls_cellphone_no,1) = ' ' then 	idw_dw[1].setitem(1,'cellphone_no',Trim(idw_dw[1].GetItemString(ll_row,'cellphone_no')))	
IF LEFT(ls_hours_of_operation,1) = ' ' OR RIGHT(ls_hours_of_operation,1) = ' ' then 	idw_dw[1].setitem(1,'hours_of_operation',Trim(idw_dw[1].GetItemString(ll_row,'hours_of_operation')))	

//2014-09-05 David Worboys
IF (LEFT(ls_fax_no,1) = ' ' OR RIGHT(ls_fax_no,1) = ' ') THEN
	idw_dw[1].SetItem(1,'fax_no',Trim(idw_dw[1].GetItemString(ll_row,'fax_no')))	
END IF


// Validate type code
SELECT COUNT(*) 
  INTO :ll_count 
  FROM Provider_Type 
 WHERE provider_type_code = :ls_provider_type_code 
   AND active_flag = "Y" ;

li_rtn = SQLCA.nf_handle_error("n_service_provider", "", "nf_check_bus_rule - SELECT COUNT(*) FROM Provider_Type")

IF ll_count <= 0 THEN
	MessageBox('Invalid Type', 'The type selected is invalid.  Please correct.')
	idw_dw[1].SetColumn("provider_type_code")
	idw_dw[1].SetFocus()
	RETURN -1
END IF

//	Validate the sub type
SELECT COUNT(*) 
  INTO :ll_count 
  FROM Provider_Sub_Type 
 WHERE provider_type_code = :ls_provider_type_code 
   AND provider_sub_type_code = :ls_provider_sub_type_code 
   AND active_flag = "Y" ;

li_rtn = SQLCA.nf_handle_error("n_service_provider", "", "nf_check_bus_rule - SELECT COUNT(*) FROM Provider_Sub_Type")

IF ll_count <= 0 THEN
	IF ls_provider_type_code = 'O' AND ls_provider_sub_type_code = '.' THEN
		// OK
		idw_dw[1].SetItem(ll_row, 'provider_sub_type_code','')
		ls_provider_sub_type_code = ''
	ELSE
		IF IsNull(ls_provider_type_code) = TRUE THEN ls_provider_type_code = "NULL"
		IF IsNull(ls_provider_sub_type_code) = TRUE THEN ls_provider_sub_type_code = "NULL"
		MessageBox("Invalid Type / Sub-type combination", "The Type and Sub-Type combination selected is invalid.  Please correct.~r~r" +&
					  "type = '" + ls_provider_type_code + "'~rsubtype = '" + ls_provider_sub_type_code + "'")
		IF ls_provider_type_code = "NULL" THEN
			idw_dw[1].SetColumn("provider_type_code")
		ELSEIF ls_provider_sub_type_code = "NULL" THEN
			idw_dw[1].SetColumn("provider_sub_type_code")
		END IF
		idw_dw[1].SetFocus()
		RETURN -1
	END IF
END IF

IF idw_dw[1].GetItemStatus(1,'provider_sub_type_code',Primary!)= DataModified! THEN
	// if the sub type has been changed, check that provider does not have payments
	nf_check_for_payments(ll_provider_no, ls_provider_type_code, ll_processed_count, ll_unprocessed_count)
	IF ll_processed_count > 0 THEN
		MessageBox('Provider Payments','This provider has processed payments. Therefore, the provider sub type cannot be changed.', StopSign!)
		RETURN -1
	ELSEIF ll_unprocessed_count > 0 THEN
		MessageBox('Provider Payments','This provider has unprocessed payments. The provider sub type cannot be changed unless the scheduled payments are deleted. ' +&
							+ 'Run the Scheduled Payments to Providers/Recipients report to identify unprocessed payments for this provider.', StopSign!)
		RETURN -1
	END IF
	
	/* The provider sub type must not be changed if the sub type is a physiotherapy clinic.
		Rationale… therapists can be assigned, entity can be created in Web…, programs assigned
	*/
	IF ls_provider_sub_type_code_org <> ls_provider_sub_type_code AND ls_provider_sub_type_code_org = '35' THEN
		MessageBox('Physio Clinic','The provider sub type must not be changed if the sub type is a physiotherapy clinic.', StopSign!)
		RETURN -1	
	END IF 	
END IF
		

//	Validate active_flag - inactive_reason_desc combo. There should only be an inactive reason comment if the
// provider is actually inactive (active flag = 'N'). Also, the inactive reason comment is a required field
// when the provider is made inactive. (SR 86 - April 17, 2001 - E.McD.)
ls_active_flag	= idw_dw[1].GetItemString(ll_row, 'active_flag')
ls_inactive_reason = idw_dw[1].GetItemString(ll_row, 'inactive_reason_desc')

IF ls_active_flag = 'Y' AND IsNull(ls_inactive_reason) THEN
	//	Set to default value of ''.  Will trigger an application error if try to save a null.
	idw_dw[1].SetItem(ll_row, 'provider_inactive_reason_code', '')
	idw_dw[1].SetItem(ll_row, 'inactive_reason_desc', '')
ELSEIF ls_active_flag = 'Y' AND IsNull(ls_provider_inactive_reason_code) = FALSE AND ls_provider_inactive_reason_code <> "" THEN
	MessageBox('Invalid Inactive Reason Code', 'An inactive reason code must not be entered when a provider is active.', Information!)
	idw_dw[1].SetColumn('provider_inactive_reason_code')
	idw_dw[1].SetFocus()
	RETURN -1	
ELSEIF ls_active_flag = 'Y' AND IsNull(ls_inactive_reason) = FALSE AND ls_inactive_reason <> "" THEN
	MessageBox('Invalid Inactive Reason', 'An inactive reason must not be entered when a provider is active.', Information!)
	idw_dw[1].SetColumn('inactive_reason_desc')
	idw_dw[1].SetFocus()
	RETURN -1
ELSEIF ls_active_flag = 'N' AND (IsNull(ls_provider_inactive_reason_code) OR Trim(ls_provider_inactive_reason_code) = '') THEN
	MessageBox('Invalid Inactive Reason Code', 'An inactive reason code must be entered when a provider is made inactive.', Information!)
	idw_dw[1].SetColumn('provider_inactive_reason_code')
	idw_dw[1].SetFocus()
	RETURN -1	
ELSEIF ls_active_flag = 'N' AND (IsNull(ls_inactive_reason) OR Trim(ls_inactive_reason) = '') THEN
	MessageBox('Invalid Inactive Reason', 'An inactive reason must be entered when a provider is made inactive.', Information!)
	idw_dw[1].SetColumn('inactive_reason_desc')
	idw_dw[1].SetFocus()
	RETURN -1
END IF

//nf_check_for_payments(ll_provider_no, ls_provider_type_code, ll_processed_count, ll_unprocessed_count)
//IF ll_unprocessed_count > 0 AND ls_active_flag = 'N' THEN
//	MessageBox('Provider Payments','This provider has unprocessed payments. The active flag cannot be changed unless the scheduled payments are deleted. ' +&
//						+ 'Run the Scheduled Payments to Providers/Recipients report to identify unprocessed payments for this provider.', StopSign!)
//	RETURN -1
//END IF

//Validate the nbms flag
SELECT ISNULL(nbms_eligible_flag, 'N') 
  INTO :ls_nbms_eligible_flag 
  FROM Provider_Sub_Type 
 WHERE provider_type_code = :ls_provider_type_code 
	AND provider_sub_type_code = :ls_provider_sub_type_code ; 

li_rtn = SQLCA.nf_handle_error('n_service_provider', '', 'nf_check_bus_rule - SELECT nbms_eligible_flag FROM Provider_Sub_Type') 

IF (ls_nbms_flag = 'Y' AND ls_provider_type_code <> 'M') OR (ls_nbms_flag = 'Y' AND ls_provider_type_code = 'M' AND ls_nbms_eligible_flag = 'N') THEN
	MessageBox("NBMS Eligibility Error", "This provider is not eligibile for the Early Filing Bonus.", Exclamation!, Ok!)
	idw_dw[1].SetColumn('nbms_early_filing_bonus_flag')
	idw_dw[1].SetFocus()
	RETURN -1		
END IF
	
IF Isnull(ls_nbms_flag) or ls_nbms_flag = '' THEN
	idw_dw[1].setitem(1,'nbms_early_filing_bonus_flag','N')
END IF	

//Validate the chiro flag
IF ls_chiro_flag = 'Y' THEN
	IF ls_prov_state_code <> 'NB' THEN
		MessageBox("Chiro Eligibility Error","This provider is not based in NB, so is not eligibile for the Early Filing Bonus.", Exclamation!, Ok!)
		idw_dw[1].SetColumn('chiro_early_filing_bonus_flag')
		idw_dw[1].SetFocus()
		RETURN -1		
	ELSEIF ls_provider_type_code <> 'M' OR ( ls_provider_type_code  = 'M' AND ls_provider_sub_type_code <> '04') THEN
		MessageBox("Chiro Eligibility Error","This provider is not eligibile for the Early Filing Bonus.",information!,ok!)
		idw_dw[1].SetColumn('chiro_early_filing_bonus_flag')
		idw_dw[1].SetFocus()
		RETURN -1
	END IF
END IF

IF ls_chiro_flag = 'Y' AND ls_nbms_flag = 'Y' THEN
	MessageBox("NBMS & Chiro Eligibility Error","Providers cannot be eligibile for both the NBMS Early Filing Bonus and the Chiro Early Filing Bonus.", Exclamation!, Ok!)
	idw_dw[1].SetColumn('nbms_early_filing_bonus_flag')
	idw_dw[1].SetFocus()
	RETURN -1	
END IF

// Validate the Cadre flag
IF ls_cadre_flag = 'Y' THEN 
	IF ls_nbms_flag = 'Y' THEN
		MessageBox("Cadre & NBMS Eligibility Error", "Providers cannot be cadre and eligible for NBMS Early Filing Bonus.", Exclamation!, Ok!)
		RETURN 1
	END IF

	IF ls_chiro_flag = 'Y' THEN
		MessageBox("Cadre & Chiro Eligibility Error", "Providers cannot be cadre and eligible for Chiro Early Filing Bonus.", Exclamation!, Ok!)
		RETURN 1		
	END IF
				
	IF  ls_provider_type_code <> 'M' THEN
		MessageBox('Error','The cadre flag can not be set to Y. The provider must be Medical Aid.',Exclamation!)
		Return -1
	ELSEIF  ls_provider_type_code = 'M' AND ls_provider_sub_type_code  <> '10' THEN
		MessageBox('Error','The cadre flag can not be set to Y. The provider must have a sub type of MD - General Practice.',Exclamation!)
		Return -1		
	ELSEIF  ls_provider_type_code = 'M' AND ls_provider_sub_type_code  = '10' AND LEFT(ls_name,5) <> 'WHSCC' THEN
		MessageBox('Error','The cadre flag can not be set to Y. The provider must have a name of WHSCC.',Exclamation!)
		Return -1		
	ELSEIF  ls_provider_type_code = 'M' AND ls_provider_sub_type_code  = '10' AND LEFT(ls_name,5) = 'WHSCC' AND LEFT(ls_contact_name,5) <> 'CADRE' THEN
		MessageBox('Error','The cadre flag can not be set to Y. The provider must have a contact name of CADRE.',Exclamation!)
		Return -1
	END IF
	
ELSEIF ls_cadre_flag = 'N' THEN
	
	IF 	 ls_provider_type_code = 'M' AND ls_provider_sub_type_code  = '10' AND LEFT(ls_contact_name,5) = 'CADRE' THEN
		MessageBox('Error','The cadre flag can not be set to N. The provider must not have a contact name of CADRE.',Exclamation!)
		Return -1
	ELSEIF 	 ls_provider_type_code <> 'M'  AND LEFT(ls_contact_name,5) = 'CADRE' THEN
		MessageBox('Error','The cadre flag can not be set to N. The provider must not have a contact name of CADRE.',Exclamation!)
		Return -1
	END IF
END IF

IF (THIS.nf_check_provider_ephysio( ) = -1 ) THEN  //2014/10/01 David Worboys
	Return -1
ELSE
	//Validate the ePhysio flag
	IF ls_provider_type_code = 'M' and ls_provider_sub_type_code = '35' THEN
		IF ls_ephysio_flag <> 'Y' AND ls_ephysio_flag <> 'N' THEN
			MessageBox('Error','This provider is a physio clinic and must have the Physio Billing flag set to Y or N.',exclamation!)
			Return -1
		END IF
		IF ls_ephysio_flag = 'N' and ls_prov_code = 'NB' and ls_active_flag = 'Y' THEN
			MessageBox('Warning','This provider is a NB physio clinic and should have the Physio Billing flag set to Y.',information!) 
		END IF
		IF ls_ephysio_flag = 'Y' and ls_prov_code <> 'NB' and ls_active_flag = 'Y' THEN
			MessageBox('Warning','This provider is not NB physio clinic and should have the Physio Billing flag set to N.',information!) 
		END IF
	END IF
	IF ls_ephysio_flag = 'Y' OR ls_physio_contract_flag = 'Y' THEN
		IF ls_provider_type_code <> 'M' OR (ls_provider_type_code = 'M' and ls_provider_sub_type_code <> '35') THEN
			MessageBox('Error','The provider must be a Physio provider when the ePhysio Billing flag or Contract flag are on.',Exclamation!)
			Return -1
		END IF
		IF ls_prov <> 'NB' AND ls_prov <> 'NS' THEN
			MessageBox('Error','The provider must be a NB or NS provider when the ePhysio Billing flag or Contract flag are on.',Exclamation!)
			Return -1
		END IF	
		IF  ls_ephysio_flag = 'N' and ls_physio_contract_flag = 'Y' THEN
			MessageBox('Error','The ePhysio Billing flag must be ticked on when the Contract flag is on.',Exclamation!)
			Return -1
		END IF	
		IF ls_active_flag = 'N' THEN
			MessageBox('Error','The provider must be active when the ePhysio Billing and Contract flags are on.',Exclamation!)
			Return -1
		END IF			
	END IF
END IF

Select active_flag
into    :ls_orginal_flag
From  PROVIDER
Where provider_no = :ll_provider_no
And     provider_type_code = :ls_provider_type_code
And     provider_sub_type_code = :ls_provider_sub_type_code
Using SQLCA;

SQLCA.nf_handle_error("n_service_provider", "nf_check_bus_rule", "SELECT COUNT(*) FROM Provider")

IF ls_provider_type_code = 'M' and ls_provider_sub_type_code = '35' and  ls_active_flag = 'N' and ls_orginal_flag = 'Y' THEN
    nf_check_rehab_tasks()
END IF

//Validate the inactive reason code
IF ls_active_flag = 'N' THEN
	IF IsNull(ls_provider_inactive_reason_code) or ls_provider_inactive_reason_code = '' THEN
		MessageBox("Inactive Reason Code Error","The inactive reason code must be entered when the active flag is set to no.",exclamation!)
		Return -1
	END IF
	IF IsNull(ls_inactive_reason) or ls_inactive_reason = "" THEN
		MessageBox("Inactive Reason Error","The inactive reason comment must be entered when the active flag is set to no.",exclamation!)
		Return -1
	END IF	
END IF


// BR 2.520  A warning must be displayed if a provider is inactivated with ‘CRA – Address contains SIN’ as the inactive reason.
IF idw_dw[1].GetItemStatus(1,'provider_inactive_reason_code',Primary!) = NotModified! THEN
	// do nothing
ELSE
	IF ls_provider_inactive_reason_code = '12' THEN
		// "CRA – Address contains SIN"
		// warn user that this inactive code cannot be modified once saved
		li_msg_rtn = MessageBox('Inactivate Provider?','Making this provider inactive cannot be reversed once the change is saved.' &
														 + '~r~n~r~nDo you want to continue with the save?',Question!,YesNo!,2)
		IF li_msg_rtn = 2 THEN RETURN -1
	END IF
END IF

IF IsNull(ldt_active_start_date) THEN
	MessageBox("Active Date Error","A valid active start date must be entered.",Exclamation!)
	Return -1
ELSEIF ldt_active_start_date > ldt_active_end_date THEN
	MessageBox("Active Date Error","The active start date must be greater than the active end date.",Exclamation!)
	Return -1
ELSEIF IsNull(ldt_active_end_date) and ls_active_flag = 'N' THEN
	MessageBox("Active End Date Error","The active end date must be entered if the provider is not active.",Exclamation!)
	Return -1
END IF
	
IF nf_check_address(ls_provider_type_code) = -1 THEN
	RETURN -1
END IF

// Now, call the nf_check_bus_rule function in n_validate_address for common address validations 
IF inv_address.nf_check_bus_rule() < 0 THEN
	RETURN -1
END IF

IF NOT IsNull(ls_email) and ls_email <> '' THEN
	
	IF LEN(ls_email) > 120 THEN
		MessageBox('Error Invalid Email','The email address must be valid and not more than 120 characters.',Exclamation!)
		Return -1
	END IF		
	
	IF LEN(ls_email) < 5  OR Pos(ls_email, "@") < 2  OR Pos(ls_email, ".") < 3 THEN
		MessageBox('Error Invalid Email','The email address must be in proper format ex. name@nb.ca .',Exclamation!)
		Return -1
	END IF
	IF Pos(ls_email,'.-') <> 0 OR   Pos(ls_email,'-.') <> 0 OR   Pos(ls_email,'..') <> 0 THEN
		MessageBox('Error Invalid Email',"The email address must not contain '.- 'or '-.' or '..' .",Exclamation!)
		Return -1
	END IF
	IF Pos(ls_email,'@@') <> 0 OR Pos(ls_email,'--') <> 0 OR  Pos(ls_email,'@.') <> 0 THEN
		MessageBox('Error Invalid Email',"The email address must not contain '@@ 'or '--' or '@.' .",Exclamation!)
		Return -1
	END IF
	IF Pos(ls_email,'.@') <> 0 THEN
		MessageBox('Error Invalid Email',"The email address must not contain '.@ '.",Exclamation!)
		Return -1
	END IF
	IF Pos(ls_email,' ') <> 0 THEN
		MessageBox('Error Invalid Email',"The email address must not contain a space.",Exclamation!)
		Return -1
	END IF	
	IF Pos(ls_email, "@") = 1  OR Pos(ls_email, ".") = 1 OR  Pos(ls_email, " ") = 1 THEN
		MessageBox('Error Invalid Email','The email address must be more than 5 characters.',Exclamation!)
		Return -1
	END IF
	ll_len = LEN(ls_email)
	
	FOR x = 1 to ll_len
		ls_string = left(ls_email,x)
		IF MID(ls_email,x,1) = '@'  then
			ll_count_at = ll_count_at + 1
		END IF
		IF MID(ls_email,x,1) = ' ' then
			ll_count_space = ll_count_space + 1
		END IF
	NEXT
	IF ll_count_at > 1 THEN
		MessageBox('Error Invalid Email',"The email address can not have more than one '@' character.",Exclamation!)
		Return -1
	END IF
	IF ll_count_space > 0 THEN
		MessageBox('Error Invalid Email',"The email address can not have a space in it.",Exclamation!)
		Return -1
	END IF
	
END IF

IF ls_cellphone_no <> '' AND  idw_dw[1].GetItemStatus(1,'cellphone_no',Primary!)= DataModified! THEN
	
	ls_cellphone_no  =TRIM(ls_cellphone_no)
	
	IF MID(ls_cellphone_no,1,1) = '0' OR MID(ls_cellphone_no,1,1) = '1' THEN
		MessageBox('Error Invalid Cellphone No','The cell phone no must be a valid phone no and can not start with 0 or 1.',Exclamation!)
		Return -1
	END IF
	
	IF LEN(ls_cellphone_no) <> 10 AND LEN(ls_cellphone_no) <> 0 THEN
		MessageBox('Error Invalid Cellphone No','The cell phone no must be a valid phone no.',Exclamation!)
		Return -1
	END IF		

	idw_dw[1].SetItem(ll_row,"cellphone_no",ls_cellphone_no)

END IF	

//2014-09-05 David Worboys - someone left out fax validation!
IF (ls_fax_no <> '' AND  idw_dw[1].GetItemStatus(1,'fax_no',Primary!)= DataModified!) THEN
	
	ls_fax_no  =TRIM(ls_fax_no)
	
	IF (MID(ls_fax_no,1,1) = '0' OR MID(ls_fax_no,1,1) = '1') THEN
		MessageBox('Error Invalid Fax No','The fax phone no must be a valid phone no and can not start with 0 or 1.',Exclamation!)
		Return -1
	END IF
	
	IF (LEN(ls_fax_no) <> 10 AND LEN(ls_fax_no) <> 0) THEN
		MessageBox('Error Invalid Fax No','The fax phone no must be a valid phone no.',Exclamation!)
		Return -1
	END IF		

	idw_dw[1].SetItem(ll_row,"fax_no",ls_fax_no)

END IF	

IF ls_hours_of_operation <> '' AND  idw_dw[1].GetItemStatus(1,'hours_of_operation',Primary!)= DataModified! THEN
	
	ls_hours_of_operation  =TRIM(ls_hours_of_operation)
	idw_dw[1].SetItem(ll_row,"hours_of_operation",ls_hours_of_operation)

END IF		


IF ls_active_flag = 'Y' THEN
	
	// BR 2.540  A warning must be displayed if a provider is saved with a name, or an address that includes nine digits.
	lb_match = Match(ls_name,'[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]')
	IF lb_match THEN
		li_msg_rtn = MessageBox('Save Provider?','You are attempting to save a provider with a name that includes 9 consecutive digits.' &
													  + '~r~nProvider fields are not allowed to include the SIN or Medicare number.' &
													  + '~r~n' &
													  + '~r~nDo you want to continue?',Question!,YesNo!,2)
		IF li_msg_rtn = 2 THEN RETURN -1
	END IF
	lb_match = Match(ls_name,'[0-9][0-9][0-9]-[0-9][0-9][0-9]-[0-9][0-9][0-9]')
	IF lb_match THEN
		li_msg_rtn = MessageBox('Save Provider?','You are attempting to save a provider with a name that includes 9 digits.' &
													  + '~r~nProvider fields are not allowed to include the SIN or Medicare number.' &
													  + '~r~n' &
													  + '~r~nDo you want to continue?',Question!,YesNo!,2)
		IF li_msg_rtn = 2 THEN RETURN -1
	END IF
	
	lb_match = Match(ls_sort_name,'[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]')
	IF lb_match THEN
		li_msg_rtn = MessageBox('Save Provider?','You are attempting to save a provider with a search name that includes 9 consecutive digits.' &
													  + '~r~nProvider fields are not allowed to include the SIN or Medicare number.' &
													  + '~r~n' &
													  + '~r~nDo you want to continue?',Question!,YesNo!,2)
		IF li_msg_rtn = 2 THEN RETURN -1
	END IF
	lb_match = Match(ls_sort_name,'[0-9][0-9][0-9]-[0-9][0-9][0-9]-[0-9][0-9][0-9]')
	IF lb_match THEN
		li_msg_rtn = MessageBox('Save Provider?','You are attempting to save a provider with a search name that includes 9 digits.' &
													  + '~r~nProvider fields are not allowed to include the SIN or Medicare number.' &
													  + '~r~n' &
													  + '~r~nDo you want to continue?',Question!,YesNo!,2)
		IF li_msg_rtn = 2 THEN RETURN -1
	END IF
	
	lb_match = Match(ls_contact_name,'[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]')
	IF lb_match THEN
		li_msg_rtn = MessageBox('Save Provider?','You are attempting to save a provider with a contact name that includes 9 consecutive digits.' &
													  + '~r~nProvider fields are not allowed to include the SIN or Medicare number.' &
													  + '~r~n' &
													  + '~r~nDo you want to continue?',Question!,YesNo!,2)
		IF li_msg_rtn = 2 THEN RETURN -1
	END IF
	lb_match = Match(ls_contact_name,'[0-9][0-9][0-9]-[0-9][0-9][0-9]-[0-9][0-9][0-9]')
	IF lb_match THEN
		li_msg_rtn = MessageBox('Save Provider?','You are attempting to save a provider with a contact name that includes 9 digits.' &
													  + '~r~nProvider fields are not allowed to include the SIN or Medicare number.' &
													  + '~r~n' &
													  + '~r~nDo you want to continue?',Question!,YesNo!,2)
		IF li_msg_rtn = 2 THEN RETURN -1
	END IF
	
	lb_match = Match(ls_address_line1,'[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]')
	IF lb_match THEN
		li_msg_rtn = MessageBox('Save Provider?','You are attempting to save a provider with an address that includes 9 consecutive digits.' &
													  + '~r~nProvider fields are not allowed to include the SIN or Medicare number.' &
													  + '~r~n' &
													  + '~r~nDo you want to continue?',Question!,YesNo!,2)
		IF li_msg_rtn = 2 THEN RETURN -1
	END IF
	lb_match = Match(ls_address_line1,'[0-9][0-9][0-9]-[0-9][0-9][0-9]-[0-9][0-9][0-9]')
	IF lb_match THEN
		li_msg_rtn = MessageBox('Save Provider?','You are attempting to save a provider with an address that includes 9 digits.' &
													  + '~r~nProvider fields are not allowed to include the SIN or Medicare number.' &
													  + '~r~n' &
													  + '~r~nDo you want to continue?',Question!,YesNo!,2)
		IF li_msg_rtn = 2 THEN RETURN -1
	END IF
	
	lb_match = Match(ls_address_line2,'[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]')
	IF lb_match THEN
		li_msg_rtn = MessageBox('Save Provider?','You are attempting to save a provider with an address that includes 9 consecutive digits.' &
													  + '~r~nProvider fields are not allowed to include the SIN or Medicare number.' &
													  + '~r~n' &
													  + '~r~nDo you want to continue?',Question!,YesNo!,2)
		IF li_msg_rtn = 2 THEN RETURN -1
	END IF
	lb_match = Match(ls_address_line2,'[0-9][0-9][0-9]-[0-9][0-9][0-9]-[0-9][0-9][0-9]')
	IF lb_match THEN
		li_msg_rtn = MessageBox('Save Provider?','You are attempting to save a provider with an address that includes 9 digits.' &
													  + '~r~nProvider fields are not allowed to include the SIN or Medicare number.' &
													  + '~r~n' &
													  + '~r~nDo you want to continue?',Question!,YesNo!,2)
		IF li_msg_rtn = 2 THEN RETURN -1
	END IF
END IF

RETURN 0
end function

public subroutine nf_check_for_awards (long al_recipient_no, string as_recipient_type_code, ref long al_awards);
SELECT count(*)
INTO   :al_awards
FROM   PERIODIC_AWARD a, PERIODIC_RECIPIENT b
WHERE  a.award_no = b.award_no
AND    a.claim_no = b.claim_no
AND    b.recipient_type_code = :as_recipient_type_code
AND    b.recipient_no = :al_recipient_no
AND    (a.award_end_date > convert(char (10),getdate(),120) /*just the date portion*/
OR     a.award_end_date is null) 
USING  SQLCA;

SQLCA.nf_handle_error("n_service_provider", "", "nf_check_for_awards - SELECT COUNT(*) FROM PERIODIC_AWARD")


end subroutine

public function integer nf_change_item (long al_datawindow);Long    ll_row, ll_provider_no, ll_processed_count, ll_unprocessed_count
Integer li_rtn, li_counter
String  ls_active_flag, ls_inactive_reason, ls_provider_type_code, ls_column_name, ls_provider_sub_type_code_org
String  ls_nbms_flag, ls_provider_sub_type_code, ls_chiro_flag, ls_prov_state_code
String  ls_nbms_provider_type = 'M', ls_nbms_eligible_flag, ls_ephysio_flag, ls_physio_contract_flag
String  ls_cadre_flag, ls_nbms_early_filing_bonus_flag, ls_chiro_early_filing_bonus_flag , ls_inactive_reason_code,ls_rehab_code
Date   ldt_date, ldt_active_start_date
datetime ldtm_active_end_date, ldtm_null

ll_provider_no = idw_dw[1].GetItemNumber(1,'provider_no')
ls_provider_type_code = idw_dw[1].GetItemString(1,'provider_type_code')
ls_provider_sub_type_code = idw_dw[1].GetItemString(1,'provider_sub_type_code')
ls_provider_sub_type_code_org = idw_dw[1].GetItemString(1, 'provider_sub_type_code',primary!, true)

SETPOINTER(HOURGLASS!) //2014-10-03 David Worboys

Select active_end_date
Into    :ldtm_active_end_date
From  PROVIDER
Where provider_no = :ll_provider_no
and     provider_type_code = :ls_provider_type_code
Using SQLCA;

 SQLCA.nf_handle_error('n_service_provider', 'nf_change_item', 'SELECT PROVIDER') 


ldt_date = Date(f_server_datetime())
SetNull(ldtm_null )

IF al_datawindow = 1 THEN
	ll_row = idw_dw[1].GetRow()
	ls_column_name = idw_dw[1].GetColumnName()

	IF ls_column_name = 'provider_type_code' THEN
		// retrieve the data into the sub type list box
		ls_provider_type_code = idw_dw[1].GetText()
		nf_set_provider_sub_type_filter(ls_provider_type_code)

		IF ls_provider_type_code <> ls_nbms_provider_type THEN
			idw_dw[1].SetItem(1, "nbms_early_filing_bonus_flag", "N")
		END IF

		IF ll_provider_no = 0 THEN
			idw_dw[1].SetItem(1,'provider_sub_type_code','.')
		END IF
	ELSEIF ls_column_name = 'provider_sub_type_code' THEN	
		ls_provider_sub_type_code =  idw_dw[1].GetText()

		SELECT ISNULL(nbms_eligible_flag, 'N')  
		  INTO :ls_nbms_eligible_flag 
		  FROM Provider_Sub_Type 
		 WHERE provider_type_code = :ls_provider_type_code 
		   AND provider_sub_type_code = :ls_provider_sub_type_code ; 

		li_rtn = SQLCA.nf_handle_error('n_service_provider', '', 'nf_change_item - SELECT nbms_eligible_flag FROM Provider_Sub_Type') 

		IF ls_provider_type_code = ls_nbms_provider_type AND ls_nbms_eligible_flag = 'N' THEN 
			idw_dw[1].SetItem(1, "nbms_early_filing_bonus_flag", "N")
		ELSEIF ls_provider_type_code <> ls_nbms_provider_type THEN
			idw_dw[1].SetItem(1, "nbms_early_filing_bonus_flag", "N")
		END IF

		IF ll_provider_no > 0 THEN
			nf_check_for_payments(ll_provider_no, ls_provider_type_code, ll_processed_count, ll_unprocessed_count)
			IF ll_processed_count > 0 THEN
				MessageBox('Provider Payments','This provider has processed payments. Therefore, the provider sub type cannot be changed.', StopSign!)
				RETURN 1
			ELSEIF ll_unprocessed_count > 0 THEN
				MessageBox('Provider Payments','This provider has unprocessed payments. The provider sub type cannot be changed unless the scheduled payments are deleted. ' +&
									+ 'Run the Scheduled Payments to Providers/Recipients report to identify unprocessed payments for this provider.', StopSign!)
				RETURN 1
			END IF

			//PR4455 (2006.05.15, R.S.) Check for open awards, deny attempt to change provider sub type code if there are any
		  	nf_check_for_awards(ll_provider_no, ls_provider_type_code, ll_unprocessed_count)
			IF ll_unprocessed_count > 0 THEN
				MessageBox('Provider Awards','This provider has open awards pending. The provider sub type cannot be changed unless the open awards are deleted or ended. ' + &
									+ 'Run the Scheduled Payments to Providers/Recipients report to identify open awards for this provider.', StopSign!)
				RETURN 1
			END IF
			// end PR4455
		END IF
		
		ls_ephysio_flag = idw_dw[1].getitemstring(1,'ephysio_flag')
		ls_physio_contract_flag = idw_dw[1].getitemstring(1,'physio_contract_flag') 
		
		IF ls_provider_sub_type_code <> '35' AND (ls_ephysio_flag = 'Y'  or ls_physio_contract_flag = 'Y') THEN
			idw_dw[1].SetItem(1,'ephysio_flag','N') 
			idw_dw[1].SetItem(1,'physio_contract_flag','N')
		END IF
		
		/* The provider sub type must not be changed if the sub type is a physiotherapy clinic.
		Rationale… therapists can be assigned, entity can be created in Web…, programs assigned
	*/
		IF ls_provider_sub_type_code_org <> ls_provider_sub_type_code AND ls_provider_sub_type_code_org = '35' THEN
			MessageBox('Physio Clinic','The provider sub type must not be changed if the sub type is a physiotherapy clinic.', StopSign!)
			RETURN 1	
		END IF 

	ELSEIF ls_column_name =	'active_flag' THEN
		ls_active_flag = idw_dw[1].GetText()
		ls_inactive_reason = idw_dw[1].GetItemString(idw_dw[1].GetRow(), 'inactive_reason_desc')

		IF ll_provider_no > 0 THEN
			nf_check_for_payments(ll_provider_no, ls_provider_type_code, ll_processed_count, ll_unprocessed_count)
//			IF ll_unprocessed_count > 0 THEN
//				MessageBox('Provider Payments','This provider has unprocessed payments. The active flag cannot be changed unless the scheduled payments are deleted. ' +&
//													+ 'Run the Scheduled Payments to Providers/Recipients report to identify unprocessed payments for this provider.', StopSign!)
//				RETURN 1
//			END IF

			//PR4455 (2006.05.15, R.S.) Check for open awards, deny attempt to set provider status to inactive if there are any
		  	nf_check_for_awards(ll_provider_no, ls_provider_type_code, ll_unprocessed_count)
			IF ll_unprocessed_count > 0  and ls_active_flag = 'N' THEN
				MessageBox('Provider Awards','This provider has open awards pending. The provider cannot be made inactive unless the open awards are deleted or ended. ' + &
									+ 'Run the Scheduled Payments to Providers/Recipients report to identify open awards for this provider.', StopSign!)
				RETURN 1
			END IF
			// end PR4455
		END IF
		
		IF ls_active_flag = 'N' THEN
			//set the active_end_date it must be equal to the current date
			idw_dw[1].SetItem(idw_dw[1].GetRow(),'active_end_date',ldt_date)	
			idw_dw[1].SetItem(idw_dw[1].GetRow(),'physio_contract_flag','N')
			idw_dw[1].SetItem(idw_dw[1].GetRow(),'ephysio_flag','N')
			
			/* 3.110	All physiotherapy programs for a physiotherapy clinic must become inactive if the physiotherapy clinic is inactive. */
			IF  ls_provider_sub_type_code = '35' AND  ls_provider_type_code = 'M' THEN
				FOR li_counter = 1 TO idw_dw[4].rowcount()
						idw_dw[4].SetItem(li_counter,'active_flag','N')	
				NEXT
			END IF 
	
		END IF
		
		IF ls_active_flag = 'Y' THEN
			//set the active end date to null.
			idw_dw[1].SetItem(idw_dw[1].GetRow(),'active_end_date',ldtm_null)
			idw_dw[1].SetItem(idw_dw[1].GetRow(),'active_start_date',ldt_date)
			idw_dw[1].SetItem(idw_dw[1].GetRow(), 'provider_inactive_reason_code','')
			idw_dw[1].SetItem(idw_dw[1].GetRow(), 'inactive_reason_desc','')
			
			
			/* 	3.100   An active physiotherapy clinic must have an active assignment to the Primary Physiotherapy program.
				   3.120   An active physiotherapy clinic must have an active assignment to the P & A program.
 			*/
			IF  ls_provider_sub_type_code = '35' AND  ls_provider_type_code = 'M' THEN
				FOR li_counter = 1 TO idw_dw[4].rowcount()
					
					ls_rehab_code = idw_dw[4].getitemstring(li_counter, 'rehab_program_code')
					IF ls_rehab_code = 'P001' OR  ls_rehab_code = 'P007' THEN 
						idw_dw[4].SetItem(li_counter,'active_flag','Y')	
					END IF 
				NEXT
			END IF
					
		END IF
			
			

	ELSEIF ls_column_name = 'nbms_early_filing_bonus_flag' THEN
		ls_nbms_flag = idw_dw[1].GetText()

		SELECT ISNULL(nbms_eligible_flag, 'N') 
		  INTO :ls_nbms_eligible_flag 
		  FROM Provider_Sub_Type 
		 WHERE provider_type_code = :ls_provider_type_code 
		   AND provider_sub_type_code = :ls_provider_sub_type_code ; 

		li_rtn = SQLCA.nf_handle_error('n_service_provider', '', 'nf_change_item - SELECT nbms_eligible_flag FROM Provider_Sub_Type') 

		IF (ls_nbms_flag = 'Y' AND ls_provider_type_code <> 'M') OR (ls_nbms_flag = 'Y' AND ls_provider_type_code = 'M' AND ls_nbms_eligible_flag = 'N') THEN  
			MessageBox("NBMS Eligibility Error", "This provider is not eligibile for the NBMS Early Filing Bonus.", Exclamation!, Ok!)
			RETURN 1
		END IF

		ls_cadre_flag = idw_dw[1].GetItemString(1, "cadre_flag")
		IF ls_nbms_flag = 'Y' AND ls_cadre_flag = 'Y' THEN
			MessageBox("NBMS Eligibility Error", "This provider is a cadre provider so it is not eligible for the NBMS Early Filing Bonus.", Exclamation!, Ok!)
			RETURN 1		
		END IF
	ELSEIF ls_column_name = 'chiro_early_filing_bonus_flag' THEN
		ls_chiro_flag = idw_dw[1].GetText()
		ls_prov_state_code = idw_dw[1].GetItemString(1, "prov_state_code")

		IF ls_chiro_flag = 'Y' THEN
			IF ls_prov_state_code <> 'NB' THEN
				MessageBox("Chiro Eligibility Error", "This provider is not based in NB, so is not eligibile for the Early Filing Bonus.", Exclamation!, Ok!)
				RETURN 1		
			ELSEIF ls_provider_type_code <> 'M' OR ( ls_provider_type_code  = 'M' AND ls_provider_sub_type_code <> '04') THEN
				MessageBox("Chiro Eligibility Error", "This provider is not eligibile for the Early Filing Bonus.", Exclamation!, Ok!)
				RETURN 1
			END IF
			
			ls_cadre_flag = idw_dw[1].GetItemString(1, "cadre_flag")
			IF ls_cadre_flag = 'Y' THEN
				MessageBox("Chiro Eligibility Error", "This provider is a cadre provider so it is not eligible for a Chiro bonus.", Exclamation!, Ok!)
				RETURN 1		
			END IF
		END IF

	ELSEIF ls_column_name = 'cadre_flag' THEN
		ls_cadre_flag = idw_dw[1].GetText()

		ls_nbms_early_filing_bonus_flag = idw_dw[1].GetItemString(1, "nbms_early_filing_bonus_flag")
		ls_chiro_early_filing_bonus_flag = idw_dw[1].GetItemString(1, "chiro_early_filing_bonus_flag")
		
		IF ls_cadre_flag = 'Y' THEN
			IF ls_nbms_early_filing_bonus_flag = 'Y' THEN
				MessageBox("Cadre Provider Error", "This provider is eligible for NBMS early filing bonus so it can't be a Cadre provider too.", Exclamation!, Ok!)
				RETURN 1		
			END IF

			IF ls_chiro_early_filing_bonus_flag = 'Y' THEN
				MessageBox("Cadre Provider Error", "This provider is eligible for Chiro bonus so it can't be a Cadre provider too.", Exclamation!, Ok!)
				RETURN 1		
			END IF

		END IF
		
	ELSEIF ls_column_name = 'ephysio_flag' THEN
		ls_ephysio_flag = idw_dw[1].GetText()
		
		IF ls_ephysio_flag = 'N' THEN
			 idw_dw[1].setitem(1,'physio_contract_flag','N')
		END IF

	ELSEIF ls_column_name = 'phsyio_contract_flag' THEN
		ls_physio_contract_flag = idw_dw[1].GetText()
		
		IF ls_physio_contract_flag = 'Y' THEN
			 idw_dw[1].setitem(1,'ephysio_flag','Y')
		END IF
	
	ELSEIF  ls_column_name = 'active_start_date' THEN
		ldt_active_start_date = Date(idw_dw[1].GetText())
	
		IF ldt_active_start_date < Date(ldtm_active_end_date) THEN
				MessageBox('Error','The active start date must be on or after the previous active end date.',exclamation!)
				Return 1
		END IF

		IF ldt_active_start_date > ldt_date THEN
				MessageBox('Error Invalid Start Date','The active start date must not be set to a future date.',exclamation!)
				Return 1
		END IF		
		

	ELSEIF  ls_column_name = 'provider_inactive_reason_code' THEN
		ls_inactive_reason_code = idw_dw[1].GetText()
				
	ELSE
		//	Now, call the nf_change_item function in n_validate_address in case any of the address fields have changed
		inv_address.nf_change_item()
	END IF
END IF

RETURN 0

end function

public function integer nf_check_rehab_tasks ();long ll_rows , ll_start
string ls_task_status_code
Date ldt_date

ldt_date = Date(f_server_datetime())

//All planned rehab tasks must be cancelled & in progress rehab tasks must be closed

ll_rows = idw_dw[3].RowCount()

For ll_start = 1 to ll_rows
	
	ls_task_status_code = idw_dw[3].Getitemstring(ll_start,'task_status_code')
	
	If ls_task_status_code = '01' then
		idw_dw[3].SetItem(ll_start,'task_status_code','03')  //change from planned to cancelled
	ElseIf ls_task_status_code = '02' then
		idw_dw[3].SetItem(ll_start,'task_status_code','04') //change from inprogress to closed
		idw_dw[3].Setitem(ll_start,'actual_completion_date',ldt_date)
	End If
	
Next




Return 1
end function

public function integer nf_log_events ();LONG			ll_row	, ll_provider_no	
INTEGER		li_counter, li_row
STRING		ls_provider_type_code, ls_provider_sub_type_code,  ls_program_array[]



ll_row = idw_dw[1].GetRow()
IF ll_row <> 1 THEN RETURN 0

ls_provider_type_code 			= idw_dw[1].GetItemString(ll_row, "provider_type_code")
ls_provider_sub_type_code 		= idw_dw[1].GetItemString(ll_row, "provider_sub_type_code")
ll_provider_no						= idw_dw[1].GetItemnumber(ll_row, "provider_no")

//Validate the ePhysio flag
IF ls_provider_type_code = 'M' and ls_provider_sub_type_code = '35' AND  idw_dw[1].getitemstatus(ll_row, 0, primary! ) = newmodified! THEN

		// set up the program array -- PRIMARY & P&A
		/* 4.80 		A physiotherapist must be assigned to the primary physiotherapy program if approved.*/
		/* 4.90 		A physiotherapist must be assigned to the P&A physiotherapy program if approved. */
		 ls_program_array[1] = 'P001'
		 ls_program_array[2] = 'P007'
		 
		// insert a row in for primary and P&A
		FOR li_counter = 1 TO upperbound(ls_program_array)	
				li_row =  idw_dw[4].insertrow(0)
				 idw_dw[4].setitem(li_row, 'rehab_program_code',ls_program_array[li_counter])
				 idw_dw[4].setitem(li_row, 'provider_no',ll_provider_no)
				 idw_dw[4].setitem(li_row, 'active_flag','Y')
		NEXT
	
END IF 




end function

public function integer nf_check_provider_ephysio ();/*
**		Type  		: Function
**		Name 		: nf_check_provider_ephysio
**		Arguments 	: 
**		Returns 		:
**		Purpose		: To Validate active and ephysio flags
**		Date			: 2014/10/01
**		Author		: David Worboys
**	
**		Modifications
**    2914-01-10 EH1 David Worboys moved to this function in this object from the clicked event of a Save Button
*/
BOOLEAN	lb_active_changed 				 = FALSE		//2014-09-04 David Worboys
BOOLEAN	lb_ephysio_changed 				 = FALSE		//2014-09-04 David Worboys
DATE 			ldt_null 											    //2014-08-22 David Worboys
LONG          ll_provider_no 						= 0                                                                                                                                                                                                                                           //2014-08-22 David Worboys
LONG			ll_result_count						= 0		    //2014-08-22 David Worboys
LONG			ll_return								= 1
STRING 		ls_message 							= ""	        //2014-08-22 David Worboys
STRING       is_provider_type_code 			= ""
STRING		ls_title								= ""		    //2014-08-27 David Worboys
dwItemStatus ldws_active_status 							    //2014-09-05 David Worboys
dwItemStatus ldws_ephysio_status 						    //2014-09-05 David Worboys

SetNull(ldt_null)												    //2014-08-22 David Worboys
ls_message = "" 												    //2014-08-29 David Worboys
ls_title 		= "" 												    //2014-08-29 David Worboys


ib_blow_entity_members_away = FALSE

//2014-08-22 David Worboys 
//Br 5.180, 5.190. 5.200 & 5.220 All users access must be removed if the provider is made inactive or electronic physio billing & reporting is now set to No.
idw_dw[1].AcceptText()

ll_provider_no = idw_dw[1].GetItemNumber(1,'provider_no')
is_provider_type_code = idw_dw[1].GetItemString(1,'provider_type_code')

ldws_active_status = idw_dw[1].GetItemStatus(1,"active_flag",PRIMARY!)
ldws_ephysio_status = idw_dw[1].GetItemStatus(1,"ephysio_flag",PRIMARY!)

IF (ldws_active_status = DATAMODIFIED! OR ldws_active_status = NEWMODIFIED!) THEN
	lb_active_changed = TRUE
END IF

IF (ldws_ephysio_status = DATAMODIFIED! OR ldws_ephysio_status = NEWMODIFIED!) THEN
	lb_ephysio_changed = TRUE
END IF

IF (lb_active_changed OR lb_ephysio_changed) THEN //Only want this check if user has made changes!

	IF (idw_dw[1].Object.active_flag[1] = 'N' OR idw_dw[1].Object.ephysio_flag[1] = 'N') THEN //Query if change really wanted
		
		select count(*) into :ll_result_count from WIF_ENTITY where entity_type_code ='P' and  entity_no = :ll_provider_no ;
		SQLCA.nf_handle_error('w_service_provider','cb_print',"embedded SQL 1")			
		
		IF (idw_dw[1].Object.active_flag[1] = 'N') THEN //Provider Deacvtivated
									
			IF (ll_result_count > 0) THEN //5.210
				ls_title		= "Deactivate Provider Confirmation…"
				ls_message = "This will remove the provider’s " + &
                                        "Access to the WorkSafeNB Web Portal and the clinic will no longer be able to submit reports and invoices electronically. " + &
                                        "Are you sure you wish to continue?"
			END  IF			
		ELSEIF (idw_dw[1].Object.ephysio_flag[1] = 'N') THEN //Ephysio Deactivated
			IF (ll_result_count > 0) THEN //5.210
				ls_title		= "Electronic Billing Change  Confirmation …"			
				ls_message = "This will remove the clinic’s access to the " + &
									"WorkSafeNB Web Portal and the clinic will no longer be able to submit reports and invoices electronically.  "   + &
									"Are you sure you wish to continue?"
			END IF
		END IF
		
		IF (Trim(ls_message) <> "") THEN //Will have a message to display
			IF (MessageBox(ls_title,ls_message,QUESTION!,YESNO!,2) = 1) THEN //5.210, 5.230
			
				ib_blow_entity_members_away = TRUE //2014-09-30 David Worboys
				ll_return = 1
				
			ELSE //Reset the flags
				
				ll_return = -1
				
				IF (idw_dw[1].Object.active_flag[1] = 'N') THEN
					idw_dw[1].Object.active_flag[1] = 'Y'
					idw_dw[1].Object.active_end_date[1] = ldt_null
				END IF
				
				IF (idw_dw[1].Object.ephysio_flag[1] = 'N') THEN
					idw_dw[1].Object.ephysio_flag[1] = 'Y'
					idw_dw[1].Object.physio_contract_flag[1] = 'Y'
				END IF				
			END IF
		END IF
	END IF
END IF
//2014-08-22 David Worboys

RETURN ll_return

end function

on destructor;call n_pdc::destructor;/* Destroy all the objects that were created by this object
*/
	IF IsValid(inv_address) THEN
		Destroy(inv_address)
	END IF
end on

on n_service_provider.create
call super::create
end on

on n_service_provider.destroy
call super::destroy
end on

