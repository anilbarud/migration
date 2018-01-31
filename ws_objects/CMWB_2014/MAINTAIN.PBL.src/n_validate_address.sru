$PBExportHeader$n_validate_address.sru
forward
global type n_validate_address from nonvisualobject
end type
end forward

shared variables

end variables

global type n_validate_address from nonvisualobject
end type
global n_validate_address n_validate_address

type variables
Protected:
u_dwa	idw_address
end variables

forward prototypes
public subroutine nf_set_datawindow (ref u_dwa adw_address)
public function integer nf_change_item ()
public function integer nf_set_defaults ()
public function integer nf_set_unused_fields ()
public subroutine class_description ()
public function boolean nf_is_letter (string as_string)
public function integer nf_validate_canadian_pc (ref string as_postal_code)
public function integer nf_validate_noncanadian_pc (ref string as_postal_code)
public function integer nf_check_mandatory ()
public function integer nf_check_bus_rule ()
end prototypes

public subroutine nf_set_datawindow (ref u_dwa adw_address);/*************************************************************************
	Description:		This function registers the address datawindow
*************************************************************************/

idw_address = adw_address


/*************************************************************************
	Revision History:
	Developer:				Lorie Vaughan
	Date:						August 2, 1996
	Comment:					Created	
*************************************************************************/

end subroutine

public function integer nf_change_item ();STRING				ls_city, ls_find_expression, ls_prov_state_code
LONG					ll_row,	ll_child_row
DATAWINDOWCHILD	ldwc_child

ll_row = idw_address.GetRow()

CHOOSE CASE idw_address.GetColumnName()
	CASE "location_desc2"
		// Get a reference to the city drop down list
		IF idw_address.GetChild("location_desc2",ldwc_child) < 0 THEN
			MessageBox("Error","Could not reference list of city codes.  Please call the Help Desk.")
			Return -1
		END IF

		// If city exists in the table, lookup and set the corresponding 
		// prov/state code and country code
		ls_city = idw_address.GetText()
		IF NOT (ls_city = '' or IsNull(ls_city)) THEN
			ls_find_expression = 'location_desc2= "' + ls_city + '"'
			ll_child_row = ldwc_child.Find(ls_find_expression,1,ldwc_child.RowCount())
			IF ll_child_row > 0 THEN
				//	Set the city field to the location_desc1 value to show correctly on mailing labels, etc.
				idw_address.SetItem(ll_row,'city',ldwc_child.GetItemString(ll_child_row,'location_desc1'))
				idw_address.SetItem(ll_row,'prov_state_code',ldwc_child.GetItemString(ll_child_row,'prov_state_code'))
				idw_address.SetItem(ll_row,'country_code',ldwc_child.GetItemString(ll_child_row,'country_code'))
				// the location code needs to be set since it is used to change the default admin region in the claim create
				idw_address.SetItem(ll_row,'location_code', ldwc_child.GetItemString(ll_child_row, 'location_code'))
				idw_address.SetItem(ll_row,'location_type_code', 'M')
			ELSE
				// If a row wasn't found, then the value entered is not in NB, so set the values correctly.
				idw_address.SetItem(ll_row,'city',ls_city)
				idw_address.SetItem(ll_row,'location_code', '')
				idw_address.SetItem(ll_row,'location_type_code', '')
			END IF
		END IF

	CASE "prov_state_code"
		// Get a reference to the province/state drop down list
		IF idw_address.GetChild("prov_state_code",ldwc_child) < 0 THEN
			MessageBox("Error","Could not reference list of province/state codes.  Please call the Help Desk.")
			Return -1
		END IF

		// If the province state code exists in the table, lookup and set the corresponding country code
		ls_prov_state_code = idw_address.GetText()
		IF NOT (ls_prov_state_code = '' or IsNull(ls_prov_state_code)) THEN
			ls_find_expression = 'location_code="' + ls_prov_state_code + '"'
			ll_child_row = ldwc_child.Find(ls_find_expression,1,ldwc_child.RowCount())
			IF ll_child_row > 0 THEN
				idw_address.SetItem(ll_row,'country_code',ldwc_child.GetItemString(ll_child_row,'country_code'))
			ELSE
				MessageBox('Warning','Invalid Province/State entered.')
				RETURN -1
			END IF
		END IF
END CHOOSE

Return 0
end function

public function integer nf_set_defaults ();LONG  ll_row

	ll_row = idw_address.GetRow()
	IF ll_row > 0 THEN

/*   	set the default values for an insert to a table containing address data
*/
	   idw_address.SetItem(ll_row,'address_line2', ' ')
   	idw_address.SetItem(ll_row,'prov_state_code', 'NB')
	   idw_address.SetItem(ll_row,'country_code', 'CAN')
	END IF

Return 0
end function

public function integer nf_set_unused_fields ();LONG  ll_row

/*	determine which fields are null and fill them in with a default value
	at some point in time any of the following fields may be unused
*/
	ll_row = idw_address.GetRow()
	IF ll_row > 0 THEN
   	IF IsNull(idw_address.GetItemString(ll_row, 'address_line1')) OR idw_address.GetItemString(ll_row, 'address_line1') = '' THEN
      	idw_address.SetItem(ll_row, 'address_line1', ' ')
	   END IF
   	IF IsNull(idw_address.GetItemString(ll_row, 'address_line2'))  OR idw_address.GetItemString(ll_row, 'address_line2') = '' THEN
      	idw_address.SetItem(ll_row, 'address_line2', ' ')
	   END IF
   	IF IsNull(idw_address.GetItemString(ll_row, 'city'))  OR idw_address.GetItemString(ll_row, 'city') = '' THEN
      	idw_address.SetItem(ll_row, 'city', ' ')
	    END IF
   	IF IsNull(idw_address.GetItemString(ll_row, 'prov_state_code'))  OR idw_address.GetItemString(ll_row, 'prov_state_code') = '' THEN
      	idw_address.SetItem(ll_row, 'prov_state_code', ' ')
	    END IF
   	IF IsNull(idw_address.GetItemString(ll_row, 'country_code')) OR idw_address.GetItemString(ll_row, 'country_code') = '' THEN
      	idw_address.SetItem(ll_row, 'country_code', ' ')
	   END IF
   	IF IsNull(idw_address.GetItemString(ll_row, 'postal_code')) OR idw_address.GetItemString(ll_row, 'postal_code') = '' THEN
      	idw_address.SetItem(ll_row, 'postal_code', ' ')
	   END IF
   	IF IsNull(idw_address.GetItemString(ll_row, 'telephone_no')) OR idw_address.GetItemString(ll_row, 'telephone_no') = '' THEN
      	idw_address.SetItem(ll_row, 'telephone_no', ' ')
	   END IF
	END IF

Return 0
end function

public subroutine class_description ();/* This class is used to standardize the business rules for
	maintaining address information in the CLAIM database.

	To use, simply create this user object, register your 
	datawindow which contains the standard address fields 
	and call it's functions.

	The following fields are considered standard and are
	referenced/validated/defaulted in the methods of this 
	class:

	address_line1
	address_line2
	city
	prov_state_code
	country
	postal_code
	telephone_no

*/
end subroutine

public function boolean nf_is_letter (string as_string);Return (as_string >= 'A' AND as_string <= 'Z') OR (as_string >= 'a' AND as_string <= 'z')
end function

public function integer nf_validate_canadian_pc (ref string as_postal_code);/*	This function checks the postal code passed to ensure it is in appropriate format for Canada
*/
	IF NOT (IsNulL(as_postal_code) OR Trim(as_postal_code) = '') THEN
		IF Mid(as_postal_code,4,1) <> ' ' AND Len(as_postal_code) = 6 THEN
			as_postal_code = Left(as_postal_code,3) + ' ' + Right(as_postal_code,3)
		END IF
      IF Len(as_postal_code) <> 7 OR NOT (IsNumber(Mid(as_postal_code,2,1)) AND IsNumber(Mid(as_postal_code,5,1)) &
			AND IsNumber(Mid(as_postal_code,7,1)))&
     	   OR NOT (nf_is_letter(Mid(as_postal_code,1,1)) AND nf_is_letter(Mid(as_postal_code,3,1)) &
         AND nf_is_letter(Mid(as_postal_code,6,1))) OR NOT (Mid(as_postal_code,4,1) = ' ') THEN
	      MessageBox("Invalid Postal Code", "The postal code entered is not in the correct format. Please correct.")
			Return -1
		END IF
	END IF

	Return 0

end function

public function integer nf_validate_noncanadian_pc (ref string as_postal_code);/*	This function checks the postal code passed. If it is in a Canadian format but
   the country is not Canada, then the user is warned of such
*/

INTEGER li_rtn

IF NOT (IsNull(as_postal_code) OR Trim(as_postal_code) = '') THEN
	IF Mid(as_postal_code,4,1) = ' ' AND Len(as_postal_code) = 7 THEN
		IF (NOT IsNumber(Left(as_postal_code,1)) AND IsNumber(Mid(as_postal_code,2,1)) &
			AND NOT IsNumber(Mid(as_postal_code,3,1)) AND IsNumber(Mid(as_postal_code,5,1)) &
			AND NOT IsNumber(Mid(as_postal_code,6,1)) AND IsNumber(Mid(as_postal_code,7,1))) THEN
			MessageBox("Canadian Postal Code", "The postal code entered is in a Canadian format.",Exclamation!)
		END IF
	END IF
END IF

Return 0

end function

public function integer nf_check_mandatory ();LONG  ll_row

	ll_row = idw_address.GetRow()
	IF ll_row > 0 THEN

/*		Check that at least one address line has been entered
*/
   	IF (IsNull(idw_address.GetItemString(ll_row, 'address_line1')) OR Trim(idw_address.GetItemString(ll_row, 'address_line1')) = '') AND &
      	(IsNull(idw_address.GetItemString(ll_row, 'address_line2')) OR Trim(idw_address.GetItemString(ll_row, 'address_line2')) = '')THEN
	      MessageBox('Missing Address', 'At least one address line must be filled in.')
			idw_address.SetColumn('address_line1')
			idw_address.SetFocus()
   	   Return -1
	   END IF

/*		Check that the city has been entered
*/
		IF IsNull(idw_address.GetItemString(ll_row,'city')) OR Trim(idw_address.GetItemString(ll_row,'city')) = '' THEN
			MessageBox('Missing City', 'The city must be specified.')
			idw_address.SetColumn('city')
			idw_address.SetFocus()
			Return -1
		END IF

/*		Check that the country has been entered
*/
		IF IsNull(idw_address.GetItemString(ll_row,'country_code')) OR Trim(idw_address.GetItemString(ll_row,'country_code')) = '' THEN
			MessageBox('Missing Country', 'The country must be specified.')
			idw_address.SetColumn('country_code')
			idw_address.SetFocus()
			Return -1
		END IF
	END IF

Return 0

end function

public function integer nf_check_bus_rule ();INTEGER          li_dw_row, ll_row
STRING           ls_country, ls_country2, ls_province, ls_province2, ls_city, ls_find_expression
STRING			  ls_pc, ls_phone, ls_location_desc2, ls_cellphone, ls_pager, ls_fax
DATAWINDOWCHILD  idwc_city, idwc_province, idwc_country
boolean			  lb_individual, lb_provider
LONG             ll_pos

/*	Validate that the city/province/country go together
	also validate the phone number and postal code
*/

/*	need to get the child datawindow and the entered values
*/
	idw_address.GetChild("location_desc2",idwc_city)
	idw_address.GetChild("prov_state_code",idwc_province)
	idw_address.GetChild("country_code",idwc_country)

	ll_row 				= idw_address.GetRow()
	ls_location_desc2 = Trim(idw_address.GetItemString(ll_row, "location_desc2"))
	ls_city           = Trim(idw_address.GetItemString(ll_row, "city"))
	ls_province 		= Trim(idw_address.GetItemString(ll_row,"prov_state_code"))
	ls_country 			= Trim(idw_address.GetItemString(ll_row,"country_code"))
	
	IF IsNull(ls_location_desc2) THEN
		ls_location_desc2 = ' '
	END IF
	IF IsNull(ls_city) THEN
		ls_city = ' '
	END IF
	IF IsNull(ls_province) THEN
		ls_province = ' '
	END IF
	IF IsNull(ls_country) THEN
		ls_country = ' '
	END IF

	idw_address.SetItem(ll_row, 'city', ls_city)
	idw_address.SetItem(ll_row, 'prov_state_code', ls_province)
	idw_address.SetItem(ll_row, 'country_code', ls_country)
	ls_phone = 		Trim(idw_address.GetItemString(ll_row,'telephone_no'))
	
	if idw_address.classname() = 'dw_individual' Then
		ls_cellphone = Trim(idw_address.GetItemString(ll_row,'individual_cellphone_no'))
		ls_pager = 		Trim(idw_address.GetItemString(ll_row,'individual_pager_no'))
		lb_individual = true
	End if
	
	if idw_address.classname() = 'dw_service_provider' Then
		ls_fax = Trim(idw_address.GetItemString(ll_row,'fax_no'))
		lb_provider = TRUE
	End if
		
	IF IsNull(ls_phone) THEN
		ls_phone = ' '
	END IF	
	IF IsNull(ls_cellphone) THEN
		ls_cellphone = ' '
	END IF
	IF IsNull(ls_pager) THEN
		ls_pager = ' '
	END IF
	IF IsNull(ls_fax) THEN
		ls_fax = ' ' 
	END IF 

/*	determine if a prov_state_code must be entered
*/	
	ls_find_expression = ' country_code = "' + Trim(ls_country) + '"'
  	li_dw_row = idwc_province.Find(ls_find_expression, 1, idwc_province.RowCount())
	IF li_dw_row > 0 AND Trim(ls_province) = '' THEN
		SELECT location_desc2
		INTO   :ls_country2 
		FROM   Location
		WHERE  location_code=:ls_country;
		IF SQLCA.nf_handle_error('Embedded SQL: Location', 'n_validate_address','nf_check_bus_rule') < 0 THEN
			Return -1
		ELSE
			MessageBox('Missing Province/State Code', 'A province/state code must be entered for ' + ls_country2 + '. Please correct.')
			idw_address.SetColumn('prov_state_code')
			idw_address.SetFocus()
			Return -1
		END IF
	ELSEIF li_dw_row <= 0 AND Trim(ls_province) <> '' THEN
		SELECT location_desc2
		INTO   :ls_country2 
		FROM   Location
		WHERE  location_code=:ls_country;
		IF SQLCA.nf_handle_error('Embedded SQL: Location', 'n_validate_address','nf_check_bus_rule') < 0 THEN
			Return -1
		ELSE
			MessageBox('Invalid Province/State Code', 'There is no valid province/state code for ' + ls_country2 + '. The field must be blank or Call Help Desk to have prov/state entered.')
			idw_address.SetColumn('prov_state_code')
			idw_address.SetFocus()
			Return -1
		END IF
	END IF

	IF ls_country = 'CAN' THEN
	   IF ls_province = 'NB' THEN
/*   		Validate city (use location_desc2 to get unique value) - it must exist in table
*/
   	   ls_find_expression = 'location_desc2 = "' + ls_location_desc2 + '" AND prov_state_code = "NB" AND country_code = "CAN"' 
      	li_dw_row = idwc_city.Find(ls_find_expression, 1, idwc_city.RowCount())
	      IF li_dw_row = 0 THEN
   	      MessageBox("Invalid City", "The city entered is not currently valid in NB.  Please correct.")
				idw_address.SetColumn('location_desc2')
				idw_address.SetFocus()
				Return -1
	      ELSEIF li_dw_row < 0 THEN
/*			error - trigger a application error
*/
				Error.Text = 'Find on city, search: ' + ls_find_expression
				Error.WindowMenu="cmwb"
				Error.Object="n_validate_address"
				Error.ObjectEvent="nf_check_bus_rule"
				SignalError()

	      ELSE
   	      idw_address.SetItem(ll_row,'location_code',idwc_city.GetItemString(li_dw_row,'location_code'))
      	   idw_address.SetItem(ll_row,'location_type_code', 'M')
	      END IF

/*			Validate the area code in the phone number
*/
			IF NOT Trim(ls_phone) = '' THEN
         	IF Left(ls_phone,3) <> '506' THEN
            	IF MessageBox('Phone Number Warning','The area code for New Brunswick is 506.  Continue save with area code ' + String(Left(ls_phone,3)) + '?', Question!, YesNo!) = 2 THEN
						idw_address.SetColumn('telephone_no')
						idw_address.SetFocus()
	   	         Return -1
					END IF
   	      END IF
      	END IF
	   ELSE

/*			Validate province - non NB
*/
			IF IsNull(ls_province) OR ls_province = "" THEN
         	MessageBox("Missing Province", "The province must be filled in when the country is Canada. Please correct.")
				idw_address.SetColumn('prov_state_code')
				idw_address.SetFocus()
	         Return -1
   	   END IF
      	ls_find_expression = 'location_code = "' + ls_province + '" AND country_code = "CAN"'
	      li_dw_row = idwc_province.Find(ls_find_expression, 1, idwc_province.RowCount())
   	   IF li_dw_row = 0 THEN
      	   MessageBox("Invalid Province", "The province entered is not currently valid in Canada.  Please correct.")
				idw_address.SetColumn('prov_state_code')
				idw_address.SetFocus()
         	Return - 1
	      ELSEIF li_dw_row < 0 THEN
/*			error - trigger a application error
*/
				Error.Text = 'Find on province, search: ' + ls_find_expression
				Error.WindowMenu="cmwb"
				Error.Object="n_validate_address"
				Error.ObjectEvent="nf_check_bus_rule"
				SignalError()
	      ELSE
   	      idw_address.SetItem(ll_row,'location_code',ls_province)
      	   idw_address.SetItem(ll_row,'location_type_code', 'P')
/*			Ensure that there is data entered into the city column
*/		
				IF Len(Trim(ls_location_desc2)) = 0 THEN
					SELECT location_desc2
					INTO   :ls_province2 
					FROM   Location
					WHERE  location_code=:ls_province;
					IF SQLCA.nf_handle_error('Embedded SQL: Location', 'n_validate_address','nf_check_bus_rule') < 0 THEN
						Return -1
					ELSE
						MessageBox("Missing City Data","A city is required for " + ls_province2 + ". Please correct.")
						idw_address.SetColumn('location_desc2')
						idw_address.SetFocus()
	   				Return -1
					END IF
				END IF
	      END IF
			
	   END IF

/*		If a phone number is entered, ensure that the appropriate number of characters have been entered
*/
   	IF NOT Trim(ls_phone) = '' THEN			
			ll_pos = Pos(ls_phone, ' ', 1)			
      	IF Len(Trim(ls_phone)) <> 10 OR ll_pos > 0 THEN
        	   MessageBox('Invalid Phone Number','The complete phone number has not been entered. Please correct.')
				idw_address.SetColumn('telephone_no')
				idw_address.SetFocus()
           	Return -1
	   	END IF
   	END IF
		
		/*		If a cellphone number is entered, ensure that the appropriate number of characters have been entered
*/
		IF lb_individual Then
			IF NOT Trim(ls_cellphone) = '' THEN
				ll_pos = Pos(ls_cellphone, ' ', 1)
				IF Len(Trim(ls_cellphone)) <> 10 OR ll_pos > 0 THEN
					MessageBox('Invalid CellPhone Number','The complete cellphone number has not been entered. Please correct.')
					idw_address.SetColumn('individual_cellphone_no')
					idw_address.SetFocus()
					Return -1
				END IF
			END IF
		END IF
		
/*		If a pager number is entered, ensure that the appropriate number of characters have been entered
*/
		IF lb_individual THen
			IF NOT Trim(ls_pager) = '' THEN
				ll_pos = Pos(ls_pager, ' ', 1)
				IF Len(Trim(ls_pager)) <> 10 OR ll_pos > 0 THEN
					MessageBox('Invalid Pager Number','The complete pager number has not been entered. Please correct.')
					idw_address.SetColumn('individual_pager_no')
					idw_address.SetFocus()
					Return -1
				END IF
			END IF
		END If

/*		If a fax number is entered, ensure that the appropriate number of characters have been entered
*/
		IF lb_provider THEN
			IF NOT Trim(ls_fax) = '' THEN
				ll_pos = Pos(ls_fax, ' ', 1)
				IF Len(Trim(ls_fax)) <> 10 OR ll_pos > 0 THEN
					MessageBox('Invalid Fax Number','The complete fax number has not been entered. Please correct.')
					idw_address.SetColumn('fax_no')
					idw_address.SetFocus()
					Return -1
				END IF
			END IF
		END If


/*		Validate the postal code
*/
   	ls_pc = idw_address.GetItemString(ll_row,'postal_code')
		IF nf_validate_canadian_pc(ls_pc) < 0 THEN
			idw_address.SetColumn('postal_code')
			idw_address.SetFocus()
			Return -1
		END IF
		idw_address.SetItem(ll_row,'postal_code', ls_pc)

	ELSE
		IF ls_country = 'USA' THEN

/*			Validate state
*/
			ls_find_expression = 'location_code = "' + ls_province + '" AND country_code = "USA"'
	      li_dw_row = idwc_province.Find(ls_find_expression, 1, idwc_province.RowCount())
   	   IF li_dw_row = 0 THEN
      	   MessageBox("Invalid State", "The state entered is not currently valid in the USA.  Please correct.")
				idw_address.SetColumn('prov_state_code')
				idw_address.SetFocus()
         	Return - 1
	      ELSEIF li_dw_row < 0 THEN
/*			error - trigger a application error
*/
				Error.Text = 'Find on state, search: ' + ls_find_expression
				Error.WindowMenu="cmwb"
				Error.Object="n_validate_address"
				Error.ObjectEvent="nf_check_bus_rule"
				SignalError()
	      END IF
   	   idw_address.SetItem(ll_row,'location_code',ls_province)
      	idw_address.SetItem(ll_row,'location_type_code', 'P')

/*			Ensure that there is data entered into the city column
*/			IF Len(Trim(ls_location_desc2)) = 0 THEN
				SELECT location_desc2
				INTO   :ls_province2 
				FROM   Location
				WHERE  location_code=:ls_province;
				IF SQLCA.nf_handle_error('Embedded SQL: Location', 'n_validate_address','nf_check_bus_rule') < 0 THEN
					Return -1
				ELSE
					MessageBox("Missing City Data","A city is required for " + ls_province2 + ". Please correct. ")
					idw_address.SetColumn('location_desc2')
					idw_address.SetFocus()
	   	   	Return -1
				END IF
			END IF
   	ELSE

/*			If a phone number is entered, ensure that the appropriate number of characters have been entered
*/ 
	      IF NOT Trim(ls_phone) = '' THEN
				ll_pos = Pos(ls_phone, ' ', 1)	
   	      IF Len(Trim(ls_phone)) <> 10 OR ll_pos > 0 THEN
      	      MessageBox('Invalid Phone Number','The complete phone number has not been entered. Please correct.')
					idw_address.SetColumn('telephone_no')
					idw_address.SetFocus()
         	   Return -1
	         END IF
      	END IF
			

/*  		Validate country
*/
  			li_dw_row = idwc_country.Find('location_code ="' + ls_country + '"',1,idwc_country.RowCount())
	      IF li_dw_row = 0 THEN
		      MessageBox("Invalid Country", "Unrecognizable country.  Please correct.")
				idw_address.SetColumn('country_code')
				idw_address.SetFocus()
	   	   Return -1
	      ELSEIF li_dw_row < 0 THEN
/*			error - trigger a application error
*/
				Error.Text = 'Find on country, search: ' + ls_find_expression
				Error.WindowMenu="cmwb"
				Error.Object="n_validate_address"
				Error.ObjectEvent="nf_check_bus_rule"
				SignalError()
	      ELSE
   	      idw_address.SetItem(ll_row,'location_code',idwc_country.GetItemString(li_dw_row,'location_code'))
      	   idw_address.SetItem(ll_row,'location_type_code', 'C')
	      END IF
		END IF
		
/*			Ensure that there is data entered into the city column
*/		
		IF Len(Trim(ls_location_desc2)) = 0 THEN
			SELECT location_desc2
			INTO   :ls_country2 
			FROM   Location
			WHERE  location_code=:ls_country;
			IF SQLCA.nf_handle_error('Embedded SQL: Location', 'n_validate_address','nf_check_bus_rule') < 0 THEN
				Return -1
			ELSE
				MessageBox("Missing City Data","A city is required for " + ls_country2 + ". Please correct. ")
				idw_address.SetColumn('location_desc2')
				idw_address.SetFocus()
	  			Return -1
			END IF
		ELSE 
/*		If postal code is empty, set it to a blank. If it is not check format. If Canadian format, warn 
		user: Canadian postal code for non-Canadian country
*/			
			ls_pc = Trim(idw_address.GetItemString(ll_row,'postal_code'))
			IF IsNull(ls_pc) OR ls_pc = '' THEN
				ls_pc = ' '
				idw_address.SetItem(ll_row,'postal_code', ls_pc)
			ELSE
				nf_validate_noncanadian_pc(ls_pc)
			END IF
		END IF   		
	END IF

/*	If address line 1 is empty bump up address line 2
*/
	idw_address.SetItem(ll_row, 'address_line1', Trim(idw_address.GetItemString(ll_row, 'address_line1')))
	IF idw_address.GetItemString(ll_row, 'address_line2') > ' ' THEN
		idw_address.SetItem(ll_row, 'address_line2', Trim(idw_address.GetItemString(ll_row, 'address_line2')))
	END IF
	IF IsNull(idw_address.GetItemString(ll_row, 'address_line1')) OR Trim(idw_address.GetItemString(ll_row, 'address_line1')) = '' THEN
   	idw_address.SetItem(ll_row,'address_line1', idw_address.GetItemString(ll_row, 'address_line2'))
	   idw_address.SetItem(ll_row,'address_line2', ' ')
	END IF
	IF IsNull(idw_address.GetItemString(ll_row, 'address_line2')) THEN
	   idw_address.SetItem(ll_row,'address_line2', ' ')
	END IF		
Return 0

end function

on n_validate_address.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_validate_address.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

