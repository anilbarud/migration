$PBExportHeader$n_user_authorizations.sru
forward
global type n_user_authorizations from nonvisualobject
end type
end forward

global type n_user_authorizations from nonvisualobject
end type
global n_user_authorizations n_user_authorizations

type variables
Private	u_ds		ids_authorizations
Private 	u_ds		ids_payment_combination
Private  u_ds     ids_module_authorizations

Private STRING	is_user_id


Private BOOLEAN	ib_authorizations_loaded
Private BOOLEAN	ib_user_has_authorizations
end variables

forward prototypes
public function integer nf_set_user_id (string as_user_id)
public function boolean nf_user_can_authorize (string as_claim_admin_region, string as_payment_type, decimal ad_payment_amount)
public function boolean nf_authorizations_exist (string as_admin_region_code, string as_authorization_type_code)
public function boolean nf_authorizations_exist (string as_admin_region)
public function decimal nf_get_authorization_limit (string as_admin_region, string as_authorization_type)
public function boolean nf_authorizations_exist ()
private function long nf_find_authorization (string as_admin_region, string as_authorization_type)
private function long nf_load_authorizations ()
public function string nf_get_authorization_type (string as_payment_type)
public function boolean nf_user_has_module_authorizations (string as_admin_region, string as_module_code)
public function integer nf_get_module_authorizations (string as_module, ref string as_authorization[])
public function integer nf_reminder_authorization ()
end prototypes

public function integer nf_set_user_id (string as_user_id);//The user id must be 1 - 16 characters

IF as_user_id = '' Then
	SignalError(-666,'The user id cannot be blank.')
End if

If Len(as_user_id) > 16 Then
	SignalError(-666,'The user id cannot be more than 16 characters.  "' + as_user_id + '" is invalid.')
End if

is_user_id = as_user_id

//load the authorizations
nf_load_authorizations()

return 1
end function

public function boolean nf_user_can_authorize (string as_claim_admin_region, string as_payment_type, decimal ad_payment_amount);//Returns TRUE if the user is allowed to authorize the current payment based on 
// the supplied arguments


decimal		ld_authorization_limit
BOOLEAN		lb_can_authorize

ld_authorization_limit = nf_get_authorization_limit(as_claim_admin_region,as_payment_type)

If isNull(ld_authorization_limit) Then
	lb_can_authorize = False
	
Elseif ld_authorization_limit >= ad_payment_amount Then
	lb_can_authorize = True
End if

return lb_can_authorize
end function

public function boolean nf_authorizations_exist (string as_admin_region_code, string as_authorization_type_code);LONG		ll_found_row

ll_found_row = nf_find_authorization(as_admin_region_code,as_authorization_type_code)

if ll_found_row <> 0 then
	RETURN TRUE
END IF

RETURN FALSE
end function

public function boolean nf_authorizations_exist (string as_admin_region);RETURN nf_authorizations_exist(as_admin_region,'')
end function

public function decimal nf_get_authorization_limit (string as_admin_region, string as_authorization_type);//*******************************************************************************************
//Returns the authorization limit for the first record that matches the specified criteria
// empty string are accepted, records are not sorted in any particular order
//*******************************************************************************************
//RETURN NULL - if no authorization record found
//			authorization_limit - if one is found
//*******************************************************************************************
LONG			ll_found_auth_row
DECIMAL		ld_authorization_limit


ll_found_auth_row = nf_find_authorization(as_admin_region, as_authorization_type)

If ll_found_auth_row = 0 Then
	SetNull(ld_authorization_limit)

Elseif ll_found_auth_row > 0 Then
	ld_authorization_limit = ids_authorizations.GetItemDecimal(ll_found_auth_row,"authorization_limit")
Else
	SignalError(-666,'Something went wrong')
End if


RETURN ld_authorization_limit
end function

public function boolean nf_authorizations_exist ();//Returns true if the user has at least one authorization record
//reguardless of region or type.

RETURN nf_authorizations_exist('','')
end function

private function long nf_find_authorization (string as_admin_region, string as_authorization_type);//Returns the first row found that meets the criteria

STRING		ls_admin_region_expr
STRING		ls_authorization_type_expr
LONG			ll_found_auth_row


//If no criteria is specified, just make sure there is at least one record
If as_admin_region = '' and as_authorization_type = '' THen
	If ib_user_has_authorizations Then
		RETURN 1
	ELSE
		RETURN 0
	END IF
END IF


If as_admin_region <> '' Then
	ls_admin_region_expr = "admin_region_code = '" + as_admin_region + "'"
End if

if as_authorization_type <> '' Then
	If ls_admin_region_expr <> '' Then
		ls_authorization_type_expr = ' and '
	End if
	ls_authorization_type_expr += "authorization_type_code = '" + as_authorization_type + "'"
End if

ll_found_auth_row = ids_authorizations.Find(ls_admin_region_expr + ls_authorization_type_expr,1,ids_authorizations.RowCount())

If ll_found_auth_row < 0 Then
	SignalError(-666,'Error finding authorizations')
End if

RETURN ll_found_auth_row

end function

private function long nf_load_authorizations ();LONG			ll_rows


//This boolean will let other functions know if this function has been called yet.
ib_authorizations_loaded = True

ll_rows = ids_authorizations.Retrieve(is_user_id)
If ll_rows = -1 Then SignalError(-666,'Error retrieving authorizations')

SQLCA.nf_handle_error('n_user_authorizations','nf_load_authorizations','Retrieve')

//This boolean will report whether the user has any authorizations
// Some modules are inaccessible if the user has no authorizations.
If ll_rows = 0 Then
	ib_user_has_authorizations = False
Else
	ib_user_has_authorizations = True	
End if


return ll_rows
end function

public function string nf_get_authorization_type (string as_payment_type);LONG		ll_found_row
STRING	ls_authorization_type


if as_payment_type = '' THen
	SignalError(-666,'Invalid payment type " "')
End if

ll_found_row = ids_payment_combination.Find("payment_type_code = '" + as_payment_type + "'",1,ids_payment_combination.RowCOunt())
If ll_found_row <= 0 Then 
	SignalError(-666,'Error finding authorization_type_code')
Else
	ls_authorization_type = ids_payment_combination.GetItemString(ll_found_row,"authorization_type_code")
	If ls_authorization_type = '' Then
		SignalError(-666,'Invalid authorization_type_code found')
	End if
End if

RETURN ls_authorization_type
end function

public function boolean nf_user_has_module_authorizations (string as_admin_region, string as_module_code);BOOLEAN	lb_answer
INTEGER	li_rtn
STRING	ls_module_authorizations[]
INTEGER	li_x


//Make sure the user has authorizations to do something in this module
li_rtn = nf_get_module_authorizations(as_module_code,ls_module_authorizations)
If li_rtn = 0 Then RETURN FALSE

For li_x = 1 To UpperBound(ls_module_authorizations)
	lb_answer = nf_authorizations_exist(as_admin_region,ls_module_authorizations[li_x])
	If lb_answer = True Then
		exit
	End if
Next

RETURN lb_answer
end function

public function integer nf_get_module_authorizations (string as_module, ref string as_authorization[]);/*return the number of authorizations
*/

LONG			ll_module_count
STRING		ls_zero_array[]
INTEGER		li_rtn
LONG			ll_rows
INTEGER		li_x

as_authorization = ls_zero_array

li_rtn = ids_module_authorizations.SetFilter("module_code = '" + as_module + "'")
If li_rtn = -1 Then 	SignalError(-666,"Error setting filter for module authorizations")

li_rtn = ids_module_authorizations.Filter()
If li_rtn = -1 Then 	SignalError(-666,"Error setting filter for module authorizations")

ll_rows = ids_module_authorizations.RowCount()
If ll_rows = 0 Then 
	Select count(*) into :ll_module_count from Module where module_code = :as_module USING SQLCA;
	SQLCA.nf_handle_error('n_user_authorizations','nf_get_module_authorizations','')
	
	If ll_module_count = 0 Then SignalError(-666,'Module code ' + as_module + " doesn't exists.")
	
	IF ll_module_count > 0 Then 
		MessageBox('Authorizations error','Module has no payments associated with it.')
		RETURN 0
	END IF
ELSE
	For li_x = 1 to ll_rows
		as_authorization[li_x] = ids_module_authorizations.GetItemString(li_x,"authorization_type_code")
	Next
End if

return 1
end function

public function integer nf_reminder_authorization ();// CPP Disability reminders can only by created by a case manager or regional director with an authorization greater than zero. 
Long ll_count


IF vgst_user_profile.position_code = 'REGDIR' OR vgst_user_profile.position_code = 'CASMGR' THEN

	Select  count(*)
	Into     :ll_count
	From   Authorizations
	Where authorized_by_login_id = :vgst_user_profile.user_id
	And     admin_region_code = :vgst_user_profile.default_admin_region_code
	And     authorization_limit > 0.0000
	And     authorization_type_code = 'loe'
	Using SQLCA;

	SQLCA.nf_handle_error('ERROR','n_user_authorization','nf_reminder_authorization')

	IF ll_count > 0 THEN 
		Return 0
	ELSE
		Return -1
	END IF
ELSE
	Return -1
END IF	
end function

event constructor;LONG		ll_rows

ids_authorizations = CREATE u_ds
ids_payment_combination = CREATE u_ds
ids_module_authorizations = CREATE u_ds

ids_authorizations.DataObject = 'd_user_authorizations'
ids_payment_combination.DataObject = 'd_payment_combination_grouped'
ids_module_authorizations.DataObject = 'd_module_authorizations'

ids_authorizations.SetTransObject(Sqlca)
ids_payment_combination.SetTransObject(Sqlca)
ids_module_authorizations.SetTransObject(Sqlca)

ll_rows = ids_payment_combination.Retrieve()
SQLCA.nf_handle_error('n_user_authorizations','constructor','Retrieve')

IF ll_rows = 0 Then
	SignalError(-666,'Error retrieving payment type / authorization type xref information.')
end if

ll_rows = ids_module_authorizations.Retrieve()
SQLCA.nf_handle_error('n_user_authorizations','constructor','Retrieve')

IF ll_rows = 0 Then
	SignalError(-666,'Error retrieving module / authorization information.')
end if
end event

on n_user_authorizations.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_user_authorizations.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

