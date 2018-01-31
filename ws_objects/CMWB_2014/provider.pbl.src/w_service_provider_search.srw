$PBExportHeader$w_service_provider_search.srw
forward
global type w_service_provider_search from w_ancestor
end type
type uo_search from u_service_provider_search within w_service_provider_search
end type
type cb_ok from commandbutton within w_service_provider_search
end type
type cb_cancel from commandbutton within w_service_provider_search
end type
type idw_provider_maintenance from u_dwa within w_service_provider_search
end type
end forward

global type w_service_provider_search from w_ancestor
integer x = 960
integer y = 552
integer width = 3214
integer height = 2620
string title = "Service Provider Search"
string menuname = ""
boolean controlmenu = false
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
long backcolor = 67108864
uo_search uo_search
cb_ok cb_ok
cb_cancel cb_cancel
idw_provider_maintenance idw_provider_maintenance
end type
global w_service_provider_search w_service_provider_search

type variables
LONG il_provider_no 
STRING is_provider_type_code
end variables

forward prototypes
public subroutine wf_set_provider_no (long al_provider_no, string al_provider_type_code)
public subroutine wf_clear_identifier ()
end prototypes

public subroutine wf_set_provider_no (long al_provider_no, string al_provider_type_code);// wf_set_provider_no - If a provider number was found, retrieve the data into the bottom dw 
//                      Otherwise, reset the bottom dw.
//
Long    ll_count, ll_child_row, ll_row
Integer li_rtn
STRING ls_location_code, ls_location_type_code, ls_find_expression
datawindowchild  ldwc_child


il_provider_no = al_provider_no
is_provider_type_code = al_provider_type_code
IF il_provider_no = 0 THEN 
	idw_provider_maintenance.Reset()
	
	cb_ok.Enabled = FALSE
ELSE
	idw_provider_maintenance.Retrieve(il_provider_no, is_provider_type_code)
	idw_provider_maintenance.Modify("name.protect=1 name.Background.Color='553648127'")
	idw_provider_maintenance.Modify("sort_name.protect=1 sort_name.Background.Color='553648127'")
	idw_provider_maintenance.Modify("contact_name.protect=1 contact_name.Background.Color='553648127'")
	idw_provider_maintenance.Modify("active_start_date.protect=1 active_start_date.Background.Color='553648127'")
	idw_provider_maintenance.Modify("inactive_reason_desc.protect=1 inactive_reason_desc.Background.Color='553648127'")
	idw_provider_maintenance.Modify("provider_inactive_reason_code.protect=1 provider_inactive_reason_code.Background.Color='553648127'")
	idw_provider_maintenance.Modify("provider_type_code.protect=1 provider_type_code.Background.Color='553648127'")
	idw_provider_maintenance.Modify("provider_sub_type_code.protect=1 provider_sub_type_code.Background.Color='553648127'")
	idw_provider_maintenance.Modify("address_line1.protect=1 address_line1.Background.Color='553648127'")
	idw_provider_maintenance.Modify("address_line2.protect=1 address_line2.Background.Color='553648127'")
	idw_provider_maintenance.Modify("location_desc2.protect=1 location_desc2.Background.Color='553648127'")
	idw_provider_maintenance.Modify("prov_state_code.protect=1 prov_state_code.Background.Color='553648127'")
	idw_provider_maintenance.Modify("country_code.protect=1 country_code.Background.Color='553648127'")
	idw_provider_maintenance.Modify("postal_code.protect=1 postal_code.Background.Color='553648127'")
	idw_provider_maintenance.Modify("telephone_no.protect=1 telephone_no.Background.Color='553648127'")
	idw_provider_maintenance.Modify("cellphone_no.protect=1 cellphone_no.Background.Color='553648127'")
	idw_provider_maintenance.Modify("fax_no.protect=1 fax_no.Background.Color='553648127'")
	idw_provider_maintenance.Modify("hours_of_operation.protect=1 hours_of_operation.Background.Color='553648127'")
	idw_provider_maintenance.Modify("email_address.protect=1 email_address.Background.Color='553648127'")
	idw_provider_maintenance.Modify("active_flag.protect=1 active_flag.Background.Color='553648127'")
	idw_provider_maintenance.Modify("nbms_early_filing_bonus_flag.protect=1 nbms_early_filing_bonus_flag.Background.Color='553648127'")
	idw_provider_maintenance.Modify("chiro_early_filing_bonus_flag.protect=1 chiro_early_filing_bonus_flag.Background.Color='553648127'")	
	idw_provider_maintenance.Modify("cadre_flag.protect=1 cadre_flag.Background.Color='553648127'")
	idw_provider_maintenance.Modify("ephysio_flag.protect=1 ephysio_flag.Background.Color='553648127'")	
	idw_provider_maintenance.Modify("physio_contract_flag.protect=1 physio_contract_flag.Background.Color='553648127'")		
	idw_provider_maintenance.Modify("service_offered_language_code.protect=1 service_offered_language_code.Background.Color='553648127'")	
	idw_provider_maintenance.Modify("preferred_correspond_language_code.protect=1 preferred_correspond_language_code.Background.Color='553648127'")	
	cb_ok.Enabled = TRUE
	
	/* PR - 25163 Set the city (location_desc2) field. In w_service_provider, this is done via a call to n_service_prover.nf_retrieve(),
	                    but in this pared-down service provider search window, we dont have a reference to that object , 
	                    so we'll just use the same piece of code to populate the display value in the 'city'  field
	*/
	
	ls_location_code = idw_provider_maintenance.GetItemString(ll_row,'location_code')
	ls_location_type_code = idw_provider_maintenance.GetItemString(ll_row,'location_type_code')
	ll_row = idw_provider_maintenance.getRow()
	IF ls_location_type_code = 'M' THEN
	/*	Get a reference to the city drop down list
	*/
		IF idw_provider_maintenance.GetChild("location_desc2",ldwc_child) < 0 THEN
			MessageBox("Error","Could not reference list of city codes.  Please call the Help Desk.")
			Return 
		END IF
		ls_find_expression = 'location_code= "' + ls_location_code + '"'
		ll_child_row = ldwc_child.Find(ls_find_expression,1,ldwc_child.RowCount())
		
		IF ll_child_row > 0 THEN
			idw_provider_maintenance.SetItem(ll_row,'location_desc2',ldwc_child.GetItemString(ll_child_row,"location_desc2"))
		ELSE
			idw_provider_maintenance.SetItem(ll_row,'location_desc2',idw_provider_maintenance.GetItemString(ll_row,'city'))
		END IF
	ELSE
		idw_provider_maintenance.SetItem(ll_row,'location_desc2',idw_provider_maintenance.GetItemString(ll_row,'city'))
	END IF	
END IF


end subroutine

public subroutine wf_clear_identifier ();/*	This function clears the current service provider.
*/
	idw_provider_maintenance.Reset()
end subroutine

event open;call super::open;
uo_search.is_passed_type_code = Message.StringParm

uo_search.uf_set_parent(THIS)
idw_provider_maintenance.SetTransObject(SQLCA)

end event

on w_service_provider_search.create
int iCurrent
call super::create
this.uo_search=create uo_search
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
this.idw_provider_maintenance=create idw_provider_maintenance
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.uo_search
this.Control[iCurrent+2]=this.cb_ok
this.Control[iCurrent+3]=this.cb_cancel
this.Control[iCurrent+4]=this.idw_provider_maintenance
end on

on w_service_provider_search.destroy
call super::destroy
destroy(this.uo_search)
destroy(this.cb_ok)
destroy(this.cb_cancel)
destroy(this.idw_provider_maintenance)
end on

type uo_search from u_service_provider_search within w_service_provider_search
integer x = 5
integer y = 4
integer width = 3191
integer taborder = 10
end type

event ue_doubleclicked;call super::ue_doubleclicked;
cb_ok.TriggerEvent(Clicked!)

end event

on uo_search.destroy
call u_service_provider_search::destroy
end on

type cb_ok from commandbutton within w_service_provider_search
integer x = 1262
integer y = 2416
integer width = 274
integer height = 108
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&OK"
end type

on clicked;S_WINDOW_MESSAGE lstr_message

	lstr_message.al_doubleparm[1] = il_provider_no
	lstr_message.as_stringparm[1] = is_provider_type_code
	CloseWithReturn(PARENT,lstr_message)

end on

type cb_cancel from commandbutton within w_service_provider_search
integer x = 1541
integer y = 2416
integer width = 274
integer height = 108
integer taborder = 30
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cance&l"
end type

on clicked;S_WINDOW_MESSAGE lstr_message

	lstr_message.al_doubleparm[1] = 0
	lstr_message.as_stringparm[1] = ""
	CloseWithReturn(PARENT,lstr_message)

end on

type idw_provider_maintenance from u_dwa within w_service_provider_search
integer x = 27
integer y = 1048
integer width = 3154
integer height = 1348
integer taborder = 60
string dataobject = "d_provider"
borderstyle borderstyle = stylelowered!
end type

