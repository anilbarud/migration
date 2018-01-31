$PBExportHeader$u_recipient_search.sru
$PBExportComments$the user object for searching for providers
forward
global type u_recipient_search from userobject
end type
type cb_search from commandbutton within u_recipient_search
end type
type cb_clear from commandbutton within u_recipient_search
end type
type dw_list from u_dw_online within u_recipient_search
end type
type dw_search from u_dw_online within u_recipient_search
end type
end forward

global type u_recipient_search from userobject
integer width = 3200
integer height = 1180
long backcolor = 67108864
long tabtextcolor = 33554432
event ue_doubleclicked pbm_custom01
cb_search cb_search
cb_clear cb_clear
dw_list dw_list
dw_search dw_search
end type
global u_recipient_search u_recipient_search

type variables
STRING	is_where, is_select, is_name, is_city, is_type_code, is_sub_type, is_search_type, is_individual_select, is_provider_select
STRING     is_first, is_last  
LONG		il_recipient_no, il_search_type
w_maintain_bank_information	iwi_window_parent 
DATAWINDOW idw_dw[]
end variables

forward prototypes
public function integer uf_set_query ()
public function integer uf_retrieve ()
public function integer uf_set_parent (window awi_parent)
public subroutine uf_clear ()
public subroutine uf_init ()
public subroutine uf_clear_highlighting ()
public subroutine uf_set_recipient_no (integer al_row)
public function integer uf_set_search_values ()
public subroutine uf_switch_type (string as_type)
public subroutine uf_init (datawindow adw_dw)
public subroutine uf_search_recipient (long al_recipient_no, string as_recipient_type_code, string as_recipient_sub_type_code, string as_name, string as_city, long al_search)
end prototypes

public function integer uf_set_query ();STRING ls_new_select, ls_modstring, ls_return

ls_new_select = ''

CHOOSE CASE is_type_code
	CASE 'I'
		IF il_recipient_no > 0 THEN
			IF ls_new_select > '' THEN 
				ls_new_select = ls_new_select + '  AND '
			END IF
			ls_new_select = ls_new_select + "(individual_no = " + STRING(il_recipient_no) + ")"
		END IF 

		/* First Name */
		IF is_first > "" THEN
			IF ls_new_select > '' THEN 
				ls_new_select = ls_new_select + '  AND '
			END IF
			ls_new_select = ls_new_select + "(given_names LIKE '%" + is_first + "%')" 
		END IF
		
		/* Last Name */
		IF is_last > "" THEN
			IF ls_new_select > '' THEN 
				ls_new_select = ls_new_select + '  AND '
			END IF
			ls_new_select = ls_new_select + "(last_name LIKE '%" + is_last + "%')" 
		END IF

		/*City */
		IF is_city > "" THEN
			IF ls_new_select > '' THEN 
				ls_new_select = ls_new_select +  '  AND '
			END IF
			IF il_search_type = 1 THEN
				// Exact match
				ls_new_select = ls_new_select + "(city = '" + is_city + "')" 
			ELSE
				IF il_search_type = 2 THEN
					// Starts with
					ls_new_select = ls_new_select + "(city LIKE '" + is_city + "%')" 		
				ELSE
					// Contains
					ls_new_select = ls_new_select + "(city LIKE '%" + is_city + "%')" 
				END IF
			END IF
		END IF

CASE ELSE
	IF il_recipient_no > 0 THEN
		IF ls_new_select > '' THEN 
			ls_new_select = ls_new_select + '  AND '
		END IF
		ls_new_select = ls_new_select + "(PROVIDER.provider_no = " + STRING(il_recipient_no) + ")"
	END IF 
	
	/* Name */
	IF is_name > "" THEN
		IF ls_new_select > '' THEN 
			ls_new_select = ls_new_select + '  AND '
		END IF
		IF il_search_type = 1 THEN
			// Exact match
			ls_new_select = ls_new_select + "(PROVIDER.name = '" + is_name + "')" 
		ELSE
			IF il_search_type = 2 THEN
				// Starts with
				ls_new_select = ls_new_select + "(PROVIDER.name LIKE '" + is_name + "%')" 				
			ELSE
				// Contains
				ls_new_select = ls_new_select + "(PROVIDER.name LIKE '%" + is_name + "%')" 
			END IF
		END IF
	END IF
	
	/*City */
	IF is_city > "" THEN
		IF ls_new_select > '' THEN 
			ls_new_select = ls_new_select +  '  AND '
		END IF
		IF il_search_type = 1 THEN
			// Exact match
			ls_new_select = ls_new_select + "(PROVIDER.city = '" + is_city + "')" 
		ELSE
			IF il_search_type = 2 THEN
				// Starts with
				ls_new_select = ls_new_select + "(PROVIDER.city LIKE '" + is_city + "%')" 		
			ELSE
				// Contains
				ls_new_select = ls_new_select + "(PROVIDER.city LIKE '%" + is_city + "%')" 
			END IF
		END IF
	END IF
	
	IF is_type_code > '' THEN	
		IF NOT IsNull(is_type_code) THEN
			IF ls_new_select > '' THEN 
				ls_new_select = ls_new_select +  '  AND '
			END IF
			ls_new_select = ls_new_select +  "(PROVIDER.provider_type_code = '" + is_type_code  + "')" 				
		END IF		
		/*	Sub Type */
		IF is_sub_type <> '00' THEN
			IF ls_new_select > '' THEN 
				ls_new_select = ls_new_select +  '  AND '
			END IF
			ls_new_select = ls_new_select +  "(PROVIDER.provider_sub_type_code = '" + is_sub_type + "')" 	
		END IF
	END IF
END CHOOSE

/*	now create the new select statement using the original select and the original where 
	with the additional where just created added on
*/
IF ls_new_select > '' THEN
	ls_modstring = "DataWindow.Table.Select=~"" + is_select + '  WHERE ' + ls_new_select + "~""
	ls_return = dw_list.Modify(ls_modstring)
	IF Len(ls_return) > 0 THEN
		RETURN -1
	ELSE
		RETURN 0
	END IF
ELSE
	RETURN 0
END IF
end function

public function integer uf_retrieve ();LONG  ll_return

IF uf_set_search_values() < 0 THEN
	RETURN -1
END IF

uf_set_query()

dw_list.SetTransObject(SQLCA)
ll_return = dw_list.Retrieve(il_recipient_no) 

IF ll_return < 0 THEN
	MessageBox('Warning', 'No data found matching specified criteria.')
END IF

RETURN 0



end function

public function integer uf_set_parent (window awi_parent);
iwi_window_parent = awi_parent

uf_init()

RETURN 0
end function

public subroutine uf_clear ();DATAWINDOWCHILD	ldwc_child

/*	Clear the datawindows and insert a row for searching
*/
IF dw_search.GetChild('sub_type', ldwc_child) < 0 THEN
	MessageBox("Error","Error retrieving list of recipient sub type codes. Please call the help desk")
	RETURN 
END IF

dw_search.Reset()
dw_search.InsertRow(0)

ldwc_child.SetFilter("provider_type_code = ''")
ldwc_child.Filter()

dw_search.SetColumn('sub_type')
dw_search.SetText('')
dw_search.SetColumn('recipient_no')

dw_list.Reset()
idw_dw[1].Reset()
end subroutine

public subroutine uf_init ();DATAWINDOWCHILD  ldwc_child

dw_search.SetTransObject(SQLCA)
dw_search.InsertRow(0)

dw_search.GetChild('sub_type', ldwc_child)
ldwc_child.SetFilter("recipient_type_code = ''")
ldwc_child.Filter()

dw_search.SetFocus()

end subroutine

public subroutine uf_clear_highlighting ();/*	This function turns off the highlighting of the search list.
*/
	dw_list.SelectRow(0,False)
end subroutine

public subroutine uf_set_recipient_no (integer al_row);STRING ls_type_code, ls_sub_type_code, ls_name, ls_address, ls_city_postal, ls_address2

IF al_row = 0 THEN
	il_recipient_no = 0
	ls_type_code = ''
	ls_name = ''
ELSE
	il_recipient_no  = dw_list.GetItemNumber(al_row, 'recipient_no')
	ls_type_code    = dw_list.GetItemString(al_row, 'recipient_type_code')
	ls_name = dw_list.GetItemString(al_row, 'name')
	ls_address = dw_list.GetItemString(al_row,'address_line1') 
	ls_address2 = dw_list.GetItemString(al_row, 'address_line2')
	ls_city_postal = dw_list.GetItemString(al_row, 'city') + ', ' + dw_list.GetItemString(al_row,'prov_state_code') + '   ' + dw_list.GetItemString(al_row,'postal_code')

	IF ls_type_code = 'I' THEN 
		ls_sub_type_code = ''
	ELSE
		ls_sub_type_code = dw_list.GetItemString(al_row, 'recipient_sub_type_code')
	END IF
END IF

iwi_window_parent.wf_set_recipient_info(il_recipient_no, ls_type_code,ls_sub_type_code, ls_name, ls_address, ls_address2,ls_city_postal)

end subroutine

public function integer uf_set_search_values ();
dw_search.AcceptText()

is_type_code = dw_search.GetItemString(1,'recipient_type')
IF is_type_code = 'I' THEN
	is_first = TRIM(dw_search.GetItemString(1, 'first_name'))
	is_last = TRIM(dw_search.GetItemString(1, 'last_name'))
ELSE
	is_name = TRIM(dw_search.GetItemString(1, 'rec_name'))
	is_sub_type = TRIM(dw_search.GetItemString(1,'sub_type'))
END IF

is_city = TRIM(dw_search.GetItemString(1, 'city'))
il_recipient_no = dw_search.GetItemNumber(1,'recipient_no')
il_search_type = dw_search.GetItemNumber(1,'search_type')

IF (IsNull(is_name) OR is_name = '')  AND (IsNull(is_city) OR is_city = '') AND (IsNull(is_sub_type) OR is_sub_type = '') AND IsNull(il_recipient_no) AND (IsNull(is_type_code) OR is_type_code = '') THEN
	MessageBox('Insufficient Search Criteria', 'At least one of the following must be specified before a retrieval can be performed: Recipient No., Recipient Type, Recipient Sub Type, Recipient Name, or City')
	Return -1
END IF

IF NOT IsNull(is_sub_type) AND IsNull(is_type_code) THEN	
	MessageBox('Search Error', 'If Recipient Sub Type is provided then the Provider Type must also be specified.')
	RETURN -1
END IF

dw_list.DataObject = 'd_bi_provider_list'

is_select = dw_list.Describe("DataWindow.Table.Select")

RETURN 0
end function

public subroutine uf_switch_type (string as_type);IF as_type = 'I' THEN
	dw_search.Object.t_first_name.Visible = TRUE
	dw_search.Object.t_last_name.Visible = TRUE
	dw_search.Object.first_name.Visible = TRUE
	dw_search.Object.last_name.Visible = TRUE
	dw_search.Object.t_name.Visible = FALSE
	dw_search.Object.t_sub_type.Visible = FALSE
	dw_search.Object.sub_type.Visible = FALSE
	dw_search.Object.rec_name.Visible = FALSE
	dw_search.Object.search_type.Visible = FALSE
ELSE
	dw_search.Object.t_first_name.Visible = FALSE
	dw_search.Object.t_last_name.Visible = FALSE
	dw_search.Object.first_name.Visible = FALSE
	dw_search.Object.last_name.Visible = FALSE
	dw_search.Object.t_name.Visible = TRUE
	dw_search.Object.t_sub_type.Visible = TRUE
	dw_search.Object.sub_type.Visible = TRUE
	dw_search.Object.rec_name.Visible = TRUE
	dw_search.Object.search_type.Visible = TRUE
END IF
end subroutine

public subroutine uf_init (datawindow adw_dw);
idw_dw[1] = adw_dw
end subroutine

public subroutine uf_search_recipient (long al_recipient_no, string as_recipient_type_code, string as_recipient_sub_type_code, string as_name, string as_city, long al_search);DATAWINDOWCHILD ldwc_child
DWObject l_dwo
STRING ls_filter

dw_search.SetItem(1, 'recipient_no',al_recipient_no)
dw_search.SetItem(1, 'recipient_type', as_recipient_type_code)

dw_search.GetChild('sub_type', ldwc_child)
ldwc_child.SetTransObject(SQLCA)
ldwc_child.Retrieve(as_recipient_type_code)

dw_search.SetColumn('recipient_type')
l_dwo = dw_search.Object.recipient_type
dw_search.Event ItemChanged(1,l_dwo,as_recipient_type_code)
dw_search.SetItem(1,'sub_type', as_recipient_sub_type_code)

dw_search.SetItem(1,'rec_name', as_name)
dw_search.SetItem(1,'city', as_city)
dw_search.SetItem(1,'search_type',al_search)



cb_search.TriggerEvent(Clicked!)
end subroutine

on u_recipient_search.create
this.cb_search=create cb_search
this.cb_clear=create cb_clear
this.dw_list=create dw_list
this.dw_search=create dw_search
this.Control[]={this.cb_search,&
this.cb_clear,&
this.dw_list,&
this.dw_search}
end on

on u_recipient_search.destroy
destroy(this.cb_search)
destroy(this.cb_clear)
destroy(this.dw_list)
destroy(this.dw_search)
end on

type cb_search from commandbutton within u_recipient_search
integer x = 2725
integer y = 116
integer width = 338
integer height = 108
integer taborder = 30
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Search"
end type

event clicked;Pointer	lp_old_pointer

lp_old_pointer=SetPointer(HourGlass!)

IF uf_retrieve() < 0 THEN
	MessageBox('Search Error','There was a problem validating/setting the search criteria. Please try again.', Information!)
	RETURN
END IF

SetPointer(lp_old_pointer)
end event

type cb_clear from commandbutton within u_recipient_search
integer x = 2725
integer y = 240
integer width = 338
integer height = 108
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Clear"
end type

on clicked;uf_clear()
dw_search.SetFocus()
end on

type dw_list from u_dw_online within u_recipient_search
integer x = 23
integer y = 608
integer width = 3131
integer height = 560
integer taborder = 40
string dataobject = "d_bi_provider_list"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event clicked;call super::clicked;/*	If the current row is the same as the row the user just clicked,
	a rowfocuschanges will not happen.  Therefore, force one since 
	so the current provider will be re-retrieved
*/

If THIS.GetRow() = row THEN
	THIS.TriggerEvent(RowFocusChanged!)
End IF
end event

event rowfocuschanged;call super::rowfocuschanged;LONG  ll_row

ll_row = currentrow
IF ll_row > 0 THEN
	dw_list.SelectRow(0, FALSE)
	dw_list.SelectRow(ll_row, TRUE)
	uf_set_recipient_no(ll_row)
	THIS.SetFocus()
ELSEIF ll_row = 0 THEN
	uf_set_recipient_no(0)
END IF


end event

event doubleclicked;call super::doubleclicked;IF row > 0 THEN
   uf_set_recipient_no(row)
   PARENT.TriggerEvent('ue_doubleclicked')
END IF

end event

event retrieveend;call super::retrieveend;
IF THIS.RowCount() > 0 THEN
	THIS.SelectRow(1,TRUE)
	THIS.SetRow(1)
	uf_set_recipient_no(1)
	THIS.SetFocus()
ELSE
	uf_set_recipient_no(0)
END IF
end event

on rbuttondown;M_DW_ONLINE_RMB_POPUP lm_popup
/*	
	create the menu
*/


	lm_popup = Create m_dw_online_rmb_popup
	lm_popup.mf_set_datawindow(This)
	lm_popup.m_options.m_sort.visible = TRUE
	lm_popup.m_options.PopMenu(w_frame.PointerX( ), w_frame.PointerY( ))
	Destroy lm_popup
end on

type dw_search from u_dw_online within u_recipient_search
integer x = 23
integer y = 8
integer width = 3131
integer height = 592
integer taborder = 10
string dataobject = "d_recipient_search"
borderstyle borderstyle = styleraised!
end type

event itemchanged;call super::itemchanged;DATAWINDOWCHILD	ldwc_child
INTEGER           li_rtn
STRING				ls_type, ls_filter

/*	If type selected then filter the sub type list based on type
*/
	IF This.GetColumnName() = 'recipient_type' THEN
		ls_type = This.GetText()
		
		uf_switch_type(ls_type)
		
		IF ls_type <> 'I' THEN
			THIS.GetChild('sub_type', ldwc_child)
			
			ldwc_child.SetTransObject(SQLCA)
			ldwc_child.Retrieve()
			
			ls_filter = "provider_type_code = '" + ls_type + "'"
			li_rtn = ldwc_child.SetFilter(ls_filter)
			li_rtn = ldwc_child.Filter()
			This.SetItem(1,'sub_type', '00')
		END IF
	END IF

/*	If enter pressed, trigger the search button
*/
	IF KeyDown(keyEnter!) THEN
	   cb_search.PostEvent(Clicked!)
	END IF
	

end event

