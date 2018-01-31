$PBExportHeader$u_clinic_search.sru
$PBExportComments$the user object for searching for providers
forward
global type u_clinic_search from userobject
end type
type cb_search from commandbutton within u_clinic_search
end type
type cb_clear from commandbutton within u_clinic_search
end type
type dw_list from u_dw_online within u_clinic_search
end type
type dw_search from u_dw_online within u_clinic_search
end type
end forward

global type u_clinic_search from userobject
integer width = 3200
integer height = 1040
long backcolor = 67108864
long tabtextcolor = 33554432
event ue_doubleclicked pbm_custom01
cb_search cb_search
cb_clear cb_clear
dw_list dw_list
dw_search dw_search
end type
global u_clinic_search u_clinic_search

type variables
STRING			is_where, is_select, is_name, is_name_type, is_city, 	is_type_code
STRING 			is_type, is_sub_type, 	 is_sub_type_specific, is_program_code
LONG				il_provider_no, il_search_type
W_ANCESTOR	iwi_window_parent 
end variables

forward prototypes
public function integer uf_validate_query ()
public function integer uf_set_query ()
public function integer uf_retrieve ()
public subroutine uf_set_provider_no (integer al_row)
public function integer uf_set_parent (window awi_parent)
public function integer uf_set_type_code (string as_type)
public subroutine uf_clear ()
public subroutine uf_init ()
public subroutine uf_clear_highlighting ()
public subroutine uf_set_provider (long al_provider_no, string as_provider_type_code, string as_provider_sub_type_code, string as_name, string as_city, string as_type, long al_search)
end prototypes

public function integer uf_validate_query ();/*	Get the criteria entered
*/
	dw_search.AcceptText()
	is_name 				= Trim(dw_search.GetItemString(1, 'name'))
	is_name_type 		= Trim(dw_search.GetItemString(1, 'name_type'))
	is_city 				= Trim(dw_search.GetItemString(1, 'city'))
	is_sub_type 			= '35' //hardcoded
	il_search_type 		= dw_search.GetItemNumber(1, 'search_type')
	il_provider_no 		= dw_search.GetItemNumber(1,'provider_no')
	is_type_code 		= 'M'
	is_program_code 	= dw_search.GetItemstring(1,'program_code')

/* now check for data in fields
*/
	
	IF (IsNull(is_name) OR is_name = '')  AND (IsNull(is_city) OR is_city = '') AND (IsNull(is_sub_type) OR is_sub_type = '') AND IsNull(il_provider_no) AND (IsNull(is_type_code) OR is_type_code = '') THEN
   	MessageBox('Insufficient Search Criteria', 'At least one of the following must be specified before a retrieval can be performed: Name, City, Type, Sub Type or Provide No.')
	   Return -1
	END IF

	IF NOT IsNull(is_name) and IsNull(is_name_type) THEN
/*		This shouldn't happen
*/
   	MessageBox('Name Type Not Specified','The name type must be specified for a search by name.')
	   Return -1
	END IF
	IF NOT IsNull(is_sub_type) AND IsNull(is_type_code) THEN	
		MessageBox('Search Error', 'If sub type is provided then the provider type must also be specified.')
		Return -1
	END IF
Return 0
end function

public function integer uf_set_query ();STRING  ls_modstring, ls_returncode, ls_new_select, ls_temp

	ls_new_select = ''

	IF il_provider_no > 0 THEN
/*		do a provider search by number + type if provided
*/
   	IF ls_new_select > '' THEN 
      	ls_new_select = ls_new_select + ' and '
	   END IF
		ls_new_select = ls_new_select + "(PROVIDER.provider_no = " + String(il_provider_no) + ")"
	END IF 

/*	set up for search by name
*/
	IF is_name > "" THEN
   	IF ls_new_select > '' THEN 
      	ls_new_select = ls_new_select + ' and '
	   END IF
   	IF is_name_type = 'M' THEN
      	IF il_search_type = 1 THEN
	      // exact match
   	      ls_new_select = ls_new_select + " (PROVIDER.name = ~~~"" + is_name  + "~~~") "
      	ELSE
         	IF il_search_type = 2 THEN
	         // starts with
   	         ls_new_select = ls_new_select + " (PROVIDER.name like ~~~"" + is_name  + "%~~~") "
      	   ELSE
         	// contains
            	ls_new_select = ls_new_select + " (PROVIDER.name like ~~~"%" + is_name  + "%~~~") "
	         END IF
   	   END IF
	   ELSE
   	   IF il_search_type = 1 THEN
      	// exact match
         	ls_new_select = ls_new_select + " (PROVIDER.sort_name = ~~~"" + is_name  + "~~~") "
			ELSE
         	IF il_search_type = 2 THEN
	         // starts with
   	         ls_new_select = ls_new_select + " (PROVIDER.sort_name like ~~~"" + is_name  + "%~~~") "
      	   ELSE
         	// contains
            ls_new_select = ls_new_select + " (PROVIDER.sort_name like ~~~"%" + is_name  + "%~~~") "
	         END IF
   	   END IF
	   END IF
	END IF

/*	search by city
*/
	IF is_city > "" THEN
   	IF ls_new_select > '' THEN 
      	ls_new_select = ls_new_select + ' and '
	   END IF
   	IF il_search_type = 1 THEN
      	ls_new_select = ls_new_select + " (PROVIDER.city = ~~~"" + is_city  + "~~~") "
	   ELSE
   	   IF il_search_type = 2 THEN
      	   ls_new_select = ls_new_select + " (PROVIDER.city like ~~~"" + is_city  + "%~~~") "
	      ELSE
   	      ls_new_select = ls_new_select + " (PROVIDER.city like ~~~"%" + is_city  + "%~~~") "
      	END IF
		END IF
	END IF
/*	search by sub type
*/
	IF is_sub_type > "" THEN
   	IF ls_new_select > '' THEN 
      	ls_new_select = ls_new_select + ' and '
	   END IF
   	ls_new_select = ls_new_select + " (PROVIDER.provider_sub_type_code = ~~~"" + is_sub_type  + "~~~") "
	END IF

/*	if type code is specified limit the search even further
*/
	IF NOT IsNull(is_type_code) THEN
   	IF is_type_code > '' THEN
      	IF ls_new_select > '' THEN 
         	ls_new_select = ls_new_select + ' and '
	      END IF
   	   ls_new_select = ls_new_select + " (PROVIDER.provider_type_code = ~~~"" + is_type_code + "~~~") "
	   END IF
	END IF
	
/*	search by program_code
*/
	IF is_program_code > "" THEN
   	IF ls_new_select > '' THEN 
      	ls_new_select = ls_new_select + ' and '
	   END IF
   	ls_new_select = ls_new_select + " (REHAB_PROGRAM_xref_PROVIDER.rehab_program_code = ~~~"" + is_program_code  + "~~~") "
	END IF
	

/*	now create the new select statement using the original select and the original where 
	with the additional where just created added on
*/
ls_ModString = "DataWindow.Table.Select=~"" + is_select + 'WHERE ' + ls_new_select + "~""
ls_ReturnCode = dw_list.Modify(ls_ModString)
IF Len(ls_ReturnCode) > 0 THEN
	Return -1
ELSE
	Return 1
END IF


end function

public function integer uf_retrieve ();LONG  ll_return

/*	validate that the query is correct
*/
	IF uf_validate_query() < 0 THEN
   	Return -1
	END IF

/*	set the new query
*/
	IF uf_set_query() < 0 THEN
   	Return -1
	END IF

/*	Call the wf_clear_identifier function to clear the current service
	provider, then retrieve the new one
*/
	iwi_window_parent.wf_clear_identifier()
	ll_return = dw_list.Retrieve()
	SQLCA.nf_handle_error("u_service_provider_search","dw_list","uf_retrieve")
	
	IF ll_return < 1 THEN
		MessageBox('Warning', 'No data found matching specified criteria.')
	END IF

	Return ll_return

end function

public subroutine uf_set_provider_no (integer al_row);STRING ls_type_code
/*
	sets the parents provider number along with the type code
*/
	il_provider_no = dw_list.GetItemNumber(al_row, 'provider_no')
	ls_type_code = dw_list.GetItemString(al_row, 'provider_type_code')
	iwi_window_parent.wf_set_provider_no(il_provider_no, ls_type_code)

end subroutine

public function integer uf_set_parent (window awi_parent);
	iwi_window_parent = awi_parent
	uf_init()
	Return 0
end function

public function integer uf_set_type_code (string as_type);
//**************************************************************************************
// The type code for the search
// if null then the search looks at all types
//
// this function is to be used by the parent window to allow the user to search out only
// certain service providers
//**************************************************************************************

is_type_code = as_type

Return 0
end function

public subroutine uf_clear ();dw_search.Reset()
dw_search.InsertRow(0)
dw_list.Reset()
end subroutine

public subroutine uf_init ();dw_list.SetTransObject(SQLCA)
dw_search.SetTransObject(SQLCA)
dw_search.InsertRow(0)
		
/*	get the original select statement
*/
is_select = dw_list.Describe("DataWindow.Table.Select")
dw_search.SetFocus()

end subroutine

public subroutine uf_clear_highlighting ();/*	This function turns off the highlighting of the search list.
*/
	dw_list.SelectRow(0,False)
end subroutine

public subroutine uf_set_provider (long al_provider_no, string as_provider_type_code, string as_provider_sub_type_code, string as_name, string as_city, string as_type, long al_search);DATAWINDOWCHILD ldwc_child
STRING ls_filter

dw_search.SetItem(1, 'provider_no',al_provider_no)
dw_search.SetItem(1, 'provider_type', as_provider_type_code)

dw_search.GetChild('sub_type', ldwc_child)
ls_filter = "provider_type_code = '" + as_provider_type_code + "'"
ldwc_child.SetFilter(ls_filter)
ldwc_child.Filter()
dw_search.SetItem(1,'sub_type', as_provider_sub_type_code)

dw_search.SetItem(1,'name', as_name)
dw_search.SetItem(1,'city', as_city)
dw_search.SetItem(1,'name_type', is_type)
dw_search.SetItem(1,'search_type',al_search)

cb_search.TriggerEvent(Clicked!)
end subroutine

on u_clinic_search.create
this.cb_search=create cb_search
this.cb_clear=create cb_clear
this.dw_list=create dw_list
this.dw_search=create dw_search
this.Control[]={this.cb_search,&
this.cb_clear,&
this.dw_list,&
this.dw_search}
end on

on u_clinic_search.destroy
destroy(this.cb_search)
destroy(this.cb_clear)
destroy(this.dw_list)
destroy(this.dw_search)
end on

type cb_search from commandbutton within u_clinic_search
integer x = 2706
integer y = 100
integer width = 283
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
iwi_window_parent.tag = 'search'
uf_retrieve()
SetPointer(lp_old_pointer)
end event

type cb_clear from commandbutton within u_clinic_search
integer x = 2706
integer y = 212
integer width = 283
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

type dw_list from u_dw_online within u_clinic_search
integer x = 23
integer y = 480
integer width = 3154
integer height = 556
integer taborder = 40
string dataobject = "d_clinic_list"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event clicked;call super::clicked;/*	If the current row is the same as the row the user just clicked,
	a rowfocuschanges will not happen.  Therefore, force one since 
	so the current provider will be re-retrieved
*/

If This.GetRow() = row THEN
	This.TriggerEvent(rowfocuschanged!)
End IF
end event

on rowfocuschanged;call u_dw_online::rowfocuschanged;LONG  ll_row

ll_row = dw_list.GetRow()
IF ll_row > 0 THEN
	dw_list.SelectRow(0, FALSE)
	dw_list.SelectRow(ll_row, TRUE)
	uf_set_provider_no(ll_row)
	This.SetFocus()
END IF


end on

event doubleclicked;call super::doubleclicked;IF row > 0 THEN
   uf_set_provider_no(row)
   PARENT.TriggerEvent('ue_doubleclicked')
END IF

end event

on retrieveend;call u_dw_online::retrieveend;IF THIS.RowCount() > 0 THEN
   THIS.SelectRow(1,TRUE)
	THIS.SetRow(1)
   uf_set_provider_no(1)
	This.SetFocus()
END IF
end on

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

type dw_search from u_dw_online within u_clinic_search
integer x = 23
integer y = 8
integer width = 3154
integer height = 468
integer taborder = 10
string dataobject = "d_clinic_search"
borderstyle borderstyle = styleraised!
end type

on itemchanged;call u_dw_online::itemchanged;DATAWINDOWCHILD	ldwc_child
STRING				ls_type, ls_filter

/*	If type selected then filter the sub type list based on type
*/
	IF This.GetColumnName() = 'provider_type' THEN
		ls_type = This.GetText()
		This.GetChild('sub_type', ldwc_child)
		ls_filter = "provider_type_code = '" + ls_type + "'"
		ldwc_child.SetFilter(ls_filter)
		ldwc_child.Filter()
		This.SetItem(1,'sub_type', '')
	END IF

/*	If enter pressed, trigger the search button
*/
	IF KeyDown(keyEnter!) THEN
	   cb_search.PostEvent(Clicked!)
	END IF
	

end on

