$PBExportHeader$u_service_provider_search.sru
$PBExportComments$the user object for searching for providers
forward
global type u_service_provider_search from userobject
end type
type cb_search from commandbutton within u_service_provider_search
end type
type cb_clear from commandbutton within u_service_provider_search
end type
type dw_list from u_dw_online within u_service_provider_search
end type
type dw_search from u_dw_online within u_service_provider_search
end type
end forward

global type u_service_provider_search from userobject
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
global u_service_provider_search u_service_provider_search

type variables
STRING		is_where, is_select, is_name, &
		is_name_type, is_city, &
		is_type_code, is_sub_type, &
		is_passed_type_code
STRING is_type
LONG		il_provider_no, il_search_type, il_show
W_ANCESTOR	iwi_window_parent 
BOOLEAN ib_multi = FALSE
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
	is_name = Trim(dw_search.GetItemString(1, 'name'))
	is_name_type = Trim(dw_search.GetItemString(1, 'name_type'))
	is_city = Trim(dw_search.GetItemString(1, 'city'))
	is_sub_type = Trim(dw_search.GetItemString(1,'sub_type'))
	il_search_type = dw_search.GetItemNumber(1, 'search_type')
	il_provider_no = dw_search.GetItemNumber(1,'provider_no')
	IF IsNull(is_passed_type_code) OR is_passed_type_code = '' THEN
		is_type_code = dw_search.GetItemString(1,'provider_type')
	ELSE
		is_type_code = is_passed_type_code
	END IF
/*
	now check for data in fields
*/
	
	IF (IsNull(is_name) OR is_name = '')  AND (IsNull(is_city) OR is_city = '') AND (IsNull(is_sub_type) OR is_sub_type = '') AND IsNull(il_provider_no) AND (IsNull(is_type_code) OR is_type_code = '') THEN
   	MessageBox('Insufficient Search Criteria', 'At least one of the following must be entered: Name, City, Type, Sub Type or Provider No.')
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

public function integer uf_set_query ();STRING  ls_modstring, ls_returncode, ls_new_select, ls_temp, ls_order
LONG ll_count

ls_new_select = ''
ib_multi = FALSE

IF il_provider_no > 0 THEN
/*		do a provider search by number + type if provided
*/
	IF ls_new_select > '' THEN 
		ls_new_select = ls_new_select + ' and '
	END IF
		
	//Check to see if provider number is only one entered
	IF (ISNULL(is_name) OR is_name = '') AND (ISNULL(is_city) OR is_city = '') AND (ISNULL(is_type_code) OR is_type_code = '')  AND il_provider_no > 0 THEN
		SELECT    Count(*)
		INTO       :ll_count
		FROM      PROVIDER P
		LEFT JOIN PROVIDER_FROM_TO PFT ON (P.provider_no = PFT.new_provider_no )
		WHERE     (PFT.old_provider_no = :il_provider_no OR P.provider_no = :il_provider_no)
		USING    SQLCA;
		
		SQLCA.nf_handle_error('u_service_provider_search', 'uf_set_query', 'embedded SQL: SELECT Count(*) from PROVIDER P, PROVIDER_FROM_TO PFT')
		IF ll_count > 1 THEN
			ls_new_select = ls_new_select + " (PROVIDER_FROM_TO.old_provider_no = " + String(il_provider_no) + " OR PROVIDER.provider_no = " + String(il_provider_no) + ")"
			ib_multi = TRUE
			il_show = 1
		ELSE
			ls_new_select = ls_new_select + " (PROVIDER.provider_no = " + String(il_provider_no) + ")"
			il_show = 0
		END IF
	ELSE		
		ls_new_select = ls_new_select + " (PROVIDER.provider_no = " + String(il_provider_no) + ")"
		il_show = 0
	END IF
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
IF is_sub_type <> "00" and is_sub_type <> "" THEN
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

/*	now create the new select statement using the original select and the original where 
	with the additional where just created added on
*/
IF ib_multi THEN
	ls_order = " ORDER BY PROVIDER_FROM_TO.old_provider_no ASC"
ELSE
	ls_order = " ORDER BY PROVIDER.name ASC" 
	il_show = 0
END IF

IF (Trim(ls_new_select) = "" OR IsNull(ls_new_select)) THEN //2014/08/28 David Worboys - Added some extra protection to prevent a carsh due to ls_select being empty
	ls_ModString = "DataWindow.Table.Select=~" " + is_select+ " " + ls_order + '"' //2014/08/28 David Worboys
ELSE //2014/08/28 David Worboys
	ls_ModString = "DataWindow.Table.Select=~" " + is_select + 'WHERE ' + ls_new_select + " " + ls_order + '"'
END IF

ls_ReturnCode = dw_list.Modify(ls_ModString)
IF Len(ls_ReturnCode) > 0 THEN
	Return -1
ELSE
	Return 1
END IF


end function

public function integer uf_retrieve ();LONG  ll_return, ll_new_provider
LONG ll_check_provider
STRING ls_check_type, ls_check_sub_type, ls_name

il_show = 0

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
ll_check_provider = il_provider_no
ls_check_type = is_type_code
ls_check_sub_type = is_sub_type

iwi_window_parent.wf_clear_identifier()
ll_return = dw_list.Retrieve(il_show)
IF ll_return < 1 THEN
	IF ll_check_provider > 0 AND ls_check_type > '' THEN
		SELECT pft.new_provider_no, p.name
		INTO     :ll_new_provider, :ls_name
		FROM    PROVIDER_FROM_TO pft, PROVIDER p
		WHERE  pft.new_provider_no = p.provider_no
		AND      pft.old_provider_no      = :ll_check_provider
		AND      pft.provider_type_code = :ls_check_type
		USING   SQLCA;
		
		ll_return = SQLCA.nf_handle_error("u_service_provider_search","SELECT new_provider_no","uf_retrieve")
		
		IF ll_new_provider > 0 THEN
			MessageBox('Information', 'Provider # ' + STRING(ll_check_provider) + ', ' + ls_name + ', has been re-assigned a new Provider Number, # ' + STRING(ll_new_provider))
		ELSE
			MessageBox('Warning', 'No data found matching specified criteria.')
		END IF
     ELSE
           MessageBox('Warning', 'No data found matching specified criteria.') //2014-08-20 David Worboys, requested by Jill Hawker (PR24523 ).
	END IF
END IF
ll_return = SQLCA.nf_handle_error("u_service_provider_search","dw_list","uf_retrieve")
Return ll_return

end function

public subroutine uf_set_provider_no (integer al_row);STRING ls_type_code
/*
	sets the parents provider number along with the type code
*/
IF al_row > 0 THEN
	il_provider_no = dw_list.GetItemNumber(al_row, 'provider_no')
	ls_type_code = dw_list.GetItemString(al_row, 'provider_type_code')
ELSE
	il_provider_no = 0
	ls_type_code = ''
END IF

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
is_passed_type_code = as_type

Return 0
end function

public subroutine uf_clear ();DATAWINDOWCHILD	ldwc_child


/*	Clear the datawindows and insert a row for searching
*/
	IF dw_search.GetChild('sub_type', ldwc_child) < 0 THEN
		MessageBox("Error","Error retrieving list of provider sub type codes. Please call the help desk")
		Return
	END IF

	dw_search.Reset()
	dw_search.InsertRow(0)
	IF NOT(IsNull(is_passed_type_code) OR is_passed_type_code = '') THEN
	// set off
		dw_search.Modify('provider_type.BackGround.Color= 553648127 provider_type.Protect=1')
		ldwc_child.SetFilter("provider_type_code = '" + is_passed_type_code + "'")
		ldwc_child.Filter()
		dw_search.SetItem(1,'sub_type', '00')
		dw_search.SetItem(1,'provider_type', is_passed_type_code)
	ELSE
 		ldwc_child.SetFilter("provider_type_code = ''")
		ldwc_child.Filter()
	END IF

	dw_list.Reset()
end subroutine

public subroutine uf_init ();DATAWINDOWCHILD  ldwc_child

	dw_list.SetTransObject(SQLCA)
	dw_search.SetTransObject(SQLCA)
	dw_search.InsertRow(0)

/*	IF a type code was passed, disable the provider_type_code field and populate the sub_type code
	drop down to only show those which are valid for the type_code passed.
	Otherwise, filter out the sub_type drop down until a valid provider_type_code is selected.
*/
	dw_search.GetChild('sub_type', ldwc_child)
	IF NOT(IsNull(is_passed_type_code) OR is_passed_type_code = '') THEN
		dw_search.Modify('provider_type.BackGround.Color= 553648127 provider_type.Protect=1')
		ldwc_child.SetFilter("provider_type_code = '" + is_passed_type_code + "'")
		ldwc_child.Filter()
		dw_search.SetItem(1,'sub_type', '00')
		dw_search.SetItem(1,'provider_type', is_passed_type_code)
	ELSE
 		ldwc_child.SetFilter("provider_type_code = ''")
		ldwc_child.Filter()
	END IF

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
dw_search.SetItem(1,'name_type', as_type)
dw_search.SetItem(1,'search_type',al_search)

cb_search.TriggerEvent(Clicked!)
end subroutine

on u_service_provider_search.create
this.cb_search=create cb_search
this.cb_clear=create cb_clear
this.dw_list=create dw_list
this.dw_search=create dw_search
this.Control[]={this.cb_search,&
this.cb_clear,&
this.dw_list,&
this.dw_search}
end on

on u_service_provider_search.destroy
destroy(this.cb_search)
destroy(this.cb_clear)
destroy(this.dw_list)
destroy(this.dw_search)
end on

type cb_search from commandbutton within u_service_provider_search
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

type cb_clear from commandbutton within u_service_provider_search
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

type dw_list from u_dw_online within u_service_provider_search
integer x = 23
integer y = 480
integer width = 3154
integer height = 556
integer taborder = 40
string dataobject = "d_provider_list"
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

event rowfocuschanged;call super::rowfocuschanged;LONG  ll_row
STRING ls_renumbered

ll_row = dw_list.GetRow()
IF ll_row > 0 THEN
	ls_renumbered = dw_list.GetItemString(ll_row,'renumber_flag')
	IF ls_renumbered = 'R' AND il_show > 0 THEN
		IF dw_list.Rowcount() > ll_row THEN
			dw_list.SelectRow(1, FALSE)
			dw_list.SelectRow(ll_row + 1, TRUE)
			uf_set_provider_no(ll_row + 1)
			This.SetFocus()
		END IF
	ELSE				
		dw_list.SelectRow(0, FALSE)
		dw_list.SelectRow(ll_row, TRUE)
		uf_set_provider_no(ll_row)
		This.SetFocus()
	END IF
ELSE
	uf_set_provider_no(0)
END IF


end event

event doubleclicked;call super::doubleclicked;STRING ls_renumbered

IF row > 0 THEN
	ls_renumbered = dw_list.GetItemString(row,'renumber_flag')
	IF ls_renumbered <> 'R' THEN
		uf_set_provider_no(row)
		PARENT.TriggerEvent('ue_doubleclicked')
	END IF
END IF

end event

event retrieveend;call super::retrieveend;STRING ls_renumbered
LONG ll_row

IF THIS.RowCount() > 0 THEN
	ls_renumbered = dw_list.GetItemString(dw_list.GetRow(),'renumber_flag')
	IF ib_multi AND il_show > 0 THEN
		IF ls_renumbered = 'R' THEN
			IF dw_list.Rowcount() > dw_list.GetRow() THEN
				dw_list.SelectRow(1, FALSE)
				dw_list.SelectRow(ll_row + 1, TRUE)
				uf_set_provider_no(ll_row + 1)
				This.SetFocus()
			END IF
		ELSE	
			THIS.SelectRow(1,TRUE)
			THIS.SetRow(1)
			uf_set_provider_no(1)
			This.SetFocus()
		END IF 	
	ELSE	
		THIS.SelectRow(1,TRUE)
		THIS.SetRow(1)
		uf_set_provider_no(1)
		This.SetFocus()
	END IF
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

event rowfocuschanging;call super::rowfocuschanging;STRING ls_renumbered
LONG ll_provider, ll_new_provider

IF newrow > 0 THEN
	ls_renumbered = dw_list.GetItemString(newrow,'renumber_flag')
	IF ls_renumbered = 'R' AND il_show > 0 THEN
		RETURN 1	
	END IF
END IF


end event

type dw_search from u_dw_online within u_service_provider_search
integer x = 23
integer y = 8
integer width = 3154
integer height = 468
integer taborder = 10
string dataobject = "d_service_provider_search"
borderstyle borderstyle = styleraised!
end type

event itemchanged;call super::itemchanged;DATAWINDOWCHILD	ldwc_child
STRING				ls_type, ls_filter

/*	If type selected then filter the sub type list based on type
*/
	IF This.GetColumnName() = 'provider_type' THEN
		ls_type = This.GetText()
		This.GetChild('sub_type', ldwc_child)
		ls_filter = "provider_type_code = '" + ls_type + "'"
		ldwc_child.SetFilter(ls_filter)
		ldwc_child.Filter()
		This.SetItem(1,'sub_type', '00')
	END IF

/*	If enter pressed, trigger the search button
*/
	IF KeyDown(keyEnter!) THEN
	   cb_search.PostEvent(Clicked!)
	END IF
	

end event

