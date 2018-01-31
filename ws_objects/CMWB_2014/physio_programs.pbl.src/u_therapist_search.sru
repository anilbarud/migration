$PBExportHeader$u_therapist_search.sru
$PBExportComments$the user object for searching for providers
forward
global type u_therapist_search from userobject
end type
type cb_search from commandbutton within u_therapist_search
end type
type cb_clear from commandbutton within u_therapist_search
end type
type dw_list from u_dw_online within u_therapist_search
end type
type dw_search from u_dw_online within u_therapist_search
end type
end forward

global type u_therapist_search from userobject
integer width = 3785
integer height = 996
long backcolor = 67108864
long tabtextcolor = 33554432
event ue_doubleclicked pbm_custom01
cb_search cb_search
cb_clear cb_clear
dw_list dw_list
dw_search dw_search
end type
global u_therapist_search u_therapist_search

type variables
STRING		is_where, is_select, is_name, &
		is_name_type, is_city, &
		is_type_code, is_sub_type, &
		is_passed_type_code
STRING is_type
LONG		 il_search_type

W_ANCESTOR	iwi_window_parent 


LONG		il_therapist_no, il_wif_principal_id , il_provider_no
DATE		idt_listed_date, idt_delisted_date
STRING	is_last_name, il_license_no
 

end variables

forward prototypes
public function integer uf_validate_query ()
public function integer uf_set_query ()
public function integer uf_retrieve ()
public function integer uf_set_parent (window awi_parent)
public function integer uf_set_type_code (string as_type)
public subroutine uf_clear ()
public subroutine uf_init ()
public subroutine uf_clear_highlighting ()
end prototypes

public function integer uf_validate_query ();/*	Get the criteria entered

select distinct THERAPIST.therapist_no, 
       THERAPIST.wif_principal_id,     
	   THERAPIST_LICENSE.listed_date,            
	   THERAPIST_LICENSE.delisted_date,           
	   THERAPIST.comments, 
       THERAPIST_LICENSE.license_type_code,
	   THERAPIST_LICENSE.license_no,      
	   THERAPIST_LICENSE.license_prov_state_code,
		wif_custom_principal.last_name,  
		wif_custom_principal.first_name
	    from THERAPIST 
left join  THERAPIST_LICENSE on THERAPIST.therapist_no= THERAPIST_LICENSE.therapist_no
join wif_custom_principal on wif_custom_principal.wif_principal_id = THERAPIST.wif_principal_id

*/

dw_search.AcceptText()
il_therapist_no  		= 	dw_search.GetItemNumber(1,'therapist_no')
il_wif_principal_id 	=  dw_search.GetItemNumber(1,'wif_principal_id')
is_last_name 			= 	Trim(dw_search.GetItemString(1, 'last_name'))
idt_listed_date 		=  dw_search.GetItemdate(1,'listed_date')
idt_delisted_date 	=  dw_search.GetItemdate(1,'delisted_date')
il_license_no  		= 	dw_search.GetItemstring(1,'license_no')
il_provider_no 		=  dw_search.GetItemNumber(1,'provider_no')
il_search_type 		=  dw_search.GetItemNumber(1,'search_type')


RETURN 0
end function

public function integer uf_set_query ();/*

il_therapist_no  	= 	dw_search.GetItemNumber(1,'therapist_no')
il_provider_no 	=  	dw_search.GetItemNumber(1,'provider_no')
is_last_name 		= 	Trim(dw_search.GetItemString(1, 'last_name'))
idt_listed_date 		=  dw_search.GetItemdate(1,'listed_date')
idt_delisted_date 	=  dw_search.GetItemdate(1,'delisted_date')
il_license_no  		= 	dw_search.GetItemNumber(1,'license_no')


select distinct THERAPIST.therapist_no, 
       THERAPIST.wif_principal_id,     
	   THERAPIST_LICENSE.listed_date,            
	   THERAPIST_LICENSE.delisted_date,           
	   THERAPIST.comments, 
       THERAPIST_LICENSE.license_type_code,
	   THERAPIST_LICENSE.license_no,      
	   THERAPIST_LICENSE.license_prov_state_code,
		wif_custom_principal.last_name,  
		wif_custom_principal.first_name
	    from THERAPIST 
left join  THERAPIST_LICENSE on THERAPIST.therapist_no= THERAPIST_LICENSE.therapist_no
join wif_custom_principal on wif_custom_principal.wif_principal_id = THERAPIST.wif_principal_id

*/

STRING  ls_modstring, ls_returncode, ls_new_select, ls_temp

ls_new_select = ''

/*		do a provider search by therapist no if provided */
IF il_therapist_no > 0 THEN

   	IF ls_new_select > '' THEN 
      	ls_new_select = ls_new_select + ' and '
	END IF
		ls_new_select = ls_new_select + "(THERAPIST.therapist_no = " + String(il_therapist_no) + ")"
		
END IF 

/*		do a provider search by license_no if provided */
IF trim(il_license_no) > '' THEN
	
   	IF ls_new_select > '' THEN 
      	ls_new_select = ls_new_select + ' and '
	END IF
		ls_new_select = ls_new_select + "(THERAPIST_LICENSE.license_no = " + "'" +  il_license_no +"'" + ")"
		
END IF 

/*	set up for search by name */
IF is_last_name > "" THEN
	IF ls_new_select > '' THEN 
      	ls_new_select = ls_new_select + ' and '
    END IF
	
	 CHOOSE CASE il_search_type
		CASE 1// exact match
				
			 ls_new_select = ls_new_select + " (wif_custom_principal.last_name = ~~~"" + is_last_name  + "~~~") "
				 
		CASE 2  // starts with
				
			 ls_new_select = ls_new_select + " (wif_custom_principal.last_name like ~~~"" + is_last_name  + "%~~~") "
				 
		CASE ELSE 	// contains
			
			 ls_new_select = ls_new_select + " (wif_custom_principal.last_name like ~~~"%" + is_last_name  + "%~~~") "	
			 
	 END CHOOSE
		
END IF 

//dates
IF NOT IsNull(idt_listed_date) OR NOT ISNULL(idt_delisted_date) THEN
	
	IF ls_new_select > '' THEN 
      	ls_new_select = ls_new_select + ' and '
    END IF
	 
	IF ISNULL(idt_delisted_date)  THEN 
	//	idt_delisted_date = DATE('2056-01-01')
		ls_new_select = ls_new_select + ' ( THERAPIST_LICENSE.listed_date >=  ' + String(idt_listed_date,'yyyy-mm-dd') + ')'
		
	ELSEIF ISNULL(idt_listed_date)  THEN 
	//	idt_listed_date = DATE('1900-01-01')
			ls_new_select = ls_new_select + ' ( THERAPIST_LICENSE.delisted_date <= ' + String(idt_delisted_date,'yyyy-mm-dd') + ')'
		
	ELSE		
			ls_new_select = ls_new_select + " ( (THERAPIST_LICENSE.listed_date >= ~~~"" + String(idt_listed_date,'yyyy-mm-dd') + "~~~") and (THERAPIST_LICENSE.delisted_date <= ~~~"" + String(idt_delisted_date,'yyyy-mm-dd') + "~~~"))"	
	END IF 
			
END IF 	


IF TRIM(ls_new_select) = '' THEN  RETURN -1

/*	now create the new select statement using the original select and the original where 
	with the additional where just created added on
*/
ls_ModString = "DataWindow.Table.Select=~"" + is_select + ' WHERE ' + ls_new_select + "~""
ls_ReturnCode = dw_list.Modify(ls_ModString)
IF Len(ls_ReturnCode) > 0 THEN
	RETURN -1
ELSE
	RETURN 1
END IF


end function

public function integer uf_retrieve ();LONG  ll_return

/*	validate that the query is correct */
IF uf_validate_query() < 0 THEN RETURN -1

/*	set the new query */
IF uf_set_query() < 0 THEN	RETURN -1

/*	Call the wf_clear_identifier function to clear the current service provider, then retrieve the new one */
iwi_window_parent.wf_clear_identifier()

ll_return = dw_list.Retrieve()
SQLCA.nf_handle_error("u_therapist_search","dw_list","uf_retrieve")

IF ll_return < 1 THEN
	MessageBox('Warning', 'No data found matching specified criteria.')
END IF

RETURN ll_return

end function

public function integer uf_set_parent (window awi_parent);
iwi_window_parent = awi_parent
uf_init()

RETURN 0
end function

public function integer uf_set_type_code (string as_type);
//**************************************************************************************
// The type code for the search
// if null then the search looks at all types
//
// this function is to be used by the parent window to allow the user to search out only
// certain service providers
//**************************************************************************************

is_type_code 				= as_type
is_passed_type_code 		= as_type

Return 0
end function

public subroutine uf_clear ();DATAWINDOWCHILD	ldwc_child

/*	Clear the datawindows and insert a row for searching */
IF dw_search.GetChild('provider_no', ldwc_child) < 0 THEN
	MessageBox("Error","Error retrieving list of providers. Please call the help desk")
	Return
END IF

// reset and insert
dw_search.Reset()
dw_search.InsertRow(0)
dw_list.Reset()
end subroutine

public subroutine uf_init ();DATAWINDOWCHILD  ldwc_child

dw_list.SetTransObject(SQLCA)

dw_search.SetTransObject(SQLCA)

dw_search.InsertRow(0)

/*	get the original select statement */
is_select = dw_list.Describe("DataWindow.Table.Select")
dw_search.SetFocus()

end subroutine

public subroutine uf_clear_highlighting ();/*	This function turns off the highlighting of the search list.*/
dw_list.SelectRow(0,False)
end subroutine

on u_therapist_search.create
this.cb_search=create cb_search
this.cb_clear=create cb_clear
this.dw_list=create dw_list
this.dw_search=create dw_search
this.Control[]={this.cb_search,&
this.cb_clear,&
this.dw_list,&
this.dw_search}
end on

on u_therapist_search.destroy
destroy(this.cb_search)
destroy(this.cb_clear)
destroy(this.dw_list)
destroy(this.dw_search)
end on

type cb_search from commandbutton within u_therapist_search
integer x = 3451
integer y = 20
integer width = 283
integer height = 84
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

type cb_clear from commandbutton within u_therapist_search
integer x = 3456
integer y = 140
integer width = 283
integer height = 84
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

type dw_list from u_dw_online within u_therapist_search
integer y = 252
integer width = 3767
integer height = 736
integer taborder = 40
string dataobject = "d_therapist_list"
boolean controlmenu = true
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

event rowfocuschanged;call super::rowfocuschanged;//LONG  ll_row
//
//ll_row = dw_list.GetRow()
//
//IF ll_row > 0 THEN
//	dw_list.SelectRow(0, FALSE)
//	dw_list.SelectRow(ll_row, TRUE)
//	This.SetFocus()
//END IF


end event

event doubleclicked;call super::doubleclicked;IF row > 0 THEN
   PARENT.TriggerEvent('ue_doubleclicked')
END IF

end event

event retrieveend;call super::retrieveend;IF THIS.RowCount() > 0 THEN
   	THIS.SelectRow(1,TRUE)
	THIS.SetRow(1)
   //	uf_set_provider_no(1)
	This.SetFocus()
END IF
end event

event rbuttondown;M_DW_ONLINE_RMB_POPUP lm_popup

/*	 create the menu */

lm_popup = Create m_dw_online_rmb_popup
lm_popup.mf_set_datawindow(This)
lm_popup.m_options.m_sort.visible = TRUE
lm_popup.m_options.PopMenu(w_frame.PointerX( ), w_frame.PointerY( ))
Destroy lm_popup
end event

event constructor;call super::constructor;THIS.settransobject(sqlca)
THIS.uf_setselect(1)
end event

type dw_search from u_dw_online within u_therapist_search
integer x = 23
integer y = 8
integer width = 3022
integer height = 196
integer taborder = 10
string dataobject = "d_therapist_search"
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

