$PBExportHeader$u_tab_selection.sru
forward
global type u_tab_selection from u_tab
end type
end forward

global type u_tab_selection from u_tab
boolean boldselectedtext = true
event ue_post_constructor ( )
end type
global u_tab_selection u_tab_selection

forward prototypes
public subroutine of_add_tabpages (string as_string_of_data_object_array[])
public function string of_get_where ()
public function string of_get_filter ()
public subroutine of_get_string_array (ref s_arg_array astr_arg_array[])
end prototypes

public subroutine of_add_tabpages (string as_string_of_data_object_array[]);STRING 		 ls_tabpage_header
INTEGER		 li_counter, li_upperbound_check, li_selected_tab, li_dataobject_array_count

//grab the currently selected tab to be used later
li_selected_tab = THIS.selectedtab

THIS.setredraw(FALSE)

/* close the tab for a refresh, this code removes the tab objects - if any exist */
li_upperbound_check = upperbound( THIS.Control[])
IF li_upperbound_check > 0 THEN
	FOR li_counter = 1 TO li_upperbound_check 
		THIS.Closetab(THIS.Control[1])	
	NEXT
END IF 

THIS.setredraw(TRUE)

/*  check to see if it is a rest */
IF as_string_of_data_object_array[1] = "RESET" THEN RETURN

//open a tabpage for each of the authorizations  - this will have to be modified once I fiqure out what to do with saved ones
li_dataobject_array_count = upperbound(as_string_of_data_object_array[])

FOR li_counter = 1 TO li_dataobject_array_count

	ls_tabpage_header 			=  as_string_of_data_object_array[li_counter]
		
	THIS.opentabwithparm(u_tabpg_selection, ls_tabpage_header,0)
		
NEXT

// select the first tab
IF li_dataobject_array_count > 0 THEN
	THIS.SelectTab(1)
END IF 


end subroutine

public function string of_get_where ();STRING 		ls_where, ls_create_where
INTEGER		li_upperbound_check, li_counter
BOOLEAN	lb_valid

/* Returns the where clause from created from the tab_pages */

ls_where = ""

li_upperbound_check = upperbound( THIS.Control[] )

IF li_upperbound_check > 0 THEN
	FOR li_counter = 1 TO li_upperbound_check 
		
		// make sure there is a valid object
		lb_valid = isvalid(THIS.Control[li_counter])
		IF lb_valid = FALSE THEN RETURN ls_where//GET OUT
		 
		ls_where = THIS.Control[li_counter].DYNAMIC of_create_where()
		
		IF trim(ls_where) = "" THEN 
			CONTINUE
		ELSE 
			ls_where = ' AND ' + ls_where
		END IF 
		
		IF li_counter = 1 THEN 
			ls_create_where = ls_where
		ELSE
			ls_create_where = ls_create_where + ls_where 
		END IF 

	NEXT
END IF 

RETURN ls_create_where
end function

public function string of_get_filter ();STRING 	ls_filter, ls_create_filter
INTEGER	li_upperbound_check, li_counter
BOOLEAN	lb_valid

/* Returns the where clause from created from the tab_pages */

ls_filter = ""

li_upperbound_check = upperbound( THIS.Control[] )

IF li_upperbound_check > 0 THEN
	FOR li_counter = 1 TO li_upperbound_check 
		
		// make sure there is a valid object
		lb_valid = isvalid(THIS.Control[li_counter])
		IF lb_valid = FALSE THEN RETURN ls_filter//GET OUT
		
		
		ls_filter = THIS.Control[li_counter].DYNAMIC of_create_filter()
		
		IF trim(ls_filter) = "" THEN CONTINUE
		
		IF ISNULL(ls_create_filter) OR TRIM(ls_create_filter) = '' THEN 
			ls_create_filter = ls_filter
		ELSE		
			ls_create_filter = ls_create_filter + " and " + ls_filter
		END IF 

	NEXT
END IF 

RETURN ls_create_filter
end function

public subroutine of_get_string_array (ref s_arg_array astr_arg_array[]);BOOLEAN	lb_valid
INTEGER	li_upper, li_counter
STRING   ls_key[], ls_values[], ls_populated_flag[], ls_null[], ls_empty[]

/* Returns array of values from the tab_pages */

SetNull(ls_null)
ls_empty[1] = ''

li_upper = upperbound( THIS.Control[] )

IF li_upper > 0 THEN
	FOR li_counter = 1 TO li_upper
		// make sure there is a valid object
		lb_valid = isvalid(THIS.Control[li_counter])
		IF lb_valid = FALSE THEN RETURN //GET OUT		
		
		THIS.Control[li_counter].DYNAMIC of_create_array(ls_key,ls_values,ls_populated_flag)
		astr_arg_array[li_counter].key = ls_key
		astr_arg_array[li_counter].value = ls_values
		astr_arg_array[li_counter].populated_flag = ls_populated_flag
		
		// reset arrays		
		ls_key       = ls_empty
		ls_values    = ls_empty
		
	NEXT
END IF

end subroutine

event constructor;call super::constructor;THIS.postevent('ue_post_constructor')
end event

event selectionchanged;call super::selectionchanged;IF newindex > 0 THEN
 THIS.Control[newindex].TabBackColor = rgb(160,204,231)
END IF 

IF oldindex > 0 THEN
 THIS.Control[oldindex].TabBackColor = 79741120
END IF 
end event

