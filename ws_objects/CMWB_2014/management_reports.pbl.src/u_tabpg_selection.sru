$PBExportHeader$u_tabpg_selection.sru
$PBExportComments$inherited for u_tabpg
forward
global type u_tabpg_selection from u_tabpg
end type
type cb_array from commandbutton within u_tabpg_selection
end type
type cb_1 from commandbutton within u_tabpg_selection
end type
type st_where from statictext within u_tabpg_selection
end type
type cb_where from commandbutton within u_tabpg_selection
end type
type cb_all_forward from commandbutton within u_tabpg_selection
end type
type cb_forward from commandbutton within u_tabpg_selection
end type
type cb_back from commandbutton within u_tabpg_selection
end type
type cb_all_back from commandbutton within u_tabpg_selection
end type
type dw_items_to_keep from u_dw_online within u_tabpg_selection
end type
type dw_values from u_dw_online within u_tabpg_selection
end type
end forward

global type u_tabpg_selection from u_tabpg
integer width = 2085
integer height = 816
cb_array cb_array
cb_1 cb_1
st_where st_where
cb_where cb_where
cb_all_forward cb_all_forward
cb_forward cb_forward
cb_back cb_back
cb_all_back cb_all_back
dw_items_to_keep dw_items_to_keep
dw_values dw_values
end type
global u_tabpg_selection u_tabpg_selection

forward prototypes
public subroutine of_setup_tabpage (string as_tabpage_text, string as_dataobject)
public function string of_create_where ()
public function boolean of_is_multi_key (string as_item_key)
public function string of_strip_out_key (string as_key)
public function string of_create_filter ()
public subroutine of_clear ()
public function boolean of_check_item_already_there (string as_key)
public subroutine of_create_array (ref string as_key[], ref string as_values[], ref string as_populated_flag[])
end prototypes

public subroutine of_setup_tabpage (string as_tabpage_text, string as_dataobject);IF ISNULL(as_tabpage_text) 	THEN as_tabpage_text = ''
IF ISNULL(as_dataobject) 		THEN as_dataobject = ''

THIS.TEXT = as_tabpage_text

dw_values.settransobject(sqlca)
dw_values.dataobject = as_dataobject
end subroutine

public function string of_create_where ();INTEGER 	li_counter, li_rowcount, li_start_pos,  li_keycounter, li_counter2, li_big_loop
STRING		ls_item_key, ls_item_to_keep,  ls_filter,  ls_key_column[], ls_key_text, ls_key_values[], ls_string_front, ls_special_text, ls_table_name
BOOLEAN 	lb_multi

li_rowcount	= dw_items_to_keep.rowcount()

IF li_rowcount > 0 THEN
	ls_key_text 		= dw_values.object.t_key_column.text 
	ls_special_text 	= dw_values.object.t_special.text //stupid to have to do this!!!!!
	ls_table_name	=  dw_values.object.t_table_name.text  
	ls_table_name = ls_table_name + '.'
END IF 

ls_string_front =  "( "

lb_multi = of_is_multi_key(ls_key_text) 

/* check for multikey -- looks in text for '&' symbol */
IF lb_multi = TRUE THEN 
	li_keycounter 	= 1
	li_start_pos 		= 1
	
	//key columns
	FOR li_counter = 1 TO LEN(ls_key_text)
	
		IF mid(ls_key_text, li_counter, 1) = '@' THEN 
			
			ls_key_column[li_keycounter] = mid(ls_key_text, li_start_pos, li_counter - li_start_pos)
				
			li_keycounter ++
			li_start_pos = li_counter
			
		ELSE
			
			// out on end 
			IF li_counter = LEN(ls_key_text) THEN 
				
				ls_key_column[li_keycounter] = mid(ls_key_text, li_start_pos + 1 , li_counter - li_start_pos)
			
			END IF 
		END IF 
	NEXT
ELSE 	
	
	ls_key_column[1] = ls_key_text
	
END IF 

/**************************************************************************************/
/* NOW DO THE CREATION BASED ON ALL THE ROWS */
FOR li_big_loop = 1 TO li_rowcount
	
	ls_item_key			= dw_items_to_keep.getitemstring(li_big_loop, 'item_key')
	ls_item_to_keep	= dw_items_to_keep.getitemstring(li_big_loop, 'item_to_keep')
	
	IF lb_multi = TRUE THEN  
		
		li_keycounter 	= 1
		li_start_pos 		= 1
		
		//key columns
		FOR li_counter = 1 TO LEN(ls_item_key)
			
			IF mid(ls_item_key, li_counter, 1) = '@' THEN 
					
				ls_key_values[li_keycounter] = mid(ls_item_key, li_start_pos, li_counter - li_start_pos)
						
				li_keycounter ++
				li_start_pos = li_counter
					
			ELSE
					
				// out on end 
				IF li_counter = LEN(ls_item_key) THEN 
						
					ls_key_values[li_keycounter] = mid(ls_item_key, li_start_pos + 1 , li_counter - li_start_pos)
					
				END IF 
			END IF 

		NEXT
	ELSE
			ls_key_values[1] = ls_item_key
	END IF 
	
	
	FOR li_counter2 = 1 TO upperbound(ls_key_values)
			
		IF li_counter2 <> upperbound(ls_key_values)  THEN 
			 ls_filter = ls_filter +  ls_table_name + ls_key_column[li_counter2] + ' = ' + ls_key_values[li_counter2]   + ' and ' 
		ELSE 	
			
			IF isnumber(ls_key_values[li_counter2]) THEN 
				
				IF left(ls_key_values[li_counter2], 1) = '0' THEN //THIS IS STUPID!!!!
					ls_filter =  ls_filter + ls_table_name + ls_key_column[li_counter2] + ' = ' +  '"' + ls_key_values[li_counter2]  +  '"' 
				ELSE
					
					IF ls_special_text = 'S' THEN 
							ls_filter =  ls_filter + ls_table_name + ls_key_column[li_counter2] + ' = ' +  '"' + ls_key_values[li_counter2]  +  '"' 
					ELSE
							ls_filter =  ls_filter + ls_table_name + ls_key_column[li_counter2] + ' = ' +  ls_key_values[li_counter2] 
					END IF 
				END IF 
				
			ELSE
			 	ls_filter =  ls_filter + ls_table_name + ls_key_column[li_counter2] + ' = ' +  '"' + ls_key_values[li_counter2]  +  '"' 
			END IF 
		END IF 
	NEXT		
			
	IF li_big_loop <> li_rowcount THEN 
			ls_filter = ls_filter + " ) or ( " 	
	END IF 
				
NEXT

IF trim(ls_filter) > '' THEN 
	//ls_filter = ls_string_front +  ls_filter + " )"
	ls_filter = "(" + ls_string_front +  ls_filter + " ))"
END IF 

	
RETURN trim(ls_filter)

end function

public function boolean of_is_multi_key (string as_item_key);BOOLEAN lb_contains


lb_contains = Match(as_item_key, "[@]+")

RETURN lb_contains
end function

public function string of_strip_out_key (string as_key);/* key members are denoted by** & ** search for that character */

INTEGER	li_position_of_key, li_len, li_counter, li_count_at, li_array_no, li_start, li_position_array[],li_position_array_no
STRING		ls_key_array[], ls_string

li_position_of_key 	= PosA(as_key, "&")
IF isnull(li_position_of_key) OR li_position_of_key = 0  THEN RETURN ""

li_len 					= LEN(as_key)
IF isnull(li_len) OR li_len = 0  THEN RETURN ""

li_array_no 				= 1
li_start 						= 1
li_position_array_no 	= 1

FOR li_counter = 1 to li_len
	ls_string = left(as_key, li_counter)
	IF MID(ls_string, li_counter, 1) = '&'  THEN
		li_position_array[li_position_array_no] = li_counter
		li_position_array_no ++
	END IF
NEXT

FOR li_counter = 1 TO upperbound(li_position_array)
	li_start = li_position_array[1]
	
	IF li_counter = 1 THEN 
			ls_key_array[li_array_no] = MID(as_key, li_counter, li_start -1 )
			li_array_no ++
		
	END IF 
	
NEXT

//FOR li_counter = 1 to li_len
//	ls_string = left(as_key, li_counter)
//	IF MID(ls_string, li_counter, 1) = '&'  THEN
//		
//		ls_key_array[li_array_no] = 
//		li_array_no ++
//		
//		li_count_at = li_count_at + 1
//	END IF
//NEXT


RETURN ''
end function

public function string of_create_filter ();INTEGER 	li_counter, li_rowcount, li_start_pos,  li_keycounter, li_counter2, li_big_loop
STRING		ls_item_key, ls_item_to_keep,  ls_filter,  ls_key_column[], ls_key_text, ls_key_values[], ls_string_front, ls_special_text
BOOLEAN lb_multi

li_rowcount	= dw_items_to_keep.rowcount()

IF li_rowcount > 0 THEN
	ls_key_text 		= dw_values.object.t_key_column.text 
	ls_special_text 	= dw_values.object.t_special.text //stupid to have to do this!!!!!
END IF 

ls_string_front =  "(( "

lb_multi = of_is_multi_key(ls_key_text) 

/* check for multikey -- looks in text for '&' symbol */
IF lb_multi = TRUE THEN 
	li_keycounter 	= 1
	li_start_pos 		= 1
	
	//key columns
	FOR li_counter = 1 TO LEN(ls_key_text)
	
		IF mid(ls_key_text, li_counter, 1) = '@' THEN 
			
			ls_key_column[li_keycounter] = mid(ls_key_text, li_start_pos, li_counter - li_start_pos)
				
			li_keycounter ++
			li_start_pos = li_counter
			
		ELSE
			
			// out on end 
			IF li_counter = LEN(ls_key_text) THEN 
				
				ls_key_column[li_keycounter] = mid(ls_key_text, li_start_pos + 1 , li_counter - li_start_pos)
			
			END IF 
		END IF 
	NEXT
ELSE 	
	
	ls_key_column[1] = ls_key_text
	
END IF 

/**************************************************************************************/
/* NOW DO THE CREATION BASED ON ALL THE ROWS */
FOR li_big_loop = 1 TO li_rowcount
	
	ls_item_key			= dw_items_to_keep.getitemstring(li_big_loop, 'item_key')
	ls_item_to_keep	= dw_items_to_keep.getitemstring(li_big_loop, 'item_to_keep')
	
	IF lb_multi = TRUE THEN  
		
		li_keycounter 	= 1
		li_start_pos 		= 1
		
		//key columns
		FOR li_counter = 1 TO LEN(ls_item_key)
			
			IF mid(ls_item_key, li_counter, 1) = '@' THEN 
					
				ls_key_values[li_keycounter] = mid(ls_item_key, li_start_pos, li_counter - li_start_pos)
						
				li_keycounter ++
				li_start_pos = li_counter
					
				// if '@' is the last character in ls_key_text, then the second element in the array must be blank
				IF Pos(ls_item_key,'@') = Len(ls_item_key) THEN
					ls_key_values[2] = ''
				END IF
					
			ELSE
					
				// out on end 
				IF li_counter = LEN(ls_item_key) THEN 
						
					ls_key_values[li_keycounter] = mid(ls_item_key, li_start_pos + 1 , li_counter - li_start_pos)
					
				END IF 
			END IF 

		NEXT
	ELSE
		ls_key_values[1] = ls_item_key
	END IF 
	
	
	FOR li_counter2 = 1 TO upperbound(ls_key_values)
			
		IF li_counter2 <> upperbound(ls_key_values) THEN
			IF ls_special_text = 'S' THEN
				ls_filter = ls_filter +  ls_key_column[li_counter2] + ' = ' +'"'+ ls_key_values[li_counter2] +'"'+ ' and ' 
			ELSE
				ls_filter = ls_filter +  ls_key_column[li_counter2] + ' = ' + ls_key_values[li_counter2] + ' and ' 
			END IF
		ELSE 	
			
			IF isnumber(ls_key_values[li_counter2]) THEN 
				
				IF left(ls_key_values[li_counter2], 1) = '0' THEN //THIS IS STUPID!!!!
					ls_filter =  ls_filter + ls_key_column[li_counter2] + ' = ' +  '"' + ls_key_values[li_counter2]  +  '"' 
				ELSE
					
					IF ls_special_text = 'S' THEN 
						ls_filter =  ls_filter + ls_key_column[li_counter2] + ' = ' +  '"' + ls_key_values[li_counter2]  +  '"' 
					ELSE
						ls_filter =  ls_filter + ls_key_column[li_counter2] + ' = ' +  ls_key_values[li_counter2] 
					END IF 
				END IF 
				
			ELSE
			 	ls_filter =  ls_filter + ls_key_column[li_counter2] + ' = ' +  '"' + ls_key_values[li_counter2]  +  '"' 
			END IF 
		END IF 
	NEXT		
			
	IF li_big_loop <> li_rowcount THEN 
		ls_filter = ls_filter + " ) or ( " 	
	END IF 
				
NEXT

IF trim(ls_filter) > '' THEN 
	ls_filter = ls_string_front +  ls_filter + " ))"
END IF 

	
RETURN trim(ls_filter)

end function

public subroutine of_clear ();dw_values.reset()
dw_items_to_keep.reset()

end subroutine

public function boolean of_check_item_already_there (string as_key);INTEGER		li_counter, li_rowcount
STRING			ls_key

li_rowcount = dw_items_to_keep.rowcount()

IF isnull(li_rowcount) OR li_rowcount < 1 THEN RETURN FALSE


FOR li_counter = 1 TO  li_rowcount
	
		ls_key = dw_items_to_keep.getitemstring(li_counter, 'item_key')

		IF trim(ls_key) = trim(as_key) THEN RETURN TRUE

NEXT

RETURN FALSE
end function

public subroutine of_create_array (ref string as_key[], ref string as_values[], ref string as_populated_flag[]);BOOLEAN  lb_multi
INTEGER 	li_counter, li_rowcount, li_start_pos, li_keycounter, li_big_loop, li_len_key_text, li_len_item_key
INTEGER 	li_key_column_counter, li_key_column_upper, li_key_mod
STRING	ls_item_key, ls_key_column[], ls_key_text, ls_key_values[], ls_key_text_char



li_rowcount	= dw_items_to_keep.rowcount()
ls_key_text = dw_values.object.t_key_column.text

lb_multi = of_is_multi_key(ls_key_text) 

/* check for multikey -- looks in text for '@' symbol */
IF lb_multi = TRUE THEN 
	li_keycounter = 1
	li_start_pos  = 1
	
	//key columns
	li_len_key_text = LEN(ls_key_text)
	FOR li_counter = 1 TO li_len_key_text
		
		ls_key_text_char = mid(ls_key_text, li_counter, 1)
		IF ls_key_text_char = '@' THEN
			ls_key_column[li_keycounter] = mid(ls_key_text, li_start_pos, li_counter - li_start_pos)
				
			li_keycounter ++
			li_start_pos = li_counter			
		ELSE			
			// out on end 
			IF li_counter = LEN(ls_key_text) THEN				
				ls_key_column[li_keycounter] = mid(ls_key_text, li_start_pos + 1, li_counter - li_start_pos)
			END IF 
		END IF 
	NEXT	
ELSE	
	ls_key_column[1] = ls_key_text
END IF

li_key_column_upper = UpperBound(ls_key_column)

/**************************************************************************************/
/* NOW DO THE CREATION BASED ON ALL THE ROWS */
IF li_rowcount > 0 THEN
	li_keycounter = 1
	li_key_mod    = 1
	
	FOR li_big_loop = 1 TO li_rowcount
		
		ls_item_key			= dw_items_to_keep.getitemstring(li_big_loop, 'item_key')
		
		IF lb_multi = TRUE THEN
			li_start_pos  = 1
			
			//key columns
			li_len_item_key = LEN(ls_item_key)
			FOR li_counter = 1 TO li_len_item_key
				IF mid(ls_item_key, li_counter, 1) = '@' THEN
					as_key[li_keycounter]    = ls_key_column[li_key_mod]
					as_values[li_keycounter] = mid(ls_item_key, li_start_pos, li_counter - li_start_pos)
					as_populated_flag[li_keycounter] = 'Y'
							
					li_keycounter ++
					li_key_mod = Mod(li_keycounter,li_key_column_upper)
					IF li_key_mod = 0 THEN li_key_mod = li_key_column_upper
					li_start_pos = li_counter
					
					// if there is nothing after the delineator (@)
					IF li_counter = li_len_item_key THEN
						as_key[li_keycounter]    = ls_key_column[li_key_mod]					
						as_values[li_keycounter] = ''
						as_populated_flag[li_keycounter] ='Y'
						li_keycounter ++
						li_key_mod = Mod(li_keycounter,li_key_column_upper)
						IF li_key_mod = 0 THEN li_key_mod = li_key_column_upper
					END IF
					
				ELSE					
					// out on end 
					IF li_counter = li_len_item_key THEN
						as_key[li_keycounter]            = ls_key_column[li_key_mod]					
						as_values[li_keycounter]         = mid(ls_item_key, li_start_pos + 1 , li_counter - li_start_pos)
						as_populated_flag[li_keycounter] = 'Y'
						li_keycounter ++
						li_key_mod = Mod(li_keycounter,li_key_column_upper)
						IF li_key_mod = 0 THEN li_key_mod = li_key_column_upper
					END IF
				END IF
			NEXT
		ELSE
			as_key[li_big_loop]            = ls_key_column[1]
			as_values[li_big_loop]         = ls_item_key
			as_populated_flag[li_big_loop] = 'Y'
		END IF	
	NEXT
ELSE	
	FOR li_key_column_counter = 1 TO li_key_column_upper
		as_key[li_key_column_counter]            = ls_key_column[li_key_column_counter]
		as_values[li_key_column_counter]         = ''
		as_populated_flag[li_key_column_counter] = 'N'
	NEXT
END IF
end subroutine

on u_tabpg_selection.create
int iCurrent
call super::create
this.cb_array=create cb_array
this.cb_1=create cb_1
this.st_where=create st_where
this.cb_where=create cb_where
this.cb_all_forward=create cb_all_forward
this.cb_forward=create cb_forward
this.cb_back=create cb_back
this.cb_all_back=create cb_all_back
this.dw_items_to_keep=create dw_items_to_keep
this.dw_values=create dw_values
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_array
this.Control[iCurrent+2]=this.cb_1
this.Control[iCurrent+3]=this.st_where
this.Control[iCurrent+4]=this.cb_where
this.Control[iCurrent+5]=this.cb_all_forward
this.Control[iCurrent+6]=this.cb_forward
this.Control[iCurrent+7]=this.cb_back
this.Control[iCurrent+8]=this.cb_all_back
this.Control[iCurrent+9]=this.dw_items_to_keep
this.Control[iCurrent+10]=this.dw_values
end on

on u_tabpg_selection.destroy
call super::destroy
destroy(this.cb_array)
destroy(this.cb_1)
destroy(this.st_where)
destroy(this.cb_where)
destroy(this.cb_all_forward)
destroy(this.cb_forward)
destroy(this.cb_back)
destroy(this.cb_all_back)
destroy(this.dw_items_to_keep)
destroy(this.dw_values)
end on

event constructor;call super::constructor;/* put special instructions in the a powerobject as a parameter */
THIS.text 	= Message.StringParm

STRING		ls_tab_name
INTEGER	li_selected_tab

dw_values.dataobject = Message.StringParm
dw_values.settransobject(sqlca)

dw_values.retrieve()
SQLCA.nf_handle_error("u_tabpg_selection","dw_values","retrieve()")

/* anything datawindow related will be encapsulated in the dataobject and used now */
ls_tab_name = dw_values.object.t_tab_name.text 

/* place the name on the tabpage saved in the dataobject */
THIS.text 	= ls_tab_name






end event

type cb_array from commandbutton within u_tabpg_selection
boolean visible = false
integer x = 978
integer y = 636
integer width = 101
integer height = 104
integer taborder = 30
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "A"
end type

event clicked;INTEGER 	         li_counter, li_upper
STRING            ls_item_key[], ls_value[], ls_populated_flag[]
S_WINDOW_MESSAGE  lstr_message



of_create_array(ls_item_key[], ls_value[], ls_populated_flag[])

li_upper = UpperBound(ls_value)
FOR li_counter = 1 TO li_upper
	MESSAGEBOX('key',      ls_item_key[li_counter])
	MESSAGEBOX('value',    ls_value[li_counter])
	MESSAGEBOX('pop flag', ls_populated_flag[li_counter])
NEXT

end event

type cb_1 from commandbutton within u_tabpg_selection
boolean visible = false
integer x = 978
integer y = 128
integer width = 110
integer height = 104
integer taborder = 30
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "W"
end type

event clicked;INTEGER 	li_counter
STRING		ls_item_key, ls_item_to_keep, ls_table_name, ls_where, ls_column_name

ls_where = of_create_where()
MESSAGEBOX('where', ls_where)

st_where.text = ls_where
end event

type st_where from statictext within u_tabpg_selection
integer x = 978
integer y = 728
integer width = 101
integer height = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean focusrectangle = false
end type

type cb_where from commandbutton within u_tabpg_selection
boolean visible = false
integer x = 978
integer y = 20
integer width = 101
integer height = 104
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "F"
end type

event clicked;INTEGER 	li_counter
STRING		ls_item_key, ls_item_to_keep, ls_table_name, ls_where, ls_column_name

//FOR li_counter = 1 TO dw_items_to_keep.rowcount()
//	
//	ls_item_key			= dw_items_to_keep.getitemstring(li_counter, 'item_key')
//	ls_item_to_keep	= dw_items_to_keep.getitemstring(li_counter, 'item_to_keep')
//	ls_table_name		= dw_items_to_keep.getitemstring(li_counter, 'table_name')
//
//	IF li_counter = 1 THEN 
//		ls_where =  ls_item_key 	
//	ELSE
//		ls_where = ls_where + " , " + ls_item_key	
//	END IF	
//	
//NEXT
//
//ls_column_name 	= ls_table_name + "_code"
//ls_where 				= " and " +  ls_table_name + "." + ls_column_name + " in ( "+ ls_where  + " )"
//
ls_where = of_create_filter()
MESSAGEBOX('filter', of_create_filter())

st_where.text = ls_where


end event

type cb_all_forward from commandbutton within u_tabpg_selection
integer x = 978
integer y = 288
integer width = 101
integer height = 104
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = ">>"
end type

event clicked;INTEGER		li_row, li_counter
STRING			ls_item_key, ls_item_to_keep, ls_table_name

ls_table_name	= dw_values.object.t_table_name.text

FOR li_counter = 1 TO dw_values.rowcount()
			
	ls_item_key	 		= dw_values.getitemstring(li_counter, 'item_key')
	ls_item_to_keep	= dw_values.getitemstring(li_counter, 'item_to_keep')
			
	//dont put dupes in
	IF of_check_item_already_there(ls_item_key) = TRUE THEN CONTINUE
	
	li_row 				= dw_items_to_keep.insertrow(0)
			
	dw_items_to_keep.setitem(li_row, 'item_key', ls_item_key)
	dw_items_to_keep.setitem(li_row, 'item_to_keep', ls_item_to_keep)
	dw_items_to_keep.setitem(li_row, 'table_name', ls_table_name)
				
NEXT
end event

type cb_forward from commandbutton within u_tabpg_selection
integer x = 978
integer y = 184
integer width = 101
integer height = 104
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = ">"
end type

event clicked;INTEGER		li_row, li_counter
STRING			ls_item_key, ls_item_to_keep, ls_table_name

ls_table_name	 		= dw_values.object.t_table_name.text

FOR li_counter = 1 TO dw_values.rowcount()
   IF  dw_values.IsSelected(li_counter) THEN 
			
		ls_item_key	 			= dw_values.getitemstring(li_counter, 'item_key')
		ls_item_to_keep	 	= dw_values.getitemstring(li_counter, 'item_to_keep')
		
		//dont put dupes in
		IF of_check_item_already_there(ls_item_key) = TRUE THEN CONTINUE
				
		li_row = dw_items_to_keep.insertrow(0)
			
		dw_items_to_keep.setitem(li_row, 'item_key', ls_item_key)
		dw_items_to_keep.setitem(li_row, 'item_to_keep', ls_item_to_keep)
		dw_items_to_keep.setitem(li_row, 'table_name', ls_table_name)
				
	END IF 	
NEXT

//deselect all rows
dw_items_to_keep.SelectRow(0, false)






end event

type cb_back from commandbutton within u_tabpg_selection
integer x = 978
integer y = 400
integer width = 101
integer height = 104
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "<"
end type

event clicked;INTEGER		li_counter, li_selected_row, li_selectedcount, li_deleted

FOR li_counter = 1 TO dw_items_to_keep.rowcount()
   IF  dw_items_to_keep.IsSelected(li_counter) = TRUE THEN 		
		li_selectedcount ++		
	END IF 	
NEXT

li_selected_row = dw_items_to_keep.GetSelectedRow(0)
IF li_selected_row = 0 THEN RETURN

/* Get the category id for selected rows. */
DO WHILE li_selected_row > 0

	dw_items_to_keep.deleterow(li_selected_row)		
	li_deleted ++
	
	IF li_deleted = li_selectedcount  THEN EXIT

	/* Get the next selected row.*/
	li_selected_row = dw_items_to_keep.GetSelectedRow(0)
LOOP
end event

type cb_all_back from commandbutton within u_tabpg_selection
integer x = 978
integer y = 504
integer width = 101
integer height = 104
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "<<"
end type

event clicked;INTEGER		li_counter, li_rowcount

dw_items_to_keep.accepttext()

dw_items_to_keep.setredraw(false)

li_rowcount = dw_items_to_keep.rowcount()

dw_items_to_keep.Reset()

dw_items_to_keep.setredraw(true)
end event

type dw_items_to_keep from u_dw_online within u_tabpg_selection
integer x = 1093
integer width = 983
integer height = 808
integer taborder = 10
string dragicon = "Form!"
string dataobject = "d_items_to_keep"
boolean vscrollbar = true
end type

event constructor;call super::constructor;THIS.uf_setselect(3)
end event

event rbuttondown;//M_DW_ONLINE_RMB_POPUP lm_popup
///*	
//	create the menu
//*/
//
//
//	lm_popup = Create m_dw_online_rmb_popup
//	lm_popup.mf_set_datawindow(This)
//	
//	lm_popup.m_options.m_sort.visible = TRUE
//	lm_popup.m_options.PopMenu(w_frame.PointerX( ), w_frame.PointerY( ))
//	Destroy lm_popup
end event

event dragdrop;call super::dragdrop;DataWindow ldw_Source

IF source.TypeOf() = DataWindow! THEN

        ldw_Source = source

        IF row > 0 THEN

           cb_forward.triggerevent(clicked!)

        END IF

END IF

end event

type dw_values from u_dw_online within u_tabpg_selection
integer width = 965
integer height = 808
integer taborder = 10
boolean hscrollbar = true
boolean vscrollbar = true
end type

event constructor;call super::constructor;THIS.uf_setselect(3)
end event

event rbuttondown;call super::rbuttondown;//M_DW_ONLINE_RMB_POPUP lm_popup
///*	
//	create the menu
//*/
//
//
//	lm_popup = Create m_dw_online_rmb_popup
//	lm_popup.mf_set_datawindow(This)
//	
//	lm_popup.m_options.m_sort.visible = TRUE
//	lm_popup.m_options.PopMenu(w_frame.PointerX( ), w_frame.PointerY( ))
//	Destroy lm_popup
end event

event dragdrop;call super::dragdrop;cb_back.triggerevent(clicked!)
end event

