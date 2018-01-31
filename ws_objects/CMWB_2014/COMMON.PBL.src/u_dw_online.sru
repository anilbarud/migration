$PBExportHeader$u_dw_online.sru
$PBExportComments$Datawindow user object for on-line applications - (Inherited from u_dwa)
forward
global type u_dw_online from u_dwa
end type
end forward

global type u_dw_online from u_dwa
integer height = 361
event we_keydown pbm_keydown
event ue_more_details pbm_custom05
event ue_print pbm_custom06
event ue_filter pbm_custom07
event ue_post_losefocus pbm_custom08
event ue_itemchangeaccepted ( long al_row,  string as_column_name )
event dwmousemove pbm_dwnmousemove
event lbuttondown pbm_lbuttondown
event lbuttonup pbm_lbuttonup
event rbuttonup pbm_dwnrbuttonup
event keydown pbm_dwnkey
event type integer ue_rowselecting ( long al_row )
event type integer ue_rowselected ( long al_row )
event ue_paste ( )
event ue_cut ( )
event ue_copy ( )
event ue_delete ( )
event ue_selectall ( )
end type
global u_dw_online u_dw_online

type variables

Protected:
Integer		ii_select_behaviour = 0
Long		il_anchor_row
Boolean          	ib_supress_pbmessage = TRUE
INTEGER		ii_DwID
STRING		is_column_name
public:
//services objects
n_sort		inv_sort
n_tooltip	inv_tooltip
n_filter		inv_filter
n_rowselection		inv_rowselection

CONSTANT		INTEGER	RS_NONE		= 0
CONSTANT		INTEGER	RS_SINGLE 	= 1
CONSTANT   	INTEGER	RS_MULTIPLE = 2
CONSTANT   	INTEGER	RS_EXTENDED = 3
end variables

forward prototypes
public subroutine uf_set_pbmessage (boolean ab_state)
public function integer uf_accept_dw ()
public function integer uf_processselect (long al_row, string as_input_type)
public subroutine uf_protect_allattributes (boolean ab_protect)
public subroutine uf_protectcolumn (string as_column_name, boolean ab_protect)
public function integer uf_datachanged ()
public subroutine uf_set_backcolor ()
public subroutine uf_trigger_itemchangeaccepted (long al_row, string as_column_name)
public function integer uf_setsort (boolean ab_switch)
public function integer uf_settooltip (boolean ab_switch)
public function integer uf_setfilter (boolean ab_switch)
public function string uf_protectcolumn (string as_column_name, string as_unprotected_expression)
public function integer uf_setselect (integer ai_select_behaviour)
public function integer setitem (long r, readonly string c, any v)
public function integer uf_validate_dwo (dwobject adwo_datawindowobject, string as_data)
end prototypes

event we_keydown;
	If KeyDown(KeyHome!) and RowCount() > 0 Then
		SetRow(1)
		ScrollToRow(1)
	ElseIf KeyDown(KeyEnd!) and RowCount() > 0 Then
		SetRow(RowCount())
		ScrollToRow(RowCount())
	End If

end event

on ue_print;call u_dwa::ue_print;This.Print()
end on

event ue_itemchangeaccepted;/*	Occurs after a field in a DataWindow control has been modified 
	and the change has been accepted into the DataWindow buffer. The 
	companion event ItemChanged occurs before the change is applied 
	to the item and ItemChangeAccepted occurs afterward.
*/
end event

event dwmousemove;integer li_X, li_Y
string ls_Text

//If isValid(inv_tooltip) THen
//	If ii_DwId = 0 Then Return 0
//	
//	If dwo.Type = "column" or dwo.type = "compute" Then
//		If String( dwo.Name ) <> is_column_name Then
//			is_column_name = String( dwo.Name )
//			ls_Text = This.Describe( is_column_name + ".tag" )
//			If ls_text <> '' and ls_text <> '?' Then
//				inv_Tooltip.of_UpdatetipRect( This, ii_dwID,0,0,UnitsToPixels(this.width,xUnitsToPixels!),UnitsToPixels(this.height,yUnitsToPixels!))
//				inv_Tooltip.of_SetTipText( This, ii_dwID, ls_Text )
//				 
//			ELSE
//				inv_Tooltip.of_SetTipText( This, ii_dwID, '' )
//			End if
//		End if
//	Else
//		inv_Tooltip.of_SetTipText( This, ii_dwID, '' )
//	End if
//End if

If isValid(inv_tooltip) THen
	If ii_DwId = 0 Then Return 0
	
	If dwo.Type = "column" or dwo.type = "compute" or dwo.type = "text" OR dwo.type = "button" OR dwo.type = "bitmap" Then
		//If the cursor is outside the tip rectangle then
		If (ypos < inv_tooltip.il_top or ypos > inv_tooltip.il_bottom) &
			or (xpos < inv_tooltip.il_left or xpos > inv_tooltip.il_right) Then
			//If String( dwo.Name ) <> is_column_name Then
				
				ls_Text = This.Describe( String( dwo.Name ) + ".tag" )
				If ls_text <> '' and ls_text <> '?' Then

					is_column_name = String( dwo.Name )
					If xpos - 10 >=0 Then
						xpos = xpos - 10
					End if
					IF ypos - 10 >= 0 Then
						ypos = ypos - 10
					End if
					
					li_X = xpos
					li_Y = ypos
					
					inv_Tooltip.of_UpdatetipRect( This, ii_dwID, &
												li_X, &
												li_Y, &
												li_X + 20, &
												li_Y + 20 )
					inv_Tooltip.of_SetTipText( This, ii_dwID, ls_Text )
				End if 
			//End If
		End if
	End If
End if


//If isValid(inv_tooltip) THen
//	If ii_DwId = 0 Then Return 0
//	
//	If dwo.Type = "column" Then
//		If String( dwo.Name ) <> is_column_name Then
//			is_column_name = String( dwo.Name )
//			ls_Text = This.Describe( is_column_name + ".tag" )
//			If ls_text <> '' and ls_text <> '?' Then
//				li_X = UnitsToPixels( Integer( dwo.X ),XUnitsToPixels! )
//				li_Y = UnitsToPixels( Integer( dwo.Y ), YUnitsToPixels! )
//				inv_Tooltip.of_UpdatetipRect( This, ii_dwID, &
//											li_X, &
//											li_Y, &
//											li_X + UnitsToPixels( Integer( dwo.Width ), XUnitsToPixels! ), &
//											li_Y + UnitsToPixels( Integer( dwo.Height ), YUnitsToPixels! ) )
//				inv_Tooltip.of_SetTipText( This, ii_dwID, ls_Text )
//			End if 
//		End If
//	End If
//End if

Return 0
end event

event lbuttondown;IF IsValid(inv_rowselection) Then
	inv_rowselection.event pfc_lbuttondown(flags,xpos,ypos)
End if
end event

event lbuttonup;If IsValid(inv_rowselection) Then
	inv_rowselection.event pfc_lbuttonup(flags,xpos,ypos)
ENd if
end event

event rbuttonup;If IsValid(inv_rowselection) Then
	inv_rowselection.event pfc_rbuttonup(xpos,ypos,row,dwo)
End if
end event

event keydown;
IF IsValid(inv_rowselection) THEN
	If KeyDown(KeyHome!) and RowCount() > 0 Then
		SetRow(1)
	ElseIf KeyDown(KeyEnd!) and RowCount() > 0 Then
		SetRow(RowCount())
	End If
END IF

end event

event ue_paste();Paste()
end event

event ue_cut();Cut()
end event

event ue_copy();Copy()
end event

event ue_delete();Clear()
end event

event ue_selectall();SelectText ( 1, 1000 ) // number should be high enough to select everything
end event

public subroutine uf_set_pbmessage (boolean ab_state);ib_supress_pbmessage = ab_state
end subroutine

public function integer uf_accept_dw ();/* ----------------------------------------------------------------------------------------------------	*/
/*	Function Name: 	uf_accept_dw																								*/
/*																																			*/
/*	Purpose:				This function validates the datawindow.  															*/
/*																																			*/
/*							It first does an AcceptText to ensure that the last column passes validations.  		*/
/*																																			*/
/*							Then, it checks to see if all required fields have been entered. Note: It only 		*/
/*							checks rows which have been inserted or modified. Note: If you want empty strings,	*/
/*							detected, you have clicked 'Empty string is NULL'.												*/
/*																																			*/
/*							This function is normally called from the clicked event of cb_save.						*/
/*																																			*/
/*	Arguments:			None																											*/
/*																																			*/
/*	Return Value:		Integer	-	Success				-	1																	*/
/*										-	Failure				-	-1																	*/
/*																																			*/
/* ----------------------------------------------------------------------------------------------------	*/

Boolean	lb_old_value
Long		ll_result,				ll_row = 1
Window	lw_parent_window
Integer	li_col = 1
String	ls_column_name,		ls_column_text_name


/*	Do an accept text to ensure that the last field entered passes the validation tests.  If not, set the
	focus back to the datawindow and return with a failure status */

If AcceptText() = -1 Then
	SetFocus()
	Return -1
End If



/*	Loop to check all rows. If 'FindRequired' does not return a row of 0, then a required column
	was found without a value */


Do While ll_row <> 0

	FindRequired(primary!,ll_row,li_col,ls_column_name,TRUE)

	If ll_row <> 0 Then
	
		lw_parent_window = parent

		/* Try to figure out the column's title as displayed in the datawindow */

		ls_column_text_name = ls_column_name + "_t.Text"
		ls_column_text_name = Describe(ls_column_text_name)
		If ls_column_text_name = "!" or ls_column_text_name = "" Then 
			ls_column_text_name = ls_column_name
		Else
			If right(ls_column_text_name,1) = ":" Then ls_column_text_name = left(ls_column_text_name,len(ls_column_text_name)-1)
		End IF

		/* Display an error message */

		MessageBox(lw_parent_window.title,"Please enter a value for '" + ls_column_text_name + "'",Exclamation!)


		/*	Set the focus back to the appropriate row and column so that a value can be entered.  Note, we are
			also supressing powerbuilder's error message here as we have already displayed one. */

		lb_old_value = ib_supress_pbmessage
		ib_supress_pbmessage = True
		SetColumn(li_col)
		SetRow(ll_row)
		ScrollToRow(ll_row)
		SetFocus()
		ib_supress_pbmessage = lb_old_value
		Return -1
	End If

Loop


Return 1
end function

public function integer uf_processselect (long al_row, string as_input_type);/* ----------------------------------------------------------------------------------------------------	*/
/*	Function Name: 	uf_ProcessSelect																							*/
/*																																			*/
/*	Purpose:				This function processes the select behaviour.  You tell the function the row you 	*/
/*							want to process and whether processing coming via the keyboard or the mouse.			*/
/*																																			*/
/*	Arguments:			Long	-	al_row			-	The row to process													*/
/*							String-	as_input_type	-	Whether the processing is coming from a keyboard or mouse*/
/*																																			*/
/* ----------------------------------------------------------------------------------------------------	*/


Long		ll_row
Boolean	lb_reset_anchor
Boolean	lb_keyboard,	lb_mouse


//	Is this keyboard or mouse action?

	If Upper(left(as_input_type,1)) = "K" then
		lb_keyboard = TRUE
	Else
		lb_mouse = TRUE
	End IF


//	Make sure the user clicked on row, not a footer, header, etc.

	If al_row < 1 Then Return -1

//	Sometimes we may not want to reset the anchor_row variable, so 
//	create a boolean to hold that fact

	lb_reset_anchor = TRUE

//	Things go faster with Redraw false!

	SetRedraw(FALSE)

//	Valid select behaviours
//	0		No Select Allowed
//	1		One Row Select
//	2		Automatic Multiple Select
//	3		Full Keyboard Select with click/shift click
//	99		No Select Allowed; Use hand indicator


	Choose Case ii_select_behaviour

	CASE 0, 99

	//	Do nothing.

	CASE 1

		SelectRow(0,FALSE)
		SelectRow(al_row,TRUE)

	CASE 2

	//	This only runs if the user clicks.  Kinda srange to do with the keyboard

		If lb_mouse then
			SelectRow(al_row, not isselected(al_row))
		End If

	CASE 3

	If keydown (KeyShift!) and keydown (KeyControl!) Then

		//	Shift and control: add to current selection

		If il_anchor_row > al_row then
			For ll_row = il_anchor_row to al_row step - 1
				this.selectrow(ll_row,true)
			next
		Else
			for ll_row = il_anchor_row to al_row
				this.selectrow(ll_row,true)
			next
		end If
	ElseIf keydown(keyshift!) Then

		//	Clear current selection then add new rows

		SelectRow(0,FALSE)
		If il_anchor_row > al_row then
			For ll_row = il_anchor_row to al_row step -1
				this.selectrow(ll_row,true)
			next
		Else
			For ll_row = il_anchor_row to al_row
				this.selectrow(ll_row,true)
			next
		End If

		lb_reset_anchor = false

	ElseIf Keydown(keyControl!) then

		//	toggle selected on and off

		SelectRow(al_row, not isselected(al_row))

	Else

		//	For a normal click, clear out existing selection 

		SelectRow(0,False)
		SelectRow(al_row,True)
	End If
End Choose

SetRedraw(True)

If lb_reset_anchor then il_anchor_row = al_row

return 0
end function

public subroutine uf_protect_allattributes (boolean ab_protect);INTEGER li_col_count, li_protect, li_loop

IF ab_protect THEN
   li_protect = 1
ELSE
   li_protect = 0
END IF

li_col_count = Integer(THIS.Describe("Datawindow.Column.Count"))

FOR li_loop = 1 TO li_col_count
   THIS.Modify("#" + String(li_loop) + ".Protect=" + String(li_protect))
NEXT



end subroutine

public subroutine uf_protectcolumn (string as_column_name, boolean ab_protect);INTEGER li_col_count, li_protect, li_loop

IF ab_protect THEN
   li_protect = 1
ELSE
   li_protect = 0
END IF
THIS.Modify( String(as_column_name) + ".Protect=" + String(li_protect))




end subroutine

public function integer uf_datachanged ();/* ----------------------------------------------------------------------------------------------------	*/
/*	Function Name: 	uf_datachanged																							   */
/*																																			*/
/*	Purpose:				This function reports the number rows that have been modified and/or deleted in a   */
/*							DataWindow.																									*/
/*																																			*/
/*	Return Value:		Number of rows modified or deleted or -1 if it fails											*/
/*																																			*/
/* ----------------------------------------------------------------------------------------------------	*/

Return this.ModifiedCount() + this.DeletedCount()

end function

public subroutine uf_set_backcolor ();INTEGER li_col_count,  li_loop

li_col_count = Integer(THIS.Describe("Datawindow.Column.Count"))

FOR li_loop = 1 TO li_col_count
   THIS.Modify("#" + String(li_loop) + ".Background.Color= '553648127'" )
NEXT



end subroutine

public subroutine uf_trigger_itemchangeaccepted (long al_row, string as_column_name);/**************************************************************************/
//		Before trigering the ItemChangeAccepted event, make sure the 
//		column was actually modified.  If it wasn't modified, in other words
//		a 1 or 2 was returned in the ItemChanged event, then the item wasn't
//		changed so don't fire the ItemChangeAccepted event.
//
//		Initially this was added to do things like filtering.  Validation
//		should still be done in the ItemChanged event.
/**************************************************************************/

If this.GetItemStatus(al_row,as_column_name,Primary!) = DataModified! Then
	this.event post ue_ItemChangeAccepted(al_row,as_column_name)
End if
end subroutine

public function integer uf_setsort (boolean ab_switch);If IsNull(ab_switch) Then
	Return -1
End If

IF ab_Switch THEN
	IF IsNull(inv_Sort) Or Not IsValid (inv_Sort) THEN
		inv_Sort = Create n_sort
		inv_Sort.of_SetRequestor ( this )
		Return 1
	END IF
ELSE 
	IF IsValid (inv_Sort) THEN
		Destroy inv_Sort
		Return 1
	END IF	
END IF

Return 0
end function

public function integer uf_settooltip (boolean ab_switch);If IsNull(ab_switch) Then
	Return -1
End If

IF ab_Switch THEN
	IF IsNull(inv_ToolTip) Or Not IsValid (inv_ToolTip) THEN
		inv_ToolTip = Create n_tooltip
		// Save ID to update text and rect (code in datawindow)
		//	.. and be able to remove the tooltip by calling of_RemoveTool
		ii_dwID = inv_ToolTip.of_AddTool( this, "" , 0 )
		inv_Tooltip.of_SetMaxWidth( 1500 )
		Return 1
	END IF
ELSE 
	IF IsValid (inv_ToolTip) THEN
		Destroy inv_ToolTip
		Return 1
	END IF	
END IF

Return 0
end function

public function integer uf_setfilter (boolean ab_switch);//Check arguments
If IsNull(ab_switch) Then
	Return -1
End If

IF ab_Switch THEN
	IF IsNull(inv_filter) Or Not IsValid (inv_Filter) THEN
		inv_filter = Create n_filter
		inv_filter.of_SetRequestor ( this )
		Return 1
	END IF
ELSE 
	IF IsValid (inv_filter) THEN
		Destroy inv_filter
		Return 1
	END IF	
END IF

Return 0
end function

public function string uf_protectcolumn (string as_column_name, string as_unprotected_expression);LONG		ll_pos
INTEGER	li_start = 1
STRING	ls_return
STRING	ls_column_style

DO
	ll_pos = POS(as_unprotected_expression,"'",li_start)
	If ll_pos <> 0 Then
		If mid(as_unprotected_expression,li_start - 1, 1) <> "~~" THEN
			as_unprotected_expression = Replace(as_unprotected_expression,ll_pos,1,"~~'")
			li_start = ll_pos + 2
		End if	
	End if
LOOP UNTIL ll_pos = 0


//ls_column_style = this.Describe(as_column_name + '.edit.style')
//
//IF ls_column_style <> 'dddw' and ls_column_style <> 'ddlb' THEN
//	ls_return = This.Modify(as_column_name + ".border = '5~tIf(" + as_unprotected_expression + ",5,0)'")
//	If ls_return <> '' Then SignalError(-666,"Column: " + as_column_name+ " Expression: " + as_unprotected_expression + "Error: " +ls_return)
//END IF	
//	
ls_return = This.Modify(as_column_name + ".protect = '1~tIf(" + as_unprotected_expression + ",0,1)'")
If ls_return <> '' Then SignalError(-666,"Column: " + as_column_name+ " Expression: " + as_unprotected_expression + "Error: " +ls_return)

//ls_return = This.Modify(as_column_name + ".background.mode = '0~tIf(" + as_unprotected_expression + ",0,1)'")
//If ls_return <> '' Then SignalError(-666,"Column: " + as_column_name+ " Expression: " + as_unprotected_expression + "Error: " +ls_return)
//

RETURN ls_return
end function

public function integer uf_setselect (integer ai_select_behaviour);
If IsNUll(ai_select_behaviour) Then
	Return -1
End If

IF ai_select_behaviour > 0 THEN
	IF IsNull(inv_RowSelection) Or Not IsValid (inv_RowSelection) THEN
		inv_RowSelection = Create n_RowSelection
		inv_RowSelection.of_SetRequestor ( this )
		inv_RowSelection.of_SetStyle(ai_select_behaviour)
		ii_select_behaviour = ai_select_behaviour
		Return 1
	END IF
ELSEIF ai_select_behaviour = 0 THEN 
	IF IsValid (inv_RowSelection) THEN
		Destroy inv_RowSelection
		Return 1
	END IF	
END IF

Return 0		

end function

public function integer setitem (long r, readonly string c, any v);INTEGER		li_rtn

li_rtn = Super::SetItem(r,c,v)
IF li_rtn < 1 or IsNull(li_rtn) Then
	SignalError(-666,This.GetParent().ClassName() + ' ' + 'EVENT: SETITEM: ROW ' + STRING(r) + ' COLUMN ' + c)
End if

return 1

end function

public function integer uf_validate_dwo (dwobject adwo_datawindowobject, string as_data);STRING	ls_coltype, ls_coltype_trimmed
STRING	ls_year, ls_month, ls_day, ls_date_problem
INTEGER	li_collength , li_allowed_collength, li_data
LONG		ll_data
DECIMAL	ldec_data
REAL		lr_data
DOUBLE	ldbl_data
UINT		luint_data

ls_coltype = lower(THIS.Describe(adwo_datawindowobject.name + ".Coltype"))

IF left(ls_coltype,4) = 'char' THEN
	ls_coltype_trimmed = 'char'
ELSEIF left(ls_coltype,3) = 'dec' THEN
	ls_coltype_trimmed = 'dec'
ELSE
	ls_coltype_trimmed = ls_coltype
END IF

CHOOSE CASE ls_coltype_trimmed
					
	CASE 'datetime','date'
		IF IsDate(as_data) THEN
			//OK
			RETURN 2
		ELSE			
			ls_year = Left(as_data,4)
			IF ls_year = '0000' THEN
				ls_date_problem = 'The year entered was "0000". Enter a non-zero value'
			END IF
				
			ls_month = Mid(as_data, 6,2)
			IF ls_month = '00' THEN
				IF ls_date_problem = '' THEN
					ls_date_problem = 'The month entered was "00". Enter a value from "01" to "12"'
				ELSE
					ls_date_problem = ls_date_problem + ', the month entered was "00"'
				END IF
			END IF
				
			ls_day = Mid(as_data, 9,2)
			IF ls_day = '00' THEN
				IF ls_date_problem = '' THEN
					ls_date_problem = 'The day entered was "00"'
				ELSE
					ls_date_problem = ls_date_problem + ' day entered was "00". Enter a value from "01" to "31"'
				END IF
			END IF
			
			IF ls_date_problem <> '' THEN
				ls_date_problem = ls_date_problem + '.'
				
				MessageBox('Data Entry Error','The date value you have entered'&
							+'~n('+Left(as_data,10)+') is not valid.'&
							+'~n' + ls_date_problem &
							+'~nPlease change or cancel the entry.')
				RETURN 1
			ELSE
				// assumed that this came from itemchanged
				RETURN 2
			END IF
		END IF
					
	CASE 'char'
		
		li_allowed_collength = Integer(  Mid(ls_coltype,6, Len(ls_coltype) - 6)  )
		ls_coltype = 'char'
		li_collength = Len(as_data)
		IF li_allowed_collength < li_collength THEN
			MessageBox('Data Entry Error','The text value you have entered'&
		 				+'~n('+as_data+') is not valid.'&
						+'~nThe length of your data entry is ' +String(li_collength)+ ' and the application only allows for ' +String(li_allowed_collength)+ ' characters.'&
						+'~nPlease change or cancel the entry.')
			RETURN 1
		ELSE
			// assumed that this came from itemchanged
			RETURN 2
		END IF
					
	CASE 'number'
		IF IsNumber(as_data) THEN
			//OK
			RETURN 2
		ELSE
			MessageBox('Data Entry Error','The value you have entered'&
						+'~n('+as_data+') is not valid.'&
						+'~nPlease change or cancel the entry.')
			RETURN 1
		END IF
					
	CASE 'int','integer'
		IF IsNumber(as_data) THEN
			// continue
			IF String(Integer(as_data)) = as_data THEN
				//OK
				RETURN 2
			ELSE
				MessageBox('Data Entry Error','The numeric value you have entered'&
						+'~n('+as_data+') is not valid.'&
						+'~nThe number entered cannot be greater than 32767 or less than -32768.'&
						+'~nPlease change or cancel the entry.')
				RETURN 1
			END IF
		ELSE
			MessageBox('Data Entry Error','The value you have entered'&
						+'~n('+as_data+') must be numeric.'&
						+'~nPlease change or cancel the entry.')
			RETURN 1
		END IF
					
	CASE 'long'
		IF IsNumber(as_data) THEN
			// continue
			IF String(Long(as_data)) = as_data THEN
				//OK
				RETURN 2
			ELSE
				MessageBox('Data Entry Error','The numeric value you have entered'&
						+'~n('+as_data+') is not valid.'&
						+'~nThe number entered cannot be greater than 2,147,483,647 or less than -2,147,483,648.'&
						+'~nPlease change or cancel the entry.')
				RETURN 1
			END IF
		ELSE
			MessageBox('Data Entry Error','The value you have entered'&
						+'~n('+as_data+') must be numeric.'&
						+'~nPlease change or cancel the entry.')
			RETURN 1
		END IF
					
	CASE 'decimal','dec'
		IF IsNumber(as_data) THEN
			// continue
			IF String(Dec(as_data)) = as_data THEN
				//OK
				RETURN 2
			ELSE
				MessageBox('Data Entry Error','The numeric value you have entered'&
						+'~n('+as_data+') is not valid.'&
						+'~nPlease change or cancel the entry.')
				RETURN 1
			END IF
		ELSE
			MessageBox('Data Entry Error','The value you have entered'&
						+'~n('+as_data+') must be numeric.'&
						+'~nPlease change or cancel the entry.')
			RETURN 1
		END IF
		
	CASE 'real'
		IF IsNumber(as_data) THEN
			// continue
			IF String(Real(as_data)) = as_data THEN
				//OK
				RETURN 2
			ELSE
				MessageBox('Data Entry Error','The numeric value you have entered'&
						+'~n('+as_data+') is not valid.'&
						+'~nPlease change or cancel the entry.')
				RETURN 1
			END IF
		ELSE
			MessageBox('Data Entry Error','The value you have entered'&
						+'~n('+as_data+') must be numeric.'&
						+'~nPlease change or cancel the entry.')
			RETURN 1
		END IF
					
	CASE 'double'
		IF IsNumber(as_data) THEN
			// continue
			IF String(Double(as_data)) = as_data THEN
				//OK
				RETURN 2
			ELSE
				MessageBox('Data Entry Error','The numeric value you have entered'&
						+'~n('+as_data+') is not valid.'&
						+'~nPlease change or cancel the entry.')
				RETURN 1
			END IF
		ELSE
			MessageBox('Data Entry Error','The value you have entered'&
						+'~n('+as_data+') must be numeric.'&
						+'~nPlease change or cancel the entry.')
			RETURN 1
		END IF
		
	CASE 'unsignedinteger','unsignedint','uint'
		IF IsNumber(as_data) THEN
			// continue
			IF String(Long(as_data)) = as_data THEN
				//OK
				RETURN 2
			ELSE
				MessageBox('Data Entry Error','The numeric value you have entered'&
						+'~n('+as_data+') is not valid.'&
						+'~nPlease change or cancel the entry.')
				RETURN 1
			END IF
		ELSE
			MessageBox('Data Entry Error','The value you have entered'&
						+'~n('+as_data+') must be numeric.'&
						+'~nPlease change or cancel the entry.')
			RETURN 1
		END IF
		
	CASE 'unsignedLong','ulong'
		IF IsNumber(as_data) THEN
			// continue
			IF LongLong(as_data) >= 0 OR LongLong(as_data) < 4294967295  THEN
				//OK
				RETURN 2
			ELSE
				MessageBox('Data Entry Error','The numeric value you have entered'&
						+'~n('+as_data+') is not valid.'&
						+'~nPlease change or cancel the entry.')
				RETURN 1
			END IF
		ELSE
			MessageBox('Data Entry Error','The value you have entered'&
						+'~n('+as_data+') must be numeric.'&
						+'~nPlease change or cancel the entry.')
			RETURN 1
		END IF
		
	CASE 'time','boolean','byte','longlong','blob'
		// these types currently have no counterpart in the WHSCC MS SQL 2005 databases
		MessageBox('Data Entry Error','The value you have entered'&
					+'~n('+as_data+') is not valid.'&
					+'~nPlease change or cancel the entry.')
		RETURN 1
					
	CASE ELSE
		RETURN 2
			
END CHOOSE

end function

event itemchanged;call super::itemchanged;

//I had to include this code because the itemchanged event
//is triggered manually somtimes and the  arguments are null.
If not IsNull(dwo) THen
	this.post uf_trigger_itemchangeaccepted(row,dwo.name)
End if
end event

event itemfocuschanged;call super::itemfocuschanged;// This routine selects the text in a field when the field gets focus.

// Note: There is an autoselect feature for edit styles but not for edit masks.
//       Beware of going through this routine in debug (it always hangs the system)


Integer vli_return_code
String vls_temp_text

vls_temp_text		=	GetText()
vli_return_code	=	SelectText(1,Len(vls_temp_text) + 100)


return
end event

event rbuttondown;call super::rbuttondown;M_DW_ONLINE_RMB_POPUP lm_popup


IF IsValid(inv_rowselection) Then
	inv_rowselection.event pfc_rbuttondown(xpos,ypos,row,dwo)
End if


/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/
	lm_popup = Create m_dw_online_rmb_popup
	lm_popup.mf_set_datawindow(This)
	
	lm_popup.m_options.PopMenu(w_frame.PointerX( ), w_frame.PointerY( ))

	Destroy lm_popup
end event

on constructor;call u_dwa::constructor;G_PFSecurity.uof_Check_Access(THIS)
end on

event clicked;call super::clicked;
IF IsValid(inv_rowselection) Then
	inv_rowselection.event pfc_clicked(xpos,ypos,row,dwo)
End if


IF IsValid(inv_Sort) Then
	inv_Sort.event ue_clicked(dwo)
End if
end event

event itemerror;call super::itemerror;INT li_rtn

/*
validates the data in the edit control & displays an error message
*/

IF ib_supress_pbmessage = TRUE THEN
	li_rtn = uf_validate_dwo(dwo,data)
	RETURN li_rtn
END IF
end event

on u_dw_online.create
call super::create
end on

on u_dw_online.destroy
call super::destroy
end on

event rowfocuschanged;call super::rowfocuschanged;IF isvalid(inv_rowselection) Then
	inv_rowselection.event pfc_rowfocuschanged(currentrow)
End if
end event

event retrievestart;call super::retrievestart;SetPointer(HourGlass!)
end event

event retrieveend;call super::retrieveend;SetPointer(Arrow!)
end event

