$PBExportHeader$n_filter.sru
forward
global type n_filter from nonvisualobject
end type
end forward

global type n_filter from nonvisualobject
end type
global n_filter n_filter

type variables
u_dw_online			idw_requestor

STRING				is_filter[] = {''}
BOOLEAN				ib_filter_on
INTEGER				ii_current_filter_position = 1
INTEGER				ii_max_filter_position = 1
INTEGER				ii_pre_toggle_filter_position
end variables

forward prototypes
public function integer of_setrequestor (datawindow adw_requestor)
public function any of_getitemany (long al_row, string as_column, dwbuffer adw_buffer, boolean ab_orig_value)
public function string of_filter_selection (string as_operation)
public function string of_move_previous ()
public function string of_toggle_filter ()
public function string of_move_next ()
public function string of_setfilter (string as_filter)
public subroutine of_remove_filter (integer ai_num)
end prototypes

public function integer of_setrequestor (datawindow adw_requestor);if IsNull(adw_requestor) or Not IsValid(adw_requestor) Then
	Return -1
End If

idw_Requestor = adw_Requestor
Return 1
end function

public function any of_getitemany (long al_row, string as_column, dwbuffer adw_buffer, boolean ab_orig_value);any 		la_value
string 	ls_computeexp

/*  Determine the datatype of the column and then call the appropriate 
	 GetItemxxx function and cast the returned value */
CHOOSE CASE Lower ( Left ( idw_requestor.Describe ( as_column + ".ColType" ) , 5 ) )

		CASE "char("				//  CHARACTER DATATYPE
			la_value = idw_requestor.GetItemString ( al_row, as_column, adw_buffer, ab_orig_value ) 
	
		CASE "date"					//  DATE DATATYPE
			la_value = idw_requestor.GetItemDate ( al_row, as_column, adw_buffer, ab_orig_value ) 

		CASE "datet"				//  DATETIME DATATYPE
			la_value = idw_requestor.GetItemDateTime ( al_row, as_column, adw_buffer, ab_orig_value ) 

		CASE "decim"				//  DECIMAL DATATYPE
			la_value = idw_requestor.GetItemDecimal ( al_row, as_column, adw_buffer, ab_orig_value ) 
	
		CASE "numbe", "long", "ulong", "real"				//  NUMBER DATATYPE	
			la_value = idw_requestor.GetItemNumber ( al_row, as_column, adw_buffer, ab_orig_value ) 
	
		CASE "time", "times"		//  TIME DATATYPE
			la_value = idw_requestor.GetItemTime ( al_row, as_column, adw_buffer, ab_orig_value ) 

		CASE ELSE 					//  MUST BE A COMPUTED COLUMN
			IF idw_requestor.Describe ( as_column + ".Type" ) = "compute" THEN 
				ls_computeexp = idw_requestor.Describe ( as_column + ".Expression" )
				la_value = idw_requestor.Describe( "Evaluate('" + ls_computeexp + "', " + string (al_row) + ")" )
			ELSE
				SetNull ( la_value ) 
			END IF 

END CHOOSE

Return la_value
end function

public function string of_filter_selection (string as_operation);STRING 			ls_filter
STRING			ls_new_filter
STRING			ls_data
STRING			ls_quotes = ''
INTEGER			li_x
STRING			ls_operator 
STRING			ls_column
STRING			ls_columntype
LONG				ll_row
STRING			ls_value
LONG				ll_position

If NOT Isvalid(idw_requestor) OR isnull(idw_requestor) THen
	SignalError(-666,'Null object idw_requestor')
End if

if idw_requestor.RowCount() = 0 Then
	Return ''
End if

//If the is toggled off
// 1. This may be the first filter applied to the datawindow
// 2. The user may have filtered previously and toggled it off. In this case
//		we want to empty is_filter so the new filter is not added to it.

ls_column = idw_requestor.GetColumnName()
If ls_column = '' Then SignalError(-666,'Error getting column name.')

ll_row = idw_requestor.GetRow()
If ll_row = 0 Then SignalError(-666,'Error getting row.')

ls_data = String(of_getitemany(ll_row,ls_column,Primary!,False))

If as_operation = 'like' Then
	IF KeyDown(KeyPeriod!) Then 
		ls_operator = '>='
	Elseif KeyDown(KeyComma!) Then
		ls_operator = '<='
	Else
		ls_operator = '='
	End if
Elseif as_operation = 'notlike' THen
	ls_operator = '<>'
Else
	SignalError(-666,'Error setting filter')
End if

//Replace single quote with ~'

ll_position = pos(ls_data,"'")
Do while ll_position <> 0
	ls_data = Replace (ls_data,ll_position,1,"~~'")
	ll_position = pos(ls_data,"'",ll_position + 2)
Loop

ls_columntype = idw_requestor.Describe(ls_column + ".ColType")

CHOOSE CASE left(ls_columntype,4)
	CASE 'char'
		ls_value = "'" + STRING(ls_data) + "'"
	CASE 'date'
		//Rellies on the date being in our standard format "yyyy-mm-dd"
		ls_data = Left(String(ls_data),10)
		IF isdate(ls_data) Then
			ls_column = "Date(" + ls_column + ")"
			ls_value = "Date('" + ls_data + "')"
		Else
			RETURN ''
		END IF
	CASE 'time'
		//We don't filter on time, it doesn't make sense to
	CASE ELSE
		ls_value = ls_data
END CHOOSE

ls_filter += String(ls_column) + ' ' + ls_operator + ' ' + ls_value


ls_new_filter = of_setfilter(ls_filter)

RETURN ls_new_filter
end function

public function string of_move_previous ();
IF ii_current_filter_position = 1 Then
	RETURN is_filter[ii_current_filter_position]
END IF


//Move the current position back one and apply that filter
ii_current_filter_position = ii_current_filter_position - 1
idw_requestor.SetFilter(is_filter[ii_current_filter_position])
idw_requestor.Filter()

IF ii_current_filter_position = 1 Then
	ib_filter_on = False
End if

return is_filter[ii_current_filter_position]



end function

public function string of_toggle_filter ();STRING		ls_applied_filter

If ib_filter_on Then
	ib_filter_on = False
	
	//Removed the filter but leave the is_filter alone
	//in case the user wants to reapply it later
	idw_requestor.SetFilter('')
	idw_requestor.Filter()
	ls_applied_filter = ''
	ii_pre_toggle_filter_position = ii_current_filter_position
	ii_current_filter_position = 1
ELSEIF ib_filter_on = False Then
	
	//IF the user clicks the toggle button and there is currently no filters
	//in the array, just return ''
	IF UpperBound(is_filter) = 1 Then
		RETURN is_filter[1]
	End if
	
	
	ib_filter_on = True
	
	ii_current_filter_position = ii_pre_toggle_filter_position
	If is_filter[ii_pre_toggle_filter_position] = '' Then return ''
	
	idw_requestor.SetFilter(is_filter[ii_current_filter_position])
	idw_requestor.Filter()
	ls_applied_filter = is_filter[ii_current_filter_position]
End if

return ls_applied_filter
end function

public function string of_move_next ();

IF ii_current_filter_position >= ii_max_filter_position Then
	RETURN is_filter[ii_current_filter_position]
END IF


//Move the current position back one and apply that filter
ii_current_filter_position = ii_current_filter_position + 1
idw_requestor.SetFilter(is_filter[ii_current_filter_position])
idw_requestor.Filter()

ii_pre_toggle_filter_position = ii_current_filter_position


IF ii_current_filter_position = 2 Then
	ib_filter_on = True
End if

return is_filter[ii_current_filter_position]
end function

public function string of_setfilter (string as_filter);STRING			ls_quotes = ''
INTEGER			li_x
LONG				ll_row
STRING			ls_filter[]
INTEGER			li_filter_upper_bound

If NOT Isvalid(idw_requestor) OR isnull(idw_requestor) THen
	SignalError(-666,'Null object idw_requestor')
End if

if idw_requestor.RowCount() = 0 Then
	Return ''
End if


//If the is toggled off
// 1. This may be the first filter applied to the datawindow
// 2. The user may have filtered previously and toggled it off. In this case
//		we want to empty is_filter so the new filter is not added to it.
If ib_filter_on = False Then
	is_filter = ls_filter
End if

ll_row = idw_requestor.GetRow()
If ll_row = 0 Then SignalError(-666,'Error getting row.')

//If an empty sting is being passed in, reset the filtering object
if as_filter = '' Then
	//reset the array
	is_filter = ls_filter

	//Empty string is passed in which will remove all filters
	ib_filter_on = False
	
	is_filter[1] = ''
	ii_current_filter_position = 1
	ii_max_filter_position = 1
	ii_pre_toggle_filter_position = 0


//A filter is applied and a new filter should be added
elseIf ib_filter_on  and as_filter <> '' Then
	//ii_current_filter_position represents the array position that is currently applied to the datawindow
	//there may be other filters after this one but the user has moved back using of_move_previous.
	//If we are appending to a filter that is not the last filter in the list then we must set the rest
	//of the filters to " " so that of_move_next will know that these are not valid filter to apply.
	is_filter[ii_current_filter_position + 1] = is_filter[ii_current_filter_position] + ' and ' + as_filter
	
	//The datawindow is filtered
	ib_filter_on = True
	//Increment the current filter position
	ii_current_filter_position += 1
	//Set the last position = to the current
	ii_max_filter_position = ii_current_filter_position
	
	ii_pre_toggle_filter_position = ii_current_filter_position
	
	
//A filter is not applied so the filter will become whatever was passed in
Elseif ib_filter_on = False and as_filter <> '' Then
	//reset the array
	is_filter = ls_filter	
	
	
	//the filter_on is set to TRUE since there is a filter applied
	ib_filter_on = True
	is_filter[2] = as_filter
	ii_current_filter_position += 1
	
	ii_max_filter_position = ii_current_filter_position
	ii_pre_toggle_filter_position = ii_current_filter_position
	
Else
	SignalError(-666,'Error filtering stuff.')
End if


IF idw_requestor.SetFilter(is_filter[ii_current_filter_position]) = -1 Then SignalError(-666,'Error filtering datawindow. Filter string: ' + is_filter[ii_current_filter_position])
If idw_requestor.Filter() = -1 Then SignalError(-666,'Error filtering datawindow. Filter string: ' + is_filter[ii_current_filter_position])


Return is_filter[ii_current_filter_position]
end function

public subroutine of_remove_filter (integer ai_num);LONG ll_upperbound, ll_cntr

ll_upperbound = UpperBound(is_filter[])

//ai_num = 0 for all,  ai_num = 1 for current(when moving to previous)
IF ai_num = 1 THEN 
	is_filter[ll_upperbound] = ''
ELSE
	FOR ll_cntr = 1 to ll_upperbound
		is_filter[ll_cntr] = ''
	NEXT
END IF
end subroutine

on n_filter.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_filter.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

