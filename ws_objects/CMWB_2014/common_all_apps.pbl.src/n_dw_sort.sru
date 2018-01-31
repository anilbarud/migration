$PBExportHeader$n_dw_sort.sru
forward
global type n_dw_sort from nonvisualobject
end type
end forward

global type n_dw_sort from nonvisualobject
event type integer ue_clicked ( dwobject adwo_obj )
end type
global n_dw_sort n_dw_sort

type variables
u_datawindow		idw_requestor

STRING				is_defaultheadersuffix = '_t'
STRING				is_sortcolumn
STRING				is_sortorder

STRING				is_group_sort

BOOLEAN			ib_sort_within_group = False
BOOLEAN			ib_display_sort_indicator = False
end variables

forward prototypes
public function boolean of_usesdisplayvalue (string as_column)
public function integer of_set_group_sort (string as_group_sort)
public function integer of_sort_within_group (boolean ab_sort_within_group)
public function integer of_display_sort_indicator (boolean ab_switch)
public function integer of_move_sort_indicator (dwobject adw_header)
public function integer of_change_sort_indicator (string as_sort_order)
public function integer of_setrequestor (datawindow adw_requestor)
end prototypes

event type integer ue_clicked(dwobject adwo_obj);string	ls_headername
string 	ls_colname
integer	li_rc
integer	li_suffixlen
integer	li_headerlen
string	ls_sortstring
long		ll_selectedrow 

// Validate the dwo reference.
IF IsNull(adwo_obj) OR NOT IsValid(adwo_obj) THEN	 
	Return -1
END IF 

// Check if the service is set to sort on column headers.
//IF NOT of_GetColumnHeader() THEN Return 0

// Only valid on header column.
If adwo_obj.Name = 'datawindow' THEN Return 0
IF LEFT(adwo_obj.Band,6) <> "header" THEN Return 0

// Get column header information.
ls_headername = adwo_obj.Name
li_headerlen = Len(ls_headername)
li_suffixlen = Len(is_defaultheadersuffix)

// Extract the columname from the header label 
// (by taking out the header suffix).
IF Right(ls_headername, li_suffixlen) <> is_defaultheadersuffix THEN 
	// Cannot determine the column name from the header.	
	Return -1
END IF 	
ls_colname = Left (ls_headername, li_headerlen - li_suffixlen)

// Validate the column name.
If IsNull(ls_colname) or Len(Trim(ls_colname))=0 Then 
	Return -1
End If

// Check the previous sort click.
IF is_sortcolumn = ls_colname THEN	
	// Second sort click on the same column, reverse sort order.
	IF is_sortorder = " A" THEN 	
		is_sortorder = " D"
	ELSE
		is_sortorder = " A"
	END IF 
	if ib_display_sort_indicator = True Then
		of_change_sort_indicator(is_sortorder)
	End if
ELSE
	// Clicked on a different column.
	is_sortcolumn = ls_colname
	is_sortorder = " A" 
	if ib_display_sort_indicator = True Then
		of_move_sort_indicator(adwo_obj)
		of_change_sort_indicator(is_sortorder)
	end if
END IF 

// Build the sort string.
IF of_UsesDisplayValue(ls_colname) THEN
	ls_sortstring = "LookUpDisplay(" + ls_colname + ") " + is_sortorder 
ELSE
	ls_sortstring = is_sortcolumn + is_sortorder
END IF 

//Add to group sort if necessary
IF ib_sort_within_group Then
	IF is_group_sort = '' Then SignalError(-666,'No group sort supplied')
	
	ls_sortstring = is_group_sort + ' , ' + ls_sortstring
End if
	
	
	
// Perform the SetSort operation (check the rc).
li_rc = idw_requestor.SetSort(ls_sortstring) 
If li_rc < 0 Then Return li_rc

// Perform the actual Sort operation (check the rc).
li_rc = idw_requestor.Sort()
If li_rc < 0 Then Return li_rc	

//The indicator might not be visible yet
if ib_display_sort_indicator = True Then
	of_display_sort_indicator(True)
End if

ll_selectedrow = idw_requestor.GetSelectedRow(0)
idw_requestor.GroupCalc()
IF ll_selectedrow > 0 THEN
	idw_requestor.SetColumn(ls_colname)
	idw_requestor.ScrollToRow(ll_selectedrow)	
END IF

Return 1

end event

public function boolean of_usesdisplayvalue (string as_column);string			ls_editstyle
string			ls_codetable

// Check parameters.
If IsNull(as_column) or Len(Trim(as_column))=0 Then Return False

ls_editstyle = Lower(idw_Requestor.Describe (as_column + ".Edit.Style"))
ls_codetable = Lower(idw_Requestor.Describe (as_column + "." + ls_editstyle + ".CodeTable"))

IF ls_editstyle = 'dddw' or ls_editstyle = 'ddlb' or ls_codetable = 'yes' THEN
	Return True
END IF

Return False

end function

public function integer of_set_group_sort (string as_group_sort);
is_group_sort = as_group_sort

return 1
end function

public function integer of_sort_within_group (boolean ab_sort_within_group);

ib_sort_within_group = ab_sort_within_group

return 1
end function

public function integer of_display_sort_indicator (boolean ab_switch);LONG		ll_visible
STRING	ls_mod

ib_display_sort_indicator = ab_switch


IF ab_switch = False or is_sortcolumn = '' THen
	ll_visible = 0
Else
	ll_visible = 1
End if

ls_mod = idw_requestor.modify('t_sort_indicator.visible = ' + String(ll_visible) )

IF ls_mod  <> '' Then
	SignalError(-666,ls_mod)
End if

return 1
end function

public function integer of_move_sort_indicator (dwobject adw_header);LONG			ll_sort_indicator_x
LONG			ll_sort_indicator_y
STRING		ls_mod_string
CONSTANT  LONG	ll_sort_indicator_width = 69

ll_sort_indicator_x = Long(adw_header.x) + Long(adw_header.width) - ll_sort_indicator_width

IF Long(adw_header.y) - 8 < 0 Then
	ll_sort_indicator_y = 0
Else
	ll_sort_indicator_y = LONG(adw_header.y) - 8
End if

ls_mod_string = 't_sort_indicator.x = ' + String(ll_sort_indicator_x)
ls_mod_string += ' t_sort_indicator.y = ' + String(ll_sort_indicator_y)

ls_mod_string = idw_requestor.modify(ls_mod_string)

if ls_mod_string <> '' Then
	SignalError(-666,ls_mod_string)
End if

return 1
end function

public function integer of_change_sort_indicator (string as_sort_order);STRING		ls_sort_indicator
STRING		ls_mod_string


IF as_sort_order = ' A' Then
	ls_sort_indicator = '5'
Elseif as_sort_order = ' D' Then
	ls_sort_indicator = '6'
else
	SignalError(-666,'Unknown sort order.')
End if

ls_mod_string = "t_sort_indicator.text = '" + ls_sort_indicator + "'"

ls_mod_string = idw_requestor.Modify(ls_mod_string)

if ls_mod_string <> '' Then
	SIgnalError(-666,'Error modifying sort indicator')
End if

return 1
end function

public function integer of_setrequestor (datawindow adw_requestor);STRING		ls_mod_string

if IsNull(adw_requestor) or Not IsValid(adw_requestor) Then
	Return -1
End If

idw_Requestor = adw_Requestor

ls_mod_string = 'create text(band=header alignment="0" text="6" border="0" color="33554432" x="0" y="0" height="72" width="69" html.valueishtml="0"  name=t_sort_indicator visible="0"  font.face="Webdings" font.height="-10" font.weight="400"  font.family="1" font.pitch="2" font.charset="2" background.mode="1" background.color="553648127" )'


idw_requestor.modify(ls_mod_string)

idw_requestor.SetPosition('t_sort_indicator','header',True)

Return 1
end function

on n_dw_sort.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_dw_sort.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

