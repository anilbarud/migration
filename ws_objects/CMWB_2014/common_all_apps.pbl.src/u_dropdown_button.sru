$PBExportHeader$u_dropdown_button.sru
forward
global type u_dropdown_button from userobject
end type
type dw_drop_button from datawindow within u_dropdown_button
end type
end forward

global type u_dropdown_button from userobject
integer width = 539
integer height = 116
long backcolor = 67108864
string text = "none"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
event type integer ue_selected ( string as_selected_value )
dw_drop_button dw_drop_button
end type
global u_dropdown_button u_dropdown_button

type prototypes
Subroutine keybd_event( int bVk, int bScan, int dwFlags, int dwExtraInfo) Library "user32.dll"
end prototypes

type variables
DATAWINDOWCHILD idwc_dropdown
end variables

forward prototypes
public function integer uf_set_label (string as_label)
public function integer uf_set_items (string as_items[], string as_values[])
public function integer uf_enable (boolean lb_enable)
end prototypes

event type integer ue_selected(string as_selected_value);/*
**		Type  		: Event
**		Name 		: ue_selected
**		Arguments 	: String - as_selected_value
**		Returns 		: Integer - 1 is good
**		Purpose		: This event is triggered when the user selects an item in the dropdown.  Code should be placed in the descendant to process this user selection
**		Date			: 2014/08/14
**		Author		: David Worboys
**	
**		Modifications
*/

RETURN 1
end event

public function integer uf_set_label (string as_label);/*
**		Type  		: Function
**		Name 		: uf_set_label
**		Arguments 	: String - as_label
**		Returns 		:
**		Purpose		: Sets the button label text
**		Date			: 2014/08/14
**		Author		: David Worboys
**	
**		Modifications
*/
dw_drop_button.Object.b_label.Text = as_label

RETURN 1
end function

public function integer uf_set_items (string as_items[], string as_values[]);/*
**		Type  		: Function
**		Name 		: uf_set_items
**		Arguments 	: String - as_items[], String - as_values[]
**		Returns 		:
**		Purpose		: Loads the items into the dropdown.  Number of items must = number of values!
**		Date			: 2014/08/14
**		Author		: David Worboys
**	
**		Modifications
*/
LONG 		ll_item	= 0
LONG 		ll_max 	= 0

idwc_dropdown.Reset()
ll_max = UpperBound(as_items)


FOR ll_item = 1 TO ll_max
	idwc_dropdown.insertrow(0)
	idwc_dropdown.setitem( ll_item, "as_Description", as_items[ll_item])
	idwc_dropdown.setitem( ll_item, "as_Value", as_values[ll_item])
NEXT

idwc_dropdown.SetRow(1)
idwc_dropdown.ScrollToRow(1)

RETURN 1
end function

public function integer uf_enable (boolean lb_enable);/*
**		Type  		: Function
**		Name 		: uf_enable
**		Arguments 	: boolean - lb_enable
**		Returns 		:
**		Purpose		: Enables the objects.
**		Date			: 2014/08/14
**		Author		: David Worboys
**	
**		Modifications
*/
INTEGER li_return = 1

IF (lb_enable) THEN //Enabled the buttons
	dw_drop_button.Object.b_label.Enabled = 1
	dw_drop_button.Object.as_Dropdown.Protect = 0
ELSE //Disable the buttons
	dw_drop_button.Object.b_label.Enabled = 0
	dw_drop_button.Object.as_Dropdown.Protect = 1
END IF

RETURN li_return
end function

on u_dropdown_button.create
this.dw_drop_button=create dw_drop_button
this.Control[]={this.dw_drop_button}
end on

on u_dropdown_button.destroy
destroy(this.dw_drop_button)
end on

type dw_drop_button from datawindow within u_dropdown_button
integer width = 549
integer height = 144
integer taborder = 10
string title = "none"
string dataobject = "d_dropdown_button"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event constructor;/*
**		Type  		: Event
**		Name 		: constructor
**		Arguments 	: 
**		Returns 		:
**		Purpose		: Sets up the object for use
**		Date			: 2014/08/14
**		Author		: David Worboys
**	
**		Modifications
*/
THIS.InsertRow(0)
THIS.GetChild( "as_dropdown", idwc_dropdown)
end event

event itemchanged;/*
**		Type  		: Event
**		Name 		: itemchanged
**		Arguments 	: long - row, dwobject - dwo, string - data
**		Returns 		:
**		Purpose		: Processes the item changed event.
**		Date			: 2014/08/14
**		Author		: David Worboys
**	
**		Modifications
*/
IF (row > 0) THEN
	PARENT.Event ue_Selected(data)
END IF
end event

event buttonclicked;/*
**		Type  		: Event
**		Name 		: buttonclicked
**		Arguments 	: long - row, long actionreturncode, dwobject - dwo
**		Returns 		:
**		Purpose		: Processes the datawidow button click event.
**		Date			: 2014/08/14
**		Author		: David Worboys
**	
**		Modifications
*/
THIS.SetRow(1)
THIS.SetColumn("as_dropdown")

//The only way I know to get a datawindow to drop in code
keybd_event( 115,0,0,0 )  // F4 key down
keybd_event( 115,0,2,0 )  // F4 key up 



end event

