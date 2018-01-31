$PBExportHeader$cb_dw_maximize.sru
forward
global type cb_dw_maximize from picturebutton
end type
end forward

global type cb_dw_maximize from picturebutton
integer width = 82
integer height = 68
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean originalsize = true
string picturename = "maximize.bmp"
alignment htextalign = left!
event ue_doubleclicked ( )
end type
global cb_dw_maximize cb_dw_maximize

type variables
//Maximized datawindow size and position
Private	Integer			ii_max_x
Private	Integer			ii_max_y
Private	Integer			ii_max_height
Private	Integer			ii_max_width

//Normal datawindow size and position
Private	Integer			ii_normal_x
Private	Integer			ii_normal_y
Private	Integer			ii_normal_height
Private	Integer			ii_normal_width

//This normal position
Private	Integer			ii_normal_button_x
Private	Integer			ii_normal_button_y

//THis maximized position
Private	Integer			ii_maximized_button_x
Private	Integer			ii_maximized_button_y

Private	u_dw_online		idw_dw

CONSTANT 	INTEGER		NORMAL = 1
CONSTANT		INTEGER		MAXIMIZED = 2

CONSTANT		STRING		is_maximized_picture = 'maximize.bmp'
CONSTANT		STRING		is_normal_picture = 'restore.bmp'

PROTECTED		INTEGER		ii_datawindow_state = 1


end variables

forward prototypes
public function integer uf_set_max_position (integer ai_x, integer ai_y, integer ai_height, integer ai_width)
private function integer uf_set_normal_position ()
public function integer uf_toggle_position ()
public function integer uf_set_requestor (u_dw_online adw_dw)
public function integer uf_calc_button_max_position ()
public function integer uf_get_datawindow_state ()
end prototypes

event ue_doubleclicked();If This.ii_datawindow_state = MAXIMIZED Then
	uf_toggle_position()
End if
end event

public function integer uf_set_max_position (integer ai_x, integer ai_y, integer ai_height, integer ai_width);
If ai_x >= 0 then
	ii_max_x = ai_x
else  
	SignalError(-666,'X position cannot be a negative number.')
End if

If ai_y >= 0 THen
	ii_max_y = ai_y
else
	SignalError(-666,'Y position cannot be a negative number.')
End if

If ai_height >= 0 THen
	ii_max_height = ai_height
else
	SignalError(-666,'height position cannot be a negative number.')
End if

If ai_width >= 0 THen
	ii_max_width = ai_width
else
	SignalError(-666,'Width position cannot be a negative number.')
End if

uf_calc_button_max_position()

return 1
end function

private function integer uf_set_normal_position ();

If IsValid(idw_dw) Then
	ii_normal_x = idw_dw.x
	ii_normal_y = idw_dw.y
	ii_normal_height = idw_dw.height
	ii_normal_width = idw_dw.width
Else
	SignalError(-666,'Cannot set the normal datawindow position without a datawidnow')
ENd if


return 1
end function

public function integer uf_toggle_position ();LONG			ll_current_row




If ii_datawindow_state = MAXIMIZED THEN
	
	//Set the bitmap
	this.picturename = is_maximized_picture
	
	//Set the position
	idw_dw.x = ii_normal_x
	idw_dw.y = ii_normal_y
	
	//Set the size
	idw_dw.height = ii_normal_height
	idw_dw.width = ii_normal_width
	
	//Set the state
	ii_datawindow_state = NORMAL
	
	//Move this button so it remains in the same place
	//relative to the datawindow
	this.x = ii_normal_button_x
	This.y = ii_normal_button_y		
	
else
	//Set the max bitmap
	This.PictureName = is_normal_picture
	
	//Set the datawindow position
	idw_dw.x = ii_max_x
	idw_dw.y = ii_max_y
	
	//Set the datawindow size
	idw_dw.height = ii_max_height
	idw_dw.width = ii_max_width
	
	//Set the state
	ii_datawindow_state = MAXIMIZED
	
	//Move this button so it remains in the same place
	//relative to the datawindow
	This.x = ii_maximized_button_x
	This.y = ii_maximized_button_y
End if

If idw_dw.RowCount() > 0 Then
	ll_current_row = idw_dw.GetRow()
	idw_dw.ScrollToRow(ll_current_row)
End if


idw_dw.BringToTop = True
This.BringToTop = True

return 1
end function

public function integer uf_set_requestor (u_dw_online adw_dw);
idw_dw = adw_dw

uf_set_normal_position()


return 1
end function

public function integer uf_calc_button_max_position ();INTEGER	li_normal_relative_x 
INTEGER	li_normal_relative_y

//This is the number of units from the normal x position of the
//button to the normal x position of the datawindow
li_normal_relative_x = ii_normal_x + ii_normal_width - ii_normal_button_x
li_normal_relative_y = ii_normal_y - ii_normal_button_y

ii_maximized_button_x = ii_max_x + ii_max_width - li_normal_relative_x 
ii_maximized_button_y = ii_max_y - li_normal_relative_y

return 1
end function

public function integer uf_get_datawindow_state ();
RETURN ii_datawindow_state
end function

on cb_dw_maximize.create
end on

on cb_dw_maximize.destroy
end on

event constructor;ii_normal_button_x = This.x
ii_normal_button_y = This.y
end event

event clicked;uf_toggle_position()
end event

