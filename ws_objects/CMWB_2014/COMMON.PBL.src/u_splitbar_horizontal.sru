$PBExportHeader$u_splitbar_horizontal.sru
forward
global type u_splitbar_horizontal from statictext
end type
end forward

global type u_splitbar_horizontal from statictext
integer width = 1248
integer height = 16
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string pointer = "SizeNS!"
long textcolor = 33554432
long backcolor = 67108864
boolean focusrectangle = false
event mousemove pbm_mousemove
event lbuttondown pbm_lbuttondown
event lbuttonup pbm_lbuttonup
end type
global u_splitbar_horizontal u_splitbar_horizontal

type variables
n_resize_splitter		inv_resize

window			iw_parent_window

LONG			il_min_units_from_top       	= 300
LONG			il_min_units_from_bottom  	= 300

BOOLEAN	ib_lbuttondown

/* used if object not placed directly on a window this would typically be the difference 
    in size between the object it sits on and the widow that all of the objects sit on.
*/
LONG         il_offset 

end variables

forward prototypes
public function integer of_setrequestor (window aw_window)
public function integer of_Register (windowobject awo_control, long al_move_vertically, long al_grow_vertically)
public function integer of_register (windowobject awo_control)
end prototypes

event mousemove;IF flags = 1 and ib_lbuttondown Then
	
	//This is to make limits so the panes will always be a minimum size
//	IF iw_parent_window.pointery() < iw_parent_window.height - il_min_units_from_bottom &
//	   and  iw_parent_window.pointery() > il_min_units_from_top THEN
//		this.y = iw_parent_window.pointery()	
//	End if

	IF iw_parent_window.pointery() < iw_parent_window.height - il_min_units_from_bottom &
	   and  iw_parent_window.pointery() > il_min_units_from_top THEN
		this.y = iw_parent_window.pointery()	 - il_offset
	End if

End if
end event

event lbuttondown;ib_lbuttondown = True
this.backcolor = rgb(0,0,0)
end event

event lbuttonup;long ll_workspacewidth,ll_workspaceheight
ulong		sizetype

ib_lbuttondown = False
this.backcolor = iw_parent_window.backcolor

// Notify the resize service that the window size has changed.
ll_workspacewidth = iw_parent_window.WorkSpaceWidth()
ll_workspaceheight = This.y

//added by james for testing
//ll_workspacewidth = iw_parent_window.WorkSpaceWidth() - il_offset_width
//ll_workspaceheight = This.y - il_offset_height


inv_resize.Event pfc_Resize (sizetype,ll_workspacewidth  , ll_workspaceheight )


end event

public function integer of_setrequestor (window aw_window);iw_parent_window = aw_window

inv_resize.of_SetOrigSize(iw_parent_window.Width,This.y)
//IF il_offset_width > 0 THEN
//	inv_resize.of_SetOrigSize(il_offset_width,This.y + il_offset_y)//jw
//ELSE
//	inv_resize.of_SetOrigSize(iw_parent_window.Width - il_offset_width,This.y + il_offset_y)//jw
//END IF 

return 1
end function

public function integer of_Register (windowobject awo_control, long al_move_vertically, long al_grow_vertically);

inv_resize.of_register(awo_control,0,al_move_vertically,0,al_grow_vertically)

return 1
end function

public function integer of_register (windowobject awo_control);DRAGOBJECT		ldgo_control


LONG				ll_move_vertical
LONG				ll_resize_vertical
BOOLEAN			lb_resizable_control

IF inv_resize.of_TypeOf(awo_control) = 'dragobject!' Then
	CHOOSE CASE awo_control.TypeOf() 
		CASE datawindow!, dropdownlistbox!, &
			dropdownpicturelistbox!, graph!, groupbox!, listbox!, &
			picturelistbox!, listview!, picture!, &
			tab!, treeview!

			lb_resizable_control = True

		CASE ELSE

			lb_resizable_control = False

	END CHOOSE
			
	ldgo_control = awo_control
	//Regestered control is above the splitter
	If ldgo_control.Y < This.Y Then
		
		IF lb_resizable_control Then
			ll_move_vertical = 0
			ll_resize_vertical = 100
		ElseIF NOT lb_resizable_control THEN
			ll_move_vertical = 100
			ll_resize_vertical = 0
		END IF

	Else
		IF lb_resizable_control Then
			ll_move_vertical = 100
			ll_resize_vertical = -100
		ElseIF NOT lb_resizable_control THEN
			ll_move_vertical = 100
			ll_resize_vertical = 0
		END IF
	End if

End if


of_ReGister(awo_control,ll_move_vertical,ll_resize_vertical)


return 1
end function

on u_splitbar_horizontal.create
end on

on u_splitbar_horizontal.destroy
end on

event constructor;inv_resize = create n_resize_splitter

powerobject	 lpo_parent

lpo_parent = this.GetParent()

// Loop getting the parent of the object until it is of type window!
do while IsValid (lpo_parent) 
	if lpo_parent.TypeOf() <> window! then
		lpo_parent = lpo_parent.GetParent()
	else
		exit
	end if
loop

if IsNull(lpo_parent) Or not IsValid (lpo_parent) then
//	setnull(aw_parent)	
	return -1
end If

of_setrequestor(lpo_parent)

//of_setrequestor(parent)








end event

