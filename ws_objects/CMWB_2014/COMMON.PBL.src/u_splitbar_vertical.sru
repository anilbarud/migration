$PBExportHeader$u_splitbar_vertical.sru
forward
global type u_splitbar_vertical from statictext
end type
end forward

global type u_splitbar_vertical from statictext
integer width = 82
integer height = 1516
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string pointer = "SizeWE!"
long textcolor = 33554432
long backcolor = 67108864
boolean focusrectangle = false
event mousemove pbm_mousemove
event lbuttondown pbm_lbuttondown
event lbuttonup pbm_lbuttonup
event ue_moved ( )
end type
global u_splitbar_vertical u_splitbar_vertical

type variables
n_resize_splitter		inv_resize_splitter

window			iw_parent_window

LONG			il_min_units_from_left = 300
LONG			il_min_units_from_right = 300

BOOLEAN	ib_lbuttondown

end variables

forward prototypes
public function integer of_register (windowobject awo_control, long al_move_horizontal, long al_grow_horizontal)
public function integer of_setrequestor (window aw_window)
public function integer of_register (windowobject awo_control)
end prototypes

event mousemove;IF flags = 1 AND ib_lbuttondown Then
	
	//This is to make limits so the panes will always be a minimum size
	IF iw_parent_window.pointerx() < iw_parent_window.width - il_min_units_from_right &
	   and  iw_parent_window.pointerx() > il_min_units_from_left THEN
		this.x = iw_parent_window.pointerx()
	End if

End if
end event

event lbuttondown;
ib_lbuttondown = True
this.backcolor = rgb(0,0,0)
end event

event lbuttonup;long ll_workspacewidth,ll_workspaceheight
ulong		sizetype

ib_lbuttondown = False
this.backcolor = iw_parent_window.backcolor

// Notify the resize service that the window size has changed.
ll_workspacewidth = This.x
ll_workspaceheight = iw_parent_window.WorkSpaceHeight()


inv_resize_splitter.Event pfc_Resize (sizetype,ll_workspacewidth  , ll_workspaceheight )

Triggerevent("ue_moved")


end event

public function integer of_register (windowobject awo_control, long al_move_horizontal, long al_grow_horizontal);

inv_resize_splitter.of_register(awo_control,al_move_horizontal,0,al_grow_horizontal,0)

return 1
end function

public function integer of_setrequestor (window aw_window);iw_parent_window = aw_window

IF IsValid(inv_resize_splitter) THEN
	inv_resize_splitter.of_SetOrigSize(This.x,iw_parent_window.Height)
END IF

return 1
end function

public function integer of_register (windowobject awo_control);DRAGOBJECT		ldgo_control
LONG				ll_default_resize
BOOLEAN			lb_resizable_control


LONG				ll_move_horizontal
LONG				ll_resize_horizontal

IF inv_resize_splitter.of_TypeOf(awo_control) = 'dragobject!' Then
	
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
	If ldgo_control.X < This.X Then
		
		IF lb_resizable_control Then
			ll_move_horizontal = 0
			ll_resize_horizontal = 100
		ElseIF NOT lb_resizable_control THEN
			ll_move_horizontal = 100
			ll_resize_horizontal = 0
		END IF

	Else
		IF lb_resizable_control Then
			ll_move_horizontal = 100
			ll_resize_horizontal = -100
		ElseIF NOT lb_resizable_control THEN
			ll_move_horizontal = 100
			ll_resize_horizontal = 0
		END IF
	End if
End if


of_ReGister(awo_control,ll_move_horizontal,ll_resize_horizontal)

return 1
end function

on u_splitbar_vertical.create
end on

on u_splitbar_vertical.destroy
end on

event constructor;//inv_resize_splitter = create n_resize_splitter
//
//of_setrequestor(Parent)

inv_resize_splitter = create n_resize_splitter

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

