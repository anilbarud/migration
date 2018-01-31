$PBExportHeader$u_new_filter_control.sru
forward
global type u_new_filter_control from u_filter_control
end type
type pb_clear from picturebutton within u_new_filter_control
end type
end forward

global type u_new_filter_control from u_filter_control
integer width = 1015
integer height = 100
event ue_retrieve ( datawindow adw_datawindow )
pb_clear pb_clear
end type
global u_new_filter_control u_new_filter_control

type variables
STRING is_filter
end variables

on u_new_filter_control.create
int iCurrent
call super::create
this.pb_clear=create pb_clear
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.pb_clear
end on

on u_new_filter_control.destroy
call super::destroy
destroy(this.pb_clear)
end on

type pb_next from u_filter_control`pb_next within u_new_filter_control
boolean visible = false
integer x = 846
integer y = 0
integer width = 174
integer height = 104
string facename = "Gill Sans MT"
string text = "clear"
string picturename = ""
alignment htextalign = right!
vtextalign vtextalign = vcenter!
long textcolor = 25511203
long backcolor = 33222891
end type

type pb_previous from u_filter_control`pb_previous within u_new_filter_control
integer x = 571
integer y = 0
integer width = 279
integer height = 104
string facename = "Gill Sans MT"
string text = "undo last"
string picturename = ""
alignment htextalign = right!
vtextalign vtextalign = vcenter!
long textcolor = 25511203
long backcolor = 33222891
end type

event pb_previous::clicked;STRING		ls_applied_filter


ls_applied_filter = idw_datawindow.inv_filter.of_Move_Previous()
is_filter = ls_applied_filter
idw_datawindow.inv_filter.of_remove_filter(1)
parent.event ue_filter_changed(ls_applied_filter)
If ls_applied_filter = '' Then
	PARENT.EVENT ue_retrieve(idw_datawindow)
	idw_datawindow.SetRow(idw_datawindow.GetSelectedRow(0))
End if

end event

type pb_toggle_filter from u_filter_control`pb_toggle_filter within u_new_filter_control
integer x = 375
integer width = 201
integer height = 104
integer textsize = -8
string facename = "Gill Sans MT"
boolean enabled = true
string text = "toggle"
string picturename = ""
string disabledname = ""
alignment htextalign = center!
vtextalign vtextalign = vcenter!
long textcolor = 18897563
long backcolor = 32240378
end type

event pb_toggle_filter::clicked;STRING		ls_applied_filter


ls_applied_filter = idw_datawindow.inv_filter.of_Toggle_Filter()
is_filter = ls_applied_filter
parent.event ue_filter_changed(ls_applied_filter)

end event

type pb_filter_not_like from u_filter_control`pb_filter_not_like within u_new_filter_control
integer x = 137
integer width = 242
integer height = 104
string facename = "Gill Sans MT"
string text = "not like"
string picturename = ""
alignment htextalign = right!
vtextalign vtextalign = vcenter!
long textcolor = 25511203
long backcolor = 33222891
end type

event pb_filter_not_like::clicked;string	 ls_filter

IF idw_datawindow.RowCount() > 0 THEN
	IF idw_datawindow.GetColumnName() <> ''  THEN
		ls_filter = idw_datawindow.inv_filter.of_filter_selection('notlike')
		is_filter = ls_filter
		pb_toggle_filter.enabled = TRUE
		PARENT.EVENT ue_filter_changed(ls_filter)
	ELSE
		MessageBox('Nothing Selected','Please select a value to filter on.', Information!)
	END IF
ELSE
	MessageBox('No records','There is nothing to filter.')
END IF
end event

type pb_filter_like from u_filter_control`pb_filter_like within u_new_filter_control
integer width = 142
integer height = 104
string facename = "Gill Sans MT"
string text = "like"
string picturename = ""
alignment htextalign = right!
vtextalign vtextalign = vcenter!
long textcolor = 25511203
long backcolor = 33222891
end type

event pb_filter_like::clicked;STRING 	ls_filter


IF NOT ISNULL(idw_datawindow) THEN
	IF idw_datawindow.RowCount() > 0 THEN
		IF idw_datawindow.GetColumnName() <> '' THEN
			ls_filter = idw_datawindow.inv_filter.of_filter_selection('like')
			is_filter = ls_filter
			pb_toggle_filter.enabled = TRUE	
			PARENT.EVENT ue_filter_changed(ls_filter)
		ELSE
			MessageBox('Nothing Selected','Please select a value to filter on.', Information!)
		END IF
	ELSE
		MessageBox('No records','There is nothing to filter.')
	END IF
END IF	
end event

type pb_clear from picturebutton within u_new_filter_control
integer x = 846
integer width = 174
integer height = 104
integer taborder = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Gill Sans MT"
string text = "clear"
boolean originalsize = true
vtextalign vtextalign = vcenter!
long textcolor = 25511203
long backcolor = 33222891
end type

event clicked;STRING ls_applied_filter

DO 
	ls_applied_filter = idw_datawindow.inv_filter.of_move_previous()
	PARENT.EVENT ue_filter_changed(ls_applied_filter)
	IF ls_applied_filter = '' THEN
		idw_datawindow.inv_filter.of_remove_filter(0)		
	END IF
LOOP UNTIL ls_applied_filter = ''

is_filter = ''
PARENT.EVENT ue_retrieve(idw_datawindow)
idw_datawindow.SetRow(idw_datawindow.GetSelectedRow(0))
end event

