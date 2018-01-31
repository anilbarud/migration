$PBExportHeader$u_dw_filter_control.sru
forward
global type u_dw_filter_control from userobject
end type
type pb_next from picturebutton within u_dw_filter_control
end type
type pb_previous from picturebutton within u_dw_filter_control
end type
type pb_toggle_filter from picturebutton within u_dw_filter_control
end type
type pb_filter_not_like from picturebutton within u_dw_filter_control
end type
type pb_filter_like from picturebutton within u_dw_filter_control
end type
end forward

global type u_dw_filter_control from userobject
integer width = 608
integer height = 104
long backcolor = 67108864
string text = "none"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
event ue_filter_changed ( string ls_new_filter )
pb_next pb_next
pb_previous pb_previous
pb_toggle_filter pb_toggle_filter
pb_filter_not_like pb_filter_not_like
pb_filter_like pb_filter_like
end type
global u_dw_filter_control u_dw_filter_control

type variables
public u_datawindow	idw_datawindow
end variables

forward prototypes
public function integer uf_set_requestor (u_datawindow adw_datawindow)
end prototypes

public function integer uf_set_requestor (u_datawindow adw_datawindow);idw_datawindow = adw_datawindow

return 1
end function

on u_dw_filter_control.create
this.pb_next=create pb_next
this.pb_previous=create pb_previous
this.pb_toggle_filter=create pb_toggle_filter
this.pb_filter_not_like=create pb_filter_not_like
this.pb_filter_like=create pb_filter_like
this.Control[]={this.pb_next,&
this.pb_previous,&
this.pb_toggle_filter,&
this.pb_filter_not_like,&
this.pb_filter_like}
end on

on u_dw_filter_control.destroy
destroy(this.pb_next)
destroy(this.pb_previous)
destroy(this.pb_toggle_filter)
destroy(this.pb_filter_not_like)
destroy(this.pb_filter_like)
end on

type pb_next from picturebutton within u_dw_filter_control
integer x = 494
integer y = 4
integer width = 101
integer height = 88
integer taborder = 50
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "VCRNext!"
alignment htextalign = left!
end type

event clicked;STRING		ls_applied_filter


ls_applied_filter = idw_datawindow.inv_filter.of_Move_Next()
parent.event ue_filter_changed(ls_applied_filter)
If ls_applied_filter = '' Then
	idw_datawindow.SetRow(idw_datawindow.GetSelectedRow(0))
End if

end event

type pb_previous from picturebutton within u_dw_filter_control
integer x = 389
integer y = 4
integer width = 101
integer height = 88
integer taborder = 40
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "VCRPrior!"
alignment htextalign = left!
end type

event clicked;STRING		ls_applied_filter


ls_applied_filter = idw_datawindow.inv_filter.of_Move_Previous()
parent.event ue_filter_changed(ls_applied_filter)
If ls_applied_filter = '' Then
	idw_datawindow.SetRow(idw_datawindow.GetSelectedRow(0))
End if

end event

type pb_toggle_filter from picturebutton within u_dw_filter_control
integer x = 247
integer width = 119
integer height = 100
integer taborder = 30
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "filtered.bmp"
string disabledname = "filtered_disabled.bmp"
alignment htextalign = left!
string powertiptext = "Toggle Filter"
end type

event clicked;STRING		ls_applied_filter


ls_applied_filter = idw_datawindow.inv_filter.of_Toggle_Filter()
parent.event ue_filter_changed(ls_applied_filter)

end event

type pb_filter_not_like from picturebutton within u_dw_filter_control
integer x = 123
integer width = 119
integer height = 100
integer taborder = 20
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "filter_not_like.bmp"
alignment htextalign = left!
string powertiptext = "Include all records except selected field."
end type

event clicked;string	 ls_filter

If idw_datawindow.RowCount() > 0 Then
	ls_filter = idw_datawindow.inv_filter.of_filter_selection('notlike')
	pb_toggle_filter.enabled = True
	parent.event ue_filter_changed(ls_filter)
Else
	MessageBox('No records','There is nothing to filter.')
End if
end event

type pb_filter_like from picturebutton within u_dw_filter_control
integer width = 119
integer height = 100
integer taborder = 10
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "filter_like.bmp"
alignment htextalign = left!
string powertiptext = "Include all records like selected field."
end type

event clicked;STRING 	ls_filter


IF NOT ISNULL(idw_datawindow) THEN
	If idw_datawindow.RowCount() > 0 Then
		ls_filter = idw_datawindow.inv_filter.of_filter_selection('like')
		pb_toggle_filter.enabled = True	
		parent.event ue_filter_changed(ls_filter)
	Else
		MessageBox('No records','There is nothing to filter.')
	End if
END IF	
end event

