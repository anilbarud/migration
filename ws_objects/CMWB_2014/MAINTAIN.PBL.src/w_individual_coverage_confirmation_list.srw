$PBExportHeader$w_individual_coverage_confirmation_list.srw
forward
global type w_individual_coverage_confirmation_list from window
end type
type gb_1 from groupbox within w_individual_coverage_confirmation_list
end type
type cb_no from commandbutton within w_individual_coverage_confirmation_list
end type
type cb_yes from commandbutton within w_individual_coverage_confirmation_list
end type
type dw_coverage_list from u_dw_online within w_individual_coverage_confirmation_list
end type
type st_confirmation from statictext within w_individual_coverage_confirmation_list
end type
end forward

global type w_individual_coverage_confirmation_list from window
integer width = 2898
integer height = 1312
boolean titlebar = true
windowtype windowtype = response!
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
gb_1 gb_1
cb_no cb_no
cb_yes cb_yes
dw_coverage_list dw_coverage_list
st_confirmation st_confirmation
end type
global w_individual_coverage_confirmation_list w_individual_coverage_confirmation_list

event open;s_window_message   lst_message
u_ds					lds_individual_coverage


// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


lst_message = message.powerobjectparm

this.title = lst_message.as_stringparm[1]
st_confirmation.text = lst_message.as_stringparm[2]

lds_individual_coverage = lst_message.apo_powerobjectparm[1]

dw_coverage_list.dataobject = lds_individual_coverage.dataobject
dw_coverage_list.object.data = lds_individual_coverage.object.data
end event

on w_individual_coverage_confirmation_list.create
this.gb_1=create gb_1
this.cb_no=create cb_no
this.cb_yes=create cb_yes
this.dw_coverage_list=create dw_coverage_list
this.st_confirmation=create st_confirmation
this.Control[]={this.gb_1,&
this.cb_no,&
this.cb_yes,&
this.dw_coverage_list,&
this.st_confirmation}
end on

on w_individual_coverage_confirmation_list.destroy
destroy(this.gb_1)
destroy(this.cb_no)
destroy(this.cb_yes)
destroy(this.dw_coverage_list)
destroy(this.st_confirmation)
end on

type gb_1 from groupbox within w_individual_coverage_confirmation_list
integer x = 14
integer y = 12
integer width = 2843
integer height = 552
integer taborder = 10
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Confirmation required:"
end type

type cb_no from commandbutton within w_individual_coverage_confirmation_list
integer x = 1422
integer y = 1096
integer width = 325
integer height = 100
integer taborder = 30
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&No"
boolean default = true
end type

event clicked;//Return "not covered"
CloseWithReturn(w_individual_coverage_confirmation_list,0)
end event

type cb_yes from commandbutton within w_individual_coverage_confirmation_list
integer x = 1083
integer y = 1096
integer width = 325
integer height = 100
integer taborder = 30
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Yes"
end type

event clicked;//Return "covered"
CloseWithReturn(w_individual_coverage_confirmation_list,1)
end event

type dw_coverage_list from u_dw_online within w_individual_coverage_confirmation_list
integer x = 5
integer y = 576
integer width = 2857
integer height = 504
integer taborder = 10
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

type st_confirmation from statictext within w_individual_coverage_confirmation_list
integer x = 114
integer y = 96
integer width = 2615
integer height = 440
boolean bringtotop = true
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
long bordercolor = 16777215
boolean focusrectangle = false
end type

