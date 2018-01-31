$PBExportHeader$w_ungrouped_payments_report_help.srw
forward
global type w_ungrouped_payments_report_help from window
end type
type st_7 from statictext within w_ungrouped_payments_report_help
end type
type st_6 from statictext within w_ungrouped_payments_report_help
end type
type st_5 from statictext within w_ungrouped_payments_report_help
end type
type st_4 from statictext within w_ungrouped_payments_report_help
end type
type st_3 from statictext within w_ungrouped_payments_report_help
end type
type st_2 from statictext within w_ungrouped_payments_report_help
end type
type st_1 from statictext within w_ungrouped_payments_report_help
end type
type cb_ok from commandbutton within w_ungrouped_payments_report_help
end type
type r_1 from rectangle within w_ungrouped_payments_report_help
end type
type r_2 from rectangle within w_ungrouped_payments_report_help
end type
end forward

global type w_ungrouped_payments_report_help from window
integer width = 2331
integer height = 1720
boolean titlebar = true
string title = "Help - Outstanding Payments for Authorization Grouping Report"
boolean controlmenu = true
boolean minbox = true
windowtype windowtype = popup!
long backcolor = 134217731
string icon = "AppIcon!"
boolean center = true
st_7 st_7
st_6 st_6
st_5 st_5
st_4 st_4
st_3 st_3
st_2 st_2
st_1 st_1
cb_ok cb_ok
r_1 r_1
r_2 r_2
end type
global w_ungrouped_payments_report_help w_ungrouped_payments_report_help

on w_ungrouped_payments_report_help.create
this.st_7=create st_7
this.st_6=create st_6
this.st_5=create st_5
this.st_4=create st_4
this.st_3=create st_3
this.st_2=create st_2
this.st_1=create st_1
this.cb_ok=create cb_ok
this.r_1=create r_1
this.r_2=create r_2
this.Control[]={this.st_7,&
this.st_6,&
this.st_5,&
this.st_4,&
this.st_3,&
this.st_2,&
this.st_1,&
this.cb_ok,&
this.r_1,&
this.r_2}
end on

on w_ungrouped_payments_report_help.destroy
destroy(this.st_7)
destroy(this.st_6)
destroy(this.st_5)
destroy(this.st_4)
destroy(this.st_3)
destroy(this.st_2)
destroy(this.st_1)
destroy(this.cb_ok)
destroy(this.r_1)
destroy(this.r_2)
end on

type st_7 from statictext within w_ungrouped_payments_report_help
integer x = 197
integer y = 1288
integer width = 1801
integer height = 160
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
string text = "-     Uncheck the Group For Authorization checkbox in the Payment Maintenance window and SAVE."
boolean focusrectangle = false
end type

type st_6 from statictext within w_ungrouped_payments_report_help
integer x = 137
integer y = 1172
integer width = 1961
integer height = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
string text = "If a payment was identified to be included in an Authorization Group in error:"
boolean focusrectangle = false
end type

type st_5 from statictext within w_ungrouped_payments_report_help
integer x = 197
integer y = 904
integer width = 1801
integer height = 212
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
string text = "-     Payments will then be automatically authorized as a group, or will be waiting in the Payments/Awards Authorization screen to be authorized."
boolean focusrectangle = false
end type

type st_4 from statictext within w_ungrouped_payments_report_help
integer x = 197
integer y = 760
integer width = 1801
integer height = 144
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
string text = "-     Flagged payments can then be added to an existing Authorization Group, or initiate the creation of a new group.  "
boolean focusrectangle = false
end type

type st_3 from statictext within w_ungrouped_payments_report_help
integer x = 197
integer y = 548
integer width = 1801
integer height = 212
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
string text = "-    To add a flagged payment to an Authorization Group, you must access the Payment Grouping window via the Group button in Payment Maintenance."
boolean focusrectangle = false
end type

type st_2 from statictext within w_ungrouped_payments_report_help
integer x = 137
integer y = 440
integer width = 2094
integer height = 76
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
string text = "Follow the instructions below to ensure the flagged payments are grouped:   "
boolean focusrectangle = false
end type

type st_1 from statictext within w_ungrouped_payments_report_help
integer x = 137
integer y = 104
integer width = 2094
integer height = 240
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
string text = "The Outstanding Payments for Authorization Grouping Report identifies any payments that have been flagged to be grouped for authorization, but have not yet been assigned to a group.  To ensure these payments are authorized appropriately, the payments must be added to an Authorization Group. "
boolean focusrectangle = false
end type

type cb_ok from commandbutton within w_ungrouped_payments_report_help
integer x = 914
integer y = 1508
integer width = 402
integer height = 104
integer taborder = 10
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "OK"
end type

event clicked;CLOSE(PARENT)
end event

type r_1 from rectangle within w_ungrouped_payments_report_help
long linecolor = 33554432
integer linethickness = 4
long fillcolor = 16777215
integer x = 55
integer y = 64
integer width = 2190
integer height = 320
end type

type r_2 from rectangle within w_ungrouped_payments_report_help
long linecolor = 33554432
integer linethickness = 4
long fillcolor = 16777215
integer x = 55
integer y = 408
integer width = 2190
integer height = 1072
end type

