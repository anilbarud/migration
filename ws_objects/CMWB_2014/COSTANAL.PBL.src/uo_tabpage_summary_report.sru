$PBExportHeader$uo_tabpage_summary_report.sru
forward
global type uo_tabpage_summary_report from userobject
end type
type dw_print_version from datawindow within uo_tabpage_summary_report
end type
type dw_print_one_option from u_dw_online within uo_tabpage_summary_report
end type
type cbx_options from checkbox within uo_tabpage_summary_report
end type
type dw_summary_report from u_dw_online within uo_tabpage_summary_report
end type
type cb_print from commandbutton within uo_tabpage_summary_report
end type
end forward

global type uo_tabpage_summary_report from userobject
integer width = 2578
integer height = 1540
long backcolor = 67108864
long tabtextcolor = 33554432
long tabbackcolor = 268435456
long picturemaskcolor = 536870912
dw_print_version dw_print_version
dw_print_one_option dw_print_one_option
cbx_options cbx_options
dw_summary_report dw_summary_report
cb_print cb_print
end type
global uo_tabpage_summary_report uo_tabpage_summary_report

on uo_tabpage_summary_report.create
this.dw_print_version=create dw_print_version
this.dw_print_one_option=create dw_print_one_option
this.cbx_options=create cbx_options
this.dw_summary_report=create dw_summary_report
this.cb_print=create cb_print
this.Control[]={this.dw_print_version,&
this.dw_print_one_option,&
this.cbx_options,&
this.dw_summary_report,&
this.cb_print}
end on

on uo_tabpage_summary_report.destroy
destroy(this.dw_print_version)
destroy(this.dw_print_one_option)
destroy(this.cbx_options)
destroy(this.dw_summary_report)
destroy(this.cb_print)
end on

type dw_print_version from datawindow within uo_tabpage_summary_report
boolean visible = false
integer x = 1723
integer y = 1308
integer width = 494
integer height = 112
integer taborder = 12
string dataobject = "d_cost_analysis_summary_report_print"
boolean livescroll = true
end type

type dw_print_one_option from u_dw_online within uo_tabpage_summary_report
boolean visible = false
integer x = 1234
integer y = 1308
integer height = 112
integer taborder = 2
string dataobject = "d_c_a_summary_report_by_option"
end type

type cbx_options from checkbox within uo_tabpage_summary_report
integer x = 471
integer y = 1440
integer width = 736
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Print chosen option only"
boolean lefttext = true
end type

type dw_summary_report from u_dw_online within uo_tabpage_summary_report
integer x = 14
integer y = 16
integer width = 2542
integer height = 1396
integer taborder = 10
string dataobject = "d_cost_analysis_summary_report"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

type cb_print from commandbutton within uo_tabpage_summary_report
integer x = 14
integer y = 1412
integer width = 379
integer height = 100
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Print"
end type

event clicked;
SetPointer(HourGlass!)
IF cbx_options.checked THEN
	IF dw_print_one_option.RowCount() > 0 THEN
		dw_print_one_option.Print()
	ELSE
		MessageBox('No option choosen','There is no recommended option entered to print.')
	END IF
ELSE	
	IF dw_print_version.RowCount() > 0 THEN
		dw_print_version.Print()
	ELSE
		MessageBox('No data to report','There is no data available to report on.')
	END IF
END IF
end event

