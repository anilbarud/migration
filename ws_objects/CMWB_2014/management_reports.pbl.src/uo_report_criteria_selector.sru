$PBExportHeader$uo_report_criteria_selector.sru
forward
global type uo_report_criteria_selector from userobject
end type
type tab_tabpage_container from u_tab_selection within uo_report_criteria_selector
end type
type tab_tabpage_container from u_tab_selection within uo_report_criteria_selector
end type
end forward

global type uo_report_criteria_selector from userobject
integer width = 2153
integer height = 944
long backcolor = 67108864
string text = "none"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
tab_tabpage_container tab_tabpage_container
end type
global uo_report_criteria_selector uo_report_criteria_selector

on uo_report_criteria_selector.create
this.tab_tabpage_container=create tab_tabpage_container
this.Control[]={this.tab_tabpage_container}
end on

on uo_report_criteria_selector.destroy
destroy(this.tab_tabpage_container)
end on

type tab_tabpage_container from u_tab_selection within uo_report_criteria_selector
integer y = 8
integer width = 2144
integer height = 932
integer taborder = 10
end type

