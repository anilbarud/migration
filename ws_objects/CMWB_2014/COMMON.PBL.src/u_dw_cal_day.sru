$PBExportHeader$u_dw_cal_day.sru
forward
global type u_dw_cal_day from datawindow
end type
end forward

global type u_dw_cal_day from datawindow
integer width = 453
integer height = 300
string dataobject = "d_day"
boolean controlmenu = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type
global u_dw_cal_day u_dw_cal_day

type variables
DATE			id_display_date
STRING	 is_original_colour
end variables

forward prototypes
public function integer nf_set_display_date (date ad_display_date, string as_day_text)
public function integer nf_set_display_date (date ad_display_date)
end prototypes

public function integer nf_set_display_date (date ad_display_date, string as_day_text);STRING		ls_filter

id_display_date = ad_display_date

ls_filter = "Date(event_date) = Date('" + String(ad_display_date,'yyyy-mm-dd') + "') "
This.SetFilter(ls_filter)
This.Filter()
//Set the daynumber
This.object.t_day_number.text = String(Day(ad_display_date))

This.object.t_day_text.text = as_day_text

RETURN 1

end function

public function integer nf_set_display_date (date ad_display_date);nf_set_display_date(ad_display_date,'')

return 1
end function

event getfocus;
is_original_colour = this.object.datawindow.color

this.SetRedraw(FALSE)
this.object.datawindow.color = RGB(209,146,82)
this.object.t_day_number.color = RGB(255,255,255)
this.object.t_day_text.color = RGB(255,255,255)
this.object.event_description.color = RGB(255,255,255)
this.SetRedraw(TRUE)

parent.dynamic nf_set_display_date(id_display_date)

parent.dynamic nf_populate_daily(id_display_date)
end event

event losefocus;this.SetRedraw(FALSE)
this.object.datawindow.color = is_original_colour
this.object.event_description.color = RGB(0,0,0)
this.object.t_day_number.color = RGB(0,0,0)
this.object.t_day_text.color = RGB(0,0,0)
this.SetRedraw(TRUE)

end event

on u_dw_cal_day.create
end on

on u_dw_cal_day.destroy
end on

