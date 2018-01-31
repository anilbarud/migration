$PBExportHeader$w_calendar.srw
forward
global type w_calendar from w_ancestor
end type
type uo_calendar from u_calendar within w_calendar
end type
type ddplb_mode from dropdownpicturelistbox within w_calendar
end type
type pb_goto from picturebutton within w_calendar
end type
type pb_previous from picturebutton within w_calendar
end type
type pb_next from picturebutton within w_calendar
end type
type st_header from statictext within w_calendar
end type
type em_date from editmask within w_calendar
end type
end forward

global type w_calendar from w_ancestor
integer width = 4306
integer height = 3464
string title = ""
string menuname = ""
windowtype windowtype = main!
long backcolor = 33554431
string icon = "AppIcon!"
long il_design_time_height = 4760
long il_design_time_width = 4270
uo_calendar uo_calendar
ddplb_mode ddplb_mode
pb_goto pb_goto
pb_previous pb_previous
pb_next pb_next
st_header st_header
em_date em_date
end type
global w_calendar w_calendar

type variables
//COLORS
CONSTANT  LONG			CLR_BUTTON_FACE = 67108864
CONSTANT  LONG       CLR_NAVY        = 8388608
CONSTANT  LONG			CLR_ACTIVE_TITLEBAR = 134217730

LONG il_individual_no
end variables

forward prototypes
public function integer wf_load_individual (long al_individual_no)
public subroutine wf_initialize_calendar ()
end prototypes

public function integer wf_load_individual (long al_individual_no);datastore		lds_claim_events

lds_claim_events = CREATE datastore

lds_claim_events.DataObject = 'd_claim_event_calendar_datasource'
lds_claim_events.SetTransObject(SQLCA)

lds_claim_events.Retrieve(al_individual_no)

uo_calendar.nf_set_datasource(lds_claim_events)
uo_calendar.nf_load_datasource()
uo_calendar.nf_set_display_date(Today())
uo_calendar.nf_filter_for_month()

uo_calendar.il_individual_no = al_individual_no

return 1
end function

public subroutine wf_initialize_calendar ();
uo_calendar.SetRedraw(FALSE)
wf_load_individual(il_individual_no)

uo_calendar.nf_load_company_calendar(Year(Today()))
uo_calendar.nf_display_company_calendar(True)

This.wf_setresize(true)

inv_resize.of_register(uo_calendar,'scaletoright&bottom')
inv_resize.of_register(uo_calendar.r_1,'scaletoright&bottom')

inv_resize.of_register(pb_previous,50,0,0,0)
inv_resize.of_register(pb_next,50,0,0,0)
inv_resize.of_register(st_header,50,0,0,0)
inv_resize.of_register(ddplb_mode,100,0,0,0)
inv_resize.of_register(em_date,100,0,0,0)
inv_resize.of_register(pb_goto,100,0,0,0)

This.Title = 'Calendar for Individual #' + String(il_individual_no)
uo_calendar.SetRedraw(TRUE)
end subroutine

event open;
il_individual_no = Message.DoubleParm
wf_initialize_calendar()


end event

on w_calendar.create
int iCurrent
call super::create
this.uo_calendar=create uo_calendar
this.ddplb_mode=create ddplb_mode
this.pb_goto=create pb_goto
this.pb_previous=create pb_previous
this.pb_next=create pb_next
this.st_header=create st_header
this.em_date=create em_date
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.uo_calendar
this.Control[iCurrent+2]=this.ddplb_mode
this.Control[iCurrent+3]=this.pb_goto
this.Control[iCurrent+4]=this.pb_previous
this.Control[iCurrent+5]=this.pb_next
this.Control[iCurrent+6]=this.st_header
this.Control[iCurrent+7]=this.em_date
end on

on w_calendar.destroy
call super::destroy
destroy(this.uo_calendar)
destroy(this.ddplb_mode)
destroy(this.pb_goto)
destroy(this.pb_previous)
destroy(this.pb_next)
destroy(this.st_header)
destroy(this.em_date)
end on

event resize;call super::resize;uo_calendar.event ue_resize()
end event

type uo_calendar from u_calendar within w_calendar
integer x = 23
integer y = 164
integer width = 4178
integer height = 3092
integer taborder = 30
end type

on uo_calendar.destroy
call u_calendar::destroy
end on

event ue_date_changing;call super::ue_date_changing;st_header.text = String(ad_new_date,'MMM dd, yyyy')

return 1
end event

type ddplb_mode from dropdownpicturelistbox within w_calendar
boolean visible = false
integer x = 2619
integer y = 24
integer width = 640
integer height = 376
integer taborder = 120
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean sorted = false
string item[] = {"Weekend split","",""}
borderstyle borderstyle = stylelowered!
integer itempictureindex[] = {1,0,0}
string picturename[] = {"BorderNone!","Tile!","Layer!"}
long picturemaskcolor = 536870912
end type

event selectionchanged;
uo_calendar.nf_set_weekend_mode(index -1)
end event

type pb_goto from picturebutton within w_calendar
integer x = 4041
integer y = 40
integer width = 133
integer height = 92
integer taborder = 30
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "go"
boolean default = true
alignment htextalign = left!
long textcolor = 16777215
long backcolor = 25511203
end type

event clicked;uo_calendar.nf_set_display_date(Date(em_date.text))
uo_calendar.nf_filter_for_month()
end event

type pb_previous from picturebutton within w_calendar
integer x = 1577
integer y = 36
integer width = 110
integer height = 96
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean flatstyle = true
string picturename = "arrow_left_orange_16.gif"
alignment htextalign = left!
string powertiptext = "Previous Month"
end type

event clicked;uo_calendar.nf_increment_date('m',-1)
end event

type pb_next from picturebutton within w_calendar
integer x = 2528
integer y = 36
integer width = 110
integer height = 96
integer taborder = 10
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean flatstyle = true
string picturename = "arrow_right_orange_16.gif"
alignment htextalign = left!
string powertiptext = "Next Month"
end type

event clicked;uo_calendar.nf_increment_date('m',1)
end event

type st_header from statictext within w_calendar
integer x = 1719
integer y = 24
integer width = 773
integer height = 108
integer textsize = -18
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Berlin Sans FB Demi"
long textcolor = 25511203
long backcolor = 553648127
string text = "Jan 19, 2006"
alignment alignment = center!
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_date from editmask within w_calendar
integer x = 3666
integer y = 48
integer width = 375
integer height = 76
integer taborder = 110
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Berlin Sans FB Demi"
long textcolor = 33554432
maskdatatype maskdatatype = datemask!
string mask = "yyyy-mm-dd"
end type

