$PBExportHeader$w_mdi_clock.srw
$PBExportComments$Places a clock on the MDI frame
forward
global type w_mdi_clock from window
end type
type st_2 from statictext within w_mdi_clock
end type
type st_1 from statictext within w_mdi_clock
end type
type st_time from statictext within w_mdi_clock
end type
end forward

global type w_mdi_clock from window
integer x = 681
integer y = 272
integer width = 603
integer height = 60
boolean enabled = false
boolean border = false
windowtype windowtype = popup!
long backcolor = 67108864
event wininichange pbm_wininichange
st_2 st_2
st_1 st_1
st_time st_time
end type
global w_mdi_clock w_mdi_clock

type prototypes




end prototypes

type variables
window iw_parent_window
integer ii_menu_ht = 0
integer ii_menu_ht2  = 74, ii_not_menu_ht =0
integer ii_resizeable_offset,ii_border,ii_border_width,ii_border_height

end variables

forward prototypes
public subroutine wf_parent_resized ()
end prototypes

public subroutine wf_parent_resized ();// move the window so it is positioned over the lower right hand portion of the Microhelp bar
int winht,winwd
winht = iw_parent_window.y + workspaceheight(iw_parent_window) + ii_border_height + 60 + ii_menu_ht
winwd = iw_parent_window.x + workspacewidth(iw_parent_window) +  ii_border_width - (this.width + 65)
move(this,winwd, winht)

end subroutine

event timer;//int	vli_sys_resource

//Jan C - Y2K - chg'd format from "m/dd/yy" to "yyyy/mm/dd"
st_time.text = string(today(),"yyyy/mm/dd")+" "+string(now(),"h:mm am/pm  ") 
//Tammy Moore commented this out
//vli_sys_resource = GetFreeSystemResources(0) 

// Set background color of system resources 
//IF vli_sys_resource < 20 THEN
//	st_system.BackColor = RGB(255,0,0)			// Red
//ELSE
//	st_system.BackColor = RGB(192,192,192)		// Grey
//END IF
//
//st_system.text = "Resources: " + string( vli_sys_resource,"###") + " %" 
//

end event

event open;INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


// Initialize and determine environment information 
iw_parent_window = parentwindow()

this.width   = st_time.width + 105
this.height  = st_time.height + 8

if len(iw_parent_window.menuname) > 0 then
	ii_menu_ht = ii_menu_ht2
else
	ii_menu_ht = ii_not_menu_ht
end if


ii_border = 	integer(ProfileString("win.ini", "windows", "borderwidth", "2")) 
ii_border_height = 4 * ii_border
ii_border_width = pixelstounits(ii_border,xpixelstounits!)

// If the window argument passed is resizable...
if iw_parent_window.resizable then
	ii_resizeable_offset = 0
else
	ii_resizeable_offset = 8*(ii_border+2)
end if
	
timer(30)
this.triggerevent(timer!)
wf_parent_resized()

end event

on w_mdi_clock.create
this.st_2=create st_2
this.st_1=create st_1
this.st_time=create st_time
this.Control[]={this.st_2,&
this.st_1,&
this.st_time}
end on

on w_mdi_clock.destroy
destroy(this.st_2)
destroy(this.st_1)
destroy(this.st_time)
end on

type st_2 from statictext within w_mdi_clock
integer width = 41
integer height = 60
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_1 from statictext within w_mdi_clock
integer x = 558
integer width = 41
integer height = 60
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_time from statictext within w_mdi_clock
integer x = 59
integer y = 4
integer width = 498
integer height = 52
integer textsize = -8
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "2000/01/12 09:12 pm  "
alignment alignment = right!
boolean focusrectangle = false
end type

