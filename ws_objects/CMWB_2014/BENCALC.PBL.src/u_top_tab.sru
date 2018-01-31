$PBExportHeader$u_top_tab.sru
$PBExportComments$User Object - Tab for folder control
forward
global type u_top_tab from UserObject
end type
type st_tab from statictext within u_top_tab
end type
type p_tab from picture within u_top_tab
end type
end forward

global type u_top_tab from UserObject
int Width=517
int Height=133
boolean Border=true
long BackColor=67108864
event ue_clicked pbm_custom01
st_tab st_tab
p_tab p_tab
end type
global u_top_tab u_top_tab

type variables
Integer          ii_tab_index
end variables

on ue_clicked;//	declare a reference variable for the user object u_top_bar

u_top_bar vlu_bar

//	assign the parent object (the bar) of this object (the tab) to the
//	variable in order to have a reference

vlu_bar = parent

//	update variables in the bar

vlu_bar.ii_lastindex = vlu_bar.ii_index
vlu_bar.ii_index = ii_tab_index

//	trigger the event in the bar to tell the bar it has been clicked

vlu_bar.TriggerEvent ("ue_bar_clicked")
end on

on u_top_tab.create
this.st_tab=create st_tab
this.p_tab=create p_tab
this.Control[]={ this.st_tab,&
this.p_tab}
end on

on u_top_tab.destroy
destroy(this.st_tab)
destroy(this.p_tab)
end on

type st_tab from statictext within u_top_tab
int X=37
int Y=41
int Width=417
int Height=65
int TabOrder=10
Alignment Alignment=Center!
boolean FocusRectangle=false
long TextColor=33554432
long BackColor=67108864
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

on clicked;//	fire off the user object user event 

p_tab.triggerEvent(clicked!)
end on

type p_tab from picture within u_top_tab
int X=5
int Y=17
int Width=508
int Height=109
int TabOrder=20
string PictureName="tab_down.bmp"
boolean FocusRectangle=false
end type

on clicked;//	fire off the user object user event 

parent.triggerEvent("ue_clicked")
end on

