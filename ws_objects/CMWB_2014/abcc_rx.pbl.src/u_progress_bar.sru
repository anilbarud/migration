$PBExportHeader$u_progress_bar.sru
$PBExportComments$Used in batch processes to show the progress of the run (i.e. meter)
forward
global type u_progress_bar from UserObject
end type
type st_1 from statictext within u_progress_bar
end type
type rc_2 from rectangle within u_progress_bar
end type
end forward

global type u_progress_bar from UserObject
int Width=1207
int Height=140
boolean Border=true
long PictureMaskColor=25166016
long TabBackColor=67108864
st_1 st_1
rc_2 rc_2
end type
global u_progress_bar u_progress_bar

forward prototypes
public subroutine set_position (decimal percent_complete)
end prototypes

public subroutine set_position (decimal percent_complete);// SUBROUTINE SET_POSITION(decimal percent_complete)

rc_2.width = percent_complete / 100.0 * this.width
st_1.text = String(percent_complete/100.0, "###%")



end subroutine

on u_progress_bar.create
this.st_1=create st_1
this.rc_2=create rc_2
this.Control[]={this.st_1,&
this.rc_2}
end on

on u_progress_bar.destroy
destroy(this.st_1)
destroy(this.rc_2)
end on

type st_1 from statictext within u_progress_bar
int X=494
int Y=32
int Width=219
int Height=64
string Text="0%"
Alignment Alignment=Center!
long TextColor=16777215
long BackColor=16711680
long BorderColor=16711680
int TextSize=-10
int Weight=700
string FaceName="Helv"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

type rc_2 from rectangle within u_progress_bar
int Height=144
boolean Enabled=false
int LineThickness=4
long FillColor=16711680
end type

