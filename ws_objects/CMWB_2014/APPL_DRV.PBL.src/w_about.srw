$PBExportHeader$w_about.srw
$PBExportComments$Help About - display current version information
forward
global type w_about from window
end type
type st_4 from statictext within w_about
end type
type st_1 from statictext within w_about
end type
type p_1 from picture within w_about
end type
type st_2 from statictext within w_about
end type
type cb_ok from commandbutton within w_about
end type
type dw_cast_and_crew from u_dw_online within w_about
end type
type ln_1 from line within w_about
end type
end forward

global type w_about from window
integer x = 1335
integer y = 688
integer width = 1952
integer height = 884
boolean titlebar = true
string title = "About"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 16777215
event delay_display pbm_custom01
st_4 st_4
st_1 st_1
p_1 p_1
st_2 st_2
cb_ok cb_ok
dw_cast_and_crew dw_cast_and_crew
ln_1 ln_1
end type
global w_about w_about

event open;INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


dw_cast_and_crew.InsertRow(1)
dw_cast_and_crew.SetItem(1,"cast_member_name","Cheryl Clancy")
dw_cast_and_crew.InsertRow(1)
dw_cast_and_crew.SetItem(1,"cast_member_name","Pat Williams")
dw_cast_and_crew.InsertRow(1)
dw_cast_and_crew.SetItem(1,"cast_member_name","Ralph Eastwood")
dw_cast_and_crew.InsertRow(1)
dw_cast_and_crew.SetItem(1,"cast_member_name","Peter Waller")
dw_cast_and_crew.InsertRow(1)
dw_cast_and_crew.SetItem(1,"cast_member_name","Wendy Doherty")
dw_cast_and_crew.InsertRow(1)
dw_cast_and_crew.SetItem(1,"cast_member_name","Bill Grey")
dw_cast_and_crew.InsertRow(1)
dw_cast_and_crew.SetItem(1,"cast_member_name","Earl Assoon")
dw_cast_and_crew.InsertRow(1)
dw_cast_and_crew.SetItem(1,"cast_member_name","Brian Burton")
dw_cast_and_crew.InsertRow(1)
dw_cast_and_crew.SetItem(1,"cast_member_name","Janet Chiswell")
dw_cast_and_crew.InsertRow(1)
dw_cast_and_crew.SetItem(1,"cast_member_name","Paul Clark")
dw_cast_and_crew.InsertRow(1)
dw_cast_and_crew.SetItem(1,"cast_member_name","Pierrette Dixon")
dw_cast_and_crew.InsertRow(1)
dw_cast_and_crew.SetItem(1,"cast_member_name","Margie Duchesneau")
dw_cast_and_crew.InsertRow(1)
dw_cast_and_crew.SetItem(1,"cast_member_name","Mike George")
dw_cast_and_crew.InsertRow(1)
dw_cast_and_crew.SetItem(1,"cast_member_name","Janet Kempster")
dw_cast_and_crew.InsertRow(1)
dw_cast_and_crew.SetItem(1,"cast_member_name","Jacqui Phinney")
dw_cast_and_crew.InsertRow(1)
dw_cast_and_crew.SetItem(1,"cast_member_name","Lorie Vaughan")
dw_cast_and_crew.InsertRow(1)
dw_cast_and_crew.SetItem(1,"cast_member_name","Paul Vautour")
dw_cast_and_crew.InsertRow(1)
dw_cast_and_crew.SetItem(1,"cast_member_name","Dany Vogel")


Timer(1)

end event

on timer;integer vli_rownum

vli_rownum = dw_cast_and_crew.GetRow()
If vli_rownum = dw_cast_and_crew.Rowcount() then
	vli_rownum = 0
	dw_cast_and_crew.Modify("st_role.Text='Led By:'")
ElseIf vli_rownum = 1 then
	dw_cast_and_crew.Modify("st_role.Text='Developers:'")
End If

dw_cast_and_crew.ScrollToRow(vli_rownum+1)

Timer(.75)
end on

on w_about.create
this.st_4=create st_4
this.st_1=create st_1
this.p_1=create p_1
this.st_2=create st_2
this.cb_ok=create cb_ok
this.dw_cast_and_crew=create dw_cast_and_crew
this.ln_1=create ln_1
this.Control[]={this.st_4,&
this.st_1,&
this.p_1,&
this.st_2,&
this.cb_ok,&
this.dw_cast_and_crew,&
this.ln_1}
end on

on w_about.destroy
destroy(this.st_4)
destroy(this.st_1)
destroy(this.p_1)
destroy(this.st_2)
destroy(this.cb_ok)
destroy(this.dw_cast_and_crew)
destroy(this.ln_1)
end on

type st_4 from statictext within w_about
integer x = 183
integer y = 672
integer width = 306
integer height = 52
integer textsize = -9
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = script!
string facename = "Brush Script MT"
long textcolor = 33554432
long backcolor = 16777215
boolean enabled = false
string text = "Version 4.0"
alignment alignment = center!
boolean focusrectangle = false
end type

type st_1 from statictext within w_about
integer x = 672
integer y = 32
integer width = 1211
integer height = 80
integer textsize = -11
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Century Gothic"
long textcolor = 33554432
long backcolor = 16777215
boolean enabled = false
string text = "Case Management Workbench"
alignment alignment = center!
boolean focusrectangle = false
end type

type p_1 from picture within w_about
integer x = 27
integer y = 36
integer width = 622
integer height = 696
string picturename = "handy.bmp"
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_2 from statictext within w_about
integer x = 667
integer y = 152
integer width = 1138
integer height = 144
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
boolean enabled = false
string text = "by WorkSafeNB"
alignment alignment = center!
boolean focusrectangle = false
end type

type cb_ok from commandbutton within w_about
integer x = 754
integer y = 628
integer width = 1006
integer height = 104
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "OK"
end type

on clicked;Close(Parent)
end on

type dw_cast_and_crew from u_dw_online within w_about
integer x = 928
integer y = 316
integer width = 640
integer height = 280
integer taborder = 10
boolean enabled = false
string dataobject = "d_cast_and_crew"
boolean border = false
end type

type ln_1 from line within w_about
long linecolor = 33554432
integer linethickness = 5
integer beginx = 686
integer beginy = 128
integer endx = 1861
integer endy = 128
end type

