$PBExportHeader$w_default_regions.srw
$PBExportComments$Response window to select proper admin region for out of province addresses when creating claims - EKP July 16/98
forward
global type w_default_regions from window
end type
type rb_prov from radiobutton within w_default_regions
end type
type cb_cancel from commandbutton within w_default_regions
end type
type cb_ok from commandbutton within w_default_regions
end type
type rb_southwest from radiobutton within w_default_regions
end type
type rb_southeast from radiobutton within w_default_regions
end type
type rb_northwest from radiobutton within w_default_regions
end type
type rb_northeast from radiobutton within w_default_regions
end type
type gb_admin_region from groupbox within w_default_regions
end type
end forward

global type w_default_regions from window
integer x = 1614
integer y = 984
integer width = 1211
integer height = 832
boolean titlebar = true
string title = "Administration Regions"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 67108864
rb_prov rb_prov
cb_cancel cb_cancel
cb_ok cb_ok
rb_southwest rb_southwest
rb_southeast rb_southeast
rb_northwest rb_northwest
rb_northeast rb_northeast
gb_admin_region gb_admin_region
end type
global w_default_regions w_default_regions

on w_default_regions.create
this.rb_prov=create rb_prov
this.cb_cancel=create cb_cancel
this.cb_ok=create cb_ok
this.rb_southwest=create rb_southwest
this.rb_southeast=create rb_southeast
this.rb_northwest=create rb_northwest
this.rb_northeast=create rb_northeast
this.gb_admin_region=create gb_admin_region
this.Control[]={this.rb_prov,&
this.cb_cancel,&
this.cb_ok,&
this.rb_southwest,&
this.rb_southeast,&
this.rb_northwest,&
this.rb_northeast,&
this.gb_admin_region}
end on

on w_default_regions.destroy
destroy(this.rb_prov)
destroy(this.cb_cancel)
destroy(this.cb_ok)
destroy(this.rb_southwest)
destroy(this.rb_southeast)
destroy(this.rb_northwest)
destroy(this.rb_northeast)
destroy(this.gb_admin_region)
end on

event open;String ls_parm, ls_default

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


ls_parm = Message.StringParm

SELECT default_admin_region_code 
INTO :ls_default
FROM Location
WHERE prov_state_code = :ls_parm;

IF ls_default = 'NE' THEN
	rb_northeast.Checked = True
ELSEIF ls_default = 'NW' THEN
	rb_northwest.Checked = True
ELSEIF ls_default = 'SE' THEN
	rb_southeast.Checked = True
ELSEIF ls_default = 'SW' THEN
	rb_southwest.Checked = True
ELSE 
	rb_prov.Checked = True
END IF
end event

event close;w_create_claim.SetFocus()
end event

type rb_prov from radiobutton within w_default_regions
integer x = 206
integer y = 500
integer width = 411
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Provincial"
end type

type cb_cancel from commandbutton within w_default_regions
integer x = 786
integer y = 300
integer width = 247
integer height = 108
integer taborder = 10
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cancel"
boolean cancel = true
end type

event clicked;Close(w_default_regions)
end event

type cb_ok from commandbutton within w_default_regions
integer x = 786
integer y = 164
integer width = 247
integer height = 108
integer taborder = 30
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "OK"
boolean default = true
end type

event clicked;IF rb_northeast.Checked = True THEN
	CloseWithReturn(w_default_regions,'NE')
ELSEIF rb_northwest.Checked = True THEN
	CloseWithReturn(w_default_regions,'NW')
ELSEIF rb_southeast.Checked = True THEN
	CloseWithReturn(w_default_regions,'SE')
ELSEIF rb_southwest.Checked = True THEN
	CloseWithReturn(w_default_regions,'SW')
ELSEIF rb_prov.Checked = True THEN
	CloseWithReturn(w_default_regions,'PRV')
ELSE
	MessageBox("Administration Regions","No Region Selected.  Please Select ~rA Region To Continue Or Press CANCEL To Exit.",Information!)
END IF 

end event

type rb_southwest from radiobutton within w_default_regions
integer x = 206
integer y = 412
integer width = 411
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Southwest"
end type

type rb_southeast from radiobutton within w_default_regions
integer x = 206
integer y = 324
integer width = 411
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Southeast"
end type

type rb_northwest from radiobutton within w_default_regions
integer x = 206
integer y = 236
integer width = 411
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Northwest"
end type

type rb_northeast from radiobutton within w_default_regions
integer x = 206
integer y = 148
integer width = 411
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Northeast"
end type

type gb_admin_region from groupbox within w_default_regions
integer x = 87
integer y = 72
integer width = 1051
integer height = 576
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Select Administration Region:"
borderstyle borderstyle = styleraised!
end type

