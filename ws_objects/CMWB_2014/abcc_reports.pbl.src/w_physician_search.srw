$PBExportHeader$w_physician_search.srw
forward
global type w_physician_search from window
end type
type dw_physician_search from u_dw_online within w_physician_search
end type
type em_physician_name from editmask within w_physician_search
end type
type em_physician_id from editmask within w_physician_search
end type
type st_2 from statictext within w_physician_search
end type
type st_1 from statictext within w_physician_search
end type
type cb_close from commandbutton within w_physician_search
end type
type cb_ok from commandbutton within w_physician_search
end type
type cb_clear from commandbutton within w_physician_search
end type
type cb_search from commandbutton within w_physician_search
end type
end forward

global type w_physician_search from window
integer width = 3259
integer height = 2592
boolean titlebar = true
string title = "Physician Search"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 67108864
string icon = "AppIcon!"
boolean clientedge = true
boolean center = true
dw_physician_search dw_physician_search
em_physician_name em_physician_name
em_physician_id em_physician_id
st_2 st_2
st_1 st_1
cb_close cb_close
cb_ok cb_ok
cb_clear cb_clear
cb_search cb_search
end type
global w_physician_search w_physician_search

on w_physician_search.create
this.dw_physician_search=create dw_physician_search
this.em_physician_name=create em_physician_name
this.em_physician_id=create em_physician_id
this.st_2=create st_2
this.st_1=create st_1
this.cb_close=create cb_close
this.cb_ok=create cb_ok
this.cb_clear=create cb_clear
this.cb_search=create cb_search
this.Control[]={this.dw_physician_search,&
this.em_physician_name,&
this.em_physician_id,&
this.st_2,&
this.st_1,&
this.cb_close,&
this.cb_ok,&
this.cb_clear,&
this.cb_search}
end on

on w_physician_search.destroy
destroy(this.dw_physician_search)
destroy(this.em_physician_name)
destroy(this.em_physician_id)
destroy(this.st_2)
destroy(this.st_1)
destroy(this.cb_close)
destroy(this.cb_ok)
destroy(this.cb_clear)
destroy(this.cb_search)
end on

event open;Long    ll_row, ll_num_rows
Integer li_rtn

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


//DataWindowChild ldwc_child
//
SetPointer(HourGlass!)
li_rtn = dw_physician_search.SetTransObject(SQLCA)
ll_row = dw_physician_search.InsertRow(0)
ll_num_rows = dw_physician_search.Retrieve('%', '%')

li_rtn = SQLCA.nf_handle_error("w_physician_search", "", "open - dw_physician_search.Retrieve()")

end event

type dw_physician_search from u_dw_online within w_physician_search
integer x = 18
integer y = 356
integer width = 3182
integer height = 1980
integer taborder = 40
string title = "none"
string dataobject = "d_physician_search"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

type em_physician_name from editmask within w_physician_search
integer x = 928
integer y = 196
integer width = 955
integer height = 96
integer taborder = 30
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string mask = "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
boolean autoskip = true
end type

type em_physician_id from editmask within w_physician_search
integer x = 928
integer y = 32
integer width = 402
integer height = 96
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string mask = "##########"
end type

type st_2 from statictext within w_physician_search
integer x = 206
integer y = 212
integer width = 690
integer height = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Physician Name Contains:"
boolean focusrectangle = false
end type

type st_1 from statictext within w_physician_search
integer x = 210
integer y = 48
integer width = 402
integer height = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Physician ID:"
boolean focusrectangle = false
end type

type cb_close from commandbutton within w_physician_search
integer x = 1664
integer y = 2364
integer width = 315
integer height = 96
integer taborder = 40
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Clos&e"
end type

event clicked;CloseWithReturn(Parent, 0)
end event

type cb_ok from commandbutton within w_physician_search
integer x = 1207
integer y = 2364
integer width = 315
integer height = 96
integer taborder = 30
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
end type

event clicked;Long ll_row
String ls_physician

ll_row = dw_physician_search.GetRow()
IF ll_row = 0 OR IsNull(ll_row) = TRUE THEN
	Messagebox("No Physician Selected", "Please select a Physician or click the Close button to cancel this search.")
	RETURN
END IF

ls_physician = dw_physician_search.GetItemString(ll_row, "physician_ID")
CloseWithReturn(Parent, ls_physician)
end event

type cb_clear from commandbutton within w_physician_search
integer x = 2505
integer y = 196
integer width = 315
integer height = 96
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Clear"
end type

event clicked;

em_physician_name.text =""
em_physician_id.text =""
dw_physician_search.Reset()
end event

type cb_search from commandbutton within w_physician_search
integer x = 2505
integer y = 32
integer width = 315
integer height = 96
integer taborder = 10
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Search"
boolean default = true
end type

event clicked;long    ll_num_rows
Integer li_rtn
String  lstr_physician_name, lstr_physician_id

lstr_physician_name = em_physician_name.Text
lstr_physician_id = em_physician_id.Text

if  (Len(lstr_physician_name) <=0 ) and ( Len(lstr_physician_id) <=0 )then
	messagebox("Incorrect Search Parameters","You must input either a Physician ID or a Physician Name for a valid search")
	return 1
end if 

if Len(lstr_physician_id) > 0 then
	lstr_physician_id =  "%" + lstr_physician_id +"%" 
	lstr_physician_name = " "
elseif Len(lstr_physician_name) > 0 then
	lstr_physician_name =  "%" + lstr_physician_name +"%" 
	lstr_physician_id = " "
end if

ll_num_rows = dw_physician_search.Retrieve(lstr_physician_id, lstr_physician_name)
li_rtn = SQLCA.nf_handle_error("w_physician_search", "", "cb_search - dw_physician_search.retrieve(lstr_physician_id,lstr_physician_name)")

IF ll_num_rows = 0 THEN
	MessageBox("No Physicians Found", "Please refine your search criteria and try again.", Information!)
	RETURN
END IF
//use just one of the parameters 
//the id would take precedence
//or
//use both of the parameters

end event

