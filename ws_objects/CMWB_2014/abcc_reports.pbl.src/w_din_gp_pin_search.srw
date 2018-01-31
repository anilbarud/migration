$PBExportHeader$w_din_gp_pin_search.srw
forward
global type w_din_gp_pin_search from window
end type
type cb_clear from commandbutton within w_din_gp_pin_search
end type
type cb_search from commandbutton within w_din_gp_pin_search
end type
type dw_din_gp_pin_search_criteria from u_dw_online within w_din_gp_pin_search
end type
type dw_din_gp_pin_search from u_dw_online within w_din_gp_pin_search
end type
type cb_ok from commandbutton within w_din_gp_pin_search
end type
type cb_close from commandbutton within w_din_gp_pin_search
end type
end forward

global type w_din_gp_pin_search from window
integer width = 3259
integer height = 2592
boolean titlebar = true
string title = "Din/GP/Pin Search"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
cb_clear cb_clear
cb_search cb_search
dw_din_gp_pin_search_criteria dw_din_gp_pin_search_criteria
dw_din_gp_pin_search dw_din_gp_pin_search
cb_ok cb_ok
cb_close cb_close
end type
global w_din_gp_pin_search w_din_gp_pin_search

on w_din_gp_pin_search.create
this.cb_clear=create cb_clear
this.cb_search=create cb_search
this.dw_din_gp_pin_search_criteria=create dw_din_gp_pin_search_criteria
this.dw_din_gp_pin_search=create dw_din_gp_pin_search
this.cb_ok=create cb_ok
this.cb_close=create cb_close
this.Control[]={this.cb_clear,&
this.cb_search,&
this.dw_din_gp_pin_search_criteria,&
this.dw_din_gp_pin_search,&
this.cb_ok,&
this.cb_close}
end on

on w_din_gp_pin_search.destroy
destroy(this.cb_clear)
destroy(this.cb_search)
destroy(this.dw_din_gp_pin_search_criteria)
destroy(this.dw_din_gp_pin_search)
destroy(this.cb_ok)
destroy(this.cb_close)
end on

event open;Long    ll_row, ll_num_rows
Integer li_rtn
DataWindowChild ldwc_child

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


SetPointer(HourGlass!)

li_rtn = dw_din_gp_pin_search.SetTransObject(SQLCA)

ll_row = dw_din_gp_pin_search_criteria.InsertRow(0)

li_rtn = dw_din_gp_pin_search_criteria.GetChild("product_type_code", ldwc_child)
li_rtn = ldwc_child.SetTransObject(SQLCA) 
ll_num_rows = ldwc_child.Retrieve()
ll_row = ldwc_child.InsertRow(0)
ldwc_child.SetItem(ll_row, "product_type_code", "")
ldwc_child.SetItem(ll_row, "product_type_desc", "")

ll_num_rows = dw_din_gp_pin_search.Retrieve("N", 0, "N", "", "N", "")

li_rtn = SQLCA.nf_handle_error("w_din_gp_pin_search", "", "open - dw_din_gp_pin_search.Retrieve()")

end event

type cb_clear from commandbutton within w_din_gp_pin_search
integer x = 2505
integer y = 196
integer width = 315
integer height = 96
integer taborder = 30
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Clear"
end type

event clicked;Long ll_row

ll_row = dw_din_gp_pin_search_criteria.GetRow()
IF ll_row > 0 THEN
	dw_din_gp_pin_search_criteria.SetItem(ll_row, "din_gp_pin", 0)
	dw_din_gp_pin_search_criteria.SetItem(ll_row, "din_gp_pin_desc", "")
	dw_din_gp_pin_search_criteria.SetItem(ll_row, "product_type_code", "")
END IF

dw_din_gp_pin_search.Reset()

end event

type cb_search from commandbutton within w_din_gp_pin_search
integer x = 2505
integer y = 32
integer width = 315
integer height = 96
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Search"
boolean default = true
end type

event clicked;Integer li_rtn
Long    ll_din_gp_pin, ll_num_rows
String  ls_din_gp_pin_desc, ls_product_type_code, ls_din_gp_pin_flag, ls_din_gp_pin_desc_flag, ls_product_type_code_flag

SetPointer(HourGlass!)

li_rtn = dw_din_gp_pin_search_criteria.AcceptText()
dw_din_gp_pin_search.Reset()

ll_din_gp_pin = dw_din_gp_pin_search_criteria.GetItemNumber(1, "din_gp_pin")
ls_din_gp_pin_desc = dw_din_gp_pin_search_criteria.GetItemString(1, "din_gp_pin_desc")
ls_product_type_code = dw_din_gp_pin_search_criteria.GetItemString(1, "product_type_code")

IF ll_din_gp_pin = 0 OR IsNull(ll_din_gp_pin) = TRUE THEN
	ls_din_gp_pin_flag = "N"
ELSE
	ls_din_gp_pin_flag = "Y"
END IF

IF ls_din_gp_pin_desc = "" OR IsNull(ls_din_gp_pin_desc) = TRUE THEN
	ls_din_gp_pin_desc_flag = "N"
ELSE
	ls_din_gp_pin_desc_flag = "Y"
	ls_din_gp_pin_desc = "%" + ls_din_gp_pin_desc + "%"
END IF

IF ls_product_type_code = "" OR IsNull(ls_product_type_code) = TRUE THEN
	ls_product_type_code_flag = "N"
ELSE
	ls_product_type_code_flag = "Y"
END IF

ll_num_rows = dw_din_gp_pin_search.Retrieve(ls_din_gp_pin_flag, ll_din_gp_pin, ls_din_gp_pin_desc_flag, &
								ls_din_gp_pin_desc, ls_product_type_code_flag, ls_product_type_code)

li_rtn = SQLCA.nf_handle_error("w_din_gp_pin_search", "", "cb_search - dw_din_gp_pin_search.Retrieve(ls_din_gp_pin_flag, ll_din_gp_pin, ls_din_gp_pin_desc_flag, ls_din_gp_pin_desc, ls_product_type_code_flag, ls_product_type_code)")

IF ll_num_rows = 0 THEN
	MessageBox("No Drugs Found", "Please refine your search criteria and try again.", Information!)
	RETURN
END IF

end event

type dw_din_gp_pin_search_criteria from u_dw_online within w_din_gp_pin_search
integer x = 18
integer y = 16
integer width = 2126
integer height = 324
integer taborder = 10
string title = "none"
string dataobject = "d_din_gp_pin_search_criteria"
boolean border = false
borderstyle borderstyle = stylelowered!
end type

type dw_din_gp_pin_search from u_dw_online within w_din_gp_pin_search
integer x = 18
integer y = 356
integer width = 3182
integer height = 2012
integer taborder = 10
string title = "none"
string dataobject = "d_din_gp_pin_search"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event dberror;SQLCA.SQLDBCode = sqldbcode
SQLCA.SQLErrText = sqlerrtext
RETURN 1
end event

event rowfocuschanged;
IF currentrow = 0 OR IsNull(currentrow) = TRUE OR This.RowCount() = 0 THEN 
	RETURN
END IF

This.SelectRow(0, FALSE)
This.SelectRow(currentrow, TRUE)

end event

type cb_ok from commandbutton within w_din_gp_pin_search
integer x = 1207
integer y = 2396
integer width = 347
integer height = 96
integer taborder = 10
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
end type

event clicked;Long ll_row, ll_din_gp_pin

ll_row = dw_din_gp_pin_search.GetRow()
IF ll_row = 0 OR IsNull(ll_row) = TRUE THEN
	Messagebox("No Row Selected", "Select a drug and try again.")
	RETURN
END IF

ll_din_gp_pin = dw_din_gp_pin_search.GetItemNumber(ll_row, "din_gp_pin")

CloseWithReturn(Parent, ll_din_gp_pin)

end event

type cb_close from commandbutton within w_din_gp_pin_search
integer x = 1664
integer y = 2392
integer width = 347
integer height = 96
integer taborder = 10
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Close"
end type

event clicked;CloseWithReturn(Parent, 0)

end event

