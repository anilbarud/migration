$PBExportHeader$w_filter_event_type_list.srw
$PBExportComments$Window to allow user to filter the event list
forward
global type w_filter_event_type_list from window
end type
type st_9 from statictext within w_filter_event_type_list
end type
type st_7 from statictext within w_filter_event_type_list
end type
type dw_event_specific_type from u_dw_online within w_filter_event_type_list
end type
type dw_event_type from u_dw_online within w_filter_event_type_list
end type
type gb_2 from groupbox within w_filter_event_type_list
end type
type st_2 from statictext within w_filter_event_type_list
end type
type st_6 from statictext within w_filter_event_type_list
end type
type st_5 from statictext within w_filter_event_type_list
end type
type st_4 from statictext within w_filter_event_type_list
end type
type st_3 from statictext within w_filter_event_type_list
end type
type st_1 from statictext within w_filter_event_type_list
end type
type cb_clear from commandbutton within w_filter_event_type_list
end type
type dw_filter_dates from u_dw_online within w_filter_event_type_list
end type
type cb_cancel from commandbutton within w_filter_event_type_list
end type
type cb_ok from commandbutton within w_filter_event_type_list
end type
type gb_select_payments from groupbox within w_filter_event_type_list
end type
type gb_1 from groupbox within w_filter_event_type_list
end type
type dw_event_type_multiple from u_dw_online within w_filter_event_type_list
end type
type ln_1 from line within w_filter_event_type_list
end type
type ln_2 from line within w_filter_event_type_list
end type
end forward

global type w_filter_event_type_list from window
integer x = 1335
integer y = 688
integer width = 2542
integer height = 2432
boolean titlebar = true
string title = "Filter Event Type List"
windowtype windowtype = response!
long backcolor = 67108864
st_9 st_9
st_7 st_7
dw_event_specific_type dw_event_specific_type
dw_event_type dw_event_type
gb_2 gb_2
st_2 st_2
st_6 st_6
st_5 st_5
st_4 st_4
st_3 st_3
st_1 st_1
cb_clear cb_clear
dw_filter_dates dw_filter_dates
cb_cancel cb_cancel
cb_ok cb_ok
gb_select_payments gb_select_payments
gb_1 gb_1
dw_event_type_multiple dw_event_type_multiple
ln_1 ln_1
ln_2 ln_2
end type
global w_filter_event_type_list w_filter_event_type_list

type prototypes

end prototypes

type variables
DataWindowChild	 vidwc_event_specific_type

end variables

event open;LONG	vll_return

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')

dw_event_type.SetTransObject(SQLCA)
dw_event_specific_type.SetTransObject(SQLCA)

dw_event_type_multiple.SetTransObject(SQLCA)

dw_filter_dates.InsertRow(0)
dw_event_type.InsertRow(0) 
dw_event_specific_type.InsertRow(0)

dw_event_type_multiple.InsertRow(0) 

dw_event_type_multiple.uf_setSelect(3)
dw_event_type_multiple.retrieve()
cb_clear.triggerevent(Clicked!)


IF dw_event_specific_type.GetChild("event_specific_code",vidwc_event_specific_type) < 0 THEN
	MessageBox("Filter List","Unable to obtain pointer to event specific type~r~nPlease call the help desk",Exclamation!)
	cb_cancel.TriggerEvent(Clicked!)
	Return
END IF

dw_event_specific_type.Modify("event_specific_code.Protect=1")





end event

on w_filter_event_type_list.create
this.st_9=create st_9
this.st_7=create st_7
this.dw_event_specific_type=create dw_event_specific_type
this.dw_event_type=create dw_event_type
this.gb_2=create gb_2
this.st_2=create st_2
this.st_6=create st_6
this.st_5=create st_5
this.st_4=create st_4
this.st_3=create st_3
this.st_1=create st_1
this.cb_clear=create cb_clear
this.dw_filter_dates=create dw_filter_dates
this.cb_cancel=create cb_cancel
this.cb_ok=create cb_ok
this.gb_select_payments=create gb_select_payments
this.gb_1=create gb_1
this.dw_event_type_multiple=create dw_event_type_multiple
this.ln_1=create ln_1
this.ln_2=create ln_2
this.Control[]={this.st_9,&
this.st_7,&
this.dw_event_specific_type,&
this.dw_event_type,&
this.gb_2,&
this.st_2,&
this.st_6,&
this.st_5,&
this.st_4,&
this.st_3,&
this.st_1,&
this.cb_clear,&
this.dw_filter_dates,&
this.cb_cancel,&
this.cb_ok,&
this.gb_select_payments,&
this.gb_1,&
this.dw_event_type_multiple,&
this.ln_1,&
this.ln_2}
end on

on w_filter_event_type_list.destroy
destroy(this.st_9)
destroy(this.st_7)
destroy(this.dw_event_specific_type)
destroy(this.dw_event_type)
destroy(this.gb_2)
destroy(this.st_2)
destroy(this.st_6)
destroy(this.st_5)
destroy(this.st_4)
destroy(this.st_3)
destroy(this.st_1)
destroy(this.cb_clear)
destroy(this.dw_filter_dates)
destroy(this.cb_cancel)
destroy(this.cb_ok)
destroy(this.gb_select_payments)
destroy(this.gb_1)
destroy(this.dw_event_type_multiple)
destroy(this.ln_1)
destroy(this.ln_2)
end on

type st_9 from statictext within w_filter_event_type_list
integer x = 1285
integer y = 268
integer width = 1184
integer height = 92
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
boolean enabled = false
string text = "Select one or multiple Event Type codes"
boolean focusrectangle = false
end type

type st_7 from statictext within w_filter_event_type_list
integer x = 1074
integer y = 244
integer width = 192
integer height = 132
integer textsize = -16
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial Black"
long textcolor = 33554432
long backcolor = 67108864
string text = "OR"
boolean focusrectangle = false
end type

type dw_event_specific_type from u_dw_online within w_filter_event_type_list
integer x = 50
integer y = 508
integer width = 974
integer height = 104
integer taborder = 30
string dataobject = "d_event_specific_type"
boolean border = false
end type

type dw_event_type from u_dw_online within w_filter_event_type_list
integer x = 50
integer y = 420
integer width = 978
integer height = 100
integer taborder = 20
string dataobject = "d_event_type"
boolean border = false
end type

event itemchanged;call super::itemchanged;	//rb_all_events.Checked = False
	
	dw_event_specific_type.reset()
	dw_event_specific_type.insertRow(0)
	
	vidwc_event_specific_type.SetFilter("event_type_code = '" + GetText() + "'")
	vidwc_event_specific_type.Filter()
	


	IF vidwc_event_specific_type.RowCount() > 0 THEN
		dw_event_specific_type.Modify("event_specific_code.Protect=0")
	ELSE
		dw_event_specific_type.Modify("event_specific_code.Protect=1")
		dw_event_specific_type.SetItem(1,"event_specific_code","")
	END IF

end event

type gb_2 from groupbox within w_filter_event_type_list
integer x = 27
integer y = 280
integer width = 1019
integer height = 424
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Select Event/Specific combination"
end type

type st_2 from statictext within w_filter_event_type_list
integer x = 1426
integer y = 460
integer width = 78
integer height = 64
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "or"
boolean focusrectangle = false
end type

type st_6 from statictext within w_filter_event_type_list
integer x = 1623
integer y = 460
integer width = 731
integer height = 56
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "key + click for a group select"
boolean focusrectangle = false
end type

type st_5 from statictext within w_filter_event_type_list
integer x = 1499
integer y = 460
integer width = 151
integer height = 56
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 255
long backcolor = 553648127
string text = "Shift"
boolean focusrectangle = false
end type

type st_4 from statictext within w_filter_event_type_list
integer x = 1609
integer y = 404
integer width = 759
integer height = 56
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "key + click for multiple select,"
boolean focusrectangle = false
end type

type st_3 from statictext within w_filter_event_type_list
integer x = 1509
integer y = 404
integer width = 114
integer height = 56
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 255
long backcolor = 553648127
string text = "Ctrl"
boolean focusrectangle = false
end type

type st_1 from statictext within w_filter_event_type_list
integer x = 1403
integer y = 404
integer width = 110
integer height = 56
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Use"
boolean focusrectangle = false
end type

type cb_clear from commandbutton within w_filter_event_type_list
integer x = 1765
integer y = 88
integer width = 494
integer height = 80
integer taborder = 50
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Clear Selection(s)"
end type

event clicked;date ldt_null_date
setnull(ldt_null_date)

dw_event_type_multiple.SelectRow(0,False)

dw_event_type.reset()
dw_event_type.insertRow(0)

dw_event_specific_type.reset()
dw_event_specific_type.insertRow(0)

dw_filter_dates.setitem(1,'paid_from', ldt_null_date)
dw_filter_dates.setitem(1,'paid_to',ldt_null_date)
end event

type dw_filter_dates from u_dw_online within w_filter_event_type_list
integer x = 361
integer y = 108
integer width = 1093
integer height = 100
integer taborder = 10
string dataobject = "d_filter_dates"
boolean border = false
end type

type cb_cancel from commandbutton within w_filter_event_type_list
integer x = 1207
integer y = 2240
integer width = 379
integer height = 96
integer taborder = 70
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cance&l"
end type

event clicked;CloseWithReturn(w_filter_event_type_list,"Cancel")
end event

type cb_ok from commandbutton within w_filter_event_type_list
integer x = 750
integer y = 2240
integer width = 379
integer height = 96
integer taborder = 60
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "OK"
end type

event clicked;DATETIME vlt_from, vlt_to, vlt_null
STRING	    vls_event_date_filter, vls_event_type_filter, vls_event_type_code, vls_event_specific_code, vls_return_filter
LONG        ll_selected_row

	SetNull(vlt_null)

	IF dw_filter_dates.AcceptText() < 0 THEN
		Return
	END IF

	IF dw_event_type.AcceptText() < 0 THEN
		Return
	END IF


	vlt_from = dw_filter_dates.GetItemDateTime(1,"paid_from")   // uses a daatawindow object used for transaction dates, thats why the name paid_from and not event_date, but its only used secure a date range to filter the events
	vlt_to   = dw_filter_dates.GetItemDateTime(1,"paid_to")

/*		Note: We have to be careful with the dates as processed_date contains the time
*/
	IF (IsNull(vlt_from) or string(vlt_from) = "0000 01 01 00:00:00") and &
		(IsNull(vlt_to)   or string(vlt_to)   = "0000 01 01 00:00:00") THEN
		vls_event_date_filter = ""

	ELSEIF IsNull(vlt_from) or string(vlt_from) = "0000 01 01 00:00:00" THEN
		vlt_to = DateTime(RelativeDate(Date(vlt_to),1))
		vls_event_date_filter = "(event_date < " + string(vlt_to,'yyyy-mm-dd') + ")"

	ELSEIF IsNull(vlt_to)   or string(vlt_to) = "0000 01 01 00:00:00"  THEN
		vls_event_date_filter = "(event_date >= " + string(vlt_from,'yyyy-mm-dd') + ")"
	ELSE
		IF vlt_from > vlt_to THEN
			MessageBox("Event Log - Validation Error","The from date cannot be after the to date",Exclamation!)
			Return
		END IF
		vlt_to = DateTime(RelativeDate(Date(vlt_to),1))
		vls_event_date_filter = "(event_date >= " + string(vlt_from,'yyyy-mm-dd') + " and event_date < " + string(vlt_to,'yyyy-mm-dd') + ")"
	END IF


/*	Get the list of selected event types
*/

vls_event_type_code = dw_event_type.getItemString(1,"event_type_code")

IF dw_event_type_multiple.getSelectedRow(0) > 0 and vls_event_type_code > "" THEN
	MESSAGEBOX("invalid Selection", "You have made a selection from both the single event type selection and the multiple type selection boxes."&
	                                              + "~r~n~r~nYou must select from only one or the other, not both. Please 'Clear Selection' and try again.",INFORMATION!) 
	RETURN
END IF

IF vls_event_type_code > "" THEN
	vls_event_type_filter = "event_type_code = '" + vls_event_type_code   + "'"
	
	// now check for a specific code , may or may not be present
	vls_event_specific_code = dw_event_specific_type.getItemString(1,"event_specific_code")
	
	IF vls_event_specific_code > "" THEN
		vls_event_type_filter = vls_event_type_filter + " and event_specific_code = '" + vls_event_specific_code + "'"
	END IF
ELSE	
	// if nothing in the left hand event type  - specific type dropdown, then check for an entry(s) in the multiple event type selection datawindow
	ll_selected_row = dw_event_type_multiple.getSelectedRow(0)
	do while ll_selected_row > 0
		IF vls_event_type_filter = '' THEN
			vls_event_type_filter = "event_type_code = '" + dw_event_type_multiple.getItemString(ll_selected_row,'event_type_code') + "'"
		ELSE
			vls_event_type_filter = vls_event_type_filter + " or event_type_code = '" + dw_event_type_multiple.getItemString(ll_selected_row,'event_type_code') + "'"
		END IF
		 ll_selected_row = dw_event_type_multiple.getSelectedRow(ll_selected_row )
	LOOP
END IF
	
IF vls_event_type_filter = "" and vls_event_date_filter = "" THEN
	vls_return_filter = ""
ELSEIF vls_event_type_filter = "" and vls_event_date_filter > "" THEN
	vls_return_filter = vls_event_date_filter
ELSEIF vls_event_type_filter > "" and vls_event_date_filter = "" THEN
	vls_return_filter = vls_event_type_filter 
ELSEIF vls_event_type_filter > "" and vls_event_date_filter > "" THEN
	vls_return_filter = vls_event_type_filter + ' and ' +  vls_event_date_filter
END IF
	

	CloseWithReturn(w_filter_event_type_list, vls_return_filter)
	
	
	
	
	
end event

type gb_select_payments from groupbox within w_filter_event_type_list
integer x = 302
integer y = 20
integer width = 1143
integer height = 200
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Select Event Dates (or none for all dates)"
end type

type gb_1 from groupbox within w_filter_event_type_list
integer x = 1358
integer y = 544
integer width = 1074
integer height = 1668
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Event Type"
end type

type dw_event_type_multiple from u_dw_online within w_filter_event_type_list
integer x = 1371
integer y = 620
integer width = 1042
integer height = 1580
integer taborder = 40
boolean bringtotop = true
string dataobject = "d_event_type_multiple"
boolean vscrollbar = true
boolean border = false
end type

event keydown;call super::keydown;
string ls_key_pressed, ls_first_letter
long  ll_row, ll_first_selected_row
 int li_ctrl

CHOOSE CASE KEY
	case keya!, keyA!
		ls_key_pressed = 'A'
	case keyb!, keyB!
		ls_key_pressed = 'B'
	case keyc!, keyC!
		ls_key_pressed = 'C'
	case keyd!, keyD!
		ls_key_pressed = 'D'
	case keye!, keyE!
		ls_key_pressed = 'E'
	case keyf!, keyF!
		ls_key_pressed = 'F'
	case keyg!, keyG!
		ls_key_pressed = 'G'
	case keyh!, keyH!
		ls_key_pressed = 'H'
	case keyi!, keyI!
		ls_key_pressed = 'I'
	case keyj!, keyJ!
		ls_key_pressed = 'J'
	case keyk!, keyK!
		ls_key_pressed = 'K'
	case keyl!, keyL!
		ls_key_pressed = 'L'
		// kill the shortcut key, or the window will close
		cb_cancel.text ="Cancel"
	case keym!, keyM!
		ls_key_pressed = 'M'
	case keyn!, keyN!
		ls_key_pressed = 'N'
	case keyo!, keyO!
		ls_key_pressed = 'O'
		// kill the shortcut key, or the window will close
		cb_ok.text = "OK"
	case keyp!, keyP!
		ls_key_pressed = 'P'
	case keyq!, keyQ!
		ls_key_pressed = 'Q'
	case keyr!, keyR!
		ls_key_pressed = 'R'
	case keys!, keyS!
		ls_key_pressed = 'S'
	case keyt!, keyT!
		ls_key_pressed = 'T'
	case keyu!, keyU!
		ls_key_pressed = 'U'
	case keyv!, keyV!
		ls_key_pressed = 'V'
	case keyw!, keyW!
		ls_key_pressed = 'W'
	case keyx!, keyX!
		ls_key_pressed = 'X'
	case keyy!, keyY!
		ls_key_pressed = 'Y'
	case keyz!, keyZ!
		ls_key_pressed = 'Z'
END CHOOSE

int li_char, li_char2
IF ls_key_pressed > '' THEN
	// get the first selected row and start from there, so that multiple key presses on the same letter will advance to the next item in the list with that starting letterr
	il_anchor_row = getSelectedRow(0)
	ls_first_letter = left(this.getItemString(il_anchor_row, "event_type_desc"), 1)
	
	// but first, see if the pressed letter is less than the first letter of any selected row, if so, reset the anchor row and continue (at beginning of loop), otherwise it won't find it
	IF ASC(ls_key_pressed) < ASC(ls_first_letter)  then il_anchor_row = 0
	
	FOR li_ctrl = il_anchor_row + 1 to this.rowcount()
		ls_first_letter = left(this.getItemString(li_ctrl, "event_type_desc"), 1)	
		IF MATCH(ls_first_letter, ls_key_pressed) THEN
			this.scrolltorow(li_ctrl)
			this.setRow(li_ctrl)	
			this.selectrow(li_ctrl, true)
			EXIT
		END IF
	NEXT
END IF


end event

type ln_1 from line within w_filter_event_type_list
long linecolor = 33554432
integer linethickness = 12
integer beginx = 1143
integer beginy = 388
integer endx = 1143
integer endy = 2200
end type

type ln_2 from line within w_filter_event_type_list
long linecolor = 33554432
integer linethickness = 12
integer beginx = 1198
integer beginy = 388
integer endx = 1198
integer endy = 2200
end type

