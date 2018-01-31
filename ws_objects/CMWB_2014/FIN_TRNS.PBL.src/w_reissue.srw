$PBExportHeader$w_reissue.srw
forward
global type w_reissue from w_a_tool
end type
type dw_reissue from u_datawindow within w_reissue
end type
type rb_cheque from radiobutton within w_reissue
end type
type rb_dd from radiobutton within w_reissue
end type
type cb_search from commandbutton within w_reissue
end type
type cb_clear from commandbutton within w_reissue
end type
type st_no from statictext within w_reissue
end type
type em_no from editmask within w_reissue
end type
type em_recipient_no from editmask within w_reissue
end type
type st_recipient_no from statictext within w_reissue
end type
type cb_reissue from commandbutton within w_reissue
end type
type gb_2 from groupbox within w_reissue
end type
type st_recipient_type from statictext within w_reissue
end type
type ddlb_recipient_type from dropdownlistbox within w_reissue
end type
type cbx_selall from checkbox within w_reissue
end type
type em_date from editmask within w_reissue
end type
type st_1 from statictext within w_reissue
end type
type dw_replace from datawindow within w_reissue
end type
type dw_daily_chq from datawindow within w_reissue
end type
type dw_daily_dd from datawindow within w_reissue
end type
type gb_1 from groupbox within w_reissue
end type
end forward

global type w_reissue from w_a_tool
dw_reissue dw_reissue
rb_cheque rb_cheque
rb_dd rb_dd
cb_search cb_search
cb_clear cb_clear
st_no st_no
em_no em_no
em_recipient_no em_recipient_no
st_recipient_no st_recipient_no
cb_reissue cb_reissue
gb_2 gb_2
st_recipient_type st_recipient_type
ddlb_recipient_type ddlb_recipient_type
cbx_selall cbx_selall
em_date em_date
st_1 st_1
dw_replace dw_replace
dw_daily_chq dw_daily_chq
dw_daily_dd dw_daily_dd
gb_1 gb_1
end type
global w_reissue w_reissue

type variables
STRING is_original_chq_sql, is_original_deposit_sql

n_reissue inv_reissue
BOOLEAN ib_cheque
end variables

on w_reissue.create
int iCurrent
call super::create
this.dw_reissue=create dw_reissue
this.rb_cheque=create rb_cheque
this.rb_dd=create rb_dd
this.cb_search=create cb_search
this.cb_clear=create cb_clear
this.st_no=create st_no
this.em_no=create em_no
this.em_recipient_no=create em_recipient_no
this.st_recipient_no=create st_recipient_no
this.cb_reissue=create cb_reissue
this.gb_2=create gb_2
this.st_recipient_type=create st_recipient_type
this.ddlb_recipient_type=create ddlb_recipient_type
this.cbx_selall=create cbx_selall
this.em_date=create em_date
this.st_1=create st_1
this.dw_replace=create dw_replace
this.dw_daily_chq=create dw_daily_chq
this.dw_daily_dd=create dw_daily_dd
this.gb_1=create gb_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_reissue
this.Control[iCurrent+2]=this.rb_cheque
this.Control[iCurrent+3]=this.rb_dd
this.Control[iCurrent+4]=this.cb_search
this.Control[iCurrent+5]=this.cb_clear
this.Control[iCurrent+6]=this.st_no
this.Control[iCurrent+7]=this.em_no
this.Control[iCurrent+8]=this.em_recipient_no
this.Control[iCurrent+9]=this.st_recipient_no
this.Control[iCurrent+10]=this.cb_reissue
this.Control[iCurrent+11]=this.gb_2
this.Control[iCurrent+12]=this.st_recipient_type
this.Control[iCurrent+13]=this.ddlb_recipient_type
this.Control[iCurrent+14]=this.cbx_selall
this.Control[iCurrent+15]=this.em_date
this.Control[iCurrent+16]=this.st_1
this.Control[iCurrent+17]=this.dw_replace
this.Control[iCurrent+18]=this.dw_daily_chq
this.Control[iCurrent+19]=this.dw_daily_dd
this.Control[iCurrent+20]=this.gb_1
end on

on w_reissue.destroy
call super::destroy
destroy(this.dw_reissue)
destroy(this.rb_cheque)
destroy(this.rb_dd)
destroy(this.cb_search)
destroy(this.cb_clear)
destroy(this.st_no)
destroy(this.em_no)
destroy(this.em_recipient_no)
destroy(this.st_recipient_no)
destroy(this.cb_reissue)
destroy(this.gb_2)
destroy(this.st_recipient_type)
destroy(this.ddlb_recipient_type)
destroy(this.cbx_selall)
destroy(this.em_date)
destroy(this.st_1)
destroy(this.dw_replace)
destroy(this.dw_daily_chq)
destroy(this.dw_daily_dd)
destroy(this.gb_1)
end on

event open;call super::open;LONG ll_rows, ll_cntr
DATAWINDOW ldw_dw[]
DATASTORE lds_recipient_types

inv_reissue = CREATE n_reissue
inv_reissue.nf_set_window_parent(THIS)

lds_recipient_types = CREATE DATASTORE
lds_recipient_types.DataObject = 'd_recipient_types_all'
lds_recipient_types.SetTransObject(SQLCA)

dw_reissue.DataObject = 'd_chq_detail'
dw_reissue.SetTransObject(SQLCA)
is_original_chq_sql = dw_reissue.Describe("DataWindow.Table.Select")

ll_rows = lds_recipient_types.Retrieve()

st_no.Text = 'Cheque No.: '
ib_cheque = TRUE
ddlb_recipient_type.AddItem('')

FOR ll_cntr = 1 to ll_rows
	ddlb_recipient_type.AddItem(lds_recipient_types.GetItemString(ll_cntr, 'recipient_type_desc'))
NEXT

ldw_dw[1] = dw_reissue
ldw_dw[2] = dw_replace
ldw_dw[3] = dw_daily_chq
ldw_dw[4] = dw_daily_dd

inv_reissue.nf_set_datawindow(ldw_dw[], SQLCA)

IF IsNull(inv_resize) OR NOT IsValid (inv_resize) THEN
	inv_resize = create n_resize
	inv_resize.of_SetOrigSize (3163,1792)
END IF

THIS.inv_resize.of_register(dw_reissue,0,0,100,100)
THIS.inv_resize.of_register(cb_reissue, 'FixedToBottom')
THIS.inv_resize.of_register(cb_close,'FixedToBottom')

end event

type st_title from w_a_tool`st_title within w_reissue
string text = "Re-Issue Cheque/Direct Deposit"
end type

type cb_close from w_a_tool`cb_close within w_reissue
integer y = 1664
integer taborder = 110
end type

type dw_reissue from u_datawindow within w_reissue
integer x = 14
integer y = 408
integer width = 3113
integer height = 1236
integer taborder = 30
boolean bringtotop = true
string dataobject = "d_chq_detail"
boolean vscrollbar = true
boolean livescroll = false
end type

event clicked;call super::clicked;STRING obj_name, ls_column_name, ls_recipient_type, ls_recipient_type2
LONG ll_tab, ll_row, ll_recipient_no, ll_xmit_no, ll_cntr, ll_xmit_no2, ll_recipient_no2, ll_chq_no, ll_chq_no2

THIS.SetRedraw(FALSE)

obj_name = THIS.GetObjectAtPointer()
ll_tab = POS(obj_name, "~t", 1)
ls_column_name = MID(obj_name, 1, ll_tab - 1)
ll_row = LONG(MID(obj_name, ll_tab + 1))

IF rb_cheque.Checked = TRUE THEN
	IF ls_column_name = 'checkbox_group' THEN
		IF THIS.IsSelected(ll_row) THEN
			THIS.SelectRow(ll_row, FALSE)
			THIS.SetItem(ll_row, 'checkbox_group',0)
			ll_chq_no = THIS.GetItemNumber(ll_row, 'cheque_no')
			ll_recipient_no = THIS.GetItemNumber(ll_row, 'recipient_no')
			ls_recipient_type = THIS.GetItemString(ll_row,'recipient_type_code')
			FOR ll_cntr = 1 to THIS.RowCount()
				ll_chq_no2 = THIS.GetItemNumber(ll_cntr, 'cheque_no')
				ll_recipient_no2 = THIS.GetItemNumber(ll_cntr, 'recipient_no')
				ls_recipient_type2 = THIS.GetItemString(ll_cntr,'recipient_type_code')
				IF ll_chq_no = ll_chq_no2 AND ll_recipient_no = ll_recipient_no2 AND ls_recipient_type = ls_recipient_type2 THEN
					THIS.SelectRow(ll_cntr, FALSE)
					THIS.SetItem(ll_cntr, 'checkbox_group', 0)
				END IF			
			NEXT
		ELSE
			THIS.SelectRow(ll_row, TRUE)
			THIS.SetItem(ll_row, 'checkbox_group',1)				
			ll_chq_no = THIS.GetItemNumber(ll_row, 'cheque_no')
			ll_recipient_no = THIS.GetItemNumber(ll_row, 'recipient_no')
			ls_recipient_type = THIS.GetItemString(ll_row,'recipient_type_code')
			FOR ll_cntr = 1 to THIS.RowCount()
				ll_chq_no2 = THIS.GetItemNumber(ll_cntr, 'cheque_no')
				ll_recipient_no2 = THIS.GetItemNumber(ll_cntr, 'recipient_no')
				ls_recipient_type2 = THIS.GetItemString(ll_cntr,'recipient_type_code')
				IF ll_chq_no = ll_chq_no2 AND ll_recipient_no = ll_recipient_no2 AND ls_recipient_type = ls_recipient_type2 THEN
					THIS.SelectRow(ll_cntr, TRUE)
					THIS.SetItem(ll_cntr, 'checkbox_group', 1)
				END IF			
			NEXT
		END IF
	END IF
ELSE
	IF ls_column_name = 'checkbox_group' THEN
		IF THIS.IsSelected(ll_row) THEN
			THIS.SelectRow(ll_row, FALSE)
			THIS.SetItem(ll_row, 'checkbox_group',0)
			ll_xmit_no = THIS.GetItemNumber(ll_row, 'direct_deposit_xmit_no')
			ll_recipient_no = THIS.GetItemNumber(ll_row, 'recipient_no')
			ls_recipient_type = THIS.GetItemString(ll_row,'recipient_type_code')
			FOR ll_cntr = 1 to THIS.RowCount()
				ll_xmit_no2 = THIS.GetItemNumber(ll_cntr, 'direct_deposit_xmit_no')
				ll_recipient_no2 = THIS.GetItemNumber(ll_cntr, 'recipient_no')
				ls_recipient_type2 = THIS.GetItemString(ll_cntr,'recipient_type_code')
				IF ll_xmit_no = ll_xmit_no2 AND ll_recipient_no = ll_recipient_no2 AND ls_recipient_type = ls_recipient_type2 THEN
					THIS.SelectRow(ll_cntr, FALSE)
					THIS.SetItem(ll_cntr, 'checkbox_group', 0)
				END IF			
			NEXT
		ELSE
			THIS.SelectRow(ll_row, TRUE)
			THIS.SetItem(ll_row, 'checkbox_group',1)				
			ll_xmit_no = THIS.GetItemNumber(ll_row, 'direct_deposit_xmit_no')
			ll_recipient_no = THIS.GetItemNumber(ll_row, 'recipient_no')
			ls_recipient_type = THIS.GetItemString(ll_row,'recipient_type_code')
			FOR ll_cntr = 1 to THIS.RowCount()
				ll_xmit_no2 = THIS.GetItemNumber(ll_cntr, 'direct_deposit_xmit_no')
				ll_recipient_no2 = THIS.GetItemNumber(ll_cntr, 'recipient_no')
				ls_recipient_type2 = THIS.GetItemString(ll_cntr,'recipient_type_code')
				IF ll_xmit_no = ll_xmit_no2 AND ll_recipient_no = ll_recipient_no2 AND ls_recipient_type = ls_recipient_type2 THEN
					THIS.SelectRow(ll_cntr, TRUE)
					THIS.SetItem(ll_cntr, 'checkbox_group', 1)
				END IF			
			NEXT
		END IF
	END IF
END IF

THIS.SetRedraw(TRUE)
end event

type rb_cheque from radiobutton within w_reissue
integer x = 91
integer y = 164
integer width = 288
integer height = 76
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
string text = "Cheque"
boolean checked = true
end type

event clicked;IF ib_cheque = FALSE THEN
	st_no.Text = 'Cheque No.: '
	ib_cheque = TRUE
	
	em_no.Text = ''
	em_recipient_no.Text = ''
	em_date.Text = ''
	ddlb_recipient_type.SelectItem(1)
	cbx_selall.Checked = FALSE
	
	dw_reissue.DataObject = 'd_chq_detail'
	dw_reissue.SetTransObject(SQLCA)
	is_original_chq_sql = dw_reissue.Describe("DataWindow.Table.Select")
END IF

end event

type rb_dd from radiobutton within w_reissue
integer x = 91
integer y = 252
integer width = 439
integer height = 76
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
string text = "Direct Deposit"
end type

event clicked;IF ib_cheque = TRUE THEN
	st_no.Text = 'Deposit No.:'
	ib_cheque = FALSE
	
	em_no.Text = ''
	em_recipient_no.Text = ''
	em_date.Text = ''
	ddlb_recipient_type.SelectItem(1)
	cbx_selall.Checked = FALSE
	
	dw_reissue.DataObject = 'd_direct_deposit_detail'
	dw_reissue.SetTransObject(SQLCA)
	is_original_deposit_sql = dw_reissue.Describe("DataWindow.Table.Select")
END IF

end event

type cb_search from commandbutton within w_reissue
integer x = 2706
integer y = 152
integer width = 352
integer height = 100
integer taborder = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Search"
boolean default = true
end type

event clicked;DATE ldt_deposit_date
LONG ll_chq_deposit_no, ll_recipient, ll_rows, ll_rtn, ll_pos, ll_reconciled
STRING ls_recipient_type, ls_modstring, ls_recipient_type_code, ls_sql, ls_rtn, ls_method

dw_reissue.Reset()
cbx_selall.Checked = FALSE

IF rb_cheque.Checked THEN
	dw_reissue.Modify(is_original_chq_sql)
	ls_modstring = is_original_chq_sql
	ls_method = 'A'
ELSE
	dw_reissue.Modify(is_original_deposit_sql)
	ls_modstring = is_original_deposit_sql
	ls_method = 'D'
END IF

ll_chq_deposit_no = LONG(TRIM(em_no.Text))
ll_recipient = LONG(TRIM(em_recipient_no.Text))
ls_recipient_type = TRIM(ddlb_recipient_type.Text)
ldt_deposit_date = DATE(em_date.Text)

IF (IsNull(ll_chq_deposit_no) OR ll_chq_deposit_no = 0)  AND (IsNull(ldt_deposit_date) OR ldt_deposit_date = DATE('1900-01-01')) THEN
	MessageBox('Search Criteria','You must enter the Cheque/Deposit Number OR Deposit Date before doing a search.', Information!)
	RETURN
ELSE
	IF ls_method = 'A' THEN
		ll_reconciled = inv_reissue.nf_is_reconciled(ll_chq_deposit_no) 
		IF ll_reconciled < 0 THEN
			MessageBox('Reconciled Cheque','The cheque you have entered has been reconciled. It cannot be re-issued.', Information!)
			RETURN
		END IF
	END IF
END IF

IF NOT (IsNull(ll_recipient) OR ll_recipient = 0) THEN
	ls_modstring = ls_modstring +  " AND a.recipient_no = " + STRING(ll_recipient)
END IF

IF NOT (IsNull(ls_recipient_type) OR ls_recipient_type = '') THEN
	SELECT recipient_type_code
	INTO       :ls_recipient_type_code
	FROM     Recipient_Type
	WHERE  recipient_type_desc = :ls_recipient_type
	USING    SQLCA;
	
	SQLCA.nf_handle_error("w_reissue","cb_search","SELECT recipient_type_code FROM Recipient_Type")
	
	IF ls_recipient_type_code > '' THEN
		ls_modstring = ls_modstring + " AND a.recipient_type_code = '" + ls_recipient_type_code + "'"
	END IF
END IF

IF NOT (IsNull(ll_chq_deposit_no) OR ll_chq_deposit_no = 0) THEN
	IF rb_cheque.Checked THEN
		ls_modstring = ls_modstring + " AND cheque_no = " + STRING(ll_chq_deposit_no) 
	ELSE
		ls_modstring = ls_modstring + " AND a.direct_deposit_xmit_no = " + STRING(ll_chq_deposit_no)
	END IF
END IF

IF NOT (IsNull(ldt_deposit_date) OR ldt_deposit_date = DATE('1900-01-01'))  THEN
	ls_modstring = ls_modstring + " AND (cheque_deposit_date >= '" + STRING(ldt_deposit_date) + "' AND cheque_deposit_date < '" + STRING(RelativeDate(ldt_deposit_date, 1)) + "')  "
END IF

IF ls_modstring > '' THEN
	
	ls_sql = 'DataWindow.Table.Select="' + ls_modstring + '"'

	ls_rtn = dw_reissue.Modify(ls_sql )
	IF IsNull(ls_rtn) OR ls_rtn >  '' THEN
		MessageBox('Syntax Error', 'There was a problem with the Search Criteria entered. Please Clear and try again.', Information!)
		RETURN
	END IF	
END IF

ll_rows = dw_reissue.Retrieve(ls_method)

IF ll_rows = 0 THEN
	MessageBox('No Records Returned','There are no Cheques/Deposits that meet the criteria you have entered. Please try again.', Information!)
ELSE
	cb_reissue.Enabled = TRUE
END IF


end event

type cb_clear from commandbutton within w_reissue
integer x = 2706
integer y = 252
integer width = 352
integer height = 100
integer taborder = 80
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Clear"
end type

event clicked;dw_reissue.Reset()

em_no.Text = ''
em_recipient_no.Text = ''
em_date.Text = ''
ddlb_recipient_type.SelectItem(1)

cb_reissue.Enabled = FALSE
cbx_selall.Checked = FALSE
end event

type st_no from statictext within w_reissue
integer x = 681
integer y = 172
integer width = 421
integer height = 56
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
string text = "Cheque No.: "
boolean focusrectangle = false
end type

type em_no from editmask within w_reissue
integer x = 1056
integer y = 160
integer width = 366
integer height = 80
integer taborder = 10
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

type em_recipient_no from editmask within w_reissue
integer x = 1056
integer y = 252
integer width = 366
integer height = 80
integer taborder = 40
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

type st_recipient_no from statictext within w_reissue
integer x = 681
integer y = 264
integer width = 352
integer height = 56
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
string text = "Recipient No.: "
boolean focusrectangle = false
end type

type cb_reissue from commandbutton within w_reissue
integer x = 2391
integer y = 1664
integer width = 357
integer height = 100
integer taborder = 100
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Re-Issue"
end type

event clicked;STRING ls_type
INTEGER li_rtn

N_PROCESS_RUN_STATUS ln_process_run_status

/******************************************************************************************
- function call to prevent updating of tables used by PRODSVCS Payment Processing
- new object N_PROCESS_RUN_STATUS is used to determine status of processes (in progress = Y/N)
- '002' refers to the Account Payment Maintenance module, '044' refers to the Payment Processing module
******************************************************************************************/
ln_process_run_status = Create N_PROCESS_RUN_STATUS

li_rtn = ln_process_run_status.nf_in_progress('056','044','processing',SQLCA)

IF li_rtn = 1 THEN
	// module is blocked
	return
END IF
/******************************************************************************************/

IF rb_cheque.Checked THEN
	ls_type = 'Cheque'
ELSE
	ls_type = 'Direct Deposit'
END IF

IF MessageBox('Confirm Re-Issue','You are about to Re-Issue a Cheque/Direct Deposit.  Are you sure you would like to continue?', Question!, YesNo!) = 2 THEN
	RETURN
END IF

SetPointer(Hourglass!)

IF inv_reissue.nf_reissue(ls_type) < 0 THEN
	RETURN -1 
END IF


SQLCA.nf_begin_transaction()

IF inv_reissue.nf_save() < 0 THEN
	SQLCA.nf_rollback_transaction()
	RETURN
END IF

SQLCA.nf_commit_transaction()


SetPointer(Arrow!)
cb_clear.TriggerEvent(Clicked!)
cb_search.Default = TRUE
end event

type gb_2 from groupbox within w_reissue
integer x = 18
integer y = 100
integer width = 571
integer height = 276
integer taborder = 120
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
end type

type st_recipient_type from statictext within w_reissue
integer x = 1495
integer y = 264
integer width = 398
integer height = 56
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
string text = "Recipient Type: "
boolean focusrectangle = false
end type

type ddlb_recipient_type from dropdownlistbox within w_reissue
integer x = 1920
integer y = 252
integer width = 608
integer height = 384
integer taborder = 50
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

type cbx_selall from checkbox within w_reissue
integer x = 2674
integer y = 444
integer width = 142
integer height = 56
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
string text = "All"
boolean lefttext = true
end type

event clicked;LONG ll_rows, ll_cntr, ll_checked
BOOLEAN lb_select

ll_rows = dw_reissue.RowCount()

IF THIS.Checked = TRUE THEN
	ll_checked = 1
	lb_select = TRUE
ELSE
	ll_checked = 0
	lb_select = FALSE
END IF

FOR ll_cntr = 1 to ll_rows
	dw_reissue.SetItem(ll_cntr, 'checkbox_group', ll_checked)
	dw_reissue.SelectRow(ll_cntr, lb_select)
NEXT 

end event

type em_date from editmask within w_reissue
integer x = 1920
integer y = 160
integer width = 608
integer height = 80
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "yyyy-mm-dd"
boolean dropdowncalendar = true
end type

type st_1 from statictext within w_reissue
integer x = 1495
integer y = 172
integer width = 398
integer height = 56
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
string text = "Deposit Date: "
boolean focusrectangle = false
end type

type dw_replace from datawindow within w_reissue
boolean visible = false
integer x = 59
integer y = 1664
integer width = 201
integer height = 108
integer taborder = 70
boolean bringtotop = true
string dataobject = "d_chq_dd_replace"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_daily_chq from datawindow within w_reissue
boolean visible = false
integer x = 338
integer y = 1672
integer width = 229
integer height = 112
integer taborder = 130
boolean bringtotop = true
string dataobject = "d_daily_chq"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_daily_dd from datawindow within w_reissue
boolean visible = false
integer x = 608
integer y = 1652
integer width = 146
integer height = 124
integer taborder = 90
boolean bringtotop = true
string dataobject = "d_daily_dd"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type gb_1 from groupbox within w_reissue
integer x = 613
integer y = 100
integer width = 2514
integer height = 276
integer taborder = 140
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
end type

