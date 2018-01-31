$PBExportHeader$w_overpayments.srw
forward
global type w_overpayments from w_a_tool
end type
type cb_add from commandbutton within w_overpayments
end type
type cb_save from commandbutton within w_overpayments
end type
type cb_cancel from commandbutton within w_overpayments
end type
type dw_overpayment_master from u_dw_online within w_overpayments
end type
type dw_overpayment_status from u_dw_online within w_overpayments
end type
type pb_maximize_overpayment_list from picturebutton within w_overpayments
end type
type dw_overpayment_details from u_dw_online within w_overpayments
end type
type dw_overpayment_list from u_dw_online within w_overpayments
end type
end forward

global type w_overpayments from w_a_tool
integer width = 3177
integer height = 1820
boolean resizable = false
cb_add cb_add
cb_save cb_save
cb_cancel cb_cancel
dw_overpayment_master dw_overpayment_master
dw_overpayment_status dw_overpayment_status
pb_maximize_overpayment_list pb_maximize_overpayment_list
dw_overpayment_details dw_overpayment_details
dw_overpayment_list dw_overpayment_list
end type
global w_overpayments w_overpayments

type variables
BOOLEAN        ib_list_maximized
LONG           il_claim_no
N_OVERPAYMENT  inv_overpayments
end variables

forward prototypes
public function integer wf_retrieve_recipients ()
public subroutine wf_prepare_datawindows (string as_mode)
public subroutine wf_select_master_recipient ()
public function integer wf_new_overpayment_recipient (long al_recipient_no, string as_overpayment_type_code)
public subroutine wf_shrink_enlarge (string as_dw_state)
public subroutine wf_select_master_op_type ()
end prototypes

public function integer wf_retrieve_recipients ();DATAWINDOWCHILD    ldwc_recipient_no
INTEGER            li_rtn


li_rtn = dw_overpayment_details.GetChild('recipient_no',ldwc_recipient_no)
ldwc_recipient_no.SetTransObject(SQLCA)
	
// Retrieve the drop down data window of claim participants, based on the claim number.
li_rtn = ldwc_recipient_no.Retrieve(il_claim_no)
SQLCA.nf_handle_error('w_overpayments', 'ldwc_recipient_no.retrieve', 'wf_retrieve_recipients')

RETURN li_rtn
end function

public subroutine wf_prepare_datawindows (string as_mode);
CHOOSE CASE as_mode
	CASE 'ADD'
		dw_overpayment_details.Visible = TRUE
		dw_overpayment_details.Height = dw_overpayment_list.Height
		dw_overpayment_details.Width  = dw_overpayment_list.Width
		dw_overpayment_details.Y      = dw_overpayment_list.Y
		dw_overpayment_master.Enabled = FALSE
		pb_maximize_overpayment_list.Visible = FALSE

	CASE 'SAVE','CANCEL'
		dw_overpayment_list.Height = dw_overpayment_details.Height
		dw_overpayment_list.Width  = dw_overpayment_details.Width
		dw_overpayment_details.Y   = dw_overpayment_details.Y
		dw_overpayment_details.Visible = FALSE
		dw_overpayment_master.Enabled  = TRUE
		pb_maximize_overpayment_list.Visible = TRUE
		
END CHOOSE
end subroutine

public subroutine wf_select_master_recipient ();DATAWINDOWCHILD     ldwc_recipient
INTEGER             li_rtn, li_find, li_rows
LONG                ll_recipient_no
STRING              ls_find, ls_recipient_name

ll_recipient_no = dw_overpayment_master.GetItemNumber(dw_overpayment_master.GetRow(),'recipient_no')
li_rtn = dw_overpayment_details.GetChild('recipient_no',ldwc_recipient)
ls_find = 'individual_no = ' + String(ll_recipient_no)

ldwc_recipient.SetTransObject(SQLCA)
li_rows = ldwc_recipient.Retrieve(il_claim_no)
li_find = ldwc_recipient.Find(ls_find,1,li_rows)

IF li_find > 0 THEN
	ldwc_recipient.SetRow(li_find)
	ldwc_recipient.ScrollToRow(li_find)
	ls_recipient_name = ldwc_recipient.GetItemString(li_find,'name')
	dw_overpayment_details.SetColumn('recipient_no')
	dw_overpayment_details.SetText(ls_recipient_name)
END IF
end subroutine

public function integer wf_new_overpayment_recipient (long al_recipient_no, string as_overpayment_type_code);INTEGER     li_count

SELECT Count(*)
INTO   :li_count
FROM   OVERPAYMENT_BALANCE
WHERE  recipient_no = :al_recipient_no
AND    overpayment_type_code = :as_overpayment_type_code
USING SQLCA;
SQLCA.nf_handle_error('w_overpayments','embedded SQL: SELECT Count(*) FROM OVERPAYMENT_BALANCE...', 'wf_new_overpayment_recipient')

RETURN li_count
end function

public subroutine wf_shrink_enlarge (string as_dw_state);LONG ll_title_master_height



ll_title_master_height = st_title.Height + dw_overpayment_master.Height + 50

IF as_dw_state = "ENLARGE" THEN
	//	Resize the height of the Overpayment list to be just below the 'master' datawindow and just above the 'add' button
	dw_overpayment_list.Y      = ll_title_master_height
	dw_overpayment_list.Height = THIS.Height - ( ll_title_master_height + cb_add.Height + 46 )
ELSE
	//	Resize the height of the Overpayment list to be just below the 'status' datawindow and just above the 'add' button
	dw_overpayment_list.Y      = ll_title_master_height + dw_overpayment_status.Height
	dw_overpayment_list.Height = THIS.Height - (ll_title_master_height + dw_overpayment_status.Height + cb_add.Height + 46)
END IF 

end subroutine

public subroutine wf_select_master_op_type ();DATAWINDOWCHILD     ldwc_op_type
INTEGER             li_rtn, li_find, li_rows
LONG                ll_recipient_no
STRING              ls_find, ls_overpayment_type_code

ls_overpayment_type_code = dw_overpayment_master.GetItemString(dw_overpayment_master.GetRow(),'overpayment_type_code')
li_rtn = dw_overpayment_details.GetChild('overpayment_type_code',ldwc_op_type)
ls_find = 'overpayment_type_code = "' + String(ls_overpayment_type_code) +'"'

ldwc_op_type.SetTransObject(SQLCA)
li_rows = ldwc_op_type.Retrieve()
li_find = ldwc_op_type.Find(ls_find,1,li_rows)

IF li_find > 0 THEN
	ldwc_op_type.SetRow(li_find)
	ldwc_op_type.ScrollToRow(li_find)
	ls_overpayment_type_code = ldwc_op_type.GetItemString(li_find,'overpayment_type_code')
	dw_overpayment_details.SetColumn('overpayment_type_code')
	dw_overpayment_details.SetText(ls_overpayment_type_code)
END IF
end subroutine

event open;call super::open;INTEGER          li_rtn, ll_rows
U_DWA	           ldw_dw[]

dw_overpayment_status.SetTransObject(SQLCA)
dw_overpayment_master.SetTransObject(SQLCA)
dw_overpayment_list.SetTransObject(SQLCA)
dw_overpayment_details.SetTransObject(SQLCA)

IF NOT IsValid(iw_active_sheet) THEN
	MessageBox('Error','Unable to determine claim information.  Check resources and try again.')
	Close(This)
	Return
END IF

IF iw_active_sheet.dw_basic_claim.RowCount() <> 1 THEN
	MessageBox('Error','No claim is selected.  Search for claim and try again.')
	Close(This)
	Return
END IF

il_claim_no = iw_active_sheet.dw_basic_claim.GetItemNumber(1,'claim_no')

inv_overpayments.nf_set_window_parent(THIS)

ldw_dw[1] = dw_overpayment_details
ldw_dw[2] = dw_overpayment_status
ldw_dw[3] = dw_overpayment_master

inv_overpayments.nf_init(ldw_dw[],SQLCA,THIS)
inv_overpayments.nf_set_commit(TRUE)

inv_overpayments.nf_set_instance(il_claim_no)

li_rtn = dw_overpayment_status.ShareData(dw_overpayment_master)
ll_rows = dw_overpayment_master.Retrieve(il_claim_no)

// resize stuff
il_design_time_width = 3168
il_design_time_height = 1812

This.wf_SetResize(True)

//(Move H,Move V,Grow H, Grow V)
inv_resize.of_register(dw_overpayment_master,0,0,100,0)
inv_resize.of_register(dw_overpayment_status,0,0,100,0)
inv_resize.of_register(dw_overpayment_list,0,0,100,100)
inv_resize.of_register(dw_overpayment_details,0,0,100,100)
inv_resize.of_register(st_title,0,0,100,0)
inv_resize.of_register(cb_add,0,100,0,0)
inv_resize.of_register(cb_save,0,100,0,0)
inv_resize.of_register(cb_cancel,0,100,0,0)
inv_resize.of_register(cb_close,0,100,0,0)
end event

on w_overpayments.create
int iCurrent
call super::create
this.cb_add=create cb_add
this.cb_save=create cb_save
this.cb_cancel=create cb_cancel
this.dw_overpayment_master=create dw_overpayment_master
this.dw_overpayment_status=create dw_overpayment_status
this.pb_maximize_overpayment_list=create pb_maximize_overpayment_list
this.dw_overpayment_details=create dw_overpayment_details
this.dw_overpayment_list=create dw_overpayment_list
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_add
this.Control[iCurrent+2]=this.cb_save
this.Control[iCurrent+3]=this.cb_cancel
this.Control[iCurrent+4]=this.dw_overpayment_master
this.Control[iCurrent+5]=this.dw_overpayment_status
this.Control[iCurrent+6]=this.pb_maximize_overpayment_list
this.Control[iCurrent+7]=this.dw_overpayment_details
this.Control[iCurrent+8]=this.dw_overpayment_list
end on

on w_overpayments.destroy
call super::destroy
destroy(this.cb_add)
destroy(this.cb_save)
destroy(this.cb_cancel)
destroy(this.dw_overpayment_master)
destroy(this.dw_overpayment_status)
destroy(this.pb_maximize_overpayment_list)
destroy(this.dw_overpayment_details)
destroy(this.dw_overpayment_list)
end on

event resize;call super::resize;LONG    ll_op_status_t_width, ll_op_status_col_width, ll_op_status_comment_t_width, ll_dw_width


ll_op_status_t_width         = Long(dw_overpayment_status.Object.op_status_code_t.Width)
ll_op_status_col_width       = Long(dw_overpayment_status.Object.op_status_code.Width)
ll_op_status_comment_t_width = Long(dw_overpayment_status.Object.comment_t.Width)

ll_dw_width            = dw_overpayment_status.Width


dw_overpayment_status.Object.comment.Width = ll_dw_width - ( ll_op_status_t_width + ll_op_status_col_width + ll_op_status_comment_t_width + 152 )
dw_overpayment_details.Object.comment.Width = dw_overpayment_status.Object.comment.Width
dw_overpayment_details.Object.gb_details.Width = dw_overpayment_details.Width - 16

pb_maximize_overpayment_list.X = dw_overpayment_list.X + (dw_overpayment_list.Width - 168)
cb_close.X = ( dw_overpayment_list.Width - cb_close.Width) + 8
end event

type st_title from w_a_tool`st_title within w_overpayments
integer width = 3127
string text = "Overpayments"
end type

type cb_close from w_a_tool`cb_close within w_overpayments
integer y = 1700
integer taborder = 30
end type

type cb_add from commandbutton within w_overpayments
integer x = 544
integer y = 1700
integer width = 379
integer height = 100
integer taborder = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Add"
end type

event clicked;
IF ib_list_maximized THEN
	MessageBox('List Maximized','The overpayment list has been maximized. Please restore the list to its normal size before attempting to add overpayment details.',Exclamation!)
	RETURN
END IF

SetPointer(HourGlass!)
IF inv_overpayments.nf_insert(0,il_claim_no) >= 0 THEN
	cb_save.enabled = TRUE
	cb_cancel.enabled = TRUE
	cb_add.enabled = FALSE
	
	wf_prepare_datawindows('ADD')
					
	// select 'master' individual in the 'detail' dddw
	IF dw_overpayment_master.GetRow() > 0 THEN
		wf_select_master_op_type()
		wf_select_master_recipient()
	END IF
	
	wf_retrieve_recipients()
	
	dw_overpayment_details.SetColumn('detail_amount')
	
END IF
end event

type cb_save from commandbutton within w_overpayments
integer x = 1033
integer y = 1700
integer width = 379
integer height = 100
integer taborder = 50
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Save"
end type

event clicked;INTEGER   li_find, li_trancount, li_master_row
LONG      ll_rows, ll_recipient_no
STRING    ls_find, ls_comment, ls_overpayment_type_code

SetPointer(HourGlass!)



dw_overpayment_details.AcceptText()
dw_overpayment_master.AcceptText()
dw_overpayment_status.AcceptText()

// clear comment column of unwanted characters
ls_comment = dw_overpayment_details.GetItemString(dw_overpayment_details.GetRow(),'comment')
ls_comment = Trim(f_clean_string_1(ls_comment))
dw_overpayment_details.SetItem(dw_overpayment_details.GetRow(),'comment',ls_comment)


// get detail values, so that master record can be selected after save
IF dw_overpayment_details.GetRow() = 1 THEN
	ll_recipient_no          = dw_overpayment_details.GetItemNumber(1,'recipient_no')
	ls_overpayment_type_code = dw_overpayment_details.GetItemString(1,'overpayment_type_code')
ELSE
	// modifying O/P status only
	li_master_row = dw_overpayment_master.GetRow()
	ll_recipient_no          = dw_overpayment_master.GetItemNumber(li_master_row,'recipient_no')
	ls_overpayment_type_code = dw_overpayment_master.GetItemString(li_master_row,'overpayment_type_code')
END IF

SQLCA.nf_begin_transaction()

IF inv_overpayments.nf_save() >= 0	THEN
	
	SQLCA.nf_commit_transaction()
	
	THIS.SetRedraw(FALSE)
	
	cb_save.enabled = FALSE
	cb_cancel.enabled = FALSE
	cb_add.enabled = TRUE
	
	ll_rows = dw_overpayment_master.Retrieve(il_claim_no)
	
	wf_prepare_datawindows('SAVE')
	
	ls_find = 'recipient_no = ' +String(ll_recipient_no)+ ' and overpayment_type_code = "' +ls_overpayment_type_code+ '"'
	li_find = dw_overpayment_master.Find(ls_find,1,ll_rows)
	dw_overpayment_master.ScrollToRow(li_find)
	dw_overpayment_master.SelectRow(li_find,TRUE)

	dw_overpayment_master.trigger event RowFocusChanged(li_find)
	
	// because of the sort order, the last row will be the one just added
	ll_rows = dw_overpayment_list.RowCount()
	dw_overpayment_list.ScrollToRow(ll_rows)
	dw_overpayment_list.SetRow(ll_rows)
	
	// clear the recently added detail
	IF dw_overpayment_details.RowCount() > 0 THEN
		dw_overpayment_details.Reset()
	END IF
	
	THIS.SetRedraw(TRUE)
ELSE
	SQLCA.nf_transaction_count(li_trancount,0,'','','',FALSE)
	IF li_trancount > 0 THEN
		SQLCA.nf_rollback_transaction()
	END IF
END IF

end event

type cb_cancel from commandbutton within w_overpayments
integer x = 1422
integer y = 1700
integer width = 379
integer height = 100
integer taborder = 40
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Cance&l"
end type

event clicked;INTEGER  li_find, li_master_row, li_master_rowcount
LONG     ll_recipient_no
STRING   ls_overpayment_type_code, ls_find


SetPointer(HourGlass!)

IF dw_overpayment_details.GetRow() = 1 THEN
	// get detail values, so that master record can be selected after save
	ll_recipient_no          = dw_overpayment_details.GetItemNumber(1,'recipient_no')
	ls_overpayment_type_code = dw_overpayment_details.GetItemString(1,'overpayment_type_code')
ELSE
	li_master_row = dw_overpayment_master.GetRow()
	IF li_master_row > 0 THEN
		ll_recipient_no          = dw_overpayment_master.GetItemNumber(li_master_row,'recipient_no')
		ls_overpayment_type_code = dw_overpayment_master.GetItemString(li_master_row,'overpayment_type_code')
	END IF
END IF

dw_overpayment_details.Reset()
dw_overpayment_status.Reset()
dw_overpayment_list.TriggerEvent(RowFocusChanged!)

cb_add.enabled    = TRUE
cb_cancel.enabled = FALSE
cb_save.enabled   = FALSE

wf_prepare_datawindows('CANCEL')




IF il_claim_no > 0 THEN 
	li_master_rowcount = dw_overpayment_master.Retrieve(il_claim_no)
	SQLCA.nf_handle_error('w_overpayments','dw_overpayment_master.retrieve','cb_cancel.clicked')
	
	dw_overpayment_master.TriggerEvent(RowFocusChanged!)
	
	IF ll_recipient_no > 0 THEN		
		ls_find = 'recipient_no = ' +String(ll_recipient_no)+ ' and overpayment_type_code = "' +ls_overpayment_type_code+ '"'
		li_find = dw_overpayment_master.Find(ls_find,1,li_master_rowcount)
		dw_overpayment_master.ScrollToRow(li_find)
		dw_overpayment_master.SelectRow(li_find,TRUE)
	END IF
END IF
end event

type dw_overpayment_master from u_dw_online within w_overpayments
integer x = 9
integer y = 92
integer width = 3127
integer height = 364
integer taborder = 10
boolean bringtotop = true
string dataobject = "d_overpayment_master"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event rowfocuschanged;call super::rowfocuschanged;INTEGER  li_list_rows, li_status_rows
LONG		ll_listrow, ll_recipient_no
STRING   ls_overpayment_type_code, ls_recipient_type_code


IF This.RowCount() = 0 THEN
	cb_add.enabled = TRUE
	Return
END IF

ll_listrow = This.GetRow()
This.SelectRow(0,false)
This.SelectRow(ll_listrow,true)

/*	Retrieve and display overpayment details                                                                 
*/
ll_recipient_no = THIS.GetItemNumber(ll_listrow,'recipient_no')
ls_overpayment_type_code = THIS.GetItemString(ll_listrow,'overpayment_type_code')
ls_recipient_type_code   = THIS.GetItemString(ll_listrow,'recipient_type_code')

li_list_rows = dw_overpayment_list.retrieve(il_claim_no,ll_recipient_no,ls_overpayment_type_code)
SQLCA.nf_handle_error('w_overpayments','dw_overpayment_list','retrieve')

li_status_rows = dw_overpayment_status.retrieve(il_claim_no,ls_overpayment_type_code,ll_recipient_no,ls_recipient_type_code)
SQLCA.nf_handle_error('w_overpayments','dw_overpayment_status','retrieve')


dw_overpayment_list.Trigger Event RowFocusChanged(1)
dw_overpayment_status.Trigger Event RowFocusChanged(1)
end event

type dw_overpayment_status from u_dw_online within w_overpayments
integer x = 9
integer y = 492
integer width = 3136
integer height = 236
integer taborder = 11
boolean bringtotop = true
string dataobject = "d_overpayment_status"
boolean border = false
borderstyle borderstyle = StyleBox!
end type

event itemchanged;call super::itemchanged;Integer li_rtn
	
li_rtn = inv_overpayments.nf_change_item(3)
IF li_rtn = -1 THEN
	RETURN 1
END IF

cb_add.Enabled = FALSE
cb_save.Enabled = TRUE
cb_cancel.Enabled = TRUE

dw_overpayment_master.Enabled = FALSE
end event

event editchanged;call super::editchanged;DWItemStatus   ldwis_op_status



IF dwo.name = 'comment' THEN
	ldwis_op_status = THIS.GetItemStatus(row,0,Primary!)
	CHOOSE CASE ldwis_op_status
		CASE NotModified!
			THIS.SetItemStatus(row,0,Primary!,DataModified!)
		CASE New!
			THIS.SetItemStatus(row,0,Primary!,NewModified!)
	END CHOOSE
END IF
end event

type pb_maximize_overpayment_list from picturebutton within w_overpayments
integer x = 3022
integer y = 732
integer width = 91
integer height = 76
integer taborder = 10
boolean bringtotop = true
integer textsize = -12
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "maximize.bmp"
alignment htextalign = left!
end type

event clicked;
IF THIS.picturename = "maximize.bmp" THEN
	PARENT.wf_shrink_enlarge('ENLARGE')
	ib_list_maximized = TRUE
	THIS.picturename = "restore.bmp"
ELSE
	PARENT.wf_shrink_enlarge('SHRINK')
	ib_list_maximized = FALSE
	THIS.picturename = "maximize.bmp"
END IF

dw_overpayment_list.bringtotop = TRUE
THIS.Y                         = dw_overpayment_list.Y
THIS.bringtotop                = TRUE

end event

type dw_overpayment_details from u_dw_online within w_overpayments
boolean visible = false
integer x = 9
integer y = 732
integer width = 3136
integer height = 944
integer taborder = 20
string dataobject = "d_overpayment_details"
boolean border = false
end type

event itemchanged;call super::itemchanged;Integer li_rtn
	
li_rtn = inv_overpayments.nf_change_item(1)
IF li_rtn = -1 THEN
	RETURN 1
END IF

cb_add.Enabled = FALSE
cb_save.Enabled = TRUE
cb_cancel.Enabled = TRUE

end event

event retrieveend;call super::retrieveend;IF THIS.GetItemDecimal(1,'detail_amount') < 0 THEN
	THIS.SetItem(1,'db_cr','C')
ELSE
	THIS.SetItem(1,'db_cr','D')
END IF		

THIS.SetItemStatus(1,0,Primary!,NotModified!)
end event

type dw_overpayment_list from u_dw_online within w_overpayments
integer x = 9
integer y = 732
integer width = 3127
integer height = 944
integer taborder = 70
string dataobject = "d_overpayment_list"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event rowfocuschanged;call super::rowfocuschanged;LONG		            ll_listrow


IF This.RowCount() = 0 THEN
	cb_add.enabled = TRUE
	Return
END IF

ll_listrow = This.GetRow()
This.SelectRow(0,false)
This.SelectRow(ll_listrow,true)


end event

event rowfocuschanging;call super::rowfocuschanging;INTEGER        li_row, li_new_recipient
DWItemStatus   ldwi_status
LONG           ll_recipient_no
STRING         ls_overpayment_type_code


li_row = dw_overpayment_details.GetRow()

IF li_row > 0 THEN
	ll_recipient_no = dw_overpayment_details.GetItemNumber(li_row,'recipient_no')
	ls_overpayment_type_code = dw_overpayment_details.GetItemString(li_row,'overpayment_type_code')
	
	
	li_new_recipient = wf_new_overpayment_recipient(ll_recipient_no,ls_overpayment_type_code)
	
	
	IF li_new_recipient > 0 THEN
		IF currentrow > 0 THEN
			ldwi_status = dw_overpayment_details.GetItemStatus(currentrow,0,Primary!)
			IF ldwi_status <> NotModified! THEN
				MessageBox('Save Changes','Please save your changes before selecting another overpayment record.',Exclamation!)
				RETURN 1
			END IF	
		END IF
	END IF
END IF
end event

