$PBExportHeader$w_maintain_cheques.srw
$PBExportComments$Maintain Cheques Window
forward
global type w_maintain_cheques from w_ancestor
end type
type rb_cheque_amount from radiobutton within w_maintain_cheques
end type
type rb_cheque_no_sort from radiobutton within w_maintain_cheques
end type
type em_cheques_found from editmask within w_maintain_cheques
end type
type st_cheques_found from statictext within w_maintain_cheques
end type
type cb_delete from commandbutton within w_maintain_cheques
end type
type cbx_between from checkbox within w_maintain_cheques
end type
type em_to_date from editmask within w_maintain_cheques
end type
type em_from_date from editmask within w_maintain_cheques
end type
type rb_cheque_date from radiobutton within w_maintain_cheques
end type
type rb_cheque_number from radiobutton within w_maintain_cheques
end type
type rb_handwritten from radiobutton within w_maintain_cheques
end type
type cb_reconcile from commandbutton within w_maintain_cheques
end type
type cb_search from commandbutton within w_maintain_cheques
end type
type cb_cancel from commandbutton within w_maintain_cheques
end type
type cb_save from commandbutton within w_maintain_cheques
end type
type em_cheque_no from editmask within w_maintain_cheques
end type
type st_1 from statictext within w_maintain_cheques
end type
type dw_cheque_details from u_dw_online within w_maintain_cheques
end type
type cb_add from commandbutton within w_maintain_cheques
end type
type dw_cheque_list from u_dw_online within w_maintain_cheques
end type
type cb_close from commandbutton within w_maintain_cheques
end type
type gb_1 from groupbox within w_maintain_cheques
end type
type gb_search_type from groupbox within w_maintain_cheques
end type
type rb_reconciled_date from radiobutton within w_maintain_cheques
end type
type dw_cheque_stub from u_dw_online within w_maintain_cheques
end type
type cbx_to from checkbox within w_maintain_cheques
end type
type em_to_cheque_no from editmask within w_maintain_cheques
end type
end forward

global type w_maintain_cheques from w_ancestor
integer width = 2752
integer height = 2352
string title = "Handwritten Cheque Maintenance"
string menuname = "m_cmwb_notools"
windowtype windowtype = main!
long backcolor = 67108864
rb_cheque_amount rb_cheque_amount
rb_cheque_no_sort rb_cheque_no_sort
em_cheques_found em_cheques_found
st_cheques_found st_cheques_found
cb_delete cb_delete
cbx_between cbx_between
em_to_date em_to_date
em_from_date em_from_date
rb_cheque_date rb_cheque_date
rb_cheque_number rb_cheque_number
rb_handwritten rb_handwritten
cb_reconcile cb_reconcile
cb_search cb_search
cb_cancel cb_cancel
cb_save cb_save
em_cheque_no em_cheque_no
st_1 st_1
dw_cheque_details dw_cheque_details
cb_add cb_add
dw_cheque_list dw_cheque_list
cb_close cb_close
gb_1 gb_1
gb_search_type gb_search_type
rb_reconciled_date rb_reconciled_date
dw_cheque_stub dw_cheque_stub
cbx_to cbx_to
em_to_cheque_no em_to_cheque_no
end type
global w_maintain_cheques w_maintain_cheques

type variables
Long il_min_handwritten_cheque_no
Long il_max_handwritten_cheque_no
STRING is_row_filter, is_filter_choice
end variables

forward prototypes
public subroutine wf_enable_reconcile_fields (string as_reconciled_code)
public subroutine wf_get_cheque_details (long al_cheque_no)
public subroutine wf_enable_dw (boolean ab_new_cheque, boolean ab_reconciled)
end prototypes

public subroutine wf_enable_reconcile_fields (string as_reconciled_code);// wf_enable_reconcile_fields
//
String ls_rtn

CHOOSE CASE as_reconciled_code
	CASE "09"  // Matched Cheque within $1.00
		ls_rtn = dw_cheque_details.Modify("replacement_cheque_no.protect='1' replacement_cheque_no.background.color='67108864' replacement_cheque_no.border='0'")
		ls_rtn = dw_cheque_details.Modify("reconciled_amount.protect='0'     reconciled_amount.background.color='16777215'     reconciled_amount.border='5'")
	CASE "20"  // Stop Payment
		ls_rtn = dw_cheque_details.Modify("replacement_cheque_no.protect='1' replacement_cheque_no.background.color='67108864' replacement_cheque_no.border='0'")
		ls_rtn = dw_cheque_details.Modify("reconciled_amount.protect='1'     reconciled_amount.background.color='67108864'     reconciled_amount.border='0'")
	CASE "21"  // Manually Reconciled
		ls_rtn = dw_cheque_details.Modify("replacement_cheque_no.protect='1' replacement_cheque_no.background.color='67108864' replacement_cheque_no.border='0'")
		ls_rtn = dw_cheque_details.Modify("reconciled_amount.protect='0'     reconciled_amount.background.color='16777215'     reconciled_amount.border='5'")
	CASE "98"  // Replacement Cheque
		ls_rtn = dw_cheque_details.Modify("replacement_cheque_no.protect='0' replacement_cheque_no.background.color='16777215' replacement_cheque_no.border='5'")
		ls_rtn = dw_cheque_details.Modify("reconciled_amount.protect='1'     reconciled_amount.background.color='67108864'     reconciled_amount.border='0'")
	CASE ELSE
		ls_rtn = dw_cheque_details.Modify("replacement_cheque_no.protect='1' replacement_cheque_no.background.color='67108864' replacement_cheque_no.border='0'")
		ls_rtn = dw_cheque_details.Modify("reconciled_amount.protect='1'     reconciled_amount.background.color='67108864'     reconciled_amount.border='0'")
END CHOOSE

end subroutine

public subroutine wf_get_cheque_details (long al_cheque_no);Long     ll_num_rows
Integer  li_rtn
String   ls_cheque_type_code, ls_payment_method_code, ls_reconciled_amount_flag
String   ls_reconciled_code
Datetime ldt_transmit_date, ldt_processed_date
Boolean  lb_new_cheque, lb_reconciled

ll_num_rows = dw_cheque_details.Retrieve(al_cheque_no)
li_rtn = SQLCA.nf_handle_error("w_maintain_cheques","dw_cheque_details","wf_get_cheque_details - dw_cheque_details.Retrieve(al_cheque_no)")

IF ll_num_rows > 0 THEN
	ldt_transmit_date = dw_cheque_details.GetItemDateTime(1, "transmit_date")
	ldt_processed_date = dw_cheque_details.GetItemDateTime(1, "processed_date")
	ls_cheque_type_code = dw_cheque_details.GetItemString(1, "cheque_type_code")
	ls_payment_method_code = dw_cheque_details.GetItemString(1, "payment_method_code")
	ls_reconciled_amount_flag = dw_cheque_details.GetItemString(1, "reconciled_amount_flag")
	ls_reconciled_code = dw_cheque_details.GetItemString(1, "reconciled_code")
	
	IF IsNull(ldt_transmit_date) = TRUE AND (ls_cheque_type_code = "OTH" OR ls_payment_method_code = "H") THEN
		lb_new_cheque = TRUE
		lb_reconciled = FALSE
		wf_enable_dw(lb_new_cheque, lb_reconciled)
	ELSE
		lb_new_cheque = FALSE
		lb_reconciled = FALSE
		wf_enable_dw(lb_new_cheque, lb_reconciled)
	END IF
	dw_cheque_details.AcceptText()

	// Enable or Disable Reconcile button
	IF ls_reconciled_amount_flag = "Y" OR IsNull(ldt_processed_date) = TRUE THEN
		cb_reconcile.Enabled = FALSE
	ELSE
		cb_reconcile.Enabled = TRUE
	END IF

	IF ls_reconciled_code = "09" AND IsNull(ldt_transmit_date) = TRUE THEN
		cb_reconcile.Enabled = FALSE
	END IF

	// Enable or Disable Delete Button
	IF (ls_cheque_type_code <> "OTH" AND ls_cheque_type_code <> "SP") OR IsNull(ldt_transmit_date) = FALSE OR ls_reconciled_amount_flag = "Y" THEN
		cb_delete.Enabled = FALSE
	ELSE
		cb_delete.Enabled = TRUE
	END IF
END IF
dw_cheque_list.SetFocus()

end subroutine

public subroutine wf_enable_dw (boolean ab_new_cheque, boolean ab_reconciled);// wf_enable_dw
//
// Arguments: ab_new_cheque
//
// Returns: Nothing
//
Integer li_rtn
String  ls_rtn
DataWindowChild ldwc_reconciled

IF ab_new_cheque = TRUE THEN
	ls_rtn = dw_cheque_details.Modify("comment.protect='0'      comment.background.color='16777215'      comment.border='5'")
	ls_rtn = dw_cheque_details.Modify("name_on_cheque.protect='0' name_on_cheque.background.color='16777215' name_on_cheque.border='5'")
	ls_rtn = dw_cheque_details.Modify("cheque_type_code.protect='0' cheque_type_code.background.color='16777215' cheque_type_code.border='5'")
	ls_rtn = dw_cheque_details.Modify("address_line1.protect='0'  address_line1.background.color='16777215'  address_line1.border='5'")
	ls_rtn = dw_cheque_details.Modify("address_line2.protect='0'  address_line2.background.color='16777215'  address_line2.border='5'")
	ls_rtn = dw_cheque_details.Modify("address_line3.protect='0'  address_line3.background.color='16777215'  address_line3.border='5'")
	ls_rtn = dw_cheque_details.Modify("address_line4.protect='0'  address_line4.background.color='16777215'  address_line4.border='5'")
	ls_rtn = dw_cheque_details.Modify("address_line5.protect='0'  address_line5.background.color='16777215'  address_line5.border='5'")
	ls_rtn = dw_cheque_details.Modify("cheque_amount.protect='0'  cheque_amount.background.color='16777215'  cheque_amount.border='5'")
	ls_rtn = dw_cheque_details.Modify("cheque_date.protect='0'    cheque_date.background.color='16777215'    cheque_date.border='5'")
	ls_rtn = dw_cheque_details.Modify("requisition_no.protect='0' requisition_no.background.color='16777215' requisition_no.border='5'")
ELSE
	ls_rtn = dw_cheque_details.Modify("comment.protect='1'      comment.background.color='67108864'      comment.border='0'")
	ls_rtn = dw_cheque_details.Modify("cheque_no.protect='1'      cheque_no.background.color='67108864'      cheque_no.border='0'")
	ls_rtn = dw_cheque_details.Modify("cheque_type_code.protect='1' cheque_type_code.background.color='67108864' cheque_type_code.border='0'")
	ls_rtn = dw_cheque_details.Modify("name_on_cheque.protect='1' name_on_cheque.background.color='67108864' name_on_cheque.border='0'")
	ls_rtn = dw_cheque_details.Modify("address_line1.protect='1'  address_line1.background.color='67108864'  address_line1.border='0'")
	ls_rtn = dw_cheque_details.Modify("address_line2.protect='1'  address_line2.background.color='67108864'  address_line2.border='0'")
	ls_rtn = dw_cheque_details.Modify("address_line3.protect='1'  address_line3.background.color='67108864'  address_line3.border='0'")
	ls_rtn = dw_cheque_details.Modify("address_line4.protect='1'  address_line4.background.color='67108864'  address_line4.border='0'")
	ls_rtn = dw_cheque_details.Modify("address_line5.protect='1'  address_line5.background.color='67108864'  address_line5.border='0'")
	ls_rtn = dw_cheque_details.Modify("cheque_amount.protect='1'  cheque_amount.background.color='67108864'  cheque_amount.border='0'")
	ls_rtn = dw_cheque_details.Modify("cheque_date.protect='1'    cheque_date.background.color='67108864'    cheque_date.border='0'")
	ls_rtn = dw_cheque_details.Modify("requisition_no.protect='1' requisition_no.background.color='67108864' requisition_no.border='0'")
END IF

IF ab_reconciled = TRUE THEN
	ls_rtn = dw_cheque_details.Modify("reconciled_code.protect='0'   reconciled_code.background.color='16777215'   reconciled_code.border='5' reconciled_code.dddw.UseAsBorder=Yes")
	ls_rtn = dw_cheque_details.Modify("reconciled_amount.protect='0' reconciled_amount.background.color='16777215' reconciled_amount.border='5'")
	ls_rtn = dw_cheque_details.Modify("comment.protect='0'      comment.background.color='16777215'      comment.border='5'")
	
	dw_cheque_details.GetChild("reconciled_code", ldwc_reconciled)
	ldwc_reconciled.SetTransObject(SQLCA)
	ldwc_reconciled.SetFilter("generated_method_code=~"M~"")
	ldwc_reconciled.Filter()
ELSE
	ls_rtn = dw_cheque_details.Modify("reconciled_code.protect='1'   reconciled_code.background.color='67108864'   reconciled_code.border='0' reconciled_code.dddw.UseAsBorder=No")
	ls_rtn = dw_cheque_details.Modify("reconciled_amount.protect='1' reconciled_amount.background.color='67108864' reconciled_amount.border='0'")
	ls_rtn = dw_cheque_details.Modify("replacement_cheque_no.protect='1' replacement_cheque_no.background.color='67108864' replacement_cheque_no.border='0'")

	dw_cheque_details.GetChild("reconciled_code", ldwc_reconciled)
	ldwc_reconciled.SetTransObject(SQLCA)
	ldwc_reconciled.SetFilter("")
	ldwc_reconciled.Filter()
END IF

end subroutine

on w_maintain_cheques.create
int iCurrent
call super::create
if IsValid(this.MenuID) then destroy(this.MenuID)
if this.MenuName = "m_cmwb_notools" then this.MenuID = create m_cmwb_notools
this.rb_cheque_amount=create rb_cheque_amount
this.rb_cheque_no_sort=create rb_cheque_no_sort
this.em_cheques_found=create em_cheques_found
this.st_cheques_found=create st_cheques_found
this.cb_delete=create cb_delete
this.cbx_between=create cbx_between
this.em_to_date=create em_to_date
this.em_from_date=create em_from_date
this.rb_cheque_date=create rb_cheque_date
this.rb_cheque_number=create rb_cheque_number
this.rb_handwritten=create rb_handwritten
this.cb_reconcile=create cb_reconcile
this.cb_search=create cb_search
this.cb_cancel=create cb_cancel
this.cb_save=create cb_save
this.em_cheque_no=create em_cheque_no
this.st_1=create st_1
this.dw_cheque_details=create dw_cheque_details
this.cb_add=create cb_add
this.dw_cheque_list=create dw_cheque_list
this.cb_close=create cb_close
this.gb_1=create gb_1
this.gb_search_type=create gb_search_type
this.rb_reconciled_date=create rb_reconciled_date
this.dw_cheque_stub=create dw_cheque_stub
this.cbx_to=create cbx_to
this.em_to_cheque_no=create em_to_cheque_no
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.rb_cheque_amount
this.Control[iCurrent+2]=this.rb_cheque_no_sort
this.Control[iCurrent+3]=this.em_cheques_found
this.Control[iCurrent+4]=this.st_cheques_found
this.Control[iCurrent+5]=this.cb_delete
this.Control[iCurrent+6]=this.cbx_between
this.Control[iCurrent+7]=this.em_to_date
this.Control[iCurrent+8]=this.em_from_date
this.Control[iCurrent+9]=this.rb_cheque_date
this.Control[iCurrent+10]=this.rb_cheque_number
this.Control[iCurrent+11]=this.rb_handwritten
this.Control[iCurrent+12]=this.cb_reconcile
this.Control[iCurrent+13]=this.cb_search
this.Control[iCurrent+14]=this.cb_cancel
this.Control[iCurrent+15]=this.cb_save
this.Control[iCurrent+16]=this.em_cheque_no
this.Control[iCurrent+17]=this.st_1
this.Control[iCurrent+18]=this.dw_cheque_details
this.Control[iCurrent+19]=this.cb_add
this.Control[iCurrent+20]=this.dw_cheque_list
this.Control[iCurrent+21]=this.cb_close
this.Control[iCurrent+22]=this.gb_1
this.Control[iCurrent+23]=this.gb_search_type
this.Control[iCurrent+24]=this.rb_reconciled_date
this.Control[iCurrent+25]=this.dw_cheque_stub
this.Control[iCurrent+26]=this.cbx_to
this.Control[iCurrent+27]=this.em_to_cheque_no
end on

on w_maintain_cheques.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.rb_cheque_amount)
destroy(this.rb_cheque_no_sort)
destroy(this.em_cheques_found)
destroy(this.st_cheques_found)
destroy(this.cb_delete)
destroy(this.cbx_between)
destroy(this.em_to_date)
destroy(this.em_from_date)
destroy(this.rb_cheque_date)
destroy(this.rb_cheque_number)
destroy(this.rb_handwritten)
destroy(this.cb_reconcile)
destroy(this.cb_search)
destroy(this.cb_cancel)
destroy(this.cb_save)
destroy(this.em_cheque_no)
destroy(this.st_1)
destroy(this.dw_cheque_details)
destroy(this.cb_add)
destroy(this.dw_cheque_list)
destroy(this.cb_close)
destroy(this.gb_1)
destroy(this.gb_search_type)
destroy(this.rb_reconciled_date)
destroy(this.dw_cheque_stub)
destroy(this.cbx_to)
destroy(this.em_to_cheque_no)
end on

event closequery;call super::closequery;Integer li_rtn

IF cb_save.Enabled = TRUE THEN
	li_rtn = Messagebox("Save Changes?", "A Cheque has changes made to it," +&
							  " Do you want to save it?",Question!, YesNoCancel!, 1)
	IF li_rtn = 1 THEN
		cb_save.Triggerevent(Clicked!)
		IF cb_save.Enabled = TRUE THEN
			dw_cheque_details.SetFocus()
			RETURN 1
		END IF
	ELSEIF li_rtn = 3 THEN
		RETURN 1
	END IF
END IF

end event

event open;call super::open;Long     ll_num_rows, ll_cheque_no
Integer  li_rtn
String   ls_rtn

li_rtn = dw_cheque_details.SetTransObject(SQLCA)
li_rtn = dw_cheque_stub.SetTransObject(SQLCA)
li_rtn = dw_cheque_list.SetTransObject(SQLCA)
ll_num_rows = dw_cheque_list.Retrieve()
li_rtn = SQLCA.nf_handle_error("w_maintain_cheques","dw_cheque_list","open - dw_cheque_list.Retrieve()")
em_cheques_found.Text = String(ll_num_rows)

IF ll_num_rows > 0 THEN
	dw_cheque_list.SetRow(1)
	dw_cheque_list.SelectRow(0, FALSE)
	dw_cheque_list.SelectRow(1, TRUE)

	ll_cheque_no = dw_cheque_list.GetItemNumber(1, "cheque_no")
	wf_get_cheque_details(ll_cheque_no)
END IF

// Get the last remaining old cheque Stock Cheque number
SELECT min_handwritten_cheque_no, max_handwritten_cheque_no 
  INTO :il_min_handwritten_cheque_no, :il_max_handwritten_cheque_no
  FROM Handwritten_Cheque_No_Range ; 

li_rtn = SQLCA.nf_handle_error("w_maintain_cheques","dw_cheque_details","open - SELECT min_handwritten_cheque_no, max_handwritten_cheque_no")
IF li_rtn = 100 THEN
	SQLCA.SQLCode = -1
	SQLCA.SQLDBCode = -1
	li_rtn = SQLCA.nf_handle_error("w_maintain_cheques","dw_cheque_details","open - Handwritten_Cheque_No_Range table has no data")
	RETURN
END IF

ls_rtn = dw_cheque_details.Modify("replacement_cheque_no.protect='1' replacement_cheque_no.background.color='67108864' replacement_cheque_no.border='0'")

end event

type rb_cheque_amount from radiobutton within w_maintain_cheques
integer x = 576
integer y = 124
integer width = 494
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
string text = "Cheque Amount"
end type

event clicked;Long    ll_row, ll_num_rows, ll_found
Decimal ld_cheque_amount

// Get the current Refund Number
ll_row = dw_cheque_list.GetRow()
IF ll_row > 0 THEN
	ld_cheque_amount = dw_cheque_list.GetItemDecimal(ll_row, "cheque_amount")
END IF

// Sort the DW
dw_cheque_list.SetSort("cheque_amount A")
dw_cheque_list.Sort()

// Go Find and Re-select the Refund
ll_num_rows = dw_cheque_list.RowCount()
ll_found = dw_cheque_list.Find("cheque_amount = " + String(ld_cheque_amount), 1, ll_num_rows)
IF ll_found > 0 THEN
	dw_cheque_list.ScrollToRow(ll_found)
END IF

end event

type rb_cheque_no_sort from radiobutton within w_maintain_cheques
integer x = 576
integer y = 64
integer width = 325
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
string text = "Cheque #"
boolean checked = true
end type

event clicked;Long ll_row, ll_num_rows, ll_found, ll_cheque_no

// Get the current Refund Number
ll_row = dw_cheque_list.GetRow()
IF ll_row > 0 THEN
	ll_cheque_no = dw_cheque_list.GetItemNumber(ll_row, "cheque_no")
END IF

// Sort the DW
dw_cheque_list.SetSort("cheque_no A")
dw_cheque_list.Sort()

// Go Find and Re-select the Refund
ll_num_rows = dw_cheque_list.RowCount()
ll_found = dw_cheque_list.Find("cheque_no = " + String(ll_cheque_no), 1, ll_num_rows)
IF ll_found > 0 THEN
	dw_cheque_list.ScrollToRow(ll_found)
END IF

end event

type em_cheques_found from editmask within w_maintain_cheques
integer x = 2235
integer y = 192
integer width = 425
integer height = 84
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
boolean border = false
alignment alignment = center!
string mask = "##,###,##0"
string displaydata = ""
end type

type st_cheques_found from statictext within w_maintain_cheques
integer x = 2235
integer y = 124
integer width = 425
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
boolean enabled = false
string text = "Cheques Found:"
boolean focusrectangle = false
end type

type cb_delete from commandbutton within w_maintain_cheques
integer x = 850
integer y = 2064
integer width = 320
integer height = 96
integer taborder = 130
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Delete"
end type

event clicked;Long     ll_row_count, ll_cheque_no, ll_num_rows
Integer  li_rtn
String   ls_cheque_type_code, ls_cheque_type_desc, ls_reconciled_amount_flag
Decimal  ld_cheque_amount
Datetime ldt_transmit_date, ldt_reconciled_date, ldt_cheque_date

// Make sure there's a cheque to delete
dw_cheque_details.SetFocus()
ll_row_count = dw_cheque_details.RowCount()
IF ll_row_count = 0 OR IsNull(ll_row_count) = TRUE THEN
	MessageBox("No Cheque to Delete", "Couldn't find a cheque to delete, Select a cheque and try again.", Information!)
	dw_cheque_list.SetFocus()
	RETURN
END IF

// Get information about the cheque
ls_cheque_type_code = dw_cheque_details.GetItemString(1, "cheque_type_code")
ldt_transmit_date = dw_cheque_details.GetItemDateTime(1, "transmit_date")
ldt_reconciled_date = dw_cheque_details.GetItemDateTime(1, "reconciled_date")
ls_reconciled_amount_flag = dw_cheque_details.GetItemString(1, "reconciled_amount_flag")
ll_cheque_no = dw_cheque_details.GetItemNumber(1, "cheque_no")
ld_cheque_amount = dw_cheque_details.GetItemDecimal(1, "cheque_amount")
ldt_cheque_date = dw_cheque_details.GetItemDatetime(1, "cheque_date")

// Make sure the cheque type is other
IF ls_cheque_type_code <> "OTH" AND ls_cheque_type_code <> 'SP' THEN
	SELECT cheque_type_desc 
	  INTO :ls_cheque_type_desc 
	  FROM Cheque_Type
	 WHERE cheque_type_code = :ls_cheque_type_code ;

	li_rtn = SQLCA.nf_handle_error("w_maintain_cheques","dw_cheque_details","cb_delete - SELECT cheque_type_desc FROM Cheque_Type")

	MessageBox("Delete Not Allowed", "You can't delete cheques that are of type " + ls_cheque_type_desc +&
				  ".~rYou can only delete cheques that are of type Other.", Information!)
	dw_cheque_list.SetFocus()
	RETURN
END IF

// Make sure the Transmit Date is null
IF IsNull(ldt_transmit_date) = FALSE THEN
	MessageBox("Delete Not Allowed", "You can't delete cheques that have already been transmitted.~rCheque " +&
				  String(ll_cheque_no) + " was transmitted on " + String(ldt_transmit_date,"mmmm dd, yyyy") + ".", Information!)
	dw_cheque_list.SetFocus()
	RETURN
END IF

// Make sure the Reconciled Date isn't set
IF IsNull(ldt_reconciled_date) = FALSE OR ls_reconciled_amount_flag = "Y" THEN
	MessageBox("Delete Not Allowed", "You can't delete cheques that have already been reconciled.~rCheque " +&
				  String(ll_cheque_no) + " was reconciled on " + String(ldt_reconciled_date,"mmmm dd, yyyy") + ".", Information!)
	dw_cheque_list.SetFocus()
	RETURN
END IF

// Ask user if they're sure they want to Delete
li_rtn = MessageBox("Delete Cheque?", "Are you sure you want to delete the following cheque:~r~r" +&
						  "Cheque Number:~t" + String(ll_cheque_no) + "~r" +&
						  "Cheque Date:~t" + String(ldt_cheque_date, "mmm dd, yyyy") + "~r" +&
						  "Cheque Amount:~t" + String(ld_cheque_amount, "$#,##0.00"), Question!, YesNo!, 2)
IF li_rtn = 2 THEN
	dw_cheque_list.SetFocus()
	RETURN
END IF


SQLCA.nf_begin_transaction()

// Delete the CHEQUE_HEADER
DELETE CHEQUE_HEADER WHERE cheque_no = :ll_cheque_no ;
li_rtn = SQLCA.nf_handle_error("w_maintain_cheques","dw_cheque_details","cb_delete - dw_cheque_details.Update()")

// Delete the CHEQUE_STUB
DELETE CHEQUE_STUB WHERE cheque_no = :ll_cheque_no ;
li_rtn = SQLCA.nf_handle_error("w_maintain_cheques","dw_cheque_details","cb_delete - DELETE CHEQUE_STUB WHERE cheque_no = :ll_cheque_no")

SQLCA.nf_commit_transaction()


// Refresh the DWs
dw_cheque_details.Reset()
ll_num_rows = dw_cheque_list.Retrieve()
li_rtn = SQLCA.nf_handle_error("w_maintain_cheques","dw_cheque_details","cb_delete - dw_cheque_list.Retrieve()")
IF ll_num_rows > 0 THEN
	dw_cheque_list.SetRow(1)
	dw_cheque_list.SelectRow(0, FALSE)
	dw_cheque_list.SelectRow(1, TRUE)

	ll_cheque_no = dw_cheque_list.GetItemNumber(1, "cheque_no")
	wf_get_cheque_details(ll_cheque_no)
END IF

cb_add.Enabled = TRUE
cb_save.Enabled = FALSE
cb_cancel.Enabled = FALSE
cb_reconcile.Enabled = TRUE
end event

type cbx_between from checkbox within w_maintain_cheques
integer x = 1499
integer y = 156
integer width = 293
integer height = 72
integer taborder = 70
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
boolean enabled = false
string text = "between"
end type

event clicked;Datetime ldt_server_datetime

IF cbx_between.Checked = TRUE THEN
	em_to_date.Enabled = TRUE
	em_to_date.BackColor = 16777215
	em_to_date.SetFocus()
	ldt_server_datetime = f_server_datetime()
	em_to_date.Text = String(ldt_server_datetime, "YYYY-MM-DD")
ELSE
	em_to_date.Enabled = FALSE
	em_to_date.BackColor = 67108864
	em_to_date.Text = ""
END IF

end event

type em_to_date from editmask within w_maintain_cheques
integer x = 1815
integer y = 144
integer width = 343
integer height = 96
integer taborder = 80
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
boolean enabled = false
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "yyyy-mm-dd"
string displaydata = ""
end type

type em_from_date from editmask within w_maintain_cheques
integer x = 1134
integer y = 144
integer width = 343
integer height = 96
integer taborder = 60
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "yyyy-mm-dd"
string displaydata = "H"
end type

type rb_cheque_date from radiobutton within w_maintain_cheques
integer x = 32
integer y = 184
integer width = 411
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
string text = "Cheque Date"
end type

event clicked;Datetime ldt_server_datetime

em_cheque_no.Enabled = FALSE
em_cheque_no.BackColor = 67108864
em_cheque_no.Text = ""

cbx_to.Enabled = FALSE
cbx_to.Checked = FALSE

em_to_cheque_no.Text = ""

em_from_date.Enabled = TRUE
em_from_date.BackColor = 16777215
ldt_server_datetime = f_server_datetime()
em_from_date.Text = String(ldt_server_datetime, "YYYY-MM-DD")

em_to_date.Enabled = FALSE
em_to_date.BackColor = 67108864
em_to_date.Text = ""

cbx_between.Enabled = TRUE

dw_cheque_list.Reset()
dw_cheque_details.Reset()
em_from_date.SetFocus()




end event

type rb_cheque_number from radiobutton within w_maintain_cheques
integer x = 32
integer y = 124
integer width = 498
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
string text = "Cheque Number"
end type

event clicked;em_cheque_no.Enabled = TRUE
em_cheque_no.BackColor = 16777215
em_cheque_no.Text = ""

cbx_to.Enabled = TRUE
cbx_to.Checked = FALSE

em_to_cheque_no.Text = ""

em_from_date.Enabled = FALSE
em_from_date.BackColor = 67108864
em_from_date.Text = ""

em_to_date.Enabled = FALSE
em_to_date.BackColor = 67108864
em_to_date.Text = ""

cbx_between.Enabled = FALSE
cbx_between.Checked = FALSE

dw_cheque_list.Reset()
dw_cheque_details.Reset()
em_cheque_no.SetFocus()




end event

type rb_handwritten from radiobutton within w_maintain_cheques
integer x = 32
integer y = 64
integer width = 398
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
string text = "Handwritten"
boolean checked = true
end type

event clicked;em_cheque_no.Enabled = FALSE
em_cheque_no.BackColor = 67108864
em_cheque_no.Text = ""

cbx_to.Enabled = FALSE
cbx_to.Checked = FALSE

em_to_cheque_no.Text = ""

em_from_date.Enabled = FALSE
em_from_date.BackColor = 67108864
em_from_date.Text = ""

em_to_date.Enabled = FALSE
em_to_date.BackColor = 67108864
em_to_date.Text = ""

cbx_between.Enabled = FALSE
cbx_between.Checked = FALSE


end event

type cb_reconcile from commandbutton within w_maintain_cheques
integer x = 1545
integer y = 2064
integer width = 320
integer height = 96
integer taborder = 150
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Reconcile"
end type

event clicked;Long     ll_cheque_no, ll_row_count, ll_num_rows
Integer  li_rtn
String   ls_rtn, ls_user_id, ls_reconciled_code, ls_reconciled_amount_flag
Datetime ldt_processed_date, ldt_transmit_date, ldt_reconciled_date,ldt_server_datetime
Boolean  lb_new_cheque, lb_reconciled
DataWindowChild ldwc_reconciled

ldt_server_datetime = f_server_datetime()

// Make sure there's a cheque to reconcile
ll_row_count = dw_cheque_details.RowCount()
IF ll_row_count <= 0 OR IsNull(ll_row_count) THEN
	MessageBox("No Cheque Selected", "No cheque selected to Reconcile, Select a cheque and try again.", Information!)
	dw_cheque_list.SetFocus()
	RETURN
END IF

// Get the Cheque Details
ll_cheque_no = dw_cheque_details.GetItemNumber(1, "cheque_no")
ldt_processed_date = dw_cheque_details.GetItemDateTime(1, "processed_date")
ldt_transmit_date = dw_cheque_details.GetItemDateTime(1, "transmit_date")
ldt_reconciled_date = dw_cheque_details.GetItemDateTime(1, "reconciled_date")
ls_reconciled_amount_flag = dw_cheque_details.GetItemString(1, "reconciled_amount_flag")
ls_reconciled_code = dw_cheque_details.GetItemString(1, "reconciled_code")

// Cheque must be Processed
IF IsNull(ldt_processed_date) = TRUE THEN
	MessageBox("Not Processed", "Unable to reconcile cheque number " + String(ll_cheque_no) +&
				  " because it has not been processed yet.", Information!)
	dw_cheque_list.SetFocus()
	RETURN
END IF

// Cheque must not already be reconciled
IF ls_reconciled_amount_flag = "Y" THEN
	MessageBox("Already Reconciled", "Unable to reconcile cheque number " + String(ll_cheque_no) +&
				  " because it has already been reconciled on " + String(ldt_reconciled_date, "mmm dd, yyyy") + ".", Information!)
	dw_cheque_list.SetFocus()
	RETURN
END IF

// Fill in the Reconcile columns that aren't enabled
f_user_id(ls_user_id)
dw_cheque_details.SetItem(1, "reconciled_user_id", ls_user_id)
dw_cheque_details.SetItem(1, "reconciled_amount_flag", "Y")
dw_cheque_details.SetItem(1, "reconciled_date", ldt_server_datetime)

// Enable the Reconcile fields
lb_new_cheque = FALSE
lb_reconciled = TRUE
wf_enable_dw(lb_new_cheque, lb_reconciled)

// Enable/Disable buttons
cb_save.Enabled = TRUE
cb_cancel.Enabled = TRUE
cb_reconcile.Enabled = FALSE

// If the type is Matched Cheque within $1.00 then only enable reconcile amount field
IF ls_reconciled_code = "09" THEN
	IF IsNull(ldt_transmit_date) = TRUE THEN
		MessageBox("Not Transmitted", "Unable to reconcile cheque number " + String(ll_cheque_no) +&
					  " because it has not been transmitted yet.", Information!)
		dw_cheque_list.SetFocus()
		RETURN
	END IF
	
	dw_cheque_details.SetItem(1, "reconciled_amount_flag", "Y")
	
	ls_rtn = dw_cheque_details.Modify("reconciled_amount.protect='0' reconciled_amount.background.color='16777215' reconciled_amount.border='5'")

	// User Can't change Reconciled code when its set to Matched Cheque within $1.00
	dw_cheque_details.GetChild("reconciled_code", ldwc_reconciled)
	ldwc_reconciled.SetTransObject(SQLCA)
	ldwc_reconciled.SetFilter("")
	ldwc_reconciled.Filter()
	ldwc_reconciled.SetFilter("reconciled_code=~"09~"")
	ldwc_reconciled.Filter()
ELSE
	ls_rtn = dw_cheque_details.Modify("reconciled_code.dddw.UseAsBorder=Yes")
	li_rtn = dw_cheque_details.GetChild("reconciled_code", ldwc_reconciled)
	li_rtn = ldwc_reconciled.SetTransObject(SQLCA)

	// If the Cheque hasn't been transmitted, all they can do is Replace it
	IF IsNull(ldt_transmit_date) = TRUE THEN
		ldwc_reconciled.SetFilter("reconciled_code=~"98~"")
		ldwc_reconciled.Filter()
	END IF

	// Default the Reconciled Code to first item in list
	ll_num_rows = ldwc_reconciled.RowCount()
	IF ll_num_rows > 0 THEN
		ls_reconciled_code = ldwc_reconciled.GetItemString(1, "reconciled_code")
		dw_cheque_details.SetItem(1, "reconciled_code", ls_reconciled_code)
		wf_enable_reconcile_fields(ls_reconciled_code)
	END IF
END IF

end event

type cb_search from commandbutton within w_maintain_cheques
integer x = 2286
integer y = 12
integer width = 320
integer height = 96
integer taborder = 90
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Search"
boolean default = true
end type

event clicked;Long     ll_cheque_no, ll_num_rows, ll_to_cheque_no
Integer  li_rtn
String   ls_select, ls_rtn, ls_cheque_type_code, ls_payment_method_code
Datetime ldt_from_date, ldt_to_date, ldt_min_date, ldt_max_date, ldt_transmit_date

// See if there are any unsaved changes
IF cb_save.Enabled = TRUE THEN
	li_rtn = Messagebox("Save Changes?", "A Cheque has changes made to it," +&
							  " Do you want to save it?", Question!, YesNoCancel!, 1)
	IF li_rtn = 1 THEN
		cb_save.Triggerevent(Clicked!)
		IF cb_save.Enabled = TRUE THEN
			dw_cheque_details.SetFocus()
			RETURN
		END IF
	ELSEIF li_rtn = 3 THEN
		RETURN
	END IF
END IF

//ls_rtn = dw_cheque_details.Modify("reconciled_code.background.color='67108864' reconciled_code.protect='1' reconciled_code.border='0' reconciled_code.dddw.UseAsBorder=No")
//ls_rtn = dw_cheque_details.Modify("reconciled_amount.background.color='67108864' reconciled_amount.protect='1' reconciled_amount.border='0'")
em_cheques_found.Text = "0"

// Change the SQL for the SELECT
ldt_min_date = DateTime(Date("1900-01-01"), Time("00:00:00"))
ldt_max_date = DateTime(Date("2079-06-06"), Time("00:00:00"))

/* Added NOLOCK optimizer hint as a work around for a bug in MS SQL 6.5 that caused locks to be held
on the CHEQUE_HEADER table when no cheques were returned
*/
ls_select = "SELECT CH.cheque_no, CH.source_code, CH.reconciled_date, CH.cheque_amount, CH.requisition_no, " +&
				"       CH.cheque_date, CH.processed_date, CH.printed_date, CH.transmit_date, CT.cheque_type_desc, " +&
				"       S.source_desc, CH.payment_method_code, CH.reconciled_amount_flag " +& 
				"  FROM CHEQUE_HEADER CH (NOLOCK), Cheque_Type CT, Source S " 

IF rb_handwritten.Checked = TRUE THEN
	ls_select = ls_select + " , Handwritten_Cheque_No_Range HCNR " +&
									" WHERE CH.cheque_no >= HCNR.min_handwritten_cheque_no " +&
									"   AND CH.cheque_no <= HCNR.max_handwritten_cheque_no" +&
									"   AND CH.cheque_type_code = CT.cheque_type_code " +&
									"   AND CH.source_code = S.source_code " +&
									" ORDER BY CH.cheque_no DESC "
ELSEIF rb_cheque_number.Checked = TRUE THEN
	ll_cheque_no = Long(em_cheque_no.Text)
	IF ll_cheque_no = 0 OR IsNull(ll_cheque_no) = TRUE THEN
		MessageBox("Invalid Cheque Number", "Enter a cheque number greater than zero and try again.", Information!)
		em_cheque_no.SetFocus()
		RETURN
	END IF

	IF cbx_to.Checked = TRUE THEN
		ll_to_cheque_no = Long(em_to_cheque_no.Text)
		IF ll_to_cheque_no = 0 OR IsNull(ll_to_cheque_no) = TRUE THEN
			MessageBox("Invalid To Cheque Number", "Enter a cheque number greater than zero and try again.", Information!)
			em_to_cheque_no.SetFocus()
			RETURN
		END IF
		
		IF ll_to_cheque_no < ll_cheque_no THEN
			MessageBox("Invalid To Cheque Number", "To cheque number must be greater than from cheque number.~rPlease re-enter.", Information!)
			em_to_cheque_no.SetFocus()
			RETURN
		END IF
		
		ls_select = ls_select + " WHERE CH.cheque_no >= " + String(ll_cheque_no) +&
										" AND CH.cheque_no <= " + String(ll_to_cheque_no) +&
										" AND CH.cheque_type_code = CT.cheque_type_code AND CH.source_code = S.source_code " +&
										" ORDER BY CH.cheque_no DESC "
	ELSE
		ls_select = ls_select + " WHERE CH.cheque_no = " + String(ll_cheque_no) +&
										" AND CH.cheque_type_code = CT.cheque_type_code AND CH.source_code = S.source_code " +&
										" ORDER BY CH.cheque_no DESC "
	END IF
ELSEIF rb_cheque_date.Checked = TRUE THEN
	ldt_from_date = DateTime(Date(em_from_date.Text), Time("00:00:00"))
	IF IsNull(ldt_from_date) = TRUE OR ldt_from_date <= ldt_min_date OR ldt_from_date > ldt_max_date THEN
		MessageBox("Invalid Date", "Date entered is invalid.~rPlease re-enter.", Information!)
		em_from_date.SetFocus()
		RETURN
	END IF

	IF cbx_between.Checked = TRUE THEN
		ldt_to_date = DateTime(Date(em_to_date.Text), Time("00:00:00"))
		IF IsNull(ldt_to_date) = TRUE OR ldt_to_date <= ldt_min_date OR ldt_to_date > ldt_max_date THEN
			MessageBox("Invalid Date", "Date entered is invalid.~rPlease re-enter.", Information!)
			em_to_date.SetFocus()
			RETURN
		END IF

		IF ldt_to_date < ldt_from_date THEN
			MessageBox("Invalid To Date", "The To Date (" + String(ldt_to_date, "mmm dd, yyyy") + ") can not be before " +&
						  "the From Date (" + String(ldt_from_date, "mmm dd, yyyy") + ").~rPlease re-enter.", Information!)
			em_to_date.SetFocus()
			RETURN
		END IF

		ls_select = ls_select + " WHERE CH.cheque_date >= ~"" + String(ldt_from_date, "yyyy-mm-dd") + "~"" +&
									   "   AND CH.cheque_date <= ~"" + String(ldt_to_date, "yyyy-mm-dd") + "~"" + &
										" AND CH.cheque_type_code = CT.cheque_type_code AND CH.source_code = S.source_code " +&
										" ORDER BY CH.cheque_date ASC"
	ELSE
		ls_select = ls_select + " WHERE CH.cheque_date = ~"" + String(ldt_from_date, "yyyy-mm-dd") + "~"" +&
									   " AND CH.cheque_type_code = CT.cheque_type_code AND CH.source_code = S.source_code " +&
										" ORDER BY CH.cheque_date ASC"
	END IF
END IF

ls_rtn = dw_cheque_list.Modify("DataWindow.Table.Select='" + ls_select + "'")
li_rtn = dw_cheque_list.SetTransObject(SQLCA)
ll_num_rows = dw_cheque_list.Retrieve()
li_rtn = SQLCA.nf_handle_error("w_maintain_cheques","dw_cheque_list","cb_search - dw_cheque_list.Retrieve()")
em_cheques_found.Text = String(ll_num_rows)

IF ll_num_rows = 0 OR IsNull(ll_num_rows) = TRUE THEN
	IF rb_handwritten.Checked = TRUE THEN
		MessageBox("No Cheques Found", "No Handwritten Cheques could be found.", Information!)
	ELSEIF rb_cheque_number.Checked = TRUE THEN
		MessageBox("No Cheque Found", "No Cheque could be found with cheque number = " + String(ll_cheque_no) + ".", Information!)
	ELSEIF rb_cheque_date.Checked = TRUE THEN
		IF cbx_between.Checked = TRUE THEN
			MessageBox("No Cheques Found", "No Cheques could be found with cheque date >= " + String(ldt_from_date, "mmm dd, yyyy") +&
						  " and <= " + String(ldt_to_date, "mmm dd, yyyy") + ".", Information!)
		ELSE
			MessageBox("No Cheques Found", "No Cheques could be found with cheque date = " +&
						  String(ldt_from_date, "mmm dd, yyyy") + ".", Information!)
		END IF
	END IF
ELSE
	dw_cheque_list.ScrollToRow(1)
	dw_cheque_list.SelectRow(0, FALSE)
	dw_cheque_list.SelectRow(1, TRUE)

	ll_cheque_no = dw_cheque_list.GetItemNumber(1, "cheque_no")
	wf_get_cheque_details(ll_cheque_no)
END IF
	
end event

type cb_cancel from commandbutton within w_maintain_cheques
integer x = 1198
integer y = 2064
integer width = 320
integer height = 96
integer taborder = 140
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Ca&ncel"
end type

event clicked;Long     ll_num_rows, ll_cheque_no, ll_found_row
Integer  li_rtn
String   ls_rtn

cb_add.Enabled = TRUE
cb_reconcile.Enabled = TRUE
cb_save.Enabled = FALSE
cb_cancel.Enabled = FALSE
cb_search.Default = TRUE

ll_cheque_no = dw_cheque_details.GetItemNumber(1, "cheque_no")

// Refresh the List DW
ll_num_rows = dw_cheque_list.Retrieve()
li_rtn = SQLCA.nf_handle_error("w_maintain_cheques","dw_cheque_list","cb_cancel - dw_cheque_list.Retrieve()")

ll_found_row = dw_cheque_list.Find("cheque_no=" + String(ll_cheque_no), 1, ll_num_rows)
IF ll_found_row > 0 THEN
	dw_cheque_list.ScrollToRow(ll_found_row)
	dw_cheque_list.SelectRow(0, FALSE)
	dw_cheque_list.SelectRow(ll_found_row, TRUE)
ELSE
	IF ll_num_rows > 0 THEN
		ll_cheque_no = dw_cheque_list.GetItemNumber(1, "cheque_no")
		dw_cheque_list.ScrollToRow(1)
		dw_cheque_list.SelectRow(0, FALSE)
		dw_cheque_list.SelectRow(1, TRUE)
	END IF
END IF

// Refresh the Details DW
wf_get_cheque_details(ll_cheque_no)
ls_rtn = dw_cheque_details.Modify("reconciled_code.dddw.UseAsBorder=No")
ls_rtn = dw_cheque_details.Modify("reconciled_code.protect='1'       reconciled_code.background.color='67108864'       reconciled_code.border='0' reconciled_code.dddw.UseAsBorder=No")
ls_rtn = dw_cheque_details.Modify("reconciled_amount.protect='1'     reconciled_amount.background.color='67108864'     reconciled_amount.border='0'")
ls_rtn = dw_cheque_details.Modify("replacement_cheque_no.protect='1' replacement_cheque_no.background.color='67108864' replacement_cheque_no.border='0'")

end event

type cb_save from commandbutton within w_maintain_cheques
integer x = 1893
integer y = 2064
integer width = 320
integer height = 96
integer taborder = 160
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Save"
boolean default = true
end type

event clicked;Integer  li_rtn
Long     ll_row, ll_cheque_no, ll_temp, ll_days, ll_requisition_no, ll_color
Long     ll_row_count, ll_num_rows, ll_found_row, ll_replacement_cheque_no
Decimal  ld_cheque_amount, ld_reconciled_amount, ld_bank_amount
String   ls_name_on_cheque, ls_rtn, ls_reconciled_code, ls_select
String   ls_repl_reconciled_amount_flag, ls_repl_cheque_type_code, ls_repl_cheque_type_desc
String   ls_original_reconciled_code, ls_payment_method_code
String   ls_original_reconciled_desc
Datetime ldt_cheque_date, ldt_server_datetime, ldt_transmit_date, ldt_printed_date
Boolean  lb_new_row, lb_being_reconciled

// Make sure theres something to save
ll_row_count = dw_cheque_details.RowCount()
IF ll_row_count = 0 OR IsNull(ll_row_count) THEN
	RETURN
END IF

// Accept the Data
li_rtn = dw_cheque_details.AcceptText()
IF li_rtn = -1 THEN 
	RETURN
END IF

// See if its a New Row
IF dw_cheque_details.GetItemStatus(1, 0, Primary!) = New! OR &
   dw_cheque_details.GetItemStatus(1, 0, Primary!) = NewModified! THEN
	lb_new_row = TRUE
ELSE
	lb_new_row = FALSE
END IF

// If its a new row do some Validations (ie. white background)
IF lb_new_row = TRUE THEN
	// Validate Cheque Number
	ll_cheque_no = dw_cheque_details.GetItemNumber(1, "cheque_no")

	IF ll_cheque_no < il_min_handwritten_cheque_no OR ll_cheque_no > il_max_handwritten_cheque_no OR IsNull(ll_cheque_no) THEN
		MessageBox("Invalid Cheque Number", "Cheque Number must be between " + String(il_min_handwritten_cheque_no) +&
					  " and " + String(il_max_handwritten_cheque_no) + ".~rPlease re-enter.", Information!)
		dw_cheque_details.SetFocus()
		dw_cheque_details.SelectText(1, Len(String(ll_cheque_no)))
		RETURN 1
	END IF

	// Make sure cheque doesn't exist in CHEQUE_HEADER or UNAPPLIED_CLAIM_TXN tables
	SELECT cheque_no 
	  INTO :ll_temp
	  FROM CHEQUE_HEADER 
	 WHERE cheque_no = :ll_cheque_no ;
	 
	li_rtn = SQLCA.nf_handle_error("w_maintain_cheques","dw_cheque_details","cb_save - SELECT cheque_no FROM CHEQUE_HEADER")
	IF li_rtn = 0 THEN
		MessageBox("Cheque Number Already Exists", "Cheque Number " + String(ll_cheque_no) + " already exists.~rChoose a " +&
					  "different cheque number and try again.", Information!)
		dw_cheque_details.SetColumn("cheque_no")
		dw_cheque_details.SetFocus()
		dw_cheque_details.SelectText(1, Len(String(ll_cheque_no)))
		RETURN
	END IF

   SELECT cheque_no 
	  INTO :ll_temp
	  FROM UNAPPLIED_CLAIM_TXN  
	 WHERE cheque_no = :ll_cheque_no ;

	li_rtn = SQLCA.nf_handle_error("w_maintain_cheques","dw_cheque_details","itemchanged - SELECT cheque_no FROM CHEQUE_HEADER")
	IF li_rtn = 0 THEN
		MessageBox("Cheque Number Already Exists", "Cheque Number " + String(ll_cheque_no) + " already exists in the " +&
					  "UNAPPLIED_CLAIM_TXN table.~rChoose a different cheque number and try again.", Information!)
		dw_cheque_details.SetFocus()
		dw_cheque_details.SelectText(1, Len(String(ll_cheque_no)))
		RETURN 1
	END IF
	
	// Validate Name on Cheque
	ls_name_on_cheque = dw_cheque_details.GetItemString(1, "name_on_cheque")
	IF ls_name_on_cheque = "" OR IsNull(ls_name_on_cheque) = TRUE THEN
		MessageBox("Name must be entered", "Name on Cheque must be entered.  Please enter.", Information!)
		dw_cheque_details.SetColumn("name_on_cheque")
		dw_cheque_details.SetFocus()
		dw_cheque_details.SelectText(1, Len(ls_name_on_cheque))
		RETURN
	END IF
	
	// Validate Cheque Amount
	ld_cheque_amount = dw_cheque_details.GetItemDecimal(1, "cheque_amount")
	IF ld_cheque_amount <= 0 OR IsNull(ld_cheque_amount) THEN
		MessageBox("Invalid Cheque Amount", "Cheque Amount must be greater than zero.~rPlease re-enter.", Information!)
		dw_cheque_details.SetColumn("cheque_amount")
		dw_cheque_details.SetFocus()
		dw_cheque_details.SelectText(1, Len(String(ld_cheque_amount)))
		RETURN
	END IF

	// Validate Cheque Date - Can't be in future or 30 days in past
	ldt_cheque_date = dw_cheque_details.GetItemDatetime(1, "cheque_date")
	IF IsNull(ldt_cheque_date) = TRUE THEN
		MessageBox("Invalid Cheque Date", "Cheque Date must be entered.~rPlease re-enter.", Information!)
		dw_cheque_details.SetColumn("cheque_date")
		dw_cheque_details.SetFocus()
		dw_cheque_details.SelectText(1, Len(String(ldt_cheque_date)))
		RETURN
	END IF
	
	ldt_server_datetime = f_server_datetime()
	ll_days = DaysAfter(Date(ldt_cheque_date), Date(ldt_server_datetime))
	IF ll_days < 0 THEN
		MessageBox("Invalid Cheque Date", "Cheque Date must be in the past.  You've entered a cheque date, " +&
					  String(ldt_cheque_date, "mmm dd, yyyy") + ", thats " + String(Abs(ll_days)) + " days in the future.~r" +&
					  "Enter a date in the past and try again.", Information!)
		dw_cheque_details.SetColumn("cheque_date")
		dw_cheque_details.SetFocus()
		dw_cheque_details.SelectText(1, Len(String(ldt_cheque_date)))
		RETURN
	END IF

	IF ll_days > 30 THEN
		MessageBox("Invalid Cheque Date", "Cheque Date must be within 30 days of today.  You've entered a cheque date, " +&
					  String(ldt_cheque_date, "mmm dd, yyyy") + ", thats " + String(Abs(ll_days)) + " days in the past.~r" +&
					  "Enter a date within the last 30 days and try again.", Information!)
		dw_cheque_details.SetColumn("cheque_date")
		dw_cheque_details.SetFocus()
		dw_cheque_details.SelectText(1, Len(String(ldt_cheque_date)))
		RETURN
	END IF

	// Validate Requisition Number
	ll_requisition_no = dw_cheque_details.GetItemNumber(1, "requisition_no")
	IF ll_requisition_no <= 0 OR IsNull(ll_requisition_no) THEN
		MessageBox("Invalid Requisition Number", "Requisition Number must be greater than zero.~rPlease re-enter.", Information!)
		dw_cheque_details.SetColumn("requisition_no")
		dw_cheque_details.SetFocus()
		dw_cheque_details.SelectText(1, Len(String(ll_requisition_no)))
		RETURN
	END IF
END IF

// See if the cheque is being reconciled 
ll_color = Long(dw_cheque_details.Describe("reconciled_code.background.color"))
IF ll_color = 16777215 THEN
	lb_being_reconciled = TRUE
ELSE
	ll_color = Long(dw_cheque_details.Describe("reconciled_amount.background.color"))
	IF ll_color = 16777215 THEN
		lb_being_reconciled = TRUE
	ELSE
		lb_being_reconciled = FALSE
	END IF
END IF

// Do some validations if its being reconciled
IF lb_being_reconciled = TRUE THEN
	ls_original_reconciled_code = dw_cheque_details.GetItemString(1, "reconciled_code", Primary!, TRUE)
	
	ll_cheque_no = dw_cheque_details.GetItemNumber(1, "cheque_no")
	ldt_transmit_date = dw_cheque_details.GetItemDatetime(1, "transmit_date")
	ldt_printed_date = dw_cheque_details.GetItemDatetime(1, "printed_date")
	ls_payment_method_code = dw_cheque_details.GetItemString(1, "payment_method_code")
	ls_reconciled_code = dw_cheque_details.GetItemString(1, "reconciled_code")
	IF ls_reconciled_code = "" OR IsNull(ls_reconciled_code) = TRUE THEN
		MessageBox("Invalid Reconcile Type", "Reconcile Type must be selected.~rPlease chooose one and try again.", Information!)
		dw_cheque_details.SetColumn("reconciled_code")
		dw_cheque_details.SetFocus()
		dw_cheque_details.SelectText(1, Len(ls_reconciled_code))
		RETURN
	END IF

	// If Reconciled type is Manually Reconcile then check the Reconciled Amount
	ld_reconciled_amount = dw_cheque_details.GetItemDecimal(1, "reconciled_amount")
	IF ls_reconciled_code = "21" AND (ld_reconciled_amount = 0 OR IsNull(ld_reconciled_amount)) THEN
		MessageBox("Invalid Reconcile Amount", "Reconcile Amount must be entered.~rPlease re-enter.", Information!)
		dw_cheque_details.SetColumn("reconciled_amount")
		dw_cheque_details.SetFocus()
		dw_cheque_details.SelectText(1, Len(String(ld_reconciled_amount)))
		RETURN
	END IF

	// Do some checks for Reconciled Type: Stop Payment
	IF ls_reconciled_code = "20" THEN
		// Printed Date must not be null for automated cheques
		IF IsNull(ldt_printed_date) = TRUE AND ls_payment_method_code = "A" THEN
			MessageBox("Cheque not Printed", "Cheque Number " + String(ll_cheque_no) + " has not been printed.~r" +&
						  "A Stop Payment can only be done on cheques that are printed.", Information!)
			dw_cheque_details.SetColumn("reconciled_code")
			dw_cheque_details.SetFocus()
			RETURN
		END IF

		// Transmit Date must Not be null
		IF IsNull(ldt_transmit_date) = TRUE THEN
			MessageBox("Cheque Not Transmitted", "Cheque Number " + String(ll_cheque_no) + " has not been " +&
						  "transmitted to the bank.~rA Stop Payment can only be done on cheques that have been " +&
						  "transmitted to the bank.", Information!)
			dw_cheque_details.SetColumn("reconciled_code")
			dw_cheque_details.SetFocus()
			RETURN
		END IF			
	END IF

	// Do some checks for Reconciled Type: Replacement Cheque
	IF ls_reconciled_code = "98" THEN
		ll_replacement_cheque_no = dw_cheque_details.GetItemNumber(1, "replacement_cheque_no")
		IF ll_replacement_cheque_no = 0 OR IsNull(ll_replacement_cheque_no) THEN
			MessageBox("Invalid Replacement Cheque Number", "Replacement Cheque Number must be entered.~rPlease re-enter.", Information!)
			dw_cheque_details.SetColumn("replacement_cheque_no")
			dw_cheque_details.SetFocus()
			dw_cheque_details.SelectText(1, Len(String(ll_replacement_cheque_no)))
			RETURN
		END IF

		// Replacement Cheque Number must exist in CHEQUE_HEADER
		SELECT CH.cheque_no, CH.reconciled_amount_flag, CH.cheque_type_code, CT.cheque_type_desc
		  INTO :ll_temp, :ls_repl_reconciled_amount_flag, :ls_repl_cheque_type_code, :ls_repl_cheque_type_desc
		  FROM CHEQUE_HEADER CH, Cheque_Type CT
		 WHERE cheque_no = :ll_replacement_cheque_no 
		   AND CH.cheque_type_code = CT.cheque_type_code ;

		li_rtn = SQLCA.nf_handle_error("w_maintain_cheques","dw_cheque_details","cb_save - SELECT cheque_no FROM CHEQUE_HEADER WHERE cheque_no = :ll_replacement_cheque_no")
		IF li_rtn = 100 THEN
			MessageBox("Invalid Replacement Cheque Number", "Cheque Number " +&
						  String(ll_replacement_cheque_no) + " has not been entered into the system " +&
						  "yet.~rEnter cheque number " + String(ll_replacement_cheque_no) +&
						  " into the system and then try reconciling cheque number " + String(ll_cheque_no) +&
						  " again.", Information!)
			dw_cheque_details.SetColumn("replacement_cheque_no")
			dw_cheque_details.SetFocus()
			dw_cheque_details.SelectText(1, Len(String(ll_replacement_cheque_no)))
			RETURN
		END IF

		// Replacement Cheque must be of type "Other"
		IF ls_repl_cheque_type_code <> "OTH" THEN
			MessageBox("Invalid Replacement Cheque Number", "Cheque Number " +&
						  String(ll_replacement_cheque_no) + " is not an Other cheque type.~r" +&
						  "It is of type " + ls_repl_cheque_type_desc + ".~rSelect a Replacement " +&
						  "Cheque of type Other and try again.", Information!)
			dw_cheque_details.SetColumn("replacement_cheque_no")
			dw_cheque_details.SetFocus()
			dw_cheque_details.SelectText(1, Len(String(ll_replacement_cheque_no)))
			RETURN
		END IF

		// Replacement Cheque must not be reconciled
		IF ls_repl_reconciled_amount_flag = "Y" THEN
			MessageBox("Invalid Replacement Cheque Number", "Cheque Number " +&
						  String(ll_replacement_cheque_no) + " has already been reconciled.~r" +&
						  "Select a Replacement Cheque that has not been reconciled and try again.", Information!)
			dw_cheque_details.SetColumn("replacement_cheque_no")
			dw_cheque_details.SetFocus()
			dw_cheque_details.SelectText(1, Len(String(ll_replacement_cheque_no)))
			RETURN
		END IF			

		// Printed Date must not be null for automated cheques
		IF IsNull(ldt_printed_date) = TRUE AND ls_payment_method_code = "A" THEN
			MessageBox("Cheque not Printed", "Cheque Number " + String(ll_cheque_no) + " has not been printed.~r" +&
						  "Cheques can only be replaced if they are printed.", Information!)
			dw_cheque_details.SetColumn("reconciled_code")
			dw_cheque_details.SetFocus()
			RETURN
		END IF

		// Transmit Date must be null
		IF IsNull(ldt_transmit_date) = FALSE THEN
			MessageBox("Cheque already Transmitted", "Cheque Number " + String(ll_cheque_no) + " has already been " +&
						  "transmitted to the bank.~rCheques that have already been transmitted can not be replaced.", Information!)
			dw_cheque_details.SetColumn("reconciled_code")
			dw_cheque_details.SetFocus()
			RETURN
		END IF			
	END IF

	// Reconciled code = 09 (Matched Cheque withing $1.00)
	IF ls_original_reconciled_code = "09" THEN
		// Can't be changed to another code
		IF ls_reconciled_code <> "09" THEN
			SELECT reconciled_desc
			  INTO :ls_original_reconciled_desc 
			  FROM Reconciled
			 WHERE reconciled_code = :ls_original_reconciled_code ;
			
			li_rtn = SQLCA.nf_handle_error("w_maintain_cheques","dw_cheque_list","cb_save - SELECT reconciled_desc FROM Reconciled")
			
			MessageBox("Reconciled Type can't change", "Reconcile Type can't be changed to another Reconciled Type " +&
						  "once it is set to " + ls_original_reconciled_desc + ".~rHit Save and Try Again.", Information!)
			dw_cheque_details.SetColumn("reconciled_code")
			dw_cheque_details.SetItem(1, "reconciled_code", "09")
			dw_cheque_details.SetFocus()
			dw_cheque_details.SelectText(1, Len(ls_reconciled_code))
			RETURN
		END IF
	END IF

	IF ls_reconciled_code = "09" THEN
		// The reconciled amount must be equal to cheque_amount or bank_amount
		ld_bank_amount = dw_cheque_details.GetItemDecimal(1, "bank_amount")
		ld_cheque_amount = dw_cheque_details.GetItemDecimal(1, "cheque_amount")
		IF ld_reconciled_amount <> ld_bank_amount AND ld_reconciled_amount <> ld_cheque_amount THEN
			MessageBox("Invalid Reconciled Amount", "Reconcile Amount must be equal to the " +&
						  "Bank Amount: " + String(ld_bank_amount, "$#,###.00") + " or the " +&
						  "Cheque Amount: " + String(ld_cheque_amount, "$#,###.00") +&
						  ".~rPlease re-enter.", Information!)
			dw_cheque_details.SetColumn("reconciled_amount")
			dw_cheque_details.SetFocus()
			dw_cheque_details.SelectText(1, Len(String(ld_reconciled_amount)))
			RETURN
		END IF
	END IF
	
	ls_rtn = dw_cheque_details.Modify("reconciled_code.dddw.UseAsBorder=No")
END IF


SQLCA.nf_begin_transaction()

// Write data to Cheque Stub DW
IF lb_new_row = TRUE THEN
	ll_row = dw_cheque_stub.InsertRow(0)
	dw_cheque_stub.SetItem(ll_row, "cheque_no", ll_cheque_no)
	dw_cheque_stub.SetItem(ll_row, "seq_no", 1)
	dw_cheque_stub.SetItem(ll_row, "line_detail", "")
	dw_cheque_stub.SetItem(ll_row, "amount", String(ld_cheque_amount, "$#,###,##0.00"))
	dw_cheque_stub.AcceptText()
	li_rtn = dw_cheque_stub.Update()
	li_rtn = SQLCA.nf_handle_error("w_maintain_cheques","dw_cheque_stub","cb_save - dw_cheque_stub.Update()")
END IF

// Save cheque to CHEQUE_HEADER
li_rtn = dw_cheque_details.Update()
li_rtn = SQLCA.nf_handle_error("w_maintain_cheques","dw_cheque_details","cb_save - dw_cheque_details.Update()")

SQLCA.nf_commit_transaction()


cb_cancel.Enabled = FALSE
cb_save.Enabled = FALSE
cb_add.Enabled = TRUE
cb_reconcile.Enabled = TRUE
cb_search.Default = TRUE

// Refresh the List DW
ll_cheque_no = dw_cheque_details.GetItemNumber(1, "cheque_no")
ll_num_rows = dw_cheque_list.Retrieve()
li_rtn = SQLCA.nf_handle_error("w_maintain_cheques","dw_cheque_details","cb_save - dw_cheque_list.Retrieve()")

ll_found_row = dw_cheque_list.Find("cheque_no=" + String(ll_cheque_no), 1, ll_num_rows)
IF ll_found_row > 0 THEN
	// Reselect the row if the cheque is displayed in the list
	dw_cheque_list.ScrollToRow(ll_found_row)
	dw_cheque_list.SelectRow(0, FALSE)
	dw_cheque_list.SelectRow(ll_found_row, TRUE)
ELSE
	// Refresh the cheque list and see if the cheque number appears
	IF ll_found_row > 0 THEN
		dw_cheque_list.ScrollToRow(ll_found_row)
		dw_cheque_list.SelectRow(0, FALSE)
		dw_cheque_list.SelectRow(ll_found_row, TRUE)
	ELSE
		// If its still not found then change the select so that its found
		ls_select = "SELECT CH.cheque_no, CH.source_code, CH.reconciled_date, CH.cheque_amount, CH.requisition_no, " +&
						"       CH.cheque_date, CH.processed_date, CH.printed_date, CH.transmit_date, CT.cheque_type_desc, " +&
						"       S.source_desc, CH.payment_method_code " +& 
						"  FROM CHEQUE_HEADER CH, Cheque_Type CT, Source S " +&
						" WHERE CH.cheque_no = " + String(ll_cheque_no) +&
						"   AND CH.cheque_type_code = CT.cheque_type_code " +&
						"   AND CH.source_code = S.source_code "
		ls_rtn = dw_cheque_list.Modify("DataWindow.Table.Select='" + ls_select + "'")
		li_rtn = dw_cheque_list.SetTransObject(SQLCA)
		ll_num_rows = dw_cheque_list.Retrieve()
		li_rtn = SQLCA.nf_handle_error("w_maintain_cheques","dw_cheque_list","cb_search - dw_cheque_list.Retrieve()")
		IF ll_num_rows > 0 THEN
			dw_cheque_list.ScrollToRow(1)
			dw_cheque_list.SelectRow(0, FALSE)
			dw_cheque_list.SelectRow(1, TRUE)
		END IF 
	END IF
END IF

// Refresh the Details DW
IF dw_cheque_list.FilteredCount() > 0 THEN
	dw_cheque_list.Reset()
	dw_cheque_list.Retrieve()
END IF

wf_get_cheque_details(ll_cheque_no)
ls_rtn = dw_cheque_details.Modify("replacement_cheque_no.protect='1' replacement_cheque_no.background.color='67108864' replacement_cheque_no.border='0'")
ls_rtn = dw_cheque_details.Modify("reconciled_amount.protect='1'     reconciled_amount.background.color='67108864'     reconciled_amount.border='0'")

end event

type em_cheque_no from editmask within w_maintain_cheques
integer x = 1134
integer y = 36
integer width = 343
integer height = 96
integer taborder = 30
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
boolean enabled = false
alignment alignment = right!
borderstyle borderstyle = stylelowered!
string mask = "#########"
string displaydata = ""
end type

type st_1 from statictext within w_maintain_cheques
integer y = 824
integer width = 416
integer height = 60
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
boolean enabled = false
string text = "Cheque Details:"
boolean focusrectangle = false
end type

type dw_cheque_details from u_dw_online within w_maintain_cheques
integer y = 884
integer width = 2706
integer height = 1156
integer taborder = 110
string dataobject = "d_cheque_details"
borderstyle borderstyle = stylelowered!
end type

event editchanged;call super::editchanged;cb_save.Enabled = TRUE
cb_cancel.Enabled = TRUE
end event

event itemchanged;call super::itemchanged;Long     ll_cheque_no, ll_temp, ll_days, ll_requisition_no, ll_child_row
Integer  li_rtn
Decimal  ld_cheque_amount
String   ls_name_on_cheque, ls_message, ls_rtn, ls_cheque_type_code, ls_generated_method_code
Datetime ldt_cheque_date, ldt_server_datetime
DATAWINDOWCHILD ldwc_child

IF dwo.Name = "cheque_no" THEN
	ll_cheque_no = Long(data)

	IF ll_cheque_no < il_min_handwritten_cheque_no OR ll_cheque_no > il_max_handwritten_cheque_no OR IsNull(ll_cheque_no) THEN
		MessageBox("Invalid Cheque Number", "Cheque Number must be between " + String(il_min_handwritten_cheque_no) +&
					  " and " + String(il_max_handwritten_cheque_no) + ".~rPlease re-enter.", Information!)
		dw_cheque_details.SetFocus()
		dw_cheque_details.SelectText(1, Len(String(ll_cheque_no)))
		RETURN 1
	END IF

	// See if cheque # already exists in CHEQUE_HEADER or UNAPPLIED_CLAIM_TXN
	SELECT cheque_no 
	  INTO :ll_temp
	  FROM CHEQUE_HEADER 
	 WHERE cheque_no = :ll_cheque_no ;

	li_rtn = SQLCA.nf_handle_error("w_maintain_cheques","dw_cheque_details","itemchanged - SELECT cheque_no FROM CHEQUE_HEADER")
	IF li_rtn = 0 THEN
		MessageBox("Cheque Number Already Exists", "Cheque Number " + String(ll_cheque_no) + " already exists.~rChoose a " +&
					  "different cheque number and try again.", Information!)
		dw_cheque_details.SetFocus()
		dw_cheque_details.SelectText(1, Len(String(ll_cheque_no)))
		RETURN 1
	END IF
	
	 SELECT cheque_no 
	   INTO :ll_temp
	   FROM UNAPPLIED_CLAIM_TXN  
	  WHERE cheque_no = :ll_cheque_no ;

	li_rtn = SQLCA.nf_handle_error("w_maintain_cheques","dw_cheque_details","itemchanged - SELECT cheque_no FROM CHEQUE_HEADER")
	IF li_rtn = 0 THEN
		MessageBox("Cheque Number Already Exists", "Cheque Number " + String(ll_cheque_no) + " already exists in the " +&
					  "UNAPPLIED_CLAIM_TXN table.~rChoose a different cheque number and try again.", Information!)
		dw_cheque_details.SetFocus()
		dw_cheque_details.SelectText(1, Len(String(ll_cheque_no)))
		RETURN 1
	END IF
//S.Manzer SR70 - Special Survivor Payment $80,000.00	- JAN 2001	
ELSEIF dwo.Name = "cheque_type_code" THEN
	ls_cheque_type_code = data
	IF ls_cheque_type_code = 'OTH' THEN
		dw_cheque_details.SetItem(1,"cheque_amount",0.00)
		ls_rtn = dw_cheque_details.Modify("cheque_amount.protect='0' cheque_amount.background.color='16777215' cheque_amount.border='5'")
	END IF	
   IF ls_cheque_type_code = 'SP' THEN
		dw_cheque_details.SetItem(1,"cheque_amount",80000.00)
      ls_rtn = dw_cheque_details.Modify("cheque_amount.protect='1'  cheque_amount.background.color='67108864'  cheque_amount.border='0'") 
	END IF
	RETURN 
ELSEIF dwo.Name = "name_on_cheque" THEN	
	ls_name_on_cheque = data
	IF ls_name_on_cheque = "" OR IsNull(ls_name_on_cheque) = TRUE THEN
		MessageBox("Name must be entered", "Name on Cheque must be entered.  Please enter.", Information!)
		dw_cheque_details.SetFocus()
		dw_cheque_details.SelectText(1, Len(ls_name_on_cheque))
		RETURN 1
	END IF
ELSEIF dwo.Name = "cheque_amount" THEN	
	ld_cheque_amount = Double(data)
	IF ld_cheque_amount <= 0 OR IsNull(ld_cheque_amount) THEN
		MessageBox("Invalid Cheque Amount", "Cheque Amount must be greater than zero.~rPlease re-enter.", Information!)
		dw_cheque_details.SetFocus()
		dw_cheque_details.SelectText(1, Len(String(ld_cheque_amount)))
		RETURN 1
	END IF
ELSEIF dwo.Name = "cheque_date" THEN
	ldt_server_datetime = f_server_datetime()
	ldt_cheque_date = Datetime(Date(Left(data, 10)))
	IF IsNull(ldt_cheque_date) = TRUE THEN
		MessageBox("Invalid Cheque Date", "Cheque Date must be entered.~rPlease re-enter.", Information!)
		dw_cheque_details.SetColumn("cheque_date")
		dw_cheque_details.SetFocus()
		dw_cheque_details.SelectText(1, Len(String(ldt_cheque_date)))
		RETURN 1
	END IF
	
	ll_days = DaysAfter(Date(ldt_cheque_date), Date(ldt_server_datetime))
	IF ll_days < 0 THEN
		MessageBox("Invalid Cheque Date", "Cheque Date must be in the past.  You've entered a cheque date, " +&
					  String(ldt_cheque_date, "mmm dd, yyyy") + ", thats " + String(Abs(ll_days)) + " days in the future.~r" +&
					  "Enter a date in the past and try again.", Information!)
		dw_cheque_details.SetColumn("cheque_date")
		dw_cheque_details.SetFocus()
		dw_cheque_details.SelectText(1, Len(String(ldt_cheque_date)))
		RETURN
	END IF

	IF ll_days > 30 THEN
		MessageBox("Invalid Cheque Date", "Cheque Date must be within 30 days of today.  You've entered a cheque date, " +&
					  String(ldt_cheque_date, "mmm dd, yyyy") + ", thats " + String(Abs(ll_days)) + " days in the past.~r" +&
					  "Enter a date within the last 30 days and try again.", Information!)
		dw_cheque_details.SetColumn("cheque_date")
		dw_cheque_details.SetFocus()
		dw_cheque_details.SelectText(1, Len(String(ldt_cheque_date)))
		RETURN
	END IF

ELSEIF dwo.Name = "requisition_no" THEN	
	ll_requisition_no = Long(data)
	IF ll_requisition_no <= 0 OR IsNull(ll_requisition_no) THEN
		MessageBox("Invalid Requisition Number", "Requisition Number must be greater than zero.~rPlease re-enter.", Information!)
		dw_cheque_details.SetColumn("requisition_no")
		dw_cheque_details.SetFocus()
		dw_cheque_details.SelectText(1, Len(String(ll_requisition_no)))
		RETURN
	END IF
ELSEIF dwo.Name = "name_on_cheque" THEN	
	ll_requisition_no = Long(data)
	IF ll_requisition_no = 0 OR IsNull(ll_requisition_no) THEN
		MessageBox("Invalid Requisition Number", "Requisition Number must be entered.~rPlease re-enter.", Information!)
		dw_cheque_details.SetFocus()
		dw_cheque_details.SelectText(1, Len(String(ll_requisition_no)))
		RETURN 1
	END IF
ELSEIF dwo.Name = "reconciled_code" THEN
	wf_enable_reconcile_fields(data)
END IF

end event

type cb_add from commandbutton within w_maintain_cheques
integer x = 503
integer y = 2064
integer width = 320
integer height = 96
integer taborder = 120
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Add"
end type

event clicked;Long     ll_row
Integer  li_rtn
Datetime ldt_null, ldt_server_datetime
String   ls_rtn
Boolean  lb_new_cheque, lb_reconciled

// See if there are any changes
IF cb_save.Enabled = TRUE THEN
	li_rtn = Messagebox("Save Changes?", "A Cheque has changes made to it," +&
							  " Do you want to save it?", Question!, YesNoCancel!, 1)
	IF li_rtn = 1 THEN
		cb_save.Triggerevent(Clicked!)
		IF cb_save.Enabled = TRUE THEN
			dw_cheque_details.SetFocus()
			RETURN
		END IF
	ELSEIF li_rtn = 2 THEN
		cb_save.Enabled = FALSE
	ELSEIF li_rtn = 3 THEN
		RETURN
	END IF
END IF

// Reset everything and add a new row
dw_cheque_list.Reset()
dw_cheque_details.Reset()
ll_row = dw_cheque_details.InsertRow(0)
cb_add.Enabled = FALSE
cb_reconcile.Enabled = FALSE
cb_save.Default = TRUE
SetNull(ldt_null)

cb_cancel.Enabled = TRUE
cb_save.Enabled = TRUE

ldt_server_datetime = f_server_datetime()
ldt_server_datetime = Datetime(Date(ldt_server_datetime), Time("00:00:00"))

dw_cheque_details.SetItem(ll_row, "cheque_batch_no", 0)
//dw_cheque_details.SetItem(ll_row, "cheque_type_code", "OTH")

dw_cheque_details.SetItem(ll_row, "source_code", "B")
dw_cheque_details.SetItem(ll_row, "payment_method_code", "H")
dw_cheque_details.SetItem(ll_row, "stub_line_count", 1)
dw_cheque_details.SetItem(ll_row, "processed_date", ldt_server_datetime)
dw_cheque_details.SetItem(ll_row, "printed_date", ldt_null)
dw_cheque_details.SetItem(ll_row, "transmit_date", ldt_null)
dw_cheque_details.SetItem(ll_row, "reconciled_date", ldt_null)
dw_cheque_details.SetItem(ll_row, "reconciled_user_id", "")
dw_cheque_details.SetItem(ll_row, "reconciled_code", "")
dw_cheque_details.SetItem(ll_row, "reconciled_amount", 0)
dw_cheque_details.SetItem(ll_row, "cheque_date", ldt_server_datetime)
dw_cheque_details.SetItem(ll_row, "benefit_class_code", "")
dw_cheque_details.SetItem(ll_row, "bank_amount", 0)
dw_cheque_details.SetItem(ll_row, "reconciled_amount_flag", "N")

lb_new_cheque = TRUE
lb_reconciled = FALSE
wf_enable_dw(lb_new_cheque, lb_reconciled)

dw_cheque_details.Modify("cheque_no.protect='0'      cheque_no.background.color='16777215'      cheque_no.border='5'")

li_rtn = dw_cheque_details.SetColumn("cheque_no")
li_rtn = dw_cheque_details.SetFocus()

ls_rtn = dw_cheque_details.Modify("reconciled_code.protect='1'       reconciled_code.background.color='67108864'       reconciled_code.border='0' reconciled_code.dddw.UseAsBorder=No")
ls_rtn = dw_cheque_details.Modify("reconciled_amount.protect='1'     reconciled_amount.background.color='67108864'     reconciled_amount.border='0'")
ls_rtn = dw_cheque_details.Modify("replacement_cheque_no.protect='1' replacement_cheque_no.background.color='67108864' replacement_cheque_no.border='0'")

end event

type dw_cheque_list from u_dw_online within w_maintain_cheques
integer y = 292
integer width = 2706
integer height = 516
integer taborder = 100
string dataobject = "d_cheque_list"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event rowfocuschanged;call super::rowfocuschanged;Long     ll_cheque_no, ll_num_rows
Integer  li_rtn

// See if there are any changes
IF cb_save.Enabled = TRUE THEN
	li_rtn = Messagebox("Save Changes?", "A change has been made but not saved.~r" +&
							  "Do you want to save it?",Question!, YesNo!, 1)
	IF li_rtn = 1 THEN
		cb_save.Triggerevent(Clicked!)
		IF cb_save.Enabled = TRUE THEN
			dw_cheque_details.SetFocus()
			RETURN
		END IF
	ELSEIF li_rtn = 2 THEN
		cb_save.Enabled = FALSE
	END IF
END IF

IF currentrow = 0 OR IsNull(currentrow) = TRUE OR This.RowCount() = 0 THEN
	RETURN
END IF
This.SelectRow(0, FALSE)
This.SelectRow(currentrow, TRUE)

ll_cheque_no = dw_cheque_list.GetItemNumber(currentrow, "cheque_no")
wf_get_cheque_details(ll_cheque_no)
end event

event rbuttondown;M_DW_ONLINE_RMB_POPUP lm_popup

	lm_popup = Create m_dw_online_rmb_popup
	lm_popup.mf_set_datawindow(This)
	lm_popup.m_options.m_filterlist.visible = true
	
	lm_popup.m_options.PopMenu(w_frame.PointerX( ), w_frame.PointerY( ))

	Destroy lm_popup
end event

event ue_filter;call super::ue_filter;S_WINDOW_MESSAGE ls_message

dw_cheque_list.SetFilter('')
dw_cheque_list.Filter()

Open(w_cheque_filter)

ls_message = Message.PowerObjectParm

is_row_filter = ls_message.as_stringparm[1]

// Check for CANCEL on response window
IF is_row_filter <> "" THEN

	// --------------------------------------
	//  Set filter on data window and filter
	// --------------------------------------
	dw_cheque_list.SelectRow(0, False) /* get rid of selected rows so they won't come back when we unfilter */
	
	dw_cheque_list.SetFilter(is_row_filter)
	dw_cheque_list.Filter()

END IF
end event

type cb_close from commandbutton within w_maintain_cheques
integer x = 2368
integer y = 2064
integer width = 320
integer height = 96
integer taborder = 170
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Close"
end type

event clicked;Close(Parent)
end event

type gb_1 from groupbox within w_maintain_cheques
integer x = 553
integer width = 544
integer height = 272
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Sort By:"
end type

type gb_search_type from groupbox within w_maintain_cheques
integer x = 9
integer width = 530
integer height = 272
integer taborder = 10
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
string text = "Search Type:"
end type

type rb_reconciled_date from radiobutton within w_maintain_cheques
integer x = 576
integer y = 184
integer width = 498
integer height = 72
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
string text = "Reconciled Date"
end type

event clicked;Long     ll_row, ll_num_rows, ll_found
Long		ll_cheque_no
String	ls_find

// Get the current Refund Number
ll_row = dw_cheque_list.GetRow()
IF ll_row > 0 THEN
	ll_cheque_no = dw_cheque_list.GetItemnumber(ll_row, "cheque_no")	
END IF

// Sort the DW
dw_cheque_list.SetSort("reconciled_amount_flag D reconciled_date D")
dw_cheque_list.Sort()

ls_find = "cheque_no = " + string(ll_cheque_no) 

// Go Find and Re-select the Refund
ll_num_rows = dw_cheque_list.RowCount()
ll_found = dw_cheque_list.Find(ls_find, 1, ll_num_rows)
IF ll_found > 0 THEN
	dw_cheque_list.ScrollToRow(ll_found)
END IF

end event

type dw_cheque_stub from u_dw_online within w_maintain_cheques
boolean visible = false
integer x = 343
integer y = 2084
integer width = 64
integer height = 80
integer taborder = 0
string dataobject = "d_cheque_stub"
borderstyle borderstyle = stylelowered!
end type

type cbx_to from checkbox within w_maintain_cheques
integer x = 1499
integer y = 52
integer width = 247
integer height = 72
integer taborder = 40
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = " to"
end type

event clicked;IF cbx_to.Checked = TRUE THEN
	em_to_cheque_no.Enabled = TRUE
	em_to_cheque_no.BackColor = 16777215
	em_to_cheque_no.SetFocus()
ELSE
	em_to_cheque_no.Enabled = FALSE
	em_to_cheque_no.BackColor = 67108864
	em_to_cheque_no.Text = ""
	
END IF
end event

type em_to_cheque_no from editmask within w_maintain_cheques
integer x = 1815
integer y = 36
integer width = 343
integer height = 100
integer taborder = 50
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
alignment alignment = right!
borderstyle borderstyle = stylelowered!
string mask = "#########"
string displaydata = " 8~b"
end type

