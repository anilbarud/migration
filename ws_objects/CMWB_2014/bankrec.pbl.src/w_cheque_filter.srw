$PBExportHeader$w_cheque_filter.srw
forward
global type w_cheque_filter from window
end type
type dw_cheque_filter from u_dw_online within w_cheque_filter
end type
type cb_ok from commandbutton within w_cheque_filter
end type
type cb_cancel from commandbutton within w_cheque_filter
end type
end forward

global type w_cheque_filter from window
integer x = 667
integer y = 320
integer width = 1719
integer height = 840
boolean titlebar = true
string title = "Filter Folder List"
windowtype windowtype = response!
long backcolor = 67108864
dw_cheque_filter dw_cheque_filter
cb_ok cb_ok
cb_cancel cb_cancel
end type
global w_cheque_filter w_cheque_filter

type variables

STRING			is_row_filter
STRING			is_filter_choice
end variables

forward prototypes
public subroutine wf_dwo_visible (string as_dwo)
public function integer wf_cheque_filter_rows ()
end prototypes

public subroutine wf_dwo_visible (string as_dwo);DATAWINDOWCHILD ldwc_child
int li_rtn

THIS.setredraw(FALSE)

dw_cheque_filter.Object.t_5.Visible = FALSE
dw_cheque_filter.Object.t_6.Visible = FALSE
dw_cheque_filter.Object.cheque_no_from.Visible = FALSE
dw_cheque_filter.Object.cheque_no_to.Visible = FALSE

dw_cheque_filter.Object.cheque_type_code.Visible = FALSE

dw_cheque_filter.Object.source_code.Visible = FALSE

dw_cheque_filter.Object.t_1.Visible = FALSE
dw_cheque_filter.Object.t_2.Visible = FALSE
dw_cheque_filter.Object.cheque_date_from.Visible = FALSE
dw_cheque_filter.Object.cheque_date_to.Visible = FALSE

dw_cheque_filter.Object.t_3.Visible = FALSE
dw_cheque_filter.Object.t_4.Visible = FALSE
dw_cheque_filter.Object.cheque_amount_from.Visible = FALSE
dw_cheque_filter.Object.cheque_amount_to.Visible = FALSE

dw_cheque_filter.Object.reconciled_amount_flag.Visible = FALSE

CHOOSE CASE as_dwo
	CASE 'N'
		dw_cheque_filter.Object.t_5.Visible = TRUE
 		dw_cheque_filter.Object.t_6.Visible = TRUE
		dw_cheque_filter.Object.cheque_no_from.Visible = TRUE
		dw_cheque_filter.Object.cheque_no_to.Visible = TRUE
	CASE 'T'
		dw_cheque_filter.Object.cheque_type_code.Visible = TRUE
		li_rtn=dw_cheque_filter.GetChild('cheque_type_code',ldwc_child)
		ldwc_child.SetTransObject(SQLCA)
		ldwc_child.Retrieve()
		
	CASE 'S'
		dw_cheque_filter.Object.source_code.Visible = TRUE
		dw_cheque_filter.GetChild('source_code',ldwc_child)
		ldwc_child.SetTransObject(SQLCA)
		ldwc_child.Retrieve()
	CASE 'D'
		dw_cheque_filter.Object.t_1.Visible = TRUE
		dw_cheque_filter.Object.t_2.Visible = TRUE
		dw_cheque_filter.Object.cheque_date_from.Visible = TRUE
		dw_cheque_filter.Object.cheque_date_to.Visible = TRUE
	CASE 'A'
		dw_cheque_filter.Object.t_3.Visible = TRUE
		dw_cheque_filter.Object.t_4.Visible = TRUE
		dw_cheque_filter.Object.cheque_amount_from.Visible = TRUE
		dw_cheque_filter.Object.cheque_amount_to.Visible = TRUE
	CASE 'R'
		dw_cheque_filter.Object.reconciled_amount_flag.Visible = TRUE
END CHOOSE

THIS.setredraw(TRUE)
end subroutine

public function integer wf_cheque_filter_rows ();//	This function sets up and filters rows in the datawindow dw_inbasket_folder_list.
//	Returns:		an integer.

LONG		ll_results, ll_counter, ll_cheque_from , ll_cheque_to
DATE		ld_from, ld_to, ld_bringfwddate
BOOLEAN	lb_first
STRING	ls_string, ls_string2
DECIMAL	 ldec_from, ldec_to

SetPointer(HourGlass!)

IF w_cheque_filter.dw_cheque_filter.AcceptText() = -1 THEN
	RETURN -1
END IF

//	Set up search on datawindow
is_filter_choice = w_cheque_filter.dw_cheque_filter.GetItemString(1,"filter_choice")
is_row_filter    = ""

CHOOSE CASE is_filter_choice
	CASE "N"				// "cheque_no"
		ll_cheque_from = w_cheque_filter.dw_cheque_filter.GetItemNumber(1,"cheque_no_from")
		ll_cheque_to = w_cheque_filter.dw_cheque_filter.GetItemNumber(1,"cheque_no_to")
		IF IsNull(ll_cheque_from) OR ll_cheque_from = 0 THEN
			IF IsNull(ll_cheque_to) OR ll_cheque_to = 0 THEN
				is_row_filter = ""
			ELSE
				is_row_filter = "cheque_no >= " + String(ll_cheque_from) + " and cheque_no <= " + String(2147483647)
			END IF
		ELSE
			IF ll_cheque_from > ll_cheque_to THEN
				MessageBox('Filter Error','The "from" number is higher than the "to" number. Please enter a proper cheque number range.')
				is_row_filter = ''
				RETURN -1
			ELSE
				is_row_filter = "cheque_no >= " + String(ll_cheque_from) + " and cheque_no <= " + String(ll_cheque_to)
			END IF
		END IF

	CASE "T"							// "cheque_type"
		IF IsNull(w_cheque_filter.dw_cheque_filter.GetItemString(1,"cheque_type_code")) OR IsNull(w_cheque_filter.dw_cheque_filter.GetItemString(1,"cheque_type_code")) THEN
			is_row_filter = ""
		ELSE
			is_row_filter = "cheque_type_code = '" + w_cheque_filter.dw_cheque_filter.GetItemString(1,"cheque_type_code") + "'"
		END IF

	CASE "S"   // "source_code"
		IF IsNull(w_cheque_filter.dw_cheque_filter.GetItemString(1,"source_code")) OR Trim(w_cheque_filter.dw_cheque_filter.GetItemString(1,"source_code")) = "" THEN
			is_row_filter = ""
		ELSE
			is_row_filter = 'source_code = "' + w_cheque_filter.dw_cheque_filter.GetItemString(1,"source_code") + '"'
		END IF
		
	CASE "D"							// "cheque_date"
		
		ld_from = w_cheque_filter.dw_cheque_filter.GetItemDate(1,"cheque_date_from")
		ld_to = w_cheque_filter.dw_cheque_filter.GetItemDate(1,"cheque_date_to")
		IF IsNull(ld_from) OR ld_from = 1900-01-01 THEN
			IF IsNull(ld_to) OR ld_to = 1900-01-01 THEN
				is_row_filter = ""
			ELSE
				is_row_filter = "cheque_date >= " + String(ld_from, 'yyyy-mm-dd') + " and cheque_date <= " + String(2079-06-06, 'yyyy-mm-dd')
			END IF
		ELSE
			IF ld_from > ld_to THEN
				MessageBox('','The "from" number is higher than the "to" number. Please enter a proper cheque date range.')
				is_row_filter = ''
				RETURN -1
			ELSE
				is_row_filter = "cheque_date >= " + String(ld_from, 'yyyy-mm-dd') + " and cheque_date <= " + String(ld_to, 'yyyy-mm-dd')
			END IF
		END IF


	CASE "A"  // "cheque_amount"
		ldec_from = w_cheque_filter.dw_cheque_filter.GetItemDecimal(1,"cheque_amount_from")
		ldec_to = w_cheque_filter.dw_cheque_filter.GetItemDecimal(1,"cheque_amount_to")
		IF IsNull(ldec_from) OR ldec_from = 0 THEN
			IF IsNull(ldec_to) OR ldec_to = 0 THEN
				is_row_filter = ""
			ELSE
				is_row_filter = "cheque_amount >= " + String(ldec_from) + " and cheque_amount <= " + String(99999999.99)
			END IF
		ELSE
			IF ldec_from > ldec_to THEN
				MessageBox('','The "from" number is higher than the "to" number. Please enter a proper cheque amount range.')
				is_row_filter = ''
				RETURN -1
			ELSE
				is_row_filter = "cheque_amount >= " + String(ldec_from) + " and cheque_amount <= " + String(ldec_to)
			END IF
		END IF


	CASE "R" // reconciled_amount_flag
		IF IsNull(w_cheque_filter.dw_cheque_filter.GetItemString(1,"reconciled_amount_flag")) OR Trim(w_cheque_filter.dw_cheque_filter.GetItemString(1,"reconciled_amount_flag")) = "" THEN
			is_row_filter = ""
		ELSE
			is_row_filter = 'reconciled_amount_flag = "' + w_cheque_filter.dw_cheque_filter.GetItemString(1,"reconciled_amount_flag") + '"'
		END IF

	CASE ELSE
		MessageBox("","Currently unavailable.")
		is_row_filter = ""
END CHOOSE

RETURN 1

end function

event open;INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


SetPointer(HourGlass!)

w_cheque_filter.dw_cheque_filter.Visible = TRUE

w_cheque_filter.dw_cheque_filter.InsertRow(0)

w_cheque_filter.dw_cheque_filter.SetItem(1, 'filter_choice','N')

end event

on w_cheque_filter.create
this.dw_cheque_filter=create dw_cheque_filter
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
this.Control[]={this.dw_cheque_filter,&
this.cb_ok,&
this.cb_cancel}
end on

on w_cheque_filter.destroy
destroy(this.dw_cheque_filter)
destroy(this.cb_ok)
destroy(this.cb_cancel)
end on

type dw_cheque_filter from u_dw_online within w_cheque_filter
integer x = 5
integer y = 8
integer width = 1696
integer height = 628
integer taborder = 10
string title = "none"
string dataobject = "d_cheque_filter_list"
boolean border = false
end type

event itemchanged;IF dwo.name = 'filter_choice' THEN
	wf_dwo_visible(data)
ELSE
	
END IF
end event

type cb_ok from commandbutton within w_cheque_filter
integer x = 987
integer y = 644
integer width = 334
integer height = 96
integer taborder = 60
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
boolean default = true
end type

event clicked;S_WINDOW_MESSAGE ls_message
INTEGER li_rtn


SetPointer(HourGlass!)

li_rtn = wf_cheque_filter_rows()

IF li_rtn = -1 THEN
	return
END IF

ls_message.as_stringparm[1] = is_row_filter

CloseWithReturn(w_cheque_filter,ls_message)
end event

type cb_cancel from commandbutton within w_cheque_filter
integer x = 1353
integer y = 644
integer width = 334
integer height = 96
integer taborder = 50
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cance&l"
end type

event clicked;S_WINDOW_MESSAGE ls_message

ls_message.al_doubleparm[1] = 0
ls_message.as_stringparm[1] = ""
ls_message.al_doubleparm[2] = 0
ls_message.as_stringparm[2] = ""
CloseWithReturn(w_cheque_filter,ls_message)
end event

