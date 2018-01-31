$PBExportHeader$w_select_calc_reason_response.srw
forward
global type w_select_calc_reason_response from window
end type
type dw_selection from u_datawindow within w_select_calc_reason_response
end type
type cb_cancel from commandbutton within w_select_calc_reason_response
end type
type cb_ok from commandbutton within w_select_calc_reason_response
end type
end forward

global type w_select_calc_reason_response from window
integer width = 1477
integer height = 624
boolean titlebar = true
string title = "Select Calculation Reason"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
dw_selection dw_selection
cb_cancel cb_cancel
cb_ok cb_ok
end type
global w_select_calc_reason_response w_select_calc_reason_response

type variables
s_calc_reason_data istr_calc_reason_data
n_calc_annuity     inv_calc_annuity
DATAWINDOWCHILD    idwc_reason_code
end variables

on w_select_calc_reason_response.create
this.dw_selection=create dw_selection
this.cb_cancel=create cb_cancel
this.cb_ok=create cb_ok
this.Control[]={this.dw_selection,&
this.cb_cancel,&
this.cb_ok}
end on

on w_select_calc_reason_response.destroy
destroy(this.dw_selection)
destroy(this.cb_cancel)
destroy(this.cb_ok)
end on

event open;INTEGER   li_rtn, li_find, li_rowcount
STRING    ls_dummy  // place holder variable for selected reason, which is used in itemchanged event
STRING    ls_annuity_calc_reason_code, ls_find, ls_annuity_calc_reason_desc_e, ls_active_filter
DWObject  l_dwo

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


istr_calc_reason_data = Message.PowerObjectParm


li_rtn = dw_selection.GetChild('annuity_calc_reason_code',idwc_reason_code)
li_rtn = idwc_reason_code.SetTransObject(SQLCA)

li_rowcount = idwc_reason_code.Retrieve()
sqlca.nf_handle_error("w_select_calc_reason_response","open","dw_selection.retrieve()")

/*	Insert a row */
li_rtn = dw_selection.InsertRow(0)


ls_active_filter = 'active_flag = "Y"'
idwc_reason_code.SetFilter(ls_active_filter)
idwc_reason_code.Filter()

inv_calc_annuity = Create n_calc_annuity

li_rtn = inv_calc_annuity.nf_validate_calculation_reason(ls_dummy,istr_calc_reason_data,ls_annuity_calc_reason_code)
IF li_rtn = 0 THEN
	li_rowcount = idwc_reason_code.RowCount()
	ls_find = 'annuity_calc_reason_code = "'+ls_annuity_calc_reason_code+'"'
	
	li_find = idwc_reason_code.find(ls_find,0,li_rowcount)
	IF li_find > 0 THEN
		ls_annuity_calc_reason_desc_e = idwc_reason_code.GetItemString(li_find,'annuity_calc_reason_desc_e')
		
		idwc_reason_code.ScrollToRow(li_find)
		idwc_reason_code.SetRow(li_find)
		
		dw_selection.scrolltorow(li_find)
		dw_selection.SetItem(li_find,'annuity_calc_reason_code',ls_annuity_calc_reason_desc_e)

		dw_selection.SetText(ls_annuity_calc_reason_desc_e)
		
		l_dwo = dw_selection.Object.annuity_calc_reason_code
		dw_selection.EVENT ItemChanged(li_find,l_dwo,ls_annuity_calc_reason_code)
		
		// item changed resets datawindowchild, so select row again
		idwc_reason_code.ScrollToRow(li_find)
		idwc_reason_code.SetRow(li_find)
	END IF	
END IF
end event

type dw_selection from u_datawindow within w_select_calc_reason_response
integer x = 18
integer width = 1120
integer height = 88
integer taborder = 10
string dataobject = "d_calc_reason_select"
boolean border = false
borderstyle borderstyle = stylelowered!
end type

event itemchanged;call super::itemchanged;INTEGER   li_rtn
STRING    ls_dummy // used in open event to set the reason, if necessary

cb_ok.Enabled = TRUE

li_rtn = inv_calc_annuity.nf_validate_calculation_reason(String(data),istr_calc_reason_data,ls_dummy)
IF li_rtn = -1 THEN
	RETURN 1
END IF
end event

event constructor;call super::constructor;this.settransobject(sqlca)
end event

type cb_cancel from commandbutton within w_select_calc_reason_response
integer x = 750
integer y = 420
integer width = 265
integer height = 104
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Cancel"
boolean default = true
end type

event clicked;	//CLOSE AND DO NOTHING
	closewithreturn(PARENT, '')
end event

type cb_ok from commandbutton within w_select_calc_reason_response
integer x = 462
integer y = 420
integer width = 265
integer height = 104
integer taborder = 10
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&OK"
end type

event clicked;/* grab the value of the item in the dropdown and return it to the calling script
*/
DATAWINDOWCHILD      ldwc_reason_code
INTEGER 					li_row, li_rtn
STRING					ls_annuity_calc_reason_code, ls_dummy


li_row = idwc_reason_code.GetRow()

IF ISNULL(li_row) OR li_row < 0 THEN 
	messagebox('Invalid Calculation Reason Selection','Please select again or `Cancel` to Exit')
	RETURN
ELSE	
	ls_annuity_calc_reason_code = idwc_reason_code.getitemstring(li_row, 'annuity_calc_reason_code')
	
	IF ISNULL(ls_annuity_calc_reason_code) OR TRIM(ls_annuity_calc_reason_code) = '' THEN 
		messagebox('Invalid Calculation Reason Selection','Please select again or `Cancel` to Exit')
		RETURN
	END IF
	
	li_rtn = inv_calc_annuity.nf_validate_calculation_reason(ls_annuity_calc_reason_code,istr_calc_reason_data,ls_dummy)
	IF li_rtn = -1 THEN
		RETURN
	END IF

	//send the calculation reason code back to be saved
	closewithreturn(PARENT, ls_annuity_calc_reason_code)
	
END IF

end event

