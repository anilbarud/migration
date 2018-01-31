$PBExportHeader$w_income_requests.srw
forward
global type w_income_requests from w_a_tool
end type
type r_details from rectangle within w_income_requests
end type
type gb_data from groupbox within w_income_requests
end type
type cb_create from commandbutton within w_income_requests
end type
type cb_save from commandbutton within w_income_requests
end type
type cb_delete from commandbutton within w_income_requests
end type
type cb_cancel from commandbutton within w_income_requests
end type
type cb_view_details from commandbutton within w_income_requests
end type
type tab_requests from tab within w_income_requests
end type
type tabpage_pending from userobject within tab_requests
end type
type dw_request_pending from u_dw_online within tabpage_pending
end type
type tabpage_pending from userobject within tab_requests
dw_request_pending dw_request_pending
end type
type tabpage_processed from userobject within tab_requests
end type
type dw_processed from u_dw_online within tabpage_processed
end type
type tabpage_processed from userobject within tab_requests
dw_processed dw_processed
end type
type tab_requests from tab within w_income_requests
tabpage_pending tabpage_pending
tabpage_processed tabpage_processed
end type
type uo_filter from u_filter_control within w_income_requests
end type
type rb_by_claim from radiobutton within w_income_requests
end type
type rb_by_individual from radiobutton within w_income_requests
end type
type rb_details from radiobutton within w_income_requests
end type
type rb_years from radiobutton within w_income_requests
end type
type gb_view from groupbox within w_income_requests
end type
type gb_filter from groupbox within w_income_requests
end type
type dw_processed_details from u_dw_online within w_income_requests
end type
type dw_details from u_dw_online within w_income_requests
end type
type dw_tax_years from u_dw_online within w_income_requests
end type
type st_details from statictext within w_income_requests
end type
type st_who from statictext within w_income_requests
end type
end forward

global type w_income_requests from w_a_tool
integer width = 2665
long il_design_time_height = 1792
long il_design_time_width = 2629
event ue_post_open ( )
r_details r_details
gb_data gb_data
cb_create cb_create
cb_save cb_save
cb_delete cb_delete
cb_cancel cb_cancel
cb_view_details cb_view_details
tab_requests tab_requests
uo_filter uo_filter
rb_by_claim rb_by_claim
rb_by_individual rb_by_individual
rb_details rb_details
rb_years rb_years
gb_view gb_view
gb_filter gb_filter
dw_processed_details dw_processed_details
dw_details dw_details
dw_tax_years dw_tax_years
st_details st_details
st_who st_who
end type
global w_income_requests w_income_requests

type variables
n_income_verification inv_income

LONG il_claim_no, il_individual_no, il_request_no, il_inbasket_request_no, il_success
STRING is_status, is_msg_parm
s_window_message		istr_window_message
BOOLEAN ib_triggered
STRING is_import_success 
w_view_response iw_details_window
end variables

forward prototypes
public subroutine wf_enable (string as_enable)
public subroutine wf_check_buttons ()
public function integer wf_redraw (boolean ab_redraw)
public function integer wf_protect (boolean ab_protect)
public function integer wf_resize ()
public function integer wf_highlight_row (datawindow adw_dw, long al_row)
public function integer wf_get_request_status (long al_request_no, long al_year)
end prototypes

event ue_post_open();LONG ll_row

IF is_msg_parm = 'INBASKET' THEN
	wf_enable('PROCESSED')
	rb_details.Checked = TRUE
	tab_requests.SelectTab('tabpage_processed')
	il_request_no = il_inbasket_request_no
	tab_requests.tabpage_processed.dw_processed.Retrieve(il_claim_no)
	ll_row = tab_requests.tabpage_processed.dw_processed.Find("iv_request_no = " + STRING(il_request_no), 1, tab_requests.tabpage_processed.dw_processed.RowCount())
	wf_highlight_row(tab_requests.tabpage_processed.dw_processed,ll_row)
	tab_requests.tabpage_pending.Enabled = FALSE	
ELSE
	tab_requests.tabpage_pending.dw_request_pending.Retrieve(il_claim_no)
	ll_row = tab_requests.tabpage_pending.dw_request_pending.GetRow()
	wf_highlight_row(tab_requests.tabpage_pending.dw_request_pending,ll_row)
END IF
end event

public subroutine wf_enable (string as_enable);
CHOOSE CASE as_enable
	CASE 'MODIFY'
		cb_create.Enabled         = TRUE
		cb_save.Enabled           = FALSE
		cb_cancel.Enabled         = FALSE
		IF tab_requests.tabpage_pending.dw_request_pending.GetRow() > 0 THEN
			cb_delete.Enabled         = TRUE
		ELSE 
			cb_delete.Enabled         = FALSE
		END IF
		
		gb_view.Enabled           = FALSE
		gb_data.Enabled           = FALSE
		rb_by_claim.Enabled     = FALSE
		rb_by_individual.Enabled= FALSE
		rb_details.Enabled         = FALSE
		rb_years.Enabled          = FALSE
	CASE 'PROCESSED'
		cb_create.Enabled         = FALSE
		cb_save.Enabled           = FALSE
		cb_cancel.Enabled         = FALSE
		cb_delete.Enabled         = FALSE
		
		gb_view.Enabled           = TRUE
		gb_data.Enabled           = TRUE
		rb_by_claim.Enabled     = TRUE
		rb_by_individual.Enabled= TRUE
		rb_details.Enabled         = TRUE
		rb_years.Enabled          = TRUE
	CASE 'CREATE'
		cb_create.Enabled         = FALSE
		cb_save.Enabled           = TRUE
		cb_cancel.Enabled         = TRUE
		cb_delete.Enabled         = FALSE
		
		gb_view.Enabled           = FALSE
		gb_data.Enabled           = FALSE
		rb_by_claim.Enabled     = FALSE
		rb_by_individual.Enabled= FALSE
		rb_details.Enabled         = FALSE
		rb_years.Enabled          = FALSE
END CHOOSE


end subroutine

public subroutine wf_check_buttons ();LONG ll_retrieve
STRING ls_name

IF tab_requests.SelectedTab = 2 THEN
	IF rb_years.Checked THEN
		r_details.Visible = FALSE
		st_details.Visible = FALSE
		dw_processed_details.Visible = FALSE
		IF rb_by_claim.Checked THEN
			tab_requests.tabpage_processed.dw_processed.DataObject = 'd_requests_processed_years'
			ll_retrieve = il_claim_no
			st_who.Text = 'Requests For Claim #: ' + STRING(il_claim_no)
		ELSEIF rb_by_individual.Checked THEN
			tab_requests.tabpage_processed.dw_processed.DataObject = 'd_requests_processed_by_individual_years'
			ll_retrieve = il_individual_no
			inv_income.nf_get_individual_info(il_individual_no, ls_name)
			st_who.Text = 'Requests For Individual #: ' + STRING(il_individual_no) + ', ' + ls_name
		END IF
	ELSEIF rb_details.Checked THEN
		r_details.Visible = TRUE
		dw_processed_details.Visible = TRUE
		IF rb_by_claim.Checked THEN
			tab_requests.tabpage_processed.dw_processed.DataObject = 'd_requests_processed'
			ll_retrieve = il_claim_no
			st_who.Text = 'Requests For Claim #: ' + STRING(il_claim_no)
		ELSEIF rb_by_individual.Checked THEN
			tab_requests.tabpage_processed.dw_processed.DataObject = 'd_requests_processed_by_individual'
			ll_retrieve = il_individual_no
			inv_income.nf_get_individual_info(il_individual_no, ls_name)
			st_who.Text = 'Requests For Individual #: ' + STRING(il_individual_no) + ', ' + ls_name
		END IF
	END IF
	
	tab_requests.tabpage_processed.dw_processed.SetTransObject(SQLCA)
	tab_requests.tabpage_processed.dw_processed.Retrieve(ll_retrieve)
	
	IF tab_requests.tabpage_processed.dw_processed.GetRow() > 0 THEN
		il_request_no =  tab_requests.tabpage_processed.dw_processed.GetItemNumber(tab_requests.tabpage_processed.dw_processed.GetRow(), 'iv_request_no')
		IF dw_processed_details.Visible = TRUE  THEN		
			dw_processed_details.Retrieve(il_request_no)
			wf_highlight_row(dw_processed_details, 1)
		END IF
	END IF
END IF
end subroutine

public function integer wf_redraw (boolean ab_redraw);
tab_requests.tabpage_pending.dw_request_pending.SetRedraw(ab_redraw)
dw_tax_years.SetRedraw(ab_redraw)
dw_details.SetRedraw(ab_redraw)

RETURN 0
end function

public function integer wf_protect (boolean ab_protect);

IF ab_protect THEN
	dw_tax_years.Object.tax_year.Protect = 1
	dw_details.Object.iv_request_inbasket_catid.Protect = 1
	dw_details.Object.iv_request_comment.Protect = 1
ELSE
	dw_tax_years.Object.tax_year.Protect = 0
	dw_details.Object.iv_request_inbasket_catid.Protect = 0
	dw_details.Object.iv_request_comment.Protect = 0
END IF

RETURN 0
end function

public function integer wf_resize ();wf_setresize(TRUE)
	
inv_resize.of_Register(tab_requests.tabpage_pending.dw_request_pending,'ScaleToRight&Bottom')
inv_resize.of_Register(tab_requests.tabpage_processed.dw_processed,'ScaleToRight&Bottom')
inv_resize.of_Register(dw_tax_years,'FixedToBottom')
inv_resize.of_Register(dw_details,'FixedToBottom')

inv_resize.of_Register(dw_processed_details,'FixedToBottom&ScaleToRight')

inv_resize.of_Register(r_details,'FixedToBottom&ScaleToRight')
inv_resize.of_Register(st_details,'FixedToBottom&ScaleToRight')
inv_resize.of_Register(st_title,'ScaleToRight')
inv_resize.of_Register(tab_requests,'ScaleToRight&Bottom')
inv_resize.of_Register(cb_view_details,'FixedToBottom')
inv_resize.of_Register(cb_create,'FixedToBottom')
inv_resize.of_Register(cb_save,'FixedToBottom')
inv_resize.of_Register(cb_cancel,'FixedToBottom')
inv_resize.of_Register(cb_close,'FixedToBottom')
inv_resize.of_Register(cb_delete,'FixedToBottom')
RETURN 0
end function

public function integer wf_highlight_row (datawindow adw_dw, long al_row);adw_dw.ScrollToRow(al_row)
adw_dw.SelectRow(al_row, TRUE)

RETURN 0
end function

public function integer wf_get_request_status (long al_request_no, long al_year);LONG ll_cntr, ll_rows
STRING ls_status

IF al_year > 0 THEN
	SELECT cra_request_status_code
	INTO       :ls_status
	FROM      v_I015_IV_RESPONSE_RESPONSE_WHSCC
	WHERE  v_I015_IV_RESPONSE_RESPONSE_WHSCC.iv_request_no = :al_request_no
	AND         v_I015_IV_RESPONSE_RESPONSE_WHSCC.tax_year = :al_year
	USING     SQLCA;
	
	SQLCA.nf_handle_error('w_income_requests','wf_get_request_status()','SELECT cra_request_status_code')
	
	IF ls_status = '01' OR ls_status = '02' THEN
		il_success = il_success + 1
	END IF
	
ELSE
	ll_rows = dw_processed_details.RowCount()

	IF ll_rows > 0 THEN
	
		FOR ll_cntr = 1 to ll_rows
			ls_status = dw_processed_details.GetItemString(ll_cntr, 'cra_request_status_code')
			IF ls_status = '01' OR ls_status = '02' THEN
				// Request is successful
				il_success = il_success + 1
			END IF
		NEXT
	
	END IF
END IF

RETURN 0
end function

on w_income_requests.create
int iCurrent
call super::create
this.r_details=create r_details
this.gb_data=create gb_data
this.cb_create=create cb_create
this.cb_save=create cb_save
this.cb_delete=create cb_delete
this.cb_cancel=create cb_cancel
this.cb_view_details=create cb_view_details
this.tab_requests=create tab_requests
this.uo_filter=create uo_filter
this.rb_by_claim=create rb_by_claim
this.rb_by_individual=create rb_by_individual
this.rb_details=create rb_details
this.rb_years=create rb_years
this.gb_view=create gb_view
this.gb_filter=create gb_filter
this.dw_processed_details=create dw_processed_details
this.dw_details=create dw_details
this.dw_tax_years=create dw_tax_years
this.st_details=create st_details
this.st_who=create st_who
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.r_details
this.Control[iCurrent+2]=this.gb_data
this.Control[iCurrent+3]=this.cb_create
this.Control[iCurrent+4]=this.cb_save
this.Control[iCurrent+5]=this.cb_delete
this.Control[iCurrent+6]=this.cb_cancel
this.Control[iCurrent+7]=this.cb_view_details
this.Control[iCurrent+8]=this.tab_requests
this.Control[iCurrent+9]=this.uo_filter
this.Control[iCurrent+10]=this.rb_by_claim
this.Control[iCurrent+11]=this.rb_by_individual
this.Control[iCurrent+12]=this.rb_details
this.Control[iCurrent+13]=this.rb_years
this.Control[iCurrent+14]=this.gb_view
this.Control[iCurrent+15]=this.gb_filter
this.Control[iCurrent+16]=this.dw_processed_details
this.Control[iCurrent+17]=this.dw_details
this.Control[iCurrent+18]=this.dw_tax_years
this.Control[iCurrent+19]=this.st_details
this.Control[iCurrent+20]=this.st_who
end on

on w_income_requests.destroy
call super::destroy
destroy(this.r_details)
destroy(this.gb_data)
destroy(this.cb_create)
destroy(this.cb_save)
destroy(this.cb_delete)
destroy(this.cb_cancel)
destroy(this.cb_view_details)
destroy(this.tab_requests)
destroy(this.uo_filter)
destroy(this.rb_by_claim)
destroy(this.rb_by_individual)
destroy(this.rb_details)
destroy(this.rb_years)
destroy(this.gb_view)
destroy(this.gb_filter)
destroy(this.dw_processed_details)
destroy(this.dw_details)
destroy(this.dw_tax_years)
destroy(this.st_details)
destroy(this.st_who)
end on

event open;call super::open;U_DWA   	ldw_dw[]

IF cb_create.Visible = FALSE THEN
	wf_protect(TRUE)
END IF

istr_window_message = Message.PowerObjectParm

il_claim_no       = iw_active_sheet.dw_basic_claim.GetItemNumber(1,'claim_no')
il_individual_no = iw_active_sheet.dw_basic_claim.GetItemNumber(1,'individual_no')

is_msg_parm = istr_window_message.as_stringparm[1]
IF is_msg_parm = 'INBASKET' THEN	
	il_claim_no = istr_window_message.al_doubleparm[1]
	il_request_no = istr_window_message.al_doubleparm[2]
	il_inbasket_request_no = il_request_no
END IF

inv_income = CREATE n_income_verification
inv_income.nf_set_window_parent(THIS)

ldw_dw[1] = dw_details
ldw_dw[2] = dw_tax_years

inv_income.nf_init(ldw_dw[],SQLCA,THIS)
inv_income.nf_set_commit(TRUE)

inv_income.nf_set_claim_info(il_claim_no, il_individual_no)

tab_requests.tabpage_pending.dw_request_pending.uf_setfilter(TRUE)
tab_requests.tabpage_pending.dw_request_pending.uf_setsort(TRUE)

tab_requests.tabpage_processed.dw_processed.uf_setfilter(TRUE)
tab_requests.tabpage_processed.dw_processed.uf_setsort(TRUE)


wf_resize()

tab_requests.tabpage_processed.dw_processed.SetTransObject(SQLCA)
tab_requests.tabpage_pending.dw_request_pending.SetTransObject(SQLCA)

THIS.PostEvent('ue_post_open')
end event

event close;call super::close;DESTROY inv_income
end event

type st_title from w_a_tool`st_title within w_income_requests
string text = "CRA Income Requests"
end type

type cb_close from w_a_tool`cb_close within w_income_requests
integer x = 2304
integer width = 315
integer taborder = 10
end type

type r_details from rectangle within w_income_requests
long linecolor = 134217745
integer linethickness = 4
long fillcolor = 67108864
integer x = 14
integer y = 1120
integer width = 2597
integer height = 540
end type

type gb_data from groupbox within w_income_requests
integer x = 32
integer y = 88
integer width = 896
integer height = 184
integer taborder = 40
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Data"
end type

type cb_create from commandbutton within w_income_requests
integer x = 599
integer y = 1680
integer width = 457
integer height = 100
integer taborder = 50
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Create Request"
end type

event clicked;LONG ll_cnt_cats

dw_tax_years.Reset()

il_request_no = 0
inv_income.nf_insert(0)

dw_tax_years.SetFocus()
dw_tax_years.SetRow(1)

wf_enable('CREATE')
end event

type cb_save from commandbutton within w_income_requests
integer x = 1061
integer y = 1680
integer width = 457
integer height = 100
integer taborder = 120
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Save Request"
end type

event clicked;LONG      ll_rowcount, ll_cntr, ll_row, ll_export, ll_rows
INTEGER   li_trancount


dw_tax_years.AcceptText()

SELECT x015_export_no
INTO       :ll_export
FROM     IV_REQUEST
WHERE iv_request_no = :il_request_no
USING   SQLCA;

SQLCA.nf_handle_error('w_income_requests','cb_save','SELECT x015_export_no')

IF ll_export  >  0 THEN		
	MessageBox('Error','This record cannot be modified.  It has been exported and sent to CRA for processing.', Information!)
	tab_requests.tabpage_pending.dw_request_pending.Retrieve(il_claim_no)
	ll_row = tab_requests.tabpage_pending.dw_request_pending.GetRow()
	IF ll_row > 0 THEN
		il_request_no =	tab_requests.tabpage_pending.dw_request_pending.GetItemNumber(ll_row, 'iv_request_no')
		inv_income.nf_retrieve_details(il_request_no)			
		inv_income.nf_fill_years()
	END IF
	wf_highlight_row(tab_requests.tabpage_pending.dw_request_pending, ll_row)
	wf_enable('MODIFY')
	RETURN
END IF


SQLCA.nf_begin_transaction()

IF inv_income.nf_save() = 0 THEN
	SQLCA.nf_commit_transaction()
	
	wf_redraw(FALSE)
	
	tab_requests.tabpage_pending.dw_request_pending.Retrieve(il_claim_no)
	ll_row = tab_requests.tabpage_pending.dw_request_pending.GetRow()
	IF ll_row > 0 THEN
		il_request_no =	tab_requests.tabpage_pending.dw_request_pending.GetItemNumber(ll_row, 'iv_request_no')
		inv_income.nf_retrieve_details(il_request_no)			
		inv_income.nf_fill_years()
	END IF
	wf_highlight_row(tab_requests.tabpage_pending.dw_request_pending, ll_row)
	wf_redraw(TRUE)
	wf_enable('MODIFY')
ELSE
	SQLCA.nf_rollback_transaction()

	IF tab_requests.tabpage_pending.dw_request_pending.GetRow() > 0 THEN
		inv_income.nf_fill_years()
		wf_highlight_row(tab_requests.tabpage_pending.dw_request_pending, ll_row)
	END IF 
END IF



end event

type cb_delete from commandbutton within w_income_requests
integer x = 1842
integer y = 1680
integer width = 457
integer height = 100
integer taborder = 150
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Delete Request"
end type

event clicked;LONG ll_export, ll_rows, ll_row

SELECT x015_export_no
INTO       :ll_export
FROM     IV_REQUEST
WHERE iv_request_no = :il_request_no
USING   SQLCA;

SQLCA.nf_handle_error('w_income_requests','cb_delete','SELECT x015_export_no')

IF ll_export > 0 THEN
	MessageBox('Cannot Delete','This request cannot be deleted. It has been exported and sent to CRA for processing.', Information!)
	tab_requests.tabpage_pending.dw_request_pending.Retrieve(il_claim_no)
	ll_row = tab_requests.tabpage_pending.dw_request_pending.GetRow()
	IF ll_row > 0 THEN
		il_request_no = tab_requests.tabpage_pending.dw_request_pending.GetItemNumber(ll_row, 'iv_request_no')
		inv_income.nf_retrieve_details(il_request_no)			
		inv_income.nf_fill_years()
	END IF
ELSE
	IF MessageBox('Delete?','Are you sure you would like to delete this request?',Question!, YesNo!) =  1 THEN		
		
		wf_redraw(FALSE)
		
		inv_income.nf_delete(il_request_no)
	
		tab_requests.tabpage_pending.dw_request_pending.Retrieve(il_claim_no)
		ll_row = tab_requests.tabpage_pending.dw_request_pending.GetRow()
		IF ll_row > 0 THEN
			il_request_no = tab_requests.tabpage_pending.dw_request_pending.GetItemNumber(ll_row, 'iv_request_no')
			inv_income.nf_retrieve_details(il_request_no)			
			inv_income.nf_fill_years()		
		ELSE
			dw_tax_years.Reset()
			dw_details.Reset()
		END IF
		wf_redraw(TRUE)		
		
	END IF
END IF
ll_row = tab_requests.tabpage_pending.dw_request_pending.GetRow()
wf_highlight_row(tab_requests.tabpage_pending.dw_request_pending, ll_row)
wf_enable('MODIFY')
end event

type cb_cancel from commandbutton within w_income_requests
integer x = 1522
integer y = 1680
integer width = 315
integer height = 100
integer taborder = 160
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cancel"
end type

event clicked;LONG ll_row

ll_row = tab_requests.tabpage_pending.dw_request_pending.GetRow()

wf_redraw(FALSE)
IF ll_row > 0 THEN
	tab_requests.tabpage_pending.dw_request_pending.Retrieve(il_claim_no)
	inv_income.nf_retrieve_details(il_request_no)
	inv_income.nf_fill_years()
	wf_highlight_row(tab_requests.tabpage_pending.dw_request_pending,ll_row)
ELSE
	dw_tax_years.Reset()
	dw_details.Reset()
END IF
wf_enable('MODIFY')
wf_redraw(TRUE)

end event

type cb_view_details from commandbutton within w_income_requests
boolean visible = false
integer x = 50
integer y = 1680
integer width = 402
integer height = 100
integer taborder = 130
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "View Tax Info"
end type

event clicked;str_tax_years lstr_tax_years
STRING ls_status
LONG ll_row, ll_count
w_view_response lw_details_window

lstr_tax_years.single_year = FALSE
lstr_tax_years.request_no = il_request_no

ll_row = tab_requests.tabpage_processed.dw_processed.GetRow() 

IF ll_row > 0 THEN

	ls_status = tab_requests.tabpage_processed.dw_processed.GetItemString(ll_row, 'iv_request_worksafe_request_status_code')
	IF ls_status = 'RR' THEN
		IF rb_years.Checked THEN
			lstr_tax_years.single_year = TRUE
			lstr_tax_years.year = tab_requests.tabpage_processed.dw_processed.GetItemNumber(tab_requests.tabpage_processed.dw_processed.GetRow(), 'iv_request_detail_tax_year')
		ELSE
			ll_count = dw_processed_details.RowCount()
			IF ll_count = 1 THEN
				ib_triggered = TRUE
			END IF 
			
			IF ib_triggered THEN
				lstr_tax_years.year = dw_processed_details.GetItemNumber(dw_processed_details.GetRow(), 'tax_year')
				lstr_tax_years.single_year = TRUE
				ib_triggered = FALSE
			ELSE
				lstr_tax_years.year = 0
			END IF
		END IF 
		
		OpenWithParm(lw_details_window, lstr_tax_years,'w_view_response')
	ELSE
		MessageBox('No Responses','There have not been any Responses received for the selected Request.',Information!)
	END IF
		
ELSE 
	IF tab_requests.tabpage_processed.dw_processed.RowCount() = 0 THEN
		MessageBox('No Requests','There are currently no requests for which you can view tax information.',Information!)
	ELSE
		MessageBox('No Requests','Please select a request or tax year to view tax information.',Information!)
	END IF
END IF
end event

type tab_requests from tab within w_income_requests
integer y = 288
integer width = 2610
integer height = 808
integer taborder = 60
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
boolean raggedright = true
boolean focusonbuttondown = true
boolean boldselectedtext = true
integer selectedtab = 1
tabpage_pending tabpage_pending
tabpage_processed tabpage_processed
end type

on tab_requests.create
this.tabpage_pending=create tabpage_pending
this.tabpage_processed=create tabpage_processed
this.Control[]={this.tabpage_pending,&
this.tabpage_processed}
end on

on tab_requests.destroy
destroy(this.tabpage_pending)
destroy(this.tabpage_processed)
end on

event selectionchanged;LONG ll_rowcount, ll_cntr, ll_row

IF newindex = 2 THEN 
	uo_filter.uf_set_Requestor(tab_requests.tabpage_processed.dw_processed)
	dw_processed_details.SetTransObject(SQLCA)
	cb_view_details.Visible = TRUE
	wf_enable('PROCESSED')
	dw_details.Visible = FALSE
	dw_tax_years.Visible = FALSE
	st_details.Visible = FALSE
	wf_resize()
	wf_check_buttons()
	
ELSE
	uo_filter.uf_set_Requestor(tab_requests.tabpage_pending.dw_request_pending)
	cb_view_details.Visible = FALSE
	st_details.Visible = TRUE
	r_details.Visible = TRUE
	dw_processed_details.Visible = FALSE
	dw_details.Visible = TRUE
	dw_tax_years.Visible = TRUE
	st_who.Text = 'Requests For Claim #: ' + STRING(il_claim_no)
	wf_resize( )
	tab_requests.tabpage_pending.dw_request_pending.Retrieve(il_claim_no)
	ll_row = tab_requests.tabpage_pending.dw_request_pending.GetRow()
	wf_highlight_row(tab_requests.tabpage_pending.dw_request_pending,ll_row)
	IF tabpage_pending.dw_request_pending.Rowcount() >0 THEN
		il_request_no = tabpage_pending.dw_request_pending.GetItemNumber(tabpage_pending.dw_request_pending.GetRow(), 'iv_request_no')
		inv_income.nf_retrieve_details(il_request_no)
		inv_income.nf_fill_years()
	END IF
	wf_enable('MODIFY')	
END IF
end event

event selectionchanging;IF newindex = 2 THEN
	IF cb_create.Enabled = FALSE  AND tabpage_pending.dw_request_pending.RowCount() > 0 AND NOT is_msg_parm = 'INBASKET' THEN
		MessageBox('Save','You have unsaved changes on the New/Pending tab. Please save or cancel before changing tabs.',Information!)
		RETURN 1
	END IF
END IF
end event

type tabpage_pending from userobject within tab_requests
event create ( )
event destroy ( )
string tag = "Pending Requests"
integer x = 18
integer y = 108
integer width = 2574
integer height = 684
long backcolor = 67108864
string text = "New/Pending"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
dw_request_pending dw_request_pending
end type

on tabpage_pending.create
this.dw_request_pending=create dw_request_pending
this.Control[]={this.dw_request_pending}
end on

on tabpage_pending.destroy
destroy(this.dw_request_pending)
end on

type dw_request_pending from u_dw_online within tabpage_pending
integer y = 36
integer width = 2565
integer height = 636
integer taborder = 10
string dataobject = "d_requests_pending"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event rowfocuschanged;call super::rowfocuschanged;LONG ll_request_no, ll_rows, ll_rowcount, ll_cntr

STRING ls_status

IF currentrow > 0 THEN
	
	THIS.SetRow(currentrow)
	THIS.SetRedraw(TRUE)
	
	wf_enable('MODIFY')
	il_request_no = dw_request_pending.GetItemNumber(currentrow, 'iv_request_no')
	ll_request_no = il_request_no
	is_status        = dw_request_pending.GetItemString(currentrow, 'worksafe_request_status_code')
	ls_status = is_status
	IF ll_request_no = 0 OR IsNull(ll_request_no) THEN
		MessageBox('Error','There was an error determining selected request number.',Information!)
		RETURN
	END IF

	IF inv_income.nf_retrieve_details(ll_request_no) < 0 THEN
		MessageBox('No Rows','No rows were returned for request # ' + STRING(ll_request_no))
		RETURN
	ELSE
		inv_income.nf_fill_years()
	END IF
	
END IF
end event

event getfocus;call super::getfocus;uo_filter.uf_set_Requestor(tab_requests.tabpage_pending.dw_request_pending)
end event

event rowfocuschanging;call super::rowfocuschanging;THIS.SelectRow(currentrow,FALSE)
THIS.SelectRow(newrow, TRUE)

end event

event rbuttondown;RETURN 0
end event

type tabpage_processed from userobject within tab_requests
integer x = 18
integer y = 108
integer width = 2574
integer height = 684
long backcolor = 67108864
string text = "Sent/Processed"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
dw_processed dw_processed
end type

on tabpage_processed.create
this.dw_processed=create dw_processed
this.Control[]={this.dw_processed}
end on

on tabpage_processed.destroy
destroy(this.dw_processed)
end on

type dw_processed from u_dw_online within tabpage_processed
event ue_view_details ( )
integer y = 36
integer width = 2565
integer height = 644
integer taborder = 10
string dataobject = "d_requests_processed"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event ue_view_details();
IF 	THIS.GetRow() > 0 THEN
	ib_triggered = FALSE
	il_request_no = THIS.GetItemNumber(THIS.GetRow(), 'iv_request_no')
	cb_view_details.TriggerEvent(Clicked!)
END IF

end event

event rowfocuschanged;call super::rowfocuschanged;LONG ll_request_no, ll_rows, ll_year

IF currentrow > 0 THEN

	il_request_no = dw_processed.GetItemNumber(currentrow, 'iv_request_no')
	ll_request_no = il_request_no
	
	is_status        = dw_processed.GetItemString(currentrow, 'iv_request_worksafe_request_status_code')
	is_import_success = dw_processed.GetItemString(currentrow,'import_success_flag')
	
	IF ll_request_no = 0 OR IsNull(ll_request_no) THEN
		MessageBox('Error','There was an error determining selected request number.',Information!)
		RETURN
	END IF
		
	IF	dw_processed_details.Retrieve(ll_request_no) < 0 THEN
		MessageBox('Error','Error retrieving processed request details.',Information!)
		RETURN
	ELSE
		wf_highlight_row(dw_processed_details,dw_processed_details.GetRow())
	END IF
	
	IF rb_years.Checked THEN
		ll_year = dw_processed.GetItemNumber(currentrow, 'iv_request_detail_tax_year')
	ELSE
		ll_year = 0
	END IF
	
	IF wf_get_request_status(ll_request_no, ll_year) <> 0 THEN 
		MessageBox('Error','Error retrieving request status.',Information!)
		RETURN
	END IF
	
	IF is_status = 'RR'  AND il_success >= 1  AND is_import_success = 'Y' THEN
		cb_view_details.Enabled = TRUE
		ib_triggered = FALSE
		il_success = 0
	ELSE
		cb_view_details.Enabled = FALSE
	END IF
END IF

end event

event rowfocuschanging;call super::rowfocuschanging;THIS.SelectRow(currentrow,FALSE)
THIS.SelectRow(newrow, TRUE)
end event

event rbuttondown;M_DW_ONLINE_RMB_POPUP lm_popup
STRING ls_status
/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/

IF row > 0 THEN

	THIS.SetRow(row)
	THIS.ScrollToRow(row)
	
	IF is_status = 'RR'  AND il_success >= 1  AND is_import_success = 'Y' THEN
		IF NOT IsValid(lm_popup) THEN
			lm_popup = Create m_dw_online_rmb_popup
			lm_popup.mf_set_datawindow(This)
			lm_popup.m_options.m_viewtaxinformation.Visible = TRUE
			lm_popup.m_options.m_1.Visible = FALSE
			lm_popup.m_options.PopMenu(w_frame.PointerX( ), w_frame.PointerY( ))		
			Destroy lm_popup
		END IF
	END IF
END IF

end event

event getfocus;call super::getfocus;uo_filter.uf_set_Requestor(tab_requests.tabpage_processed.dw_processed)
end event

event itemfocuschanged;call super::itemfocuschanged;ib_triggered = FALSE
end event

type uo_filter from u_filter_control within w_income_requests
integer x = 1961
integer y = 148
integer taborder = 140
boolean bringtotop = true
end type

on uo_filter.destroy
call u_filter_control::destroy
end on

event ue_filter_changed;call super::ue_filter_changed;idw_datawindow.object.st_filter.text = ls_new_filter
end event

type rb_by_claim from radiobutton within w_income_requests
integer x = 119
integer y = 156
integer width = 402
integer height = 76
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Claim"
boolean checked = true
end type

event clicked;wf_check_buttons()
end event

type rb_by_individual from radiobutton within w_income_requests
integer x = 466
integer y = 156
integer width = 370
integer height = 76
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Individual"
end type

event clicked;wf_check_buttons()
end event

type rb_details from radiobutton within w_income_requests
integer x = 1079
integer y = 156
integer width = 402
integer height = 76
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Requests"
end type

event clicked;dw_processed_details.Visible = TRUE
//tab_requests.Height = 808
//tab_requests.tabpage_processed.dw_processed.Height = 644

wf_check_buttons()
end event

type rb_years from radiobutton within w_income_requests
integer x = 1431
integer y = 156
integer width = 352
integer height = 76
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Tax Years"
boolean checked = true
end type

event clicked;dw_processed_details.Visible = FALSE
dw_tax_years.Visible = FALSE

wf_check_buttons()
end event

type gb_view from groupbox within w_income_requests
integer x = 974
integer y = 88
integer width = 896
integer height = 184
integer taborder = 80
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "View"
end type

type gb_filter from groupbox within w_income_requests
integer x = 1906
integer y = 88
integer width = 713
integer height = 184
integer taborder = 100
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Filter"
end type

type dw_processed_details from u_dw_online within w_income_requests
event ue_view_details ( )
boolean visible = false
integer x = 14
integer y = 1112
integer width = 2597
integer height = 556
integer taborder = 30
string dataobject = "d_processed_request_details"
end type

event ue_view_details();IF THIS.GetRow() > 0 THEN
	ib_triggered =  TRUE
	il_request_no = THIS.GetItemNumber(THIS.GetRow(), 'iv_request_no')
	cb_view_details.TriggerEvent(Clicked!)
END IF
end event

event buttonclicked;call super::buttonclicked;IF dwo.name = 'b_view' THEN
	ib_triggered = TRUE
	il_request_no = dw_processed_details.GetItemNumber(row, 'iv_request_no')
	cb_view_details.TriggerEvent(Clicked!)
END IF
end event

event rbuttondown;M_DW_ONLINE_RMB_POPUP lm_popup
STRING ls_status
LONG ll_row
/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/

IF row > 0 THEN
	THIS.SetRow(row)
	THIS.ScrollToRow(row)
	ls_status = dw_processed_details.GetItemString(row, 'cra_request_status_code')
	IF is_status = 'RR'  AND (ls_status = '01' OR ls_status = '02')  AND is_import_success = 'Y' THEN
		lm_popup = Create m_dw_online_rmb_popup
		lm_popup.mf_set_datawindow(This)
		lm_popup.m_options.m_viewtaxinformation.Visible = TRUE
		lm_popup.m_options.m_viewtaxinformation.Text = 'View Details'
		lm_popup.m_options.m_print.Visible = FALSE 
		lm_popup.m_options.m_1.Visible = FALSE
		lm_popup.m_options.PopMenu(w_frame.PointerX( ), w_frame.PointerY( ))
		
		Destroy lm_popup
	END IF
END IF
end event

event rowfocuschanging;call super::rowfocuschanging;THIS.SelectRow(currentrow,FALSE)
THIS.SelectRow(newrow, TRUE)
end event

type dw_details from u_dw_online within w_income_requests
event ue_dwnkey pbm_dwnkey
event ue_command pbm_command
event ue_mousemove pbm_mousemove
integer x = 1083
integer y = 1228
integer width = 1413
integer height = 308
integer taborder = 90
boolean bringtotop = true
string dataobject = "d_pending_details"
boolean border = false
boolean righttoleft = true
end type

event itemchanged;call super::itemchanged;wf_enable('CREATE')
dw_details.SetColumn('iv_request_comment')
end event

event editchanged;call super::editchanged;wf_enable('CREATE')
end event

event rbuttondown;RETURN 0
end event

type dw_tax_years from u_dw_online within w_income_requests
integer x = 187
integer y = 1240
integer width = 704
integer height = 404
integer taborder = 20
boolean bringtotop = true
string dataobject = "d_pending_details_years"
boolean border = false
end type

event editchanged;call super::editchanged;
STRING ls_data

IF data = '' THEN dw_tax_years.SetItem(row,'tax_year',0)

wf_enable('CREATE')

end event

event rbuttondown;RETURN 0
end event

type st_details from statictext within w_income_requests
integer x = 18
integer y = 1120
integer width = 2592
integer height = 80
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = " Request Detail Information"
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_who from statictext within w_income_requests
integer x = 933
integer y = 304
integer width = 1664
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16711680
long backcolor = 67108864
boolean focusrectangle = false
end type

