$PBExportHeader$w_rx_coverage.srw
forward
global type w_rx_coverage from window
end type
type st_3 from statictext within w_rx_coverage
end type
type dw_3 from u_dw_online within w_rx_coverage
end type
type dw_4 from u_dw_online within w_rx_coverage
end type
type dw_2 from u_dw_online within w_rx_coverage
end type
type st_2 from statictext within w_rx_coverage
end type
type dw_1 from u_dw_online within w_rx_coverage
end type
type st_1 from statictext within w_rx_coverage
end type
type cb_close from commandbutton within w_rx_coverage
end type
type dw_5 from u_dw_online within w_rx_coverage
end type
end forward

global type w_rx_coverage from window
integer width = 3063
integer height = 2692
boolean titlebar = true
string title = "Rx Coverage for Registered Claimiants"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 67108864
string icon = "AppIcon!"
st_3 st_3
dw_3 dw_3
dw_4 dw_4
dw_2 dw_2
st_2 st_2
dw_1 dw_1
st_1 st_1
cb_close cb_close
dw_5 dw_5
end type
global w_rx_coverage w_rx_coverage

type variables
LONG		il_claim_no

w_sheet	iw_active_sheet
end variables

event open;LONG	ll_row1, ll_row2, ll_row3, ll_rows
DATE	ldt_primary_start_date, ldt_primary_end_date , ldt_secondary_end_date
STRING	ls_coverage, ls_start, ls_end, ls_nature_of_injury_code, ls_primary_formulary_code, ls_secondary_formulary_code
BOOLEAN 	lb_set_dates = FALSE
s_window_message lstr_message
N_BR_BLUECROSS	 lnv_bc
INTEGER li_rtn, li_find , li_rowcount, li_counter

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


SetPointer(HourGlass!)
dw_1.Reset()
dw_4.Reset()
dw_5.Reset()
dw_1.Visible = FALSE
dw_4.Visible = FALSE
dw_5.Visible = FALSE

lstr_message = Message.PowerObjectParm

ls_coverage = lstr_message.as_stringparm[1]

IF ls_coverage = 'registered' THEN
	dw_1.dataobject = 'd_rxcov_header'
	dw_4.dataobject = 'd_rxcov_pending_eligibility'
	dw_5.dataobject = 'd_rxcov_pend_formulary'
ELSE
	dw_1.dataobject = 'd_rx_pending_coverage'
	dw_4.dataobject = 'd_rxcov_pending_ecoverage'
	dw_5.dataobject = 'd_rxcov_pend_fcoverage'
	lb_set_dates = TRUE
END IF

dw_1.Visible = TRUE
dw_4.Visible = TRUE
dw_5.Visible = TRUE


iw_active_sheet = w_frame.GetActiveSheet()
il_claim_no = iw_active_sheet.dw_basic_claim.GetItemNumber(1,'claim_no')

//Retrieve Claim & Registration Date
dw_1.SetTransObject(SQLCA)
ll_row3 = dw_1.Retrieve(il_claim_no)
SQLCA.nf_handle_error('w_rx_coverage','open','dw_1.Retrieve(ll_claim_no)') 

//Retrieve BC Eligibility 
dw_2.SetTransObject(SQLCA)
ll_row1 = dw_2.Retrieve(il_claim_no)
SQLCA.nf_handle_error('w_rx_coverage','open','dw_2.Retrieve(ll_claim_no)') 

//Retrieve BC Formulary
dw_3.SetTransObject(SQLCA)
dw_3.Retrieve(il_claim_no)
SQLCA.nf_handle_error('w_rx_coverage','open','dw_3.Retrieve(ll_claim_no)') 

//Retrieve WHSCC Eligibility
dw_4.SetTransObject(SQLCA)
ll_row2 = dw_4.Retrieve(il_claim_no)
SQLCA.nf_handle_error('w_rx_coverage','open','dw_4.Retrieve(ll_claim_no)') 

//Retrieve WHSCC Formulary
dw_5.SetTransObject(SQLCA)
ll_rows = dw_5.Retrieve(il_claim_no)
SQLCA.nf_handle_error('w_rx_coverage','open','dw_5.Retrieve(ll_claim_no)') 

IF lb_set_dates = TRUE THEN
	lnv_bc = Create n_br_bluecross
	ldt_primary_start_date = lnv_bc.uf_determine_effective_start_date(il_claim_no)
	IF ISNULL(ldt_primary_start_date) THEN
		MessageBox('Cannot Open','This claim is Not Eligible to be registered at this time.',Exclamation!,OK!)
		Return -1
	END IF	
	li_rtn = lnv_bc.uf_determine_termination_date(il_claim_no,ldt_primary_end_date)
	IF li_rtn = 1 THEN
		ls_start = STRING(ldt_primary_start_date,"yyyy-mm-dd")
		ls_end = STRING(ldt_primary_end_date,"yyyy-mm-dd")
		IF ISNULL(ls_end) OR ls_end = '' THEN ls_end = "Open-Ended"
		
		dw_4.Modify("st_eligibility_start_date.Text = '" + ls_start + "'")
		dw_4.Modify("st_eligibility_end_date.Text = '" + ls_end + "'")
		dw_5.SetItem(1,'start_date',ls_start )
		dw_5.SetItem(1,'end_date',ls_end )
		
		li_rowcount = dw_5.RowCount()
		IF li_rowcount > 1 THEN
			ls_primary_formulary_code = dw_5.GetItemString(1,'formulary_code')
			
			// If there are no automatic secondary formularies pending, then 
			// there will only be one row in dw_5 (the pending primary)
			// and this FOR loop will not execute
			FOR li_counter = 2 to li_rowcount
				dw_5.SetItem(li_counter,'start_date',ls_start )
				
				ls_secondary_formulary_code = dw_5.GetItemString(li_counter,'formulary_code')
				li_rtn = lnv_bc.uf_determine_secondary_termination_date(ldt_primary_start_date,ldt_primary_end_date,ls_primary_formulary_code,ls_secondary_formulary_code,ldt_secondary_end_date)
				ls_end = String(ldt_secondary_end_date,"yyyy-mm-dd")
				IF ISNULL(ls_end) OR ls_end = '' THEN ls_end = 'Open-Ended'
				dw_5.SetItem(li_counter,'end_date', ls_end )				
			NEXT
		END IF
		
	ELSE
		MessageBox('Cannot Open','This claim is Not Eligible to be registered at this time.',Exclamation!,OK!)
		Return -1		
	END IF
	
END IF


IF (ll_row1 <= 0 AND ll_row2 <= 0 AND ls_coverage = 'registered') OR &
    (ll_row3 <= 0 AND ls_coverage = 'pending') THEN
	
	SELECT nature_of_injury_code
	INTO   :ls_nature_of_injury_code
	FROM   ACCIDENT
	WHERE  claim_no = :il_claim_no
	USING  SQLCA;
	
	SQLCA.nf_handle_error("w_rx_coverage", "open", "SELECT on ACCIDENT")
	
	IF IsNull(ls_nature_of_injury_code) OR ls_nature_of_injury_code = '' THEN
		MessageBox('ERROR','The Nature of Injury Code is missing, cannot display Coverage.',Exclamation!,ok!)
	ELSE
		MessageBox('Error','There is no information to display.',information!)
	END IF
	
	CLOSE(this)
END IF
end event

on w_rx_coverage.create
this.st_3=create st_3
this.dw_3=create dw_3
this.dw_4=create dw_4
this.dw_2=create dw_2
this.st_2=create st_2
this.dw_1=create dw_1
this.st_1=create st_1
this.cb_close=create cb_close
this.dw_5=create dw_5
this.Control[]={this.st_3,&
this.dw_3,&
this.dw_4,&
this.dw_2,&
this.st_2,&
this.dw_1,&
this.st_1,&
this.cb_close,&
this.dw_5}
end on

on w_rx_coverage.destroy
destroy(this.st_3)
destroy(this.dw_3)
destroy(this.dw_4)
destroy(this.dw_2)
destroy(this.st_2)
destroy(this.dw_1)
destroy(this.st_1)
destroy(this.cb_close)
destroy(this.dw_5)
end on

type st_3 from statictext within w_rx_coverage
integer x = 37
integer y = 1376
integer width = 1106
integer height = 64
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Pending WorkSafeNB Coverage"
boolean focusrectangle = false
end type

type dw_3 from u_dw_online within w_rx_coverage
integer x = 23
integer y = 848
integer width = 3003
integer height = 480
integer taborder = 30
string title = "none"
string dataobject = "d_rxcov_bc_formulary"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

type dw_4 from u_dw_online within w_rx_coverage
integer x = 23
integer y = 1472
integer width = 3003
integer height = 480
integer taborder = 30
string title = "none"
string dataobject = "d_rxcov_pending_eligibility"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

type dw_2 from u_dw_online within w_rx_coverage
integer x = 23
integer y = 352
integer width = 3003
integer height = 480
integer taborder = 20
string title = "none"
string dataobject = "d_rxcov_bc_eligibility"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

type st_2 from statictext within w_rx_coverage
integer x = 32
integer y = 248
integer width = 768
integer height = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Current BlueCross Coverage "
boolean focusrectangle = false
end type

type dw_1 from u_dw_online within w_rx_coverage
integer x = 9
integer y = 100
integer width = 2021
integer height = 108
integer taborder = 10
string title = "none"
string dataobject = "d_rxcov_header"
boolean border = false
end type

type st_1 from statictext within w_rx_coverage
integer x = 9
integer y = 8
integer width = 3026
integer height = 72
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Rx Coverage Inquiry"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cb_close from commandbutton within w_rx_coverage
integer x = 2629
integer y = 2480
integer width = 402
integer height = 104
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Close"
end type

event clicked;	SetPointer(HourGlass!)
	Close(parent)
end event

type dw_5 from u_dw_online within w_rx_coverage
integer x = 23
integer y = 1972
integer width = 3003
integer height = 480
integer taborder = 10
string title = "none"
string dataobject = "d_rxcov_pend_formulary"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

