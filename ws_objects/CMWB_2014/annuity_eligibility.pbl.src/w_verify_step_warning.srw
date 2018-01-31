$PBExportHeader$w_verify_step_warning.srw
forward
global type w_verify_step_warning from window
end type
type mle_red_warning_2 from multilineedit within w_verify_step_warning
end type
type mle_red_warning from multilineedit within w_verify_step_warning
end type
type st_click from statictext within w_verify_step_warning
end type
type dw_sub_ledger_adjustments from u_datawindow within w_verify_step_warning
end type
type cb_cancel from commandbutton within w_verify_step_warning
end type
type cb_ok from commandbutton within w_verify_step_warning
end type
end forward

global type w_verify_step_warning from window
integer width = 2464
integer height = 2076
boolean titlebar = true
string title = "Verify Benefit Entitlement Step Warning"
windowtype windowtype = response!
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
mle_red_warning_2 mle_red_warning_2
mle_red_warning mle_red_warning
st_click st_click
dw_sub_ledger_adjustments dw_sub_ledger_adjustments
cb_cancel cb_cancel
cb_ok cb_ok
end type
global w_verify_step_warning w_verify_step_warning

forward prototypes
public function w_verify_step_warning wf_get_window_reference ()
end prototypes

public function w_verify_step_warning wf_get_window_reference ();RETURN THIS
end function

on w_verify_step_warning.create
this.mle_red_warning_2=create mle_red_warning_2
this.mle_red_warning=create mle_red_warning
this.st_click=create st_click
this.dw_sub_ledger_adjustments=create dw_sub_ledger_adjustments
this.cb_cancel=create cb_cancel
this.cb_ok=create cb_ok
this.Control[]={this.mle_red_warning_2,&
this.mle_red_warning,&
this.st_click,&
this.dw_sub_ledger_adjustments,&
this.cb_cancel,&
this.cb_ok}
end on

on w_verify_step_warning.destroy
destroy(this.mle_red_warning_2)
destroy(this.mle_red_warning)
destroy(this.st_click)
destroy(this.dw_sub_ledger_adjustments)
destroy(this.cb_cancel)
destroy(this.cb_ok)
end on

event open;DATETIME             ldtm_annuity_start_date, ldtm_annuity_end_date, ldtm_null, ldtm_annuity_eligibility_end_date_used
DECIMAL              ldec_annuity_percentage
INTEGER              li_rows
LONG                 ll_annuity_account_no, ll_annuity_eligibility_no, ll_claim_no, ll_individual_no, ll_annuity_payout_no
STRING               ls_claim_role_code, ls_window_mode
s_verify_warning     lstr_verify_warning

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


lstr_verify_warning = Message.PowerObjectParm

ll_annuity_account_no                  = lstr_verify_warning.annuity_account_no
ll_annuity_eligibility_no              = lstr_verify_warning.annuity_eligibility_no
ll_annuity_payout_no                   = lstr_verify_warning.annuity_payout_no
ll_claim_no                            = lstr_verify_warning.claim_no
ll_individual_no                       = lstr_verify_warning.individual_no
ls_claim_role_code                     = lstr_verify_warning.claim_role_code
ldtm_annuity_start_date                = lstr_verify_warning.annuity_start_date
ldtm_annuity_eligibility_end_date_used = lstr_verify_warning.annuity_eligibility_end_date_used
ldtm_annuity_end_date                  = lstr_verify_warning.annuity_end_date
ldec_annuity_percentage                = lstr_verify_warning.annuity_percentage
ls_window_mode                         = lstr_verify_warning.window_mode

IF String(ldtm_annuity_start_date,'yyyy-mm-dd') = '1900-01-01' OR String(ldtm_annuity_end_date,'yyyy-mm-dd')  = '1900-01-01' THEN
	SetNull(ldtm_null)
	ldtm_annuity_start_date = ldtm_null
	ldtm_annuity_end_date   = ldtm_null	
END IF


CHOOSE CASE ls_window_mode
	CASE 'warning','inquiry'
		
		IF ls_window_mode = 'warning' THEN
			dw_sub_ledger_adjustments.DataObject = 'd_sub_ledger_adjustments_by_claim_warning'
			st_click.Visible = TRUE
			mle_red_warning.Visible = FALSE
			mle_red_warning_2.Visible = FALSE
			cb_ok.Visible = TRUE
			cb_cancel.Text = '&Cancel'
			THIS.Title = 'Verify Benefit Entitlement Step Warning'
		ELSEIF ls_window_mode = 'inquiry' THEN
			dw_sub_ledger_adjustments.DataObject = 'd_sub_ledger_adjustments_by_claim_inquiry'
			st_click.Visible = FALSE
			mle_red_warning.Visible = FALSE
			mle_red_warning_2.Visible = FALSE
			cb_ok.Visible = FALSE
			cb_cancel.Text = '&Close'
			THIS.Title = 'Annuity Benefit Inquiry'
		END IF
		dw_sub_ledger_adjustments.SetTransObject(SQLCA)

		/*
																	@annuity_account_no             
																	@annuity_calc_reason_code 
																	
																	@annuity_eligibility_no         
																	@annuity_payout_no              
																	@annuity_start_date             
																	@annuity_eligibility_end_date_used
																	@annuity_end_date               
																	
																	@annuity_set_aside_percent      
																	@mode                           
		*/
		li_rows = dw_sub_ledger_adjustments.Retrieve(ll_annuity_account_no                                                , &
		                                             '05'                                                                 , &																	
																	ll_annuity_eligibility_no                                            , &
																	ll_annuity_payout_no                                                 , &
																	DateTime(String(ldtm_annuity_start_date,'yyyy-mm-dd'))               , &
																	DateTime(String(ldtm_annuity_eligibility_end_date_used,'yyyy-mm-dd')), &
																	DateTime(String(ldtm_annuity_end_date,'yyyy-mm-dd'))                 , &
																	ldec_annuity_percentage                                              , &
																	'display')                                              
		SQLCA.nf_handle_error('w_verify_step_warning', 'retrieve dw_sub_ledger_adjustments', 'open event')
		IF li_rows = 0 THEN
			CloseWithReturn(THIS,'continue')
		END IF
		
	CASE ELSE
		dw_sub_ledger_adjustments.Visible = FALSE
		st_click.Y = 530
		mle_red_warning.Visible = TRUE
		cb_ok.Y = 680
		cb_cancel.Y = 680
		THIS.Title = 'Verify Benefit Entitlement Step Warning'
		IF ls_window_mode = 'NRA' THEN
			THIS.Title = THIS.Title + ' - Annuity Calculation Not Required'
		ELSEIF ls_window_mode = 'NAA' THEN
			THIS.Title = THIS.Title + ' - Annuity Calculation Not Allowed'
		END IF
		THIS.Height = 980
		
END CHOOSE



end event

type mle_red_warning_2 from multilineedit within w_verify_step_warning
integer x = 91
integer y = 296
integer width = 2286
integer height = 224
integer taborder = 30
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 255
long backcolor = 67108864
string text = "If Benefit Entitlement requires correction, just click the Cancel button below, and click on the Verify Benefit Entitlement button to re-open the module  before completing this step."
boolean border = false
end type

type mle_red_warning from multilineedit within w_verify_step_warning
integer x = 91
integer y = 68
integer width = 2286
integer height = 224
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 255
long backcolor = 67108864
string text = "You MUST ensure ALL Benefit Entitlement is set up correctly. Otherwise, Annuity Eligibility may be INCORRECTLY established (ex. not eligible, incorrect eligibility period, no benefit entitlement after qualification, etc.)."
boolean border = false
end type

type st_click from statictext within w_verify_step_warning
integer x = 87
integer y = 1688
integer width = 2249
integer height = 116
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
string text = "Click OK to save the checklist step as ~'Completed~' or click Cancel to leave the step as ~'Incomplete~'"
boolean focusrectangle = false
end type

type dw_sub_ledger_adjustments from u_datawindow within w_verify_step_warning
integer x = 73
integer y = 64
integer width = 2299
integer height = 1576
integer taborder = 10
string dataobject = "d_sub_ledger_adjustments_by_claim_warning"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event rbuttondown;M_DW_RMB_POPUP lm_popup
window		lw_window

IF IsValid(inv_rowselection) Then
	inv_rowselection.event pfc_rbuttondown(xpos,ypos,row,dwo)
End if


/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/
lm_popup = Create m_dw_rmb_popup
lm_popup.mf_set_datawindow(This)

// don't display lines
lm_popup.m_options.m_1.Visible = FALSE
lm_popup.m_options.m_2.Visible = FALSE

lw_window = wf_get_window_reference()	

lm_popup.m_options.PopMenu(lw_window.PointerX( ), lw_window.PointerY( ))

Destroy lm_popup
end event

type cb_cancel from commandbutton within w_verify_step_warning
integer x = 1934
integer y = 1852
integer width = 402
integer height = 104
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cance&l"
boolean default = true
end type

event clicked;CloseWithReturn(PARENT,'cancel')
end event

type cb_ok from commandbutton within w_verify_step_warning
integer x = 1385
integer y = 1852
integer width = 402
integer height = 104
integer taborder = 10
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
end type

event clicked;CloseWithReturn(PARENT,'calculate')
end event

