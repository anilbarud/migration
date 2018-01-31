$PBExportHeader$w_close_previous_coc_period.srw
forward
global type w_close_previous_coc_period from window
end type
type st_3 from statictext within w_close_previous_coc_period
end type
type st_2 from statictext within w_close_previous_coc_period
end type
type cb_close from commandbutton within w_close_previous_coc_period
end type
type cb_close_coc_period from commandbutton within w_close_previous_coc_period
end type
type cb_update from commandbutton within w_close_previous_coc_period
end type
type dw_unapplied_txns_for_prev_period from u_datawindow within w_close_previous_coc_period
end type
type st_1 from statictext within w_close_previous_coc_period
end type
end forward

global type w_close_previous_coc_period from window
integer width = 3333
integer height = 2796
boolean titlebar = true
string title = "Close Previous Cost of Claims Period"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
st_3 st_3
st_2 st_2
cb_close cb_close
cb_close_coc_period cb_close_coc_period
cb_update cb_update
dw_unapplied_txns_for_prev_period dw_unapplied_txns_for_prev_period
st_1 st_1
end type
global w_close_previous_coc_period w_close_previous_coc_period

type variables
LONG il_current_coc_period, il_previous_coc_period
end variables

on w_close_previous_coc_period.create
this.st_3=create st_3
this.st_2=create st_2
this.cb_close=create cb_close
this.cb_close_coc_period=create cb_close_coc_period
this.cb_update=create cb_update
this.dw_unapplied_txns_for_prev_period=create dw_unapplied_txns_for_prev_period
this.st_1=create st_1
this.Control[]={this.st_3,&
this.st_2,&
this.cb_close,&
this.cb_close_coc_period,&
this.cb_update,&
this.dw_unapplied_txns_for_prev_period,&
this.st_1}
end on

on w_close_previous_coc_period.destroy
destroy(this.st_3)
destroy(this.st_2)
destroy(this.cb_close)
destroy(this.cb_close_coc_period)
destroy(this.cb_update)
destroy(this.dw_unapplied_txns_for_prev_period)
destroy(this.st_1)
end on

event open;/*	This option closes the previous Cost of Claims Posting Period.
*/

INTEGER	li_return_code, li_year, li_month, li_rtn
LONG		ll_unapplied_rowcount, ll_previous_coc_period
STRING	ls_previous_coc_closed_flag, ls_current_date, ls_txn_type_code, ls_module
DATETIME	ldt_current_datetime

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


/*	Determine the current Cost of Claims posting period. The current period is always the current
	day and month.
*/
ldt_current_datetime = f_server_datetime()
li_year = Year(Date(ldt_current_datetime)) 
li_month = Month(Date(ldt_current_datetime))


/*	Make sure that the Cost of Claims period is 6 digits long. (i.e. if year is 1997 & 
	month is 6, current CoC period should be 199706, not 19976.
*/
ls_current_date = String(li_year,'0000') + String(li_month,'00')
il_current_coc_period = Long(ls_current_date)



/*	Get the most recent Coc Control record.
*/
SELECT previous_coc_period,
       previous_coc_closed_flag
INTO   :il_previous_coc_period,
       :ls_previous_coc_closed_flag
FROM   Coc_Control
WHERE  current_coc_period = :il_current_coc_period
USING SQLCA;
SQLCA.nf_handle_error('w_close_previous_coc_period','Embedded SQL: select previous_coc_period, current_coc_period, previous_coc_closed_flag from Coc_Control...','Open event')


/*	If the period is already closed, you cannot close it again.
*/
IF ls_previous_coc_closed_flag = 'Y' THEN
	MessageBox('Cannot Close ' + String(ll_previous_coc_period,'0000-00'),'The previous Cost of Claims period is already closed!',Exclamation!)
	RETURN
ELSEIF ls_previous_coc_closed_flag = '' OR IsNull(ls_previous_coc_closed_flag) THEN
	MessageBox('No Period','There is no current Cost of Claims period. Contact the HELPDESK.',Exclamation!)
	cb_close.TriggerEvent(Clicked!)
	RETURN
END IF

IF il_previous_coc_period = il_current_coc_period THEN
	MessageBox('Period Error','You cannot close the current cost of claims period: ' + String(ll_previous_coc_period,'0000-00')+ '.',Exclamation!)
	RETURN
END IF



// Prior to closing the previous coc period, ensure that there are no unprocessed txns for that period
ll_unapplied_rowcount = dw_unapplied_txns_for_prev_period.Retrieve(il_previous_coc_period,il_current_coc_period)
IF ll_unapplied_rowcount = 0 THEN
	MessageBox('No Unposted Transactions','There are no unposted transactions. You may close the Cost of Claims period.')
	cb_close_coc_period.Enabled               = TRUE
ELSE
	dw_unapplied_txns_for_prev_period.Enabled = TRUE
	cb_update.Enabled                         = TRUE
END IF


end event

type st_3 from statictext within w_close_previous_coc_period
integer x = 101
integer y = 48
integer width = 3099
integer height = 64
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "All scheduled payments and transactions will be moved to the current CoC Period."
alignment alignment = center!
boolean focusrectangle = false
end type

type st_2 from statictext within w_close_previous_coc_period
integer x = 27
integer y = 32
integer width = 3259
integer height = 100
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cb_close from commandbutton within w_close_previous_coc_period
integer x = 2885
integer y = 2572
integer width = 402
integer height = 104
integer taborder = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Close"
end type

event clicked;CLOSE(PARENT)
end event

type cb_close_coc_period from commandbutton within w_close_previous_coc_period
integer x = 878
integer y = 2572
integer width = 741
integer height = 104
integer taborder = 50
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Close Previous Coc Period"
end type

event clicked;INTEGER  li_msg, li_rtn
LONG     ll_unapplied_txn_count
DATETIME	ldt_current_datetime


N_PROCESS_RUN_STATUS ln_process_run_status

/******************************************************************************************
P10275 - Daytime Payment Processing
- added new function call to prevent updating of tables used by PRODSVCS Payment Processing
- new object N_PROCESS_RUN_STATUS is used to determine status of processes (in progress = Y/N)
- '013' refers to the Close Coc Period module, '044' refers to the Payment Processing module
******************************************************************************************/
ln_process_run_status = Create N_PROCESS_RUN_STATUS

li_rtn = ln_process_run_status.nf_in_progress('013','044','close of the CoC period',SQLCA)

IF li_rtn = 1 THEN
	// module is blocked
	return
END IF
/******************************************************************************************/




SELECT COUNT(*)
INTO   :ll_unapplied_txn_count
FROM   UNAPPLIED_CLAIM_TXN
WHERE  coc_period  <> 0
AND    coc_period  <= :il_previous_coc_period
USING SQLCA;
SQLCA.nf_handle_error('w_close_previous_coc_period','embedded SQL: SELECT COUNT(*) FROM PAYMENT, UNAPPLIED_CLAIM_TXN...(1)','cb_close_coc_period.clicked')

IF IsNull(ll_unapplied_txn_count) THEN
	ll_unapplied_txn_count = 0
END IF

IF ll_unapplied_txn_count > 0 THEN
	MessageBox('Unposted Transactions','There are unposted transactions associated with the Cost of Claims period that you are attempting to close.',Exclamation!)
	RETURN
END IF


/*	Confirm Closing the period.
*/
li_msg = MessageBox('Confirm Close of Period','Do you wish to close ' + String(il_previous_coc_period,'0000-00') + '?',Question!,YesNo!,2)
IF li_msg = 2 THEN
	RETURN
END IF

	
/*	If they want to close the period, update the Coc_Control record.
*/

ldt_current_datetime = f_server_datetime()


SQLCA.nf_begin_transaction()

UPDATE Coc_Control
SET    previous_coc_closed_flag = 'Y',
		 previous_coc_closed_date = :ldt_current_datetime
WHERE current_coc_period = :il_current_coc_period
USING SQLCA;
SQLCA.nf_handle_error('w_close_previous_coc_period','Embedded SQL: Update Coc_Control','cb_close_coc_period.clicked')

SQLCA.nf_commit_transaction()


MessageBox('Period Closed','The previous Cost of Claims period is now closed!',Exclamation!)
cb_close_coc_period.Enabled = FALSE

end event

type cb_update from commandbutton within w_close_previous_coc_period
integer x = 27
integer y = 2572
integer width = 850
integer height = 104
integer taborder = 40
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Update to Current Coc Period"
end type

event clicked;INTEGER     li_row, ll_unapplied_rowcount, li_array_upperbound
LONG        ll_current_payment_no, ll_payments_to_update[]


ll_unapplied_rowcount = dw_unapplied_txns_for_prev_period.RowCount()


// create temp table to hold all txns to be updated
EXECUTE IMMEDIATE 'CREATE TABLE #txns ( payment_no   INT   NOT NULL)'
USING SQLCA;
SQLCA.nf_handle_error('w_close_previous_coc_period','embedded SQL: CREATE TABLE #txns','cb_update.clicked')


FOR li_row = 1 TO ll_unapplied_rowcount
	li_array_upperbound = UpperBound(ll_payments_to_update)
	
	ll_current_payment_no = dw_unapplied_txns_for_prev_period.GetItemNumber(li_row,'payment_no')
	
	ll_payments_to_update[li_array_upperbound + 1] = ll_current_payment_no
	
	INSERT #txns
	( payment_no )
	SELECT :ll_current_payment_no 
	USING SQLCA;
	SQLCA.nf_handle_error('w_close_previous_coc_period','embedded SQL: INSERT #txns...','cb_update.clicked')		

NEXT


SQLCA.nf_begin_transaction()

IF UpperBound(ll_payments_to_update) > 0 THEN
	
	// update payments in the temp table
	UPDATE a
	SET    a.coc_period = :il_current_coc_period
	FROM   UNAPPLIED_CLAIM_TXN a
	JOIN   #txns               b ON a.payment_no = b.payment_no
	USING SQLCA;
	SQLCA.nf_handle_error('w_close_previous_coc_period','embedded SQL: UPDATE UNAPPLIED_CLAIM_TXN...','cb_update.clicked')
ELSE
	MessageBox('No Payments Selected','You must select at least one payment to update the Coc period of its transactions.',Exclamation!)

	// drop temp table
	EXECUTE IMMEDIATE 'DROP TABLE #txns'
	USING SQLCA;
	SQLCA.nf_handle_error('w_close_previous_coc_period','embedded SQL: DROP TABLE #txns','cb_update.clicked')

	RETURN
END IF

SQLCA.nf_commit_transaction()


// drop temp table
EXECUTE IMMEDIATE 'DROP TABLE #txns'
USING SQLCA;
SQLCA.nf_handle_error('w_close_previous_coc_period','embedded SQL: DROP TABLE #txns','cb_update.clicked')


// Prior to closing the previous coc period, ensure that there are no unprocessed txns for that period
ll_unapplied_rowcount = dw_unapplied_txns_for_prev_period.Retrieve(il_previous_coc_period,il_current_coc_period)
IF ll_unapplied_rowcount = 0 THEN
	MessageBox('No Unposted Transactions','There are no unposted transactions. You may close the Cost of Claims period.')
	dw_unapplied_txns_for_prev_period.Enabled = FALSE
	cb_update.Enabled                         = FALSE
	cb_close_coc_period.Enabled               = TRUE
ELSE
	dw_unapplied_txns_for_prev_period.Enabled = TRUE
	cb_update.Enabled                         = TRUE
END IF

end event

type dw_unapplied_txns_for_prev_period from u_datawindow within w_close_previous_coc_period
integer x = 82
integer y = 244
integer width = 3150
integer height = 2224
integer taborder = 10
boolean enabled = false
string dataobject = "d_unapplied_txns_for_prev_period"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event constructor;call super::constructor;THIS.SetTransObject(SQLCA)
end event

event rbuttondown;M_DW_RMB_POPUP lm_popup
WINDOW         lw_frame

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

lw_frame = PARENT.PARENTWINDOW()


lm_popup.m_options.PopMenu(lw_frame.PointerX( ), lw_frame.PointerY( ))

Destroy lm_popup

end event

type st_1 from statictext within w_close_previous_coc_period
integer x = 27
integer y = 188
integer width = 3259
integer height = 2340
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

