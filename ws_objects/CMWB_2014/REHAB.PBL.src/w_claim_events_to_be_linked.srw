$PBExportHeader$w_claim_events_to_be_linked.srw
forward
global type w_claim_events_to_be_linked from window
end type
type dw_rehab_task_progress_note_record from u_dw_online within w_claim_events_to_be_linked
end type
type cb_close from commandbutton within w_claim_events_to_be_linked
end type
type cb_link from commandbutton within w_claim_events_to_be_linked
end type
type dw_claim_events_to_be_linked from u_dw_online within w_claim_events_to_be_linked
end type
end forward

global type w_claim_events_to_be_linked from window
integer x = 1074
integer y = 480
integer width = 2606
integer height = 1520
boolean titlebar = true
string title = "Link Events To Rehab Plans"
windowtype windowtype = response!
long backcolor = 67108864
dw_rehab_task_progress_note_record dw_rehab_task_progress_note_record
cb_close cb_close
cb_link cb_link
dw_claim_events_to_be_linked dw_claim_events_to_be_linked
end type
global w_claim_events_to_be_linked w_claim_events_to_be_linked

type variables
LONG		il_claim_no
LONG		il_task_no
STRING		is_did_a_link
end variables

forward prototypes
public subroutine wf_retrieve_claim_events ()
end prototypes

public subroutine wf_retrieve_claim_events ();/*	This function is used to select the CLAIM_EVENT records for a given claim number
	that are not currently linked to a Rehab Plan.
*/
	LONG	ll_rows

	ll_rows = dw_claim_events_to_be_linked.Retrieve(il_claim_no)
	
	IF SQLCA.nf_Handle_Error("w_claim_events_to_be_linked","dw_claim_events_to_be_linked.Retrieve(il_claim_no)","Open Event")	< 0 THEN
		cb_close.TriggerEvent(Clicked!)
//		Close(w_claim_events_to_be_linked)
		Return
	END IF

/*	If any rows returned, highlight the first one.
*/
	IF ll_rows > 0 THEN
		dw_claim_events_to_be_linked.SetFocus()
		dw_claim_events_to_be_linked.uf_processselect(1,"Mouse")
	END IF
	
end subroutine

on w_claim_events_to_be_linked.create
this.dw_rehab_task_progress_note_record=create dw_rehab_task_progress_note_record
this.cb_close=create cb_close
this.cb_link=create cb_link
this.dw_claim_events_to_be_linked=create dw_claim_events_to_be_linked
this.Control[]={this.dw_rehab_task_progress_note_record,&
this.cb_close,&
this.cb_link,&
this.dw_claim_events_to_be_linked}
end on

on w_claim_events_to_be_linked.destroy
destroy(this.dw_rehab_task_progress_note_record)
destroy(this.cb_close)
destroy(this.cb_link)
destroy(this.dw_claim_events_to_be_linked)
end on

event open;/*	Get the retrieval argument out of the Message object and then 
	retrieve all the non linked claim events for that claim.
*/
	S_WINDOW_MESSAGE	lstr_message
	LONG					ll_rows
	
INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


	dw_claim_events_to_be_linked.SetTransObject(SQLCA)
	dw_rehab_task_progress_note_record.SetTransObject(SQLCA)
	dw_claim_events_to_be_linked.uf_setselect(1)
	
	lstr_message = Message.PowerObjectParm
	il_claim_no = lstr_message.al_doubleparm[1]
	il_task_no = lstr_message.al_doubleparm[2]
	is_did_a_link = 'NO'
	
	wf_retrieve_claim_events()
	
end event

type dw_rehab_task_progress_note_record from u_dw_online within w_claim_events_to_be_linked
boolean visible = false
integer x = 1586
integer y = 1240
integer width = 905
integer height = 156
integer taborder = 11
string dataobject = "d_new_rehab_task_progress_note"
end type

type cb_close from commandbutton within w_claim_events_to_be_linked
integer x = 1257
integer y = 1276
integer width = 279
integer height = 108
integer taborder = 10
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Close"
end type

event clicked;CloseWithReturn(PARENT,is_did_a_link)

end event

type cb_link from commandbutton within w_claim_events_to_be_linked
integer x = 933
integer y = 1276
integer width = 279
integer height = 108
integer taborder = 30
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Link"
end type

event clicked;/*	Reset the datawindow and then insert the new record to be saved.
*/
	LONG	ll_selected_row, ll_event_no, ll_record_exists
	INTEGER		li_rtn
	N_PROCESS_RUN_STATUS ln_process_run_status
	
	/******************************************************************************************
	P10275 - Daytime Payment Processing
	- added new function call to prevent updating of tables used by PRODSVCS Payment Processing
	- new object N_PROCESS_RUN_STATUS is used to determine status of processes (in progress = Y/N)
	- '025' refers to the Rehab Plan Maintenance module, '044' refers to the Payment Processing module
	******************************************************************************************/
	ln_process_run_status = Create N_PROCESS_RUN_STATUS
	
	li_rtn = ln_process_run_status.nf_in_progress('025','044','linking of an event',SQLCA)
	
	IF li_rtn = 1 THEN
		// module is blocked
		return
	END IF
	/******************************************************************************************/

	
	ll_selected_row = dw_claim_events_to_be_linked.GetSelectedRow(0)
	
	IF ll_selected_row > 0 THEN
		ll_event_no = dw_claim_events_to_be_linked.GetItemNumber(ll_selected_row,'event_no')
		dw_rehab_task_progress_note_record.Reset()
		dw_rehab_task_progress_note_record.InsertRow(0)
		dw_rehab_task_progress_note_record.SetItem(1,'claim_no',il_claim_no)
		dw_rehab_task_progress_note_record.SetItem(1,'task_no',il_task_no)
		dw_rehab_task_progress_note_record.SetItem(1,'event_no',ll_event_no)
		
		SELECT count(*)
		  INTO :ll_record_exists
		  FROM REHAB_TASK_PROGRESS_NOTE
		 WHERE claim_no = :il_claim_no
		   AND task_no = :il_task_no
			AND event_no = :ll_event_no
		 USING SQLCA;
		
		IF SQLCA.nf_Handle_Error("w_claim_events_to_be_linked","SELECT count(*) INTO :ll_record_exists","cb_link - Clicked!") < 0 THEN
			MessageBox("Claim Event Link Error","An error occured while determining if event " + String(ll_event_no) + " was already linked.",Exclamation!)
			Close(PARENT)
		ELSE
			IF ll_record_exists > 0 THEN
				MessageBox("Claim Event Link Error","Event " + String(ll_event_no) + " is already linked to task " + String(il_task_no) + ".",Exclamation!)
			ELSE
				SQLCA.nf_begin_transaction()
				
				dw_rehab_task_progress_note_record.Update()
				IF SQLCA.nf_Handle_Error("w_claim_events_to_be_linked","dw_rehab_task_progress_note_record.Update()","cb_link - Clicked!")	< 0 THEN
					MessageBox("Claim Event Link Error","An error occured while linking the claim event.",Exclamation!)
					Close(PARENT)
				ELSE
					SQLCA.nf_commit_transaction()
					
					MessageBox("Claim Event Link","The linking of event " + String(ll_event_no) + " to task " + String(il_task_no) + " was successful.",Exclamation!)
					is_did_a_link = 'YES'
					wf_retrieve_claim_events()					
				END IF				
			END IF
		END IF
	ELSE
		MessageBox("Claim Event Link","A claim event must first be selected before a link can be performed.",Exclamation!)
	END IF
	
end event

type dw_claim_events_to_be_linked from u_dw_online within w_claim_events_to_be_linked
integer x = 59
integer y = 48
integer width = 2464
integer height = 1188
integer taborder = 20
string dataobject = "d_claim_events_to_be_linked"
boolean hscrollbar = true
boolean vscrollbar = true
end type

event rowfocuschanged;call super::rowfocuschanged;/*	Determine the current row and highlight it.
*/
	LONG	ll_row
	
	ll_row = THIS.GetRow()

	IF ll_row > 0 THEN
		uf_processselect(ll_row,"Mouse")
	END IF
	
end event

