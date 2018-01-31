$PBExportHeader$w_reminder.srw
$PBExportComments$Reminders - Main window for entering payment related reminders
forward
global type w_reminder from w_ancestor
end type
type dw_reminder from u_dw_online within w_reminder
end type
type cb_ok from commandbutton within w_reminder
end type
type cb_cancel from commandbutton within w_reminder
end type
type st_1 from statictext within w_reminder
end type
type sle_claim_no from singlelineedit within w_reminder
end type
end forward

global type w_reminder from w_ancestor
integer x = 1024
integer y = 616
integer width = 1975
integer height = 852
string title = "Reminders"
string menuname = ""
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
long backcolor = 67108864
dw_reminder dw_reminder
cb_ok cb_ok
cb_cancel cb_cancel
st_1 st_1
sle_claim_no sle_claim_no
end type
global w_reminder w_reminder

type variables
LONG il_claim_no
BOOLEAN ib_reminder_found
end variables

forward prototypes
public function long wf_next_available_payment_reminder_no ()
end prototypes

public function long wf_next_available_payment_reminder_no ();LONG 		ll_result, ll_last_payment_reminder_no

/*	Function Name: wf_next_available_payment_reminder_no                                         
	Purpose:       The purpose of this module is to get the next available payment_reminder_no         
	Arguments:     none                                                                          
	Return Values: The next available payment_reminder_no                                             
*/
	SELECT Max(payment_reminder_no)
	  INTO :ll_last_payment_reminder_no
	  FROM PAYMENT_REMINDER
	 WHERE claim_no = :il_claim_no
	 USING SQLCA;

	ll_result = SQLCA.nf_handle_error("Embedded SQL: Select from PAYMENT_REMINDER","w_reminder","wf_next_available_payment_reminder_no")
	IF ll_result < 0 THEN
		Return -1
	ELSE
		IF IsNull(ll_last_payment_reminder_no) THEN
			ll_last_payment_reminder_no = 1
		ELSE
			ll_last_payment_reminder_no = ll_last_payment_reminder_no + 1
		END IF
	END IF
	
Return ll_last_payment_reminder_no


end function

on open;call w_ancestor::open;LONG    ll_nmbr_records


il_claim_no = message.DoubleParm
sle_claim_no.text = string(il_claim_no)


dw_reminder.SetTransObject (SQLCA)


ll_nmbr_records = dw_reminder.Retrieve(il_claim_no)
IF SQLCA.nf_handle_error("dw_reminder","w_reminder","open for w_reminder") < 0 THEN
	Close(this)
	Return
END IF

IF ll_nmbr_records = 0 THEN
	dw_reminder.InsertRow(0)
	dw_reminder.SetItem(1,"claim_no",il_claim_no)
	dw_reminder.SetItem(1,"reminder_type_code","pay")
	ib_reminder_found = FALSE
ELSE
	ib_reminder_found = TRUE
End If

dw_reminder.SetFocus()


end on

on w_reminder.create
int iCurrent
call super::create
this.dw_reminder=create dw_reminder
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
this.st_1=create st_1
this.sle_claim_no=create sle_claim_no
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_reminder
this.Control[iCurrent+2]=this.cb_ok
this.Control[iCurrent+3]=this.cb_cancel
this.Control[iCurrent+4]=this.st_1
this.Control[iCurrent+5]=this.sle_claim_no
end on

on w_reminder.destroy
call super::destroy
destroy(this.dw_reminder)
destroy(this.cb_ok)
destroy(this.cb_cancel)
destroy(this.st_1)
destroy(this.sle_claim_no)
end on

type dw_reminder from u_dw_online within w_reminder
integer x = 91
integer y = 176
integer width = 1422
integer height = 348
integer taborder = 20
string dataobject = "d_reminder"
boolean border = false
end type

type cb_ok from commandbutton within w_reminder
integer x = 1641
integer y = 208
integer width = 274
integer height = 108
integer taborder = 30
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
end type

event clicked;LONG		ll_payment_reminder_no, ll_rownum
STRING	ls_reminder_note


/*	We are only allowing 1 payment reminder.  

	If a payment reminder was found during the open event (vib_reminder_found = true) THEN 
	-		If the user has deleted the text of the reminder THEN
	-		Delete the record and update
	Otherwise,
	-		Just update	

	If a payment reminder was not found during the open event (vib_reminder_found = false) THEN
	-		If the user has not entered any text for the reminder THEN
	-		Don't save the reminder
	Otherwise,
	-		Get the next available reminder number and update the database
*/
	
	SetPointer(HourGlass!)
	IF dw_reminder.AcceptText() < 0 THEN
		Return
	END IF

	ll_rownum = dw_reminder.GetRow()
	ls_reminder_note = dw_reminder.GetItemString(ll_rownum,"reminder_note")

	IF ib_reminder_found THEN
		IF ls_reminder_note = "" or IsNull(ls_reminder_note) THEN
			dw_reminder.DeleteRow(1)
		END IF
	ELSE
		IF ls_reminder_note = "" or IsNull(ls_reminder_note) THEN
			dw_reminder.Reset()
			Close(parent)
			Return
		ELSE
			ll_payment_reminder_no = wf_next_available_payment_reminder_no()
			IF ll_payment_reminder_no < 0 THEN
				Close(Parent)
				Return
			END IF
			dw_reminder.SetItem(1,"payment_reminder_no",ll_payment_reminder_no)
		END IF
	END IF
	
	SQLCA.nf_begin_transaction()
	
	dw_reminder.Update()
	IF SQLCA.nf_handle_error("dw_reminder","w_reminder","clicked for cb_OK") < 0 THEN
		Close(parent)
		Return
	END IF
	
	SQLCA.nf_commit_transaction()

	Close(Parent)


end event

type cb_cancel from commandbutton within w_reminder
integer x = 1641
integer y = 340
integer width = 274
integer height = 108
integer taborder = 40
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cance&l"
end type

on clicked;SetPointer(HourGlass!)
dw_reminder.Reset()

Close(Parent)
end on

type st_1 from statictext within w_reminder
integer x = 82
integer y = 56
integer width = 526
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Reminder for Claim:"
alignment alignment = center!
boolean focusrectangle = false
end type

type sle_claim_no from singlelineedit within w_reminder
integer x = 640
integer y = 40
integer width = 357
integer height = 84
integer taborder = 10
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean autohscroll = false
end type

