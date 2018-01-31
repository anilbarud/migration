$PBExportHeader$w_travel_expense.srw
forward
global type w_travel_expense from window
end type
type dw_travel_expenses_scheduled from u_datawindow within w_travel_expense
end type
type st_3 from statictext within w_travel_expense
end type
type st_2 from statictext within w_travel_expense
end type
type st_1 from statictext within w_travel_expense
end type
type cb_close from commandbutton within w_travel_expense
end type
type cb_save from commandbutton within w_travel_expense
end type
type cb_cancel from commandbutton within w_travel_expense
end type
type dw_travel_expense_paid from u_datawindow within w_travel_expense
end type
type dw_travel_expense from u_datawindow within w_travel_expense
end type
end forward

global type w_travel_expense from window
integer width = 3218
integer height = 2432
boolean titlebar = true
string title = "Physio Travel Expense Log"
boolean resizable = true
long backcolor = 67108864
string icon = "AppIcon!"
boolean toolbarvisible = false
boolean center = true
dw_travel_expenses_scheduled dw_travel_expenses_scheduled
st_3 st_3
st_2 st_2
st_1 st_1
cb_close cb_close
cb_save cb_save
cb_cancel cb_cancel
dw_travel_expense_paid dw_travel_expense_paid
dw_travel_expense dw_travel_expense
end type
global w_travel_expense w_travel_expense

type variables
S_WINDOW_MESSAGE istr_message

N_ACCOUNT_PAYMENT_CONTROLLER inv_controller

long il_claim_no, il_payment_no

Boolean ib_save, ib_cancel, ib_close

end variables

forward prototypes
public function integer wf_add_travel_expense (long al_claim_no, long al_payment_no, long al_rehab_invoice_no, integer al_line_no)
end prototypes

public function integer wf_add_travel_expense (long al_claim_no, long al_payment_no, long al_rehab_invoice_no, integer al_line_no);long ll_iw_travel_expense_no, ll_row
date ldt_date

//get the last travel expense no
ll_iw_travel_expense_no = inv_controller.nf_get_last_travel_expense_no()
ldt_date = Date(f_server_datetime())


ll_row = dw_travel_expense.insertrow(0)

dw_travel_expense.setitem(ll_row,"iw_travel_expense_no",ll_iw_travel_expense_no)
dw_travel_expense.setitem(ll_row,"claim_no",al_claim_no)
dw_travel_expense.setitem(ll_row,"payment_no",al_payment_no)
dw_travel_expense.setitem(ll_row,"rehab_invoice_no",al_rehab_invoice_no)
dw_travel_expense.setitem(ll_row,"line_no",al_line_no)
dw_travel_expense.setitem(ll_row,"reimbursement_date",ldt_date)


//Insert into IW_TRAVEL_EXPENSE
//(iw_travel_expense_no,
// claim_no,
// payment_no,
// rehab_invoice_no,
// line_no,
// reimbursement_date)
//VALUES
// (:ll_iw_travel_expense_no,
// :al_claim_no,
// :al_payment_no,
// :al_rehab_invoice_no,
// :al_line_no,
// :ldt_date)
//Using SQLCA;
//
//SQLCA.nf_handle_error("w_travel_expense","wf_add_travel_expense","insert into IW_TRAVEL_EXPENSE")

Return 0
end function

on w_travel_expense.create
this.dw_travel_expenses_scheduled=create dw_travel_expenses_scheduled
this.st_3=create st_3
this.st_2=create st_2
this.st_1=create st_1
this.cb_close=create cb_close
this.cb_save=create cb_save
this.cb_cancel=create cb_cancel
this.dw_travel_expense_paid=create dw_travel_expense_paid
this.dw_travel_expense=create dw_travel_expense
this.Control[]={this.dw_travel_expenses_scheduled,&
this.st_3,&
this.st_2,&
this.st_1,&
this.cb_close,&
this.cb_save,&
this.cb_cancel,&
this.dw_travel_expense_paid,&
this.dw_travel_expense}
end on

on w_travel_expense.destroy
destroy(this.dw_travel_expenses_scheduled)
destroy(this.st_3)
destroy(this.st_2)
destroy(this.st_1)
destroy(this.cb_close)
destroy(this.cb_save)
destroy(this.cb_cancel)
destroy(this.dw_travel_expense_paid)
destroy(this.dw_travel_expense)
end on

event open;
INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


istr_message= Message.PowerObjectParm
	
il_claim_no = istr_message.al_doubleparm[1]
il_payment_no = istr_message.al_doubleparm[2]

inv_controller = Create n_account_payment_controller


dw_travel_expense.SetTransObject(SQLCA)
dw_travel_expenses_scheduled.SetTransObject(SQLCA)
dw_travel_expense_paid.SetTransObject(SQLCA)

dw_travel_expense.Retrieve(il_claim_no,il_payment_no)
SQLCA.nf_handle_error('w_travel_expense','open','dw_travel_expense.Retrieve')

dw_travel_expenses_scheduled.Retrieve(il_claim_no,il_payment_no)
SQLCA.nf_handle_error('w_travel_expense','open','dw_travel_expense.Retrieve')

dw_travel_expense_paid.Retrieve(il_claim_no)
SQLCA.nf_handle_error('w_travel_expense','open','dw_travel_expense_paid.Retrieve')


end event

event close;//CloseWithReturn(w_account_payment_maintenance,il_iw_travel_expense_no

//send back an array of 


w_travel_expense.visible = false
Return
end event

type dw_travel_expenses_scheduled from u_datawindow within w_travel_expense
integer x = 27
integer y = 792
integer width = 3122
integer height = 640
integer taborder = 0
string dataobject = "d_travel_expenses_scheduled"
end type

type st_3 from statictext within w_travel_expense
integer x = 37
integer y = 728
integer width = 905
integer height = 76
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Travel Expenses Scheduled:"
boolean focusrectangle = false
end type

type st_2 from statictext within w_travel_expense
integer x = 37
integer y = 12
integer width = 1257
integer height = 76
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Travel Expenses To Be Scheduled/Unscheduled:"
boolean focusrectangle = false
end type

type st_1 from statictext within w_travel_expense
integer x = 37
integer y = 1448
integer width = 631
integer height = 76
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Travel Expenses Paid:"
boolean focusrectangle = false
end type

type cb_close from commandbutton within w_travel_expense
integer x = 2821
integer y = 2180
integer width = 325
integer height = 104
integer taborder = 40
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "C&lose"
end type

event clicked;//close(w_travel_expense)

If cb_save.enabled = True Then
	Messagebox('Warning','Please Save or Cancel before closing.',Information!)
	Return
Else
	w_travel_expense.visible = false
End If
end event

type cb_save from commandbutton within w_travel_expense
integer x = 2487
integer y = 2180
integer width = 325
integer height = 104
integer taborder = 30
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Save"
end type

event clicked;//long ll_max, ll_rehab_invoice_no, ll_claim_no
//int li_travel, li_travel_expense_no, li_line_no, row
//
//ll_max = w_travel_expense.dw_travel_expense.RowCount()
//
//For row= 1 to ll_max 
//	li_travel = w_travel_expense.dw_travel_expense.getitemnumber(row,"travel")
//	li_travel_expense_no = w_travel_expense.dw_travel_expense.getitemnumber(row,"iw_travel_expense_no")
//	ll_rehab_invoice_no = w_travel_expense.dw_travel_expense.getitemnumber(row,"rehab_invoice_no")
//	li_line_no = w_travel_expense.dw_travel_expense.getitemnumber(row,"line_no")
//	ll_claim_no = w_travel_expense.dw_travel_expense.getitemnumber(row,"claim_no")
//	
//	IF li_travel_expense_no = 0 THEN //there isn't a travel expense record 
//		IF li_travel = 1 THEN  // there is a travel expense
//			IF wf_add_travel_expense(ll_claim_no, 1, ll_rehab_invoice_no, li_line_no) < 0 THEN
//				RETURN -1
//			END IF
//		END IF
//	ELSE //there was a pre-existing travel expense record
//		IF li_travel = 0 THEN // the existing travel expense record is now being removed
//			//remove the travel expense record, break the link with the payment
//		END IF
//	END IF
//Next

ib_save = True

cb_save.enabled = False
cb_cancel.enabled = False
end event

type cb_cancel from commandbutton within w_travel_expense
integer x = 2144
integer y = 2180
integer width = 325
integer height = 104
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "C&ancel"
end type

event clicked;

dw_travel_expense.Retrieve(il_claim_no, il_payment_no)
SQLCA.nf_handle_error('w_travel_expense','open','dw_travel_expense.Retrieve')

ib_cancel = True
cb_save.enabled = False
cb_cancel.enabled = False
end event

type dw_travel_expense_paid from u_datawindow within w_travel_expense
integer x = 27
integer y = 1512
integer width = 3122
integer height = 640
integer taborder = 0
boolean bringtotop = true
string dataobject = "d_travel_expense_paid"
boolean vscrollbar = true
end type

type dw_travel_expense from u_datawindow within w_travel_expense
integer x = 27
integer y = 72
integer width = 3122
integer height = 640
integer taborder = 10
boolean bringtotop = true
string dataobject = "d_travel_expense"
boolean vscrollbar = true
end type

event itemchanged;call super::itemchanged;


IF dwo.name = "travel" THEN
	
// Check to see if it was already checked off. If so then display warning message. They should not uncheck a travel payment, if it's intensional then the link must be broken to the
// orginal payment. If a payment has not been created yet for the travel expense then a message is not required and the record is not saved.

  IF data = "0" THEN //unchecked travel expense
	 IF this.getitemnumber(this.getrow(),"iw_travel_expense_no") <> 0 THEN //there was a travel expense associated
	     Messagebox('Travel Expense Warning','Unchecking the travel expense paid will remove the link to the payment.',Information!)
	END IF
 END IF
 
END IF

cb_cancel.enabled = True
cb_save.enabled = True
end event

