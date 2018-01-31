$PBExportHeader$w_direct_deposit_vouchers.srw
$PBExportComments$Medical Aid Vouchers
forward
global type w_direct_deposit_vouchers from window
end type
type cb_search from commandbutton within w_direct_deposit_vouchers
end type
type rb_deposit_number from radiobutton within w_direct_deposit_vouchers
end type
type rb_last_printed from radiobutton within w_direct_deposit_vouchers
end type
type cb_print_batch from commandbutton within w_direct_deposit_vouchers
end type
type cb_preview from commandbutton within w_direct_deposit_vouchers
end type
type cb_next from commandbutton within w_direct_deposit_vouchers
end type
type cb_prior from commandbutton within w_direct_deposit_vouchers
end type
type dw_direct_deposit from u_dw_online within w_direct_deposit_vouchers
end type
type cb_print_voucher from commandbutton within w_direct_deposit_vouchers
end type
type dw_direct_deposit_voucher from u_dw_online within w_direct_deposit_vouchers
end type
type dw_dd_history from u_datawindow within w_direct_deposit_vouchers
end type
type cb_close from commandbutton within w_direct_deposit_vouchers
end type
type gb_search_by from groupbox within w_direct_deposit_vouchers
end type
type rb_service_provider from radiobutton within w_direct_deposit_vouchers
end type
end forward

global type w_direct_deposit_vouchers from window
integer width = 3607
integer height = 2144
boolean titlebar = true
string title = "Provider Direct Deposit Vouchers"
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 67108864
event ue_open ( )
cb_search cb_search
rb_deposit_number rb_deposit_number
rb_last_printed rb_last_printed
cb_print_batch cb_print_batch
cb_preview cb_preview
cb_next cb_next
cb_prior cb_prior
dw_direct_deposit dw_direct_deposit
cb_print_voucher cb_print_voucher
dw_direct_deposit_voucher dw_direct_deposit_voucher
dw_dd_history dw_dd_history
cb_close cb_close
gb_search_by gb_search_by
rb_service_provider rb_service_provider
end type
global w_direct_deposit_vouchers w_direct_deposit_vouchers

type variables
Long il_cheques[] // Used to store cheques that need cover sheet
end variables

forward prototypes
public function integer wf_search_vouchers ()
public function integer wf_get_voucher (long al_deposit_no, long al_provider_no)
end prototypes

event ue_open();Long     ll_num_rows, ll_deposit_no
Integer  li_rtn

DatawindowChild ldwc_deposit_no

SetPointer(HourGlass!)
dw_dd_history.SetTransObject(SQLCA)
dw_direct_deposit_voucher.SetTransObject(SQLCA)

this.setredraw(false)
// Put DW in PrintPreview Mode and Zoom Out
dw_direct_deposit_voucher.Modify("Datawindow.Print.Preview=Yes")
dw_direct_deposit_voucher.Modify("Datawindow.Print.Preview.Zoom=75")

// trigger the clicked event of rb_deposit_no to set the search fields background and protection, and load the dropdown
rb_deposit_number.triggerEvent(Clicked!)

// Get all the deposit numbers
li_rtn = dw_direct_deposit.GetChild("deposit_no", ldwc_deposit_no)
ldwc_deposit_no.SetTransObject(SQLCA)
ll_num_rows = ldwc_deposit_no.Retrieve()

SQLCA.nf_handle_error("w_direct_deposit_vouchers","","open - ldwc_processed_date.Retrieve()")
IF ll_num_rows > 0 THEN
	ll_deposit_no = ldwc_deposit_no.GetItemNumber(1, "direct_deposit_xmit_no")
	dw_direct_deposit.SetItem(1, "deposit_no", ll_deposit_no)
	wf_search_vouchers()
END IF

this.setredraw(true)

end event

public function integer wf_search_vouchers ();// wf_search_vouchers
Long     ll_number, ll_rows
Integer  li_rtn
String   ls_sql, ls_message, ls_sql2, ls_sql_where
	
ls_sql = " SELECT a.direct_deposit_xmit_no," + &
			" a.recipient_no," + &
             " b.name," + &
             " sum(a.txn_amount) as total_amount," + &
             " b.provider_no," + &
             " a.voucher_printed_date" + &
             " FROM DIRECT_DEPOSIT_DETAIL a " + &
             " JOIN   PROVIDER b on a.recipient_no = b.provider_no and a.recipient_type_code = b.provider_type_code " 

ls_sql_where = " WHERE  a.direct_deposit_xmit_no = :al_number " 

ls_sql2 = " group  by b.name, "                       + &
                           "a.recipient_no, "               + &
                           "a.direct_deposit_xmit_no, " + &
                           "b.provider_no, "                + &
                           "a.voucher_printed_date "    + &									
              "order by a.direct_deposit_xmit_no desc, b.provider_no " 				 

SetPointer(HourGlass!)

dw_direct_deposit.AcceptText()
dw_dd_history.Reset()
dw_direct_deposit_voucher.Reset()
dw_direct_deposit_voucher.Visible = FALSE

IF rb_deposit_number.checked THEN
	ll_number = dw_direct_deposit.getItemNumber(1,"deposit_no")
	IF ll_number > 0 THEN
		ls_sql_where = " WHERE  a.direct_deposit_xmit_no = :al_number " 
	END IF

ELSEIF rb_service_provider.checked THEN
	ll_number = dw_direct_deposit.getItemNumber(1,"number")
	IF ll_number < 1  then 
		messagebox("", "Not a valid provider number.")
		return 1
	END IF
	ls_sql_where = " WHERE b.provider_no = :al_number" 
	
ELSEIF rb_last_printed.checked THEN
	ll_number = 0
	ls_sql_where = " WHERE  a.direct_deposit_xmit_no > :al_number and a.voucher_printed_date is null  " 
END IF

ls_message = dw_dd_history.modify('Datawindow.table.select = " ' + ls_sql + ls_sql_where + ls_sql2  + ' " ')
IF ls_message <> '' THEN RETURN -1

ll_rows = dw_dd_history.retrieve(ll_number)

IF ll_rows = 0 THEN 
	IF rb_last_printed.checked THEN
		MESSAGEBOX("","No vouchers found, that have not been printed.")
	ELSE
		MESSAGEBOX("","No vouchers available for the criteria entered.")
	END IF
END IF
RETURN 1
end function

public function integer wf_get_voucher (long al_deposit_no, long al_provider_no);Long    ll_num_rows
Integer li_rtn

dw_direct_deposit_voucher.ScrollToRow(1)
ll_num_rows = dw_direct_deposit_voucher.Retrieve(al_deposit_no,al_provider_no)
li_rtn = SQLCA.nf_handle_error("w_direct_deposit_vouchers","","wf_get_voucher - dw_voucher_header.Retrieve(al_deposit_no,al_provider_no)")
IF ll_num_rows > 0 THEN
	dw_direct_deposit_voucher.Visible = TRUE
	IF rb_last_printed.Checked = TRUE THEN
		cb_print_voucher.Enabled = TRUE
		cb_print_batch.Enabled = TRUE
	ELSEIF rb_service_provider.Checked = TRUE THEN
		cb_print_voucher.Enabled = TRUE
		cb_print_batch.Enabled = TRUE
	ELSEIF rb_deposit_number.Checked = TRUE THEN
		cb_print_voucher.Enabled = TRUE
		cb_print_batch.Enabled = TRUE
	END IF
	cb_preview.Enabled = TRUE
	
	IF	cb_preview.Text = "Un-P&review" THEN
		cb_next.Enabled = TRUE
		cb_prior.Enabled = TRUE
	END IF	
ELSE
	dw_direct_deposit_voucher.Visible = FALSE
	cb_print_voucher.Enabled = FALSE
	IF rb_last_printed.Checked = TRUE THEN
		cb_print_batch.Enabled = TRUE
	ELSE
		cb_print_batch.Enabled = FALSE
	END IF
	cb_preview.Enabled = FALSE
	MessageBox("Voucher Not Found", "Could not find Provider Voucher for provider number " +&
				  String(al_provider_no) + ".~rTry another provider_no or deposit number or call the Helpdesk for assistance.", Information!)
	IF	cb_preview.Text = "Un-P&review" THEN
		cb_next.Enabled = FALSE
		cb_prior.Enabled = FALSE
	END IF
	RETURN -1
END IF

RETURN 1

end function

on w_direct_deposit_vouchers.create
this.cb_search=create cb_search
this.rb_deposit_number=create rb_deposit_number
this.rb_last_printed=create rb_last_printed
this.cb_print_batch=create cb_print_batch
this.cb_preview=create cb_preview
this.cb_next=create cb_next
this.cb_prior=create cb_prior
this.dw_direct_deposit=create dw_direct_deposit
this.cb_print_voucher=create cb_print_voucher
this.dw_direct_deposit_voucher=create dw_direct_deposit_voucher
this.dw_dd_history=create dw_dd_history
this.cb_close=create cb_close
this.gb_search_by=create gb_search_by
this.rb_service_provider=create rb_service_provider
this.Control[]={this.cb_search,&
this.rb_deposit_number,&
this.rb_last_printed,&
this.cb_print_batch,&
this.cb_preview,&
this.cb_next,&
this.cb_prior,&
this.dw_direct_deposit,&
this.cb_print_voucher,&
this.dw_direct_deposit_voucher,&
this.dw_dd_history,&
this.cb_close,&
this.gb_search_by,&
this.rb_service_provider}
end on

on w_direct_deposit_vouchers.destroy
destroy(this.cb_search)
destroy(this.rb_deposit_number)
destroy(this.rb_last_printed)
destroy(this.cb_print_batch)
destroy(this.cb_preview)
destroy(this.cb_next)
destroy(this.cb_prior)
destroy(this.dw_direct_deposit)
destroy(this.cb_print_voucher)
destroy(this.dw_direct_deposit_voucher)
destroy(this.dw_dd_history)
destroy(this.cb_close)
destroy(this.gb_search_by)
destroy(this.rb_service_provider)
end on

event resize;dw_direct_deposit_voucher.height = This.height - 684
dw_direct_deposit_voucher.width = This.width - 77

cb_next.y = This.height - 236
cb_prior.y = This.height - 236
cb_close.y = This.height - 236
cb_preview.y = This.height - 236
cb_print_voucher.y = This.height - 236
cb_print_batch.y = This.height - 236

end event

event open;/*
Sven Oborn
April 7/2011

This window is copied from ProdSvcs dirdep.pbl window of same name.  No changes has been made.  Look for changes 
sub class w_direct_deposit_vouchers_cmwb

*/

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


PostEvent("ue_open")
end event

type cb_search from commandbutton within w_direct_deposit_vouchers
integer x = 1129
integer y = 60
integer width = 242
integer height = 108
integer taborder = 100
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Search"
boolean default = true
end type

event clicked;wf_search_vouchers()

end event

type rb_deposit_number from radiobutton within w_direct_deposit_vouchers
integer x = 50
integer y = 100
integer width = 544
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
string text = "Deposit Number"
boolean checked = true
end type

event clicked;dw_direct_deposit.reset()
dw_direct_deposit.insertRow(0)
dw_direct_deposit.Modify("deposit_no.background.color='16777215' deposit_no.protect='0'")
dw_direct_deposit.Modify("number.background.color='67108864'         number.protect='1'")
dw_direct_deposit.SetItem(1, "number", 0)

dw_direct_deposit.SetColumn("deposit_no")
dw_direct_deposit.SetFocus()

dw_dd_history.Reset()
dw_direct_deposit_voucher.Reset()
dw_direct_deposit_voucher.Visible = FALSE

cb_print_voucher.Enabled = FALSE
cb_print_batch.Enabled = FALSE

end event

type rb_last_printed from radiobutton within w_direct_deposit_vouchers
integer x = 50
integer y = 292
integer width = 645
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
string text = "Since Last Printed"
end type

event clicked;dw_direct_deposit.reset()
dw_direct_deposit.insertRow(0)

dw_direct_deposit.Modify("deposit_no.background.color='67108864'     deposit_no.protect='1'")  
dw_direct_deposit.Modify("number.background.color='67108864' number.protect='1'")


dw_dd_history.Reset()
dw_direct_deposit_voucher.Reset()
dw_direct_deposit_voucher.Visible = FALSE

cb_print_voucher.Enabled = FALSE
cb_print_batch.Enabled = FALSE

cb_search.triggerEvent(CLICKED!)


end event

type cb_print_batch from commandbutton within w_direct_deposit_vouchers
integer x = 2190
integer y = 1908
integer width = 434
integer height = 108
integer taborder = 70
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Print &Batch"
end type

event clicked;Long    n, ll_num_rows, ll_row,  ll_deposit_no, ll_provider_no
Integer li_rtn, li_rtn2
datetime ldt_currentdatetime

ll_num_rows = dw_dd_history.RowCount()
IF ll_num_rows = 0 THEN
	MessageBox("No Vouchers", "There are no Vouchers to be printed.", Information!)
	RETURN
END IF

ldt_currentdatetime = f_server_datetime()

// Get the current row for direct deposit history being displayed
ll_row = dw_dd_history.GetRow()

// Loop through and print all the vouchers
FOR n = 1 TO ll_num_rows
	ll_deposit_no = dw_dd_history.GetItemNumber(n, "direct_deposit_xmit_no")
	ll_provider_no = dw_dd_history.GetItemNumber(n, "provider_no")
	li_rtn = wf_get_voucher(ll_deposit_no,ll_provider_no)
	IF li_rtn = 1 THEN
		li_rtn2 = dw_direct_deposit_voucher.Print(FALSE)
		IF li_rtn2 = 1 THEN
			dw_dd_history.setItem(n, 'voucher_printed_date', ldt_currentdatetime)
			
			SQLCA.nf_begin_transaction()
			
         dw_dd_history.update()

			// if the printing is successful for this provider and deposit, then commit the voucher printed date
			SQLCA.nf_commit_transaction()
		END IF
	END IF
NEXT

// Display the voucher that was originally being displayed
IF ll_row > 0 THEN
	dw_dd_history.setRow(ll_row)
	dw_dd_history.scrollToRow(ll_row)
	ll_deposit_no = dw_dd_history.GetItemNumber(ll_row, "direct_deposit_xmit_no")
	ll_provider_no = dw_dd_history.GetItemNumber(ll_row, "provider_no")
	li_rtn = wf_get_voucher(ll_deposit_no,ll_provider_no)
END IF

end event

type cb_preview from commandbutton within w_direct_deposit_vouchers
integer x = 2651
integer y = 1908
integer width = 434
integer height = 108
integer taborder = 90
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Un-P&review"
end type

event clicked;// Put DW in PrintPreview Mode and Zoom Out

IF cb_preview.Text = "P&review" THEN
	dw_direct_deposit_voucher.Modify("Datawindow.Print.Preview=Yes")
	dw_direct_deposit_voucher.Modify("Datawindow.Print.Preview.Zoom=75")
	cb_preview.Text = "Un-P&review"
	cb_next.Enabled = TRUE
	cb_prior.Enabled = TRUE
ELSE
	dw_direct_deposit_voucher.Modify("Datawindow.Print.Preview=No")
	dw_direct_deposit_voucher.Modify("Datawindow.Print.Preview.Zoom=75")
	cb_preview.Text = "P&review"
	cb_next.Enabled = FALSE
	cb_prior.Enabled = FALSE
END IF

end event

type cb_next from commandbutton within w_direct_deposit_vouchers
integer x = 1490
integer y = 1908
integer width = 87
integer height = 92
integer taborder = 50
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = ">"
end type

event clicked;dw_direct_deposit_voucher.ScrollNextPage()
end event

type cb_prior from commandbutton within w_direct_deposit_vouchers
integer x = 1353
integer y = 1908
integer width = 87
integer height = 92
integer taborder = 60
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "<"
end type

event clicked;dw_direct_deposit_voucher.ScrollPriorPage()
end event

type dw_direct_deposit from u_dw_online within w_direct_deposit_vouchers
integer x = 576
integer y = 80
integer width = 489
integer height = 208
integer taborder = 110
string dataobject = "d_direct_deposit"
boolean border = false
end type

type cb_print_voucher from commandbutton within w_direct_deposit_vouchers
integer x = 1728
integer y = 1908
integer width = 434
integer height = 108
integer taborder = 120
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Print Voucher"
end type

event clicked;
dw_direct_deposit_voucher.Print()

end event

type dw_direct_deposit_voucher from u_dw_online within w_direct_deposit_vouchers
integer x = 14
integer y = 424
integer width = 3529
integer height = 1460
integer taborder = 10
string dataobject = "d_direct_deposit_voucher"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

type dw_dd_history from u_datawindow within w_direct_deposit_vouchers
integer x = 1422
integer y = 20
integer width = 2103
integer height = 392
integer taborder = 30
string dataobject = "d_provider_direct_deposit_summary"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event rowfocuschanged;call super::rowfocuschanged;Long    ll_num_rows, ll_deposit_no, ll_provider_no
Integer li_rtn

IF currentrow = 0 OR IsNull(currentrow) = TRUE OR This.RowCount() = 0 THEN
	RETURN
END IF

This.SelectRow(0, FALSE)
This.SelectRow(currentrow, TRUE)
This.SetFocus()

ll_deposit_no = This.GetItemNumber(currentrow, "direct_deposit_xmit_no")
ll_provider_no =This.GetItemNumber(currentrow, "provider_no") 
IF ll_deposit_no > 0 AND NOT IsNull(ll_deposit_no) AND ll_provider_no > 0 THEN
	wf_get_voucher(ll_deposit_no,ll_provider_no)
END IF

end event

event constructor;call super::constructor;THIS.uf_setSort(true)
end event

type cb_close from commandbutton within w_direct_deposit_vouchers
integer x = 3113
integer y = 1908
integer width = 434
integer height = 108
integer taborder = 40
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Close"
end type

event clicked;Close(Parent)
end event

type gb_search_by from groupbox within w_direct_deposit_vouchers
integer x = 18
integer y = 32
integer width = 1070
integer height = 372
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
string text = "Search By:"
end type

type rb_service_provider from radiobutton within w_direct_deposit_vouchers
integer x = 50
integer y = 196
integer width = 544
integer height = 72
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
string text = "Service Provider"
end type

event clicked;dw_direct_deposit.reset()
dw_direct_deposit.insertRow(0)
dw_direct_deposit.Modify("deposit_no.background.color='67108864'     deposit_no.protect='1'") 
dw_direct_deposit.Modify("number.background.color='16777215'         number.protect='0'")
dw_direct_deposit.SetItem(1, "number", 0)

dw_direct_deposit.SetColumn("number")
dw_direct_deposit.SetFocus()

dw_dd_history.Reset()
dw_direct_deposit_voucher.Reset()
dw_direct_deposit_voucher.Visible = FALSE

cb_print_voucher.Enabled = FALSE
cb_print_batch.Enabled = FALSE




end event

