$PBExportHeader$w_deleted_documents.srw
forward
global type w_deleted_documents from w_a_report
end type
type cb_ok from commandbutton within w_deleted_documents
end type
type gb_1 from groupbox within w_deleted_documents
end type
type dw_enter_date_range from u_dw_online within w_deleted_documents
end type
end forward

global type w_deleted_documents from w_a_report
string title = "Deleted Documents"
cb_ok cb_ok
gb_1 gb_1
dw_enter_date_range dw_enter_date_range
end type
global w_deleted_documents w_deleted_documents

event open;call super::open;/*	Database connections and initialization.
*/
	dw_report.SetTransObject (ImageTrans)

	dw_enter_date_range.InsertRow(0)
	dw_enter_date_range.SetRow(1)
	dw_enter_date_range.SetColumn("from_date")
	dw_enter_date_range.setfocus()





end event

on w_deleted_documents.create
int iCurrent
call super::create
this.cb_ok=create cb_ok
this.gb_1=create gb_1
this.dw_enter_date_range=create dw_enter_date_range
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_ok
this.Control[iCurrent+2]=this.gb_1
this.Control[iCurrent+3]=this.dw_enter_date_range
end on

on w_deleted_documents.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_ok)
destroy(this.gb_1)
destroy(this.dw_enter_date_range)
end on

type dw_report from w_a_report`dw_report within w_deleted_documents
integer y = 492
integer taborder = 40
string dataobject = "d_deleted_documents"
boolean hscrollbar = true
end type

type cb_ok from commandbutton within w_deleted_documents
integer x = 2153
integer y = 212
integer width = 389
integer height = 108
integer taborder = 30
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
boolean default = true
end type

event clicked;/*	Variables and initialization.
*/
	LONG 		ll_numrows
	DATE	adt_from_date, adt_to_date

	dw_enter_date_range.AcceptText()

/*	Validate the dates and set the to date to midnight of the next day to ensure that all transactions
	from the requested day are included.
*/

	adt_from_date = date(dw_enter_date_range.GetItemDateTime(1,"from_date"))
	adt_to_date   = Date(dw_enter_date_range.GetItemDateTime(1,"to_date"))

	IF IsNull(adt_from_date) or &
		IsNull(adt_to_date) THEN
		MessageBox("Validation Error","Both the from and to dates must have a value.",Exclamation!)
		Return
	END IF

	IF adt_from_date < Date(1900,01,01) OR adt_to_date < Date(1900,01,01) THEN
		MessageBox("Validation Error","Dates cannot be earlier than 1900-01-01!",Exclamation!)
		Return
	END IF

	IF adt_from_date > Date(2079,06,06) OR adt_to_date > Date(2079,06,06) THEN
		MessageBox("Validation Error","Dates cannot be later than 2079-06-06!",Exclamation!)
		Return
	END IF
	
	IF adt_from_date >= adt_to_date then
		MessageBox("Validation Error","The to date must be later than the from date.",Exclamation!)
		Return
	END IF

/*	Retrieve the report.
*/
	ll_numrows = dw_report.Retrieve(adt_from_date,adt_to_date)
	ImageTrans.nf_handle_error("w_deleted_documents","dw_report","cb_ok")
	IF ll_numrows <= 0 then
		MessageBox("Deleted Documents","No data found to satisfy request")
	END IF

end event

type gb_1 from groupbox within w_deleted_documents
integer x = 50
integer y = 84
integer width = 1518
integer height = 276
integer taborder = 10
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Processing Period"
end type

type dw_enter_date_range from u_dw_online within w_deleted_documents
integer x = 105
integer y = 188
integer width = 1385
integer height = 92
integer taborder = 20
boolean bringtotop = true
string dataobject = "d_enter_date_range"
boolean border = false
boolean livescroll = true
end type

