$PBExportHeader$w_docid_reference.srw
$PBExportComments$window to track creation of batches and associated Doc information
forward
global type w_docid_reference from w_a_report
end type
type cb_ok from commandbutton within w_docid_reference
end type
type gb_1 from groupbox within w_docid_reference
end type
type dw_enter_date_range from u_dw_online within w_docid_reference
end type
type cbx_index from checkbox within w_docid_reference
end type
type dw_reference from u_dw_online within w_docid_reference
end type
type st_1 from statictext within w_docid_reference
end type
type st_2 from statictext within w_docid_reference
end type
type cb_reset from commandbutton within w_docid_reference
end type
end forward

global type w_docid_reference from w_a_report
string title = "Docid/Reference Association"
cb_ok cb_ok
gb_1 gb_1
dw_enter_date_range dw_enter_date_range
cbx_index cbx_index
dw_reference dw_reference
st_1 st_1
st_2 st_2
cb_reset cb_reset
end type
global w_docid_reference w_docid_reference

event open;call super::open;/*	Database connections and initialization.
*/
	dw_report.SetTransObject (ImageTrans)

	dw_enter_date_range.InsertRow(0)
	dw_enter_date_range.SetRow(1)
	dw_enter_date_range.SetColumn("from_date")
	dw_enter_date_range.setfocus()
	
	dw_reference.InsertRow(0)
	dw_reference.SetRow(1)
	





end event

on w_docid_reference.create
int iCurrent
call super::create
this.cb_ok=create cb_ok
this.gb_1=create gb_1
this.dw_enter_date_range=create dw_enter_date_range
this.cbx_index=create cbx_index
this.dw_reference=create dw_reference
this.st_1=create st_1
this.st_2=create st_2
this.cb_reset=create cb_reset
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_ok
this.Control[iCurrent+2]=this.gb_1
this.Control[iCurrent+3]=this.dw_enter_date_range
this.Control[iCurrent+4]=this.cbx_index
this.Control[iCurrent+5]=this.dw_reference
this.Control[iCurrent+6]=this.st_1
this.Control[iCurrent+7]=this.st_2
this.Control[iCurrent+8]=this.cb_reset
end on

on w_docid_reference.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_ok)
destroy(this.gb_1)
destroy(this.dw_enter_date_range)
destroy(this.cbx_index)
destroy(this.dw_reference)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.cb_reset)
end on

type dw_report from w_a_report`dw_report within w_docid_reference
integer x = 27
integer y = 492
integer taborder = 60
string dataobject = "d_docid_reference"
boolean hscrollbar = true
end type

type cb_ok from commandbutton within w_docid_reference
integer x = 2011
integer y = 368
integer width = 389
integer height = 104
integer taborder = 50
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
	LONG 	 ll_numrows,ll_reference_from,ll_reference_to,ll_max_reference
	DATE	 adt_from_date, adt_to_date
	STRING ls_indexed

	dw_enter_date_range.AcceptText()
	dw_reference.AcceptText()
	dw_report.reset()

/*	Validate the dates and set the to date to midnight of the next day to ensure that all transactions
	from the requested day are included.
*/

	adt_from_date = date(dw_enter_date_range.GetItemDateTime(1,"from_date"))
	adt_to_date   = Date(dw_enter_date_range.GetItemDateTime(1,"to_date"))

	IF IsNull(adt_from_date) or &
		IsNull(adt_to_date) THEN
		MessageBox("Validation Error","Both the from and to dates must have a value.",Exclamation!)
		dw_enter_date_range.setfocus()
		Return
	END IF

	IF adt_from_date < Date(1900,01,01) OR adt_to_date < Date(1900,01,01) THEN
		MessageBox("Validation Error","Dates cannot be earlier than 1900-01-01!",Exclamation!)
		dw_enter_date_range.setfocus()
		Return
	END IF

	IF adt_from_date > Date(2079,06,06) OR adt_to_date > Date(2079,06,06) THEN
		MessageBox("Validation Error","Dates cannot be later than 2079-06-06!",Exclamation!)
		dw_enter_date_range.setfocus()
		Return
	END IF
	
	IF adt_from_date >= adt_to_date then
		MessageBox("Validation Error","The to date must be later than the from date.",Exclamation!)
		dw_enter_date_range.setfocus()
		Return
	END IF
	
	/* validate the information in the reference number search if it is applicable */
	ll_reference_from = dw_reference.getitemnumber(1,"reference_from")
	ll_reference_to   = dw_reference.getitemnumber(1,"reference_to")
	
	/* if the from is null then set it to 1 and the to to the max reference number */
	IF isnull(ll_reference_from) AND ISNULL(ll_reference_to) THEN 
		ll_reference_from = 1
		
		SELECT max(reference_no) 
		  INTO :ll_max_reference 
		  FROM Docid_Reference_Xref;
		  
		ImageTrans.nf_handle_error("w_deleted_documents","dw_report","cb_ok")
		
		IF isnull(ll_max_reference) THEN 
			MessageBox("Docid/Reference Report","No data found to satisfy request")
			RETURN
		END IF 
	   ll_reference_to = ll_max_reference
	
	ELSEIF ISNULL(ll_reference_to) OR ll_reference_to = 0 THEN
		ll_reference_to = ll_reference_from
	END IF 
	
	IF ll_reference_from > ll_reference_to THEN
		MessageBox("Docid/Reference Report","The reference from number must be less than the reference to number")
		dw_reference.setfocus()
		RETURN
	END IF 
	
	IF ll_reference_from < 0 OR ll_reference_to < 0 THEN 
		MessageBox("Docid/Reference Report","The reference from and to numbers must be greater than 0")
		dw_reference.setfocus()
		RETURN
	END IF 
	
	
	/* grab the value from the check box */
	IF cbx_index.checked = TRUE THEN
		ls_indexed = "Y"
	ELSE
		ls_indexed = "N"
	END IF 
	
/*	Retrieve the report.
*/
	ll_numrows = dw_report.Retrieve(adt_from_date,adt_to_date,ls_indexed,ll_reference_from,ll_reference_to)
	ImageTrans.nf_handle_error("w_deleted_documents","dw_report","cb_ok")
	IF ll_numrows <= 0 then
		MessageBox("Docid/Reference Report","No data found to satisfy request")
	END IF

end event

type gb_1 from groupbox within w_docid_reference
integer x = 50
integer y = 24
integer width = 1929
integer height = 448
integer taborder = 10
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Search"
end type

type dw_enter_date_range from u_dw_online within w_docid_reference
integer x = 283
integer y = 168
integer width = 1253
integer height = 92
integer taborder = 20
boolean bringtotop = true
string dataobject = "d_enter_date_range"
boolean border = false
end type

type cbx_index from checkbox within w_docid_reference
integer x = 1614
integer y = 168
integer width = 325
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
string text = "Indexed"
end type

type dw_reference from u_dw_online within w_docid_reference
integer x = 274
integer y = 340
integer width = 1262
integer height = 96
integer taborder = 30
boolean bringtotop = true
string title = "none"
string dataobject = "d_reference_input"
boolean border = false
end type

type st_1 from statictext within w_docid_reference
integer x = 82
integer y = 100
integer width = 507
integer height = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Processing Period:"
boolean focusrectangle = false
end type

type st_2 from statictext within w_docid_reference
integer x = 82
integer y = 280
integer width = 402
integer height = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Reference No:"
boolean focusrectangle = false
end type

type cb_reset from commandbutton within w_docid_reference
integer x = 2011
integer y = 60
integer width = 389
integer height = 104
integer taborder = 40
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Reset"
end type

event clicked;dw_enter_date_range.reset()
dw_reference.reset()
dw_report.reset()

/*	Database connections and initialization.
*/
dw_report.SetTransObject (ImageTrans)

dw_enter_date_range.InsertRow(0)
dw_enter_date_range.SetRow(1)
dw_enter_date_range.SetColumn("from_date")
dw_enter_date_range.setfocus()
	
dw_reference.InsertRow(0)
dw_reference.SetRow(1)
end event

