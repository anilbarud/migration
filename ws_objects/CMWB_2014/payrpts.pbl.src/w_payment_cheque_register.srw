$PBExportHeader$w_payment_cheque_register.srw
$PBExportComments$Main screen used for the producing of Payment and/or Cheque Registers.
forward
global type w_payment_cheque_register from w_a_report
end type
type cb_print from commandbutton within w_payment_cheque_register
end type
type cb_process from commandbutton within w_payment_cheque_register
end type
type cb_close from commandbutton within w_payment_cheque_register
end type
type cb_moredetails from commandbutton within w_payment_cheque_register
end type
type dw_moredetails from u_dw_online within w_payment_cheque_register
end type
type gb_filter from groupbox within w_payment_cheque_register
end type
type rb_cash from radiobutton within w_payment_cheque_register
end type
type rb_cheque from radiobutton within w_payment_cheque_register
end type
type rb_direct from radiobutton within w_payment_cheque_register
end type
type rb_handwritten from radiobutton within w_payment_cheque_register
end type
type st_handwritten from statictext within w_payment_cheque_register
end type
type rb_all from radiobutton within w_payment_cheque_register
end type
type dw_cheque_payment_register_criteria from u_dw_online within w_payment_cheque_register
end type
end forward

global type w_payment_cheque_register from w_a_report
boolean TitleBar=true
string Title="Payment Registers"
cb_print cb_print
cb_process cb_process
cb_close cb_close
cb_moredetails cb_moredetails
dw_moredetails dw_moredetails
gb_filter gb_filter
rb_cash rb_cash
rb_cheque rb_cheque
rb_direct rb_direct
rb_handwritten rb_handwritten
st_handwritten st_handwritten
rb_all rb_all
dw_cheque_payment_register_criteria dw_cheque_payment_register_criteria
end type
global w_payment_cheque_register w_payment_cheque_register

type variables
NVO_PAYMENT_CHEQUE_REGISTER	invo_pc_register

end variables

on w_payment_cheque_register.create
int iCurrent
call w_a_report::create
this.cb_print=create cb_print
this.cb_process=create cb_process
this.cb_close=create cb_close
this.cb_moredetails=create cb_moredetails
this.dw_moredetails=create dw_moredetails
this.gb_filter=create gb_filter
this.rb_cash=create rb_cash
this.rb_cheque=create rb_cheque
this.rb_direct=create rb_direct
this.rb_handwritten=create rb_handwritten
this.st_handwritten=create st_handwritten
this.rb_all=create rb_all
this.dw_cheque_payment_register_criteria=create dw_cheque_payment_register_criteria
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=cb_print
this.Control[iCurrent+2]=cb_process
this.Control[iCurrent+3]=cb_close
this.Control[iCurrent+4]=cb_moredetails
this.Control[iCurrent+5]=dw_moredetails
this.Control[iCurrent+6]=gb_filter
this.Control[iCurrent+7]=rb_cash
this.Control[iCurrent+8]=rb_cheque
this.Control[iCurrent+9]=rb_direct
this.Control[iCurrent+10]=rb_handwritten
this.Control[iCurrent+11]=st_handwritten
this.Control[iCurrent+12]=rb_all
this.Control[iCurrent+13]=dw_cheque_payment_register_criteria
end on

on w_payment_cheque_register.destroy
call w_a_report::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_print)
destroy(this.cb_process)
destroy(this.cb_close)
destroy(this.cb_moredetails)
destroy(this.dw_moredetails)
destroy(this.gb_filter)
destroy(this.rb_cash)
destroy(this.rb_cheque)
destroy(this.rb_direct)
destroy(this.rb_handwritten)
destroy(this.st_handwritten)
destroy(this.rb_all)
destroy(this.dw_cheque_payment_register_criteria)
end on

event closequery;call super::closequery;/*	Destroy object containing the process's functions.
*/
	DESTROY invo_pc_register

end event

event open;call super::open;Long     ll_criteria_row, ll_month, ll_year
Datetime ldt_server_datetime, ldt_day1_of_month

SetPointer(HourGlass!)
ldt_server_datetime = f_server_datetime()
ll_month = Month(Date(ldt_server_datetime))
ll_year = Year(Date(ldt_server_datetime))
ldt_day1_of_month = Datetime(Date(String(ll_year) + "/" + String(ll_month) + "/01"))

//	Create object containing necessary functions to do the processing.
invo_pc_register = CREATE nvo_payment_cheque_register
	
//	Set the transactions for the datawindow.
dw_cheque_payment_register_criteria.SetTransObject(SQLCA)

//	Insert row into dw_benefit_cheque_extract_criteria and default the dates.
ll_criteria_row = dw_cheque_payment_register_criteria.InsertRow(0)
	
//	Set the default for the criteria.
IF ll_criteria_row > 0 THEN
	dw_cheque_payment_register_criteria.SetItem(ll_criteria_row,'register_type','A')
	dw_cheque_payment_register_criteria.SetItem(ll_criteria_row,'benefit_class_code','LOE')
	dw_cheque_payment_register_criteria.SetItem(ll_criteria_row,'start_date',ldt_day1_of_month)
	dw_cheque_payment_register_criteria.SetItem(ll_criteria_row,'end_date',ldt_server_datetime)
	invo_pc_register.of_set_processed_date_filter('A','register_type',dw_cheque_payment_register_criteria)
	dw_cheque_payment_register_criteria.SetColumn('cbh_processed_date')

	dw_cheque_payment_register_criteria.SetFocus()
	
ELSE
	MessageBox("Case Management Workbench","An error occured opening the screen.",Exclamation!)
	Close(THIS)
	Return
END IF

SetPointer(Arrow!)

end event

event resize;call super::resize;dw_report.width = w_payment_cheque_register.width - 119
end event

type dw_report from w_a_report`dw_report within w_payment_cheque_register
int X=37
int Width=2661
int Height=1817
int TabOrder=30
string DataObject="d_bp_chq_register_one_ben_class"
boolean HScrollBar=true
end type

type cb_print from commandbutton within w_payment_cheque_register
int X=1943
int Y=2389
int Width=357
int Height=109
int TabOrder=60
boolean Enabled=false
string Text="P&rint"
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

event clicked;/*	Call function to print the register.
*/
	invo_pc_register.of_print_register()

end event

type cb_process from commandbutton within w_payment_cheque_register
int X=1564
int Y=2389
int Width=357
int Height=109
int TabOrder=80
string Text="&Process"
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

event clicked;	INTEGER					li_result

/*	Call function to validate the entered criteria.
*/
	li_result = invo_pc_register.of_validate_criteria(dw_cheque_payment_register_criteria, dw_report)
	
	IF li_result < 0 THEN
		Return
	END IF
	
	cb_moredetails.Text = '&More Details'
	dw_moredetails.Visible = FALSE
	dw_report.Visible = TRUE
	
/* Call function to perform the benefit cheque extract process.
*/
	li_result = invo_pc_register.of_payment_cheque_register_process()
	
	IF li_result < 0 THEN
		Return
	END IF

/* make more details available/unavailable based on MA and Cheque
*/
	IF dw_cheque_payment_register_criteria.GetItemString(1, 'register_type') = 'A' AND &
		dw_cheque_payment_register_criteria.GetItemString(1, 'benefit_class_code') = 'MA'  THEN
		cb_moredetails.visible = TRUE
		dw_report.uf_setselect(1)
	ELSE
		cb_moredetails.visible = FALSE
		dw_report.uf_setselect(0)
	END IF
	
/* Show/Hide the filter buttons for payment_register
*/
	rb_all.Checked = TRUE
	rb_all.TriggerEvent(Clicked!) // Make sure that no filter left over

	IF dw_cheque_payment_register_criteria.GetItemString(1, 'register_type') = 'C' THEN
		gb_filter.Visible = TRUE
		rb_all.Visible = TRUE
		rb_cash.Visible = TRUE
		rb_cheque.Visible = TRUE
		rb_direct.Visible = TRUE
		rb_handwritten.Visible = TRUE
		st_handwritten.Visible = TRUE
	ELSE
		gb_filter.Visible = FALSE
		rb_all.Visible = FALSE
		rb_cash.Visible = FALSE
		rb_cheque.Visible = FALSE
		rb_direct.Visible = FALSE
		rb_handwritten.Visible = FALSE
		st_handwritten.Visible = FALSE
	END IF	
	
/*	Turn print button on.
*/
	cb_print.Enabled = TRUE

end event

type cb_close from commandbutton within w_payment_cheque_register
int X=2318
int Y=2389
int Width=357
int Height=109
int TabOrder=70
string Text="&Close"
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

event clicked;Close(PARENT)

end event

type cb_moredetails from commandbutton within w_payment_cheque_register
int X=1185
int Y=2389
int Width=357
int Height=109
int TabOrder=40
boolean Visible=false
boolean BringToTop=true
string Text="&More Details"
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

event clicked;Long ll_cheque_no

SetPointer(HourGlass!)

IF dw_report.GetRow() = 0 THEN Return

IF this.Text = '&More Details' THEN
	ll_cheque_no = dw_report.GetItemNumber(dw_report.GetRow(), 'cheque_no')
	dw_moredetails.Retrieve(ll_cheque_no)
	dw_report.Visible = FALSE
	dw_moredetails.Visible = TRUE
	this.Text = 'Report &View'
ELSE
	dw_moredetails.Reset()
	dw_report.Visible = TRUE
	dw_moredetails.Visible = FALSE
	this.Text = '&More Details'
END IF
end event

type dw_moredetails from u_dw_online within w_payment_cheque_register
int X=37
int Y=553
int Width=2647
int Height=1817
int TabOrder=50
boolean Visible=false
string DataObject="d_txns_for_cheque2"
boolean HScrollBar=true
boolean VScrollBar=true
end type

event constructor;call super::constructor;this.SetTransObject(SQLCA)
end event

type gb_filter from groupbox within w_payment_cheque_register
int X=2209
int Y=29
int Width=490
int Height=493
int TabOrder=20
boolean Visible=false
string Text="Filter"
BorderStyle BorderStyle=StyleLowered!
long TextColor=33554432
long BackColor=67108864
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

type rb_cash from radiobutton within w_payment_cheque_register
int X=2245
int Y=161
int Width=247
int Height=73
boolean Visible=false
boolean BringToTop=true
string Text="Cash"
BorderStyle BorderStyle=StyleLowered!
long TextColor=33554432
long BackColor=67108864
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

event clicked;IF this.Checked = TRUE THEN
	dw_report.SetFilter('payment_method_code = "C"')
	dw_report.Filter()
	dw_report.Sort()
END IF
end event

type rb_cheque from radiobutton within w_payment_cheque_register
int X=2245
int Y=237
int Width=439
int Height=73
boolean Visible=false
boolean BringToTop=true
string Text="Auto Cheque"
BorderStyle BorderStyle=StyleLowered!
long TextColor=33554432
long BackColor=67108864
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

event clicked;IF this.Checked = TRUE THEN
	dw_report.SetFilter('payment_method_code = "A"')
	dw_report.Filter()
	dw_report.Sort()
END IF
end event

type rb_direct from radiobutton within w_payment_cheque_register
int X=2245
int Y=313
int Width=444
int Height=73
boolean Visible=false
boolean BringToTop=true
string Text="Direct Deposit"
BorderStyle BorderStyle=StyleLowered!
long TextColor=33554432
long BackColor=67108864
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

event clicked;IF this.Checked = TRUE THEN
	dw_report.SetFilter('payment_method_code = "D"')
	dw_report.Filter()
	dw_report.Sort()
END IF
end event

type rb_handwritten from radiobutton within w_payment_cheque_register
int X=2245
int Y=389
int Width=421
int Height=73
boolean Visible=false
boolean BringToTop=true
string Text="Handwritten"
BorderStyle BorderStyle=StyleLowered!
long TextColor=33554432
long BackColor=67108864
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

event clicked;IF this.Checked = TRUE THEN
	dw_report.SetFilter('payment_method_code = "H"')
	dw_report.Filter()
	dw_report.Sort()
END IF
end event

type st_handwritten from statictext within w_payment_cheque_register
int X=2277
int Y=445
int Width=234
int Height=69
boolean Visible=false
boolean Enabled=false
boolean BringToTop=true
string Text="Cheque"
Alignment Alignment=Right!
boolean FocusRectangle=false
long TextColor=33554432
long BackColor=67108864
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

type rb_all from radiobutton within w_payment_cheque_register
int X=2245
int Y=85
int Width=421
int Height=73
boolean Visible=false
boolean BringToTop=true
string Text="All Payments"
BorderStyle BorderStyle=StyleLowered!
boolean Checked=true
long TextColor=33554432
long BackColor=67108864
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

event clicked;IF this.Checked = TRUE THEN
	dw_report.SetFilter('')
	dw_report.Filter()
	dw_report.Sort()
END IF
end event

type dw_cheque_payment_register_criteria from u_dw_online within w_payment_cheque_register
int X=1
int Y=21
int Width=2199
int Height=517
int TabOrder=10
string DataObject="d_cheque_payment_register_criteria"
boolean Border=false
BorderStyle BorderStyle=StyleBox!
end type

event itemchanged;call super::itemchanged;/*	Based on the type of benefit class code selected, filter out the appropriate dates in the
	processed date column. Only do if the register type or benefit class code change.
*/
	IF dwo.Name = 'register_type' OR dwo.Name = 'benefit_class_code' THEN
		invo_pc_register.of_set_processed_date_filter(data,dwo.Name,dw_cheque_payment_register_criteria)
	END IF

end event

