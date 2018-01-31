$PBExportHeader$w_duplicate_invoice.srw
forward
global type w_duplicate_invoice from w_a_report
end type
type sle_invoice from singlelineedit within w_duplicate_invoice
end type
type st_1 from statictext within w_duplicate_invoice
end type
type cb_ok from commandbutton within w_duplicate_invoice
end type
type dw_recipient_type from u_dw_online within w_duplicate_invoice
end type
type st_2 from statictext within w_duplicate_invoice
end type
type st_3 from statictext within w_duplicate_invoice
end type
type sle_recipient_no from singlelineedit within w_duplicate_invoice
end type
end forward

global type w_duplicate_invoice from w_a_report
sle_invoice sle_invoice
st_1 st_1
cb_ok cb_ok
dw_recipient_type dw_recipient_type
st_2 st_2
st_3 st_3
sle_recipient_no sle_recipient_no
end type
global w_duplicate_invoice w_duplicate_invoice

on open;call w_a_report::open;	dw_report.SetTransObject(SQLCA)
	dw_recipient_type.SetTransObject(SQLCA)
	dw_recipient_type.InsertRow(0)

	sle_invoice.SetFocus()
end on

on w_duplicate_invoice.create
int iCurrent
call w_a_report::create
this.sle_invoice=create sle_invoice
this.st_1=create st_1
this.cb_ok=create cb_ok
this.dw_recipient_type=create dw_recipient_type
this.st_2=create st_2
this.st_3=create st_3
this.sle_recipient_no=create sle_recipient_no
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=sle_invoice
this.Control[iCurrent+2]=st_1
this.Control[iCurrent+3]=cb_ok
this.Control[iCurrent+4]=dw_recipient_type
this.Control[iCurrent+5]=st_2
this.Control[iCurrent+6]=st_3
this.Control[iCurrent+7]=sle_recipient_no
end on

on w_duplicate_invoice.destroy
call w_a_report::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.sle_invoice)
destroy(this.st_1)
destroy(this.cb_ok)
destroy(this.dw_recipient_type)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.sle_recipient_no)
end on

type dw_report from w_a_report`dw_report within w_duplicate_invoice
int TabOrder=50
string DataObject="d_duplicate_invoice_list"
boolean HScrollBar=true
end type

type sle_invoice from singlelineedit within w_duplicate_invoice
int X=476
int Y=81
int Width=956
int Height=89
int TabOrder=20
boolean BringToTop=true
BorderStyle BorderStyle=StyleLowered!
boolean AutoHScroll=false
TextCase TextCase=Upper!
long TextColor=33554432
long BackColor=16777215
int TextSize=-9
int Weight=400
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

type st_1 from statictext within w_duplicate_invoice
int X=42
int Y=97
int Width=293
int Height=73
boolean Enabled=false
boolean BringToTop=true
string Text="Invoice No:"
Alignment Alignment=Center!
boolean FocusRectangle=false
long TextColor=33554432
long BackColor=67108864
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

type cb_ok from commandbutton within w_duplicate_invoice
int X=1884
int Y=253
int Width=385
int Height=109
int TabOrder=40
boolean BringToTop=true
string Text="&OK"
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

on clicked;LONG		ll_numrows, ll_recipient_no
STRING	ls_invoice_no, ls_recipient_type_code

	dw_recipient_type.AcceptText()
	ls_invoice_no = Trim(sle_invoice.text)
	ll_recipient_no = Long(sle_recipient_no.text)
	ls_recipient_type_code = dw_recipient_type.GetItemString(1,'recipient_type_code')

	IF IsNull(ls_invoice_no) or ls_invoice_no <= '' THEN
		IF IsNull(ll_recipient_no) or ll_recipient_no = 0 THEN
			MessageBox("Missing Information","You must supply the invoice number or recipient number.",Exclamation!)
			RETURN
		ELSE
			dw_report.SetRedraw(FALSE)
			dw_report.DataObject = 'd_duplicate_invoice_list_recipient'
			dw_report.SetTransObject(SQLCA)
			dw_report.SetRedraw(TRUE)

			ll_numrows = dw_report.Retrieve(ll_recipient_no)
			SQLCA.nf_handle_error("w_duplicate_invoice","dw_report","cb_ok")
			IF ls_recipient_type_code > '' THEN
				dw_report.SetFilter('recipient_type_code = "' + ls_recipient_type_code + '"')
				dw_report.Filter()
			END IF
			IF ll_numrows <= 0 then
				MessageBox("Invoice List","No data found to satisfy request")
			END IF
		END IF
	ELSE
/*		report by invoice no
*/
		dw_report.SetRedraw(FALSE)
		dw_report.DataObject = 'd_duplicate_invoice_list'
		dw_report.SetTransObject(SQLCA)
		dw_report.SetRedraw(TRUE)

		ll_numrows = dw_report.Retrieve(ls_invoice_no)
		SQLCA.nf_handle_error("w_duplicate_invoice","dw_report","cb_ok")
		IF ll_recipient_no > 0 AND ls_recipient_type_code > '' THEN
			dw_report.SetFilter('recipient_no = ' + String(ll_recipient_no) + ' AND recipient_type_code = "' + ls_recipient_type_code + '"')
			dw_report.Filter()
		ELSEIF ll_recipient_no > 0 THEN
			dw_report.SetFilter('recipient_no = ' + String(ll_recipient_no) )
			dw_report.Filter()
		ELSEIF ls_recipient_type_code > '' THEN
			dw_report.SetFilter(' recipient_type_code = "' + ls_recipient_type_code + '"')
			dw_report.Filter()
		END IF

		IF ll_numrows <= 0 then
			MessageBox("Invoice List","No data found to satisfy request")
		END IF
	END IF

end on

type dw_recipient_type from u_dw_online within w_duplicate_invoice
int X=467
int Y=277
int Width=1153
int Height=101
int TabOrder=10
string DataObject="d_recipient_types"
boolean Border=false
end type

type st_2 from statictext within w_duplicate_invoice
int X=33
int Y=285
int Width=407
int Height=73
boolean Enabled=false
boolean BringToTop=true
string Text="Recipient Type:"
Alignment Alignment=Center!
boolean FocusRectangle=false
long TextColor=33554432
long BackColor=67108864
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

type st_3 from statictext within w_duplicate_invoice
int X=42
int Y=193
int Width=353
int Height=73
boolean Enabled=false
boolean BringToTop=true
string Text="Recipient No:"
Alignment Alignment=Center!
boolean FocusRectangle=false
long TextColor=33554432
long BackColor=67108864
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

type sle_recipient_no from singlelineedit within w_duplicate_invoice
int X=476
int Y=181
int Width=403
int Height=89
int TabOrder=30
boolean BringToTop=true
BorderStyle BorderStyle=StyleLowered!
boolean AutoHScroll=false
long TextColor=33554432
long BackColor=16777215
int TextSize=-9
int Weight=400
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

