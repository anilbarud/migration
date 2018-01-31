$PBExportHeader$w_filter_document_list.srw
$PBExportComments$used to filter the document lists in association with reporting fees
forward
global type w_filter_document_list from w_ancestor
end type
type gb_2 from groupbox within w_filter_document_list
end type
type gb_select_physician from groupbox within w_filter_document_list
end type
type cb_ok from commandbutton within w_filter_document_list
end type
type cb_cancel from commandbutton within w_filter_document_list
end type
type dw_treatment_date from u_dw_online within w_filter_document_list
end type
type gb_select_documents from groupbox within w_filter_document_list
end type
type gb_1 from groupbox within w_filter_document_list
end type
type st_1 from statictext within w_filter_document_list
end type
type st_2 from statictext within w_filter_document_list
end type
type dw_select_document_type from u_dw_online within w_filter_document_list
end type
type em_claim from editmask within w_filter_document_list
end type
type em_physician from editmask within w_filter_document_list
end type
end forward

global type w_filter_document_list from w_ancestor
integer x = 1289
integer y = 316
integer width = 1449
integer height = 1796
string title = "Filter Transaction List"
string menuname = ""
boolean controlmenu = false
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
long backcolor = 67108864
gb_2 gb_2
gb_select_physician gb_select_physician
cb_ok cb_ok
cb_cancel cb_cancel
dw_treatment_date dw_treatment_date
gb_select_documents gb_select_documents
gb_1 gb_1
st_1 st_1
st_2 st_2
dw_select_document_type dw_select_document_type
em_claim em_claim
em_physician em_physician
end type
global w_filter_document_list w_filter_document_list

forward prototypes
public function boolean wf_check_claim_number (long al_claim_number)
public function boolean wf_check_provider_number (long al_provider_number)
end prototypes

public function boolean wf_check_claim_number (long al_claim_number);/*
This is for the filter window and checks to make sure 
that if a claim number was entered then it must be valid


ARGS: al_claim_number

RETURNS: Boolean - (true if claim is valid)
*/

BOOLEAN		lb_valid

LONG			ll_claim

lb_valid = false


SELECT count(*) INTO :ll_claim FROM CLAIM WHERE claim_no = :al_claim_number USING SQLCA ;
	
SQLCA.nf_handle_error(" ","w_filter_document_list","on wf_check_claim_number")

IF ll_claim > 0 THEN
	RETURN TRUE
END IF

RETURN FALSE
end function

public function boolean wf_check_provider_number (long al_provider_number);/*
This is for the filter window and checks to make sure 
that if a provider number was entered then it must be valid


ARGS: al_provider_number

RETURNS: Boolean - (true if provider is valid)
*/

BOOLEAN		lb_valid

LONG			ll_provider

lb_valid = false


SELECT count(*) INTO :ll_provider FROM PROVIDER WHERE provider_no = :al_provider_number USING SQLCA ;
	
SQLCA.nf_handle_error(" ","w_filter_document_list","on wf_check_provider_number")

IF ll_provider > 0 THEN
	RETURN TRUE
END IF

RETURN FALSE
end function

event open;call super::open;datawindowchild  ldwc_child

	// Set database object for drop down datawindow
   dw_select_document_type.SetTransObject(SQLCA)
	
	dw_select_document_type.InsertRow(0)
	dw_treatment_date.InsertRow(0)
	


end event

on w_filter_document_list.create
int iCurrent
call super::create
this.gb_2=create gb_2
this.gb_select_physician=create gb_select_physician
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
this.dw_treatment_date=create dw_treatment_date
this.gb_select_documents=create gb_select_documents
this.gb_1=create gb_1
this.st_1=create st_1
this.st_2=create st_2
this.dw_select_document_type=create dw_select_document_type
this.em_claim=create em_claim
this.em_physician=create em_physician
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.gb_2
this.Control[iCurrent+2]=this.gb_select_physician
this.Control[iCurrent+3]=this.cb_ok
this.Control[iCurrent+4]=this.cb_cancel
this.Control[iCurrent+5]=this.dw_treatment_date
this.Control[iCurrent+6]=this.gb_select_documents
this.Control[iCurrent+7]=this.gb_1
this.Control[iCurrent+8]=this.st_1
this.Control[iCurrent+9]=this.st_2
this.Control[iCurrent+10]=this.dw_select_document_type
this.Control[iCurrent+11]=this.em_claim
this.Control[iCurrent+12]=this.em_physician
end on

on w_filter_document_list.destroy
call super::destroy
destroy(this.gb_2)
destroy(this.gb_select_physician)
destroy(this.cb_ok)
destroy(this.cb_cancel)
destroy(this.dw_treatment_date)
destroy(this.gb_select_documents)
destroy(this.gb_1)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.dw_select_document_type)
destroy(this.em_claim)
destroy(this.em_physician)
end on

type gb_2 from groupbox within w_filter_document_list
integer x = 69
integer y = 784
integer width = 1303
integer height = 248
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Select Claim"
end type

type gb_select_physician from groupbox within w_filter_document_list
integer x = 69
integer y = 1080
integer width = 1303
integer height = 244
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Select Provider"
end type

type cb_ok from commandbutton within w_filter_document_list
integer x = 407
integer y = 1560
integer width = 274
integer height = 108
integer taborder = 60
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
boolean default = true
end type

event clicked;/*
Enterable fields and buttons for w_filter_document_list

TYPE             - required = N - Valid document types are SDD, MPD and AD
STATUS           - required = N - Must be item from list
FROM (date)      - required = N - to be used for filtering based on treatment_date 
                                  if entered, must be a valid date. If to the date is entered then 
									       the from_date must be entered.
TO(DATE)         - required = N - to be used for filtering based on treatment_date 
                                  if entered, must be a valid date. If the from date is entered then 
									       the to_date must be entered.
CLAIM NUMBER     - must be a valid claim number
PHYSICIAN NUMBER - must be a valid medical aid provider
OK               - At least one document must be checked as approved for button to be enabled???????

*/

DATAWINDOWCHILD    ldwc_child
DATE     ldtm_from, ldtm_to, ldtm_null
STRING	ls_treatment_filter,ls_return_filter,ls_joiner,ls_document_filter,ls_type_filter
STRING   ls_claim_filter,ls_provider_filter,ls_document_type,ls_document_status
BOOLEAN  lb_check


	SetPointer(HourGlass!)
	SetNull(ldtm_null)

	IF dw_select_document_type.AcceptText() < 0 THEN
		Return
	END IF

	IF dw_treatment_date.AcceptText() < 0 THEN
		Return
	END IF

	/* Grab the values that we need from the datawindows - these will be added to our filter
	*/
	ldtm_from          = date(dw_treatment_date.GetItemDateTime(1,"paid_from"))
	ldtm_to            = date(dw_treatment_date.GetItemDateTime(1,"paid_to"))
	
	/* grab the child datawindow
	*/
	ls_document_status = dw_select_document_type.GetItemstring(1,"document_status")
	IF trim(ls_document_status) <> "" THEN
		ls_document_filter = "eligibility_code = '" + ls_document_status + "'"
	ELSE
		ls_document_filter = ""
	END IF
	
	/* create our filter for this datawindow
	*/
	ls_document_type   = dw_select_document_type.GetItemstring(1,"document_type")
	IF trim(ls_document_type) <> "" THEN
		ls_type_filter = "type_code = '" + ls_document_type + "'"
	ELSE
		ls_type_filter = ""
	END IF

	// Note: We have to be careful with the dates as treatment_date contains the time
	IF isdate(string(ldtm_from)) AND isdate(string(ldtm_to)) THEN
			ls_treatment_filter = "(date_on_document >= " + String(ldtm_from,'yyyy-mm-dd') + " and date_on_document < " + string(ldtm_to,'yyyy-mm-dd') + ")"
	END IF		
			
	IF ISNULL(ldtm_from)	AND NOT ISNULL(ldtm_from) THEN
		MessageBox("Reporting Fees - Validation Error","Both dates must be entered",Exclamation!)
		Return
	ELSEIF ISNULL(ldtm_to)	AND NOT ISNULL(ldtm_from) THEN
		MessageBox("Reporting Fees - Validation Error","Both dates must be entered",Exclamation!)
		Return
	END IF
	
	
/* now we need to check and see if the claim number was entered
*/
IF isnumber(trim(em_claim.text)) THEN
	/* check that the claim number is valid
	*/
	lb_check = wf_check_claim_number(long(em_claim.text))
	IF lb_check = FALSE THEN
		messagebox("Reporting Fees - Validation Error","The claim number " + em_claim.text + " is not valid!",Exclamation!)
		em_claim.setfocus()
		RETURN
	END IF

	ls_claim_filter = "claim_no = " + em_claim.text
ELSE
	ls_claim_filter = ""
END IF

/* now we need to check and see if the physician number was entered
*/
IF isnumber(trim(em_physician.text)) THEN
	/* check that the provider number is valid
	*/
	lb_check = wf_check_provider_number(long(em_physician.text))
	IF lb_check = FALSE THEN
		messagebox("Reporting Fees - Validation Error","The provider number " + em_physician.text + " is not valid!",Exclamation!)
		em_physician.setfocus()
		RETURN
	END IF
	
	ls_provider_filter = "provider_no = " + em_physician.text
ELSE
	ls_provider_filter = ""
END IF

/* Build the return filter
*/

	ls_return_filter = ""
	ls_joiner = ""
	
	IF ls_document_filter <> "" THEN
		ls_return_filter = ls_return_filter + ls_joiner + ls_document_filter
		ls_joiner = " and "
	END IF
	IF ls_type_filter <> "" THEN
		ls_return_filter = ls_return_filter + ls_joiner + ls_type_filter
		ls_joiner = " and "
	END IF
	IF ls_treatment_filter <> "" THEN
		ls_return_filter = ls_return_filter + ls_joiner + ls_treatment_filter
		ls_joiner = " and "
	END IF
	IF ls_claim_filter <> "" THEN
		ls_return_filter = ls_return_filter + ls_joiner + ls_claim_filter
		ls_joiner = " and "
	END IF
	IF ls_provider_filter <> "" THEN
		ls_return_filter = ls_return_filter + ls_joiner + ls_provider_filter
		ls_joiner = " and "
	END IF

	CloseWithReturn(w_filter_document_list,ls_return_filter)
end event

type cb_cancel from commandbutton within w_filter_document_list
integer x = 690
integer y = 1560
integer width = 274
integer height = 108
integer taborder = 50
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cance&l"
boolean cancel = true
end type

event clicked;	
	SetPointer(HourGlass!)
	CloseWithReturn(w_filter_document_list,"Cancel")
end event

type dw_treatment_date from u_dw_online within w_filter_document_list
integer x = 151
integer y = 584
integer width = 1083
integer height = 84
integer taborder = 20
string dataobject = "d_filter_dates"
boolean border = false
end type

type gb_select_documents from groupbox within w_filter_document_list
integer x = 69
integer y = 88
integer width = 1303
integer height = 364
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Select Documents"
end type

type gb_1 from groupbox within w_filter_document_list
integer x = 69
integer y = 500
integer width = 1303
integer height = 228
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Select Treatment Date"
end type

type st_1 from statictext within w_filter_document_list
integer x = 155
integer y = 884
integer width = 393
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
boolean enabled = false
string text = "Claim Number:"
boolean focusrectangle = false
end type

type st_2 from statictext within w_filter_document_list
integer x = 151
integer y = 1188
integer width = 498
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
boolean enabled = false
string text = "Provider Number:"
boolean focusrectangle = false
end type

type dw_select_document_type from u_dw_online within w_filter_document_list
integer x = 87
integer y = 164
integer width = 1253
integer height = 260
integer taborder = 10
boolean bringtotop = true
string dataobject = "d_select_document_crieteria"
boolean border = false
end type

type em_claim from editmask within w_filter_document_list
integer x = 654
integer y = 876
integer width = 347
integer height = 84
integer taborder = 30
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
string mask = "##########"
end type

type em_physician from editmask within w_filter_document_list
integer x = 654
integer y = 1168
integer width = 347
integer height = 84
integer taborder = 40
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
string mask = "#########"
end type

