$PBExportHeader$w_filter_doc_list.srw
$PBExportComments$used to filter the document lists in association with reporting fees
forward
global type w_filter_doc_list from w_ancestor
end type
type st_8 from statictext within w_filter_doc_list
end type
type st_7 from statictext within w_filter_doc_list
end type
type st_3 from statictext within w_filter_doc_list
end type
type st_4 from statictext within w_filter_doc_list
end type
type st_5 from statictext within w_filter_doc_list
end type
type st_6 from statictext within w_filter_doc_list
end type
type st_2 from statictext within w_filter_doc_list
end type
type cb_search from commandbutton within w_filter_doc_list
end type
type cbx_all_class from checkbox within w_filter_doc_list
end type
type cbx_all from checkbox within w_filter_doc_list
end type
type dw_document_type from u_dw_online within w_filter_doc_list
end type
type dw_doc_class from u_dw_online within w_filter_doc_list
end type
type cb_cancel from commandbutton within w_filter_doc_list
end type
type gb_select_documents from groupbox within w_filter_doc_list
end type
type em_physician from editmask within w_filter_doc_list
end type
type gb_1 from groupbox within w_filter_doc_list
end type
type gb_2 from groupbox within w_filter_doc_list
end type
type st_1 from statictext within w_filter_doc_list
end type
type cb_ok from commandbutton within w_filter_doc_list
end type
end forward

global type w_filter_doc_list from w_ancestor
integer x = 1289
integer y = 316
integer width = 2395
integer height = 1908
string title = "Document Filter"
string menuname = ""
boolean maxbox = false
windowtype windowtype = popup!
long backcolor = 67108864
boolean clientedge = true
st_8 st_8
st_7 st_7
st_3 st_3
st_4 st_4
st_5 st_5
st_6 st_6
st_2 st_2
cb_search cb_search
cbx_all_class cbx_all_class
cbx_all cbx_all
dw_document_type dw_document_type
dw_doc_class dw_doc_class
cb_cancel cb_cancel
gb_select_documents gb_select_documents
em_physician em_physician
gb_1 gb_1
gb_2 gb_2
st_1 st_1
cb_ok cb_ok
end type
global w_filter_doc_list w_filter_doc_list

type variables
STRING is_filter_string
u_dw_online  idw_datawindow
u_dw_online  idw_datawindow_shared
w_sheet        iw_sheet

end variables
forward prototypes
public function boolean wf_check_claim_number (long al_claim_number)
public function boolean wf_check_provider_number (long al_provider_number)
public subroutine wf_show_current_filter ()
public subroutine wf_set_shared_dw (u_dw_online adw_datawindow_shared)
public subroutine wf_set_dw (u_dw_online adw_datawindow_main)
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

public subroutine wf_show_current_filter ();string  ls_doc_type_filter, ls_doc_class_code
long ll_start_pos = 1, ll_rows, ll_doc_class_Rowcount, ll_current_row, ll_find_row
boolean lb_found

ls_doc_type_filter = idw_datawindow.describe("Datawindow.table.filter")
IF ls_doc_type_filter <> '?' and  ls_doc_type_filter> '' THEN
	
	ll_start_pos = Pos(ls_doc_type_filter, 'docindex_type', ll_start_pos)
	DO WHILE ll_start_pos > 0
		//d_documents uses docindex_type as the doc type code, so here we replace all occurences of docindex_type with 'doc_type_code' so we can filter dw_document_type appropriately
		ls_doc_type_filter = Replace(ls_doc_type_filter, ll_start_pos,  Len('docindex_type'), 'doc_type_code')    
		ll_start_pos = Pos(ls_doc_type_filter, 'docindex_type',  ll_start_pos+Len('doc_type_code'))
	LOOP
	dw_document_type.setFilter(ls_doc_type_filter)	
	dw_document_type.filter()
	ll_rows = dw_document_type.retrieve()
	
	ll_doc_class_Rowcount = dw_doc_class.rowcount()
	
	FOR ll_current_row = 1 to ll_rows
		ls_doc_class_code = dw_document_type.getitemstring(ll_current_row, 'doc_class_code')
		CHOOSE CASE ls_doc_class_code
			CASE 'A','B','E','L','M','N','P','R','S'
				ll_find_row = dw_doc_class.find("doc_class_code = '" +ls_doc_class_code +"'" , 1, ll_doc_class_Rowcount)
				IF ll_find_row > 0 then dw_doc_class.selectrow(ll_find_row, TRUE)
		END CHOOSE
	NEXT
END IF
end subroutine

public subroutine wf_set_shared_dw (u_dw_online adw_datawindow_shared);idw_datawindow_shared = adw_datawindow_shared
end subroutine

public subroutine wf_set_dw (u_dw_online adw_datawindow_main);idw_datawindow = adw_datawindow_main
end subroutine

event open;call super::open;
// Set database object for drop down datawindow
dw_document_type.SetTransObject(SQLCA)
dw_doc_class.SetTransObject(SQLCA)

dw_doc_class.uf_setselect(3)
dw_doc_class.Retrieve()
dw_doc_class.selectrow(1,false)  // clear the automatic highlighting on the first row
	
IF IsNull(inv_resize) OR NOT IsValid (inv_resize) THEN
	inv_resize = create n_resize
	inv_resize.of_SetOrigSize (3163,1792)
END IF

THIS.inv_resize.of_register(dw_document_type, 'ScaleToRight&Bottom')
THIS.inv_resize.of_register(gb_1, 'ScaleToRight&Bottom')
THIS.inv_resize.of_register(cb_ok, 'FixedToBottom')
THIS.inv_resize.of_register(cb_cancel,'FixedToBottom')
dw_document_type.width = 2300  //not sure why, resize service object messes with this dw's starting point width, without this statement
gb_1.width = 2350                       // same as above




	


end event

on w_filter_doc_list.create
int iCurrent
call super::create
this.st_8=create st_8
this.st_7=create st_7
this.st_3=create st_3
this.st_4=create st_4
this.st_5=create st_5
this.st_6=create st_6
this.st_2=create st_2
this.cb_search=create cb_search
this.cbx_all_class=create cbx_all_class
this.cbx_all=create cbx_all
this.dw_document_type=create dw_document_type
this.dw_doc_class=create dw_doc_class
this.cb_cancel=create cb_cancel
this.gb_select_documents=create gb_select_documents
this.em_physician=create em_physician
this.gb_1=create gb_1
this.gb_2=create gb_2
this.st_1=create st_1
this.cb_ok=create cb_ok
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_8
this.Control[iCurrent+2]=this.st_7
this.Control[iCurrent+3]=this.st_3
this.Control[iCurrent+4]=this.st_4
this.Control[iCurrent+5]=this.st_5
this.Control[iCurrent+6]=this.st_6
this.Control[iCurrent+7]=this.st_2
this.Control[iCurrent+8]=this.cb_search
this.Control[iCurrent+9]=this.cbx_all_class
this.Control[iCurrent+10]=this.cbx_all
this.Control[iCurrent+11]=this.dw_document_type
this.Control[iCurrent+12]=this.dw_doc_class
this.Control[iCurrent+13]=this.cb_cancel
this.Control[iCurrent+14]=this.gb_select_documents
this.Control[iCurrent+15]=this.em_physician
this.Control[iCurrent+16]=this.gb_1
this.Control[iCurrent+17]=this.gb_2
this.Control[iCurrent+18]=this.st_1
this.Control[iCurrent+19]=this.cb_ok
end on

on w_filter_doc_list.destroy
call super::destroy
destroy(this.st_8)
destroy(this.st_7)
destroy(this.st_3)
destroy(this.st_4)
destroy(this.st_5)
destroy(this.st_6)
destroy(this.st_2)
destroy(this.cb_search)
destroy(this.cbx_all_class)
destroy(this.cbx_all)
destroy(this.dw_document_type)
destroy(this.dw_doc_class)
destroy(this.cb_cancel)
destroy(this.gb_select_documents)
destroy(this.em_physician)
destroy(this.gb_1)
destroy(this.gb_2)
destroy(this.st_1)
destroy(this.cb_ok)
end on

type st_8 from statictext within w_filter_doc_list
integer x = 9
integer y = 912
integer width = 302
integer height = 56
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "select,  or "
boolean focusrectangle = false
end type

type st_7 from statictext within w_filter_doc_list
integer x = 9
integer y = 860
integer width = 110
integer height = 56
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Use"
boolean focusrectangle = false
end type

type st_3 from statictext within w_filter_doc_list
integer x = 114
integer y = 860
integer width = 114
integer height = 56
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 255
long backcolor = 553648127
string text = "Ctrl"
boolean focusrectangle = false
end type

type st_4 from statictext within w_filter_doc_list
integer x = 219
integer y = 860
integer width = 576
integer height = 56
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "key + click for multiple"
boolean focusrectangle = false
end type

type st_5 from statictext within w_filter_doc_list
integer x = 279
integer y = 912
integer width = 142
integer height = 56
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 255
long backcolor = 553648127
string text = "Shift"
boolean focusrectangle = false
end type

type st_6 from statictext within w_filter_doc_list
integer x = 411
integer y = 912
integer width = 329
integer height = 56
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "key + click"
boolean focusrectangle = false
end type

type st_2 from statictext within w_filter_doc_list
integer x = 9
integer y = 968
integer width = 457
integer height = 64
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "for a group select"
boolean focusrectangle = false
end type

type cb_search from commandbutton within w_filter_doc_list
integer x = 608
integer y = 1188
integer width = 110
integer height = 92
integer taborder = 50
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "?"
end type

event clicked;S_WINDOW_MESSAGE lstr_message

STRING ls_type, ls_service_provider_name, ls_comment

ls_type = ''

OpenWithParm(w_service_provider_search, ls_type)

lstr_message = Message.PowerObjectParm

IF lstr_message.al_doubleparm[1] <> 0 THEN
	em_physician.text = STRING(lstr_message.al_doubleparm[1])
END IF
end event

type cbx_all_class from checkbox within w_filter_doc_list
integer x = 37
integer y = 104
integer width = 347
integer height = 100
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Select All"
end type

event clicked;
int li_ctrl

dwobject l_dwo
setnull(l_dwo) 

FOR li_ctrl = 1 to dw_doc_class.rowCount()
	dw_doc_class.selectRow(li_ctrl, this.checked)
NEXT

dw_doc_class.event clicked(1, 1, 10, l_dwo)

//dw_doc_class.triggerevent(Clicked!)  doesn't work
end event

type cbx_all from checkbox within w_filter_doc_list
integer x = 805
integer y = 104
integer width = 347
integer height = 100
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Select All"
boolean checked = true
end type

event clicked;
int li_ctrl
STRING ls_checked
IF checked THEN
	ls_checked = 'Y'
ELSE
	ls_checked = 'N'
	is_filter_string= ""
END IF

FOR li_ctrl = 1 to dw_document_type.rowCount()
	dw_document_type.setItem(li_ctrl, 'on_off', ls_checked)
NEXT
end event

type dw_document_type from u_dw_online within w_filter_doc_list
integer x = 818
integer y = 204
integer width = 1481
integer height = 1376
integer taborder = 30
string dataobject = "d_document_type_filter_list"
boolean vscrollbar = true
end type

type dw_doc_class from u_dw_online within w_filter_doc_list
integer x = 32
integer y = 204
integer width = 718
integer height = 648
integer taborder = 10
string dataobject = "d_document_class"
end type

event clicked;call super::clicked;
STRING ls_doc_class, ls_filter
int li_ctrl

dw_document_type.setRedraw(false)

IF row > 0 THEN   // rebuild the filter based on whats still on 
	FOR li_Ctrl = 1 to this.rowCount()
		
		if isSelected(li_Ctrl) THEN
			ls_doc_class = this.getItemString(li_Ctrl, 'doc_class_code')
			ls_filter =  ls_filter + " or doc_class_code = '" + ls_doc_class + "'"
		END IF	
	NEXT	
	
	IF ls_filter = '' THEN
		ls_filter = "doc_class_code ='NADA'"   //filter out everything
	ELSE
		ls_filter = mid(ls_filter, 5)  // remove the first ' or '
	END IF	
	
	dw_document_type.setFIlter(ls_filter)
	dw_document_type.filter()
	dw_document_type.retrieve()
	cbx_all.checked = TRUE
END IF

dw_document_type.setRedraw(true)
end event

type cb_cancel from commandbutton within w_filter_doc_list
integer x = 1070
integer y = 1640
integer width = 274
integer height = 108
integer taborder = 60
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cance&l"
boolean cancel = true
end type

event clicked;
    close(parent)
end event

type gb_select_documents from groupbox within w_filter_doc_list
integer x = 5
integer y = 44
integer width = 759
integer height = 796
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Select Document Class(s)"
end type

type em_physician from editmask within w_filter_doc_list
integer x = 78
integer y = 1188
integer width = 498
integer height = 100
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

type gb_1 from groupbox within w_filter_doc_list
integer x = 795
integer y = 40
integer width = 1527
integer height = 1568
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "                   Select all or some Document Types"
end type

type gb_2 from groupbox within w_filter_doc_list
integer x = 37
integer y = 1092
integer width = 709
integer height = 268
integer taborder = 40
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Provider No. (optional)"
end type

type st_1 from statictext within w_filter_doc_list
integer x = 55
integer y = 1356
integer width = 658
integer height = 216
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "To see all documents for a particular provider, enter just the provider no."
boolean focusrectangle = false
end type

type cb_ok from commandbutton within w_filter_doc_list
integer x = 613
integer y = 1644
integer width = 320
integer height = 108
integer taborder = 70
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
boolean cancel = true
boolean default = true
end type

event clicked;/*
Enterable fields and buttons for w_filter_doc_list

*/

STRING   ls_return_filter, ls_joiner, ls_document_type_filter, ls_doc_type_code, ls_provider_filter
BOOLEAN  lb_check
INT li_ctrl

LONG  ll_rehab_sheet_handle
w_rehab_sheet  lw_rehab_sheet

IF dw_document_type.AcceptText() < 0 THEN
	Return
END IF

IF dw_doc_class.AcceptText() < 0 THEN
	Return
END IF

//dw_document_type
FOR li_ctrl = 1 to dw_document_type.rowCount()
	IF dw_document_type.getItemString(li_ctrl, 'on_off') = 'Y' THEN
		ls_doc_type_code = dw_document_type.getItemString(li_ctrl,'doc_type_code')
		ls_document_type_filter = ls_document_type_filter + " or docindex_type = '" + ls_doc_type_code +"'"
	END IF
NEXT

ls_document_type_filter = mid(ls_document_type_filter, 5)  // remove the first ' or '

/* now we need to check and see if the physician number was entered
*/
IF isnumber(trim(em_physician.text)) THEN
	/* check that the provider number is valid
	*/
	lb_check = wf_check_provider_number(long(em_physician.text))
	IF lb_check = FALSE THEN
		messagebox("Document Filtering - Validation Error","The provider number " + em_physician.text + " is not valid!",Exclamation!)
		em_physician.setfocus()
		RETURN
	END IF
	
	ls_provider_filter = "service_provider_no = " + em_physician.text
ELSE
	ls_provider_filter = ""
END IF

/* Build the return filter
*/

	ls_return_filter = ""
	ls_joiner = ""
	
	IF ls_document_type_filter <> "" THEN
		ls_return_filter = ls_document_type_filter
		ls_joiner = " and "
	END IF
	
	IF ls_provider_filter <> "" THEN
		ls_return_filter = ls_return_filter + ls_joiner + ls_provider_filter
	END IF

IF isvalid(idw_datawindow) then	
	idw_datawindow.setFilter(ls_return_filter)
	idw_datawindow.filter()
	IF isvalid(idw_datawindow_shared) then
		IF ls_return_filter > '' then
			idw_datawindow_shared.object.t_filtered_indicator.visible = TRUE
		ELSE
			idw_datawindow_shared.object.t_filtered_indicator.visible = FALSE
		END IF
	END IF
END IF

/// break point here and see if it will trigger the indicator
if isvalid(iw_sheet) THEN
	iw_sheet.wf_find_related_rehab(ll_rehab_sheet_handle,lw_rehab_sheet)
	IF ll_rehab_sheet_handle > 0 THEN
		IF ls_return_filter > '' then
			lw_rehab_sheet.dw_documents.object.t_filtered_indicator.visible = 1
		ELSE
			lw_rehab_sheet.dw_documents.object.t_filtered_indicator.visible = 0
		END IF
		
	END IF
END IF

CLOSE(Parent)





	
end event

