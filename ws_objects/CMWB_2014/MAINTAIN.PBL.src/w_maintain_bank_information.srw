$PBExportHeader$w_maintain_bank_information.srw
forward
global type w_maintain_bank_information from w_a_tool
end type
type uo_search from u_recipient_search within w_maintain_bank_information
end type
type cb_save from commandbutton within w_maintain_bank_information
end type
type cb_cancel from commandbutton within w_maintain_bank_information
end type
type st_name from statictext within w_maintain_bank_information
end type
type st_1 from statictext within w_maintain_bank_information
end type
type st_number from statictext within w_maintain_bank_information
end type
type gb_1 from groupbox within w_maintain_bank_information
end type
type dw_bank_info from u_datawindow within w_maintain_bank_information
end type
type st_address from statictext within w_maintain_bank_information
end type
type cb_delete from commandbutton within w_maintain_bank_information
end type
type cb_add from commandbutton within w_maintain_bank_information
end type
type st_city_postal from statictext within w_maintain_bank_information
end type
type st_address2 from statictext within w_maintain_bank_information
end type
end forward

global type w_maintain_bank_information from w_a_tool
integer width = 3301
integer height = 1836
uo_search uo_search
cb_save cb_save
cb_cancel cb_cancel
st_name st_name
st_1 st_1
st_number st_number
gb_1 gb_1
dw_bank_info dw_bank_info
st_address st_address
cb_delete cb_delete
cb_add cb_add
st_city_postal st_city_postal
st_address2 st_address2
end type
global w_maintain_bank_information w_maintain_bank_information

type variables
LONG il_recipient_no, il_original_recipient_no, il_search, il_search_recipient_no
STRING is_recipient_type_code, is_name, is_open, is_recipient_sub_type_code, is_city, is_type
n_bank_information inv_bank_info
s_message istr_window_message
end variables

forward prototypes
public subroutine wf_set_recipient_info (long al_recipient_no, string as_recipient_type_code, string as_recipient_sub_type_code, string as_name, string as_address, string as_address2, string as_city_postal)
end prototypes

public subroutine wf_set_recipient_info (long al_recipient_no, string as_recipient_type_code, string as_recipient_sub_type_code, string as_name, string as_address, string as_address2, string as_city_postal);LONG ll_row

IF al_recipient_no = 0 THEN
	st_number.Text = ''
	st_name.Text = ''
	st_address.Text = ''
	st_address2.Text = ''
	st_city_postal.Text = ''
ELSE
	inv_bank_info.nf_set_recipient_info(al_recipient_no, as_recipient_type_code, as_recipient_sub_type_code)
	
	st_number.Text = STRING(al_recipient_no)
	st_name.Text = as_name
	st_address.Text = as_address
	st_address2.Text = as_address2
	st_city_postal.Text = as_city_postal
	
	ll_row = inv_bank_info.nf_retrieve() 
	
	IF ll_row < 0 THEN
		MessageBox('Error Retrieving','There was a problem retrieving the Bank Information for the selected recipient.', Information!)
	ELSEIF ll_row = 1 THEN
		cb_add.Enabled = FALSE
		cb_delete.Enabled = TRUE
		cb_save.Enabled = FALSE
		cb_cancel.Enabled = FALSE
	ELSEIF ll_row = 0 THEN
		cb_add.Enabled = TRUE
		cb_save.Enabled = FALSE
		cb_cancel.Enabled = FALSE
		cb_delete.Enabled = FALSE
	END IF
END IF
end subroutine

on w_maintain_bank_information.create
int iCurrent
call super::create
this.uo_search=create uo_search
this.cb_save=create cb_save
this.cb_cancel=create cb_cancel
this.st_name=create st_name
this.st_1=create st_1
this.st_number=create st_number
this.gb_1=create gb_1
this.dw_bank_info=create dw_bank_info
this.st_address=create st_address
this.cb_delete=create cb_delete
this.cb_add=create cb_add
this.st_city_postal=create st_city_postal
this.st_address2=create st_address2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.uo_search
this.Control[iCurrent+2]=this.cb_save
this.Control[iCurrent+3]=this.cb_cancel
this.Control[iCurrent+4]=this.st_name
this.Control[iCurrent+5]=this.st_1
this.Control[iCurrent+6]=this.st_number
this.Control[iCurrent+7]=this.gb_1
this.Control[iCurrent+8]=this.dw_bank_info
this.Control[iCurrent+9]=this.st_address
this.Control[iCurrent+10]=this.cb_delete
this.Control[iCurrent+11]=this.cb_add
this.Control[iCurrent+12]=this.st_city_postal
this.Control[iCurrent+13]=this.st_address2
end on

on w_maintain_bank_information.destroy
call super::destroy
destroy(this.uo_search)
destroy(this.cb_save)
destroy(this.cb_cancel)
destroy(this.st_name)
destroy(this.st_1)
destroy(this.st_number)
destroy(this.gb_1)
destroy(this.dw_bank_info)
destroy(this.st_address)
destroy(this.cb_delete)
destroy(this.cb_add)
destroy(this.st_city_postal)
destroy(this.st_address2)
end on

event open;call super::open;
cb_add.Enabled = FALSE
cb_save.Enabled = FALSE
cb_cancel.Enabled = FALSE
cb_delete.Enabled = FALSE

istr_window_message = Message.PowerObjectParm
il_recipient_no = istr_window_message.al_doubleparm[1]
is_open = istr_window_message.as_stringparm[1]
is_recipient_type_code = istr_window_message.as_stringparm[2]
is_recipient_sub_type_code = istr_window_message.as_stringparm[3]
is_name = istr_window_message.as_stringparm[4]
is_city = istr_window_message.as_stringparm[5]
is_type = istr_window_message.as_stringparm[6]
il_search = istr_window_message.al_doubleparm[2]
il_search_recipient_no = istr_window_message.al_doubleparm[3]

IF IsNull(is_recipient_type_code) THEN is_recipient_type_code = ''
IF IsNull(is_recipient_sub_type_code) THEN is_recipient_sub_type_code = ''

il_original_recipient_no = il_recipient_no

uo_search.uf_set_parent(THIS)

inv_bank_info = CREATE n_bank_information
dw_bank_info.SetTransObject(SQLCA)

inv_bank_info.nf_init(dw_bank_info)
uo_search.uf_init(dw_bank_info)

IF il_recipient_no > 0 THEN
	uo_search.uf_search_recipient(il_recipient_no, is_recipient_type_code, is_recipient_sub_type_code, is_name, is_city, il_search)
END IF

end event

event closequery;call super::closequery;S_WINDOW_MESSAGE lstr_window_message
LONG ll_provider_no, ll_row

IF cb_save.Enabled = TRUE THEN
	IF MessageBox('Save Changes?','Changes have been made to the Bank Information. Would you like to save them before closing?', Question!, YesNo!)  = 1 THEN
		RETURN 1
	END IF
END IF

IF is_open = 'P' THEN
	IF il_recipient_no > 0 THEN
		lstr_window_message.as_mode = ''
		lstr_window_message.al_doubleparm[1] = il_recipient_no
		lstr_window_message.as_stringparm[1] = 'B'
		lstr_window_message.as_stringparm[2] = is_recipient_type_code
		lstr_window_message.as_stringparm[3] = is_recipient_sub_type_code
		lstr_window_message.as_stringparm[4] = is_name
		lstr_window_message.as_stringparm[5] = is_city
		lstr_window_message.as_stringparm[6] = is_type
		lstr_window_message.al_doubleparm[2] = il_search
		lstr_window_message.al_doubleparm[3] = il_search_recipient_no
		Close(THIS)				
		
		OpenSheetWithParm (w_service_provider,lstr_window_message,w_frame,2,Layered! )
	ELSE
		MessageBox('Provider Number','There was a problem getting the Provider Number for the current record.', Information!)
		RETURN
	END IF
END IF


end event

type st_title from w_a_tool`st_title within w_maintain_bank_information
integer width = 3177
string text = "Maintain Banking Information"
end type

type cb_close from w_a_tool`cb_close within w_maintain_bank_information
end type

type uo_search from u_recipient_search within w_maintain_bank_information
integer y = 88
integer width = 3209
integer height = 1184
integer taborder = 20
boolean bringtotop = true
long tabtextcolor = 0
end type

on uo_search.destroy
call u_recipient_search::destroy
end on

type cb_save from commandbutton within w_maintain_bank_information
integer x = 1303
integer y = 1680
integer width = 297
integer height = 100
integer taborder = 11
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Save"
end type

event clicked;INTEGER   li_trancount


SQLCA.nf_begin_transaction()

IF inv_bank_info.nf_save() <  0 THEN
	SQLCA.nf_transaction_count(li_trancount,0,'','','',FALSE)
	IF li_trancount > 0 THEN
		SQLCA.nf_rollback_transaction()
	END IF
	RETURN -1
ELSE
	SQLCA.nf_commit_transaction()
END IF

cb_add.Enabled = FALSE
cb_save.Enabled = FALSE
cb_cancel.Enabled = FALSE
cb_delete.Enabled = TRUE
end event

type cb_cancel from commandbutton within w_maintain_bank_information
integer x = 1605
integer y = 1680
integer width = 297
integer height = 100
integer taborder = 21
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cancel"
end type

event clicked;LONG ll_row

IF uo_search.dw_list.GetRow() > 0 THEN
	ll_row = inv_bank_info.nf_retrieve()
END IF

IF ll_row = 0 THEN
	cb_add.Enabled = TRUE
	cb_delete.Enabled = FALSE
ELSE
	cb_add.Enabled = FALSE
	cb_delete.Enabled = TRUE
END IF

cb_save.Enabled = FALSE
cb_cancel.Enabled = FALSE

end event

type st_name from statictext within w_maintain_bank_information
integer x = 174
integer y = 1396
integer width = 1225
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean focusrectangle = false
end type

type st_1 from statictext within w_maintain_bank_information
integer x = 128
integer y = 1332
integer width = 375
integer height = 60
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Recipient No.: "
boolean focusrectangle = false
end type

type st_number from statictext within w_maintain_bank_information
integer x = 581
integer y = 1332
integer width = 402
integer height = 60
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean focusrectangle = false
end type

type gb_1 from groupbox within w_maintain_bank_information
integer x = 18
integer y = 1260
integer width = 3177
integer height = 408
integer taborder = 50
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
end type

type dw_bank_info from u_datawindow within w_maintain_bank_information
integer x = 1417
integer y = 1368
integer width = 1157
integer height = 260
integer taborder = 11
boolean bringtotop = true
string dataobject = "d_bank_info"
boolean border = false
end type

event editchanged;call super::editchanged;cb_add.Enabled = FALSE
cb_save.Enabled = TRUE
cb_cancel.Enabled = TRUE
cb_delete.Enabled = FALSE
end event

type st_address from statictext within w_maintain_bank_information
integer x = 174
integer y = 1452
integer width = 1225
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean focusrectangle = false
end type

type cb_delete from commandbutton within w_maintain_bank_information
integer x = 1906
integer y = 1680
integer width = 297
integer height = 100
integer taborder = 31
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Delete"
end type

event clicked;INTEGER   li_trancount

IF MessageBox('Delete?','Are you sure you want to remove this banking information?', Question!, YesNo!) = 1 THEN
	
	SQLCA.nf_begin_transaction()
	
	IF inv_bank_info.nf_delete(dw_bank_info.GetRow()) < 0 THEN
		
		SQLCA.nf_transaction_count(li_trancount,0,'','','',FALSE)
		IF li_trancount > 0 THEN
			SQLCA.nf_rollback_transaction()
		END IF
		RETURN
	ELSE
		SQLCA.nf_commit_transaction()
	END IF
ELSE
	RETURN
END IF

IF dw_bank_info.GetRow() > 0 THEN
	cb_delete.Enabled = TRUE
	cb_add.Enabled = FALSE
ELSE
	cb_delete.Enabled = FALSE
	cb_add.Enabled = TRUE
END IF

cb_save.Enabled = FALSE
cb_cancel.Enabled = FALSE


end event

type cb_add from commandbutton within w_maintain_bank_information
integer x = 1001
integer y = 1680
integer width = 297
integer height = 100
integer taborder = 21
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Add"
end type

event clicked;LONG ll_row

inv_bank_info.nf_insert()

cb_add.Enabled = FALSE
cb_save.Enabled = TRUE
cb_cancel.Enabled = TRUE
cb_delete.Enabled = FALSE

end event

type st_city_postal from statictext within w_maintain_bank_information
integer x = 174
integer y = 1568
integer width = 1225
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean focusrectangle = false
end type

type st_address2 from statictext within w_maintain_bank_information
integer x = 174
integer y = 1512
integer width = 1225
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean focusrectangle = false
end type

