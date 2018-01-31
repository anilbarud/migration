$PBExportHeader$w_filter_account_pay_list.srw
forward
global type w_filter_account_pay_list from w_ancestor
end type
type rb_physio from radiobutton within w_filter_account_pay_list
end type
type rb_neuropathic_account from radiobutton within w_filter_account_pay_list
end type
type rb_opioid from radiobutton within w_filter_account_pay_list
end type
type cb_ok from commandbutton within w_filter_account_pay_list
end type
type cb_cancel from commandbutton within w_filter_account_pay_list
end type
type rb_accounts_only from radiobutton within w_filter_account_pay_list
end type
type rb_accts_no_entry from radiobutton within w_filter_account_pay_list
end type
type rb_all_docs from radiobutton within w_filter_account_pay_list
end type
type rb_clinic from radiobutton within w_filter_account_pay_list
end type
type rb_doctor from radiobutton within w_filter_account_pay_list
end type
type rb_hospital from radiobutton within w_filter_account_pay_list
end type
type rb_institutional from radiobutton within w_filter_account_pay_list
end type
type rb_prescription from radiobutton within w_filter_account_pay_list
end type
type rb_personal from radiobutton within w_filter_account_pay_list
end type
type rb_travel from radiobutton within w_filter_account_pay_list
end type
type gb_accounts from groupbox within w_filter_account_pay_list
end type
end forward

global type w_filter_account_pay_list from w_ancestor
integer x = 1609
integer y = 644
integer width = 1449
integer height = 1492
string title = "Filter Account Payment List"
string menuname = ""
boolean controlmenu = false
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
long backcolor = 67108864
rb_physio rb_physio
rb_neuropathic_account rb_neuropathic_account
rb_opioid rb_opioid
cb_ok cb_ok
cb_cancel cb_cancel
rb_accounts_only rb_accounts_only
rb_accts_no_entry rb_accts_no_entry
rb_all_docs rb_all_docs
rb_clinic rb_clinic
rb_doctor rb_doctor
rb_hospital rb_hospital
rb_institutional rb_institutional
rb_prescription rb_prescription
rb_personal rb_personal
rb_travel rb_travel
gb_accounts gb_accounts
end type
global w_filter_account_pay_list w_filter_account_pay_list

event open;call super::open;

	rb_all_docs.checked = TRUE

end event

on w_filter_account_pay_list.create
int iCurrent
call super::create
this.rb_physio=create rb_physio
this.rb_neuropathic_account=create rb_neuropathic_account
this.rb_opioid=create rb_opioid
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
this.rb_accounts_only=create rb_accounts_only
this.rb_accts_no_entry=create rb_accts_no_entry
this.rb_all_docs=create rb_all_docs
this.rb_clinic=create rb_clinic
this.rb_doctor=create rb_doctor
this.rb_hospital=create rb_hospital
this.rb_institutional=create rb_institutional
this.rb_prescription=create rb_prescription
this.rb_personal=create rb_personal
this.rb_travel=create rb_travel
this.gb_accounts=create gb_accounts
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.rb_physio
this.Control[iCurrent+2]=this.rb_neuropathic_account
this.Control[iCurrent+3]=this.rb_opioid
this.Control[iCurrent+4]=this.cb_ok
this.Control[iCurrent+5]=this.cb_cancel
this.Control[iCurrent+6]=this.rb_accounts_only
this.Control[iCurrent+7]=this.rb_accts_no_entry
this.Control[iCurrent+8]=this.rb_all_docs
this.Control[iCurrent+9]=this.rb_clinic
this.Control[iCurrent+10]=this.rb_doctor
this.Control[iCurrent+11]=this.rb_hospital
this.Control[iCurrent+12]=this.rb_institutional
this.Control[iCurrent+13]=this.rb_prescription
this.Control[iCurrent+14]=this.rb_personal
this.Control[iCurrent+15]=this.rb_travel
this.Control[iCurrent+16]=this.gb_accounts
end on

on w_filter_account_pay_list.destroy
call super::destroy
destroy(this.rb_physio)
destroy(this.rb_neuropathic_account)
destroy(this.rb_opioid)
destroy(this.cb_ok)
destroy(this.cb_cancel)
destroy(this.rb_accounts_only)
destroy(this.rb_accts_no_entry)
destroy(this.rb_all_docs)
destroy(this.rb_clinic)
destroy(this.rb_doctor)
destroy(this.rb_hospital)
destroy(this.rb_institutional)
destroy(this.rb_prescription)
destroy(this.rb_personal)
destroy(this.rb_travel)
destroy(this.gb_accounts)
end on

type rb_physio from radiobutton within w_filter_account_pay_list
integer x = 398
integer y = 788
integer width = 512
integer height = 76
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Physio Accounts"
boolean lefttext = true
end type

type rb_neuropathic_account from radiobutton within w_filter_account_pay_list
integer x = 293
integer y = 1132
integer width = 617
integer height = 76
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Neuropathic Account"
boolean lefttext = true
end type

type rb_opioid from radiobutton within w_filter_account_pay_list
integer x = 402
integer y = 1048
integer width = 507
integer height = 76
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Opioid Accounts"
boolean lefttext = true
end type

type cb_ok from commandbutton within w_filter_account_pay_list
integer x = 398
integer y = 1268
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

event clicked;STRING				ls_account_filter
S_WINDOW_MESSAGE	lstr_message
	SetPointer(HourGlass!)


	IF rb_all_docs.checked THEN
		ls_account_filter = ''
		lstr_message.as_stringparm[2] = 'All Documents'
	ELSEIF rb_accounts_only.checked THEN
		ls_account_filter = "(Left(type, 1) = 'A' OR type = 'MPC' OR type = 'MPD' OR type = 'SDC' OR type = 'SDD')"
		lstr_message.as_stringparm[2] = 'Accounts Only'
	ELSEIF rb_accts_no_entry.checked THEN
		ls_account_filter = "(Left(type, 1) = 'A' OR type = 'MPC' OR type = 'MPD' OR type = 'SDC' OR type = 'SDD') AND (paid_count = 0)"
		lstr_message.as_stringparm[2] = 'Accounts with no Entry'
	ELSEIF rb_clinic.checked THEN
		ls_account_filter = "type = 'AC' OR type = 'MPC' OR type = 'SDC'"
		lstr_message.as_stringparm[2] = 'Clinic Accounts'
	ELSEIF rb_doctor.checked THEN
		ls_account_filter = "type = 'AD' OR type = 'MPD' OR type = 'SDD'"
		lstr_message.as_stringparm[2] = 'Doctor Accounts'
	ELSEIF rb_hospital.checked THEN
		ls_account_filter = "type = 'AH' or type = 'AW'"
		lstr_message.as_stringparm[2] = 'Hospital Accounts'
	ELSEIF rb_institutional.checked THEN
		ls_account_filter = "type = 'AI'"
		lstr_message.as_stringparm[2] = 'Institutional Accounts'
	ELSEIF rb_personal.checked THEN
		ls_account_filter = "type = 'AP'"
		lstr_message.as_stringparm[2] = 'Personal Accounts'
	ELSEIF rb_physio.checked THEN
		ls_account_filter = "type = 'AC' and doc_subtype_code = 'A2'"
		lstr_message.as_stringparm[2] = 'Physio Accounts'
	ELSEIF rb_prescription.checked THEN
		ls_account_filter = "type = 'AR' OR type = 'ARX'"
		lstr_message.as_stringparm[2] = 'Prescription Accounts'
	ELSEIF rb_travel.checked THEN
		ls_account_filter = "type = 'AT' or type = 'AV'"
		lstr_message.as_stringparm[2] = 'Travel Accounts'
	ELSEIF rb_opioid.checked THEN
		ls_account_filter = "type = 'AO'"
		lstr_message.as_stringparm[2] = 'Opioid Accounts'
	ELSEIF rb_neuropathic_account.checked THEN
		ls_account_filter = "type = 'ANP'"
		lstr_message.as_stringparm[2] = 'Neuropathic Accounts'
	END IF

	lstr_message.as_stringparm[1] = ''
	
	IF ls_account_filter > '' THEN
		IF lstr_message.as_stringparm[1] > '' THEN
			lstr_message.as_stringparm[1] +=  ' AND ' + ls_account_filter
		ELSE
			lstr_message.as_stringparm[1] = ls_account_filter
		END IF
	END IF
	
	CloseWithReturn(Parent,lstr_message)
end event

type cb_cancel from commandbutton within w_filter_account_pay_list
integer x = 681
integer y = 1268
integer width = 274
integer height = 108
integer taborder = 40
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cance&l"
boolean cancel = true
end type

on clicked;S_WINDOW_MESSAGE	lstr_message
	
	SetPointer(HourGlass!)
	lstr_message.as_stringparm[1] = 'Cancel'
	CloseWithReturn(Parent,lstr_message)
end on

type rb_accounts_only from radiobutton within w_filter_account_pay_list
integer x = 453
integer y = 188
integer width = 457
integer height = 76
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Accounts Only"
boolean lefttext = true
end type

type rb_accts_no_entry from radiobutton within w_filter_account_pay_list
integer x = 96
integer y = 268
integer width = 818
integer height = 76
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Accounts ready for payment"
boolean lefttext = true
end type

type rb_all_docs from radiobutton within w_filter_account_pay_list
integer x = 453
integer y = 108
integer width = 457
integer height = 76
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "All Documents"
boolean lefttext = true
end type

type rb_clinic from radiobutton within w_filter_account_pay_list
integer x = 425
integer y = 352
integer width = 485
integer height = 76
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Clinic Accounts"
boolean lefttext = true
end type

type rb_doctor from radiobutton within w_filter_account_pay_list
integer x = 393
integer y = 440
integer width = 517
integer height = 76
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Doctor Accounts"
boolean lefttext = true
end type

type rb_hospital from radiobutton within w_filter_account_pay_list
integer x = 357
integer y = 528
integer width = 553
integer height = 76
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Hospital Accounts"
boolean lefttext = true
end type

type rb_institutional from radiobutton within w_filter_account_pay_list
integer x = 265
integer y = 616
integer width = 645
integer height = 76
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Institutional Accounts"
boolean lefttext = true
end type

type rb_prescription from radiobutton within w_filter_account_pay_list
integer x = 247
integer y = 872
integer width = 663
integer height = 76
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Prescription Accounts"
boolean lefttext = true
end type

type rb_personal from radiobutton within w_filter_account_pay_list
integer x = 343
integer y = 704
integer width = 567
integer height = 76
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Personal Accounts"
boolean lefttext = true
end type

type rb_travel from radiobutton within w_filter_account_pay_list
integer x = 402
integer y = 960
integer width = 507
integer height = 76
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Travel Accounts"
boolean lefttext = true
end type

type gb_accounts from groupbox within w_filter_account_pay_list
integer x = 73
integer y = 28
integer width = 1243
integer height = 1212
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Accounts"
end type

