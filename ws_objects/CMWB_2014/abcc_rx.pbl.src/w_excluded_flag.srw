$PBExportHeader$w_excluded_flag.srw
forward
global type w_excluded_flag from w_a_tool
end type
type dw_data from u_dw_online within w_excluded_flag
end type
type st_noi from statictext within w_excluded_flag
end type
type st_bday from statictext within w_excluded_flag
end type
type st_gender from statictext within w_excluded_flag
end type
type st_addy1 from statictext within w_excluded_flag
end type
type st_addy2 from statictext within w_excluded_flag
end type
type st_postal from statictext within w_excluded_flag
end type
type st_noi_error from statictext within w_excluded_flag
end type
type st_bday_error from statictext within w_excluded_flag
end type
type st_gender_error from statictext within w_excluded_flag
end type
type st_postal_error from statictext within w_excluded_flag
end type
type st_addy2_error from statictext within w_excluded_flag
end type
type st_addy1_error from statictext within w_excluded_flag
end type
type cb_include from commandbutton within w_excluded_flag
end type
type cb_print from commandbutton within w_excluded_flag
end type
type gb_1 from groupbox within w_excluded_flag
end type
end forward

global type w_excluded_flag from w_a_tool
integer height = 1832
dw_data dw_data
st_noi st_noi
st_bday st_bday
st_gender st_gender
st_addy1 st_addy1
st_addy2 st_addy2
st_postal st_postal
st_noi_error st_noi_error
st_bday_error st_bday_error
st_gender_error st_gender_error
st_postal_error st_postal_error
st_addy2_error st_addy2_error
st_addy1_error st_addy1_error
cb_include cb_include
cb_print cb_print
gb_1 gb_1
end type
global w_excluded_flag w_excluded_flag

type variables
S_WINDOW_MESSAGE istr_message
LONG             il_claim_no, il_errors
STRING           is_excluded
DATASTORE        ids_errors

end variables

forward prototypes
public function integer wf_validate_data ()
public function integer wf_valid_date (ref string as_date)
end prototypes

public function integer wf_validate_data ();STRING   ls_noi, ls_postal, ls_gender, ls_address1, ls_address2, ls_country
DATETIME ldtm_birth_date
LONG     ll_cntr, ll_row
n_validate_address lnv_address

lnv_address = CREATE n_validate_address

ls_noi          = dw_data.GetItemString(dw_data.GetRow(),'accident_nature_of_injury_code')
ls_gender       = dw_data.GetItemString(dw_data.GetRow(),'individual_sex')
ldtm_birth_date = dw_data.GetItemDateTime(dw_data.GetRow(),'individual_birth_date')
ls_address1     = dw_data.GetItemString(dw_data.GetRow(),'individual_address_line1')
ls_address2     = dw_data.GetItemString(dw_data.GetRow(),'individual_address_line2')
ls_postal       = dw_data.GetItemString(dw_data.GetRow(),'individual_postal_code')
ls_country      = dw_data.GetItemString(dw_data.GetRow(),'individual_country_code')

//If NOI has not been coded, display as an error
IF IsNull(ls_noi) OR TRIM(ls_noi) = '' THEN
	il_errors += 1
	st_noi_error.Visible = TRUE
	st_noi.TextColor = RGB(0,0,0)	
	ll_row = ids_errors.InsertRow(0) 
	ids_errors.SetItem(ll_row,'error','The Nature Of Injury has not been coded for this claim.')
END IF

//If Gender is Unknown, dispaly as an error
IF ls_gender <> 'M' AND ls_gender <> 'F' THEN
	il_errors += 1
	st_gender_error.Visible = TRUE
	st_gender.TextColor = RGB(0,0,0)	
	ll_row = ids_errors.InsertRow(0) 
	ids_errors.SetItem(ll_row,'error','The Gender must be set to Male or Female.')
END IF

//If address line 1 does not have atleast 2 characters, display as an error
IF IsNull(ls_address1) OR LEN(TRIM(ls_address1)) < 2 THEN
	il_errors += 1
	st_addy1_error.Visible = TRUE
	st_addy1.TextColor = RGB(0,0,0)
	ll_row = ids_errors.InsertRow(0) 
	ids_errors.SetItem(ll_row,'error','The Address Line 1 must be greater or equal to 2 characters in length.')
END IF

//If address line 2 only has 1 character, display as an error
IF TRIM(ls_address2) > '' AND LEN(TRIM(ls_address2)) < 2 THEN
	il_errors += 1
	st_addy2_error.Visible = TRUE
	st_addy2.TextColor = RGB(0,0,0)
	ll_row = ids_errors.InsertRow(0) 
	ids_errors.SetItem(ll_row,'error','If entered, the Address Line 2 must be greater or equal to 2 characters in length.')
END IF

//If the country is Canada and the postal code is empty or invalid, display as an error
IF ls_country = 'CAN' THEN
	IF ls_postal > '' THEN
		IF Mid(ls_postal,4,1) <> ' ' AND Len(ls_postal) = 6 THEN
			ls_postal = Left(ls_postal,3) + ' ' + Right(ls_postal,3)
		END IF
      IF (Len(ls_postal) = 7 AND Mid(ls_postal,4,1) = ' ') &
		   AND (IsNumber(Mid(ls_postal,2,1)) AND IsNumber(Mid(ls_postal,5,1)) AND IsNumber(Mid(ls_postal,7,1))) &
			AND (lnv_address.nf_is_letter(Mid(ls_postal,1,1)) AND lnv_address.nf_is_letter(Mid(ls_postal,3,1)) AND lnv_address.nf_is_letter(Mid(ls_postal,6,1))) THEN 
		ELSE
			il_errors += 1
			st_postal_error.Visible = TRUE
			st_postal.TextColor = RGB(0,0,0)
			ll_row = ids_errors.InsertRow(0) 
			ids_errors.SetItem(ll_row,'error','The Postal Code must be in the proper Canadian format.')
		END IF
	ELSE
		il_errors += 1
		st_postal_error.Visible = TRUE
		st_postal.TextColor = RGB(0,0,0)
		ll_row = ids_errors.InsertRow(0) 
		ids_errors.SetItem(ll_row,'error','The Postal Code must be entered.')
	END IF
END IF

//If the Birth Date has not been entered, display as an error
IF IsNull(ldtm_birth_date) THEN
	il_errors += 1
	st_bday_error.Visible = TRUE
	st_bday.TextColor = RGB(0,0,0)
	ll_row = ids_errors.InsertRow(0) 
	ids_errors.SetItem(ll_row,'error','The Birthdate must be entered.')
END IF

//If there are errors, enable the Print button so the errors can be printed.
IF il_errors > 0 THEN
	ids_errors.SetItem(1,'claim_no',il_claim_no)
	cb_print.Enabled = TRUE
END IF

DESTROY lnv_address

RETURN 0
end function

public function integer wf_valid_date (ref string as_date);STRING ls_month, ls_day, ls_year, ls_date 

ls_month = LEFT(as_date,3)
ls_day   = MID(as_date,5,2)
ls_year  = RIGHT(as_date,4)

ls_date  = ls_year + '-' + ls_month + '-' + ls_day

IF IsDate(ls_date) THEN
	as_date = ls_date
ELSE
	RETURN -1
END IF

RETURN 0
end function

on w_excluded_flag.create
int iCurrent
call super::create
this.dw_data=create dw_data
this.st_noi=create st_noi
this.st_bday=create st_bday
this.st_gender=create st_gender
this.st_addy1=create st_addy1
this.st_addy2=create st_addy2
this.st_postal=create st_postal
this.st_noi_error=create st_noi_error
this.st_bday_error=create st_bday_error
this.st_gender_error=create st_gender_error
this.st_postal_error=create st_postal_error
this.st_addy2_error=create st_addy2_error
this.st_addy1_error=create st_addy1_error
this.cb_include=create cb_include
this.cb_print=create cb_print
this.gb_1=create gb_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_data
this.Control[iCurrent+2]=this.st_noi
this.Control[iCurrent+3]=this.st_bday
this.Control[iCurrent+4]=this.st_gender
this.Control[iCurrent+5]=this.st_addy1
this.Control[iCurrent+6]=this.st_addy2
this.Control[iCurrent+7]=this.st_postal
this.Control[iCurrent+8]=this.st_noi_error
this.Control[iCurrent+9]=this.st_bday_error
this.Control[iCurrent+10]=this.st_gender_error
this.Control[iCurrent+11]=this.st_postal_error
this.Control[iCurrent+12]=this.st_addy2_error
this.Control[iCurrent+13]=this.st_addy1_error
this.Control[iCurrent+14]=this.cb_include
this.Control[iCurrent+15]=this.cb_print
this.Control[iCurrent+16]=this.gb_1
end on

on w_excluded_flag.destroy
call super::destroy
destroy(this.dw_data)
destroy(this.st_noi)
destroy(this.st_bday)
destroy(this.st_gender)
destroy(this.st_addy1)
destroy(this.st_addy2)
destroy(this.st_postal)
destroy(this.st_noi_error)
destroy(this.st_bday_error)
destroy(this.st_gender_error)
destroy(this.st_postal_error)
destroy(this.st_addy2_error)
destroy(this.st_addy1_error)
destroy(this.cb_include)
destroy(this.cb_print)
destroy(this.gb_1)
end on

event open;call super::open;LONG  ll_rows, ll_claim_no

ids_errors  = CREATE DATASTORE

ids_errors.SetTransObject(SQLCA)
ids_errors.DataObject = 'd_excluded_errors'

istr_message = Message.PowerObjectParm
is_excluded  = istr_message.as_stringparm[1]

iw_active_sheet = w_frame.GetActiveSheet()
il_claim_no = iw_active_sheet.dw_basic_claim.GetItemNumber(1,'claim_no')

dw_data.SetTransObject(SQLCA)


ll_rows = dw_data.Retrieve(il_claim_no)

IF ll_rows <= 0 THEN
	MessageBox('No Claim','There is no information to display. Please call Help Desk.',information!)
	RETURN -1
END IF

IF is_excluded = 'N' THEN
	cb_include.Text = 'Exclude'
ELSE
	cb_include.Text = 'Include'
END IF
cb_include.Enabled = TRUE	
cb_print.Enabled   = FALSE

//Validate data to check if there are problems with the data
IF wf_validate_data() < 0 THEN
	MessageBox('Error','There was a problem validating data. Please call Help Desk.',information!)
	RETURN -1
END IF


end event

event close;call super::close;DESTROY ids_errors

end event

type st_title from w_a_tool`st_title within w_excluded_flag
integer width = 2610
string text = "Excluded Claim"
end type

type cb_close from w_a_tool`cb_close within w_excluded_flag
integer y = 1656
integer width = 325
end type

type dw_data from u_dw_online within w_excluded_flag
integer x = 59
integer y = 124
integer width = 2528
integer height = 1096
integer taborder = 10
boolean bringtotop = true
string dataobject = "d_exclude_flag"
boolean border = false
end type

type st_noi from statictext within w_excluded_flag
integer x = 421
integer y = 1344
integer width = 805
integer height = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 134217745
long backcolor = 67108864
boolean enabled = false
string text = "Nature Of Injury Not Coded"
boolean focusrectangle = false
end type

type st_bday from statictext within w_excluded_flag
integer x = 421
integer y = 1420
integer width = 805
integer height = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 134217745
long backcolor = 67108864
boolean enabled = false
string text = "Birth Date Not Entered"
boolean focusrectangle = false
end type

type st_gender from statictext within w_excluded_flag
integer x = 421
integer y = 1496
integer width = 805
integer height = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 134217745
long backcolor = 67108864
boolean enabled = false
string text = "Gender Invalid"
boolean focusrectangle = false
end type

type st_addy1 from statictext within w_excluded_flag
integer x = 1618
integer y = 1344
integer width = 741
integer height = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 134217745
long backcolor = 67108864
boolean enabled = false
string text = "Address Line 1 Invalid"
boolean focusrectangle = false
end type

type st_addy2 from statictext within w_excluded_flag
integer x = 1618
integer y = 1420
integer width = 741
integer height = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 134217745
long backcolor = 67108864
boolean enabled = false
string text = "Address Line 2 Invalid"
boolean focusrectangle = false
end type

type st_postal from statictext within w_excluded_flag
integer x = 1618
integer y = 1496
integer width = 741
integer height = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 134217745
long backcolor = 67108864
boolean enabled = false
string text = "Postal Code Invalid"
boolean focusrectangle = false
end type

type st_noi_error from statictext within w_excluded_flag
boolean visible = false
integer x = 311
integer y = 1344
integer width = 46
integer height = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 255
long backcolor = 67108864
string text = "x"
boolean focusrectangle = false
end type

type st_bday_error from statictext within w_excluded_flag
boolean visible = false
integer x = 311
integer y = 1420
integer width = 46
integer height = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 255
long backcolor = 67108864
string text = "x"
boolean focusrectangle = false
end type

type st_gender_error from statictext within w_excluded_flag
boolean visible = false
integer x = 311
integer y = 1496
integer width = 46
integer height = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 255
long backcolor = 67108864
string text = "x"
boolean focusrectangle = false
end type

type st_postal_error from statictext within w_excluded_flag
boolean visible = false
integer x = 1518
integer y = 1496
integer width = 46
integer height = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 255
long backcolor = 67108864
string text = "x"
boolean focusrectangle = false
end type

type st_addy2_error from statictext within w_excluded_flag
boolean visible = false
integer x = 1518
integer y = 1420
integer width = 46
integer height = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 255
long backcolor = 67108864
string text = "x"
boolean focusrectangle = false
end type

type st_addy1_error from statictext within w_excluded_flag
boolean visible = false
integer x = 1518
integer y = 1344
integer width = 46
integer height = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 255
long backcolor = 67108864
string text = "x"
boolean focusrectangle = false
end type

type cb_include from commandbutton within w_excluded_flag
integer x = 1874
integer y = 1656
integer width = 325
integer height = 100
integer taborder = 11
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Include"
end type

event clicked;STRING   ls_message, ls_term_date, ls_err_message, ls_isdate
INTEGER  li_rtn
LONG     ll_count, ll_unprocessed
DATE     ldt_term_date
DATETIME ldtm_death_date
BOOLEAN  lb_use_death_date, lb_not_eligible
N_BR_BLUECROSS	 lnv_bc

lnv_bc = Create n_br_bluecross

IF is_excluded = 'N' THEN
	/* Doublecheck the claim has not been registered.
	   A claim cannot be 'Excluded' once it has been registered.
	*/
/* use the functionality in n_br_bluecross in place of embedded SQL */
	IF lnv_bc.uf_is_registered( il_claim_no) = 1 THEN  // claim is registered
		MessageBox('Error','This claim is already registered for drug coverage.' &
					+ 'The claim cannot be Excluded.',information!)
		RETURN -1
	END IF
	
	IF MessageBox(THIS.Text,'Are you sure you would like to Exclude this claim from drug coverage?',question!,yesno!) = 2 THEN
		RETURN -1
	END IF
	
	/* If the claim is being 'Excluded', no unprocessed (record_no = 0) manual 
		eligibility records should exist - remove them if they do.
	*/
	/* this functionality has been removed as per :
	Project P10236 – Pharmaceutical Epay Phase 2, Detailed Design, 
	Changes to Maintain Excluded Claim   */
	
	is_excluded = 'Y'
ELSE
	IF MessageBox(THIS.Text,'Are you sure you would like to Include this claim for drug coverage?',question!,yesno!) = 2 THEN
		RETURN -1
	END IF
	is_excluded = 'N'
END IF


SQLCA.nf_begin_Transaction()

/* Update the excluded_flag.
*/

UPDATE X001_EXCLUDED_CLAIM
SET    excluded_flag = :is_excluded
WHERE  claim_no      = :il_claim_no
USING  SQLCA;
	
li_rtn = SQLCA.nf_handle_error('w_excluded_flag', 'cb_include', 'UPDATE X001_EXCLUDED_CLAIM')

/* If everything has gone well, display a message.
*/
IF is_excluded = 'N'	THEN 

	MessageBox('Included','This claim will be included in the next Eligibility Export.~rYou can view details on coverage through the Rx Coverage Indicator Screen.',information!)

	This.Text = 'Exclude'
ELSE

	MessageBox('Excluded','This claim has been Excluded. It will not be included in the next Eligibility Export.',information!)
	This.Text = 'Include'
END IF

IF li_rtn < 0 THEN
	SQLCA.nf_rollback_transaction()
	MessageBox('Error Updating','A problem was encountered during the update. Please call Help Desk.',information!)
	RETURN -1
END IF

SQLCA.nf_commit_transaction()

iw_active_sheet.wf_set_claim(il_claim_no)

end event

type cb_print from commandbutton within w_excluded_flag
integer x = 1486
integer y = 1656
integer width = 357
integer height = 100
integer taborder = 30
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Print Errors"
end type

event clicked;IF ids_errors.RowCount() > 0 THEN
	ids_errors.Object.DataWindow.Print.Orientation = 2
	ids_errors.Print()
ELSE
	MessageBox('No Errors','There are no errors to be printed.',information!)
END IF
end event

type gb_1 from groupbox within w_excluded_flag
integer x = 87
integer y = 1244
integer width = 2478
integer height = 376
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
string text = "Errors To Be Corrected Prior to Registration"
borderstyle borderstyle = styleraised!
end type

