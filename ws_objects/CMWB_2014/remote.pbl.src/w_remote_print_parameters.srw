$PBExportHeader$w_remote_print_parameters.srw
forward
global type w_remote_print_parameters from w_ancestor
end type
type cb_non_member from commandbutton within w_remote_print_parameters
end type
type cb_member from commandbutton within w_remote_print_parameters
end type
type st_2 from statictext within w_remote_print_parameters
end type
type st_1 from statictext within w_remote_print_parameters
end type
type cb_cancel from commandbutton within w_remote_print_parameters
end type
type cb_save from commandbutton within w_remote_print_parameters
end type
type dw_user_profile_selected from u_dw_online within w_remote_print_parameters
end type
type cb_close from commandbutton within w_remote_print_parameters
end type
type dw_parameters from u_dw_online within w_remote_print_parameters
end type
type dw_user_profile_all from u_dw_online within w_remote_print_parameters
end type
end forward

global type w_remote_print_parameters from w_ancestor
integer x = 5
integer y = 4
integer width = 2766
integer height = 2488
string title = "Remote Print Parameter Maintenance"
string menuname = "m_cmwb_notools"
windowtype windowtype = main!
long backcolor = 79741120
event ue_print ( )
cb_non_member cb_non_member
cb_member cb_member
st_2 st_2
st_1 st_1
cb_cancel cb_cancel
cb_save cb_save
dw_user_profile_selected dw_user_profile_selected
cb_close cb_close
dw_parameters dw_parameters
dw_user_profile_all dw_user_profile_all
end type
global w_remote_print_parameters w_remote_print_parameters

type variables

end variables

forward prototypes
public function integer wf_retrieve ()
end prototypes

event ue_print;long ll_job

ll_job = PrintOpen( )
This.Print(ll_job, 1,1)
PrintClose(ll_job)


end event

public function integer wf_retrieve ();dw_user_profile_all.Retrieve()
dw_parameters.Retrieve()
dw_user_profile_all.RowsCopy(1, dw_user_profile_all.RowCount(), Primary!, dw_user_profile_selected, 1, Primary!)
dw_user_profile_all.SetFilter('custom_user_profile_coup_automation_flag = "N"')
dw_user_profile_all.Filter()
dw_user_profile_all.SelectRow(0, FALSE)
dw_user_profile_selected.Reset()
dw_user_profile_all.RowsCopy(1, dw_user_profile_all.RowCount(), Filter!, dw_user_profile_selected, 1, Primary!)
dw_user_profile_selected.Sort()
dw_parameters.SetFocus()
Return 0
end function

event open;call super::open;dw_user_profile_all.SetTransObject(SQLCA)
dw_parameters.SetTransObject(SQLCA)
wf_retrieve()
end event

on w_remote_print_parameters.create
int iCurrent
call super::create
if IsValid(this.MenuID) then destroy(this.MenuID)
if this.MenuName = "m_cmwb_notools" then this.MenuID = create m_cmwb_notools
this.cb_non_member=create cb_non_member
this.cb_member=create cb_member
this.st_2=create st_2
this.st_1=create st_1
this.cb_cancel=create cb_cancel
this.cb_save=create cb_save
this.dw_user_profile_selected=create dw_user_profile_selected
this.cb_close=create cb_close
this.dw_parameters=create dw_parameters
this.dw_user_profile_all=create dw_user_profile_all
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_non_member
this.Control[iCurrent+2]=this.cb_member
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.st_1
this.Control[iCurrent+5]=this.cb_cancel
this.Control[iCurrent+6]=this.cb_save
this.Control[iCurrent+7]=this.dw_user_profile_selected
this.Control[iCurrent+8]=this.cb_close
this.Control[iCurrent+9]=this.dw_parameters
this.Control[iCurrent+10]=this.dw_user_profile_all
end on

on w_remote_print_parameters.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_non_member)
destroy(this.cb_member)
destroy(this.st_2)
destroy(this.st_1)
destroy(this.cb_cancel)
destroy(this.cb_save)
destroy(this.dw_user_profile_selected)
destroy(this.cb_close)
destroy(this.dw_parameters)
destroy(this.dw_user_profile_all)
end on

event closequery;call super::closequery;dw_parameters.AcceptText()
dw_user_profile_all.AcceptText()
IF dw_user_profile_all.ModifiedCount() > 0 OR dw_parameters.ModifiedCount() > 0 THEN
	MessageBox('Error', 'Please save or cancel changes before closing window')
	RETURN 1
END IF
end event

type cb_non_member from commandbutton within w_remote_print_parameters
integer x = 1294
integer y = 1280
integer width = 133
integer height = 108
integer taborder = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = ">>"
end type

event clicked;long		ll_row, ll_row2, ll_count
string	ls_claim_manager

ll_row = dw_user_profile_selected.GetSelectedRow(0)
IF ll_row = 0 THEN
	MessageBox('Error', 'Please select a Member user first')
	Return -1
END IF

SetPointer(HourGlass!)

ls_claim_manager = dw_user_profile_selected.GetItemString(ll_row, 'custom_user_profile_user_id')
SELECT COUNT(*)
INTO :ll_count
FROM CLAIM
WHERE admin_region_code > ''
AND claim_status_code = 'P'
AND claim_manager_user_id = :ls_claim_manager
USING SQLCA;

IF SQLCA.nf_handle_error('w_remote_print_parameters','cb_non_member.clicked', 'Embedded select from CLAIM') < 0 THEN
	Return -1
END IF

IF ll_count > 0 THEN
	MessageBox('Warning', 'Claim Manager is to assigned Pre-Adjudication claims.')
END IF

dw_user_profile_all.SetRedraw(FALSE)
dw_user_profile_all.SetFilter('')
dw_user_profile_all.Filter()
ll_row2 = dw_user_profile_all.Find('custom_user_profile_user_id = "' + dw_user_profile_selected.GetItemString(ll_row, 'custom_user_profile_user_id') + '"', 1, dw_user_profile_all.RowCount())
dw_user_profile_all.SetItem(ll_row2, 'custom_user_profile_coup_automation_flag', 'N')
dw_user_profile_selected.DeleteRow(ll_row)
dw_user_profile_all.SetRedraw(TRUE)

dw_user_profile_all.SetFilter('custom_user_profile_coup_automation_flag = "N"')
dw_user_profile_all.Filter()

dw_user_profile_selected.Reset()
dw_user_profile_all.RowsCopy(1, dw_user_profile_all.RowCount(), Filter!, dw_user_profile_selected, 1, Primary!)
dw_user_profile_selected.Sort()
end event

type cb_member from commandbutton within w_remote_print_parameters
integer x = 1294
integer y = 1136
integer width = 133
integer height = 108
integer taborder = 50
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "<<"
end type

event clicked;long	ll_row, ll_row2

ll_row = dw_user_profile_all.GetSelectedRow(0)
IF ll_row = 0 THEN
	MessageBox('Error', 'Please select a Non Member user first')
	Return -1
END IF
IF dw_user_profile_all.GetItemString(ll_row, 'custom_user_profile_active_flag') = 'N' THEN
	MessageBox('Error', 'User is inactive on system and cannot be added to coup automation at this time')
	Return -1
END IF

dw_user_profile_all.SetItem(ll_row, 'custom_user_profile_coup_automation_flag', 'Y')
dw_user_profile_all.Filter()
dw_user_profile_selected.Reset()
dw_user_profile_all.RowsCopy(1, dw_user_profile_all.RowCount(), Filter!, dw_user_profile_selected, 1, Primary!)
dw_user_profile_selected.Sort()

end event

type st_2 from statictext within w_remote_print_parameters
integer x = 1463
integer y = 1024
integer width = 846
integer height = 76
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Non Coup Automation Members:"
boolean focusrectangle = false
end type

type st_1 from statictext within w_remote_print_parameters
integer x = 5
integer y = 1016
integer width = 768
integer height = 76
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Coup Automation Members:"
boolean focusrectangle = false
end type

type cb_cancel from commandbutton within w_remote_print_parameters
integer x = 1307
integer y = 2188
integer width = 379
integer height = 108
integer taborder = 40
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cancel"
end type

event clicked;wf_retrieve()
end event

type cb_save from commandbutton within w_remote_print_parameters
integer x = 919
integer y = 2188
integer width = 379
integer height = 108
integer taborder = 30
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Save"
end type

event clicked;LONG ll_last_rfi_sent_days_nc
LONG ll_doc_age_days_nc
LONG ll_last_rfi_sent_days_cr
LONG ll_doc_age_days_cr
LONG ll_claim_age_days_cr
LONG ll_claim_age_days_rfi
LONG ll_max_auto_page_letter
LONG ll_max_auto_page_legal
LONG ll_max_auto_letters_same_recip
LONG ll_rtn1, ll_rtn2

dw_parameters.AcceptText()
dw_user_profile_all.AcceptText()

// Edit checks
ll_last_rfi_sent_days_nc = dw_parameters.GetItemNumber(1, 'last_rfi_sent_days_nc')
ll_doc_age_days_nc = dw_parameters.GetItemNumber(1, 'doc_age_days_nc')
ll_last_rfi_sent_days_cr = dw_parameters.GetItemNumber(1, 'last_rfi_sent_days_cr')
ll_doc_age_days_cr = dw_parameters.GetItemNumber(1, 'doc_age_days_cr')
ll_claim_age_days_cr = dw_parameters.GetItemNumber(1, 'claim_age_days_cr')
ll_claim_age_days_rfi = dw_parameters.GetItemNumber(1, 'claim_age_days_rfi')
ll_max_auto_page_letter = dw_parameters.GetItemNumber(1, 'max_auto_page_letter')
ll_max_auto_page_legal = dw_parameters.GetItemNumber(1, 'max_auto_page_legal')
ll_max_auto_letters_same_recip = dw_parameters.GetItemNumber(1, 'max_auto_letters_same_recip')

IF ll_last_rfi_sent_days_nc < 1 OR ll_last_rfi_sent_days_nc > 30 THEN
	MessageBox('Validation Error', 'The valid range for last_rfi_sent_days_nc is 1 - 30')
	dw_parameters.SetColumn('last_rfi_sent_days_nc')
	dw_parameters.SetFocus()
	Return -1
END IF
IF ll_doc_age_days_nc < 20 OR ll_doc_age_days_nc > 50 THEN
	MessageBox('Validation Error', 'The valid range for doc_age_days_nc is 20 - 50')
	dw_parameters.SetColumn('doc_age_days_nc')
	dw_parameters.SetFocus()
	Return -1
END IF
IF ll_last_rfi_sent_days_cr < 1 OR ll_last_rfi_sent_days_cr > 30 THEN
	MessageBox('Validation Error', 'The valid range for last_rfi_sent_days_cr is 1 - 30')
	dw_parameters.SetColumn('last_rfi_sent_days_cr')
	dw_parameters.SetFocus()
	Return -1
END IF
IF ll_doc_age_days_cr < 20 OR ll_doc_age_days_cr > 60 THEN
	MessageBox('Validation Error', 'The valid range for doc_age_days_cr is 20 - 60')
	dw_parameters.SetColumn('doc_age_days_cr')
	dw_parameters.SetFocus()
	Return -1
END IF
IF ll_claim_age_days_cr < 20 OR ll_claim_age_days_cr > 60 THEN
	MessageBox('Validation Error', 'The valid range for claim_age_days_cr is 20 - 60')
	dw_parameters.SetColumn('claim_age_days_cr')
	dw_parameters.SetFocus()
	Return -1
END IF
IF ll_claim_age_days_rfi < 1 OR ll_claim_age_days_rfi > 30 THEN
	MessageBox('Validation Error', 'The valid range for claim_age_days_rfi is 1 - 30')
	dw_parameters.SetColumn('claim_age_days_rfi')
	dw_parameters.SetFocus()
	Return -1
END IF
IF ll_max_auto_page_letter < 1 OR ll_max_auto_page_letter > 10 THEN
	MessageBox('Validation Error', 'The valid range for max_auto_page_letter is 1 - 10')
	dw_parameters.SetColumn('max_auto_page_letter')
	dw_parameters.SetFocus()
	Return -1
END IF
IF ll_max_auto_page_legal < 1 OR ll_max_auto_page_legal > 10 THEN
	MessageBox('Validation Error', 'The valid range for max_auto_page_legal is 1 - 10')
	dw_parameters.SetColumn('max_auto_page_legal')
	dw_parameters.SetFocus()
	Return -1
END IF
IF ll_max_auto_letters_same_recip < 1 OR ll_max_auto_letters_same_recip > 20 THEN
	MessageBox('Validation Error', 'The valid range for max_auto_letters_same_recip is 1 - 20')
	dw_parameters.SetColumn('max_auto_letters_same_recip')
	dw_parameters.SetFocus()
	Return -1
END IF


SQLCA.nf_begin_transaction()

ll_rtn1 = dw_user_profile_all.Update()
IF ll_rtn1 <> 1 THEN
	SQLCA.SQLDBCode = -1
END IF
IF SQLCA.nf_handle_error("dw_user_profile.Update()","cb_save.clicked","w_remote_print_parameters") < 0 THEN
	Return -1
END IF

ll_rtn2 = dw_parameters.Update()
IF ll_rtn2 <> 1 THEN
	SQLCA.SQLDBCode = -1
END IF
IF SQLCA.nf_handle_error("dw_parameters.Update()","cb_save.clicked","w_remote_print_parameters") < 0 THEN
	Return -1
END IF

IF ll_rtn1 = 1 AND ll_rtn2 = 1 THEN
	SQLCA.nf_commit_transaction()
ELSE
	SQLCA.nf_rollback_transaction()
END IF

dw_user_profile_all.Retrieve()

dw_parameters.Retrieve()
end event

type dw_user_profile_selected from u_dw_online within w_remote_print_parameters
integer x = 5
integer y = 1100
integer width = 1243
integer height = 1056
integer taborder = 80
string dataobject = "d_user_profile"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event rowfocuschanged;SelectRow(0,FALSE)
SelectRow(currentrow,TRUE)
end event

event clicked;SelectRow(0, FALSE)
IF row > 0 THEN
	SelectRow(row, TRUE)
END IF
end event

event getfocus;dw_user_profile_all.SelectRow(0, FALSE)
w_frame.SetMicroHelp('Click >> button to remove users from Coup Automation process')
end event

event losefocus;w_frame.SetMicroHelp('')
end event

type cb_close from commandbutton within w_remote_print_parameters
integer x = 2158
integer y = 2188
integer width = 379
integer height = 108
integer taborder = 70
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Close"
end type

event clicked;SetPointer(HourGlass!)
Close(Parent)
end event

type dw_parameters from u_dw_online within w_remote_print_parameters
integer x = 5
integer y = 116
integer width = 2702
integer height = 896
integer taborder = 10
string dataobject = "d_remote_print_parameter"
end type

event getfocus;call super::getfocus;String ls_tag

ls_tag = THIS.Describe(THIS.GetColumnName() + '.Tag')
w_frame.SetMicroHelp(ls_tag)
end event

event itemfocuschanged;call super::itemfocuschanged;String ls_tag

ls_tag = THIS.Describe(dwo.name + '.Tag')
w_frame.SetMicroHelp(ls_tag)
end event

event losefocus;call super::losefocus;w_frame.SetMicroHelp('')
end event

type dw_user_profile_all from u_dw_online within w_remote_print_parameters
integer x = 1463
integer y = 1100
integer width = 1243
integer height = 1056
integer taborder = 20
string dataobject = "d_user_profile"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event clicked;call super::clicked;SelectRow(0, FALSE)
IF row > 0 THEN
	SelectRow(row, TRUE)
END IF
end event

event getfocus;call super::getfocus;dw_user_profile_selected.SelectRow(0, FALSE)
w_frame.SetMicroHelp('Click << button to add users to Coup Automation process')
end event

event losefocus;call super::losefocus;w_frame.SetMicroHelp('')
end event

event rowfocuschanged;call super::rowfocuschanged;SelectRow(0,FALSE)
SelectRow(currentrow,TRUE)
end event

event we_keydown;call super::we_keydown;long ll_row

Choose Case key
	Case KeyA!
		ll_row = Find('Left(custom_user_profile_user_id, 1) = "a"', 1, RowCount())
	Case KeyB!
		ll_row = Find('Left(custom_user_profile_user_id, 1) = "b"', 1, RowCount())
	Case KeyC!
		ll_row = Find('Left(custom_user_profile_user_id, 1) = "c"', 1, RowCount())
	Case KeyD!
		ll_row = Find('Left(custom_user_profile_user_id, 1) = "d"', 1, RowCount())
	Case KeyE!
		ll_row = Find('Left(custom_user_profile_user_id, 1) = "e"', 1, RowCount())
	Case KeyF!
		ll_row = Find('Left(custom_user_profile_user_id, 1) = "f"', 1, RowCount())
	Case KeyG!
		ll_row = Find('Left(custom_user_profile_user_id, 1) = "g"', 1, RowCount())
	Case KeyH!
		ll_row = Find('Left(custom_user_profile_user_id, 1) = "h"', 1, RowCount())
	Case KeyI!
		ll_row = Find('Left(custom_user_profile_user_id, 1) = "i"', 1, RowCount())
	Case KeyJ!
		ll_row = Find('Left(custom_user_profile_user_id, 1) = "j"', 1, RowCount())
	Case KeyK!
		ll_row = Find('Left(custom_user_profile_user_id, 1) = "k"', 1, RowCount())
	Case KeyL!
		ll_row = Find('Left(custom_user_profile_user_id, 1) = "l"', 1, RowCount())
	Case KeyM!
		ll_row = Find('Left(custom_user_profile_user_id, 1) = "m"', 1, RowCount())
	Case KeyN!
		ll_row = Find('Left(custom_user_profile_user_id, 1) = "n"', 1, RowCount())
	Case KeyO!
		ll_row = Find('Left(custom_user_profile_user_id, 1) = "o"', 1, RowCount())
	Case KeyP!
		ll_row = Find('Left(custom_user_profile_user_id, 1) = "p"', 1, RowCount())
	Case KeyQ!
		ll_row = Find('Left(custom_user_profile_user_id, 1) = "q"', 1, RowCount())
	Case KeyR!
		ll_row = Find('Left(custom_user_profile_user_id, 1) = "r"', 1, RowCount())
	Case KeyS!
		ll_row = Find('Left(custom_user_profile_user_id, 1) = "s"', 1, RowCount())
	Case KeyT!
		ll_row = Find('Left(custom_user_profile_user_id, 1) = "t"', 1, RowCount())
	Case KeyU!
		ll_row = Find('Left(custom_user_profile_user_id, 1) = "u"', 1, RowCount())
	Case KeyV!
		ll_row = Find('Left(custom_user_profile_user_id, 1) = "v"', 1, RowCount())
	Case KeyW!
		ll_row = Find('Left(custom_user_profile_user_id, 1) = "w"', 1, RowCount())
	Case KeyX!
		ll_row = Find('Left(custom_user_profile_user_id, 1) = "x"', 1, RowCount())
	Case KeyY!
		ll_row = Find('Left(custom_user_profile_user_id, 1) = "y"', 1, RowCount())
	Case KeyZ!
		ll_row = Find('Left(custom_user_profile_user_id, 1) = "z"', 1, RowCount())
End Choose

IF ll_row > 0 THEN
	ScrollToRow(ll_row)
	SelectRow(0, FALSE)
	SelectRow(ll_row, TRUE)
END IF
end event

