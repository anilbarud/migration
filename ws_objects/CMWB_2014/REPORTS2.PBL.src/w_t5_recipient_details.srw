$PBExportHeader$w_t5_recipient_details.srw
forward
global type w_t5_recipient_details from w_a_report
end type
type cb_retrieve from commandbutton within w_t5_recipient_details
end type
type st_transmission_date from statictext within w_t5_recipient_details
end type
type cb_print_details from commandbutton within w_t5_recipient_details
end type
type cb_close from commandbutton within w_t5_recipient_details
end type
type cb_search from commandbutton within w_t5_recipient_details
end type
type st_1 from statictext within w_t5_recipient_details
end type
type sle_individual from singlelineedit within w_t5_recipient_details
end type
type st_2 from statictext within w_t5_recipient_details
end type
type sle_claim_no from singlelineedit within w_t5_recipient_details
end type
type cb_by_payee from commandbutton within w_t5_recipient_details
end type
type dw_by_payee from u_dw_online within w_t5_recipient_details
end type
type em_taxation_year from editmask within w_t5_recipient_details
end type
end forward

global type w_t5_recipient_details from w_a_report
integer width = 2821
integer height = 2736
string title = "T5 Recipient Details Report"
cb_retrieve cb_retrieve
st_transmission_date st_transmission_date
cb_print_details cb_print_details
cb_close cb_close
cb_search cb_search
st_1 st_1
sle_individual sle_individual
st_2 st_2
sle_claim_no sle_claim_no
cb_by_payee cb_by_payee
dw_by_payee dw_by_payee
em_taxation_year em_taxation_year
end type
global w_t5_recipient_details w_t5_recipient_details

type variables
DATETIME	   idtm_start_date, idtm_end_date
STRING      is_org_select

LONG        il_design_time_height, il_design_time_width, il_workspace_width_diff, il_workspace_height_diff
N_DW_RESIZE inv_resize
end variables

forward prototypes
public subroutine wf_setresize (boolean ab_switch)
end prototypes

public subroutine wf_setresize (boolean ab_switch);

IF ab_switch = True Then
	IF il_design_time_height = 0 or il_design_time_width = 0 THEN
		SignalError(-666,'The resize service requires that both the il_design_time_height and il_design_time_width be filled in.')
	End if
	
	/* default instance of the resize object */
	IF IsNull(inv_resize) OR NOT IsValid (inv_resize) THEN
		inv_resize = create n_dw_resize
		If this.WindowType = Child! Then
			inv_resize.of_SetOrigSize (il_design_time_width , il_design_time_height)
			inv_resize.of_SetMinSize (il_design_time_width , il_design_time_height)
		Else
			inv_resize.of_SetOrigSize (il_design_time_width - il_workspace_width_diff, il_design_time_height - il_workspace_height_diff)
			inv_resize.of_SetMinSize (il_design_time_width - il_workspace_width_diff, il_design_time_height - il_workspace_height_diff)
		End if
	END IF 
Else
	Destroy inv_resize
End if
end subroutine

on w_t5_recipient_details.create
int iCurrent
call super::create
this.cb_retrieve=create cb_retrieve
this.st_transmission_date=create st_transmission_date
this.cb_print_details=create cb_print_details
this.cb_close=create cb_close
this.cb_search=create cb_search
this.st_1=create st_1
this.sle_individual=create sle_individual
this.st_2=create st_2
this.sle_claim_no=create sle_claim_no
this.cb_by_payee=create cb_by_payee
this.dw_by_payee=create dw_by_payee
this.em_taxation_year=create em_taxation_year
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_retrieve
this.Control[iCurrent+2]=this.st_transmission_date
this.Control[iCurrent+3]=this.cb_print_details
this.Control[iCurrent+4]=this.cb_close
this.Control[iCurrent+5]=this.cb_search
this.Control[iCurrent+6]=this.st_1
this.Control[iCurrent+7]=this.sle_individual
this.Control[iCurrent+8]=this.st_2
this.Control[iCurrent+9]=this.sle_claim_no
this.Control[iCurrent+10]=this.cb_by_payee
this.Control[iCurrent+11]=this.dw_by_payee
this.Control[iCurrent+12]=this.em_taxation_year
end on

on w_t5_recipient_details.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_retrieve)
destroy(this.st_transmission_date)
destroy(this.cb_print_details)
destroy(this.cb_close)
destroy(this.cb_search)
destroy(this.st_1)
destroy(this.sle_individual)
destroy(this.st_2)
destroy(this.sle_claim_no)
destroy(this.cb_by_payee)
destroy(this.dw_by_payee)
destroy(this.em_taxation_year)
end on

event open;Long    ll_num_rows, ll_row
Integer li_rtn
datetime ldt_server_datetime

SetPointer(HourGlass!)

// Set database information
dw_report.SetTransObject(SQLCA)
dw_by_payee.SetTransObject(SQLCA)

is_org_select = dw_by_payee.Describe("DataWindow.Table.Select")

// Set the taxation year on the edit mask
ldt_server_datetime = f_server_datetime()
IF Month(Date(ldt_server_datetime)) <= 3 THEN
	em_taxation_year.Text = String(Long(String(ldt_server_datetime, "yyyy"))-1,"####")
ELSE
	em_taxation_year.Text = String(ldt_server_datetime, "yyyy") 
END IF

em_taxation_year.SetFocus()


// Call this event so window opens quicker
PostEvent("ue_postopen")
end event

event activate;call super::activate;Long ll_claim_no
w_sheet lwi_active_sheet

ll_claim_no = 0

IF ClassName(w_frame.GetNextSheet(This)) = 'w_sheet' THEN
	lwi_active_sheet = w_frame.GetNextSheet(This)
END IF

IF IsValid(lwi_active_sheet) THEN
	IF lwi_active_sheet.dw_basic_claim.RowCount() > 0 THEN
		ll_claim_no = lwi_active_sheet.dw_basic_claim.GetItemNumber(1,"claim_no")
	END IF
END IF
IF ll_claim_no > 0 AND ll_claim_no <> Long(sle_claim_no.Text) AND sle_individual.Text = '' THEN
	sle_claim_no.Text = String(ll_claim_no)
	cb_retrieve.PostEvent(Clicked!)
END IF
end event

event ue_print;// Ancestor overridden
IF dw_report.Visible = TRUE THEN
	dw_report.Print(FALSE)
ELSE
	dw_by_payee.Print(FALSE)
END IF
end event

event ue_postopen;call super::ue_postopen;

// resize stuff
il_design_time_width  = 2784
il_design_time_height = 2556

This.wf_SetResize(True)

THIS.inv_resize.of_register(dw_by_payee,'ScaleToRight&Bottom')
THIS.inv_resize.of_register(dw_report,'ScaleToRight&Bottom')
THIS.inv_resize.of_register(cb_by_payee,100,100,0,0)
THIS.inv_resize.of_register(cb_retrieve,100,100,0,0)
THIS.inv_resize.of_register(cb_print_details,100,100,0,0)
THIS.inv_resize.of_register(cb_close,100,100,0,0)

trigger event resize(0,il_design_time_width,il_design_time_height)
end event

event resize;call super::resize;LONG ll_workspacewidth,ll_workspaceheight

IF IsValid(inv_resize) THEN
	// Notify the resize service that the window size has changed.
	ll_workspacewidth  = This.WorkSpaceWidth()
	ll_workspaceheight = This.WorkSpaceHeight()

	IF IsValid (inv_resize) THEN
		inv_resize.Event pfc_Resize (sizetype,ll_workspacewidth  , ll_workspaceheight )
	END IF	
END IF


end event

type dw_report from w_a_report`dw_report within w_t5_recipient_details
integer x = 0
integer y = 164
integer width = 2715
integer height = 2192
integer taborder = 50
string dataobject = "d_t5_recipient_details"
boolean hscrollbar = true
end type

event dw_report::retrievestart;call super::retrievestart;Return 2 // Don't reset the datawindow
end event

type cb_retrieve from commandbutton within w_t5_recipient_details
integer x = 1234
integer y = 2388
integer width = 434
integer height = 108
integer taborder = 80
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Retrieve "
boolean default = true
end type

event clicked;Long 			ll_taxation_year, ll_row, ll_rows, ll_recipient_no, ll_claim_no, ll_new_individ
DateTime 	ldt_server_date, ldt_server_datetime
u_ds		 	lds_participants
String	  	ls_rtn
Boolean		lb_multi

SetPointer(HourGlass!)

ldt_server_datetime = f_server_datetime()
ldt_server_date = Datetime(Date(ldt_server_datetime), Time("00:00:00"))

// Get Taxation Year
ll_taxation_year = Long(em_taxation_year.Text)
ll_rows = 0

SELECT Count(*)
INTO   :ll_rows
FROM   T5007_HISTORY 
WHERE  taxation_year = :ll_taxation_year
USING SQLCA;
SQLCA.nf_handle_error('w_t5_recipient_details','cb_retrieve.clicked','Select from T5007_HISTORY')

IF ll_rows = 0 THEN
	em_taxation_year.SetFocus()
	MessageBox('Error', 'T5 Extract has not yet been run for this taxation year.')
	RETURN -1
END IF
	
IF ll_taxation_year < 1999 OR ll_taxation_year > Year(Date(ldt_server_date)) THEN
	em_taxation_year.SetFocus()
	MessageBox('Error', 'Taxation year does not pass validation.  It must be >= 1999 and <= the current year.')
	RETURN -1
END IF

idtm_start_date = DateTime(Date(String(ll_taxation_year) + '/01/01'))
idtm_end_date = DateTime(Date(String(ll_taxation_year + 1) + '/01/01'))

ll_claim_no = Long(sle_claim_no.Text)
ll_recipient_no = Long(sle_individual.Text)

dw_report.Visible = TRUE
dw_by_payee.Visible = FALSE
cb_by_payee.Text = 'View by Payee'

dw_report.Reset() // Manually reset as we return 2 in retrievestart event.


IF ll_claim_no > 0 THEN
	// Set up report for Claim query
	lds_participants = CREATE u_ds
	lds_participants.DataObject = 'd_claim_participant_maint'
	lds_participants.SetTransObject(SQLCA)
	ll_rows = lds_participants.Retrieve(ll_claim_no)
	SQLCA.nf_handle_error('w_t5_recipient_details','cb_retrieve.clicked','lds_participants.Retrieve')
	
	ls_rtn = dw_report.Modify("claim_parameter.Visible=1")
	IF ls_rtn <> '' THEN 
		MessageBox('Error', 'Cannot modify report parameters. ' + ls_rtn + '.')
	END IF
	ls_rtn = dw_report.Modify("individual_parameter.Visible=0")
	IF ls_rtn <> '' THEN 
		MessageBox('Error', 'Cannot modify report parameters. ' + ls_rtn + '.')
	END IF

	FOR ll_row = 1 TO ll_rows
		ll_recipient_no = lds_participants.GetItemNumber(ll_row, 'individual_no')
		IF dw_report.Retrieve(idtm_start_date, idtm_end_date, ll_recipient_no) < 0 THEN
			MessageBox('Error', 'Error retrieving report information')
			DESTROY lds_participants
			RETURN -1
		END IF
	NEXT
	DESTROY lds_participants
	// If multiple claims then give a messagebox
	ll_rows = dw_report.RowCount()
	FOR ll_row = 2 TO ll_rows
		IF dw_report.GetItemNumber(ll_row, 'claim_no') <> dw_report.GetItemNumber(ll_row - 1, 'claim_no') THEN
			lb_multi = TRUE
		END IF
	NEXT
	IF lb_multi THEN
		MessageBox('Warning', 'There are multiple claims for this individual')
	END IF
	IF ll_rows = 0 THEN
		MessageBox('Warning', 'No data found')
	END IF
ELSEIF ll_recipient_no > 0 THEN
	// Set up report for Individual query
	ls_rtn = dw_report.Modify("claim_parameter.Visible=0")
	IF ls_rtn < '' THEN 
		MessageBox('Error', 'Cannot modify report parameters. ' + ls_rtn + '.')
	END IF
	ls_rtn = dw_report.Modify("individual_parameter.Visible=1")
	IF ls_rtn <> '' THEN 
		MessageBox('Error', 'Cannot modify report parameters. ' + ls_rtn + '.')
	END IF

	IF dw_report.Retrieve(idtm_start_date, idtm_end_date, ll_recipient_no) < 0 THEN
		MessageBox('Error', 'Error retrieving report information')
		Return -1
	END IF
	IF dw_report.RowCount() = 0 THEN // Check for duplicate fix
		ll_new_individ = ll_recipient_no		
		DO // Recursive select to get to end of chain
			SELECT new_individual_no INTO :ll_new_individ
			FROM ARCHIVE_INDIVIDUAL
			WHERE individual_no = :ll_new_individ
			USING SQLCA;
			SQLCA.nf_handle_error('w_t5_recipient_details', 'cb_retrieve', 'select from ARCHIVE_INDIVIDUAL')
			
		LOOP WHILE SQLCA.SQLCode <> 100
		
		IF ll_new_individ > 0 THEN
			IF dw_report.Retrieve(idtm_start_date, idtm_end_date, ll_new_individ) < 0 THEN
				MessageBox('Error', 'Error retrieving report information')
				Return -1
			ELSE
				IF ll_recipient_no <> ll_new_individ THEN
					MessageBox('Information', 'Individual no.: ' + String(ll_recipient_no) + ' has been changed to: ' + String(ll_new_individ))
				ELSE
					MessageBox('Warning', 'No data found')
				END IF
			END IF
		END IF
	END IF
ELSE
	// must enter either claim_no or individual_no
	MessageBox('Warning', 'You must enter either the claim number or the individual number to retrieve this report.',Information!)
END IF


dw_report.Sort()
dw_report.GroupCalc()
cb_print_details.Enabled = TRUE

end event

type st_transmission_date from statictext within w_t5_recipient_details
integer x = 37
integer y = 36
integer width = 453
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
boolean enabled = false
string text = "Taxation Year:"
boolean focusrectangle = false
end type

type cb_print_details from commandbutton within w_t5_recipient_details
integer x = 1691
integer y = 2388
integer width = 434
integer height = 108
integer taborder = 60
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Print Report"
end type

event clicked;Parent.TriggerEvent('ue_print')

end event

type cb_close from commandbutton within w_t5_recipient_details
integer x = 2281
integer y = 2388
integer width = 434
integer height = 108
integer taborder = 70
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Close"
end type

event clicked;Close(Parent)
end event

type cb_search from commandbutton within w_t5_recipient_details
integer x = 2222
integer y = 32
integer width = 91
integer height = 80
integer taborder = 40
boolean bringtotop = true
integer textsize = -12
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "?"
end type

event clicked;S_WINDOW_MESSAGE lstr_message
LONG ll_individual_no

SetPointer(HourGlass!)
lstr_message.awi_parent_window = PARENT
lstr_message.as_stringparm[1] = 'd_basic_claim_search'
OpenWithParm(w_individual_search2, lstr_message)
lstr_message = Message.PowerObjectParm
IF lstr_message.al_doubleparm[1] > 0 THEN
	sle_individual.Text = String(lstr_message.al_doubleparm[1])
	sle_claim_no.Text = ''
END IF
end event

type st_1 from statictext within w_t5_recipient_details
integer x = 1536
integer y = 36
integer width = 375
integer height = 72
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Individual No.:"
boolean focusrectangle = false
end type

type sle_individual from singlelineedit within w_t5_recipient_details
integer x = 1943
integer y = 24
integer width = 270
integer height = 96
integer taborder = 30
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean autohscroll = false
borderstyle borderstyle = stylelowered!
end type

event modified;sle_claim_no.Text = ''
end event

type st_2 from statictext within w_t5_recipient_details
integer x = 850
integer y = 36
integer width = 270
integer height = 72
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Claim No.:"
boolean focusrectangle = false
end type

type sle_claim_no from singlelineedit within w_t5_recipient_details
integer x = 1138
integer y = 24
integer width = 270
integer height = 96
integer taborder = 20
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean autohscroll = false
borderstyle borderstyle = stylelowered!
end type

event modified;sle_individual.Text = ''
end event

type cb_by_payee from commandbutton within w_t5_recipient_details
integer x = 37
integer y = 2388
integer width = 498
integer height = 108
integer taborder = 90
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "View by Payee"
end type

event clicked;LONG ll_row, ll_rows
STRING ls_where, ls_mod_string, ls_rc

ll_rows = dw_report.RowCount()
IF ll_rows = 0 THEN
	Return 0
END IF

IF This.Text = 'View by Payee' THEN
	ls_where = 'AND txn_no IN ('

	FOR ll_row = 1 TO ll_rows	
		ls_where += String(dw_report.GetItemNumber(ll_row, 'txn_no')) + ','
	NEXT
	ls_where = Mid(ls_where, 1, Len(ls_where) - 1) + ')'

	ls_mod_string = 'DataWindow.Table.Select="' + is_org_select + ls_where + '"'

	ls_rc = dw_by_payee.Modify(ls_mod_string)

	IF ls_rc = "" THEN
		dw_by_payee.Retrieve( )
	ELSE
		MessageBox("Status", "Modify Failed" + ls_rc)
	END IF

	This.Text = 'View by Recipient'
	dw_report.Visible = FALSE
	dw_by_payee.Object.st_parameters.text = dw_report.Object.claim_parameter[1]
	dw_by_payee.Visible = TRUE
ELSE
	This.Text = 'View by Payee'
	dw_report.Visible = TRUE
	dw_by_payee.Visible = FALSE
END IF

end event

type dw_by_payee from u_dw_online within w_t5_recipient_details
boolean visible = false
integer y = 164
integer width = 2715
integer height = 2192
integer taborder = 50
boolean bringtotop = true
string dataobject = "d_t5_recipient_details_by_payee"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

type em_taxation_year from editmask within w_t5_recipient_details
integer x = 421
integer y = 20
integer width = 283
integer height = 104
integer taborder = 10
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
string mask = "####"
boolean spin = true
string minmax = "1999~~"
end type

