$PBExportHeader$w_din_report.srw
forward
global type w_din_report from w_a_report
end type
type st_1 from statictext within w_din_report
end type
type st_2 from statictext within w_din_report
end type
type cb_individual_no_search from commandbutton within w_din_report
end type
type st_3 from statictext within w_din_report
end type
type em_din_gp_pin from editmask within w_din_report
end type
type em_start_date from editmask within w_din_report
end type
type em_end_date from editmask within w_din_report
end type
type st_4 from statictext within w_din_report
end type
type st_5 from statictext within w_din_report
end type
type cb_retrieve from commandbutton within w_din_report
end type
type cb_din_search from commandbutton within w_din_report
end type
type em_claim_no from editmask within w_din_report
end type
type em_individual_no from editmask within w_din_report
end type
type ddlb_report_type from dropdownlistbox within w_din_report
end type
type st_6 from statictext within w_din_report
end type
type cb_clear from commandbutton within w_din_report
end type
type uo_filter from u_filter_control within w_din_report
end type
type cb_physician_search from commandbutton within w_din_report
end type
type st_7 from statictext within w_din_report
end type
type em_physician_search from editmask within w_din_report
end type
end forward

global type w_din_report from w_a_report
string title = "DIN Report"
st_1 st_1
st_2 st_2
cb_individual_no_search cb_individual_no_search
st_3 st_3
em_din_gp_pin em_din_gp_pin
em_start_date em_start_date
em_end_date em_end_date
st_4 st_4
st_5 st_5
cb_retrieve cb_retrieve
cb_din_search cb_din_search
em_claim_no em_claim_no
em_individual_no em_individual_no
ddlb_report_type ddlb_report_type
st_6 st_6
cb_clear cb_clear
uo_filter uo_filter
cb_physician_search cb_physician_search
st_7 st_7
em_physician_search em_physician_search
end type
global w_din_report w_din_report

type variables
n_resize inv_resize
end variables

on w_din_report.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_2=create st_2
this.cb_individual_no_search=create cb_individual_no_search
this.st_3=create st_3
this.em_din_gp_pin=create em_din_gp_pin
this.em_start_date=create em_start_date
this.em_end_date=create em_end_date
this.st_4=create st_4
this.st_5=create st_5
this.cb_retrieve=create cb_retrieve
this.cb_din_search=create cb_din_search
this.em_claim_no=create em_claim_no
this.em_individual_no=create em_individual_no
this.ddlb_report_type=create ddlb_report_type
this.st_6=create st_6
this.cb_clear=create cb_clear
this.uo_filter=create uo_filter
this.cb_physician_search=create cb_physician_search
this.st_7=create st_7
this.em_physician_search=create em_physician_search
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.cb_individual_no_search
this.Control[iCurrent+4]=this.st_3
this.Control[iCurrent+5]=this.em_din_gp_pin
this.Control[iCurrent+6]=this.em_start_date
this.Control[iCurrent+7]=this.em_end_date
this.Control[iCurrent+8]=this.st_4
this.Control[iCurrent+9]=this.st_5
this.Control[iCurrent+10]=this.cb_retrieve
this.Control[iCurrent+11]=this.cb_din_search
this.Control[iCurrent+12]=this.em_claim_no
this.Control[iCurrent+13]=this.em_individual_no
this.Control[iCurrent+14]=this.ddlb_report_type
this.Control[iCurrent+15]=this.st_6
this.Control[iCurrent+16]=this.cb_clear
this.Control[iCurrent+17]=this.uo_filter
this.Control[iCurrent+18]=this.cb_physician_search
this.Control[iCurrent+19]=this.st_7
this.Control[iCurrent+20]=this.em_physician_search
end on

on w_din_report.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.cb_individual_no_search)
destroy(this.st_3)
destroy(this.em_din_gp_pin)
destroy(this.em_start_date)
destroy(this.em_end_date)
destroy(this.st_4)
destroy(this.st_5)
destroy(this.cb_retrieve)
destroy(this.cb_din_search)
destroy(this.em_claim_no)
destroy(this.em_individual_no)
destroy(this.ddlb_report_type)
destroy(this.st_6)
destroy(this.cb_clear)
destroy(this.uo_filter)
destroy(this.cb_physician_search)
destroy(this.st_7)
destroy(this.em_physician_search)
end on

event open;call super::open;Integer li_rtn

SetPointer(HourGlass!)

li_rtn = dw_report.SetTransObject(SQLCA)

dw_report.uf_setsort(TRUE)
dw_report.uf_setfilter(TRUE)
uo_filter.uf_set_Requestor(dw_report)

dw_report.object.datawindow.hidegrayline = TRUE

if IsNull(inv_resize) Or not IsValid (inv_resize) then
	inv_resize = create n_resize
	inv_resize.of_SetOrigSize (2766,2761)
end if

This.inv_resize.of_register(dw_report,'scaletoright&bottom')
This.inv_resize.of_register(uo_filter,'fixedtoright&bottom')


ddlb_report_type.selectitem(1)
em_physician_search.Enabled = FALSE
cb_physician_search.Enabled = FALSE



end event

event ue_print;dw_report.Print()

end event

event resize;call super::resize;//dw_report.width = This.width - 80

long ll_workspacewidth,ll_workspaceheight


// Notify the resize service that the window size has changed.
ll_workspacewidth = This.WorkSpaceWidth()
ll_workspaceheight = This.WorkSpaceHeight()

If IsValid (inv_resize) Then
	inv_resize.Event pfc_Resize (sizetype,ll_workspacewidth  , ll_workspaceheight )
End If

end event

type dw_report from w_a_report`dw_report within w_din_report
boolean visible = false
integer x = 23
integer y = 348
integer width = 2688
integer height = 2052
integer taborder = 110
string dataobject = "d_din_report_individuals_by_drug_summary"
boolean hscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event dw_report::rbuttondown;M_DW_ONLINE_RMB_POPUP lm_popup

lm_popup = Create m_dw_online_rmb_popup
lm_popup.mf_set_datawindow(This)

lm_popup.m_options.m_sort.Visible = TRUE
lm_popup.m_options.m_filterlist.Visible = FALSE


lm_popup.m_options.PopMenu(w_frame.PointerX( ), w_frame.PointerY( ))

Destroy lm_popup
end event

type st_1 from statictext within w_din_report
integer x = 1600
integer y = 240
integer width = 215
integer height = 76
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Claim #:"
boolean focusrectangle = false
end type

type st_2 from statictext within w_din_report
integer x = 827
integer y = 240
integer width = 320
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
string text = "Individual #:"
boolean focusrectangle = false
end type

type cb_individual_no_search from commandbutton within w_din_report
integer x = 1490
integer y = 232
integer width = 69
integer height = 88
integer taborder = 80
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "?"
end type

event clicked;Long ll_individual_no
s_window_message lstr_message

SetPointer(HourGlass!)

lstr_message.awi_parent_window = Parent
lstr_message.as_stringparm[1] = 'd_basic_claim_search'

OpenWithParm(w_individual_search2, lstr_message)
lstr_message = Message.PowerObjectParm

IF lstr_message.al_doubleparm[1] > 0 THEN
	em_individual_no.Text = String(lstr_message.al_doubleparm[1])
	em_claim_no.Text = ''
END IF

end event

type st_3 from statictext within w_din_report
integer x = 23
integer y = 140
integer width = 133
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
string text = "DIN:"
boolean focusrectangle = false
end type

type em_din_gp_pin from editmask within w_din_report
integer x = 366
integer y = 124
integer width = 357
integer height = 88
integer taborder = 20
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
string mask = "############"
end type

type em_start_date from editmask within w_din_report
integer x = 1143
integer y = 124
integer width = 407
integer height = 88
integer taborder = 50
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string text = "none"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "yyyy-mm-dd"
boolean dropdowncalendar = true
end type

type em_end_date from editmask within w_din_report
integer x = 1659
integer y = 124
integer width = 407
integer height = 88
integer taborder = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "yyyy-mm-dd"
boolean dropdowncalendar = true
end type

type st_4 from statictext within w_din_report
integer x = 878
integer y = 140
integer width = 251
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
string text = "Between:"
boolean focusrectangle = false
end type

type st_5 from statictext within w_din_report
integer x = 1577
integer y = 144
integer width = 73
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
string text = "to:"
boolean focusrectangle = false
end type

type cb_retrieve from commandbutton within w_din_report
integer x = 2203
integer y = 16
integer width = 334
integer height = 96
integer taborder = 90
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Retrieve"
boolean default = true
end type

event clicked;Long     ll_din_gp_pin, ll_num_rows, ll_individual_no, ll_claim_no, ll_temp
Integer  li_rtn
String   ls_report_type, ls_din_gp_pin_flag, ls_claim_no_flag, ls_individual_no_flag, ls_birth_date
String   ls_start_date_flag, ls_end_date_flag, ls_parameters, ls_name, ls_din_gp_pin_desc, ls_sub_heading1
String   ls_physician_id, ls_physician_id_flag, ls_physician_name
Datetime ldt_start_date, ldt_end_date, ldt_birth_date

SetPointer(HourGlass!)
dw_report.Reset()
dw_report.Visible = FALSE


// Get the Report Selected
ls_report_type = ddlb_report_type.Text
IF ls_report_type = "Individuals Receiving a Rx Drug - Summary" THEN
	dw_report.dataobject = 'd_din_report_individuals_by_drug_summary'
	dw_report.inv_sort.of_sort_within_group(FALSE)
ELSEIF ls_report_type = "Individuals Receiving a Rx Drug - Details" THEN
	dw_report.dataobject = 'd_din_report_individuals_by_drug_details'
	dw_report.inv_sort.of_sort_within_group(TRUE)
	dw_report.inv_sort.of_set_group_sort("last_name A, given_names A, individual_no A, claim_no A")
ELSEIF ls_report_type = "Rx Drugs Dispensed to an Individual - Summary" THEN
	dw_report.dataobject = 'd_din_report_drugs_by_individual_summary'
	dw_report.inv_sort.of_sort_within_group(TRUE)
	dw_report.inv_sort.of_set_group_sort("claim_no A")
ELSEIF ls_report_type = "Physicians Prescribing a Rx Drug" THEN
	dw_report.dataobject = 'd_din_report_physcians_by_drug'
	dw_report.inv_sort.of_sort_within_group(TRUE)
	dw_report.inv_sort.of_set_group_sort("physician_name A, physician_id A")
ELSEIF ls_report_type = "Rx Drugs Prescribed by Physcian" THEN
	dw_report.dataobject = 'd_din_report_drug_by_physcians'
	dw_report.inv_sort.of_sort_within_group(TRUE)
	dw_report.inv_sort.of_set_group_sort("physician_name A, physician_id A")
ELSE 	
	dw_report.dataobject = 'd_din_report_drugs_by_individual_details'
	dw_report.inv_sort.of_sort_within_group(TRUE)
	dw_report.inv_sort.of_set_group_sort("claim_no A, din_gp_pin A")
END IF
li_rtn = dw_report.SetTransObject(SQLCA)

// Get Din_Gp_Pin Parameter
ll_din_gp_pin = Long(em_din_gp_pin.Text)
IF ll_din_gp_pin = 0 OR IsNull(ll_din_gp_pin) = TRUE THEN
	ls_din_gp_pin_flag = "N"
ELSE
	ls_din_gp_pin_flag = "Y"

	SELECT din_gp_pin_desc 
	  INTO :ls_din_gp_pin_desc 
	  FROM Din_Gp_Pin 
	 WHERE din_gp_pin = :ll_din_gp_pin ; 

	li_rtn = SQLCA.nf_handle_error("w_din_report", "", "cb_retrieve - SELECT din_dp_pin_desc FROM Din_Gp_Pin")
	
	IF li_rtn = 100 THEN
		Messagebox("Invalid Din_Gp_Pin", "The Din_Gp_Pin Number you entered is invalid.  Please re-enter.")
		em_din_gp_pin.SetFocus()
		RETURN
	END IF
	
	IF ls_report_type = "Physicians Prescribing a Rx Drug" THEN
		ls_sub_heading1 = "DIN: " + String(ll_din_gp_pin) + " Drug Name: " + ls_din_gp_pin_desc + " "
	END IF	
END IF

IF ls_din_gp_pin_flag = "N" AND (ls_report_type = "Individuals Receiving a Rx Drug - Summary" OR ls_report_type = "Individuals Receiving a Rx Drug - Details" OR &
   ls_report_type = "Physicians Prescribing a Rx Drug" ) THEN
	MessageBox("No Din_Gp_Pin Specified", "A Din_Gp_Pin must be specified to run this report.~r~rEnter a Din_Gp_Pin and try again.")
	RETURN
END IF


// Get Physician_ID Parameter
ls_physician_id = em_physician_search.Text
IF ls_physician_id = "" OR IsNull(ls_physician_id) = TRUE THEN
	ls_physician_id_flag = "N"
ELSE
	ls_physician_id_flag = "Y"

	SELECT physician_name 
	  INTO :ls_physician_name
	  FROM Physician  
	 WHERE physician_id = :ls_physician_id ; 

	li_rtn = SQLCA.nf_handle_error("w_din_report", "", "cb_retrieve - SELECT physician_name  FROM Physician")

	IF li_rtn = 100 THEN
		Messagebox("Invalid Physician_Id", "The Physician_Id you entered is invalid.  Please re-enter.")
		em_physician_search.SetFocus()
		RETURN
	END IF
	
	IF ls_report_type = "Rx Drugs Prescribed by Physcian" THEN
		ls_sub_heading1 = "Physician ID: " + ls_physician_id + "~r Physician Name: " + ls_physician_name + " "
		//ls_parameters = ls_parameters+"Physician Id: "+ls_physician_id +" Physician Name: "+ls_physician_name+" "
	END IF	
END IF

IF ls_physician_id_flag = "N" AND ls_report_type = "Rx Drugs Prescribed by Physcian" THEN
	MessageBox("No Physician_Id Specified", "A valid Physician_Id to run this report.~r~rEnter a Physician_Id and try again.")
	em_physician_search.SetFocus()
	RETURN	
END IF

// Get Individual Number Parameter
ll_individual_no = Long(em_individual_no.Text)
IF ll_individual_no = 0 OR IsNull(ll_individual_no) = TRUE OR ls_report_type = "Physicians Prescribing a Rx Drug" THEN
	// Check to see if a claim number was entered to get Individual no from
	ll_claim_no = Long(em_claim_no.Text)
	IF ll_claim_no = 0 OR IsNull(ll_claim_no) = TRUE THEN
		ls_individual_no_flag = "N"
	ELSE
		SELECT C.individual_no, I.given_names + " " + I.last_name, I.birth_date 
		  INTO :ll_individual_no, :ls_name, :ldt_birth_date 
		  FROM CLAIM C, 
		       INDIVIDUAL I 
		 WHERE C.claim_no = :ll_claim_no 
		   AND C.individual_no = I.individual_no ; 

		li_rtn = SQLCA.nf_handle_error("w_din_report", "", "cb_retrieve - SELECT individual_no FROM CLAIM WHERE claim_no = ....")

		IF li_rtn = 100 THEN
			Messagebox("Invalid Claim Number", "The Claim Number you entered is invalid.  Please re-enter.")
			em_claim_no.SetFocus()
			RETURN
		END IF
		
		IF ll_individual_no > 0 THEN
			em_individual_no.Text = String(ll_individual_no)
			ls_individual_no_flag = "Y"		
		ELSE
			ls_individual_no_flag = "N"		
		END IF
	END IF
ELSE
	ls_individual_no_flag = "Y"

	SELECT given_names + " " + last_name, birth_date 
	  INTO :ls_name, :ldt_birth_date 
	  FROM INDIVIDUAL 
	 WHERE individual_no = :ll_individual_no ; 

	li_rtn = SQLCA.nf_handle_error("w_din_report", "", "cb_retrieve - 	SELECT given_names, last_name FROM INDIVIDUAL")

	IF li_rtn = 100 THEN
		Messagebox("Invalid Individual", "The Individual Number you entered is invalid.  Please re-enter.")
		em_individual_no.SetFocus()
		RETURN
	END IF
END IF

IF ls_individual_no_flag = "N" AND (ls_report_type = "Rx Drugs Dispensed to an Individual - Summary" OR ls_report_type = "Rx Drugs Dispensed to an Individual - Details") THEN
	MessageBox("No Individual Specified", "An Individual Number or Claim Number must be specified when running the Rx Drugs Dispensed to an Individual Reports.~r~rEnter an Individual Number and try again.")
	RETURN
END IF

// Start setting up the Parameters of the Report
IF IsNull(ldt_birth_date) = TRUE THEN
	ls_birth_date = ""
ELSE
	ls_birth_date = String(ldt_birth_date, "mmm dd, yyyy")
END IF

IF ls_report_type = "Individuals Receiving a Rx Drug - Summary" OR ls_report_type = "Individuals Receiving a Rx Drug - Details" THEN
	IF ls_individual_no_flag = "Y" THEN
		ls_parameters = ls_parameters + "Individual Number: " + String(ll_individual_no) + " Individual Name: " + ls_name + "  "
	END IF
	ls_sub_heading1 = "DIN: " + String(ll_din_gp_pin) + " Drug Name: " + ls_din_gp_pin_desc + "~r"
ELSEIF ls_report_type = "Rx Drugs Dispensed to an Individual - Summary" OR ls_report_type = "Rx Drugs Dispensed to an Individual - Details" THEN
	IF ls_din_gp_pin_flag = "Y" THEN
		ls_parameters = ls_parameters + "DIN: " + String(ll_din_gp_pin) + " Drug Name: " + ls_din_gp_pin_desc + "  "
	END IF
	ls_sub_heading1 = "for " + ls_name + "    Individual #: " + String(ll_individual_no) + "     Birth Date: " + ls_birth_date
END IF

// Get Claim Number Parameter
ll_claim_no = Long(em_claim_no.Text)
IF ll_claim_no = 0 OR IsNull(ll_claim_no) = TRUE OR ls_report_type = "Physicians Prescribing a Rx Drug" THEN
	ls_claim_no_flag = "N"
ELSE
	ls_claim_no_flag = "Y"
	
	SELECT claim_no  
	  INTO :ll_temp 
	  FROM CLAIM  
	 WHERE claim_no = :ll_claim_no ; 

	li_rtn = SQLCA.nf_handle_error("w_din_report", "", "cb_retrieve - 	SELECT claim_no FROM CLAIM")
	
	IF li_rtn = 100 THEN
		Messagebox("Invalid Claim Number", "The Claim Number you entered is invalid.  Please re-enter.")
		em_claim_no.SetFocus()
		RETURN
	END IF

	ls_parameters = ls_parameters + "Claim Number: " + String(ll_claim_no) + "  "
END IF

// Get Start Date Parameter
IF IsDate(em_start_date.Text) = FALSE THEN
	ls_start_date_flag = "N"
ELSE
	ls_start_date_flag = "Y"
	ldt_start_date = Datetime(Date(em_start_date.Text))

	IF ldt_start_date < DateTime(Date(1900,01,01)) OR ldt_start_date > DateTime(Date(2079,06,06)) THEN
		MessageBox("Invalid Start Date", "Start date must be after Jan 1, 1900 and before Jun 6, 2079.", Exclamation!)
		em_start_date.SetFocus()
		RETURN
	END IF
	
	IF ls_report_type = "Physicians Prescribing a Rx Drug" OR ls_report_type = "Rx Drugs Prescribed by Physcian" THEN
		ls_parameters = "Start Date >= " + em_start_date.Text + "  "
	ELSE	
		ls_parameters = ls_parameters + "Start Date >= " + em_start_date.Text + "  "
	END IF	
END IF

// Get End Date Parameter
IF IsDate(em_end_date.Text) = FALSE THEN
	ls_end_date_flag = "N"
ELSE
	ls_end_date_flag = "Y"
	ldt_end_date = Datetime(Date(em_end_date.Text))

	IF ldt_end_date < DateTime(Date(1900,01,01)) OR ldt_end_date > DateTime(Date(2079,06,06)) THEN
		MessageBox("Invalid End Date", "End date must be after Jan 1, 1900 and before Jun 6, 2079.", Exclamation!)
		em_end_date.SetFocus()
		RETURN
	END IF

	IF ls_report_type = "Physicians Prescribing a Rx Drug" OR ls_report_type = "Rx Drugs Prescribed by Physcian" THEN
		ls_parameters = ls_parameters + "End Date <= " + em_end_date.Text + "  "
	ELSE	
		ls_parameters = ls_parameters + "End Date <= " + em_end_date.Text + "  "
	END IF
END IF

// Make sure start date isn't after end date
IF	ls_start_date_flag = "Y" AND 	ls_end_date_flag = "Y" AND ldt_start_date > ldt_end_date THEN
	MessageBox("Invalid Date Range", "Start Date must be before End Date.  Please re-enter dates.")
	RETURN
END IF

// Make sure a parameter was specified
IF	ls_din_gp_pin_flag = "N" AND ls_claim_no_flag = "N" AND ls_individual_no_flag = "N" AND ls_start_date_flag = "N" AND ls_end_date_flag = "N" AND ls_physician_id_flag = "N" THEN
	MessageBox("No Parameters Specified", "A parameter must be supplied to run the report.  Enter one and try again.")
	RETURN
END IF

// Retrieve the Report
IF ls_report_type = "Physicians Prescribing a Rx Drug" THEN
	ll_num_rows = dw_report.Retrieve(ll_din_gp_pin, ls_start_date_flag, ldt_start_date, ls_end_date_flag, ldt_end_date)
	li_rtn = SQLCA.nf_handle_error("w_din_report", "", "cb_retrieve - dw_report.Retrieve(ls_din_gp_pin_flag, ll_din_gp_pin)")
ELSEIF ls_report_type = "Rx Drugs Prescribed by Physcian" THEN
	ll_num_rows = dw_report.Retrieve(ls_physician_id, ls_start_date_flag, ldt_start_date, ls_end_date_flag, ldt_end_date)
	li_rtn = SQLCA.nf_handle_error("w_din_report", "", "cb_retrieve - dw_report.Retrieve(ls_physician_id)")
ELSE
	ll_num_rows = dw_report.Retrieve(ls_din_gp_pin_flag, ll_din_gp_pin, ls_claim_no_flag, ll_claim_no, &
											ls_individual_no_flag, ll_individual_no, ls_start_date_flag, ldt_start_date, ls_end_date_flag, ldt_end_date)
	li_rtn = SQLCA.nf_handle_error("w_din_report", "", "cb_retrieve - dw_report.Retrieve(ls_din_gp_pin_flag, ll_din_gp_pin, ls_claim_no_flag, ll_claim_no)")
END IF

li_rtn = SQLCA.nf_handle_error("w_din_report", "", "cb_retrieve - dw_report.Retrieve(ls_din_gp_pin_flag, ll_din_gp_pin, ls_claim_no_flag, ll_claim_no)")

IF ll_num_rows = 0 THEN
	MessageBox("No data found", "No data was found with the parameters you specified:~r~r" + ls_parameters + "~r~rChange your parameters and try again.")
	RETURN
END IF

dw_report.Modify("t_report_parameters.text = '" + ls_parameters + "'")
dw_report.Modify("t_sub_heading1.text = '" + ls_sub_heading1 + "'")

dw_report.Visible = TRUE

end event

type cb_din_search from commandbutton within w_din_report
integer x = 741
integer y = 124
integer width = 69
integer height = 88
integer taborder = 30
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "?"
end type

event clicked;Long ll_din_gp_pin

Open(w_din_gp_pin_search) 

ll_din_gp_pin = Message.DoubleParm
IF ll_din_gp_pin > 0 THEN
	em_din_gp_pin.Text = String(ll_din_gp_pin)
END IF

end event

type em_claim_no from editmask within w_din_report
integer x = 1819
integer y = 232
integer width = 325
integer height = 88
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
string mask = "#######"
end type

type em_individual_no from editmask within w_din_report
integer x = 1143
integer y = 232
integer width = 329
integer height = 88
integer taborder = 70
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
string mask = "#######"
end type

type ddlb_report_type from dropdownlistbox within w_din_report
integer x = 366
integer y = 12
integer width = 1527
integer height = 496
integer taborder = 10
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean vscrollbar = true
string item[] = {"Individuals Receiving a Rx Drug - Summary","Individuals Receiving a Rx Drug - Details","Rx Drugs Dispensed to an Individual - Summary","Rx Drugs Dispensed to an Individual - Details","Physicians Prescribing a Rx Drug","Rx Drugs Prescribed by Physcian"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;dw_report.Reset()
dw_report.Visible = FALSE

dw_report.InsertRow(0)

em_din_gp_pin.Enabled = TRUE
cb_din_search.Enabled = TRUE
em_physician_search.Enabled = FALSE
cb_physician_search.Enabled = FALSE

IF ddlb_report_type.text = "Physicians Prescribing a Rx Drug" THEN 
	em_claim_no.Enabled = FALSE
	em_individual_no.Enabled = FALSE
	cb_individual_no_search.Enabled = FALSE
ELSEIF ddlb_report_type.text = "Rx Drugs Prescribed by Physcian" THEN
	em_claim_no.Enabled = FALSE
	em_individual_no.Enabled = FALSE
	cb_individual_no_search.Enabled = FALSE
	em_din_gp_pin.Enabled = FALSE
	cb_din_search.Enabled = FALSE
	em_physician_search.Enabled = TRUE
	cb_physician_search.Enabled = TRUE
ELSE
	em_claim_no.Enabled = TRUE
	em_individual_no.Enabled = TRUE
	cb_individual_no_search.Enabled = TRUE
END IF	

em_claim_no.Text = ""
em_individual_no.Text = ""
em_din_gp_pin.Text = ""
em_start_date.Text = ""
em_end_date.Text = ""
em_physician_search.Text = ""
dw_report.inv_filter.of_setfilter("")
end event

type st_6 from statictext within w_din_report
integer x = 23
integer y = 24
integer width = 338
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
string text = "Report Type:"
boolean focusrectangle = false
end type

type cb_clear from commandbutton within w_din_report
integer x = 2203
integer y = 168
integer width = 334
integer height = 96
integer taborder = 100
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Clear"
end type

event clicked;dw_report.Reset()
//dw_report.InsertRow(0)
dw_report.Visible = FALSE
em_claim_no.Text = ""
em_din_gp_pin.Text = ""
em_end_date.Text = ""
em_individual_no.Text = ""
em_start_date.Text = ""
em_physician_search.Text = ""
dw_report.inv_filter.of_setfilter("")



end event

type uo_filter from u_filter_control within w_din_report
integer x = 119
integer y = 2428
integer taborder = 120
boolean bringtotop = true
end type

on uo_filter.destroy
call u_filter_control::destroy
end on

type cb_physician_search from commandbutton within w_din_report
integer x = 741
integer y = 232
integer width = 69
integer height = 88
integer taborder = 120
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "?"
end type

event clicked;String ls_physician_id

Open(w_physician_search) 

ls_physician_id = Message.StringParm

IF ls_physician_id <> "" THEN
	em_physician_search.Text = String(ls_physician_id)
END IF
end event

type st_7 from statictext within w_din_report
integer x = 18
integer y = 240
integer width = 334
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
string text = "Physician Id:"
boolean focusrectangle = false
end type

type em_physician_search from editmask within w_din_report
integer x = 366
integer y = 232
integer width = 357
integer height = 88
integer taborder = 80
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string mask = "##########"
end type

