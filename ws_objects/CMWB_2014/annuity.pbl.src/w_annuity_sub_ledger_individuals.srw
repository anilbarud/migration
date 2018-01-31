$PBExportHeader$w_annuity_sub_ledger_individuals.srw
$PBExportComments$Report - Shows annuiity sub-ledger by claim
forward
global type w_annuity_sub_ledger_individuals from window
end type
type cb_excel from commandbutton within w_annuity_sub_ledger_individuals
end type
type cb_ok from commandbutton within w_annuity_sub_ledger_individuals
end type
type dw_report from u_datawindow within w_annuity_sub_ledger_individuals
end type
type st_1 from statictext within w_annuity_sub_ledger_individuals
end type
type em_claim_no from editmask within w_annuity_sub_ledger_individuals
end type
type cbx_all from checkbox within w_annuity_sub_ledger_individuals
end type
type rb_detail from radiobutton within w_annuity_sub_ledger_individuals
end type
type rb_summary from radiobutton within w_annuity_sub_ledger_individuals
end type
type gb_1 from groupbox within w_annuity_sub_ledger_individuals
end type
type gb_2 from groupbox within w_annuity_sub_ledger_individuals
end type
end forward

global type w_annuity_sub_ledger_individuals from window
integer x = 1893
integer y = 48
integer width = 2766
integer height = 2760
boolean titlebar = true
string title = "Annuity Sub-Ledger Report"
string menuname = "m_cmwb_notools"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 67108864
event ue_print pbm_custom01
event ue_postopen pbm_custom02
cb_excel cb_excel
cb_ok cb_ok
dw_report dw_report
st_1 st_1
em_claim_no em_claim_no
cbx_all cbx_all
rb_detail rb_detail
rb_summary rb_summary
gb_1 gb_1
gb_2 gb_2
end type
global w_annuity_sub_ledger_individuals w_annuity_sub_ledger_individuals

type variables
datawindowchild	iw_region_list
n_dw_resize inv_resize
m_cmwb_notools im_menu
boolean I_Authorized_Access  //flag to check window security
end variables

event ue_print;
dw_report.object.datawindow.print.orientation = 1  //Landscape
dw_report.Print()
end event

event open;W_SHEET    	lw_wsheet
window 			lw_window
LONG				ll_claim_no
STRING			wName

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


//	Set up APPLICATION SECURITY CODE

G_PFSecurity.UOF_Check_Access(This)
This.I_Authorized_Access = True              //declared as an instance variable


// Set up the instance variable for the menu so that we can refer to the frames menu later
im_menu = m_cmwb_notools


IF IsNull(inv_resize) OR NOT IsValid (inv_resize) THEN
	inv_resize = create n_dw_resize
	inv_resize.of_SetOrigSize (2729,2580)
END IF

THIS.inv_resize.of_register(dw_report,0,0,100,100)

/*
default to the selected Claim on open of the report; 
if no claim number previously selected (aka tombstone claim) a claim number can be entered 
OR an option for ALL CLAIMS can be selected
*/

/*	Determine if there is a work sheet opened, grab the claim no
*/
ll_claim_no = 0 

lw_window = w_frame.GetFirstSheet()

IF IsValid(lw_window) THEN
	wName = lw_window.ClassName()
	IF wname = 'w_sheet' THEN
		lw_wsheet = w_frame.GetFirstSheet()
		ll_claim_no = lw_wsheet.dw_basic_claim.GetitemNumber(1,'claim_no')
	END IF
	
	IF ll_claim_no = 0 THEN 
		DO
			lw_wsheet = w_frame.GetNextSheet(lw_wsheet)
			IF IsValid(lw_wsheet) THEN
				wName = lw_wsheet.ClassName()
				IF wname = 'w_sheet' THEN
					ll_claim_no = lw_wsheet.dw_basic_claim.GetitemNumber(1,'claim_no')
					EXIT
				END IF
			END IF	
		LOOP WHILE IsValid(lw_wsheet)
	END IF 
END IF

IF NOT ISNULL(ll_claim_no) THEN em_claim_no.TEXT = STRING(ll_claim_no)

end event

on w_annuity_sub_ledger_individuals.create
if this.MenuName = "m_cmwb_notools" then this.MenuID = create m_cmwb_notools
this.cb_excel=create cb_excel
this.cb_ok=create cb_ok
this.dw_report=create dw_report
this.st_1=create st_1
this.em_claim_no=create em_claim_no
this.cbx_all=create cbx_all
this.rb_detail=create rb_detail
this.rb_summary=create rb_summary
this.gb_1=create gb_1
this.gb_2=create gb_2
this.Control[]={this.cb_excel,&
this.cb_ok,&
this.dw_report,&
this.st_1,&
this.em_claim_no,&
this.cbx_all,&
this.rb_detail,&
this.rb_summary,&
this.gb_1,&
this.gb_2}
end on

on w_annuity_sub_ledger_individuals.destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_excel)
destroy(this.cb_ok)
destroy(this.dw_report)
destroy(this.st_1)
destroy(this.em_claim_no)
destroy(this.cbx_all)
destroy(this.rb_detail)
destroy(this.rb_summary)
destroy(this.gb_1)
destroy(this.gb_2)
end on

event resize;long ll_workspacewidth,ll_workspaceheight


// Notify the resize service that the w_a_report size has changed.
ll_workspacewidth = This.WorkSpaceWidth()
ll_workspaceheight = This.WorkSpaceHeight()

If IsValid (inv_resize) Then
	inv_resize.Event pfc_Resize (sizetype,ll_workspacewidth  , ll_workspaceheight )
End If
end event

type cb_excel from commandbutton within w_annuity_sub_ledger_individuals
integer x = 2235
integer y = 180
integer width = 443
integer height = 96
integer taborder = 50
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Export to Excel"
end type

event clicked;STRING ls_filename, ls_return_filename, ls_record_type, ls_datawindow_object, ls_initial_directory
INT li_rtn
string ls_year_month_day
date ldt_today
U_DS lds_excel

ls_initial_directory = "C:\temp\"

IF NOT DirectoryExists (ls_initial_directory) THEN
	ls_initial_directory = "C:\"
END IF

IF cbx_all.checked THEN
	ls_datawindow_object = 'ds_annuity_sub_ledger_all_accounts_excel'
	ls_filename =  "Annuity_Subledger_Export_Summary_All_Accounts.XLS"
ELSE
	IF rb_summary.checked then
		ls_datawindow_object = 'ds_annuity_sub_ledger_individual_summary_excel'
		ls_filename =  "Annuity_Subledger_Export_Summary.XLS"
	ELSE
		ls_datawindow_object = 'ds_annuity_sub_ledger_individual_details_excel'
		ls_filename =  "Annuity_Subledger_Export_Detail.XLS"
	END IF
END IF

li_rtn =	GetFileSaveName("Save To", ls_filename, ls_return_filename,"XLS","XLS File (*.xls), *.XLS", ls_initial_directory)
	
IF FileExists(ls_return_filename) THEN
	IF MessageBox('Overwrite File?','The file exists. Do you want to overwrite the file?',Question!,YesNo!,2) = 2 THEN
		RETURN
	END IF
END IF

lds_excel = Create U_DS
lds_excel.DataObject = ls_datawindow_object
lds_excel.SetTransObject(SQLCA)
li_rtn = dw_report.ShareData(lds_excel)

li_rtn = lds_excel.SaveAs(ls_return_filename, HTMLTABLE!,true)

IF li_rtn <= 0  THEN
	MESSAGEBOX("Error Saving File", "There was an error saving the file to the specified location." &
	+"~r~nPlease check that the file is not already open and that you have accesss to the selected save-to location.", EXCLAMATION!)
END IF
end event

type cb_ok from commandbutton within w_annuity_sub_ledger_individuals
integer x = 1870
integer y = 180
integer width = 320
integer height = 96
integer taborder = 40
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
boolean default = true
end type

event clicked;/* from DD

By Claim  - Detailed or Summarized (available from a number of report menu items)
The report is to be accessed by annuity account, which is defined as by claim for surviving spouse or by 
individual for injured workers (includes all claims where the individual has the injured worker role).  
The claim number will be entered and if there are both an injured worker and a surviving spouse associated with the claim, 
the user will be prompted to select the appropriate individual (I/W or SS) for the claim before report data is extracted and displayed.

The selection criteria to allow a range of claim numbers is not typcially used and will be removed (typically 
accessed for a single claim which will become an annuity account).  If there is a need to produce the report for ALL Annuity Accounts, 
this feature could be made available via a ‘radio button’ option for ‘ALL Accounts’.

As well, the date range option in the selection criteria will be removed as typcially all annuity-based 
transactions are to be included for the annuity account.

The report contents will be enhanced to include all claims with annuity transactions (sub-ledger information for each claim)
for an injured worker role and only the one (1) claim for a Surviving Spouse. As well, there will be an indication on the 
report if the individual has account(s) in the opposite role or any other claim(s) as a SS role.
 
To simplify Annuity Reports, the current Sub-ledger reports, excluding those specific to Annuity Quarterly Interest postings, 
will be combined into a single report with the option to select details by annuity account or to just produce the 
summary annuity account balance. This selection option (detail vs. summary) will be available as a radio button option 
on the report selection section of the w_a_report. 

Sub ledger – Summarized Transaction 
Report will have minimal changes to ensure continued performance.
 
*/

LONG						ll_claim_no, ll_individual_no, ll_claim_count, ll_ss_count,  ll_ss_count2
s_window_message  	lstr_message


//GRAB THE ENTERED CLAIM NUMBER
ll_claim_no = long(em_claim_no.text)

/*   ********************************* RADIO BUTTON **************************
	).  If there is a need to produce the report for ALL Annuity Accounts, this feature could be made available 
	via a ‘radio button’ option for ‘ALL Accounts’. -- Not sure what this is???
*/
IF cbx_all.checked = TRUE THEN
	IF MessageBox('All Accounts?','The All Accounts report will take a few minutes to run. Do you want to continue?',Question!,YesNo!) = 1 THEN
		//do all the work in here
		dw_report.dataobject = 'd_annuity_sub_ledger_all_accounts'
		
		/* Retrieve the report */ 
		dw_report.settransobject(sqlca)
		IF dw_report.Retrieve() <= 0 THEN
			SQLCA.nf_handle_error('w_annuity_by_claim', 'cb_ok.clicked', 'Retrieve')
			MessageBox("Annuity Sub Ledger","No data found to satisfy request")
		END IF
	END IF	
//	EXIT OUT
	RETURN
ELSE
	IF rb_summary.checked = TRUE THEN
		dw_report.dataobject = 'd_annuity_sub_ledger_individual_summary'
		dw_report.settransobject(sqlca)
	ELSEIF rb_detail.checked = TRUE THEN
		dw_report.dataobject = 'd_annuity_sub_ledger_individual_details'
		dw_report.settransobject(sqlca)
	END IF		
END IF 

//Make sure we have a valid value
IF IsNull(ll_claim_no) OR ll_claim_no <= 0 THEN
	messagebox("Report Criteria", "A valid Claim Number must be entered")
	RETURN
END IF


/* Retrieve the report */ 
dw_report.settransobject(sqlca)
IF dw_report.dataobject = 'd_annuity_sub_ledger_individual_details' OR dw_report.dataobject = 'd_annuity_sub_ledger_individual_summary' THEN
	
	
	Select individual_no
	Into    :ll_individual_no
	From  CLAIM
	Where claim_no = :ll_claim_no
	Using SQLCA;
	SQLCA.nf_handle_error('w_annuity_by_claim', 'cb_ok.clicked', 'embedded SQL: SELECT Count(*) from CLAIM')
	
	Select count(*)
	Into    :ll_claim_count	
	From   CLAIM_PARTICIPANT a
	Join   INDIVIDUAL b on a.individual_no = b.individual_no
	Join   PAYMENT c on a.claim_no = c.claim_no
	Join   APPLIED_CLAIM_TXN d on c.payment_no = d.payment_no
	                          and a.individual_no = d.recipient_no
	Where  a.individual_no = :ll_individual_no
	And    a.claim_role_code = 'C'
	And    c.payment_type_code = '97'
	Using SQLCA;
   SQLCA.nf_handle_error('w_annuity_by_claim', 'cb_ok.clicked', 'embedded SQL: SELECT Count(*) from CLAIM_PARTICIPANT (1)')
	
	Select Count(*)
	Into    :ll_ss_count
	From   CLAIM_PARTICIPANT a
	Join   INDIVIDUAL b on a.individual_no = b.individual_no
	Join   PAYMENT c on a.claim_no = c.claim_no
	Join   APPLIED_CLAIM_TXN d on c.payment_no = d.payment_no
	                          and a.individual_no = d.recipient_no
	Where a.claim_no = :ll_claim_no
	And    a.claim_role_code = 'SS'  
	And    c.payment_type_code = '97'
	Using SQLCA;
   SQLCA.nf_handle_error('w_annuity_by_claim', 'cb_ok.clicked', 'embedded SQL: SELECT Count(*) from CLAIM_PARTICIPANT (2)')

	// This will check for a SS on the claim and that the individual is an SS on other claims.
	Select Count(*)
	Into    :ll_ss_count2
	From   CLAIM_PARTICIPANT a
	Join   INDIVIDUAL b on a.individual_no = b.individual_no
	Join   PAYMENT c on a.claim_no = c.claim_no
	Join   APPLIED_CLAIM_TXN d on c.payment_no = d.payment_no
	                          and a.individual_no = d.recipient_no
	Where   a.individual_no = :ll_individual_no
	And    a.claim_role_code = 'SS'  
	And    c.payment_type_code = '97'
	Using SQLCA;
   SQLCA.nf_handle_error('w_annuity_by_claim', 'cb_ok.clicked', 'embedded SQL: SELECT Count(*) from CLAIM_PARTICIPANT (2)')

		
	IF ll_claim_count > 0 AND ll_ss_count = 0 THEN
		Select individual_no
		Into   :ll_individual_no
		From   CLAIM_PARTICIPANT
		Where  claim_no = :ll_claim_no
		and    claim_role_code = 'C'
		Using SQLCA;
	   SQLCA.nf_handle_error('w_annuity_by_claim', 'cb_ok.clicked', 'embedded SQL: SELECT individual_no from CLAIM_PARTICIPANT (1)')

	ELSEIF ll_claim_count = 0 AND ll_ss_count > 0 THEN
		Select individual_no
		Into   :ll_individual_no
		From   CLAIM_PARTICIPANT
		Where  claim_no = :ll_claim_no
		and    claim_role_code = 'SS'
		Using SQLCA;
	   SQLCA.nf_handle_error('w_annuity_by_claim', 'cb_ok.clicked', 'embedded SQL: SELECT individual_no from CLAIM_PARTICIPANT (2)')

	ELSEIF ll_claim_count > 0 AND ll_ss_count > 0 THEN
		Messagebox('Information','This claim has annuity set-aside transactions for both the injured worker and surviving spouse.'&
										+'~r~nPlease choose which individual you would like the report run for.')
		OpenWithParm(w_select_iw_or_ss, ll_claim_no)
		lstr_message = Message.powerobjectparm
				
		IF lstr_message.as_stringparm[1] = 'cancel' THEN
			RETURN
		ELSE
			ll_individual_no = lstr_message.al_doubleparm[1]
		END IF
		
	ELSEIF ll_claim_count + ll_ss_count + ll_ss_count2 = 0 THEN
		Messagebox('No Set-Asides','There is no annuity set-aside transactions for the individual(s) associated with this claim.',Exclamation!)
		RETURN		
	END IF
	
END IF

IF dw_report.Retrieve(ll_individual_no) <= 0 THEN
	SQLCA.nf_handle_error('w_annuity_by_claim', 'cb_ok.clicked', 'Retrieve')
	MessageBox("Annuity Sub Ledger","No data found to satisfy request")
END IF

end event

type dw_report from u_datawindow within w_annuity_sub_ledger_individuals
integer x = 5
integer y = 360
integer width = 2720
integer height = 2212
integer taborder = 50
string dataobject = "d_annuity_sub_ledger_individual_summary"
boolean hscrollbar = true
boolean vscrollbar = true
end type

event constructor;call super::constructor;THIS.uf_SetSort(True)
end event

event clicked;call super::clicked;THIS.GroupCalc()
end event

type st_1 from statictext within w_annuity_sub_ledger_individuals
integer x = 82
integer y = 148
integer width = 229
integer height = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Claim:"
boolean focusrectangle = false
end type

type em_claim_no from editmask within w_annuity_sub_ledger_individuals
integer x = 279
integer y = 136
integer width = 338
integer height = 88
integer taborder = 10
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string text = "none"
borderstyle borderstyle = stylelowered!
string mask = "#########"
end type

event modified;if  cbx_all.checked  =  true then
	cbx_all.checked = false
end if
end event

event getfocus;if  cbx_all.checked  =  true then
	cbx_all.checked = false
end if
rb_detail.enabled = true
end event

type cbx_all from checkbox within w_annuity_sub_ledger_individuals
integer x = 741
integer y = 144
integer width = 475
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "ALL CLAIMS"
end type

event clicked;rb_summary.checked 	= TRUE
em_claim_no.text 			= ''
rb_detail.enabled = false
end event

type rb_detail from radiobutton within w_annuity_sub_ledger_individuals
integer x = 1349
integer y = 184
integer width = 293
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
string text = "Detail"
end type

event clicked;
dw_report.dataobject = 'd_annuity_sub_ledger_individual_details'
dw_report.settransobject(sqlca)
end event

type rb_summary from radiobutton within w_annuity_sub_ledger_individuals
integer x = 1349
integer y = 112
integer width = 402
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
string text = "Summary"
boolean checked = true
end type

event clicked;
dw_report.dataobject = 'd_annuity_sub_ledger_individual_summary'
dw_report.settransobject(sqlca)
end event

type gb_1 from groupbox within w_annuity_sub_ledger_individuals
integer x = 46
integer y = 40
integer width = 1262
integer height = 244
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Select Criteria"
end type

type gb_2 from groupbox within w_annuity_sub_ledger_individuals
integer x = 1326
integer y = 40
integer width = 503
integer height = 244
integer taborder = 30
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Selection"
end type

