$PBExportHeader$w_authorizations.srw
$PBExportComments$Main Window for Authorizations - allows users to accept payments entered by other users for amounts greater than their payment limit.
forward
global type w_authorizations from w_ancestor
end type
type cb_open_report from commandbutton within w_authorizations
end type
type cb_check from commandbutton within w_authorizations
end type
type cb_uncheck from commandbutton within w_authorizations
end type
type st_6 from statictext within w_authorizations
end type
type cb_clear from commandbutton within w_authorizations
end type
type cb_ann_payout_report from commandbutton within w_authorizations
end type
type uo_image_append from u_image_append within w_authorizations
end type
type cb_view from commandbutton within w_authorizations
end type
type st_5 from statictext within w_authorizations
end type
type st_4 from statictext within w_authorizations
end type
type st_2 from statictext within w_authorizations
end type
type em_claim_no from editmask within w_authorizations
end type
type rb_all_awards from radiobutton within w_authorizations
end type
type rb_rtw from radiobutton within w_authorizations
end type
type rb_apen from radiobutton within w_authorizations
end type
type rb_ltd from radiobutton within w_authorizations
end type
type rb_cl from radiobutton within w_authorizations
end type
type rb_ca from radiobutton within w_authorizations
end type
type rb_all_payments from radiobutton within w_authorizations
end type
type rb_ssp from radiobutton within w_authorizations
end type
type rb_ppen from radiobutton within w_authorizations
end type
type rb_act from radiobutton within w_authorizations
end type
type rb_loe from radiobutton within w_authorizations
end type
type cb_close from commandbutton within w_authorizations
end type
type st_3 from statictext within w_authorizations
end type
type ddlb_admin_region from dropdownlistbox within w_authorizations
end type
type dw_authorize_details from u_datawindow within w_authorizations
end type
type dw_docs_for_payment_no from u_dw_online within w_authorizations
end type
type rb_annuities from radiobutton within w_authorizations
end type
type ddlb_payments from dropdownlistbox within w_authorizations
end type
type st_1 from statictext within w_authorizations
end type
type cb_refresh from commandbutton within w_authorizations
end type
type cb_authorize from commandbutton within w_authorizations
end type
type tab_authorizations from tab within w_authorizations
end type
type tabpage_unauthorized from userobject within tab_authorizations
end type
type dw_unauthorized from u_datawindow within tabpage_unauthorized
end type
type tabpage_unauthorized from userobject within tab_authorizations
dw_unauthorized dw_unauthorized
end type
type tabpage_grouped from userobject within tab_authorizations
end type
type dw_unauthorized_grouped_payments from u_datawindow within tabpage_grouped
end type
type tabpage_grouped from userobject within tab_authorizations
dw_unauthorized_grouped_payments dw_unauthorized_grouped_payments
end type
type tabpage_authorized from userobject within tab_authorizations
end type
type dw_authorized from u_datawindow within tabpage_authorized
end type
type tabpage_authorized from userobject within tab_authorizations
dw_authorized dw_authorized
end type
type tab_authorizations from tab within w_authorizations
tabpage_unauthorized tabpage_unauthorized
tabpage_grouped tabpage_grouped
tabpage_authorized tabpage_authorized
end type
type gb_3 from groupbox within w_authorizations
end type
end forward

global type w_authorizations from w_ancestor
integer width = 4827
integer height = 3324
string title = "Payment and Award Authorizations"
string menuname = "m_cmwb_notools"
windowtype windowtype = main!
long backcolor = 67108864
boolean clientedge = true
boolean center = true
long il_design_time_height = 2512
long il_design_time_width = 4718
event ue_print pbm_custom01
cb_open_report cb_open_report
cb_check cb_check
cb_uncheck cb_uncheck
st_6 st_6
cb_clear cb_clear
cb_ann_payout_report cb_ann_payout_report
uo_image_append uo_image_append
cb_view cb_view
st_5 st_5
st_4 st_4
st_2 st_2
em_claim_no em_claim_no
rb_all_awards rb_all_awards
rb_rtw rb_rtw
rb_apen rb_apen
rb_ltd rb_ltd
rb_cl rb_cl
rb_ca rb_ca
rb_all_payments rb_all_payments
rb_ssp rb_ssp
rb_ppen rb_ppen
rb_act rb_act
rb_loe rb_loe
cb_close cb_close
st_3 st_3
ddlb_admin_region ddlb_admin_region
dw_authorize_details dw_authorize_details
dw_docs_for_payment_no dw_docs_for_payment_no
rb_annuities rb_annuities
ddlb_payments ddlb_payments
st_1 st_1
cb_refresh cb_refresh
cb_authorize cb_authorize
tab_authorizations tab_authorizations
gb_3 gb_3
end type
global w_authorizations w_authorizations

type variables
u_dw_document_path iu_dw_document_path
STRING		is_retrieve_type, is_type, is_selected_admin_region, is_all_regions[], is_default_region, is_old_type

u_ds		    ids_admin_region
n_resize  inv_tab_resize

BOOLEAN ib_rowwasset, ib_payout_with_dependants, ib_explodewasgood, ib_fxn_call
LONG il_annuity_payout_no, il_individual_no
INT ii_dependant_txn_count

w_authorizations iw_win
W_SHEET		iw_passedwindow
s_window_message 	istr_window_message

INT ii_frameheight, ii_framewidth, ii_framex, ii_framey, ii_oldsheetwidth, ii_oldsheetx, ii_oldsheetheight, ii_oldsheety

DATAWINDOW idw_dw
end variables

forward prototypes
public function integer wf_load_regions ()
public subroutine wf_select_row (long al_new_row, datawindow adw_requestor)
public function integer wf_retrieve_details (long al_row, datawindow adw_requestor)
public subroutine wf_payout_has_dependants ()
public function integer wf_explode_frame_and_set_sheets ()
public subroutine wf_reset_screen_sizes ()
public subroutine wf_sel_des_all (long il_sel, datawindow adw_dw)
public function integer wf_check_pac (date adt_start, date adt_end, string as_award_type, ref date adt_from_date, ref date adt_to_date)
end prototypes

event ue_print;idw_dw.Print()

end event

public function integer wf_load_regions ();LONG ll_cntr, ll_rowcount, ll_default_region, ll_find
STRING ls_default_region, ls_find

ll_rowcount = ids_admin_region.RowCount()

IF ll_rowcount > 0 THEN
	FOR ll_cntr = 1 to ll_rowcount 
		ddlb_admin_region.AddItem(ids_admin_region.GetItemString(ll_cntr, 'admin_region_desc'))
		is_all_regions[ll_cntr] = ids_admin_region.GetItemString(ll_cntr, 'admin_region_code')
	NEXT
ELSE
	MessageBox('Error Loading Regions','Unable to load the Admin Regions, please contact Help Desk.', Exclamation!)
	RETURN -1
END IF

ddlb_admin_region.InsertItem('ALL REGIONS', ll_rowcount + 1)

ls_find = "admin_region_code = '" + vgst_user_profile.default_admin_region_code + "'"
ll_find = ids_admin_region.Find(ls_find, 1, ll_rowcount + 1)
IF ll_find > 0 THEN
	ls_default_region = ids_admin_region.GetItemString(ll_find, 'admin_region_desc')
ELSE
	ls_default_region = 'ALL REGIONS'
END IF

is_default_region = ls_default_region

ll_default_region = ddlb_admin_region.FindItem(ls_default_region,0)
IF ll_default_region >= 0 THEN
	ddlb_admin_region.SelectItem(ll_default_region)
END IF

is_selected_admin_region = ddlb_admin_region.Text(ll_default_region)
end function

public subroutine wf_select_row (long al_new_row, datawindow adw_requestor);
// Deselect all rows.
adw_requestor.SelectRow ( 0, FALSE ) 

// Select the one row.
adw_requestor.SelectRow(al_new_row, TRUE)

adw_requestor.SetRow ( al_new_row ) 


end subroutine

public function integer wf_retrieve_details (long al_row, datawindow adw_requestor);LONG ll_payment_award_no, ll_rowsindw, ll_annuity_payout_no, ll_claim_no

IF al_row > 0 THEN
	IF is_retrieve_type = 'PAYMENTS' THEN
		ll_payment_award_no = adw_requestor.GetItemNumber(al_row,"payment_no")
		dw_authorize_details.Retrieve(ll_payment_award_no)
		
		/* Retrieve any related documents.
		*/
		ll_rowsindw = dw_docs_for_payment_no.Retrieve(ll_payment_award_no)
	
		/*	If any documents return, give ability to see them.
		*/
		cb_view.Visible = TRUE
		IF ll_rowsindw > 0 THEN
			cb_view.Enabled = TRUE
		ELSE
			cb_view.Enabled = FALSE
		END IF
		
	ELSEIF is_retrieve_type = 'ANNUITIES' THEN
		cb_view.Visible = FALSE
		
		ll_annuity_payout_no = adw_requestor.GetItemNumber(al_row,'annuity_payout_no')
		
		dw_authorize_details.Retrieve(ll_annuity_payout_no)
	ELSE
		ll_claim_no = adw_requestor.GetItemNumber(al_row,"claim_no")
		ll_payment_award_no = adw_requestor.GetItemNumber(al_row,"award_no")

		dw_authorize_details.Retrieve(ll_claim_no,ll_payment_award_no)
		
		cb_view.Visible = TRUE
		cb_view.Enabled = FALSE
	END IF

/*	Check the results of the dw_authorize_details retrieve.
*/
	IF SQLCA.nf_handle_error("dw_authorize_details","w_authorizations","dw_authorize_details.Retrieve()") < 0 THEN
		MessageBox('Error','An error occurred attempting to retrieve authorization details.', Information!)
		RETURN -1
	END IF
END IF

RETURN 0
end function

public subroutine wf_payout_has_dependants ();INTEGER li_count

// check if there are dependants, set instance variable


SELECT COUNT(*)
INTO   :li_count
FROM   ANNUITY_PAYOUT_PARTICIPANT a
JOIN   Annuity_Role               b ON a.annuity_role_code = b.annuity_role_code
WHERE  a.annuity_payout_no        = :il_annuity_payout_no
AND    b.annuity_eligibility_flag = 'N'
AND    b.annuity_entitlement_flag = 'Y'
USING SQLCA;
SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT COUNT(*) FROM ANNUITY_PAYOUT_PARTICIPANT...','wf_payout_has_dependants')
	
IF li_count = 0 THEN
	ib_payout_with_dependants = FALSE
ELSE
	ib_payout_with_dependants = TRUE
	
	SELECT Count(*)
	INTO   :ii_dependant_txn_count
	FROM   ANNUITY_PAYOUT_TXN_DETAIL
	WHERE  recipient_no in ( SELECT a.recipient_no
	                         FROM   ANNUITY_PAYOUT_PARTICIPANT a
	                         JOIN   Annuity_Role               b ON a.annuity_role_code = b.annuity_role_code
	                         WHERE  a.annuity_payout_no        = :il_annuity_payout_no
	                         AND    b.annuity_eligibility_flag = 'N'
	                         AND    b.annuity_entitlement_flag = 'Y' )
	USING SQLCA;
	SQLCA.nf_handle_error('w_prepare_annuity_account','embedded SQL: SELECT Count(*)	FROM ANNUITY_PAYOUT_TXN_DETAIL...','wf_payout_has_dependants')
END IF

end subroutine

public function integer wf_explode_frame_and_set_sheets ();/*	This function is used to resize the frame and the 'Work Sheet'. It also move all
	the controls over to the right side.
*/
	INTEGER			 li_screenwidth, li_screenheight

/*	Turn off the drawing of the application until all is done.
*/
	w_frame.SetRedraw(FALSE)

/*	Grab the position and width of the both the frame and the sheet.
*/

	li_screenheight = 3700
	li_screenwidth = 5000

	ii_framewidth = w_frame.width
	ii_frameheight = w_frame.height
	ii_framex = w_frame.x
	ii_framey = w_frame.y

 	ii_oldsheetx =  iw_passedwindow.X
	ii_oldsheetwidth = iw_passedwindow.Width 
	ii_oldsheety = iw_passedwindow.Y
	ii_oldsheetheight = iw_passedwindow.Height


/*	Make the size of the window equal to the size of the screen.
*/
	w_frame.Height = li_screenheight
	w_frame.Width = li_screenwidth  

	w_frame.SetRedraw(TRUE)
	
	RETURN 0
end function

public subroutine wf_reset_screen_sizes ();COMMANDBUTTON	lcb_currentcontrol
DATAWINDOW		ldw_currentcontrol
USEROBJECT		luo_currentcontrol
INTEGER			li_loopcount, li_numberofcontrols, li_openedsheets
ANY				la_controltype
OBJECT			lo_controltype
WINDOWOBJECT	lwo_controls[]
W_SHEET			lw_activesheet
WINDOW			lw_wsheet
STRING			wName

/*	Set the size and position of the frame back to their originals.
*/
	w_frame.Width = ii_framewidth
	w_frame.Height = ii_frameheight
	w_frame.X = ii_framex
	w_frame.Y = ii_framey

/*	Determine if there is a work sheet opened, and if so then move all the controls over.
*/
	li_openedsheets = 0
	lw_wsheet = w_frame.GetFirstSheet()
	IF IsValid(lw_wsheet) THEN
		wName = lw_wsheet.ClassName()
		IF wname = 'w_sheet' THEN
			li_openedsheets ++
		END IF
		DO
			lw_wsheet = w_frame.GetNextSheet(lw_wsheet)
			IF IsValid(lw_wsheet) THEN
				wName = lw_wsheet.ClassName()
				IF wname = 'w_sheet' THEN
					li_openedsheets ++
				END IF
			END IF	
		LOOP WHILE IsValid(lw_wsheet)
	END IF

	IF li_openedsheets > 0 THEN
		iw_passedwindow.X = ii_oldsheetx
		iw_passedwindow.Width = ii_oldsheetwidth
		iw_passedwindow.Y = ii_oldsheety
		iw_passedwindow.Height = ii_oldsheetheight
	END IF
	
end subroutine

public subroutine wf_sel_des_all (long il_sel, datawindow adw_dw);DATAWINDOW ldw_dw
LONG ll_cntr, ll_rowcount, ll_checked, ll_count_checked
DATE ldt_serverdate
STRING ls_message

ll_rowcount = adw_dw.RowCount()	

IF ll_rowcount > 0 THEN	
	FOR ll_cntr = 1 to ll_rowcount
		adw_dw.SetItem(ll_cntr, 'checkbox_group', il_sel)
	NEXT
END IF


end subroutine

public function integer wf_check_pac (date adt_start, date adt_end, string as_award_type, ref date adt_from_date, ref date adt_to_date);LONG ll_periods, ll_periods_total, ll_periods_processed

SELECT COUNT(*)
INTO    :ll_periods_total
FROM   PERIODIC_AWARD_CONTROL pac
WHERE  pac.period_from_date >= :adt_start 
AND      pac.period_to_date     <= :adt_end   
AND       pac.award_type_code   = :as_award_type
USING    SQLCA;

SQLCA.nf_handle_error('w_authorizations','wf_check_pac()','SELECT COUNT(*) FROM PERIODIC_AWARD_CONTROL')

SELECT COUNT(*), MIN(pac.period_from_date), MAX(pac.period_to_date)
INTO    :ll_periods_processed, :adt_from_date, :adt_to_date
FROM   PERIODIC_AWARD_CONTROL pac
WHERE  pac.period_from_date >= :adt_start 
AND      pac.period_to_date     <= :adt_end   
AND       pac.award_type_code   = :as_award_type
AND       pac.processed_date IS NOT NULL
USING    SQLCA;

SQLCA.nf_handle_error('w_authorizations','wf_check_pac()','SELECT COUNT(*) FROM PERIODIC_AWARD_CONTROL')

ll_periods =  ll_periods_total - ll_periods_processed 
IF ll_periods <> ll_periods_total THEN
	RETURN -1
END IF
	
RETURN 0
end function

event open;call super::open;LONG	ll_result, ll_rtn
INT  li_result
DATAWINDOWCHILD	 dwc_award_types

istr_window_message = Message.PowerObjectParm
iw_passedwindow = istr_window_message.awi_parent_window

ids_admin_region = CREATE u_ds
ids_admin_region.DataObject = 'd_select_admin_region'

is_retrieve_type = 'PAYMENTS'
is_type = ''
rb_all_payments.Checked = TRUE
ib_rowwasset = FALSE


ids_admin_region.SetTransObject(SQLCA)
dw_docs_for_payment_no.SetTransObject(SQLCA)

/*	Retrieve the list of regions.
*/
ll_result = ids_admin_region.Retrieve()
IF SQLCA.nf_handle_error("w_authorizations","open for w_authorizations","ids_admin_region.Retrieve()") < 0 THEN
	RETURN
END IF

ll_rtn = wf_load_regions()

iw_win = THIS

/*	Create an instance of the user object for the view function
*/
iu_dw_document_path = u_dw_document_path
This.OpenUserObject(iu_dw_document_path)
iu_dw_document_path.Hide()

IF IsValid(inv_resize) THEN
ELSE
	inv_resize = CREATE n_resize
END IF

inv_resize.of_SetOrigSize (THIS.width , THIS.height -40 )

inv_resize.of_register(tab_authorizations,'ScaleToRight')
inv_resize.of_register(tab_authorizations.tabpage_unauthorized,'ScaleToRight')
inv_resize.of_register(tab_authorizations.tabpage_grouped,'ScaleToRight')
inv_resize.of_register(tab_authorizations.tabpage_authorized,'ScaleToRight')
inv_resize.of_register(tab_authorizations.tabpage_unauthorized.dw_unauthorized,'ScaleToRight')
inv_resize.of_register(tab_authorizations.tabpage_grouped.dw_unauthorized_grouped_payments,'ScaleToRight')
inv_resize.of_register(tab_authorizations.tabpage_authorized.dw_authorized,'ScaleToRight')
inv_resize.of_register(dw_authorize_details, 'ScaleToRight&Bottom')
inv_resize.of_register(cb_view, 'FixedToBottom')
inv_resize.of_register(cb_ann_payout_report, 'FixedToBottom')
inv_resize.of_register(cb_authorize, 'FixedToBottom')
inv_resize.of_register(cb_close, 'FixedToBottom')

/*	Call function to explode the frame, sheet, and open up a rehab sheet.
*/
ib_explodewasgood = TRUE
li_result = wf_explode_frame_and_set_sheets()
IF li_result < 0 THEN
	ib_explodewasgood = FALSE
	CLOSE(THIS)
	RETURN
END IF

end event

on w_authorizations.create
int iCurrent
call super::create
if IsValid(this.MenuID) then destroy(this.MenuID)
if this.MenuName = "m_cmwb_notools" then this.MenuID = create m_cmwb_notools
this.cb_open_report=create cb_open_report
this.cb_check=create cb_check
this.cb_uncheck=create cb_uncheck
this.st_6=create st_6
this.cb_clear=create cb_clear
this.cb_ann_payout_report=create cb_ann_payout_report
this.uo_image_append=create uo_image_append
this.cb_view=create cb_view
this.st_5=create st_5
this.st_4=create st_4
this.st_2=create st_2
this.em_claim_no=create em_claim_no
this.rb_all_awards=create rb_all_awards
this.rb_rtw=create rb_rtw
this.rb_apen=create rb_apen
this.rb_ltd=create rb_ltd
this.rb_cl=create rb_cl
this.rb_ca=create rb_ca
this.rb_all_payments=create rb_all_payments
this.rb_ssp=create rb_ssp
this.rb_ppen=create rb_ppen
this.rb_act=create rb_act
this.rb_loe=create rb_loe
this.cb_close=create cb_close
this.st_3=create st_3
this.ddlb_admin_region=create ddlb_admin_region
this.dw_authorize_details=create dw_authorize_details
this.dw_docs_for_payment_no=create dw_docs_for_payment_no
this.rb_annuities=create rb_annuities
this.ddlb_payments=create ddlb_payments
this.st_1=create st_1
this.cb_refresh=create cb_refresh
this.cb_authorize=create cb_authorize
this.tab_authorizations=create tab_authorizations
this.gb_3=create gb_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_open_report
this.Control[iCurrent+2]=this.cb_check
this.Control[iCurrent+3]=this.cb_uncheck
this.Control[iCurrent+4]=this.st_6
this.Control[iCurrent+5]=this.cb_clear
this.Control[iCurrent+6]=this.cb_ann_payout_report
this.Control[iCurrent+7]=this.uo_image_append
this.Control[iCurrent+8]=this.cb_view
this.Control[iCurrent+9]=this.st_5
this.Control[iCurrent+10]=this.st_4
this.Control[iCurrent+11]=this.st_2
this.Control[iCurrent+12]=this.em_claim_no
this.Control[iCurrent+13]=this.rb_all_awards
this.Control[iCurrent+14]=this.rb_rtw
this.Control[iCurrent+15]=this.rb_apen
this.Control[iCurrent+16]=this.rb_ltd
this.Control[iCurrent+17]=this.rb_cl
this.Control[iCurrent+18]=this.rb_ca
this.Control[iCurrent+19]=this.rb_all_payments
this.Control[iCurrent+20]=this.rb_ssp
this.Control[iCurrent+21]=this.rb_ppen
this.Control[iCurrent+22]=this.rb_act
this.Control[iCurrent+23]=this.rb_loe
this.Control[iCurrent+24]=this.cb_close
this.Control[iCurrent+25]=this.st_3
this.Control[iCurrent+26]=this.ddlb_admin_region
this.Control[iCurrent+27]=this.dw_authorize_details
this.Control[iCurrent+28]=this.dw_docs_for_payment_no
this.Control[iCurrent+29]=this.rb_annuities
this.Control[iCurrent+30]=this.ddlb_payments
this.Control[iCurrent+31]=this.st_1
this.Control[iCurrent+32]=this.cb_refresh
this.Control[iCurrent+33]=this.cb_authorize
this.Control[iCurrent+34]=this.tab_authorizations
this.Control[iCurrent+35]=this.gb_3
end on

on w_authorizations.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_open_report)
destroy(this.cb_check)
destroy(this.cb_uncheck)
destroy(this.st_6)
destroy(this.cb_clear)
destroy(this.cb_ann_payout_report)
destroy(this.uo_image_append)
destroy(this.cb_view)
destroy(this.st_5)
destroy(this.st_4)
destroy(this.st_2)
destroy(this.em_claim_no)
destroy(this.rb_all_awards)
destroy(this.rb_rtw)
destroy(this.rb_apen)
destroy(this.rb_ltd)
destroy(this.rb_cl)
destroy(this.rb_ca)
destroy(this.rb_all_payments)
destroy(this.rb_ssp)
destroy(this.rb_ppen)
destroy(this.rb_act)
destroy(this.rb_loe)
destroy(this.cb_close)
destroy(this.st_3)
destroy(this.ddlb_admin_region)
destroy(this.dw_authorize_details)
destroy(this.dw_docs_for_payment_no)
destroy(this.rb_annuities)
destroy(this.ddlb_payments)
destroy(this.st_1)
destroy(this.cb_refresh)
destroy(this.cb_authorize)
destroy(this.tab_authorizations)
destroy(this.gb_3)
end on

event close;call super::close;IF ib_explodewasgood 				THEN	wf_reset_screen_sizes()


end event

type cb_open_report from commandbutton within w_authorizations
integer x = 3552
integer y = 468
integer width = 1088
integer height = 104
integer taborder = 30
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "~'Payments Flagged For Grouping~' Report"
end type

event clicked;open(w_ungrouped_payments)
end event

type cb_check from commandbutton within w_authorizations
integer x = 4279
integer y = 1904
integer width = 192
integer height = 84
integer taborder = 30
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Sel. All"
end type

event clicked;DATAWINDOW ldw_dw

IF tab_authorizations.SelectedTab = 1 THEN
	ldw_dw = tab_authorizations.tabpage_unauthorized.dw_unauthorized
ELSEIF tab_authorizations.SelectedTab = 2 THEN
	ldw_dw = tab_authorizations.tabpage_grouped.dw_unauthorized_grouped_payments
ELSE
	ldw_dw = tab_authorizations.tabpage_authorized.dw_authorized
END IF

wf_sel_des_all(1, ldw_dw)
end event

type cb_uncheck from commandbutton within w_authorizations
integer x = 4471
integer y = 1904
integer width = 206
integer height = 84
integer taborder = 20
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Des. All"
end type

event clicked;DATAWINDOW ldw_dw

IF tab_authorizations.SelectedTab = 1 THEN
	ldw_dw = tab_authorizations.tabpage_unauthorized.dw_unauthorized
ELSEIF tab_authorizations.SelectedTab = 2 THEN
	ldw_dw = tab_authorizations.tabpage_grouped.dw_unauthorized_grouped_payments
ELSE
	ldw_dw = tab_authorizations.tabpage_authorized.dw_authorized
END IF

wf_sel_des_all(0, ldw_dw)
end event

type st_6 from statictext within w_authorizations
integer x = 46
integer y = 1912
integer width = 1509
integer height = 84
integer textsize = -12
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 8388608
long backcolor = 67108864
string text = "Transaction Details for Selected Payment:"
boolean focusrectangle = false
end type

type cb_clear from commandbutton within w_authorizations
integer x = 4151
integer y = 332
integer width = 498
integer height = 96
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Clear"
end type

event clicked;LONG ll_default_region

ll_default_region = ddlb_admin_region.FindItem(is_default_region,0)
IF ll_default_region >= 0 THEN
	ddlb_admin_region.SelectItem(ll_default_region)
END IF

is_selected_admin_region = ddlb_admin_region.Text(ll_default_region)

rb_all_payments.Checked =TRUE
rb_all_payments.Checked = FALSE
em_claim_no.Text = ''
is_retrieve_type = ''
is_type = ''

tab_authorizations.tabpage_unauthorized.dw_unauthorized.Reset()
tab_authorizations.tabpage_grouped.dw_unauthorized_grouped_payments.Reset()
tab_authorizations.tabpage_authorized.dw_authorized.Reset()
dw_authorize_details.Reset()
end event

type cb_ann_payout_report from commandbutton within w_authorizations
integer x = 1824
integer y = 2984
integer width = 626
integer height = 96
integer taborder = 30
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Annuity Payout Report"
end type

event clicked;S_WINDOW_MESSAGE          lstr_message

lstr_message.al_doubleparm[1] = 0                     //claim_no
lstr_message.al_doubleparm[2] = il_individual_no
lstr_message.al_doubleparm[3] = il_annuity_payout_no

//call funtion to set up instance variables ib_payout_with_dependants, and ii_dependant_txn_count 
wf_payout_has_dependants()

//set a value in the stingparm, so report window knows which datawindow to use for claim txn details
IF ib_payout_with_dependants AND ii_dependant_txn_count > 0 THEN
	lstr_message.as_stringparm[1] = 'has dependants'
ELSE
	lstr_message.as_stringparm[1] = 'no dependants'
END IF

OpenWithParm(w_annuity_payout_report_viewer, lstr_message, iw_win)
end event

type uo_image_append from u_image_append within w_authorizations
boolean visible = false
integer x = 3346
integer y = 2280
integer width = 1385
integer height = 276
integer taborder = 140
boolean bringtotop = true
end type

on uo_image_append.destroy
call u_image_append::destroy
end on

type cb_view from commandbutton within w_authorizations
integer x = 1408
integer y = 2984
integer width = 411
integer height = 96
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "View Account"
end type

event clicked;LONG	ll_rowcount, ll_cntr, ll_docid, ll_doc_id
string ls_doc_type
integer li_rtn

ll_cntr = 1
ll_rowcount = dw_docs_for_payment_no.RowCount()
DO WHILE ll_cntr <= ll_rowcount			
			
		ll_doc_id =dw_docs_for_payment_no.GetItemNumber(ll_cntr,"doc_id")	
			
		IF uo_image_append.of_init(ll_doc_id)	<= 0 THEN
			RETURN
		END IF			
			
		ls_doc_type =  uo_image_append.of_get_file_type()
					
		CHOOSE CASE ls_doc_type
			/*  Imaged document */ 
			CASE 'IMA', 'TIF'
				li_rtn = uo_image_append.of_append_image(ll_doc_id)
				IF li_rtn < 0 THEN
					RETURN
				END IF
			CASE ELSE
				iu_dw_document_path.f_manage_document(ll_doc_id,"V","NORMAL")
		END CHOOSE
		
	ll_cntr ++
LOOP
end event

type st_5 from statictext within w_authorizations
integer x = 3419
integer y = 220
integer width = 489
integer height = 60
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 8388608
long backcolor = 67108864
string text = "Annuity Payout"
boolean focusrectangle = false
end type

type st_4 from statictext within w_authorizations
integer x = 1993
integer y = 220
integer width = 489
integer height = 60
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 8388608
long backcolor = 67108864
string text = "Periodic Awards"
boolean focusrectangle = false
end type

type st_2 from statictext within w_authorizations
integer x = 178
integer y = 220
integer width = 402
integer height = 60
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 8388608
long backcolor = 67108864
string text = "Payments"
boolean focusrectangle = false
end type

type em_claim_no from editmask within w_authorizations
integer x = 1952
integer y = 96
integer width = 462
integer height = 80
integer taborder = 30
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = right!
borderstyle borderstyle = stylelowered!
string mask = "#########"
end type

type rb_all_awards from radiobutton within w_authorizations
integer x = 2720
integer y = 440
integer width = 402
integer height = 76
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "ALL Awards"
end type

event clicked;is_retrieve_type = 'AWARDS'
is_type = ''

tab_authorizations.tabpage_authorized.dw_authorized.Reset()
tab_authorizations.tabpage_grouped.dw_unauthorized_grouped_payments.Reset()
tab_authorizations.tabpage_unauthorized.dw_unauthorized.Reset()
dw_authorize_details.Reset()

end event

type rb_rtw from radiobutton within w_authorizations
integer x = 2720
integer y = 364
integer width = 453
integer height = 76
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "RTW Incentive"
end type

event clicked;is_retrieve_type = 'AWARDS'
is_type = 'RWI'

tab_authorizations.tabpage_authorized.dw_authorized.Reset()
tab_authorizations.tabpage_grouped.dw_unauthorized_grouped_payments.Reset()
tab_authorizations.tabpage_unauthorized.dw_unauthorized.Reset()
dw_authorize_details.Reset()

end event

type rb_apen from radiobutton within w_authorizations
integer x = 2720
integer y = 288
integer width = 402
integer height = 76
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Pension"
end type

event clicked;is_retrieve_type = 'AWARDS'
is_type = 'PEN'

tab_authorizations.tabpage_authorized.dw_authorized.Reset()
tab_authorizations.tabpage_grouped.dw_unauthorized_grouped_payments.Reset()
tab_authorizations.tabpage_unauthorized.dw_unauthorized.Reset()
dw_authorize_details.Reset()

end event

type rb_ltd from radiobutton within w_authorizations
integer x = 2011
integer y = 440
integer width = 608
integer height = 76
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Long Term Disability"
end type

event clicked;is_retrieve_type = 'AWARDS'
is_type = 'LTD'

tab_authorizations.tabpage_authorized.dw_authorized.Reset()
tab_authorizations.tabpage_grouped.dw_unauthorized_grouped_payments.Reset()
tab_authorizations.tabpage_unauthorized.dw_unauthorized.Reset()
dw_authorize_details.Reset()

end event

type rb_cl from radiobutton within w_authorizations
integer x = 2011
integer y = 364
integer width = 576
integer height = 76
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Clothing Allowance"
end type

event clicked;is_retrieve_type = 'AWARDS'
is_type = 'CL'

tab_authorizations.tabpage_authorized.dw_authorized.Reset()
tab_authorizations.tabpage_grouped.dw_unauthorized_grouped_payments.Reset()
tab_authorizations.tabpage_unauthorized.dw_unauthorized.Reset()
dw_authorize_details.Reset()

end event

type rb_ca from radiobutton within w_authorizations
integer x = 2011
integer y = 288
integer width = 489
integer height = 76
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Care Allowance"
end type

event clicked;is_retrieve_type = 'AWARDS'
is_type = 'CA'

tab_authorizations.tabpage_authorized.dw_authorized.Reset()
tab_authorizations.tabpage_grouped.dw_unauthorized_grouped_payments.Reset()
tab_authorizations.tabpage_unauthorized.dw_unauthorized.Reset()
dw_authorize_details.Reset()

end event

type rb_all_payments from radiobutton within w_authorizations
integer x = 1157
integer y = 364
integer width = 457
integer height = 76
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "ALL Payments"
end type

event clicked;is_retrieve_type = 'PAYMENTS'
is_type = ''

tab_authorizations.tabpage_authorized.dw_authorized.Reset()
tab_authorizations.tabpage_grouped.dw_unauthorized_grouped_payments.Reset()
tab_authorizations.tabpage_unauthorized.dw_unauthorized.Reset()
dw_authorize_details.Reset()


end event

type rb_ssp from radiobutton within w_authorizations
integer x = 1157
integer y = 288
integer width = 549
integer height = 76
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Special Payments"
end type

event clicked;is_retrieve_type = 'PAYMENTS'
is_type = 'spp'

tab_authorizations.tabpage_authorized.dw_authorized.Reset()
tab_authorizations.tabpage_grouped.dw_unauthorized_grouped_payments.Reset()
tab_authorizations.tabpage_unauthorized.dw_unauthorized.Reset()
dw_authorize_details.Reset()

end event

type rb_ppen from radiobutton within w_authorizations
integer x = 197
integer y = 440
integer width = 713
integer height = 76
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Pension / TSB Payments"
end type

event clicked;is_retrieve_type = 'PAYMENTS'
is_type = 'pen'

tab_authorizations.tabpage_authorized.dw_authorized.Reset()
tab_authorizations.tabpage_grouped.dw_unauthorized_grouped_payments.Reset()
tab_authorizations.tabpage_unauthorized.dw_unauthorized.Reset()
dw_authorize_details.Reset()

end event

type rb_act from radiobutton within w_authorizations
integer x = 197
integer y = 364
integer width = 571
integer height = 76
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Account Payments"
end type

event clicked;is_retrieve_type = 'PAYMENTS'
is_type = 'act'

tab_authorizations.tabpage_authorized.dw_authorized.Reset()
tab_authorizations.tabpage_grouped.dw_unauthorized_grouped_payments.Reset()
tab_authorizations.tabpage_unauthorized.dw_unauthorized.Reset()
dw_authorize_details.Reset()

end event

type rb_loe from radiobutton within w_authorizations
integer x = 197
integer y = 288
integer width = 859
integer height = 76
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Loss of Earnings / Allowances"
end type

event clicked;is_retrieve_type = 'PAYMENTS'
is_type = 'loe'

tab_authorizations.tabpage_authorized.dw_authorized.Reset()
tab_authorizations.tabpage_grouped.dw_unauthorized_grouped_payments.Reset()
tab_authorizations.tabpage_unauthorized.dw_unauthorized.Reset()
dw_authorize_details.Reset()

end event

type cb_close from commandbutton within w_authorizations
integer x = 2834
integer y = 2984
integer width = 357
integer height = 96
integer taborder = 30
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Close"
end type

event clicked;CLOSE(PARENT)
end event

type st_3 from statictext within w_authorizations
integer x = 1591
integer y = 104
integer width = 302
integer height = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Claim No:"
boolean focusrectangle = false
end type

type ddlb_admin_region from dropdownlistbox within w_authorizations
integer x = 617
integer y = 88
integer width = 855
integer height = 540
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean vscrollbar = true
end type

event selectionchanged;is_selected_admin_region = ddlb_admin_region.Text(index)
end event

type dw_authorize_details from u_datawindow within w_authorizations
integer x = 59
integer y = 2004
integer width = 4640
integer height = 952
integer taborder = 20
string dataobject = "d_authorize_details"
boolean vscrollbar = true
end type

type dw_docs_for_payment_no from u_dw_online within w_authorizations
boolean visible = false
integer x = 50
integer y = 16
integer width = 645
integer height = 220
integer taborder = 10
string dataobject = "d_docs_for_payment_no"
boolean vscrollbar = true
borderstyle borderstyle = styleraised!
end type

on rowfocuschanged;call u_dw_online::rowfocuschanged;LONG	ll_rownum
ll_rownum = This.GetRow()
IF ll_rownum > 0 THEN
	This.uf_processselect(ll_rownum,"Mouse")
END IF
end on

type rb_annuities from radiobutton within w_authorizations
integer x = 3442
integer y = 288
integer width = 485
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
string text = "Annuity Payout"
end type

event clicked;is_retrieve_type = 'ANNUITIES'
is_type = 'ann'

tab_authorizations.tabpage_authorized.dw_authorized.Reset()
tab_authorizations.tabpage_grouped.dw_unauthorized_grouped_payments.Reset()
tab_authorizations.tabpage_unauthorized.dw_unauthorized.Reset()
dw_authorize_details.Reset()




end event

type ddlb_payments from dropdownlistbox within w_authorizations
boolean visible = false
integer x = 4224
integer y = 1508
integer width = 855
integer height = 452
integer taborder = 10
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
boolean vscrollbar = true
string item[] = {"Loss of Earnings / Allowances","Account Payments","Pension / TSB Payments","Special Payments","ALL Payments"}
end type

type st_1 from statictext within w_authorizations
integer x = 178
integer y = 100
integer width = 384
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
string text = "Admin Region:"
boolean focusrectangle = false
end type

type cb_refresh from commandbutton within w_authorizations
integer x = 4151
integer y = 232
integer width = 498
integer height = 96
integer taborder = 10
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Search"
boolean default = true
end type

event clicked;STRING	ls_admin_region_code[], ls_get_admin_region, ls_screen_type, ls_message_value, ls_payment_award_type, ls_type
DECIMAL	ldec_authorization_limit
LONG		ll_rowsindw, ll_current_row, ll_counter, ll_claim_no, ll_find, ll_rowsindw2, ll_rowsindw3, ll_return
STRING  ls_ignore_pa='N', ls_ignore_claim='N'

SetPointer(HourGlass!)

ls_get_admin_region = is_selected_admin_region

IF Trim(ls_get_admin_region) = "" THEN
	MessageBox("Authorization Module","There must be a region selected",Exclamation!)
	RETURN
END IF

IF ls_get_admin_region = 'ALL REGIONS' THEN
	ls_admin_region_code[] =is_all_regions
ELSE
	ll_find = ids_admin_region.Find("admin_region_desc='" + ls_get_admin_region + "'", 1, ids_admin_region.Rowcount())
	IF ll_find > 0 THEN
		ls_admin_region_code[1] = ids_admin_region.GetItemString(ll_find, 'admin_region_code')
	END IF
END IF

/* Get Claim # if entered
*/
ll_claim_no = LONG(em_claim_no.Text)
IF IsNull(ll_claim_no) THEN
	ll_claim_no = 0
END IF

IF ll_claim_no = 0 THEN 	ls_ignore_claim = 'Y'

/*
*/
CHOOSE CASE is_retrieve_type
	CASE 'PAYMENTS'
		tab_authorizations.tabpage_unauthorized.dw_unauthorized.DataObject = 'd_payments_to_authorize'
		tab_authorizations.tabpage_grouped.dw_unauthorized_grouped_payments.DataObject = 'd_payments_to_authorize_group'
		tab_authorizations.tabpage_authorized.dw_authorized.DataObject = 'd_payments_to_unauthorize'
		dw_authorize_details.DataObject = 'd_authorize_details'
		IF is_type = 'act' THEN
			tab_authorizations.tabpage_grouped.Visible = FALSE
		ELSE
			tab_authorizations.tabpage_grouped.Visible = TRUE
		END IF
	CASE 'AWARDS'
		tab_authorizations.tabpage_unauthorized.dw_unauthorized.DataObject = 'd_awards_to_authorize'
		tab_authorizations.tabpage_authorized.dw_authorized.DataObject = 'd_awards_to_unauthorize'
		dw_authorize_details.DataObject = 'd_authorize_awards_details'
		tab_authorizations.tabpage_grouped.Visible = FALSE
	CASE 'ANNUITIES'
		tab_authorizations.tabpage_unauthorized.dw_unauthorized.DataObject = 'd_annuities_to_authorize' 
		tab_authorizations.tabpage_authorized.dw_authorized.DataObject = 'd_annuities_to_unauthorize' 
		dw_authorize_details.DataObject = 'd_authorize_annuities_details'
		tab_authorizations.tabpage_grouped.Visible = FALSE
	CASE ELSE
		Messagebox('Retrieval Type','You must select a valid type of Payment to Authorize.', Exclamation!)
		RETURN
END CHOOSE

tab_authorizations.tabpage_unauthorized.dw_unauthorized.SetTransObject(SQLCA)
tab_authorizations.tabpage_grouped.dw_unauthorized_grouped_payments.SetTransObject(SQLCA)
tab_authorizations.tabpage_authorized.dw_authorized.SetTransObject(SQLCA)
dw_authorize_details.SetTransObject(SQLCA)
	
/* Check to see if a type is selected. And if so, get its value.
*/
IF Trim(is_type) = "" THEN
	IF NOT(rb_all_payments.Checked OR rb_all_awards.Checked OR rb_annuities.Checked) THEN
		MessageBox("Authorization Module","There must be a payment, award or annuity type selected",Exclamation!)
		RETURN
	ELSE
		ls_ignore_pa = 'Y'
	END IF
END IF

tab_authorizations.SelectTab(1)

ll_rowsindw2 = tab_authorizations.tabpage_authorized.dw_authorized.Retrieve(vgst_user_profile.user_id,ls_admin_region_code[],is_type, ll_claim_no, ls_ignore_pa, ls_ignore_claim)

IF SQLCA.nf_handle_error("dw_authorized","w_authorizations","clicked for cb_refresh") < 0 THEN
	RETURN
END IF

ll_rowsindw = tab_authorizations.tabpage_unauthorized.dw_unauthorized.Retrieve(vgst_user_profile.user_id,ls_admin_region_code[],is_type, ll_claim_no, ls_ignore_pa, ls_ignore_claim)

IF SQLCA.nf_handle_error("dw_unauthorized","w_authorizations","clicked for cb_refresh") < 0 THEN
	RETURN
END IF

IF is_retrieve_type = 'PAYMENTS' THEN
	ll_rowsindw3 = tab_authorizations.tabpage_grouped.dw_unauthorized_grouped_payments.Retrieve(vgst_user_profile.user_id,ls_admin_region_code[],is_type, ll_claim_no, ls_ignore_pa, ls_ignore_claim)
	
	IF SQLCA.nf_handle_error("dw_unauthorized","w_authorizations","clicked for cb_refresh") < 0 THEN
		RETURN
	END IF
END IF

SetPointer(Arrow!)

IF ll_rowsindw = 0 AND ll_rowsindw2 = 0 AND ll_rowsindw3 = 0 THEN
	dw_authorize_details.Reset()
	IF ib_fxn_call = FALSE THEN
		MessageBox("Results","No records were found for the criteria selected. ~r~n ~r~nPlease verify you have the proper Authorizations for this region, or select a different type within the Payments, Periodic Awards and Annuity Payout options.", Information!)
	END IF
END IF

IF ll_rowsindw = 0 AND ll_rowsindw3 > 0 THEN
	tab_authorizations.SelectTab(2)
	tab_authorizations.tabpage_grouped.dw_unauthorized_grouped_payments.SetRow(1)
	ll_return = wf_retrieve_details(1, tab_authorizations.tabpage_grouped.dw_unauthorized_grouped_payments)
ELSEIF ll_rowsindw = 0 AND ll_rowsindw3 = 0 AND ll_rowsindw2 > 0 THEN
	tab_authorizations.SelectTab(3)
	tab_authorizations.tabpage_authorized.dw_authorized.SetRow(1)
	ll_return = wf_retrieve_details(1, tab_authorizations.tabpage_authorized.dw_authorized)
ELSEIF ll_rowsindw > 0 THEN
	tab_authorizations.tabpage_unauthorized.dw_unauthorized.SetRow(1)
	ll_return = wf_retrieve_details(1, tab_authorizations.tabpage_unauthorized.dw_unauthorized)
END IF
IF ll_return < 0 THEN
	MessageBox('Error Retrieving','There was a problem retrieving the payment detail records', Information!)
	RETURN
END IF
ib_fxn_call = FALSE


end event

type cb_authorize from commandbutton within w_authorizations
integer x = 2455
integer y = 2984
integer width = 370
integer height = 96
integer taborder = 30
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Authorize"
end type

event clicked;LONG	ll_rowcount, ll_checked, ll_cntr, ll_result, ll_count_checked, ll_rtn, ll_award_no, ll_claim_no
DATETIME	ldt_serverdate, ldt_nulldate
DATE ldt_start, ldt_end, ldt_from_date, ldt_to_date
STRING ls_message, ls_pmt_msg, ls_award_type
DATAWINDOW ldw_dw

/*	Authorize all highlighted records by setting the authorized_date and authorized_by_user_id.
*/

IF tab_authorizations.SelectedTab = 1 THEN
	ll_rowcount = tab_authorizations.tabpage_unauthorized.dw_unauthorized.RowCount()
	IF ll_rowcount = 0 THEN
		MessageBox('No Records','Please retrieve unauthorized payment records to Authorize', Information!)
		RETURN
	END IF
	ldw_dw = tab_authorizations.tabpage_unauthorized.dw_unauthorized
ELSEIF tab_authorizations.SelectedTab = 2 THEN
	ll_rowcount = tab_authorizations.tabpage_grouped.dw_unauthorized_grouped_payments.RowCount()
	IF ll_rowcount = 0 THEN
		MessageBox('No Records','Please retrieve unauthorized grouped payment records to Authorize', Information!)
		RETURN
	END IF
	ldw_dw = tab_authorizations.tabpage_grouped.dw_unauthorized_grouped_payments
ELSE
	ll_rowcount =  tab_authorizations.tabpage_authorized.dw_authorized.RowCount()
		IF ll_rowcount = 0 THEN
		MessageBox('No Records','Please retrieve authorized payment records to Unauthorize', Information!)
		RETURN
	END IF
	ldw_dw = tab_authorizations.tabpage_authorized.dw_authorized
END IF

SQLCA.nf_begin_transaction()

IF THIS.Text = 'Authorize' THEN
	SetPointer(HourGlass!)
	ldt_serverdate = f_server_datetime()
		
	FOR ll_cntr = 1 to ll_rowcount
		ll_checked = ldw_dw.GetItemNumber(ll_cntr, 'checkbox_group')
		IF ll_checked > 0 THEN
			//call function to see if the processing period for this payment has passed, for awards only
			IF is_retrieve_type = 'AWARDS' THEN
				ldt_start = DATE(ldw_dw.GetItemDateTime(ll_cntr, 'award_start_date'))
				ldt_end  = DATE(ldw_dw.GetItemDateTime(ll_cntr, 'award_end_date'))
				ls_award_type = ldw_dw.GetItemString(ll_cntr,'award_type_code')
				ll_rtn = wf_check_pac(ldt_start, ldt_end, ls_award_type, ldt_from_date, ldt_to_date)
				IF ll_rtn < 0 THEN
					ll_award_no = ldw_dw.GetItemNumber(ll_cntr,'award_no')
					ll_claim_no = ldw_dw.GetItemNumber(ll_cntr, 'claim_no')
					
					ls_pmt_msg = ls_pmt_msg + '~r~n' + 'Claim # ' + STRING(ll_claim_no) + ', Award # ' + STRING(ll_award_no) + ', Period: ' + STRING(ldt_from_date, 'YYYY-MM-DD') + ' to ' + STRING(ldt_to_date, 'YYYY-MM-DD')
				ELSEIF ll_rtn > 0 THEN
					
				END IF				
			END IF
			
			ldw_dw.SetItem(ll_cntr,"authorized_date",ldt_serverdate)
			//NOTE: using column number here as the column names in all dws are different, but all are column # 13. DO NOT CHANGE ORDER
			ldw_dw.SetItem(ll_cntr,13,vgst_user_profile.user_id)
			ll_count_checked = ll_count_checked + 1
		END IF
	NEXT
	IF TRIM(ls_pmt_msg) > '' THEN
		IF MessageBox('Award Period Passed','The Processing Period for the following Award(s) has passed and a payment will not be produced for the period(s) listed.  '    &
				+ 'However, a payment will be produced for any future unprocessed Award periods.~r~n' +   ls_pmt_msg &
				+ ' ~r~n ~r~nPlease contact the user that created the Award to inform them a manual payment must be created.'  &
				+  ' ~r~n~r~nIf you continue, the Awards listed will be removed from the list.  Continue?', Question!, OkCancel!) = 2 THEN 

			SQLCA.nf_rollback_transaction()
			cb_refresh.TriggerEvent(Clicked!)
			RETURN
		END IF
	END IF
	IF ll_count_checked = 0 THEN
		MessageBox('','You must have at least one record checked before you are able to Authorize.', Information!)
		SQLCA.nf_rollback_transaction()
		RETURN
	END IF
	ls_message = 'Clicked event for cb_authorize - tab_authorizations.tabpage_unauthorized.dw_unauthorized.Update()'
	ldw_dw.Update()
	SetPointer(Arrow!)

ELSEIF THIS.Text = 'Unauthorize' THEN

/*	Unauthorize all highlighted records by setting the authorized_date to NULL and authorized_by_user_id to ''.
*/

	SetPointer(HourGlass!)
	SetNull(ldt_nulldate)

	FOR ll_cntr = 1 to ll_rowcount
	ll_checked = tab_authorizations.tabpage_authorized.dw_authorized.GetItemNumber(ll_cntr, 'checkbox_group')
		IF ll_checked > 0 THEN
			tab_authorizations.tabpage_authorized.dw_authorized.SetItem(ll_cntr,"authorized_date",ldt_nulldate)
			//using column number here as the column names in all dws are different, but all are column # 13. do not change order
			tab_authorizations.tabpage_authorized.dw_authorized.SetItem(ll_cntr,13,'')
			ll_count_checked = ll_count_checked + 1
		END IF
	NEXT
	
	IF ll_count_checked = 0 THEN
		MessageBox('','You have at least one record checked before you are able to Unauthorize.', Information!)
		SQLCA.nf_rollback_transaction()
		RETURN
	END IF
	ls_message = 'Clicked event for cb_authorize - tab_authorizations.tabpage_unauthorized.dw_unauthorized.Update()'
	tab_authorizations.tabpage_authorized.dw_authorized.Update()
	SetPointer(Arrow!)

END IF

ll_result = SQLCA.nf_handle_error("tab_authorizations","w_authorizations",ls_message)
IF ll_result < 0 THEN
	SQLCA.nf_rollback_transaction()
	Close(Parent)
	Return 
END IF

SQLCA.nf_commit_transaction()
ib_fxn_call = TRUE
cb_refresh.TriggerEvent(Clicked!)


end event

type tab_authorizations from tab within w_authorizations
event create ( )
event destroy ( )
event ue_resize pbm_size
integer x = 59
integer y = 664
integer width = 4654
integer height = 1240
integer taborder = 10
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
boolean raggedright = true
boolean focusonbuttondown = true
integer selectedtab = 1
tabpage_unauthorized tabpage_unauthorized
tabpage_grouped tabpage_grouped
tabpage_authorized tabpage_authorized
end type

on tab_authorizations.create
this.tabpage_unauthorized=create tabpage_unauthorized
this.tabpage_grouped=create tabpage_grouped
this.tabpage_authorized=create tabpage_authorized
this.Control[]={this.tabpage_unauthorized,&
this.tabpage_grouped,&
this.tabpage_authorized}
end on

on tab_authorizations.destroy
destroy(this.tabpage_unauthorized)
destroy(this.tabpage_grouped)
destroy(this.tabpage_authorized)
end on

event selectionchanged;LONG ll_rowsindw, ll_row, ll_return
DATAWINDOW ldw_dw
 
 inv_tab_resize = create n_resize
 
 IF newindex = 3 THEN
	cb_authorize.Text = 'Unauthorize'
	ll_row = tabpage_authorized.dw_authorized.GetRow()
	ldw_dw = THIS.tabpage_authorized.dw_authorized
	idw_dw = ldw_dw
ELSEIF newindex = 2 THEN
	cb_authorize.Text = 'Authorize'
	ll_row = tabpage_grouped.dw_unauthorized_grouped_payments.GetRow()
	ldw_dw = THIS.tabpage_grouped.dw_unauthorized_grouped_payments
	idw_dw = ldw_dw
ELSE
	cb_authorize.Text = 'Authorize'
	ll_row = tabpage_unauthorized.dw_unauthorized.GetRow()
	ldw_dw = THIS.tabpage_unauthorized.dw_unauthorized
	idw_dw = ldw_dw
END IF

IF ll_row > 0 THEN
	ll_return = wf_retrieve_details(ll_row, ldw_dw)
ELSE
	dw_authorize_details.Reset()
END IF

IF ll_return < 0 THEN
	RETURN
END IF


end event

event constructor; inv_tab_resize = create n_resize
end event

event selectionchanging;DATAWINDOW ldw_dw
LONG ll_cntr, ll_rowcount, ll_checked, ll_count_checked
DATE ldt_serverdate
STRING ls_message

ldt_serverdate = DATE(f_server_datetime())

IF oldindex = 1 THEN
	ldw_dw = tab_authorizations.tabpage_unauthorized.dw_unauthorized
	ls_message = 'Authorized'
ELSEIF oldindex = 2 THEN
	ldw_dw = tab_authorizations.tabpage_grouped.dw_unauthorized_grouped_payments
	ls_message = 'Authorized'
ELSE
	ldw_dw = tab_authorizations.tabpage_authorized.dw_authorized
	ls_message = 'Unauthorized'
END IF
	
ll_rowcount = ldw_dw.RowCount()	

FOR ll_cntr = 1 to ll_rowcount
	ll_checked = ldw_dw.GetItemNumber(ll_cntr, 'checkbox_group')
	IF ll_checked > 0 THEN
		ldw_dw.SetItem(ll_cntr,"authorized_date",ldt_serverdate)
		//using column number here as the column names in all dws are different, but all are column # 13. do not change order
		ldw_dw.SetItem(ll_cntr,13,vgst_user_profile.user_id)
		ll_count_checked = ll_count_checked + 1
	END IF
NEXT

IF ll_count_checked > 0 THEN
	IF MessageBox('Continue?','You have records checked to be ' + ls_message + '. These will be unchecked if you continue.  ~r~n~r~nContinue?', Question!, YesNo!) = 2 THEN
		RETURN 1
	ELSE
		wf_sel_des_all(0, ldw_dw) 
	END IF
END IF
end event

type tabpage_unauthorized from userobject within tab_authorizations
event create ( )
event destroy ( )
integer x = 18
integer y = 108
integer width = 4617
integer height = 1116
long backcolor = 67108864
string text = "Unauthorized Payments"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
dw_unauthorized dw_unauthorized
end type

on tabpage_unauthorized.create
this.dw_unauthorized=create dw_unauthorized
this.Control[]={this.dw_unauthorized}
end on

on tabpage_unauthorized.destroy
destroy(this.dw_unauthorized)
end on

type dw_unauthorized from u_datawindow within tabpage_unauthorized
integer x = 9
integer y = 16
integer width = 4585
integer height = 1088
integer taborder = 10
string dataobject = "d_payments_to_authorize"
boolean vscrollbar = true
end type

event rbuttondown;call super::rbuttondown;/* Rob Head 98/09/14 override ancestor script instead of extending */
M_DW_ONLINE_RMB_POPUP lm_popup
/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/
	SetPointer(HourGlass!)

	lm_popup = Create m_dw_online_rmb_popup
	lm_popup.mf_set_datawindow(This)
	lm_popup.m_options.m_sort.visible = TRUE	
	lm_popup.m_options.PopMenu(w_frame.PointerX( ), w_frame.PointerY( ))

	Destroy lm_popup
end event

event rowfocuschanged;call super::rowfocuschanged;LONG	ll_return

IF currentrow > 0 THEN
	wf_select_row(currentrow, THIS)	
	ll_return = wf_retrieve_details(currentrow, THIS)
	
	IF THIS.DataObject = 'd_annuities_to_authorize'  THEN
		il_individual_no = THIS.GetItemNumber(currentrow, 'individual_no')
		il_annuity_payout_no = THIS.GetItemNumber(currentrow, 'annuity_payout_no')
		cb_ann_payout_report.Enabled = TRUE
	ELSE
		cb_ann_payout_report.Enabled = FALSE
	END IF 

END IF
end event

event clicked;call super::clicked;LONG	ll_return

IF row > 0 THEN	
	wf_select_row(row, THIS)	
END IF



end event

type tabpage_grouped from userobject within tab_authorizations
integer x = 18
integer y = 108
integer width = 4617
integer height = 1116
long backcolor = 67108864
string text = "Unauthorized Grouped Payments"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
dw_unauthorized_grouped_payments dw_unauthorized_grouped_payments
end type

on tabpage_grouped.create
this.dw_unauthorized_grouped_payments=create dw_unauthorized_grouped_payments
this.Control[]={this.dw_unauthorized_grouped_payments}
end on

on tabpage_grouped.destroy
destroy(this.dw_unauthorized_grouped_payments)
end on

type dw_unauthorized_grouped_payments from u_datawindow within tabpage_grouped
integer x = 9
integer y = 16
integer width = 4585
integer height = 1088
integer taborder = 20
string dataobject = "d_payments_to_authorize_group"
boolean vscrollbar = true
end type

event clicked;call super::clicked;LONG	ll_return, ll_auth_no, ll_rowcount, ll_cntr, ll_is_checked, ll_next_auth_no, ll_row

IF row > 0 THEN	
	
	wf_select_row(row, THIS)	
	
	ll_rowcount = THIS.RowCount()
	ll_row = row
	IF dwo.name = 'checkbox_group' THEN		
		ll_is_checked = THIS.GetItemNumber(ll_row, 'checkbox_group')		
		IF ll_is_checked = 1 THEN
			ll_is_checked = 0
		ELSE
			ll_is_checked = 1
		END IF
		ll_auth_no = THIS.GetItemNumber(row, 'authorization_group_no')
		IF ll_auth_no > 0 THEN			
			FOR ll_cntr = 1 to ll_rowcount
				ll_next_auth_no = THIS.GetItemNumber(ll_cntr, 'authorization_group_no')
				IF ll_auth_no = ll_next_auth_no AND row <> ll_cntr THEN
					THIS.SetItem(ll_cntr, 'checkbox_group', ll_is_checked)
				END IF
			NEXT
		END IF
	END IF
END IF



end event

event rbuttondown;call super::rbuttondown;/* Rob Head 98/09/14 override ancestor script instead of extending */
M_DW_ONLINE_RMB_POPUP lm_popup
/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/
	SetPointer(HourGlass!)

	lm_popup = Create m_dw_online_rmb_popup
	lm_popup.mf_set_datawindow(This)
	lm_popup.m_options.m_sort.visible = TRUE	
	lm_popup.m_options.PopMenu(w_frame.PointerX( ), w_frame.PointerY( ))

	Destroy lm_popup
end event

event rowfocuschanged;call super::rowfocuschanged;LONG	ll_return

IF currentrow > 0 THEN
	wf_select_row(currentrow, THIS)	
	ll_return = wf_retrieve_details(currentrow, THIS)
END IF
end event

type tabpage_authorized from userobject within tab_authorizations
event create ( )
event destroy ( )
integer x = 18
integer y = 108
integer width = 4617
integer height = 1116
long backcolor = 67108864
string text = "Authorized Payments Waiting for Processing"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
dw_authorized dw_authorized
end type

on tabpage_authorized.create
this.dw_authorized=create dw_authorized
this.Control[]={this.dw_authorized}
end on

on tabpage_authorized.destroy
destroy(this.dw_authorized)
end on

type dw_authorized from u_datawindow within tabpage_authorized
integer x = 9
integer y = 16
integer width = 4585
integer height = 1088
integer taborder = 20
string dataobject = "d_payments_to_unauthorize"
boolean vscrollbar = true
end type

event clicked;call super::clicked;LONG	ll_return, ll_auth_no, ll_rowcount, ll_cntr, ll_is_checked, ll_next_auth_no, ll_row

IF row > 0 THEN	
	
	wf_select_row(row, THIS)	
	
	ll_rowcount = THIS.RowCount()
	ll_row = row
	IF dwo.name = 'checkbox_group' THEN		
		ll_is_checked = THIS.GetItemNumber(ll_row, 'checkbox_group')		
		IF ll_is_checked = 1 THEN
			ll_is_checked = 0
		ELSE
			ll_is_checked = 1
		END IF
		IF is_retrieve_type = 'PAYMENTS' THEN
			ll_auth_no = THIS.GetItemNumber(row, 'authorization_group_no')
			IF ll_auth_no > 0 THEN			
				FOR ll_cntr = 1 to ll_rowcount
					ll_next_auth_no = THIS.GetItemNumber(ll_cntr, 'authorization_group_no')
					IF ll_auth_no = ll_next_auth_no AND row <> ll_cntr THEN
						THIS.SetItem(ll_cntr, 'checkbox_group', ll_is_checked)
					END IF
				NEXT
			END IF
		END IF
	END IF
END IF



end event

event rowfocuschanged;call super::rowfocuschanged;LONG	ll_return

IF currentrow > 0 THEN
	wf_select_row(currentrow, THIS)	
	ll_return = wf_retrieve_details(currentrow, THIS)
			
	IF THIS.DataObject = 'd_annuities_to_unauthorize'  THEN
		il_individual_no = THIS.GetItemNumber(currentrow, 'individual_no')
		il_annuity_payout_no = THIS.GetItemNumber(currentrow, 'annuity_payout_no')
		cb_ann_payout_report.Enabled = TRUE
	ELSE
		cb_ann_payout_report.Enabled = FALSE
	END IF 

END IF


end event

event rbuttondown;call super::rbuttondown;/* Rob Head 98/09/14 override ancestor script instead of extending */
M_DW_ONLINE_RMB_POPUP lm_popup
/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/
	SetPointer(HourGlass!)

	lm_popup = Create m_dw_online_rmb_popup
	lm_popup.mf_set_datawindow(This)
	lm_popup.m_options.m_sort.visible = TRUE	
	lm_popup.m_options.PopMenu(w_frame.PointerX( ), w_frame.PointerY( ))

	Destroy lm_popup
end event

type gb_3 from groupbox within w_authorizations
integer x = 59
integer y = 24
integer width = 4645
integer height = 616
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
end type

