$PBExportHeader$w_create_claim.srw
$PBExportComments$main window for creating a claim - individual created or modified if exists
forward
global type w_create_claim from w_ancestor
end type
type dw_claim_no from u_dw_online within w_create_claim
end type
type dw_claim_participant from u_dw_online within w_create_claim
end type
type dw_individual_name from u_dw_online within w_create_claim
end type
type cb_ok from commandbutton within w_create_claim
end type
type cb_cancel from commandbutton within w_create_claim
end type
type st_claim_create from statictext within w_create_claim
end type
type dw_next_individual_no from u_dw_online within w_create_claim
end type
type cb_override from commandbutton within w_create_claim
end type
type dw_create_claim from u_dw_online within w_create_claim
end type
type dw_claim_for_individual from u_dw_online within w_create_claim
end type
type dw_individual from u_dw_online within w_create_claim
end type
type dw_individual_main_name from u_dw_online within w_create_claim
end type
type uo_sin_med_check from u_check_sin_med_vert within w_create_claim
end type
end forward

global type w_create_claim from w_ancestor
integer x = 987
integer y = 320
integer width = 1742
integer height = 2792
string menuname = ""
boolean controlmenu = false
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
long backcolor = 67108864
dw_claim_no dw_claim_no
dw_claim_participant dw_claim_participant
dw_individual_name dw_individual_name
cb_ok cb_ok
cb_cancel cb_cancel
st_claim_create st_claim_create
dw_next_individual_no dw_next_individual_no
cb_override cb_override
dw_create_claim dw_create_claim
dw_claim_for_individual dw_claim_for_individual
dw_individual dw_individual
dw_individual_main_name dw_individual_main_name
uo_sin_med_check uo_sin_med_check
end type
global w_create_claim w_create_claim

type variables
S_WINDOW_MESSAGE istr_parameters
N_CREATE_CLAIM in_control_claim_create
W_DOCUMENT_INDEXING iwi_doc
BOOLEAN ib_alias_switch

DATAWINDOWCHILD	idwc_sob
end variables

event open;call super::open;U_DWA ldw_dw[]
LONG			ll_individual_no[]
INTEGER		li_rtn
// create object
// initialize 

// get the individual number from the message paramete to determine 
//if it is a new individual or the indiviual already exists

	istr_parameters = Message.PowerObjectParm

	ll_individual_no[1] = istr_parameters.al_doubleparm[1]

	iwi_doc = istr_parameters.awi_parent_window

	in_control_claim_create = Create n_create_claim

	in_control_claim_create.nf_set_window_parent(THIS)
	ldw_dw[1] = dw_create_claim
	ldw_dw[2] = dw_individual
	ldw_dw[3] = dw_individual_main_name
	ldw_dw[4] = dw_claim_participant
	ldw_dw[5] = dw_claim_no
	ldw_dw[6] = dw_individual_name
	ldw_dw[7] = dw_next_individual_no
	ldw_dw[8] = dw_claim_for_individual
	in_control_claim_create.nf_set_datawindow(ldw_dw[],SQLCA)

	in_control_claim_create.nf_init()

	IF ll_individual_no[1] = 0 THEN
   	in_control_claim_create.nf_insert()	
	ELSE
   	in_control_claim_create.nf_retrieve(ll_individual_no[1])
		in_control_claim_create.nf_disable_fields()
	END IF
		
	dw_create_claim.SetColumn('accident_date')
	dw_create_claim.SetFocus()
	
	li_rtn = dw_claim_for_individual.Retrieve(ll_individual_no ,0)
	IF li_rtn <  0 Then SignalError(-666,'Error occured retrieving claims for individual #' + String(ll_individual_no))
	
	SQLCA.nf_handle_error('w_create_claim','OPEN','Retrieve dw_claims_for_individual')

	ldw_dw[1].GetChild("side_of_body_code",idwc_sob)
	idwc_sob.SetTransObject(SQLCA)
	
	
/* set the location of this window = to the frame x and y */
	this.x = w_frame.x
	this.y = w_frame.y

end event

on close;call w_ancestor::close;// destroy objects
Destroy in_control_claim_create
end on

on w_create_claim.create
int iCurrent
call super::create
this.dw_claim_no=create dw_claim_no
this.dw_claim_participant=create dw_claim_participant
this.dw_individual_name=create dw_individual_name
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
this.st_claim_create=create st_claim_create
this.dw_next_individual_no=create dw_next_individual_no
this.cb_override=create cb_override
this.dw_create_claim=create dw_create_claim
this.dw_claim_for_individual=create dw_claim_for_individual
this.dw_individual=create dw_individual
this.dw_individual_main_name=create dw_individual_main_name
this.uo_sin_med_check=create uo_sin_med_check
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_claim_no
this.Control[iCurrent+2]=this.dw_claim_participant
this.Control[iCurrent+3]=this.dw_individual_name
this.Control[iCurrent+4]=this.cb_ok
this.Control[iCurrent+5]=this.cb_cancel
this.Control[iCurrent+6]=this.st_claim_create
this.Control[iCurrent+7]=this.dw_next_individual_no
this.Control[iCurrent+8]=this.cb_override
this.Control[iCurrent+9]=this.dw_create_claim
this.Control[iCurrent+10]=this.dw_claim_for_individual
this.Control[iCurrent+11]=this.dw_individual
this.Control[iCurrent+12]=this.dw_individual_main_name
this.Control[iCurrent+13]=this.uo_sin_med_check
end on

on w_create_claim.destroy
call super::destroy
destroy(this.dw_claim_no)
destroy(this.dw_claim_participant)
destroy(this.dw_individual_name)
destroy(this.cb_ok)
destroy(this.cb_cancel)
destroy(this.st_claim_create)
destroy(this.dw_next_individual_no)
destroy(this.cb_override)
destroy(this.dw_create_claim)
destroy(this.dw_claim_for_individual)
destroy(this.dw_individual)
destroy(this.dw_individual_main_name)
destroy(this.uo_sin_med_check)
end on

type dw_claim_no from u_dw_online within w_create_claim
boolean visible = false
integer x = 192
integer y = 1564
integer width = 165
integer height = 160
integer taborder = 0
string dataobject = "d_claim_no"
end type

type dw_claim_participant from u_dw_online within w_create_claim
boolean visible = false
integer x = 370
integer y = 1564
integer width = 165
integer height = 160
integer taborder = 0
string dataobject = "d_claim_participant_maint"
end type

type dw_individual_name from u_dw_online within w_create_claim
boolean visible = false
integer x = 535
integer y = 1564
integer width = 165
integer height = 160
integer taborder = 0
string dataobject = "d_individual_name"
end type

type cb_ok from commandbutton within w_create_claim
integer x = 539
integer y = 2584
integer width = 315
integer height = 96
integer taborder = 70
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&OK"
end type

event clicked;INTEGER	li_rtn, li_trancount
LONG		ll_claim_no, ll_return_value, ll_cnt, ll_no, ll_individual_no, ll_return
STRING	ls_old_alias_lname, ls_old_alias_fname, ls_old_main_lname, ls_old_main_fname, ls_new_alias_lname, &
			ls_new_alias_fname, ls_new_main_lname, ls_new_main_fname, ls_temp_alias_lname, ls_temp_alias_fname
STRING	ls_temp_main_lname, ls_temp_main_fname
W_SHEET	lw_sheet

N_PROCESS_RUN_STATUS ln_process_run_status

/******************************************************************************************
P10275 - Daytime Payment Processing
- added new function call to prevent updating of tables used by PRODSVCS Payment Processing
- new object N_PROCESS_RUN_STATUS is used to determine status of processes (in progress = Y/N)
- '008' refers to the Document Indexing module, '044' refers to the Payment Processing module
- likewise, '046' refers to the ABCC Eligibility Export module
******************************************************************************************/
ln_process_run_status = Create N_PROCESS_RUN_STATUS

li_rtn = ln_process_run_status.nf_in_progress('019','044','creation of a claim',SQLCA)

IF li_rtn = 1 THEN
	// module is blocked
	return
END IF

//clear variable
li_rtn = 0
li_rtn = ln_process_run_status.nf_in_progress('008','046','creation of a claim',SQLCA)

IF li_rtn = 1 THEN
	// module is blocked
	return
END IF

/******************************************************************************************/



IF dw_create_claim.AcceptText() < 0 THEN Return
IF dw_individual.AcceptText() < 0 THEN Return


ib_alias_switch = FALSE

ll_individual_no = dw_individual.GetItemNUmber ( 1, "individual_no" )

SetPointer(HourGlass!)

/*	Check if main or alias names have been changed.  If name(s) have been changed, then check if the alias & main names
	are being switched.  Since SQLServer07 was implemented, users have had duplicate primary key problems when trying
	to switch the alias & main	names for an individual.  The problems are due to how primary keys are now handled.
	In order to avoid a violation of the 'pk_INDIVIDUAL_NAME' primary key constraint, the name switch must be done in
	two stages.
*/
IF ( dw_individual_main_name.GetItemStatus ( dw_individual_main_name.GetRow(), 0, Primary! ) = DataModified! OR &
	  ( dw_individual.GetItemStatus ( 1, "individual_name_last_name", Primary! ) = DataModified! OR &
	    dw_individual.GetItemStatus ( 1, "individual_name_given_names", Primary! ) = DataModified! ) ) THEN

/* Get original alias names from table
*/
	SELECT last_name, given_names
	  INTO :ls_old_alias_lname, :ls_old_alias_fname 
	  FROM INDIVIDUAL_NAME
	 WHERE individual_no = :ll_individual_no
	   AND name_type_code = 'A'
	 USING SQLCA ;
 
	IF SQLCA.nf_handle_error('Embedded SQL: SELECT Alias Name FROM INDIVIDUAL_NAME', 'w_individual', 'cb_save clicked') < 0 THEN
		Return -1
	END IF

/* Get original main names from table
*/
	SELECT last_name, given_names
	  INTO :ls_old_main_lname, :ls_old_main_fname
	  FROM INDIVIDUAL_NAME
	 WHERE individual_no = :ll_individual_no
	   AND name_type_code = 'M'
	 USING SQLCA ;
 
	IF SQLCA.nf_handle_error('Embedded SQL: SELECT Main Name FROM INDIVIDUAL_NAME', 'w_individual', 'cb_save clicked') < 0 THEN
		Return -1
	END IF
 
	ls_new_alias_lname = dw_individual.GetItemString( 1, 'individual_name_last_name' )
	ls_new_alias_fname = dw_individual.GetItemString( 1, 'individual_name_given_names' )
	ls_new_main_lname = dw_individual_main_name.GetItemString( 1, 'last_name' )
	ls_new_main_fname = dw_individual_main_name.GetItemString( 1, 'given_names' )

//check to see if main last name is null
	IF ls_new_main_lname = '' THEN
		MessageBox('Last Name', 'Last Name is required.',information!)
		Return -1
	END IF
	
//check to see if main first name is null
	IF	ls_new_main_fname = '' THEN
		MessageBox('First Name', 'First Name is required.',information!)
		Return -1
	END IF
	
//check if new name and alias name are the same, if so prevent save.
	IF ls_new_alias_fname + ls_new_alias_lname = ls_new_main_fname + ls_new_main_lname THEN
		MessageBox("Error", "The alias cannot be the same as the main name, please delete or change it.")
		dw_individual.SetFocus()
		dw_individual.SetColumn('individual_name_last_name')
		Return -1
	END IF
END IF

SQLCA.nf_begin_transaction()

ll_return_value = in_control_claim_create.nf_save()
IF ll_return_value < 0 THEN
	SQLCA.nf_transaction_count(li_trancount,0,'','','',FALSE)
	IF li_trancount > 0 THEN
		SQLCA.nf_rollback_transaction()
	END IF

	Return -1
END IF

// this function call was formerly in n_create_claim's nf_commit function
in_control_claim_create.nf_image_create()

SQLCA.nf_commit_transaction()

ib_alias_switch = FALSE
ll_claim_no = in_control_claim_create.nf_get_claim_no()
iwi_doc.wf_set_claim(ll_claim_no)
cb_cancel.TriggerEvent(Clicked!)


Return  // call close - cb_cancel.TriggerEvent(Clicked!)
end event

type cb_cancel from commandbutton within w_create_claim
integer x = 873
integer y = 2584
integer width = 315
integer height = 96
integer taborder = 80
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cance&l"
end type

event clicked;SetPointer(HourGlass!)
//dw_individual.Modify("event_desc.Visible='0'")
//dw_individual.Modify("court_order_t.Visible='0'")
iwi_doc.Show()
Close(PARENT)
end event

type st_claim_create from statictext within w_create_claim
integer x = 18
integer y = 8
integer width = 1691
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Claim Create"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_next_individual_no from u_dw_online within w_create_claim
boolean visible = false
integer x = 709
integer y = 1564
integer width = 165
integer height = 160
integer taborder = 0
string dataobject = "d_next_individual_no"
end type

type cb_override from commandbutton within w_create_claim
integer x = 850
integer y = 124
integer width = 265
integer height = 68
integer taborder = 60
integer textsize = -8
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "O&verride"
end type

on clicked;// open up the field for edit
dw_create_claim.SetTabOrder('claim_no',5)
dw_create_claim.SetColumn('claim_no')
dw_create_claim.SetFocus()
end on

type dw_create_claim from u_dw_online within w_create_claim
integer x = 14
integer y = 84
integer width = 1714
integer height = 516
integer taborder = 20
string dataobject = "d_claim_create"
borderstyle borderstyle = styleraised!
end type

on losefocus;call u_dw_online::losefocus;	This.PostEvent('ue_post_losefocus')
end on

event itemfocuschanged;/* override */
LONG      ll_rowcount, ll_num_rows, ll_rtn
STRING    ls_side_of_body_code, ls_side_of_body_desc, ls_dwo_name, ls_part_of_body_code


ls_dwo_name = dwo.name

//Set filtering on side of body
if ls_dwo_name = 'side_of_body_code' THEN		
		
	//filter the side of body drop down.
	ll_rtn = this.GetChild("side_of_body_code",idwc_sob)
	ll_num_rows = idwc_sob.RowCount()
	
	idwc_sob.Retrieve()
	SQLCA.nf_handle_error("w_create_claim","itemchanged","idwc_sob.Retrieve()")
	ll_rowcount = idwc_sob.RowCount()
	
	ls_part_of_body_code = THIS.GetItemString(1,'part_of_body_code')
	
	idwc_sob.SetFilter("part_of_body_code = '" + ls_part_of_body_code + "'" )
	idwc_sob.Filter()
	idwc_sob.SetSort("#2 A")
	idwc_sob.Sort()
	
	this.SetItem(1,'side_of_body_code','')
	this.object.side_of_body_code.protect = 0
	
	IF idwc_sob.RowCount() = 1 THEN
		ls_side_of_body_code = idwc_sob.GetItemString(1,'side_of_body_side_of_body_code')
		ls_side_of_body_desc = idwc_sob.GetItemString(1,'side_of_body_side_of_body_desc_e')
		THIS.SetItem(1,'side_of_body_code',ls_side_of_body_code)
		
		THIS.SetColumn('side_of_body_code')
		THIS.SetText(ls_side_of_body_desc)		
	END IF
	
END IF
	
end event

event itemchanged;LONG      ll_rowcount, ll_num_rows, ll_rtn
STRING    ls_side_of_body_code, ls_side_of_body_desc, ls_dwo_name

IF in_control_claim_create.nf_change_item(1) < 0 THEN
	RETURN 1
END IF


cb_ok.enabled = TRUE

end event

event ue_post_losefocus;call super::ue_post_losefocus;//	IF This.AcceptText() < 0 THEN
//		This.SetFocus()
//	END IF
end event

event rbuttondown;M_DW_ONLINE_RMB_POPUP lm_popup
/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/
	lm_popup = Create m_dw_online_rmb_popup
	lm_popup.mf_set_datawindow(This)
	
	lm_popup.m_options.PopMenu(w_create_claim.PointerX(), w_create_claim.PointerY())
	
	Destroy lm_popup
end event

event ue_print;LONG ll_job

	ll_job = PrintOpen()
	Parent.Print(ll_job,0,0)
	PrintClose(ll_job)

end event

type dw_claim_for_individual from u_dw_online within w_create_claim
boolean visible = false
integer x = 375
integer y = 1552
integer height = 360
integer taborder = 10
string dataobject = "d_claims_for_individual"
end type

type dw_individual from u_dw_online within w_create_claim
integer x = 14
integer y = 596
integer width = 1714
integer height = 1980
integer taborder = 50
boolean bringtotop = true
string dataobject = "d_individual_vert"
borderstyle borderstyle = styleraised!
end type

on losefocus;call u_dw_online::losefocus;	This.PostEvent('ue_post_losefocus')
end on

event itemfocuschanged;/*	this is here because it screws up on edit masked columns
*/
IF dwo.name = "individual_court_order_flag" THEN
	this.setitem(row,'event_desc','')
END IF
end event

event itemchanged;call super::itemchanged;String ls_prov, ls_region

IF dwo.name = "individual_court_order_flag" THEN
	this.Modify("event_desc.Visible='1'")
	this.Modify("court_order_t.Visible='1'")
END IF
IF in_control_claim_create.nf_change_item(2) < 0 THEN
	RETURN 1
END IF

//Code Re-Added September 3, 1998 EKP
IF w_create_claim.dw_individual.GetColumnName() = 'prov_state_code' AND w_create_claim.dw_individual.GetText() <> 'NB' THEN
	ls_prov = w_create_claim.dw_individual.GetText()
	OpenWithParm(w_default_regions, ls_prov)
	ls_region = Message.StringParm
	dw_create_claim.SetItem(1,'admin_region_code',ls_region)
END IF
//End Re-Add

cb_ok.enabled = TRUE
end event

event ue_post_losefocus;call super::ue_post_losefocus;//	IF This.AcceptText() < 0 THEN
//		This.SetFocus()
//	END IF
end event

event rbuttondown;M_DW_ONLINE_RMB_POPUP lm_popup
/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/
	lm_popup = Create m_dw_online_rmb_popup
	lm_popup.mf_set_datawindow(This)
	
	lm_popup.m_options.PopMenu(w_create_claim.PointerX( ), w_create_claim.PointerY( ))

	Destroy lm_popup
end event

event ue_print;LONG ll_job

	ll_job = PrintOpen()
	Parent.Print(ll_job,0,0)
	PrintClose(ll_job)

end event

event retrieveend;call super::retrieveend;STRING ls_eventMessage,ls_eventSpecfic
long ll_individual, ll_sin, ll_medicare

if rowcount > 0 then
	ll_individual = this.getitemnumber(this.getrow(),'individual_no')
	SELECT TOP 1 CLAIM_EVENT.event_comment ,event_specific_code  
	INTO :ls_eventMessage,:ls_eventSpecfic 
	FROM CLAIM_EVENT  
	WHERE 	CLAIM_EVENT.event_type_code = '020' AND
		CLAIM_EVENT.claim_no in (  SELECT CLAIM.claim_no  
												FROM CLAIM  
												WHERE CLAIM.individual_no = :ll_individual )   
	Order By CLAIM_EVENT.event_date desc,
				CLAIM_EVENT.event_no desc;
	
	IF len(trim(ls_eventMessage)) > 0 then
		this.setitem(this.getrow(),'event_desc',ls_eventMessage)
	end if
end if


end event

event rowfocuschanged;call super::rowfocuschanged;STRING ls_eventMessage,ls_eventSpecfic
long ll_individual
if this.rowcount() > 0 then
	ll_individual = this.getitemnumber(currentrow,'individual_no')
	SELECT TOP 1 CLAIM_EVENT.event_comment ,event_specific_code  
	INTO :ls_eventMessage,:ls_eventSpecfic 
	FROM CLAIM_EVENT  
	WHERE 	CLAIM_EVENT.event_type_code = '020' AND
		CLAIM_EVENT.claim_no in (  SELECT CLAIM.claim_no  
												FROM CLAIM  
												WHERE CLAIM.individual_no = :ll_individual )   
	Order By CLAIM_EVENT.event_date desc,
				CLAIM_EVENT.event_no desc;
	
	IF len(trim(ls_eventMessage)) > 0 then
		this.setitem(currentrow,'event_desc',ls_eventMessage)
	end if
end if
end event

event editchanged;call super::editchanged;
IF dwo.name = 'individual_name_last_name' OR dwo.name ='individual_name_given_names' THEN
	IF len(data) = 1  OR (  len(data) > 1 AND MATCH( MID(data, len(data) - 1, 1) , "[ '-]" ) ) THEN   //looks for a space or a hyphen or a apostrophy
		// this.settext(WordCap(data))  WordCap function works for the space but not the hyphen or apostrophy so manually upper case the last character
		this.settext (  left (data,len(data)-1) + upper(right(data,1)  ))	
		this.SelectText(this.selectedstart() + len(data) + 1, 0)	
	END IF
END IF

// this removes a leading edge space and any space that follows a space (so there is no double spaces)
IF dwo.name = 'individual_name_last_name' OR dwo.name ='individual_name_given_names' THEN
	IF MATCH( MID(data, len(data) - 1, 1) , "[ ]" )   AND right(data,1) = ' ' THEN
		this.settext (  left (data,len(data)-1) )	
		this.SelectText(this.selectedstart() + len(data) + 1, 0)	
	END IF
END IF
end event

type dw_individual_main_name from u_dw_online within w_create_claim
integer x = 55
integer y = 680
integer width = 1211
integer height = 148
integer taborder = 30
boolean bringtotop = true
string dataobject = "d_individual_name_main_vert"
boolean border = false
end type

on losefocus;call u_dw_online::losefocus;	This.PostEvent('ue_post_losefocus')
end on

event itemchanged;call super::itemchanged;
IF in_control_claim_create.nf_change_item(3) < 0 THEN
	RETURN 1
END IF
cb_ok.enabled = TRUE
end event

event ue_post_losefocus;call super::ue_post_losefocus;//	IF This.AcceptText() < 0 THEN
//		This.SetFocus()
//	END IF
end event

event rbuttondown;M_DW_ONLINE_RMB_POPUP lm_popup
/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/
	lm_popup = Create m_dw_online_rmb_popup
	lm_popup.mf_set_datawindow(This)
	
	lm_popup.m_options.PopMenu(w_create_claim.PointerX(), w_create_claim.PointerY())
	
	Destroy lm_popup
end event

event ue_print;LONG ll_job

	ll_job = PrintOpen()
	Parent.Print(ll_job,0,0)
	PrintClose(ll_job)

end event

event editchanged;call super::editchanged;IF len(data) = 1  OR (  len(data) > 1 AND MATCH( MID(data, len(data) - 1, 1) , "[ '-]" ) ) THEN   //looks for a space or a hyphen or a apostrophy
	// this.settext(WordCap(data))  WordCap function works for the space but not the hyphen or apostrophy so manually upper case the last character
	this.settext (  left (data,len(data)-1) + upper(right(data,1)  ))	
	this.SelectText(this.selectedstart() + len(data) + 1, 0)	
END IF

// this removes a leading edge space and any space that follows a space (so there is no double spaces)
IF MATCH( MID(data, len(data) - 1, 1) , "[ ]" )   AND right(data,1) = ' ' THEN
	this.settext (  left (data,len(data)-1) )	
	this.SelectText(this.selectedstart() + len(data) + 1, 0)	
END IF

end event

type uo_sin_med_check from u_check_sin_med_vert within w_create_claim
integer x = 27
integer y = 172
integer taborder = 60
end type

on uo_sin_med_check.destroy
call u_check_sin_med_vert::destroy
end on

