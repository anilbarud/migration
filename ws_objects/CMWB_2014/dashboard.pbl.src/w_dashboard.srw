$PBExportHeader$w_dashboard.srw
forward
global type w_dashboard from w_ancestor
end type
type dw_report from datawindow within w_dashboard
end type
type st_claim_no from statictext within w_dashboard
end type
type st_claim from statictext within w_dashboard
end type
type st_filter_heading from statictext within w_dashboard
end type
type st_filter_claims from statictext within w_dashboard
end type
type uo_filter from u_new_filter_control within w_dashboard
end type
type cbx_all from checkbox within w_dashboard
end type
type cbx_recent from checkbox within w_dashboard
end type
type cbx_claims from checkbox within w_dashboard
end type
type cbx_keys from checkbox within w_dashboard
end type
type cbx_alerts from checkbox within w_dashboard
end type
type pb_print from picturebutton within w_dashboard
end type
type tv_filter from treeview within w_dashboard
end type
type pb_refresh from picturebutton within w_dashboard
end type
type dw_header from datawindow within w_dashboard
end type
type st_4 from statictext within w_dashboard
end type
type dw_profile from datawindow within w_dashboard
end type
type pb_alerts from picturebutton within w_dashboard
end type
type pb_claims from picturebutton within w_dashboard
end type
type st_keys from statictext within w_dashboard
end type
type st_claims from statictext within w_dashboard
end type
type st_6 from statictext within w_dashboard
end type
type st_alerts from statictext within w_dashboard
end type
type tv_2 from treeview within w_dashboard
end type
type tv_1 from treeview within w_dashboard
end type
type st_3 from statictext within w_dashboard
end type
type st_2 from statictext within w_dashboard
end type
type st_1 from statictext within w_dashboard
end type
type rr_alerts from roundrectangle within w_dashboard
end type
type rr_claims from roundrectangle within w_dashboard
end type
type rr_keys from roundrectangle within w_dashboard
end type
type pb_keys from picturebutton within w_dashboard
end type
type st_week from statictext within w_dashboard
end type
type st_current from statictext within w_dashboard
end type
type rr_recent from roundrectangle within w_dashboard
end type
type st_recent from statictext within w_dashboard
end type
type pb_recent from picturebutton within w_dashboard
end type
type gb_1 from groupbox within w_dashboard
end type
type rr_current from roundrectangle within w_dashboard
end type
type pb_calendar_area4 from picturebutton within w_dashboard
end type
type gb_2 from groupbox within w_dashboard
end type
type dw_claims from u_dw_online within w_dashboard
end type
type dw_recent from u_dw_online within w_dashboard
end type
type dw_keys from u_dw_online within w_dashboard
end type
type dw_alerts from u_dw_online within w_dashboard
end type
type st_filter_recent from statictext within w_dashboard
end type
end forward

global type w_dashboard from w_ancestor
string tag = "key_indicators"
integer width = 6738
integer height = 3552
string title = "Individual Dashboard"
string menuname = ""
boolean hscrollbar = true
boolean vscrollbar = true
windowtype windowtype = main!
long backcolor = 16777215
string icon = "views_16.ico"
boolean clientedge = true
boolean center = true
long il_design_time_height = 3368
long il_design_time_width = 6610
dw_report dw_report
st_claim_no st_claim_no
st_claim st_claim
st_filter_heading st_filter_heading
st_filter_claims st_filter_claims
uo_filter uo_filter
cbx_all cbx_all
cbx_recent cbx_recent
cbx_claims cbx_claims
cbx_keys cbx_keys
cbx_alerts cbx_alerts
pb_print pb_print
tv_filter tv_filter
pb_refresh pb_refresh
dw_header dw_header
st_4 st_4
dw_profile dw_profile
pb_alerts pb_alerts
pb_claims pb_claims
st_keys st_keys
st_claims st_claims
st_6 st_6
st_alerts st_alerts
tv_2 tv_2
tv_1 tv_1
st_3 st_3
st_2 st_2
st_1 st_1
rr_alerts rr_alerts
rr_claims rr_claims
rr_keys rr_keys
pb_keys pb_keys
st_week st_week
st_current st_current
rr_recent rr_recent
st_recent st_recent
pb_recent pb_recent
gb_1 gb_1
rr_current rr_current
pb_calendar_area4 pb_calendar_area4
gb_2 gb_2
dw_claims dw_claims
dw_recent dw_recent
dw_keys dw_keys
dw_alerts dw_alerts
st_filter_recent st_filter_recent
end type
global w_dashboard w_dashboard

type variables
n_dashboard inv_dashboard
LONG il_claim_no, il_individual_no, il_sheet_handle
STRING is_selected_recent, is_filtering_dw, is_claims_filter, is_recent_filter

BOOLEAN ib_claims_expanded
//n_resize inv_resize
end variables

forward prototypes
public subroutine wf_load_objects ()
public function integer wf_retrieve_dashboard (long al_individual_no)
public subroutine wf_set_size_vars ()
public subroutine wf_get_active_claim (long al_individual_no)
public subroutine wf_set_individual_no (long al_individual_no)
public function long wf_get_opening (long al_claim_no)
public subroutine wf_expand ()
public subroutine wf_setresize (boolean ab_switch)
public function long wf_get_sheet_handle ()
public subroutine wf_refresh (long al_individual_no, long al_sheet_handle)
public subroutine wf_set_sheet_handle (long al_sheet_handle)
end prototypes

public subroutine wf_load_objects ();INTEGER li_areas[4]
ROUNDRECTANGLE lr_rr[4]
STATICTEXT lst_st[4]
PICTUREBUTTON lpb_pb[4]
U_DW_ONLINE ldw_adw[4]
CHECKBOX lcb_cb[4]

li_areas[] = {1,2,3,4}
lr_rr[] = {rr_alerts, rr_keys, rr_claims, rr_recent}
lst_st[] = {st_alerts, st_keys, st_claims, st_recent}
lpb_pb[] = {pb_alerts, pb_keys, pb_claims, pb_recent}
ldw_adw[] = {dw_alerts, dw_keys, dw_claims, dw_recent}
lcb_cb[] = {cbx_alerts, cbx_keys, cbx_claims, cbx_recent}

wf_set_size_vars()

inv_dashboard.nf_initialize_areas(li_areas[], ldw_adw[], lr_rr[], lst_st[], lpb_pb[], pb_calendar_area4, lcb_cb[])


end subroutine

public function integer wf_retrieve_dashboard (long al_individual_no);LONG ll_upperbound, ll_cntr, ll_profile, ll_header, ll_rtn, ll_row
STRING ls_name, ls_individual_no
DATE ldt_birth_date, ldt_death_date

pb_alerts.Text = '+ see more...'
pb_keys.Text = '+ see more...'
pb_claims.Text = '+ see more...'
pb_recent.Text = '+ see more...'

il_individual_no = al_individual_no
inv_dashboard.nf_set_individual(il_individual_no)

dw_profile.SetTransObject(SQLCA)
ll_profile = dw_profile.Retrieve(il_individual_no)

IF ll_profile <= 0 THEN
	MessageBox('Error','We were not able to find the Dashboard profile for the selected individual.', Information!)
	RETURN -1
END IF

ll_row = dw_profile.GetRow()

ldt_birth_date = DATE(dw_profile.GetItemDateTime(ll_row, 'birth_date'))
ldt_death_date = DATE(dw_profile.GetItemDateTime(ll_row, 'death_date'))
IF NOT IsNull(ldt_birth_date) THEN
	dw_profile.SetItem(ll_row, 'age', inv_dashboard.nf_calculate_age(ldt_birth_date, ldt_death_date))
END IF


dw_header.SetTransObject(SQLCA)
ll_header = dw_header.Retrieve(il_individual_no)

IF ll_header <= 0 THEN
	MessageBox('Error','We were not able to find the Name for the selected individual.', Information!)
	RETURN -1
END IF

ll_rtn = inv_dashboard.nf_retrieve()
IF ll_rtn < 0 THEN
	MessageBox('No Individual Information','There is no Individual information to display.', Information!)
	RETURN -1
END IF

IF inv_dashboard.ib_key_information THEN
	dw_keys.Object.t_info.Visible = FALSE
ELSE
	dw_keys.Object.t_info.Visible = TRUE
END IF

RETURN 0
end function

public subroutine wf_set_size_vars ();int li_dwa_x,  li_dwa_y, li_dwa_w, li_dwa_h
int li_dwk_x ,li_dwk_y ,li_dwk_w,li_dwk_h
int li_dwc_x ,li_dwc_y,li_dwc_w,li_dwc_h
int li_dwr_x,li_dwr_y,li_dwr_w ,li_dwr_h 

int li_rra_x,  li_rra_y, li_rra_w, li_rra_h
int li_rrk_x ,li_rrk_y ,li_rrk_w,li_rrk_h
int li_rrc_x ,li_rrc_y,li_rrc_w,li_rrc_h
int li_rrr_x,li_rrr_y,li_rrr_w ,li_rrr_h 

int li_sta_x,  li_sta_y, li_sta_w, li_sta_h
int li_stk_x ,li_stk_y ,li_stk_w,li_stk_h
int li_stc_x ,li_stc_y,li_stc_w,li_stc_h
int li_str_x,li_str_y,li_str_w ,li_str_h 

int li_pba_x,  li_pba_y, li_pba_w, li_pba_h
int li_pbk_x ,li_pbk_y ,li_pbk_w,li_pbk_h
int li_pbc_x ,li_pbc_y,li_pbc_w,li_pbc_h
int li_pbr_x,li_pbr_y,li_pbr_w ,li_pbr_h 

INT li_init_xywh_dw[4,4]
INT li_init_xywh_rr[4,4] 
INT li_init_xywh_st[4,4]  
INT li_init_xywh_pb[4,4]

//dws
li_dwa_x = dw_alerts.x 
li_dwa_y = dw_alerts.y 
li_dwa_w =dw_alerts.width 
li_dwa_h = dw_alerts.height 

li_dwk_x = dw_keys.x 
li_dwk_y = dw_keys.y 
li_dwk_w = dw_keys.width 
li_dwk_h = dw_keys.height

li_dwc_x = dw_claims.x 
li_dwc_y = dw_claims.y
li_dwc_w = dw_claims.width 
li_dwc_h = dw_claims.height 

li_dwr_x = dw_recent.x 
li_dwr_y = dw_recent.y 
li_dwr_w = dw_recent.width
li_dwr_h = dw_recent.height

//rrs
li_rra_x = rr_alerts.x 
li_rra_y = rr_alerts.y 
li_rra_w =rr_alerts.width 
li_rra_h = rr_alerts.height 

li_rrk_x = rr_keys.x 
li_rrk_y = rr_keys.y 
li_rrk_w = rr_keys.width 
li_rrk_h = rr_keys.height

li_rrc_x = rr_claims.x 
li_rrc_y = rr_claims.y
li_rrc_w = rr_claims.width 
li_rrc_h = rr_claims.height 

li_rrr_x = rr_recent.x 
li_rrr_y = rr_recent.y 
li_rrr_w = rr_recent.width
li_rrr_h = rr_recent.height

//sts
li_sta_x = st_alerts.x 
li_sta_y = st_alerts.y 
li_sta_w =st_alerts.width 
li_sta_h = st_alerts.height 

li_stk_x = st_keys.x 
li_stk_y = st_keys.y 
li_stk_w = st_keys.width 
li_stk_h = st_keys.height

li_stc_x = st_claims.x 
li_stc_y = st_claims.y
li_stc_w = st_claims.width 
li_stc_h = st_claims.height 

li_str_x = st_recent.x 
li_str_y = st_recent.y 
li_str_w = st_recent.width
li_str_h = st_recent.height

//pbs
li_pba_x = pb_alerts.x 
li_pba_y = pb_alerts.y 
li_pba_w =pb_alerts.width 
li_pba_h = pb_alerts.height 

li_pbk_x = pb_keys.x 
li_pbk_y = pb_keys.y 
li_pbk_w = pb_keys.width 
li_pbk_h = pb_keys.height

li_pbc_x = pb_claims.x 
li_pbc_y = pb_claims.y
li_pbc_w = pb_claims.width 
li_pbc_h = pb_claims.height 

li_pbr_x = pb_recent.x 
li_pbr_y = pb_recent.y 
li_pbr_w = pb_recent.width
li_pbr_h = pb_recent.height

 li_init_xywh_dw[] = {li_dwa_x, li_dwa_y, li_dwa_w, li_dwa_h,  li_dwk_x, li_dwk_y, li_dwk_w, li_dwk_h, li_dwc_x, li_dwc_y, li_dwc_w, li_dwc_h,  li_dwr_x, li_dwr_y, li_dwr_w, li_dwr_h}
 li_init_xywh_rr[]  =  {li_rra_x, li_rra_y, li_rra_w, li_rra_h,  li_rrk_x, li_rrk_y, li_rrk_w, li_rrk_h, li_rrc_x, li_rrc_y, li_rrc_w, li_rrc_h,  li_rrr_x, li_rrr_y, li_rrr_w, li_rrr_h}
 li_init_xywh_st[]  =  {li_sta_x, li_sta_y, li_sta_w, li_sta_h,  li_stk_x, li_stk_y, li_stk_w, li_stk_h, li_stc_x, li_stc_y, li_stc_w, li_stc_h,  li_str_x, li_str_y, li_str_w, li_str_h}
 li_init_xywh_pb[] = {li_pba_x, li_pba_y, li_pba_w, li_pba_h,  li_pbk_x, li_pbk_y, li_pbk_w, li_pbk_h, li_pbc_x, li_pbc_y, li_pbc_w, li_pbc_h,  li_pbr_x, li_pbr_y, li_pbr_w, li_pbr_h}

inv_dashboard.nf_init_position_size(li_init_xywh_dw[], li_init_xywh_rr[], li_init_xywh_st[], li_init_xywh_pb[])
end subroutine

public subroutine wf_get_active_claim (long al_individual_no);LONG ll_claim_no, ll_days, ll_weeks
INT li_opening
DATE ldt_benefit_start_date
STRING ls_history

SELECT a.claim_no
INTO     :ll_claim_no
FROM    CLAIM a 
WHERE  a.individual_no = :al_individual_no
AND       a.accident_date = (SELECT MAX(b.accident_date)
                                         FROM     CLAIM b
								     WHERE   a.individual_no = b.individual_no
								     AND       b.claim_status_code = 'A')
USING SQLCA;

SQLCA.nf_handle_error('w_dashboard','wf_get_active_claim','SELECT a.claim_no FROM CLAIM')
						
IF ll_claim_no <= 0 OR IsNull(ll_claim_no) THEN
	ll_claim_no = 0
	st_claim.Visible = FALSE
	st_claim_no.Visible = FALSE
	st_current.Visible = FALSE
	st_week.Visible = FALSE
	rr_current.Visible = FALSE
ELSE
	st_claim.Visible = TRUE
	st_claim_no.Visible = TRUE
	st_current.Visible = TRUE
	st_week.Visible = TRUE
	rr_current.Visible = TRUE
	st_claim_no.Text = STRING(ll_claim_no)

	li_opening = wf_get_opening(ll_claim_no)
	
	IF li_opening > 0 Then
		SELECT benefit_start_date
		INTO   :ldt_benefit_start_date
		FROM   OPENING
		WHERE claim_no = :ll_claim_no
		AND     opening_no = :li_opening
		USING  SQLCA;
		
		SQLCA.nf_handle_error('w_dashboard','wf_get_active_claim','SELECT benefit_start_date')
	
		ll_days = DaysAfter(Date(ldt_benefit_start_date), Date(f_server_datetime()))

	END IF

	IF ll_days > 0 THEN
		ll_weeks = ll_days / 7 + 1
		st_week.Text = 'Week ' + STRING(ll_weeks)
	ELSE
		st_week.Visible = FALSE
	END IF

END IF




end subroutine

public subroutine wf_set_individual_no (long al_individual_no);il_individual_no = al_individual_no
end subroutine

public function long wf_get_opening (long al_claim_no);INTEGER		li_opening_no

SELECT opening_no into :li_opening_no 
FROM OPENING
WHERE claim_no = :al_claim_no
AND     opening_type_code = 'RLOE'
AND     recurrence_type_code = 'R'
AND      benefit_end_date IS NULL
USING SQLCA;

SQLCA.nf_handle_error('w_dashboard','wf_get_opening','SELECT opening_no')

IF li_opening_no > 0 THEN 	
	RETURN li_opening_no
END IF

SELECT opening_no 
INTO    :li_opening_no
FROM   OPENING
WHERE claim_no = :al_claim_no
AND      opening_type_code = 'RLOE'
AND      recurrence_type_code = 'R'
AND      benefit_end_date = (SELECT MAX(benefit_end_date) 
									FROM OPENING 
									WHERE claim_no = :al_claim_no
									AND     opening_type_code = 'RLOE'
									AND     recurrence_type_code = 'R'
									AND     benefit_end_date IS NOT NULL)
USING SQLCA;

SQLCA.nf_handle_error('w_dashboard','nf_get_opening','SELECT opening_no')

RETURN li_opening_no
end function

public subroutine wf_expand ();LONG ll_cntr

IF ib_claims_expanded THEN

//IF pb_claims.Text = '- see less...' THEN
	dw_claims.SetRedraw(FALSE)
	FOR ll_cntr = 1 to dw_claims.RowCount()
		dw_claims.ExpandAllChildren(ll_cntr, 1)
	NEXT 
	dw_claims.SetRedraw(TRUE)
END IF

end subroutine

public subroutine wf_setresize (boolean ab_switch);

IF ab_switch = TRUE THEN
	/* default instance of the resize object */
	IF IsNull(inv_resize) OR NOT IsValid (inv_resize) THEN
		inv_resize = create n_resize	
	END IF 
	inv_resize.of_SetOrigSize (il_design_time_width, il_design_time_height)
	inv_resize.of_SetMinSize (4855 , 2756)

ELSE
	DESTROY inv_resize
END IF
end subroutine

public function long wf_get_sheet_handle ();RETURN il_sheet_handle
end function

public subroutine wf_refresh (long al_individual_no, long al_sheet_handle);
w_dashboard.SetRedraw(FALSE)
wf_retrieve_dashboard(al_individual_no)
w_dashboard.SetRedraw(TRUE)
wf_get_active_claim(al_individual_no)

IF al_sheet_handle > 0 THEN
	il_sheet_handle = al_sheet_handle
END IF

cbx_alerts.Checked = FALSE
cbx_recent.Checked = FALSE
cbx_keys.Checked = FALSE
cbx_claims.Checked = FALSE
cbx_all.Checked = TRUE
end subroutine

public subroutine wf_set_sheet_handle (long al_sheet_handle);il_sheet_handle = al_sheet_handle
end subroutine

on w_dashboard.create
int iCurrent
call super::create
this.dw_report=create dw_report
this.st_claim_no=create st_claim_no
this.st_claim=create st_claim
this.st_filter_heading=create st_filter_heading
this.st_filter_claims=create st_filter_claims
this.uo_filter=create uo_filter
this.cbx_all=create cbx_all
this.cbx_recent=create cbx_recent
this.cbx_claims=create cbx_claims
this.cbx_keys=create cbx_keys
this.cbx_alerts=create cbx_alerts
this.pb_print=create pb_print
this.tv_filter=create tv_filter
this.pb_refresh=create pb_refresh
this.dw_header=create dw_header
this.st_4=create st_4
this.dw_profile=create dw_profile
this.pb_alerts=create pb_alerts
this.pb_claims=create pb_claims
this.st_keys=create st_keys
this.st_claims=create st_claims
this.st_6=create st_6
this.st_alerts=create st_alerts
this.tv_2=create tv_2
this.tv_1=create tv_1
this.st_3=create st_3
this.st_2=create st_2
this.st_1=create st_1
this.rr_alerts=create rr_alerts
this.rr_claims=create rr_claims
this.rr_keys=create rr_keys
this.pb_keys=create pb_keys
this.st_week=create st_week
this.st_current=create st_current
this.rr_recent=create rr_recent
this.st_recent=create st_recent
this.pb_recent=create pb_recent
this.gb_1=create gb_1
this.rr_current=create rr_current
this.pb_calendar_area4=create pb_calendar_area4
this.gb_2=create gb_2
this.dw_claims=create dw_claims
this.dw_recent=create dw_recent
this.dw_keys=create dw_keys
this.dw_alerts=create dw_alerts
this.st_filter_recent=create st_filter_recent
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_report
this.Control[iCurrent+2]=this.st_claim_no
this.Control[iCurrent+3]=this.st_claim
this.Control[iCurrent+4]=this.st_filter_heading
this.Control[iCurrent+5]=this.st_filter_claims
this.Control[iCurrent+6]=this.uo_filter
this.Control[iCurrent+7]=this.cbx_all
this.Control[iCurrent+8]=this.cbx_recent
this.Control[iCurrent+9]=this.cbx_claims
this.Control[iCurrent+10]=this.cbx_keys
this.Control[iCurrent+11]=this.cbx_alerts
this.Control[iCurrent+12]=this.pb_print
this.Control[iCurrent+13]=this.tv_filter
this.Control[iCurrent+14]=this.pb_refresh
this.Control[iCurrent+15]=this.dw_header
this.Control[iCurrent+16]=this.st_4
this.Control[iCurrent+17]=this.dw_profile
this.Control[iCurrent+18]=this.pb_alerts
this.Control[iCurrent+19]=this.pb_claims
this.Control[iCurrent+20]=this.st_keys
this.Control[iCurrent+21]=this.st_claims
this.Control[iCurrent+22]=this.st_6
this.Control[iCurrent+23]=this.st_alerts
this.Control[iCurrent+24]=this.tv_2
this.Control[iCurrent+25]=this.tv_1
this.Control[iCurrent+26]=this.st_3
this.Control[iCurrent+27]=this.st_2
this.Control[iCurrent+28]=this.st_1
this.Control[iCurrent+29]=this.rr_alerts
this.Control[iCurrent+30]=this.rr_claims
this.Control[iCurrent+31]=this.rr_keys
this.Control[iCurrent+32]=this.pb_keys
this.Control[iCurrent+33]=this.st_week
this.Control[iCurrent+34]=this.st_current
this.Control[iCurrent+35]=this.rr_recent
this.Control[iCurrent+36]=this.st_recent
this.Control[iCurrent+37]=this.pb_recent
this.Control[iCurrent+38]=this.gb_1
this.Control[iCurrent+39]=this.rr_current
this.Control[iCurrent+40]=this.pb_calendar_area4
this.Control[iCurrent+41]=this.gb_2
this.Control[iCurrent+42]=this.dw_claims
this.Control[iCurrent+43]=this.dw_recent
this.Control[iCurrent+44]=this.dw_keys
this.Control[iCurrent+45]=this.dw_alerts
this.Control[iCurrent+46]=this.st_filter_recent
end on

on w_dashboard.destroy
call super::destroy
destroy(this.dw_report)
destroy(this.st_claim_no)
destroy(this.st_claim)
destroy(this.st_filter_heading)
destroy(this.st_filter_claims)
destroy(this.uo_filter)
destroy(this.cbx_all)
destroy(this.cbx_recent)
destroy(this.cbx_claims)
destroy(this.cbx_keys)
destroy(this.cbx_alerts)
destroy(this.pb_print)
destroy(this.tv_filter)
destroy(this.pb_refresh)
destroy(this.dw_header)
destroy(this.st_4)
destroy(this.dw_profile)
destroy(this.pb_alerts)
destroy(this.pb_claims)
destroy(this.st_keys)
destroy(this.st_claims)
destroy(this.st_6)
destroy(this.st_alerts)
destroy(this.tv_2)
destroy(this.tv_1)
destroy(this.st_3)
destroy(this.st_2)
destroy(this.st_1)
destroy(this.rr_alerts)
destroy(this.rr_claims)
destroy(this.rr_keys)
destroy(this.pb_keys)
destroy(this.st_week)
destroy(this.st_current)
destroy(this.rr_recent)
destroy(this.st_recent)
destroy(this.pb_recent)
destroy(this.gb_1)
destroy(this.rr_current)
destroy(this.pb_calendar_area4)
destroy(this.gb_2)
destroy(this.dw_claims)
destroy(this.dw_recent)
destroy(this.dw_keys)
destroy(this.dw_alerts)
destroy(this.st_filter_recent)
end on

event open;s_window_message lstr_message
LONG ll_claim_no, ll_individual_no
STRING ls_set_settings
n_user_setting lnvo_user_setting

lstr_message = Message.PowerObjectParm

ll_individual_no = lstr_message.al_doubleparm[1]
il_sheet_handle  = lstr_message.al_doubleparm[2]

N_OBJECTHELPER lnv_object_helper
inv_dashboard  = CREATE n_dashboard
inv_resize = CREATE n_resize

wf_load_objects()

inv_dashboard.nf_create_destroy_datastores('CREATE')

inv_dashboard.nf_reset_position()
wf_retrieve_dashboard(ll_individual_no)
wf_get_active_claim(ll_individual_no)

f_populate_app_log(gs_appname_and_handle , lnv_object_helper.get_app_component_type(THIS), lnv_object_helper.getpath(THIS), 'Open Individual Dashboard - individual ' + string( il_individual_no ) )

dw_claims.uf_setfilter(TRUE)
dw_recent.uf_setfilter(TRUE)

//resizing not working quite right, will look at it at a later time

//This.wf_setresize(true)
//
//inv_resize.of_register(st_keys, 'ScaleToRight')
//inv_resize.of_register(rr_keys, 'ScaleToRight')
//inv_resize.of_register(dw_keys, 'ScaleToRight')
//
//inv_resize.of_register(st_filter_claims, 'ScaleToBottom')
//
/* set the registry settings if applicable */
IF vgst_user_profile.registry_viewer_setting_flag = "Y" THEN 
	lnvo_user_setting = create n_user_setting
    lnvo_user_setting.of_setrequestor(this)
	lnvo_user_setting.of_restore(gs_view_setting_directory + gs_appname + "\" + THIS.Title + "\")
END IF


end event

event closequery;String ls_setting_string
n_user_setting lnvo_user_setting

inv_dashboard.nf_create_destroy_datastores('DESTROY')

IF This.windowstate = Normal! AND vgst_user_profile.registry_viewer_setting_flag = "Y" AND TRIM(gs_appname) <> "" THEN
	// check to see which application this is being called from and set accordingly to that
	ls_setting_string = gs_view_setting_directory + gs_appname + "\" + THIS.Title + "\"

	lnvo_user_setting = CREATE n_user_setting
	lnvo_user_setting.of_setrequestor(This)
	lnvo_user_setting.of_setpossize()
	lnvo_user_setting.of_save(TRUE, ls_setting_string, '')
END IF 
end event

type dw_report from datawindow within w_dashboard
boolean visible = false
integer x = 50
integer y = 1504
integer width = 690
integer height = 400
integer taborder = 30
string title = "none"
string dataobject = "d_dashboard_report"
boolean border = false
boolean livescroll = true
end type

type st_claim_no from statictext within w_dashboard
integer x = 5719
integer y = 224
integer width = 457
integer height = 108
integer textsize = -14
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Gill Sans MT"
long textcolor = 134217730
long backcolor = 134217742
boolean focusrectangle = false
end type

type st_claim from statictext within w_dashboard
integer x = 5271
integer y = 224
integer width = 457
integer height = 108
integer textsize = -14
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Gill Sans MT"
long textcolor = 134217730
long backcolor = 134217742
string text = " Claim No."
boolean focusrectangle = false
end type

type st_filter_heading from statictext within w_dashboard
integer x = 59
integer y = 2508
integer width = 1033
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Gill Sans MT"
long textcolor = 27940909
long backcolor = 134217742
boolean focusrectangle = false
end type

type st_filter_claims from statictext within w_dashboard
integer x = 59
integer y = 2588
integer width = 1038
integer height = 732
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Gill Sans MT"
long textcolor = 27940909
long backcolor = 134217742
boolean focusrectangle = false
end type

type uo_filter from u_new_filter_control within w_dashboard
integer x = 69
integer y = 2320
integer taborder = 30
end type

on uo_filter.destroy
call u_new_filter_control::destroy
end on

event ue_filter_changed;call super::ue_filter_changed;LONG ll_cntr
N_OBJECTHELPER lnv_object_helper
f_populate_app_log(gs_appname_and_handle ,1100, lnv_object_helper.getpath(THIS), 'Filtering ' + is_filtering_dw + ' in Individual Dashboard - individual ' + string( il_individual_no ) )

st_filter_heading.Text = "Filter for " + is_filtering_dw

IF is_filtering_dw = 'Recent Activity' THEN
	st_filter_recent.Text = ls_new_filter
	IF st_filter_recent.Text = '' THEN st_filter_heading.Text = ''
	st_filter_recent.Visible = TRUE
	st_filter_claims.Visible = FALSE
ELSE
	st_filter_claims.Text = ls_new_filter
	IF st_filter_claims.Text = '' THEN st_filter_heading.Text = ''
	st_filter_recent.Visible = FALSE
	st_filter_claims.Visible = TRUE
END IF

wf_expand()

end event

event ue_retrieve;call super::ue_retrieve;IF adw_datawindow.DataObject = 'd_recent_activity' THEN
	adw_datawindow.Reset()
	inv_dashboard.nf_load_recent(adw_datawindow)
ELSE
	adw_datawindow.Retrieve(il_individual_no)
END IF



end event

type cbx_all from checkbox within w_dashboard
string tag = "All"
integer x = 581
integer y = 1796
integer width = 206
integer height = 96
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Gill Sans MT"
boolean italic = true
long textcolor = 27940909
long backcolor = 134217742
string text = "All"
boolean checked = true
end type

event clicked;cbx_alerts.Checked = FALSE
cbx_keys.Checked = FALSE
cbx_claims.Checked = FALSE
cbx_recent.Checked = FALSE
end event

type cbx_recent from checkbox within w_dashboard
string tag = "Recent"
integer x = 581
integer y = 1700
integer width = 475
integer height = 96
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Gill Sans MT"
boolean italic = true
long textcolor = 27940909
long backcolor = 134217742
string text = "Recent Activity"
end type

event clicked;cbx_all.Checked = FALSE
end event

type cbx_claims from checkbox within w_dashboard
string tag = "Claims"
integer x = 105
integer y = 1900
integer width = 466
integer height = 96
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Gill Sans MT"
boolean italic = true
long textcolor = 27940909
long backcolor = 134217742
string text = "Claim History"
end type

event clicked;cbx_all.Checked = FALSE
end event

type cbx_keys from checkbox within w_dashboard
string tag = "Keys"
integer x = 105
integer y = 1796
integer width = 453
integer height = 96
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Gill Sans MT"
boolean italic = true
long textcolor = 27940909
long backcolor = 134217742
string text = "Information"
end type

event clicked;cbx_all.Checked = FALSE
end event

type cbx_alerts from checkbox within w_dashboard
string tag = "Alerts"
integer x = 105
integer y = 1700
integer width = 457
integer height = 96
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Gill Sans MT"
boolean italic = true
long textcolor = 27940909
long backcolor = 134217742
string text = "Alerts"
end type

event clicked;cbx_all.Checked = FALSE
end event

type pb_print from picturebutton within w_dashboard
integer x = 50
integer y = 2080
integer width = 1061
integer height = 108
integer taborder = 30
integer transparency = 30
integer textsize = -11
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Gill Sans MT"
string text = "Print"
boolean originalsize = true
long textcolor = 27940909
long backcolor = 33222891
end type

event clicked;DATAWINDOWCHILD ldwc_keys_report, ldwc_recent_report
N_OBJECTHELPER lnv_object_helper
f_populate_app_log(gs_appname_and_handle ,300, lnv_object_helper.getpath(THIS), 'Print Individual Dashboard Report(s) - individual ' + string( il_individual_no ) )


dw_report.Object.dw_alerts.Visible = FALSE
dw_report.Object.dw_keys.Visible = FALSE
dw_report.Object.dw_claims.Visible = FALSE
dw_report.Object.dw_recent.Visible = FALSE

dw_report.SetTransObject(SQLCA)
dw_report.Retrieve(il_individual_no)

dw_report.GetChild("dw_keys", ldwc_keys_report)
dw_report.GetChild("dw_recent", ldwc_recent_report)
dw_keys.ShareData(ldwc_keys_report)
dw_recent.ShareData(ldwc_recent_report)

IF cbx_alerts.Checked OR (cbx_all.Checked AND cbx_alerts.Enabled) THEN dw_report.Object.dw_alerts.Visible = TRUE
IF cbx_keys.Checked OR (cbx_all.Checked AND cbx_keys.Enabled) THEN dw_report.Object.dw_keys.Visible = TRUE
IF cbx_claims.Checked OR (cbx_all.Checked AND cbx_claims.Enabled) THEN dw_report.Object.dw_claims.Visible = TRUE
IF cbx_recent.Checked OR (cbx_all.Checked AND cbx_recent.Enabled) THEN dw_report.Object.dw_recent.Visible = TRUE

dw_report.Print()


end event

type tv_filter from treeview within w_dashboard
boolean visible = false
integer x = 73
integer y = 2060
integer width = 1033
integer height = 1244
integer taborder = 90
integer textsize = -11
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Gill Sans MT"
long backcolor = 16777215
boolean border = false
integer indent = 40
boolean hideselection = false
boolean tooltips = false
boolean fullrowselect = true
long picturemaskcolor = 536870912
long statepicturemaskcolor = 536870912
end type

type pb_refresh from picturebutton within w_dashboard
integer x = 50
integer y = 1420
integer width = 1061
integer height = 108
integer taborder = 20
integer transparency = 30
integer textsize = -11
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Gill Sans MT"
string text = "Refresh"
boolean originalsize = true
vtextalign vtextalign = vcenter!
long textcolor = 27940909
long backcolor = 33222891
end type

event clicked;wf_refresh(il_individual_no, 0)
end event

type dw_header from datawindow within w_dashboard
integer x = 64
integer y = 60
integer width = 3657
integer height = 272
integer taborder = 10
string title = "none"
string dataobject = "d_individual_dashboard_profile_name"
boolean border = false
boolean livescroll = true
end type

type st_4 from statictext within w_dashboard
boolean visible = false
integer x = 91
integer y = 348
integer width = 1019
integer height = 124
integer textsize = -14
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Gill Sans MT"
long textcolor = 16777215
long backcolor = 12632256
boolean focusrectangle = false
end type

type dw_profile from datawindow within w_dashboard
integer x = 50
integer y = 412
integer width = 1051
integer height = 992
integer taborder = 60
string title = "none"
string dataobject = "d_individual_dashboard_profile"
boolean border = false
boolean livescroll = true
end type

type pb_alerts from picturebutton within w_dashboard
integer x = 1166
integer y = 1156
integer width = 3575
integer height = 108
integer taborder = 90
integer transparency = 30
integer textsize = -11
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Gill Sans MT"
string text = "+ see more..."
boolean originalsize = true
alignment htextalign = left!
long textcolor = 128
long backcolor = 32567551
end type

event clicked;STRING ls_method
IF THIS.Text = '+ see more...' THEN
	THIS.Text = '- see less...'
	ls_method = 'X'
ELSE
	THIS.Text = '+ see more...'
	ls_method = 'C'
END IF

inv_dashboard.nf_expand_collapse(1, ls_method)



end event

type pb_claims from picturebutton within w_dashboard
integer x = 1166
integer y = 2256
integer width = 3575
integer height = 108
integer taborder = 80
integer transparency = 30
integer textsize = -11
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Gill Sans MT"
string text = "+ see more..."
boolean originalsize = true
alignment htextalign = left!
long textcolor = 21663449
long backcolor = 32240378
end type

event clicked;STRING ls_method
LONG ll_cntr
N_OBJECTHELPER lnv_object_helper
f_populate_app_log(gs_appname_and_handle, 300, lnv_object_helper.getpath(THIS), 'See More Claim History (Individual Dashboard) - individual ' + string( il_individual_no ) )

IF THIS.Text = '+ see more...' THEN
	THIS.Text = '- see less...'
	dw_claims.SetRedraw(FALSE)
	FOR ll_cntr = 1 to dw_claims.RowCount()
		dw_claims.ExpandAllChildren(ll_cntr, 1)
	NEXT 
	dw_claims.SetRedraw(TRUE)
	ib_claims_expanded = TRUE
	ls_method = 'X'
ELSE
	THIS.Text = '+ see more...'
	dw_claims.SetRedraw(FALSE)
	FOR ll_cntr = 1 to dw_claims.RowCount()
		dw_claims.CollapseAllChildren(ll_cntr, 1)
	NEXT 
	dw_claims.SetRedraw(TRUE)
	
	ib_claims_expanded = FALSE
	ls_method = 'C'
END IF

inv_dashboard.nf_expand_collapse(3, ls_method)


end event

type st_keys from statictext within w_dashboard
integer x = 4818
integer y = 464
integer width = 1728
integer height = 124
integer textsize = -14
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Gill Sans MT"
long textcolor = 16777215
long backcolor = 8421376
string text = "  Information"
boolean focusrectangle = false
end type

type st_claims from statictext within w_dashboard
integer x = 1166
integer y = 1348
integer width = 3575
integer height = 124
integer textsize = -14
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Gill Sans MT"
long textcolor = 16777215
long backcolor = 21663449
string text = "  Claim History                 "
boolean focusrectangle = false
end type

type st_6 from statictext within w_dashboard
boolean visible = false
integer x = 123
integer y = 3116
integer width = 229
integer height = 128
boolean bringtotop = true
integer textsize = -20
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Gill Sans MT"
long textcolor = 128
long backcolor = 553648127
string text = "5"
boolean focusrectangle = false
end type

type st_alerts from statictext within w_dashboard
integer x = 1166
integer y = 464
integer width = 3575
integer height = 124
integer textsize = -14
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Gill Sans MT"
long textcolor = 16777215
long backcolor = 128
string text = "  Alerts"
boolean focusrectangle = false
end type

type tv_2 from treeview within w_dashboard
boolean visible = false
integer x = 55
integer y = 948
integer width = 818
integer height = 2060
integer taborder = 50
integer textsize = -14
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Gill Sans MT"
long textcolor = 1073741824
borderstyle borderstyle = stylelowered!
long picturemaskcolor = 536870912
long statepicturemaskcolor = 536870912
end type

type tv_1 from treeview within w_dashboard
boolean visible = false
integer x = 123
integer y = 428
integer width = 741
integer height = 2256
integer taborder = 40
integer textsize = -14
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Gill Sans MT"
long textcolor = 8388608
long backcolor = 30838350
borderstyle borderstyle = stylelowered!
long picturemaskcolor = 536870912
long statepicturemaskcolor = 536870912
end type

type st_3 from statictext within w_dashboard
boolean visible = false
integer x = 2528
integer y = 3132
integer width = 393
integer height = 100
boolean bringtotop = true
integer textsize = -14
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Gill Sans MT"
long textcolor = 15780518
long backcolor = 553648127
string text = "Recent"
boolean focusrectangle = false
end type

type st_2 from statictext within w_dashboard
boolean visible = false
integer x = 2162
integer y = 3196
integer width = 283
integer height = 92
boolean bringtotop = true
integer textsize = -14
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Gill Sans MT"
long textcolor = 8388736
long backcolor = 553648127
string text = "ppi"
boolean focusrectangle = false
end type

type st_1 from statictext within w_dashboard
boolean visible = false
integer x = 1225
integer y = 3180
integer width = 389
integer height = 124
boolean bringtotop = true
integer textsize = -18
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Gill Sans MT"
long textcolor = 18897563
long backcolor = 553648127
string text = "claims"
boolean focusrectangle = false
end type

type rr_alerts from roundrectangle within w_dashboard
string tag = "Alerts"
long fillcolor = 1073741824
integer x = 1134
integer y = 436
integer width = 3634
integer height = 860
integer cornerheight = 40
integer cornerwidth = 46
end type

type rr_claims from roundrectangle within w_dashboard
string tag = "Claims"
integer linethickness = 1
long fillcolor = 1073741824
integer x = 1134
integer y = 1316
integer width = 3634
integer height = 1068
integer cornerheight = 40
integer cornerwidth = 46
end type

type rr_keys from roundrectangle within w_dashboard
string tag = "Keys"
integer linethickness = 1
long fillcolor = 1073741824
integer x = 4791
integer y = 436
integer width = 1778
integer height = 2884
integer cornerheight = 40
integer cornerwidth = 46
end type

type pb_keys from picturebutton within w_dashboard
boolean visible = false
integer x = 4818
integer y = 3180
integer width = 1234
integer height = 112
integer taborder = 70
integer transparency = 30
integer textsize = -11
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Gill Sans MT"
string text = "+ see more..."
boolean originalsize = true
alignment htextalign = left!
long textcolor = 8421376
long backcolor = 32504032
end type

event clicked;STRING ls_method

IF THIS.Text = '+ see more...' THEN
	THIS.Text = '- see less...'
	ls_method = 'X'
ELSE
	THIS.Text = '+ see more...'
	ls_method = 'C'
END IF

inv_dashboard.nf_expand_collapse(2, ls_method)
end event

type st_week from statictext within w_dashboard
integer x = 4763
integer y = 220
integer width = 494
integer height = 116
boolean bringtotop = true
integer textsize = -16
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Gill Sans MT"
boolean italic = true
long textcolor = 21663449
long backcolor = 134217742
string text = "Week 33"
boolean focusrectangle = false
end type

type st_current from statictext within w_dashboard
integer x = 4690
integer y = 92
integer width = 1376
integer height = 112
boolean bringtotop = true
integer textsize = -18
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Gill Sans MT"
long textcolor = 27940909
long backcolor = 134217742
string text = "Most Recent Active Claim"
boolean focusrectangle = false
end type

type rr_recent from roundrectangle within w_dashboard
string tag = "Recent"
integer linethickness = 1
long fillcolor = 1073741824
integer x = 1134
integer y = 2412
integer width = 3634
integer height = 912
integer cornerheight = 40
integer cornerwidth = 46
end type

type st_recent from statictext within w_dashboard
integer x = 1166
integer y = 2436
integer width = 3575
integer height = 124
boolean bringtotop = true
integer textsize = -14
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Gill Sans MT"
long textcolor = 16777215
long backcolor = 27940909
string text = "  Most Recent Activity"
boolean focusrectangle = false
end type

type pb_recent from picturebutton within w_dashboard
integer x = 1166
integer y = 3188
integer width = 3575
integer height = 108
integer taborder = 90
boolean bringtotop = true
integer transparency = 30
integer textsize = -11
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Gill Sans MT"
string text = "+ see more..."
boolean originalsize = true
alignment htextalign = left!
long textcolor = 27940909
long backcolor = 33222891
end type

event clicked;STRING ls_method
IF THIS.Text = '+ see more...' THEN
	THIS.Text = '- see less...'
	ls_method = 'X'
ELSE
	THIS.Text = '+ see more...'
	ls_method = 'C'
END IF

inv_dashboard.nf_expand_collapse(4, ls_method)


end event

type gb_1 from groupbox within w_dashboard
integer x = 50
integer y = 1564
integer width = 1047
integer height = 500
integer taborder = 70
integer textsize = -11
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Gill Sans MT"
long textcolor = 22188753
long backcolor = 134217742
string text = "Print Options"
end type

type rr_current from roundrectangle within w_dashboard
long linecolor = 33222891
integer linethickness = 9
long fillcolor = 1073741824
integer x = 4544
integer y = 44
integer width = 1669
integer height = 356
integer cornerheight = 40
integer cornerwidth = 46
end type

type pb_calendar_area4 from picturebutton within w_dashboard
integer x = 4585
integer y = 2448
integer width = 128
integer height = 96
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Gill Sans MT"
string picturename = "calendar_16.gif"
alignment htextalign = left!
end type

event clicked;N_OBJECTHELPER lnv_object_helper
f_populate_app_log(gs_appname_and_handle ,300, lnv_object_helper.getpath(THIS), 'Open Calendar in Individual Dashboard - individual ' + string( il_individual_no ) )

OpenWithParm(w_calendar, il_individual_no)

end event

type gb_2 from groupbox within w_dashboard
integer x = 50
integer y = 2228
integer width = 1047
integer height = 240
integer taborder = 80
integer textsize = -11
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Gill Sans MT"
long textcolor = 22188753
long backcolor = 134217742
string text = "Filter Options"
end type

type dw_claims from u_dw_online within w_dashboard
string tag = "Claim History"
integer x = 1166
integer y = 1468
integer width = 3575
integer height = 788
integer taborder = 100
string dataobject = "d_claim_summary"
boolean vscrollbar = true
boolean border = false
end type

event getfocus;call super::getfocus;LONG ll_cntr
STRING ls_filter

uo_filter.uf_set_Requestor(THIS)
is_filtering_dw = THIS.Tag
st_filter_claims.Visible = TRUE
st_filter_recent.Visible = FALSE
IF st_filter_claims.Text > '' AND st_filter_claims.Visible  THEN
	st_filter_heading.Text = "Filter for " + is_filtering_dw
END IF

end event

event expanded;call super::expanded;LONG ll_tvi

IF grouplevel = 1 THEN
	THIS.ExpandLevel(2)
	ib_claims_expanded = TRUE
END IF
end event

event retrieveend;call super::retrieveend;wf_expand()
IF rowcount > 0 THEN
	uo_filter.uf_set_Requestor(THIS)
END IF

st_claims.Text = '  Claim History (' + STRING(rowcount) + ')'


end event

event collapsed;call super::collapsed;ib_claims_expanded = FALSE


end event

type dw_recent from u_dw_online within w_dashboard
string tag = "Recent Activity"
integer x = 1166
integer y = 2556
integer width = 3575
integer height = 636
integer taborder = 80
string dataobject = "d_recent_activity"
boolean vscrollbar = true
boolean border = false
end type

event getfocus;call super::getfocus;LONG ll_cntr
STRING ls_filter

uo_filter.uf_set_Requestor(THIS)
is_filtering_dw = THIS.Tag
st_filter_recent.Visible = TRUE
st_filter_claims.Visible = FALSE
IF st_filter_recent.Text > '' AND st_filter_recent.Visible THEN	
	st_filter_heading.Text = "Filter for " + is_filtering_dw
END IF

end event

type dw_keys from u_dw_online within w_dashboard
string tag = "Information"
integer x = 4818
integer y = 580
integer width = 1728
integer height = 2704
integer taborder = 100
string dataobject = "d_keys"
boolean hscrollbar = true
boolean vscrollbar = true
boolean border = false
end type

type dw_alerts from u_dw_online within w_dashboard
string tag = "Alerts"
integer x = 1166
integer y = 580
integer width = 3575
integer height = 580
integer taborder = 90
string dataobject = "d_alerts"
boolean vscrollbar = true
boolean border = false
end type

event retrieveend;call super::retrieveend;st_alerts.Text = '  Alerts (' + STRING(rowcount) + ')'

end event

type st_filter_recent from statictext within w_dashboard
integer x = 96
integer y = 2588
integer width = 978
integer height = 704
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Gill Sans MT"
long textcolor = 27940909
long backcolor = 134217742
boolean focusrectangle = false
end type

