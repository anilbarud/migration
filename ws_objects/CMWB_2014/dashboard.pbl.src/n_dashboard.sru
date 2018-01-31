$PBExportHeader$n_dashboard.sru
forward
global type n_dashboard from n_pdc
end type
end forward

global type n_dashboard from n_pdc
end type
global n_dashboard n_dashboard

type variables

INT ii_areas[]
ROUNDRECTANGLE ir_rr[4]
STATICTEXT ist_st[4]
PICTUREBUTTON ipb_pb[4] , ipb_cal
U_DW_ONLINE idw_adw[4]
CHECKBOX icb_cb[4]
LONG il_rows[]

LONG il_individual_no
LONG il_ex_dw
STRING is_areas[], is_areas_for_filter[4,4], is_areas_not_displayed[]

U_DS ids_cppd, ids_elig, ids_ppi, ids_wr, ids_appeals, ids_cacl, ids_ann_status, ids_court, ids_surg, ids_modified, ids_ppd, ids_iro
U_DS ids_recent_payments, ids_recent_events, ids_recent_rx, ids_recent_tasks, ids_recent[], ids_recent_missed

INT ii_init_xywh_dw[4,4] 
INT ii_init_xywh_rr[4,4] 
INT ii_init_xywh_st[4,4] 
INT ii_init_xywh_pb[4,4]

INT ii_xywh_dw[4,4]  
INT ii_xywh_rr[4,4]  
INT ii_xywh_st[4,4]  
INT ii_xywh_pb[4,4] 

INT ii_pb_cal_y[4] = { 476, 476, 1360, 2448}
INT ii_pb_cal_hh_y = 1930

//Full width values
CONSTANT INT cii_fw_dw = 3575, cii_fw_rr = 3634, cii_fw_st = 3575, cii_fw_pb = 3575

//Full height values
CONSTANT INT cii_fh_dw = 2612, cii_fh_rr = 2880

//Half height values
CONSTANT INT cii_hh_dw = 1160 , cii_hh_rr = 1420, cii_hh_dwby = 2028, cii_hh_rrby = 1896
CONSTANT INT cii_hh_sty = 1920, cii_hh_pby = 1732, cii_hh_cal = 1935

BOOLEAN ib_key_information, ib_hh, ib_onscreen
end variables

forward prototypes
public subroutine nf_expand_collapse (integer ai_area, string as_method)
public subroutine nf_reassign_areas (long al_rows[])
public subroutine nf_visible (integer ai_area, boolean ab_true_false)
public subroutine nf_reset_position ()
public subroutine nf_initialize_areas (integer ai_areas[], u_dw_online adw_dw[], roundrectangle ar_rr[], statictext ast_static_texts[], picturebutton apb_picture_buttons[], picturebutton apb_cal, checkbox acb_cb[])
public subroutine nf_init_position_size (integer ai_dw[], integer ai_rr[], integer ai_st[], integer ai_pb[])
public subroutine nf_set_individual (long al_individual_no)
public function integer nf_retrieve ()
public subroutine nf_create_destroy_datastores (string as_mode)
public function long nf_load_information ()
public function integer nf_insert_info (string as_info, string as_detail)
public function integer nf_load_recent (datawindow adw_dw)
public function integer nf_calculate_age (date adt_birth, date adt_death)
end prototypes

public subroutine nf_expand_collapse (integer ai_area, string as_method);LONG ll_loop, ll_cntr

//nf_redraw(FALSE)

//If expanding
IF as_method = 'X' THEN
	idw_adw[ai_area].x = ii_xywh_dw[1, 1]
	idw_adw[ai_area].y = ii_xywh_dw[2, 1]
	idw_adw[ai_area].width = cii_fw_dw
	idw_adw[ai_area].height = cii_fh_dw
	
	ir_rr[ai_area].x = ii_xywh_rr[1, 1]
	ir_rr[ai_area].y = ii_xywh_rr[2, 1]
	ir_rr[ai_area].width = cii_fw_rr
	ir_rr[ai_area].height = cii_fh_rr
	
	ist_st[ai_area].x = ii_xywh_st[1, 1]
	ist_st[ai_area].y = ii_xywh_st[2, 1]
	ist_st[ai_area].width = cii_fw_st
	
	ipb_pb[ai_area].x = ii_xywh_pb[1, 4]
	ipb_pb[ai_area].y = ii_init_xywh_pb[2, 4]
	ipb_pb[ai_area].width = cii_fw_pb
							
	//make others invisible here
	FOR ll_loop = 1 to 4
		IF ll_loop = ai_area THEN
			CONTINUE
		END IF
		IF idw_adw[ll_loop].Visible = TRUE THEN
			ib_onscreen = TRUE
		ELSE
			ib_onscreen = FALSE
		END IF
		IF idw_adw[ll_loop].Tag <> 'Information' THEN
			nf_visible(ll_loop,FALSE)
		END IF
	NEXT 
	ipb_cal.Visible = FALSE
	
	IF idw_adw[ai_area].Tag = 'Recent Activity' THEN
		ipb_cal.y = ii_pb_cal_y[1]
		IF	idw_adw[ai_area].Visible THEN
			ipb_cal.Visible = TRUE
		ELSE
			ipb_cal.Visible = FALSE
		END IF
	END IF	

//If collapsing
ELSEIF as_method = 'C' THEN
	idw_adw[ai_area].x = ii_xywh_dw[1, ai_area]
	idw_adw[ai_area].y = ii_xywh_dw[2, ai_area]
	idw_adw[ai_area].width = ii_xywh_dw[3, ai_area]
	idw_adw[ai_area].height = ii_xywh_dw[4, ai_area]
	
	ir_rr[ai_area].x = ii_xywh_rr[1, ai_area]
	ir_rr[ai_area].y = ii_xywh_rr[2, ai_area]
	ir_rr[ai_area].width = ii_xywh_rr[3, ai_area]
	ir_rr[ai_area].height = ii_xywh_rr[4, ai_area]
	
	ist_st[ai_area].x = ii_xywh_st[1, ai_area]
	ist_st[ai_area].y = ii_xywh_st[2, ai_area]
	ist_st[ai_area].width = ii_xywh_st[3, ai_area]
	
	ipb_pb[ai_area].x = ii_xywh_pb[1, ai_area]
	ipb_pb[ai_area].y = ii_xywh_pb[2, ai_area]
	ipb_pb[ai_area].width = ii_xywh_pb[3, ai_area]
	
	FOR ll_loop = 1 to 4
		IF ll_loop = ai_area THEN
			CONTINUE
		END IF
		IF il_rows[ll_loop] > 0 THEN
			nf_visible(ll_loop,TRUE)
		END IF
	NEXT 


	FOR ll_cntr = 1 to UpperBound(is_areas[])
		IF is_areas[ll_cntr] = 'Recent' THEN
			IF ib_hh THEN
				ipb_cal.y = ii_pb_cal_hh_y
			ELSE
				ipb_cal.y = ii_pb_cal_y[ll_cntr]
			END IF
			IF idw_adw[ll_cntr].Visible THEN
				ipb_cal.Visible = TRUE
			ELSE
				ipb_cal.Visible = FALSE
			END IF
		END IF
	NEXT
	
END IF

//nf_redraw(TRUE)

end subroutine

public subroutine nf_reassign_areas (long al_rows[]);LONG ll_cntr

ib_hh = FALSE

IF al_rows[1] = 0 THEN
	//IF area 1 has no rows, expand area 2 to cover both areas
	IF al_rows[3] > 0 THEN
		nf_visible(3, TRUE)
		is_areas[1] = ir_rr[3].Tag
		is_areas[2] = ''
		is_areas[3] = ''
		//move 3 up to area 1
		idw_adw[3].y = ii_xywh_dw[2,1]
		ii_xywh_dw[2,3] = idw_adw[3].y
		
		idw_adw[3].height = cii_hh_dw
		ii_xywh_dw[4,3] = idw_adw[3].height
		
		ir_rr[3].y = ii_xywh_rr[2,1]
		ii_xywh_rr[2,3] = ir_rr[3].y
		
		ir_rr[3].height = cii_hh_rr
		ii_xywh_rr[4,3] = ir_rr[3].height
		
		ist_st[3].y = ii_xywh_st[2,1]
		ii_xywh_st[2,3] = ist_st[3].y
		
		ipb_pb[3].y = cii_hh_pby
		ii_xywh_pb[2,3] = ipb_pb[3].y
		
		IF al_rows[4] > 0 THEN
			ib_hh = TRUE
			//move 4 up to 3
			is_areas[3] = ir_rr[4].Tag
			is_areas[4] = ''
			
			idw_adw[4].y = cii_hh_dwby
			ii_xywh_dw[2,4] = idw_adw[4].y
			
			idw_adw[4].height = cii_hh_dw
			ii_xywh_dw[4,4] = idw_adw[4].height
			
			ir_rr[4].y = cii_hh_rrby
			ii_xywh_rr[2,4] = ir_rr[4].y
			
			ir_rr[4].height = cii_hh_rr
			ii_xywh_rr[4,4] = ir_rr[4].height
			
			ist_st[4].y = cii_hh_sty
			ii_xywh_st[2,4] = ist_st[4].y

			ipb_pb[4].y = idw_adw[4].y + idw_adw[4].height
			ii_xywh_pb[2,4] = ipb_pb[4].y
		ELSE
			idw_adw[3].SetRedraw(FALSE)
			FOR ll_cntr = 1 to idw_adw[3].RowCount()
				idw_adw[3].ExpandAllChildren(ll_cntr, 1)
			NEXT 
			idw_adw[3].SetRedraw(TRUE)
			nf_expand_collapse(3, 'X')
			ipb_pb[3].Text = '- see less...'
		END IF
	ELSE
		IF al_rows[4] > 0 THEN
			is_areas[1] = ir_rr[4].Tag
			is_areas[2] = ''
			is_areas[3] = ''
			is_areas[4] = ''

			nf_expand_collapse(4, 'X')
			ipb_pb[4].Text = '- see less...'
		END IF
	END IF
END IF


IF al_rows[3] = 0 THEN 
	///move 4 up to 3
	is_areas[3] = ir_rr[4].Tag
	is_areas[4] = ''
	idw_adw[4].x = ii_xywh_dw[1,3]
	ii_xywh_dw[1,4] = idw_adw[4].x
	
	ir_rr[4].x = ii_xywh_rr[1,3]
	ii_xywh_rr[1,4] = ir_rr[4].x
	
	ist_st[4].x = ii_xywh_st[1,3]
	ii_xywh_st[1,4] = ist_st[4].x
	
	ipb_pb[4].x = ii_xywh_pb[1,3]
	ii_xywh_pb[1,4] = ipb_pb[4].x
END IF

IF al_rows[4] = 0 THEN
	is_areas[4] = ''	
END IF

FOR ll_cntr = 1 to UpperBound(is_areas[])
	IF is_areas[ll_cntr] = 'Recent' THEN
		IF ib_hh THEN
			ipb_cal.y = ii_pb_cal_hh_y
		ELSE
			ipb_cal.y = ii_pb_cal_y[ll_cntr]
		END IF
	
		IF idw_adw[ll_cntr].Visible = TRUE THEN
			ipb_cal.Visible = TRUE
		ELSE
			ipb_cal.Visible = FALSE
		END IF
	END IF
NEXT

end subroutine

public subroutine nf_visible (integer ai_area, boolean ab_true_false);
IF idw_adw[ai_area].Tag <> 'Information' THEN
	idw_adw[ai_area].Visible = ab_true_false
	idw_adw[ai_area].Vscrollbar = ab_true_false
	ir_rr[ai_area].Visible = ab_true_false
	ist_st[ai_area].Visible = ab_true_false
	ipb_pb[ai_area].Visible = ab_true_false
END IF

IF idw_adw[ai_area].RowCount() > 0 THEN
	icb_cb[ai_area].Enabled = TRUE
ELSE
	icb_cb[ai_area].Enabled = FALSE
END IF

end subroutine

public subroutine nf_reset_position ();LONG ll_cntr, ll_loop

ii_xywh_dw = ii_init_xywh_dw
ii_xywh_rr = ii_init_xywh_rr
ii_xywh_st = ii_init_xywh_st
ii_xywh_pb = ii_init_xywh_pb

FOR ll_cntr = 1 to UpperBound(ii_areas[])

	idw_adw[ll_cntr].x = ii_init_xywh_dw[1, ll_cntr]
	idw_adw[ll_cntr].y = ii_init_xywh_dw[2, ll_cntr]
	idw_adw[ll_cntr].width = ii_init_xywh_dw[3, ll_cntr]
	idw_adw[ll_cntr].height = ii_init_xywh_dw[4, ll_cntr]
		
	ir_rr[ll_cntr].x = ii_init_xywh_rr[1, ll_cntr]
	ir_rr[ll_cntr].y = ii_init_xywh_rr[2, ll_cntr]
	ir_rr[ll_cntr].width = ii_init_xywh_rr[3, ll_cntr]
	ir_rr[ll_cntr].height = ii_init_xywh_rr[4, ll_cntr]
		
	ist_st[ll_cntr].x = ii_init_xywh_st[1, ll_cntr]
	ist_st[ll_cntr].y = ii_init_xywh_st[2, ll_cntr]
	ist_st[ll_cntr].width = ii_init_xywh_st[3, ll_cntr]
	ist_st[ll_cntr].height = ii_init_xywh_st[4, ll_cntr]
		
	ipb_pb[ll_cntr].x = ii_init_xywh_pb[1, ll_cntr]
	ipb_pb[ll_cntr].y = ii_init_xywh_pb[2, ll_cntr]
	ipb_pb[ll_cntr].width = ii_init_xywh_pb[3, ll_cntr]
	ipb_pb[ll_cntr].height = ii_init_xywh_pb[4, ll_cntr]
	
	 is_areas[ll_cntr] = ir_rr[ll_cntr].Tag
	
NEXT

end subroutine

public subroutine nf_initialize_areas (integer ai_areas[], u_dw_online adw_dw[], roundrectangle ar_rr[], statictext ast_static_texts[], picturebutton apb_picture_buttons[], picturebutton apb_cal, checkbox acb_cb[]);LONG ll_cntr
LONG ll_group = 1

//loading all of the "areas" and objects
DO WHILE ll_group <= UpperBound(ai_areas[])
	 ii_areas[ll_group] = ai_areas[ll_group]
	 idw_adw[ll_group] = adw_dw[ll_group]
	 ir_rr[ll_group] = ar_rr[ll_group]
	 ist_st[ll_group] = ast_static_texts[ll_group]
	 ipb_pb[ll_group] = apb_picture_buttons[ll_group] 
	 is_areas[ll_group] = ar_rr[ll_group].Tag
	 icb_cb[ll_group] = acb_cb[ll_group]
	 ll_group ++
LOOP
ipb_cal = apb_cal


end subroutine

public subroutine nf_init_position_size (integer ai_dw[], integer ai_rr[], integer ai_st[], integer ai_pb[]);//setting initial position/size values

ii_init_xywh_dw = ai_dw
ii_init_xywh_rr = ai_rr
ii_init_xywh_st = ai_st
ii_init_xywh_pb = ai_pb

ii_xywh_dw = ii_init_xywh_dw
ii_xywh_rr = ii_init_xywh_rr
ii_xywh_st = ii_init_xywh_st
ii_xywh_pb = ii_init_xywh_pb
end subroutine

public subroutine nf_set_individual (long al_individual_no);il_individual_no = al_individual_no
end subroutine

public function integer nf_retrieve ();LONG ll_loop, ll_move

nf_reset_position()

FOR ll_loop = 1 TO UpperBound(idw_adw[])
	idw_adw[ll_loop].SetTransObject(SQLCA)

	IF idw_adw[ll_loop].Tag = 'Information' THEN
		il_ex_dw = ll_loop
		idw_adw[ll_loop].Reset()
		il_rows[ll_loop] = nf_load_information()
		IF il_rows[ll_loop] > 0 THEN
			ib_key_information = TRUE
		ELSE
			ib_key_information = FALSE
		END IF
	ELSEIF idw_adw[ll_loop].Tag = 'Recent Activity' THEN
		idw_adw[ll_loop].Reset()
		il_rows[ll_loop] = nf_load_recent(idw_adw[ll_loop])		
	ELSE
		il_rows[ll_loop] = idw_adw[ll_loop].Retrieve(il_individual_no)
	END IF
	IF il_rows[ll_loop] = 0 THEN
		ll_move++
		nf_visible(ll_loop, FALSE) 
		CONTINUE  
	END IF
	nf_visible(ll_loop, TRUE)
NEXT

IF ll_move < UpperBound(idw_adw[]) THEN	
	nf_reassign_areas(il_rows[])
ELSE
	RETURN -1
END IF

RETURN 0
end function

public subroutine nf_create_destroy_datastores (string as_mode);
IF as_mode = 'CREATE' THEN
	ids_cppd = CREATE U_DS
	ids_elig = CREATE U_DS
	ids_ppi = CREATE U_DS
	ids_wr = CREATE U_DS
	ids_appeals = CREATE U_DS
	ids_cacl = CREATE U_DS
	ids_ann_status = CREATE U_DS
	ids_court = CREATE U_DS
	ids_surg = CREATE U_DS
	ids_modified = CREATE U_DS
	ids_ppd = CREATE U_DS
	ids_iro = CREATE U_DS
		
	ids_recent_payments = CREATE U_DS
	ids_recent_events = CREATE U_DS
	ids_recent_rx = CREATE U_DS
	ids_recent_tasks = CREATE U_DS
	ids_recent_missed = CREATE U_DS
	
	ids_cppd.DataObject = 'd_cppd_status'
	ids_cppd.SetTransObject(SQLCA)
	
	ids_elig.DataObject = 'd_elig'
	ids_elig.SetTransObject(SQLCA)
	
	ids_ppi.DataObject = 'd_ppi'
	ids_ppi.SetTransObject(SQLCA)
	
	ids_wr.DataObject = 'd_work_restriction'
	ids_wr.SetTransObject(SQLCA)
	
	ids_appeals.DataObject = 'd_appeals'
	ids_appeals.SetTransObject(SQLCA)
	
	ids_cacl.DataObject = 'd_cacl'
	ids_cacl.SetTransObject(SQLCA)
	
	ids_ann_status.DataObject = 'd_annuity_elig'
	ids_ann_status.SetTransObject(SQLCA)
	
	ids_court.DataObject = 'd_court_order'
	ids_court.SetTransObject(SQLCA)
	
	ids_surg.DataObject = 'd_surgeries'
	ids_surg.SetTransObject(SQLCA)
	
	ids_modified.DataObject = 'd_modified_duties'
	ids_modified.SetTransObject(SQLCA)
	
	ids_ppd.DataObject = 'd_ppd'
	ids_ppd.SetTransObject(SQLCA)
	
	ids_iro.DataObject = 'd_iro'
	ids_iro.SetTransObject(SQLCA)
	
	ids_recent_payments.DataObject = 'd_recent_payments'
	ids_recent_payments.SetTransObject(SQLCA)
	
	ids_recent_events.DataObject = 'd_recent_events'
	ids_recent_events.SetTransObject(SQLCA)
	
	ids_recent_rx.DataObject = 'd_recent_rx'
	ids_recent_rx.SetTransObject(SQLCA)
	
	ids_recent_tasks.DataObject = 'd_recent_tasks'
	ids_recent_tasks.SetTransObject(SQLCA)
	
	ids_recent_missed.DataObject = 'd_recent_missed'
	ids_recent_missed.SetTransObject(SQLCA)

ELSE
	DESTROY ids_cppd
	DESTROY ids_elig
	DESTROY ids_ppi
	DESTROY ids_wr
	DESTROY ids_appeals
	DESTROY ids_cacl
	DESTROY ids_ann_status
	DESTROY ids_court
	DESTROY ids_surg
	DESTROY ids_modified
	DESTROY ids_ppd
	DESTROY ids_iro

	DESTROY ids_recent_payments	
	DESTROY ids_recent_events
	DESTROY ids_recent_rx	
	DESTROY ids_recent_tasks 
	DESTROY ids_recent_missed

END IF
end subroutine

public function long nf_load_information ();LONG ll_row, ll_rows, ll_cntr, ll_claim_no, ll_percent, ll_event_no, ll_task_no, ll_info_rows, ll_cnt_rows, ll_hdr_row, ll_last_claim_no, ll_change, ll_last_event_no
STRING  ls_cppd_status, ls_date, ls_claim_no, ls_info, ls_percent, ls_groups, ls_event_no, ls_desc, ls_task_no, ls_last_group, ls_change, ls_claim_info
DATE ldt_cppd_start, ldt_date
BOOLEAN lb_new_group
DECIMAL {2} ldec_percent, ldec_change

////CPPD 

ll_rows = ids_cppd.Retrieve(il_individual_no)

IF ll_rows > 0 THEN
	ll_hdr_row = nf_insert_info('CPPD Status', 'N')
	FOR ll_cntr = 1 to ll_rows
		ll_claim_no = ids_cppd.GetItemNumber(ll_cntr, 'claim_no')
		IF IsNull(ll_claim_no) OR ll_claim_no = 0 THEN
			ls_claim_no = ''
		ELSE
			ls_claim_no = STRING(ll_claim_no)
		END IF
		ls_cppd_status = ids_cppd.GetItemString(ll_cntr, 'cppd_status_desc')
		ldt_date = DATE(ids_cppd.GetItemDateTime(ll_cntr, 'cppd_benefit_start_date'))
		IF IsNull(ldt_date) OR ldt_date = DATE('1900-01-01') THEN
			ls_date = ''
		ELSE
			ls_date = STRING(ldt_date, 'yyyy-mm-dd')
		END IF
		ls_info = "Claim No. " + ls_claim_no + ", "  + ls_cppd_status + " - " + ls_date
		ll_row = nf_insert_info(ls_info, 'Y')
		ll_cnt_rows = ll_cnt_rows + 1
	NEXT 
END IF

////Open Eligibility

ll_rows = ids_elig.Retrieve(il_individual_no)

IF ll_rows > 0 THEN

	FOR ll_cntr = 1 to ll_rows
		ls_groups = ids_elig.GetItemString(ll_cntr, 'groups')
		IF (ls_last_group = '') OR (ls_last_group > '' AND ls_groups <> ls_last_group) THEN
			ll_hdr_row = nf_insert_info(ls_groups, 'N')
			ls_last_group = ls_groups
			ll_last_claim_no = 0
			lb_new_group = TRUE
		END IF	
		ll_claim_no = ids_elig.GetItemNumber(ll_cntr, 'claim_no')
		IF IsNull(ll_claim_no) OR ll_claim_no = 0 THEN
			ls_claim_no = ''
		ELSE
			ls_claim_no = STRING(ll_claim_no)
			IF lb_new_group = FALSE AND ll_claim_no = ll_last_claim_no THEN
					CONTINUE
			END IF
			ll_last_claim_no = ll_claim_no
			ls_info = "Claim No. " + ls_claim_no
			ll_row = nf_insert_info(ls_info,'Y')
			ll_cnt_rows = ll_cnt_rows + 1
		END IF	
		lb_new_group = FALSE
	NEXT 

	
END IF

////PPI

ll_rows = ids_ppi.Retrieve(il_individual_no)

IF ll_rows > 0 THEN
	ll_hdr_row = nf_insert_info('PPI','N')
	FOR ll_cntr = 1 to ll_rows
		ll_claim_no = ids_ppi.GetItemNumber(ll_cntr, 'claim_no')
		IF IsNull(ll_claim_no) OR ll_claim_no = 0 THEN
			ls_claim_no = ''
		ELSE
			ls_claim_no = STRING(ll_claim_no)
		END IF
		ldec_percent = ids_ppi.GetItemNumber(ll_cntr, 'percent_impaired')
		ls_percent = STRING(ldec_percent)	+ ' %'
			
		ldt_date = DATE(ids_ppi.GetItemDateTime(ll_cntr, 'impaired_determination_date'))
		IF IsNull(ldt_date) OR ldt_date = DATE('1900-01-01') THEN
			ls_date = ''
		ELSE
			ls_date = STRING(ldt_date, 'yyyy-mm-dd')
		END IF
		ls_info = "Claim No. " + ls_claim_no + ", " + ls_percent + " - " + ls_date 
		ll_row = nf_insert_info(ls_info,'Y')
		ll_cnt_rows = ll_cnt_rows + 1

	NEXT 
END IF

////PPD

ll_rows = ids_ppd.Retrieve(il_individual_no)

IF ll_rows > 0 THEN
	ll_hdr_row = nf_insert_info('PPD','N')
	FOR ll_cntr = 1 to ll_rows
		ll_claim_no = ids_ppd.GetItemNumber(ll_cntr, 'claim_no')
		IF IsNull(ll_claim_no) OR ll_claim_no = 0 THEN
			ls_claim_no = ''
		ELSE
			ls_claim_no = STRING(ll_claim_no)
		END IF
		ldec_percent = ids_ppd.GetItemNumber(ll_cntr, 'percent_disability')
		ls_percent = STRING(ldec_percent)	+ ' %'
		
		ldec_change = ids_ppd.GetItemNumber(ll_cntr, 'percent_change_disability')
		ls_change = STRING(ldec_change) + ' %'
		
		ldt_date = DATE(ids_ppd.GetItemDateTime(ll_cntr, 'disability_determination_date'))
		IF IsNull(ldt_date) OR ldt_date = DATE('1900-01-01') THEN
			ls_date = ''
		ELSE
			ls_date = STRING(ldt_date, 'yyyy-mm-dd')
		END IF
		ls_info = "Claim No. " + ls_claim_no + ", " + ls_percent + " Disability, " + ls_change + " Change" + " - " + ls_date 
		ll_row = nf_insert_info(ls_info,'Y')
		ll_cnt_rows = ll_cnt_rows + 1

	NEXT 
END IF


////Work Restriction

ll_rows = ids_wr.Retrieve(il_individual_no)

IF ll_rows > 0 THEN
	ll_hdr_row = nf_insert_info('Work Restriction','N')
	FOR ll_cntr = 1 to ll_rows
		ll_claim_no = ids_wr.GetItemNumber(ll_cntr, 'opening_claim_no')
		IF IsNull(ll_claim_no) OR ll_claim_no = 0 THEN
			ls_claim_no = ''
		ELSE
			ls_claim_no = STRING(ll_claim_no)
		END IF
		
		ldt_date = DATE(ids_wr.GetItemDateTime(ll_cntr, 'opening_benefit_end_date'))
		IF IsNull(ldt_date) OR ldt_date = DATE('1900-01-01') THEN
			ls_date = ''
		ELSE
			ls_date = STRING(ldt_date, 'yyyy-mm-dd')
		END IF
		ls_info = "Claim No. " + ls_claim_no + " - " + ls_date 
		ll_row = nf_insert_info(ls_info,'Y')
		ll_cnt_rows = ll_cnt_rows + 1

	NEXT 
END IF



//Appeals

ll_last_claim_no = 0

ll_rows = ids_appeals.Retrieve(il_individual_no)

IF ll_rows > 0 THEN
	ll_hdr_row = nf_insert_info('Appeals', 'N')
	FOR ll_cntr = 1 to ll_rows
		ll_claim_no = ids_appeals.GetItemNumber(ll_cntr, 'claim_no')
		IF ll_cntr > 1 THEN
			ll_last_claim_no= ids_appeals.GetItemNumber(ll_cntr - 1,'claim_no')
		END IF
		IF IsNull(ll_claim_no) OR ll_claim_no = 0 THEN
			ls_claim_no = ''
		ELSE
			ls_claim_no = STRING(ll_claim_no)
			IF ll_claim_no = ll_last_claim_no THEN
				CONTINUE
			ELSE 
				ls_info = "Claim No. " + ls_claim_no  
				ll_row = nf_insert_info(ls_info, 'Y')
				ll_cnt_rows = ll_cnt_rows + 1
			END IF
		END IF
	NEXT 
	

END IF


//Active CA or CL

ll_rows = ids_cacl.Retrieve(il_individual_no)

IF ll_rows > 0 THEN
	ll_hdr_row = nf_insert_info('Active Care & Clothing Allowance','N')
	FOR ll_cntr = 1 to ll_rows
		ll_claim_no = ids_cacl.GetItemNumber(ll_cntr, 'claim_no')
		IF IsNull(ll_claim_no) OR ll_claim_no = 0 THEN
			ls_claim_no = ''
		ELSE
			ls_claim_no = STRING(ll_claim_no)
			
			ls_info = "Claim No. " + ls_claim_no
			ll_row = nf_insert_info(ls_info,'Y')
			ll_cnt_rows = ll_cnt_rows + 1
		END IF
	NEXT 
END IF


////Annuity Eligibility Status

ll_rows = ids_ann_status.Retrieve(il_individual_no)

IF ll_rows > 0 THEN
	ll_hdr_row = nf_insert_info('Annuity Eligibility Status','N')
	FOR ll_cntr = 1 to ll_rows
		ldt_date = DATE(ids_ann_status.GetItemDateTime(ll_cntr, 'annuity_end_date'))
		IF IsNull(ldt_date) OR ldt_date = DATE('1900-01-01') THEN
			ls_date = ''
		ELSE	
			ls_date = STRING(ldt_date, 'yyyy-mm-dd')		
		END IF
		ls_info = 'Eligible for Annuity - End Date ' + ls_date
		ll_row = nf_insert_info(ls_info, 'Y')
		ll_cnt_rows = ll_cnt_rows + 1
	NEXT 
END IF

//Court Order

ll_rows = ids_court.Retrieve(il_individual_no)

IF ll_rows > 0 THEN
	ll_hdr_row = nf_insert_info('Court Order', 'N')
	FOR ll_cntr = 1 to ll_rows
		ll_event_no = ids_court.GetItemNumber(ll_cntr, 'event_no')
		IF IsNull(ll_event_no) OR ll_event_no = 0 THEN
			ls_event_no = ''
		ELSE
			ls_event_no = STRING(ll_event_no)
		END IF
		ls_desc = ids_court.GetItemString(ll_cntr, 'event_specific_desc')
		
		ls_info = ls_desc + ', EL ' + ls_event_no + ' - All Claims'
		ll_row = nf_insert_info(ls_info, 'Y')
		ll_cnt_rows = ll_cnt_rows + 1
	
	NEXT 
END IF

////Previous Surgeries

ll_rows = ids_surg.Retrieve(il_individual_no)

IF ll_rows > 0 THEN
	ll_hdr_row = nf_insert_info('Surgeries Authorized', 'N')
	FOR ll_cntr = 1 to ll_rows
		ll_claim_no = ids_surg.GetItemNumber(ll_cntr, 'claim_no')
		ldt_date = DATE(ids_surg.GetItemDateTime(ll_cntr, 'create_date'))
		IF IsNull(ll_claim_no) OR ll_claim_no = 0 THEN
			ls_claim_no = ''
		ELSE
			ls_claim_no = STRING(ll_claim_no)
		END IF
		ls_desc = ids_surg.GetItemString(ll_cntr, 'desc') + ', ' + STRING(ldt_date, 'yyyy-mm-dd')
		
		ls_info = "Claim No. " + ls_claim_no  + ", " + ls_desc
		ll_row = nf_insert_info(ls_info, 'Y')
		ll_cnt_rows = ll_cnt_rows + 1
		
	NEXT 
END IF

////Modified Duties

ll_rows = ids_modified.Retrieve(il_individual_no)

IF ll_rows > 0 THEN
	ll_hdr_row = nf_insert_info('Modified Duties','N')
	FOR ll_cntr = 1 to ll_rows
		ll_claim_no = ids_modified.GetItemNumber(ll_cntr, 'claim_no')
		IF IsNull(ll_claim_no) OR ll_claim_no = 0 THEN
			ls_claim_no = ''
		ELSE
			ls_claim_no = STRING(ll_claim_no)
		END IF
		ll_event_no = ids_modified.GetItemNumber(ll_cntr, 'event_no')
		IF IsNull(ll_event_no) OR ll_event_no = 0 THEN
			ls_event_no = ''
		ELSE
			ls_event_no = STRING(ll_event_no)
		END IF
	
		ldt_date = DATE(ids_modified.GetItemDateTime(ll_cntr, 'event_date'))
		
		IF IsNull(ldt_date) OR ldt_date = DATE('1900-01-01') THEN
			ls_date = ''
		ELSE
			ls_date =  ', ' + STRING(ldt_date, 'yyyy-mm-dd')
		END IF
		
		ls_info = "Claim No. " + ls_claim_no + "  EL " + ls_event_no +  ls_date
		ll_row = nf_insert_info(ls_info, 'Y')
		ll_cnt_rows = ll_cnt_rows + 1

	NEXT 
END IF

// IRO

ll_rows = ids_iro.Retrieve(il_individual_no)

IF ll_rows > 0 THEN
	
	FOR ll_cntr = 1 to ll_rows
			
		ls_groups = ids_iro.GetItemString(ll_cntr, 'groups')
		IF (ls_last_group = '') OR (ls_last_group > '' AND ls_groups <> ls_last_group) THEN
			ll_hdr_row = nf_insert_info(ls_groups, 'N')
			IF ll_hdr_row < 0 THEN
				RETURN -1
			END IF
			ls_last_group = ls_groups
			lb_new_group = TRUE
		END IF	
		
		ll_claim_no = ids_iro.GetItemNumber(ll_cntr, 'claim_no')
		ll_event_no = ids_iro.GetItemNumber(ll_cntr, 'event_no')
		
		IF IsNull(ll_claim_no) OR ll_claim_no = 0 THEN
			ls_claim_no = ''
		ELSE
			ls_claim_no = STRING(ll_claim_no)			
		END IF
		
		IF IsNull(ll_event_no) OR ll_event_no = 0 THEN
			ls_event_no = ''
		ELSE
			ls_event_no = STRING(ll_event_no)
		END IF
		
		ldt_date = DATE(ids_iro.GetItemDateTime(ll_cntr, 'event_date'))
		IF IsNull(ldt_date) OR ldt_date = DATE('1900-01-01') THEN
			ls_date = ''
		ELSE
			ls_date =  ', ' + STRING(ldt_date, 'yyyy-mm-dd')
		END IF
	
		ls_info = ls_claim_no + "  EL " + ls_event_no + ls_date
		ll_row = nf_insert_info(ls_info, 'Y')
		ll_cnt_rows = ll_cnt_rows + 1
		
	NEXT
		
END IF

ll_info_rows = ll_cnt_rows
RETURN ll_info_rows

end function

public function integer nf_insert_info (string as_info, string as_detail);LONG ll_row

ll_row = idw_adw[il_ex_dw].InsertRow(0)

IF ll_row < 0 THEN
	MessageBox('Error Loading Info','An error occurred while loading the Information section.', Information!)
	RETURN -1
END IF

idw_adw[il_ex_dw].SetItem(ll_row, 'information', as_info)
idw_adw[il_ex_dw].SetItem(ll_row, 'detail', as_detail)

RETURN 0
end function

public function integer nf_load_recent (datawindow adw_dw);//LONG ll_row, ll_rows, ll_cntr, ll_claim_no, ll_percent, ll_event_no, ll_task_no, ll_info_rows, ll_cnt_rows, ll_hdr_row, ll_dw
//STRING  ls_cppd_status, ls_date, ls_claim_no, ls_info, ls_percent, ls_groups, ls_event_no, ls_desc, ls_task_no, ls_last_group
LONG ll_rows, ll_claim_no, ll_dw, ll_cntr, ll_row, ll_qty, ll_cnt_rows
DATE  ldt_date,ldt_from, ldt_to
STRING ls_desc1, ls_desc2, ls_group
DECIMAL{2} ldec_number
U_DS lds_recent[] 

lds_recent[] = {ids_recent_payments, ids_recent_rx, ids_recent_tasks, ids_recent_events, ids_recent_missed}
	
adw_dw.SetRedraw(FALSE)

FOR ll_dw = 1 to UpperBound(lds_recent[])
	ll_rows = lds_recent[ll_dw].Retrieve(il_individual_no)
	IF ll_rows > 0 THEN
		FOR ll_cntr = 1 to ll_rows
					
			ll_claim_no = lds_recent[ll_dw].GetItemNumber(ll_cntr, 'claim_no')
			ls_desc1 = lds_recent[ll_dw].GetItemString(ll_cntr, 'desc1')
			ls_desc2 = lds_recent[ll_dw].GetItemString(ll_cntr, 'desc2')
			ldt_date = DATE(lds_recent[ll_dw].GetItemDateTime(ll_cntr, 'processed_date'))
			ldec_number = lds_recent[ll_dw].GetItemNumber(ll_cntr, 'num')
			ldt_from = DATE(lds_recent[ll_dw].GetItemString(ll_cntr, 'from'))
			ldt_to = DATE(lds_recent[ll_dw].GetItemString(ll_cntr, 'to'))
			ll_qty = lds_recent[ll_dw].GetItemNumber(ll_cntr, 'quantity')
			ls_group = lds_recent[ll_dw].GetItemString(ll_cntr, 'groups')
							
			ll_row = adw_dw.InsertRow(0)
			IF ll_row >= 0 THEN
				adw_dw.SetItem(ll_row, 'claim_no', ll_claim_no)
				adw_dw.SetItem(ll_row, 'processed_date', ldt_date)
				adw_dw.SetItem(ll_row, 'payment_type_desc', ls_desc1)
				adw_dw.SetItem(ll_row, 'payment_sub_type_desc', ls_desc2)
				adw_dw.SetItem(ll_row, 'amount', ldec_number)
				adw_dw.SetItem(ll_row, 'paid_from_date', ldt_from)
				adw_dw.SetItem(ll_row, 'paid_to_date',ldt_to)
				adw_dw.SetItem(ll_row, 'quantity', ll_qty)
				adw_dw.SetItem(ll_row, 'groups', ls_group)
			ELSE
				RETURN -1
			END IF

			ll_cnt_rows = ll_cnt_rows + 1
			
		NEXT 
	END IF
NEXT

adw_dw.GroupCalc()
adw_dw.SetRedraw(TRUE)

RETURN ll_cnt_rows

end function

public function integer nf_calculate_age (date adt_birth, date adt_death);LONG		ll_Today_Date, ll_birth_date
STRING	ls_Age

/*	Difference in years between the Birth Date and the Other Date (i.e. Today)
	Makes sure the string is 8 characters long so that the first 4 characters reflects the age
*/

	ll_Birth_Date = Long(String(adt_birth,"YYYYMMDD"))

	IF IsNull(adt_death) OR String(adt_death,'yyyy/mm/dd') = '1900/01/01' THEN
		ll_Today_Date = Long(String(Date(f_server_datetime()),"YYYYMMDD"))
	ELSE
		ll_Today_Date = Long(String(adt_death,'YYYYMMDD'))
	END IF

	ls_Age = String(ll_Today_Date - ll_Birth_Date)
	ls_Age = Space(8 - Len(ls_Age)) + ls_Age
	ls_Age = Left(ls_Age, 4)

Return Integer(ls_Age)
end function

on n_dashboard.create
call super::create
end on

on n_dashboard.destroy
call super::destroy
end on

