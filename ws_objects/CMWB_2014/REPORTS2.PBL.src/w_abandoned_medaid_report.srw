$PBExportHeader$w_abandoned_medaid_report.srw
$PBExportComments$Report to identify outstanding Medical Aid Payments
forward
global type w_abandoned_medaid_report from w_a_report
end type
type cb_print from commandbutton within w_abandoned_medaid_report
end type
type cb_close from commandbutton within w_abandoned_medaid_report
end type
type uo_report_selector_tabs from uo_report_criteria_selector within w_abandoned_medaid_report
end type
type cb_retrieve from commandbutton within w_abandoned_medaid_report
end type
type dw_doc_index_create_date from u_dw_online within w_abandoned_medaid_report
end type
type cb_clear from commandbutton within w_abandoned_medaid_report
end type
type cb_extract from commandbutton within w_abandoned_medaid_report
end type
type p_expand from picture within w_abandoned_medaid_report
end type
type st_1 from statictext within w_abandoned_medaid_report
end type
type st_2 from statictext within w_abandoned_medaid_report
end type
type st_3 from statictext within w_abandoned_medaid_report
end type
type st_4 from statictext within w_abandoned_medaid_report
end type
end forward

global type w_abandoned_medaid_report from w_a_report
integer width = 3278
integer height = 2704
string title = "Abandoned Medical Aid Report"
boolean controlmenu = false
cb_print cb_print
cb_close cb_close
uo_report_selector_tabs uo_report_selector_tabs
cb_retrieve cb_retrieve
dw_doc_index_create_date dw_doc_index_create_date
cb_clear cb_clear
cb_extract cb_extract
p_expand p_expand
st_1 st_1
st_2 st_2
st_3 st_3
st_4 st_4
end type
global w_abandoned_medaid_report w_abandoned_medaid_report

type variables
BOOLEAN    ib_criteria_chosen
LONG       il_design_time_width, il_design_time_height, il_workspace_width_diff, il_workspace_height_diff
LONG       il_normal_y, il_normal_height
N_RESIZE   inv_resize
STRING     is_medaid_doc_date_range
STRING     is_admin_region, is_medaid_status_filter, is_claim_status_type_filter


N_TOOLTIP  inv_tooltip
end variables

forward prototypes
public subroutine wf_setresize (boolean ab_switch)
public subroutine wf_shrink_enlarge (string as_arg)
public subroutine wf_create_temp_tables ()
public subroutine wf_set_up_tab_pages ()
public subroutine wf_drop_temp_tables ()
public subroutine wf_post_retrieval_reset ()
public subroutine wf_clear_instance_variables ()
public subroutine wf_setup_tabpg_tooltips ()
public function integer wf_validate_date_criteria (ref date adt_medaid_doc_start, ref date adt_medaid_doc_end)
public function integer wf_process_tabpage_criteria (ref string as_admin_region_array[], ref string as_ignore_admin_region, ref string as_claim_status_array[], ref string as_ignore_claim_status, ref string as_claim_status_type_array[], ref string as_ignore_claim_status_type, ref string as_medaid_status_array[], ref string as_ignore_medaid_status)
public subroutine wf_populate_medaid_status (string as_medaid_status_array[])
public subroutine wf_populate_medaid_claim_region (string as_admin_region_array[])
public subroutine wf_populate_claim_status_type (string as_claim_status_type_array[])
public subroutine wf_set_filter (string as_filter)
end prototypes

public subroutine wf_setresize (boolean ab_switch);

IF ab_switch = True Then
	IF il_design_time_height = 0 or il_design_time_width = 0 THEN
		SignalError(-666,'The resize service requires that both the il_design_time_height and il_design_time_width be filled in.')
	End if
	
	/* default instance of the resize object */
	IF IsNull(inv_resize) OR NOT IsValid (inv_resize) THEN
		inv_resize = create n_resize
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

public subroutine wf_shrink_enlarge (string as_arg);
INTEGER     li_row, li_tab_Y_plus_height

li_tab_Y_plus_height = uo_report_selector_tabs.y + uo_report_selector_tabs.height

IF as_arg = "ENLARGE" THEN
	il_normal_y 		= dw_report.y
	il_normal_height 	= dw_report.height
	
	dw_report.y			= 40
	dw_report.height 	= il_normal_height + (il_normal_y - 40)
ELSE
	dw_report.y 		= li_tab_Y_plus_height + 40
	dw_report.height 	= cb_print.y - (li_tab_Y_plus_height + 60)
	
END IF 

li_row = dw_report.getrow()
IF li_row > 0 THEN 
	dw_report.scrolltorow(li_row)
END IF 
end subroutine

public subroutine wf_create_temp_tables ();STRING    ls_claim_status_and_type_array_table, ls_medaid_status_array_table
STRING    ls_medaid_region_array_table, ls_results_table

/*

-- ** FOR TESTING PURPOSES

IF OBJECT_ID('tempdb..#CLAIM_STATUS_TYPE') IS NOT NULL DROP TABLE #CLAIM_STATUS_TYPE
CREATE TABLE #CLAIM_STATUS_TYPE
(      claim_status_code            CHAR(1)       NOT NULL ,
       claim_status_type_code       CHAR(2)       NOT NULL )

IF OBJECT_ID('tempdb..#MEDAID_STATUS') IS NOT NULL DROP TABLE #MEDAID_STATUS
CREATE TABLE #MEDAID_STATUS
(      paid_status_code             CHAR(3)       NOT NULL )

IF OBJECT_ID('tempdb..#MEDAID_CLAIM_REGION') IS NOT NULL DROP TABLE #MEDAID_CLAIM_REGION
CREATE TABLE #MEDAID_CLAIM_REGION
(      admin_region_code            CHAR(3)       NOT NULL )

IF OBJECT_ID('tempdb..#REPORT_RESULTS') IS NOT NULL DROP TABLE #REPORT_RESULTS
CREATE TABLE #REPORT_RESULTS
(       claim_no                 INT            NOT NULL,
        doc_type                 CHAR(3)        NOT NULL,
        admin_region_code        CHAR(3)        NOT NULL,
        doc_id                   INT            NOT NULL,
        claim_status_type_desc   VARCHAR(50)    NOT NULL,
		  paid_status_desc         VARCHAR(30)    NOT NULL,
        invoice_date             SMALLDATETIME      NULL,
        index_date               SMALLDATETIME      NULL,
        index_comment            VARCHAR(20)    NOT NULL,
        hold_invoice_user_id     VARCHAR(16)    NOT NULL)
*/


/* declare claim status & type array table */
ls_claim_status_and_type_array_table = 'CREATE TABLE #CLAIM_STATUS_TYPE ' &
                                     + '(      claim_status_code            CHAR(1)       NOT NULL, ' &
												 + '       claim_status_type_code       CHAR(2)       NOT NULL )'
EXECUTE IMMEDIATE :ls_claim_status_and_type_array_table ;
SQLCA.nf_handle_error('w_overpayment_report','wf_clear_temp_tables','CREATE TABLE #CLAIM_STATUS_TYPE_ARRAY...')

/* declare medaid payment status array table */
ls_medaid_status_array_table         = 'CREATE TABLE #MEDAID_STATUS ' &
                                     + '(      paid_status_code             CHAR(1)       NOT NULL )'
EXECUTE IMMEDIATE :ls_medaid_status_array_table ;
SQLCA.nf_handle_error('w_overpayment_report','wf_create_temp_tables','CREATE TABLE #MEDAID_STATUS...')

/* declare medaid claim region array table */
ls_medaid_region_array_table         = 'CREATE TABLE #MEDAID_CLAIM_REGION ' &
                                     + '(      admin_region_code            CHAR(3)       NOT NULL )'
EXECUTE IMMEDIATE :ls_medaid_region_array_table ;
SQLCA.nf_handle_error('w_overpayment_report','wf_create_temp_tables','CREATE TABLE #MEDAID_CLAIM_REGION...')

///* declare report results table */
//ls_results_table                     = 'CREATE TABLE #REPORT_RESULTS ' &
//                                     + '(       claim_no                         INT            NOT NULL,' &
//                                     + '        doc_type                         CHAR(3)        NOT NULL,' &
//                                     + '        admin_region_code                CHAR(3)        NOT NULL,' &
//                                     + '        doc_id                           INT            NOT NULL,' &
//                                     + '        claim_status_type_desc           VARCHAR(50)    NOT NULL,' &
//         									 + '        paid_status_desc                 VARCHAR(30)    NOT NULL,' &
//         									 + '        invoice_date                     SMALLDATETIME      NULL,' &
//         									 + '        index_date                       SMALLDATETIME      NULL,' &
//         									 + '        index_comment                    VARCHAR(20)    NOT NULL,' &
//         									 + '        hold_invoice_user_id             VARCHAR(16)    NOT NULL)'
//EXECUTE IMMEDIATE :ls_results_table ;
//SQLCA.nf_handle_error('w_overpayment_report','wf_create_temp_tables','CREATE TABLE #REPORT_RESULTS...')

end subroutine

public subroutine wf_set_up_tab_pages ();STRING   ls_array_of_dataobjects[]



ls_array_of_dataobjects[1] = 'd_medaid_region'
ls_array_of_dataobjects[2] = 'd_medaid_status'
ls_array_of_dataobjects[3] = 'd_medaid_claim_status'

uo_report_selector_tabs.tab_tabpage_container.of_add_tabpages(ls_array_of_dataobjects)
end subroutine

public subroutine wf_drop_temp_tables ();STRING    ls_claim_status_and_type_array_table, ls_medaid_status_array_table, ls_medaid_region_array_table, ls_results_table


/* drop claim status & type array table */
ls_claim_status_and_type_array_table = 'DROP TABLE #CLAIM_STATUS_TYPE '
EXECUTE IMMEDIATE :ls_claim_status_and_type_array_table ;
SQLCA.nf_handle_error('w_overpayment_report','wf_drop_temp_tables','DROP TABLE #CLAIM_STATUS_TYPE...')

/* drop medaid status array table */
ls_medaid_status_array_table    = 'DROP TABLE #MEDAID_STATUS '
EXECUTE IMMEDIATE :ls_medaid_status_array_table ;
SQLCA.nf_handle_error('w_overpayment_report','wf_drop_temp_tables','DROP TABLE #MEDAID_STATUS...')

/* drop overpayment claim region array table */
ls_medaid_region_array_table    = 'DROP TABLE #MEDAID_CLAIM_REGION '
EXECUTE IMMEDIATE :ls_medaid_region_array_table ;
SQLCA.nf_handle_error('w_overpayment_report','wf_drop_temp_tables','DROP TABLE #MEDAID_CLAIM_REGION...')

///* drop report results table */
//ls_results_table            = 'DROP TABLE #REPORT_RESULTS '
//EXECUTE IMMEDIATE :ls_results_table ;
//SQLCA.nf_handle_error('w_overpayment_report','wf_drop_temp_tables','DROP TABLE #REPORT_RESULTS...')
//
end subroutine

public subroutine wf_post_retrieval_reset ();
wf_drop_temp_tables()

wf_clear_instance_variables()
end subroutine

public subroutine wf_clear_instance_variables ();
// clear instance variables
is_medaid_doc_date_range    = ''
is_admin_region             = ''
is_medaid_status_filter     = ''
is_claim_status_type_filter = ''
ib_criteria_chosen          = FALSE
end subroutine

public subroutine wf_setup_tabpg_tooltips ();INTEGER            li_tab_counter, li_tab_upperbound
U_TAB_SELECTION    l_tab
U_TABPG_SELECTION  luo_tabpage


// set up tool tips for buttons on criteria tab control
l_tab = uo_report_selector_tabs.tab_tabpage_container

inv_tooltip = CREATE N_TOOLTIP

li_tab_upperbound = UpperBound(l_tab.Control)
FOR li_tab_counter = 1 TO li_tab_upperbound
	luo_tabpage = l_tab.Control[li_tab_counter]
	
	inv_tooltip.of_AddTool(luo_tabpage.cb_forward,    'Add',       0)
	inv_tooltip.of_AddTool(luo_tabpage.cb_all_forward,'Add All',   0)
	inv_tooltip.of_AddTool(luo_tabpage.cb_back,       'Remove',    0)
	inv_tooltip.of_AddTool(luo_tabpage.cb_all_back,   'Remove All',0)
NEXT
end subroutine

public function integer wf_validate_date_criteria (ref date adt_medaid_doc_start, ref date adt_medaid_doc_end);DATE      ldt_one_year_in_past
INTEGER   li_msg

//----------------- Med Aid Document Index Create Date checks
/* do an accepttext on the Document Index create date criteria DW */
dw_doc_index_create_date.AcceptText()

// grab the medaid_doc date range
adt_medaid_doc_start = dw_doc_index_create_date.GetItemDate(1,'date_range_start')
adt_medaid_doc_end   = dw_doc_index_create_date.GetItemDate(1,'date_range_end')

/* couple of checks here */
IF isnull(adt_medaid_doc_start) THEN
	MessageBox('Validation Error', 'The start date must be entered.')
	RETURN -1
END IF

IF isnull(adt_medaid_doc_end) 	THEN
	MessageBox('Validation Error', 'The end date must be entered.')
	RETURN -1
END IF

/* do a couple of checks */
IF adt_medaid_doc_start > Date('2079-06-06') OR adt_medaid_doc_start < Date('1900-01-01') THEN 
	MessageBox('Validation Error', 'The start date, if entered, must be a valid date between 1900-01-01 and 2079-06-06.')
	RETURN -1
END IF 

IF adt_medaid_doc_end > Date('2079-06-06') OR adt_medaid_doc_end < Date('1900-01-01') THEN 
	MessageBox('Validation Error', 'The end date, if entered, must be a valid date between 1900-01-01 and 2079-06-06.')
	RETURN -1
END IF 

IF adt_medaid_doc_start > adt_medaid_doc_end THEN 
	MessageBox('Validation Error', 'The end date, if entered, cannot be greater than the start date.')
	RETURN -1
ELSE
	// 
	SELECT TOP 1 DATEADD(YY,-1,:adt_medaid_doc_end)
	INTO   :ldt_one_year_in_past
	FROM   sysobjects
	USING SQLCA;
	SQLCA.nf_handle_error('w_abandoned_medaid_report','embedded SQL: SELECT DATEADD(YY,-1,ldt_current)...','wf_validate_date_criteria')
	
	IF ldt_one_year_in_past > adt_medaid_doc_start THEN
		li_msg = MessageBox('Long date range','The date range that you have entered is more than a year, which could result in a long retrieval for this report. Do you want to continue with this date range?',Question!,YesNo!,2)
		IF li_msg = 2 THEN
			RETURN -1
		END IF
	END IF

END IF 

// populate Document Index date criteria string for report
is_medaid_doc_date_range = String(adt_medaid_doc_start)+ ' to ' +String(adt_medaid_doc_end)


RETURN 0


end function

public function integer wf_process_tabpage_criteria (ref string as_admin_region_array[], ref string as_ignore_admin_region, ref string as_claim_status_array[], ref string as_ignore_claim_status, ref string as_claim_status_type_array[], ref string as_ignore_claim_status_type, ref string as_medaid_status_array[], ref string as_ignore_medaid_status);INTEGER		li_upper, li_counter, li_upper2, li_counter2, li_msg
S_ARG_ARRAY lstr_arg_array[]
STRING      ls_current_key


// there are 3 tabs associated with this report
// call the object function to get arrays of arguments for dw_report

uo_report_selector_tabs.tab_tabpage_container.of_get_string_array(lstr_arg_array)

li_upper = UpperBound(lstr_arg_array)

for li_counter = 1 to li_upper
	li_upper2 = UpperBound(lstr_arg_array[li_counter].key)
	
	IF li_upper2 > 0 THEN
		FOR li_counter2 = 1 TO li_upper2
			ls_current_key = lstr_arg_array[li_counter].key[li_counter2]
			
			CHOOSE CASE ls_current_key
//					
//				CASE 'claim_status_code'
//					as_claim_status_array = lstr_arg_array[li_counter].value
//					IF lstr_arg_array[li_counter].populated_flag[1] ='Y' THEN						
//						as_ignore_claim_status = 'N'
//						ib_criteria_chosen     = TRUE
//					ELSE
//						as_ignore_claim_status = 'Y'
//						is_claim_status_filter = ''
//					END IF
					
				CASE 'claim_status_type_code'
					as_claim_status_type_array = lstr_arg_array[li_counter].value
					IF lstr_arg_array[li_counter].populated_flag[1] ='Y' THEN						
						as_ignore_claim_status_type = 'N'
						ib_criteria_chosen          = TRUE
					ELSE
						as_ignore_claim_status_type = 'Y'
						is_claim_status_type_filter = ''
					END IF
					
				CASE 'paid_status_code'
					as_medaid_status_array = lstr_arg_array[li_counter].value
					IF lstr_arg_array[li_counter].populated_flag[1] ='Y' THEN
						as_ignore_medaid_status = 'N'
						ib_criteria_chosen      = TRUE
					ELSE
						as_ignore_medaid_status = 'Y'
						is_medaid_status_filter = ''
					END IF
					
				CASE 'admin_region_code'
					as_admin_region_array = lstr_arg_array[li_counter].value
					IF lstr_arg_array[li_counter].populated_flag[1] ='Y' THEN
						as_ignore_admin_region = 'N'
						ib_criteria_chosen     = TRUE
					ELSE
						as_ignore_admin_region = 'Y'
						is_admin_region = ''
					END IF
					
				CASE ELSE
					// ??
			END CHOOSE
		NEXT
	END IF
NEXT

IF ib_criteria_chosen THEN
	// OK, no warning
ELSE
	li_msg = MessageBox('No Criteria?','You have not selected any criteria for this report.' &
	                                 + '~r~n~r~n' &
	                                 + 'Do you want to retrieve this report without any criteria?', &
												+ Question!,YesNo!,2)
	IF li_msg = 2 THEN RETURN -1
END IF

// create temp tables
wf_create_temp_tables()

// populate temp tables from arrays
IF as_ignore_medaid_status = 'N'     THEN   wf_populate_medaid_status(as_medaid_status_array)
IF as_ignore_claim_status_type = 'N' THEN   wf_populate_claim_status_type(as_claim_status_type_array)
IF as_ignore_admin_region = 'N'      THEN   wf_populate_medaid_claim_region(as_admin_region_array)


RETURN 0
end function

public subroutine wf_populate_medaid_status (string as_medaid_status_array[]);INTEGER    li_counter, li_upper, li_dw_rowcount, li_find
STRING     ls_filter, ls_paid_desc, ls_find, ls_index
U_DS       lds_medaid_status

/*

If overpayment status criteria have been selected, then
populate a temporary table from the selection.
This table will be used in the report retrieval

*/


li_upper = UpperBound(as_medaid_status_array)

// if some O/P statuses were selected
IF li_upper <> 0 THEN
	lds_medaid_status = Create U_DS
	lds_medaid_status.DataObject = 'd_medaid_status'
	lds_medaid_status.SetTransObject(SQLCA)
	
	li_dw_rowcount = lds_medaid_status.Retrieve()
	
	FOR li_counter = 1 TO li_upper
		INSERT #MEDAID_STATUS
		(      paid_status_code )
		SELECT :as_medaid_status_array[li_counter]
		USING SQLCA;
		SQLCA.nf_handle_error('w_abandoned_medaid_report','embedded SQL: INSERT #MEDAID_STATUS...','wf_populate_medaid_status')
			
		// if the user has NOT selected ALL O/P statuses
		IF li_dw_rowcount <> li_upper THEN
			ls_find = 'paid_status_code = "' +as_medaid_status_array[li_counter]+ '"'
			li_find = lds_medaid_status.Find(ls_find,1,li_dw_rowcount)
			IF li_find > 0 THEN
				ls_paid_desc = lds_medaid_status.GetItemString(li_find,'paid_status_desc')
				IF li_upper = 1 THEN
					ls_filter = ls_paid_desc
				ELSE
					IF li_counter = 1 THEN
						ls_filter = ls_paid_desc+ ', '

					ELSEIF li_counter = li_upper THEN
						ls_filter = ls_filter + ' ' +ls_paid_desc

					ELSE
						ls_filter = ls_filter + ' ' +ls_paid_desc+ ', '
					END IF
				END IF
			ELSEIF li_find = 0 AND as_medaid_status_array[li_counter] = 'X' THEN
				// 'X' corresponds to 'unpaid' documents,
				// which includes PAYMENT_DOCUMENT.paid_status = 'E'
				// and DOCUMENT_INDEX records with no corresponding PAYMENT_DOCUMENT record
				IF li_upper = 1 THEN
					ls_filter = 'Unpaid'
				ELSE
					IF li_counter = 1 THEN
						ls_filter = 'Unpaid, '

					ELSEIF li_counter = li_upper THEN
						ls_filter = ls_filter + ' Unpaid'

					ELSE
						ls_filter = ls_filter + ' Unpaid, '
					END IF
				END IF
			END IF
			
		ELSE
			ls_filter = 'ALL'
		END IF
	NEXT
	
	is_medaid_status_filter = ls_filter

END IF

IF li_upper > 0 THEN
	// add index to temp table
	ls_index = 'CREATE UNIQUE CLUSTERED INDEX idx1 ON #MEDAID_STATUS (paid_status_code)'
	EXECUTE IMMEDIATE :ls_index
	USING SQLCA;
	SQLCA.nf_handle_error('w_abandoned_medaid_report','embedded SQL: CREATE UNIQUE CLUSTERED INDEX idx1','wf_populate_medaid_status')
END IF



//int li_table_count
//
//SELECT COUNT(*)
//INTO   :li_table_count
//FROM   #OVERPAYMENT_STATUS
//USING SQLCA;
//SQLCA.nf_handle_error('w_overpayment_report','embedded SQL: count #OVERPAYMENT_STATUS...','wf_populate_claim_manager_table')
//
//messagebox('#OVERPAYMENT_STATUS record count',li_table_count)
end subroutine

public subroutine wf_populate_medaid_claim_region (string as_admin_region_array[]);INTEGER    li_counter, li_upper, li_dw_rowcount, li_find
STRING     ls_filter, ls_find, ls_admin_region, ls_index
U_DS       lds_medaid_region

/*

If claim admin region criteria have been selected, then
populate a temporary table from the selection.
This table will be used in the report retrieval

*/


li_upper = UpperBound(as_admin_region_array)

// if some regions were selected
IF li_upper <> 0 THEN
	lds_medaid_region = Create U_DS
	lds_medaid_region.DataObject = 'd_medaid_region'
	lds_medaid_region.SetTransObject(SQLCA)
	
	li_dw_rowcount = lds_medaid_region.Retrieve()
	
	FOR li_counter = 1 TO li_upper
		INSERT #MEDAID_CLAIM_REGION
		(      admin_region_code )
		SELECT :as_admin_region_array[li_counter]
		USING SQLCA;
		SQLCA.nf_handle_error('w_abandoned_medaid_report','embedded SQL: INSERT #MEDAID_CLAIM_REGION...','wf_populate_medaid_claim_region')
		
	   // if the user has NOT selected ALL regions
		IF li_dw_rowcount <> li_upper THEN
			ls_find = 'admin_region_code = "' +as_admin_region_array[li_counter]+ '"'
			li_find = lds_medaid_region.Find(ls_find,1,li_dw_rowcount)
			IF li_find > 0 THEN
				ls_admin_region = lds_medaid_region.GetItemString(li_find,'admin_region_code')
				IF li_upper = 1 THEN
					ls_filter = ls_admin_region
				ELSE
					IF li_counter = 1 THEN
						ls_filter = ls_admin_region+ ', '
					ELSEIF li_counter = li_upper THEN
						ls_filter = ls_filter + ' ' +ls_admin_region
					ELSE
						ls_filter = ls_filter + ' ' +ls_admin_region+ ', '
					END IF
				END IF
			END IF
		ELSE
			ls_filter = 'ALL'
		END IF
	NEXT
	
	is_admin_region = ls_filter
END IF

IF li_upper > 0 THEN
	// add index to temp table
	ls_index = 'CREATE UNIQUE CLUSTERED INDEX idx1 ON #MEDAID_CLAIM_REGION (admin_region_code)'
	EXECUTE IMMEDIATE :ls_index
	USING SQLCA;
	SQLCA.nf_handle_error('w_abandoned_medaid_report','embedded SQL: CREATE UNIQUE CLUSTERED INDEX idx1','wf_populate_claim_status_type')
END IF



//int li_table_count
//
//SELECT COUNT(*)
//INTO   :li_table_count
//FROM   #MEDAID_CLAIM_REGION
//USING SQLCA;
//SQLCA.nf_handle_error('w_overpayment_report','embedded SQL: count #MEDAID_CLAIM_REGION...','wf_populate_medaid_claim_region')
//
//messagebox('#MEDAID_CLAIM_REGION record count',li_table_count)
end subroutine

public subroutine wf_populate_claim_status_type (string as_claim_status_type_array[]);INTEGER    li_counter, li_upper, li_dw_rowcount, li_find
STRING     ls_filter, ls_find, ls_claim_status_desc, ls_index
U_DS       lds_medaid_claim_status

/*

If claim status/status type criteria have been selected, then
populate a temporary table from the selection.
This table will be used in the report retrieval

*/


// if some claim statuses were selected
li_upper = UpperBound(as_claim_status_type_array)

IF li_upper <> 0 THEN
	lds_medaid_claim_status = Create U_DS
	lds_medaid_claim_status.DataObject = 'd_medaid_claim_status'
	lds_medaid_claim_status.SetTransObject(SQLCA)
	
	li_dw_rowcount = lds_medaid_claim_status.Retrieve()
	SQLCA.nf_handle_error('w_abandoned_medaid_report','lds_medaid_claim_status.Retrieve','wf_populate_claim_status_type_table')
	
	FOR li_counter = 1 TO li_upper
		INSERT #CLAIM_STATUS_TYPE
		(      claim_status_code,
		       claim_status_type_code)
		SELECT :as_claim_status_type_array[li_counter],
		       :as_claim_status_type_array[li_counter+1]
		USING SQLCA;
		SQLCA.nf_handle_error('w_abandoned_medaid_report','embedded SQL: INSERT #CLAIM_STATUS_TYPE...','wf_populate_claim_status_type_table')
		
		// if the user has NOT selected ALL claim statuses
		IF li_dw_rowcount*2 <> li_upper THEN
			IF as_claim_status_type_array[li_counter+1] = '' THEN
				ls_find = 'claim_status_code = "' +as_claim_status_type_array[li_counter]+ '"'
				li_find = lds_medaid_claim_status.Find(ls_find,1,li_dw_rowcount)
			ELSE
				ls_find = 'claim_status_code = "' +as_claim_status_type_array[li_counter]+ '" AND claim_status_type_code = "' +as_claim_status_type_array[li_counter+1]+ '"'
				li_find = lds_medaid_claim_status.Find(ls_find,1,li_dw_rowcount)
			END IF
			
			IF li_find > 0 THEN
				ls_claim_status_desc = lds_medaid_claim_status.GetItemString(li_find,'claim_status_description')
				IF li_upper = 2 THEN    // status & status type codes occupy 2 different elements in array
					ls_filter = ls_claim_status_desc
				ELSE
					IF li_counter = 1 THEN
						ls_filter = ls_claim_status_desc+ ', '
					ELSEIF li_counter+1 = li_upper THEN
						ls_filter = ls_filter + ' ' +ls_claim_status_desc
					ELSE
						ls_filter = ls_filter + ' ' +ls_claim_status_desc+ ', '
					END IF
				END IF
			END IF
			is_claim_status_type_filter = ls_filter
		ELSE
			is_claim_status_type_filter = 'ALL'
		END IF
		li_counter++ // status & status type codes occupy 2 different elements in array
	NEXT
	
END IF

IF li_upper > 0 THEN
	// add index to temp table
	ls_index = 'CREATE UNIQUE CLUSTERED INDEX idx1 ON #CLAIM_STATUS_TYPE (claim_status_code,claim_status_type_code)'
	EXECUTE IMMEDIATE :ls_index
	USING SQLCA;
	SQLCA.nf_handle_error('w_abandoned_medaid_report','embedded SQL: CREATE UNIQUE CLUSTERED INDEX idx1','wf_populate_claim_status_type')
END IF

//int li_table_count
//
//SELECT COUNT(*)
//INTO   :li_table_count
//FROM   #CLAIM_STATUS
//USING SQLCA;
//SQLCA.nf_handle_error('w_overpayment_report','embedded SQL: count #CLAIM_STATUS...','wf_populate_claim_manager_table')
//
//messagebox('#CLAIM_STATUS record count',li_table_count)
end subroutine

public subroutine wf_set_filter (string as_filter);INTEGER    li_rtn


//set filter to nothing
dw_report.SetRedraw(FALSE)
dw_report.setfilter('')
dw_report.Filter()
dw_report.SetRedraw(TRUE)


/* only filter if there are rows */
IF TRIM(as_filter) > '' THEN
	
	dw_report.SetRedraw(FALSE)
	
	// filter
	li_rtn = dw_report.SetFilter(as_filter)
	li_rtn = dw_report.Filter()
	
	dw_report.SetRedraw(TRUE)
	
END IF 
end subroutine

on w_abandoned_medaid_report.create
int iCurrent
call super::create
this.cb_print=create cb_print
this.cb_close=create cb_close
this.uo_report_selector_tabs=create uo_report_selector_tabs
this.cb_retrieve=create cb_retrieve
this.dw_doc_index_create_date=create dw_doc_index_create_date
this.cb_clear=create cb_clear
this.cb_extract=create cb_extract
this.p_expand=create p_expand
this.st_1=create st_1
this.st_2=create st_2
this.st_3=create st_3
this.st_4=create st_4
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_print
this.Control[iCurrent+2]=this.cb_close
this.Control[iCurrent+3]=this.uo_report_selector_tabs
this.Control[iCurrent+4]=this.cb_retrieve
this.Control[iCurrent+5]=this.dw_doc_index_create_date
this.Control[iCurrent+6]=this.cb_clear
this.Control[iCurrent+7]=this.cb_extract
this.Control[iCurrent+8]=this.p_expand
this.Control[iCurrent+9]=this.st_1
this.Control[iCurrent+10]=this.st_2
this.Control[iCurrent+11]=this.st_3
this.Control[iCurrent+12]=this.st_4
end on

on w_abandoned_medaid_report.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_print)
destroy(this.cb_close)
destroy(this.uo_report_selector_tabs)
destroy(this.cb_retrieve)
destroy(this.dw_doc_index_create_date)
destroy(this.cb_clear)
destroy(this.cb_extract)
destroy(this.p_expand)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.st_4)
end on

event open;call super::open;ulong   lul_null



dw_doc_index_create_date.SetTransObject(SQLCA)
dw_doc_index_create_date.InsertRow(0)


il_design_time_width  = 3100
il_design_time_height = 2500

This.wf_SetResize(True)

THIS.inv_resize.of_register(dw_report,'ScaleToRight&Bottom')
THIS.inv_resize.of_register(uo_report_selector_tabs,0,0,100,0)
THIS.inv_resize.of_register(cb_print,100,100,0,0)
THIS.inv_resize.of_register(cb_close,100,100,0,0)
THIS.inv_resize.of_register(cb_extract,'FixedToBottom')



wf_set_up_tab_pages()



/* for testing purposes */

//DATETIME ldtm_start, ldtm_end
//
//ldtm_start = datetime('2010-01-01')
//ldtm_end   = datetime(today())
//
//dw_doc_index_create_date.SetItem(1,'date_range_start', ldtm_start)
//dw_doc_index_create_date.SetItem(1,'date_range_end',   ldtm_end)
//dw_doc_index_create_date.AcceptText()
//



wf_setup_tabpg_tooltips()

wf_shrink_enlarge('SHRINK')


SetNull(lul_null)

POST EVENT resize(lul_null,THIS.width,THIS.height)
end event

event resize;call super::resize;LONG ll_workspacewidth,ll_workspaceheight

IF IsValid(inv_resize) THEN
	// Notify the resize service that the window size has changed.
	ll_workspacewidth  = This.WorkSpaceWidth()
	ll_workspaceheight = This.WorkSpaceHeight()

	IF IsValid (inv_resize) THEN
		inv_resize.Event pfc_Resize (sizetype,ll_workspacewidth  , ll_workspaceheight )
	END IF
	
	dw_report.TriggerEvent(Resize!)
END IF 
end event

type dw_report from w_a_report`dw_report within w_abandoned_medaid_report
integer x = 50
integer y = 1196
integer width = 2994
integer height = 1176
integer taborder = 0
string title = "Abandoned Medical Aid Report"
string dataobject = "d_abandoned_medaid"
boolean hscrollbar = true
end type

event dw_report::constructor;call super::constructor;THIS.SetTransObject(SQLCA)
THIS.uf_setSelect(1) //SINGLE SELECT
THIS.Object.DataWindow.Print.Orientation= '1'

end event

event dw_report::rbuttondown;M_DW_RMB_POPUP lm_popup

IF IsValid(inv_rowselection) Then
	inv_rowselection.event pfc_rbuttondown(xpos,ypos,row,dwo)
End if


/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/
lm_popup = Create m_dw_rmb_popup
lm_popup.mf_set_datawindow(This)

// don't display lines & spaces
lm_popup.m_options.m_.Visible  = FALSE
lm_popup.m_options.m_0.Visible = FALSE
lm_popup.m_options.m_1.Visible = FALSE
lm_popup.m_options.m_2.Visible = FALSE

// display sort
lm_popup.m_options.m_sort.Visible = TRUE


lm_popup.m_options.PopMenu(w_frame.PointerX( ), w_frame.PointerY( ))

Destroy lm_popup
end event

event dw_report::resize;call super::resize;// move p_expand

p_expand.Y = THIS.Y
p_expand.X = THIS.Width - 140
end event

type cb_print from commandbutton within w_abandoned_medaid_report
integer x = 2203
integer y = 2392
integer width = 402
integer height = 80
integer taborder = 40
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Print"
end type

event clicked;
IF dw_report.rowcount() > 0 THEN 
	dw_report.object.datawindow.print.orientation = 1  //Landscape

	dw_report.PRINT()
	
//	dw_filter.Print()
END IF 

end event

type cb_close from commandbutton within w_abandoned_medaid_report
integer x = 2647
integer y = 2392
integer width = 402
integer height = 80
integer taborder = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Close"
end type

event clicked;Close(PARENT)
end event

type uo_report_selector_tabs from uo_report_criteria_selector within w_abandoned_medaid_report
event ue_mousemove pbm_mousemove
integer x = 50
integer y = 208
integer width = 2149
integer height = 952
integer taborder = 30
boolean bringtotop = true
end type

event ue_mousemove;//LONG               ll_ab_x, ll_ab_width, ll_ab_y, ll_ab_height
//U_TAB_SELECTION    l_tab
//U_TABPG_SELECTION  luo_tabpage
//
//l_tab = uo_report_selector_tabs.tab_tabpage_container
//luo_tabpage = l_tab.Control[l_tab.SelectedTab]
//
//
//ll_ab_x      = luo_tabpage.cb_all_back.X - uo_report_selector_tabs.X
//ll_ab_width  = luo_tabpage.cb_all_back.Width
//ll_ab_y      = luo_tabpage.cb_all_back.Y - uo_report_selector_tabs.Y
//ll_ab_height = luo_tabpage.cb_all_back.Height
//
//IF xpos >= ll_ab_x AND xpos <= (ll_ab_x+ll_ab_width) AND ypos >= ll_ab_y AND ypos <= (ll_ab_x+ll_ab_width) THEN
//	luo_tabpage.cb_all_back.Text = 'Remove All'
//	luo_tabpage.cb_all_back.Width = 420
//ELSE
//	luo_tabpage.cb_all_back.Text = '<<'
//	luo_tabpage.cb_all_back.Width = 101
//END IF
end event

on uo_report_selector_tabs.destroy
call uo_report_criteria_selector::destroy
end on

type cb_retrieve from commandbutton within w_abandoned_medaid_report
integer x = 2757
integer y = 632
integer width = 288
integer height = 120
integer taborder = 50
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Retrieve"
boolean default = true
end type

event clicked;DATE			ldt_medaid_doc_start, ldt_medaid_doc_end
DATETIME    ldtm_medaid_doc_start, ldtm_medaid_doc_end
INTEGER		li_rtn, li_rowcount, li_pos, li_next_pos

STRING      ls_claim_status_array[], ls_ignore_claim_status
STRING      ls_claim_status_type_array[], ls_ignore_claim_status_type
STRING      ls_medaid_status_array[], ls_ignore_medaid_status
STRING      ls_admin_region_array[], ls_ignore_admin_region
string ls_filter, ls_new_filter, ls_medaid_status_argument


SetPointer(HourGlass!)

// validate date criteria
li_rtn = wf_validate_date_criteria(ldt_medaid_doc_start, ldt_medaid_doc_end)
IF li_rtn < 0 THEN RETURN

ldtm_medaid_doc_start = DateTime(ldt_medaid_doc_start)
ldtm_medaid_doc_end   = DateTime(ldt_medaid_doc_end)

ls_filter	           	= uo_report_selector_tabs.tab_tabpage_container.of_get_filter()
//is_current_filter 	= ls_filter

// paid status will not be filtered, it will be argument for stored proc for efficiency
li_pos = Pos(ls_filter,'paid_status_code')
IF li_pos > 0 THEN
	li_next_pos = Pos(ls_filter,'paid_status_code = "A"',li_pos)
	IF li_next_pos > 0 THEN
		ls_new_filter = Replace(ls_filter, li_next_pos, 22, 'paid_status_code<> "Z"')
	ELSE
		li_next_pos = Pos(ls_filter,'paid_status_code = "H"',li_pos)
		IF li_next_pos > 0 THEN
			ls_new_filter = Replace(ls_filter, li_next_pos, 22, 'paid_status_code<> "Z"')
		ELSE
			li_next_pos = Pos(ls_filter,'paid_status_code = "X"',li_pos)
			IF li_next_pos > 0 THEN
				ls_new_filter = Replace(ls_filter, li_next_pos, 22, 'paid_status_code<> "Z"')
			END IF
		END IF	
	END IF
ELSE
	ls_new_filter = ls_filter
END IF

dw_report.SetRedraw(FALSE)

// process the selected values on the tabpages
li_rtn = wf_process_tabpage_criteria(ls_admin_region_array,      ls_ignore_admin_region , &
												 ls_claim_status_array,      ls_ignore_claim_status , &
												 ls_claim_status_type_array, ls_ignore_claim_status_type, &
												 ls_medaid_status_array[],   ls_ignore_medaid_status)
IF li_rtn < 0 THEN RETURN

IF ls_ignore_medaid_status = 'Y' THEN
	ls_medaid_status_argument = 'N' // no paid status selected
ELSEIF UpperBound(ls_medaid_status_array) = 2 THEN
	ls_medaid_status_argument = 'A' // both (all) paid statuses selected
ELSE
	ls_medaid_status_argument = ls_medaid_status_array[1]
END IF

// add a day to the selected end date, so that it can be included
ldtm_medaid_doc_end = DateTime(RelativeDate (ldt_medaid_doc_end, 1) )

li_rowcount = dw_report.Retrieve(ldtm_medaid_doc_start, ldtm_medaid_doc_end, ls_medaid_status_argument, is_medaid_doc_date_range, is_admin_region, is_claim_status_type_filter, is_medaid_status_filter)
SQLCA.nf_handle_error('w_abandoned_medaid_report','dw_1.Retrieve','cb_retrieve.clicked')
//
////messagebox('after retrieval rowcount',li_rowcount)
//


// set the filter -- should we check and see if null?
wf_set_filter(ls_new_filter)

li_rowcount = dw_report.RowCount()
IF li_rowcount = 0 THEN
	messagebox('No records','The selected criteria did not return any records.',Exclamation!)
END IF
//
// reset temp tables, variables after retrieval
wf_post_retrieval_reset()


dw_report.SETREDRAW(TRUE)

end event

type dw_doc_index_create_date from u_dw_online within w_abandoned_medaid_report
string tag = "this is a test"
integer x = 2382
integer y = 228
integer width = 663
integer height = 260
integer taborder = 10
boolean bringtotop = true
string dataobject = "d_doc_index_create_date_search_criteria"
borderstyle borderstyle = stylelowered!
end type

type cb_clear from commandbutton within w_abandoned_medaid_report
integer x = 2757
integer y = 796
integer width = 288
integer height = 120
integer taborder = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "C&lear"
end type

event clicked;INTEGER  li_rowcount
STRING   ls_array_of_dataobjects[]



IF messagebox('Confirm Clear', 'Are you sure you want to clear your search criteria?',Question!, YesNo! , 2) = 2 THEN RETURN 


THIS.SetRedraw(FALSE)

/* reset dates */
dw_doc_index_create_date.reset()
dw_doc_index_create_date.insertrow(0)


// clear all the tab pages
ls_array_of_dataobjects[1] = 'RESET'
uo_report_selector_tabs.tab_tabpage_container.of_add_tabpages(ls_array_of_dataobjects)

// set them up again
wf_set_up_tab_pages()

// tooltips
wf_setup_tabpg_tooltips()

// reset variables
wf_clear_instance_variables()

/* set the details to nothing */
dw_report.reset()


THIS.SetRedraw(TRUE)
end event

type cb_extract from commandbutton within w_abandoned_medaid_report
boolean visible = false
integer x = 50
integer y = 2392
integer width = 402
integer height = 80
integer taborder = 50
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Extract"
end type

event clicked;/* Extracting the Data */
IF dw_report.RowCount() = 0 THEN
	MessageBox('No rows','There is no Report Data to extract.')
	RETURN
END IF

dw_report.Saveas('',Excel8!,TRUE)

end event

type p_expand from picture within w_abandoned_medaid_report
integer x = 2866
integer y = 1196
integer width = 105
integer height = 76
boolean bringtotop = true
string picturename = "maximize.bmp"
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

event clicked;

IF THIS.picturename = "maximize.bmp" THEN
	PARENT.wf_shrink_enlarge('ENLARGE')	
	THIS.picturename = "restore.bmp"
ELSE
	PARENT.wf_shrink_enlarge('SHRINK')
	THIS.picturename = "maximize.bmp"
END IF

dw_report.bringtotop = TRUE
THIS.bringtotop = TRUE

end event

type st_1 from statictext within w_abandoned_medaid_report
integer x = 197
integer y = 40
integer width = 1088
integer height = 84
boolean bringtotop = true
integer textsize = -12
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Optional Filter by Main Criteria"
boolean focusrectangle = false
end type

type st_2 from statictext within w_abandoned_medaid_report
integer x = 87
integer y = 128
integer width = 910
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16711680
long backcolor = 67108864
string text = "Criteria Available for Selection"
boolean focusrectangle = false
end type

type st_3 from statictext within w_abandoned_medaid_report
integer x = 1431
integer y = 128
integer width = 503
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 32768
long backcolor = 67108864
string text = "Selected Criteria"
boolean focusrectangle = false
end type

type st_4 from statictext within w_abandoned_medaid_report
integer x = 2258
integer y = 48
integer width = 905
integer height = 84
boolean bringtotop = true
integer textsize = -12
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Mandatory Filter by Dates"
boolean focusrectangle = false
end type

