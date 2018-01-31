$PBExportHeader$w_overpayment_report.srw
forward
global type w_overpayment_report from w_a_report
end type
type cb_print from commandbutton within w_overpayment_report
end type
type cb_close from commandbutton within w_overpayment_report
end type
type uo_report_selector_tabs from uo_report_criteria_selector within w_overpayment_report
end type
type cb_retrieve from commandbutton within w_overpayment_report
end type
type dw_op_create_date from u_dw_online within w_overpayment_report
end type
type dw_op_fin_pay_date from u_dw_online within w_overpayment_report
end type
type cb_clear from commandbutton within w_overpayment_report
end type
type cb_extract from commandbutton within w_overpayment_report
end type
type p_expand from picture within w_overpayment_report
end type
type dw_op_last_payment_date from u_dw_online within w_overpayment_report
end type
type st_1 from statictext within w_overpayment_report
end type
type st_2 from statictext within w_overpayment_report
end type
type st_3 from statictext within w_overpayment_report
end type
type st_4 from statictext within w_overpayment_report
end type
end forward

global type w_overpayment_report from w_a_report
integer width = 3278
integer height = 2704
string title = "Overpayment Summary Report"
boolean controlmenu = false
cb_print cb_print
cb_close cb_close
uo_report_selector_tabs uo_report_selector_tabs
cb_retrieve cb_retrieve
dw_op_create_date dw_op_create_date
dw_op_fin_pay_date dw_op_fin_pay_date
cb_clear cb_clear
cb_extract cb_extract
p_expand p_expand
dw_op_last_payment_date dw_op_last_payment_date
st_1 st_1
st_2 st_2
st_3 st_3
st_4 st_4
end type
global w_overpayment_report w_overpayment_report

type variables
BOOLEAN    ib_criteria_chosen
LONG       il_design_time_width, il_design_time_height, il_workspace_width_diff, il_workspace_height_diff
LONG       il_normal_y, il_normal_height
N_RESIZE   inv_resize
STRING     is_op_date_range, is_op_fin_pay_date_range, is_op_last_pay_date_range
STRING     is_op_type_code, is_admin_region, is_op_status_filter, is_claim_status_filter, is_claim_manager_filter
WINDOW     iw_win

N_TOOLTIP  inv_tooltip
end variables

forward prototypes
public subroutine wf_setresize (boolean ab_switch)
public subroutine wf_shrink_enlarge (string as_arg)
public function window wf_get_window_reference ()
public subroutine wf_populate_claim_manager_table (string as_claim_manager_array[])
public subroutine wf_populate_overpayment_status (string as_op_status_array[])
public subroutine wf_populate_overpayment_type (string as_overpayment_type_array[])
public subroutine wf_populate_overpayment_claim_region (string as_admin_region_array[])
public subroutine wf_create_temp_tables ()
public subroutine wf_populate_claim_status_table (string as_claim_status_array[])
public function integer wf_process_tabpage_criteria (ref string as_claim_manager_array[], ref string as_ignore_claim_manager, ref string as_claim_status_array[], ref string as_ignore_claim_status, ref string as_op_status_array[], ref string as_ignore_op_status, ref string as_overpayment_type_array[], ref string as_ignore_overpayment_type, ref string as_admin_region_array[], ref string as_ignore_admin_region)
public subroutine wf_set_up_tab_pages ()
public subroutine wf_drop_temp_tables ()
public subroutine wf_post_retrieval_reset ()
public subroutine wf_clear_instance_variables ()
public function integer wf_validate_date_criteria (ref date adt_op_start, ref date adt_op_end, ref date adt_fin_start, ref date adt_fin_end, ref date adt_op_pay_start, ref date adt_op_pay_end)
public subroutine wf_populate_report_results_table (date adt_op_create_start, date adt_op_create_end, date adt_op_fin_pay_start, date adt_op_fin_pay_end, date adt_op_last_pay_start, date adt_op_last_pay_end, string as_ignore_op_status, string as_ignore_op_type, string as_ignore_claim_manager, string as_ignore_claim_status, string as_ignore_admin_region)
public subroutine wf_setup_tabpg_tooltips ()
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

public function window wf_get_window_reference ();RETURN THIS
end function

public subroutine wf_populate_claim_manager_table (string as_claim_manager_array[]);INTEGER    li_counter, li_upper, li_dw_rowcount
STRING     ls_filter
U_DS       lds_op_claim_manager

/*

If claim manager criteria have been selected, then
populate a temporary table from the selection.
This table will be used in the report retrieval

*/

li_upper = UpperBound(as_claim_manager_array)

// if some claim managers were selected
IF li_upper <> 0 THEN
	lds_op_claim_manager = Create U_DS
	lds_op_claim_manager.DataObject = 'd_op_claim_manager'
	lds_op_claim_manager.SetTransObject(SQLCA)
	
	li_dw_rowcount = lds_op_claim_manager.Retrieve()

	
	FOR li_counter = 1 TO li_upper
		INSERT #CLAIM_MANAGER
		(      user_id )
		SELECT :as_claim_manager_array[li_counter]
		USING SQLCA;
		SQLCA.nf_handle_error('w_overpayment_report','embedded SQL: INSERT #CLAIM_MANAGER...','wf_populate_claim_manager_table')
		
		// if the user has NOT selected ALL claim managers
		IF li_dw_rowcount <> li_upper THEN
			IF li_upper = 1 THEN
				ls_filter = as_claim_manager_array[li_counter]
			ELSE
				IF li_counter = 1 THEN
					ls_filter = as_claim_manager_array[li_counter]+ ', '
				ELSEIF li_counter = li_upper THEN
					ls_filter = ls_filter + ' ' +as_claim_manager_array[li_counter]
				ELSE
					ls_filter = ls_filter + ' ' +as_claim_manager_array[li_counter]+ ', '
				END IF
			END IF
		ELSE
			ls_filter = 'ALL'
		END IF
	NEXT
	
	is_claim_manager_filter = 'Claim Manager:  ' + ls_filter
END IF
//
//int li_table_count
//
//SELECT COUNT(*)
//INTO   :li_table_count
//FROM   #CLAIM_MANAGER
//USING SQLCA;
//SQLCA.nf_handle_error('w_overpayment_report','embedded SQL: count #CLAIM_MANAGER...','wf_populate_claim_manager_table')
//
//messagebox('#CLAIM_MANAGER record count',li_table_count)
end subroutine

public subroutine wf_populate_overpayment_status (string as_op_status_array[]);INTEGER    li_counter, li_upper, li_dw_rowcount, li_find
STRING     ls_filter, ls_op_desc, ls_find
U_DS       lds_op_status

/*

If overpayment status criteria have been selected, then
populate a temporary table from the selection.
This table will be used in the report retrieval

*/


li_upper = UpperBound(as_op_status_array)

// if some O/P statuses were selected
IF li_upper <> 0 THEN
	lds_op_status = Create U_DS
	lds_op_status.DataObject = 'd_op_status'
	lds_op_status.SetTransObject(SQLCA)
	
	li_dw_rowcount = lds_op_status.Retrieve()
	
	FOR li_counter = 1 TO li_upper
		INSERT #OVERPAYMENT_STATUS
		(      op_status_code )
		SELECT :as_op_status_array[li_counter]
		USING SQLCA;
		SQLCA.nf_handle_error('w_overpayment_report','embedded SQL: INSERT #OVERPAYMENT_STATUS...','wf_populate_overpayment_status')
			
		// if the user has NOT selected ALL O/P statuses
		IF li_dw_rowcount <> li_upper THEN
			ls_find = 'op_status_code = "' +as_op_status_array[li_counter]+ '"'
			li_find = lds_op_status.Find(ls_find,1,li_dw_rowcount)
			IF li_find > 0 THEN
				ls_op_desc = lds_op_status.GetItemString(li_find,'op_status_desc')
				IF li_upper = 1 THEN
					ls_filter = ls_op_desc
				ELSE
					IF li_counter = 1 THEN
						ls_filter = ls_op_desc+ ', '

					ELSEIF li_counter = li_upper THEN
						ls_filter = ls_filter + ' ' +ls_op_desc

					ELSE
						ls_filter = ls_filter + ' ' +ls_op_desc+ ', '
					END IF
				END IF
			END IF
			
		ELSE
			ls_filter = 'ALL'
		END IF
	NEXT
	
	is_op_status_filter = 'O/P Status:  ' + ls_filter + '~r~n'

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

public subroutine wf_populate_overpayment_type (string as_overpayment_type_array[]);INTEGER    li_counter, li_upper, li_dw_rowcount, li_find
STRING     ls_find, ls_filter, ls_op_type_desc
U_DS       lds_op_type

/*

If overpayment type criteria have been selected, then
populate a temporary table from the selection.
This table will be used in the report retrieval

*/


li_upper = UpperBound(as_overpayment_type_array)


// if some O/P types were selected
IF li_upper <> 0 THEN
	lds_op_type = Create U_DS
	lds_op_type.DataObject = 'd_op_type'
	lds_op_type.SetTransObject(SQLCA)
	
	li_dw_rowcount = lds_op_type.Retrieve()
	
	
	FOR li_counter = 1 TO li_upper
		INSERT #OVERPAYMENT_TYPE
		(      overpayment_type_code )
		SELECT :as_overpayment_type_array[li_counter]
		USING SQLCA;
		SQLCA.nf_handle_error('w_overpayment_report','embedded SQL: INSERT #OVERPAYMENT_TYPE...','wf_populate_overpayment_type')
		
		// if the user has NOT selected ALL O/P types
		IF li_dw_rowcount <> li_upper THEN
			ls_find = 'overpayment_type_code = "' +as_overpayment_type_array[li_counter]+ '"'
			li_find = lds_op_type.Find(ls_find,1,li_dw_rowcount)
			IF li_find > 0 THEN
				ls_op_type_desc = lds_op_type.GetItemString(li_find,'overpayment_type_desc')
				IF li_upper = 1 THEN
					ls_filter = ls_op_type_desc
				ELSE
					IF li_counter = 1 THEN
						ls_filter = ls_op_type_desc+ ', '

					ELSEIF li_counter = li_upper THEN
						ls_filter = ls_filter + ' ' +ls_op_type_desc

					ELSE
						ls_filter = ls_filter + ' ' +ls_op_type_desc+ ', '
					END IF
				END IF
				is_op_type_code = ls_filter + '~r~n'
			END IF
		ELSE
			is_op_type_code = 'ALL~r~n'
		END IF
	NEXT

END IF



//
//int li_table_count
//
//SELECT COUNT(*)
//INTO   :li_table_count
//FROM   #OVERPAYMENT_TYPE
//USING SQLCA;
//SQLCA.nf_handle_error('w_overpayment_report','embedded SQL: count #OVERPAYMENT_TYPE...','wf_populate_claim_manager_table')
//
//messagebox('#OVERPAYMENT_TYPE record count',li_table_count)
end subroutine

public subroutine wf_populate_overpayment_claim_region (string as_admin_region_array[]);INTEGER    li_counter, li_upper, li_dw_rowcount, li_find
STRING     ls_filter, ls_find, ls_admin_region
U_DS       lds_op_region

/*

If claim admin region criteria have been selected, then
populate a temporary table from the selection.
This table will be used in the report retrieval

*/


li_upper = UpperBound(as_admin_region_array)

// if some regions were selected
IF li_upper <> 0 THEN
	lds_op_region= Create U_DS
	lds_op_region.DataObject = 'd_op_region'
	lds_op_region.SetTransObject(SQLCA)
	
	li_dw_rowcount = lds_op_region.Retrieve()
	
	FOR li_counter = 1 TO li_upper
		INSERT #OVERPAYMENT_CLAIM_REGION
		(      admin_region_code )
		SELECT :as_admin_region_array[li_counter]
		USING SQLCA;
		SQLCA.nf_handle_error('w_overpayment_report','embedded SQL: INSERT #OVERPAYMENT_CLAIM_REGION...','wf_populate_overpayment_claim_region')
		
	   // if the user has NOT selected ALL regions
		IF li_dw_rowcount <> li_upper THEN
			ls_find = 'admin_region_code = "' +as_admin_region_array[li_counter]+ '"'
			li_find = lds_op_region.Find(ls_find,1,li_dw_rowcount)
			IF li_find > 0 THEN
				ls_admin_region = lds_op_region.GetItemString(li_find,'admin_region_code')
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
	
	is_admin_region = 'Admin Region:  ' + ls_filter + '~r~n'
END IF


//int li_table_count
//
//SELECT COUNT(*)
//INTO   :li_table_count
//FROM   #OVERPAYMENT_CLAIM_REGION
//USING SQLCA;
//SQLCA.nf_handle_error('w_overpayment_report','embedded SQL: count #OVERPAYMENT_CLAIM_REGION...','wf_populate_claim_manager_table')
//
//messagebox('#OVERPAYMENT_CLAIM_REGION record count',li_table_count)
end subroutine

public subroutine wf_create_temp_tables ();STRING    ls_claim_manager_array_table, ls_claim_status_and_type_array_table, ls_overpayment_status_array_table
STRING    ls_overpayment_type_array_table, ls_overpayment_region_array_table

STRING    ls_results_table, ls_balance_table, ls_correspondence_table
STRING    ls_last_op_adjustment_table, ls_last_op_payment_table, ls_last_op_finance_table


/*

-- ** FOR TESTING PURPOSES

IF OBJECT_ID('tempdb..#CLAIM_MANAGER') IS NOT NULL DROP TABLE #CLAIM_MANAGER
CREATE TABLE #CLAIM_MANAGER
(      user_id                      VARCHAR(16)   NOT NULL )

IF OBJECT_ID('tempdb..#CLAIM_STATUS') IS NOT NULL DROP TABLE #CLAIM_STATUS
CREATE TABLE #CLAIM_STATUS 
(      claim_status_code            CHAR(1)       NOT NULL )

IF OBJECT_ID('tempdb..#OVERPAYMENT_STATUS') IS NOT NULL DROP TABLE #OVERPAYMENT_STATUS
CREATE TABLE #OVERPAYMENT_STATUS
(      op_status_code               CHAR(3)       NOT NULL )

IF OBJECT_ID('tempdb..#OVERPAYMENT_TYPE') IS NOT NULL DROP TABLE #OVERPAYMENT_TYPE
CREATE TABLE #OVERPAYMENT_TYPE
(      overpayment_type_code        CHAR(1)       NOT NULL )

IF OBJECT_ID('tempdb..#OVERPAYMENT_CLAIM_REGION') IS NOT NULL DROP TABLE #OVERPAYMENT_CLAIM_REGION
CREATE TABLE #OVERPAYMENT_CLAIM_REGION
(      admin_region_code            CHAR(3)       NOT NULL )

IF OBJECT_ID('tempdb..#REPORT_RESULTS') IS NOT NULL DROP TABLE #REPORT_RESULTS
CREATE TABLE #REPORT_RESULTS
(       admin_region_code        CHAR(3)        NOT NULL,
        recipient_no             INT            NOT NULL,
        claim_no                 INT            NOT NULL,
        claim_manager_user_id    VARCHAR(16)    NOT NULL,
        claim_status             VARCHAR(50)    NOT NULL,
        last_name                VARCHAR(20)    NOT NULL,
        given_names              VARCHAR(20)    NOT NULL,
        overpayment_type_code    CHAR(1)        NOT NULL,
        balance_amount           MONEY          NOT NULL,
        first_op_record_date     SMALLDATETIME      NULL,
        op_status_desc           VARCHAR(40)    NOT NULL,
        last_letter_date         SMALLDATETIME      NULL,
        last_letter_type         VARCHAR(40)        NULL,
        last_payment_date        SMALLDATETIME      NULL,
        date_last_pay_app_by_finance     SMALLDATETIME      NULL,
        op_status_comment           VARCHAR(500)   NOT NULL )

IF OBJECT_ID('tempdb..#MAX_CORRESPONDENCE') IS NOT NULL DROP TABLE #MAX_CORRESPONDENCE
CREATE TABLE #MAX_CORRESPONDENCE
(      claim_no                 INT            NOT NULL,
       last_letter_date         SMALLDATETIME  NOT NULL,
       template_desc            VARCHAR(40)    NOT NULL )

IF OBJECT_ID('tempdb..#MAX_OP_ADJ_TXN') IS NOT NULL DROP TABLE #MAX_OP_ADJ_TXN
CREATE TABLE #MAX_OP_ADJ_TXN
(      claim_no                 INT            NOT NULL,
       recipient_no             INT            NOT NULL,
       last_op_adj_txn_date     SMALLDATETIME  NOT NULL )

IF OBJECT_ID('tempdb..#MAX_OP_PAY') IS NOT NULL DROP TABLE #MAX_OP_PAY
CREATE TABLE #MAX_OP_PAY
(      claim_no                 INT            NOT NULL,
       recipient_no             INT            NOT NULL,
       last_op_pay_date         SMALLDATETIME  NOT NULL )

IF OBJECT_ID('tempdb..#MAX_OP_FINANCE_DATE') IS NOT NULL DROP TABLE #MAX_OP_FINANCE_DATE
CREATE TABLE #MAX_OP_FINANCE_DATE
(      claim_no                 INT            NOT NULL,
       recipient_no             INT            NOT NULL,
       last_op_finance_date     SMALLDATETIME  NOT NULL )

*/


/* declare claim manager array table */
ls_claim_manager_array_table         = 'CREATE TABLE #CLAIM_MANAGER ' &
                                     + '(      user_id                      VARCHAR(16)   NOT NULL ) '
EXECUTE IMMEDIATE :ls_claim_manager_array_table ;
SQLCA.nf_handle_error('w_overpayment_report','wf_create_temp_tables','CREATE TABLE #CLAIM_MANAGER_ARRAY...')


/* declare claim status & type array table */
ls_claim_status_and_type_array_table = 'CREATE TABLE #CLAIM_STATUS' &
                                     + '(      claim_status_code            CHAR(1)       NOT NULL )'
EXECUTE IMMEDIATE :ls_claim_status_and_type_array_table ;
SQLCA.nf_handle_error('w_overpayment_report','wf_create_temp_tables','CREATE TABLE #CLAIM_STATUS...')


/* declare overpayment status array table */
ls_overpayment_status_array_table    = 'CREATE TABLE #OVERPAYMENT_STATUS ' &
                                     + '(      op_status_code               CHAR(3)       NOT NULL )'
EXECUTE IMMEDIATE :ls_overpayment_status_array_table ;
SQLCA.nf_handle_error('w_overpayment_report','wf_create_temp_tables','CREATE TABLE #OVERPAYMENT_STATUS...')


/* declare overpayment type array table */
ls_overpayment_type_array_table      = 'CREATE TABLE #OVERPAYMENT_TYPE ' &
                                     + '(      overpayment_type_code        CHAR(1)       NOT NULL )'
EXECUTE IMMEDIATE :ls_overpayment_type_array_table ;
SQLCA.nf_handle_error('w_overpayment_report','wf_create_temp_tables','CREATE TABLE #OVERPAYMENT_TYPE...')


/* declare overpayment claim region array table */
ls_overpayment_region_array_table    = 'CREATE TABLE #OVERPAYMENT_CLAIM_REGION ' &
                                     + '(      admin_region_code            CHAR(3)       NOT NULL )'
EXECUTE IMMEDIATE :ls_overpayment_region_array_table ;
SQLCA.nf_handle_error('w_overpayment_report','wf_create_temp_tables','CREATE TABLE #OVERPAYMENT_CLAIM_REGION...')



/* declare report results table */
ls_results_table            = 'CREATE TABLE #REPORT_RESULTS ' &
                            + '(       admin_region_code                CHAR(3)        NOT NULL,' &
                            + '        recipient_no                     INT            NOT NULL,' &
                            + '        claim_no                         INT            NOT NULL,' &
                            + '        claim_manager_user_id            VARCHAR(16)    NOT NULL,' &
                            + '        claim_status                     VARCHAR(50)    NOT NULL,' &
                            + '        last_name                        VARCHAR(20)    NOT NULL,' &
                            + '        given_names                      VARCHAR(20)    NOT NULL,' &
                            + '        overpayment_type_code            CHAR(1)        NOT NULL,' &
                            + '        balance_amount                   MONEY          NOT NULL,' &
                            + '        first_op_record_date             SMALLDATETIME      NULL,' &
                            + '        op_status_desc                   VARCHAR(40)    NOT NULL,' &
                            + '        last_letter_date                 SMALLDATETIME      NULL,' &
                            + '        last_letter_type                 VARCHAR(40)        NULL,' &
                            + '        last_payment_date                SMALLDATETIME      NULL,' &
                            + '        date_last_pay_app_by_finance     SMALLDATETIME      NULL,' &
                            + '        op_status_comment                VARCHAR(500)   NOT NULL )'
EXECUTE IMMEDIATE :ls_results_table ;
SQLCA.nf_handle_error('w_overpayment_report','wf_create_temp_tables','CREATE TABLE #REPORT_RESULTS...')


/* declare last overpayment correspondence temp table */
ls_correspondence_table     = 'CREATE TABLE #MAX_CORRESPONDENCE ' &
                            + '(       claim_no                         INT            NOT NULL,' &
                            + '        last_letter_date                 SMALLDATETIME  NOT NULL,' &
                            + '        template_desc                    VARCHAR(40)    NOT NULL )'
EXECUTE IMMEDIATE :ls_correspondence_table ;
SQLCA.nf_handle_error('w_overpayment_report','wf_create_temp_tables','CREATE TABLE #MAX_CORRESPONDENCE...')


/* declare last overpayment adjustment txn temp table (finance) */
ls_last_op_adjustment_table = 'CREATE TABLE #MAX_OP_ADJ_TXN ' &
                            + '(       claim_no                         INT            NOT NULL,' &
                            + '        recipient_no                     INT            NOT NULL,' &
                            + '        last_op_adj_txn_date             SMALLDATETIME  NOT NULL )'
EXECUTE IMMEDIATE :ls_last_op_adjustment_table ;
SQLCA.nf_handle_error('w_overpayment_report','wf_create_temp_tables','CREATE TABLE #MAX_OP_ADJ_TXN...')


/* declare last overpayment payment temp table (finance) */
ls_last_op_payment_table    = 'CREATE TABLE #MAX_OP_PAY ' &
                            + '(       claim_no                         INT            NOT NULL,' &
                            + '        recipient_no                     INT            NOT NULL,' &
                            + '        last_op_pay_date                 SMALLDATETIME  NOT NULL )'
EXECUTE IMMEDIATE :ls_last_op_payment_table ;
SQLCA.nf_handle_error('w_overpayment_report','wf_create_temp_tables','CREATE TABLE #MAX_OP_PAY...')


/* declare last overpayment finance temp table */
ls_last_op_finance_table    = 'CREATE TABLE #MAX_OP_FINANCE_DATE ' &
                            + '(       claim_no                         INT            NOT NULL,' &
                            + '        recipient_no                     INT            NOT NULL,' &
                            + '        last_op_finance_date             SMALLDATETIME  NOT NULL )'
EXECUTE IMMEDIATE :ls_last_op_finance_table ;
SQLCA.nf_handle_error('w_overpayment_report','wf_create_temp_tables','CREATE TABLE #MAX_OP_FINANCE_DATE...')




end subroutine

public subroutine wf_populate_claim_status_table (string as_claim_status_array[]);INTEGER    li_counter, li_upper, li_dw_rowcount, li_find
STRING     ls_filter, ls_find, ls_claim_status_desc
U_DS       lds_op_claim_status

/*

If claim status/status type criteria have been selected, then
populate a temporary table from the selection.
This table will be used in the report retrieval

*/


// if some claim statuses were selected
li_upper = UpperBound(as_claim_status_array)

IF li_upper <> 0 THEN
	lds_op_claim_status = Create U_DS
	lds_op_claim_status.DataObject = 'd_op_claim_status'
	lds_op_claim_status.SetTransObject(SQLCA)
	
	li_dw_rowcount = lds_op_claim_status.Retrieve()
	
	FOR li_counter = 1 TO li_upper
		INSERT #CLAIM_STATUS
		(      claim_status_code )
		SELECT :as_claim_status_array[li_counter]
		USING SQLCA;
		SQLCA.nf_handle_error('w_overpayment_report','embedded SQL: INSERT #CLAIM_STATUS...','wf_populate_claim_status_type_table')
		
		// if the user has NOT selected ALL claim statuses
		IF li_dw_rowcount <> li_upper THEN
			ls_find = 'claim_status_code = "' +as_claim_status_array[li_counter]+ '"'
			li_find = lds_op_claim_status.Find(ls_find,1,li_dw_rowcount)
			
			IF li_find > 0 THEN
				ls_claim_status_desc = lds_op_claim_status.GetItemString(li_find,'claim_status_desc')
				IF li_upper = 1 THEN
					ls_filter = ls_claim_status_desc

				ELSE
					IF li_counter = 1 THEN
						ls_filter = ls_claim_status_desc+ ', '
					ELSEIF li_counter = li_upper THEN
						ls_filter = ls_filter + ' ' +ls_claim_status_desc
					ELSE
						ls_filter = ls_filter + ' ' +ls_claim_status_desc+ ', '
					END IF
				END IF
			END IF
			is_claim_status_filter = ls_filter
		ELSE
			is_claim_status_filter = 'ALL'
		END IF
	NEXT
	
	is_claim_status_filter = 'Claim Status:  ' + is_claim_status_filter + '~r~n'

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

public function integer wf_process_tabpage_criteria (ref string as_claim_manager_array[], ref string as_ignore_claim_manager, ref string as_claim_status_array[], ref string as_ignore_claim_status, ref string as_op_status_array[], ref string as_ignore_op_status, ref string as_overpayment_type_array[], ref string as_ignore_overpayment_type, ref string as_admin_region_array[], ref string as_ignore_admin_region);INTEGER		li_upper, li_counter, li_upper2, li_counter2, li_msg
S_ARG_ARRAY lstr_arg_array[]
STRING      ls_current_key


// there are 5 tabs associated with this report
// call the object function to get arrays of arguments for dw_report

uo_report_selector_tabs.tab_tabpage_container.of_get_string_array(lstr_arg_array)

li_upper = UpperBound(lstr_arg_array)

for li_counter = 1 to li_upper
	li_upper2 = UpperBound(lstr_arg_array[li_counter].key)
	
	IF li_upper2 > 0 THEN
		FOR li_counter2 = 1 TO li_upper2
			ls_current_key = lstr_arg_array[li_counter].key[li_counter2]
			
			CHOOSE CASE ls_current_key
				CASE 'claim_manager_user_id'
					as_claim_manager_array = lstr_arg_array[li_counter].value
					IF lstr_arg_array[li_counter].populated_flag[1] ='Y' THEN
						as_ignore_claim_manager = 'N'
						ib_criteria_chosen      = TRUE
					ELSE
						as_ignore_claim_manager = 'Y'
						is_claim_manager_filter = ''
					END IF
					
				CASE 'claim_status_code'
					as_claim_status_array = lstr_arg_array[li_counter].value
					IF lstr_arg_array[li_counter].populated_flag[1] ='Y' THEN						
						as_ignore_claim_status = 'N'
						ib_criteria_chosen     = TRUE
					ELSE
						as_ignore_claim_status = 'Y'
						is_claim_status_filter = ''
					END IF
					
				CASE 'op_status_code'
					as_op_status_array = lstr_arg_array[li_counter].value
					IF lstr_arg_array[li_counter].populated_flag[1] ='Y' THEN
						as_ignore_op_status = 'N'
						ib_criteria_chosen  = TRUE
					ELSE
						as_ignore_op_status = 'Y'
						is_op_status_filter = ''
					END IF
	
				CASE 'overpayment_type_code'
					as_overpayment_type_array = lstr_arg_array[li_counter].value
					IF lstr_arg_array[li_counter].populated_flag[1] ='Y' THEN
						as_ignore_overpayment_type = 'N'
						ib_criteria_chosen         = TRUE
					ELSE
						as_ignore_overpayment_type = 'Y'
						is_op_type_code = ''
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
IF as_ignore_op_status = 'N'        THEN   wf_populate_overpayment_status(as_op_status_array)
IF as_ignore_overpayment_type = 'N' THEN   wf_populate_overpayment_type(as_overpayment_type_array)
IF as_ignore_claim_manager = 'N'    THEN   wf_populate_claim_manager_table(as_claim_manager_array)
IF as_ignore_claim_status = 'N'     THEN   wf_populate_claim_status_table(as_claim_status_array)
IF as_ignore_admin_region = 'N'     THEN   wf_populate_overpayment_claim_region(as_admin_region_array)


RETURN 0
end function

public subroutine wf_set_up_tab_pages ();STRING   ls_array_of_dataobjects[]



ls_array_of_dataobjects[1] = 'd_op_type'
ls_array_of_dataobjects[2] = 'd_op_region'
ls_array_of_dataobjects[3] = 'd_op_status'
ls_array_of_dataobjects[4] = 'd_op_claim_status'
ls_array_of_dataobjects[5] = 'd_op_claim_manager'

uo_report_selector_tabs.tab_tabpage_container.of_add_tabpages(ls_array_of_dataobjects)
end subroutine

public subroutine wf_drop_temp_tables ();STRING    ls_claim_manager_array_table, ls_claim_status_and_type_array_table, ls_overpayment_status_array_table
STRING    ls_overpayment_type_array_table, ls_overpayment_region_array_table
STRING    ls_results_table, ls_balance_table, ls_correspondence_table
STRING    ls_last_op_adjustment_table, ls_last_op_payment_table, ls_last_op_finance_table


/* drop claim manager array table */
ls_claim_manager_array_table         = 'DROP TABLE #CLAIM_MANAGER '
EXECUTE IMMEDIATE :ls_claim_manager_array_table ;
SQLCA.nf_handle_error('w_overpayment_report','wf_drop_temp_tables','DROP TABLE #CLAIM_MANAGER...')

/* drop claim status & type array table */
ls_claim_status_and_type_array_table = 'DROP TABLE #CLAIM_STATUS '
EXECUTE IMMEDIATE :ls_claim_status_and_type_array_table ;
SQLCA.nf_handle_error('w_overpayment_report','wf_drop_temp_tables','DROP TABLE #CLAIM_STATUS...')

/* drop overpayment status array table */
ls_overpayment_status_array_table    = 'DROP TABLE #OVERPAYMENT_STATUS '
EXECUTE IMMEDIATE :ls_overpayment_status_array_table ;
SQLCA.nf_handle_error('w_overpayment_report','wf_drop_temp_tables','DROP TABLE #OVERPAYMENT_STATUS...')

/* drop overpayment type array table */
ls_overpayment_type_array_table      = 'DROP TABLE #OVERPAYMENT_TYPE '
EXECUTE IMMEDIATE :ls_overpayment_type_array_table ;
SQLCA.nf_handle_error('w_overpayment_report','wf_drop_temp_tables','DROP TABLE #OVERPAYMENT_TYPE...')

/* drop overpayment claim region array table */
ls_overpayment_region_array_table    = 'DROP TABLE #OVERPAYMENT_CLAIM_REGION '
EXECUTE IMMEDIATE :ls_overpayment_region_array_table ;
SQLCA.nf_handle_error('w_overpayment_report','wf_drop_temp_tables','DROP TABLE #OVERPAYMENT_CLAIM_REGION...')

/* drop report results table */
ls_results_table            = 'DROP TABLE #REPORT_RESULTS '
EXECUTE IMMEDIATE :ls_results_table ;
SQLCA.nf_handle_error('w_overpayment_report','wf_drop_temp_tables','DROP TABLE #REPORT_RESULTS...')

/* drop last overpayment correspondence temp table */
ls_correspondence_table     = 'DROP TABLE #MAX_CORRESPONDENCE '
EXECUTE IMMEDIATE :ls_correspondence_table ;
SQLCA.nf_handle_error('w_overpayment_report','wf_drop_temp_tables','DROP TABLE #MAX_CORRESPONDENCE...')

/* drop last overpayment adjustment txn temp table (finance) */
ls_last_op_adjustment_table = 'DROP TABLE #MAX_OP_ADJ_TXN '
EXECUTE IMMEDIATE :ls_last_op_adjustment_table ;
SQLCA.nf_handle_error('w_overpayment_report','wf_drop_temp_tables','DROP TABLE #MAX_OP_ADJ_TXN...')

/* drop last overpayment payment temp table (finance) */
ls_last_op_payment_table    = 'DROP TABLE #MAX_OP_PAY '
EXECUTE IMMEDIATE :ls_last_op_payment_table ;
SQLCA.nf_handle_error('w_overpayment_report','wf_drop_temp_tables','DROP TABLE #MAX_OP_PAY...')

/* drop last overpayment finance temp table */
ls_last_op_finance_table    = 'DROP TABLE #MAX_OP_FINANCE_DATE '
EXECUTE IMMEDIATE :ls_last_op_finance_table ;
SQLCA.nf_handle_error('w_overpayment_report','wf_drop_temp_tables','DROP TABLE #MAX_OP_FINANCE_DATE...')



end subroutine

public subroutine wf_post_retrieval_reset ();
wf_drop_temp_tables()

wf_clear_instance_variables()
end subroutine

public subroutine wf_clear_instance_variables ();
// clear instance variables
is_op_date_range         = ''
is_op_fin_pay_date_range = ''
is_op_type_code          = ''
is_admin_region          = ''
is_op_status_filter      = ''
is_claim_status_filter   = ''
is_claim_manager_filter  = ''
ib_criteria_chosen       = FALSE
end subroutine

public function integer wf_validate_date_criteria (ref date adt_op_start, ref date adt_op_end, ref date adt_fin_start, ref date adt_fin_end, ref date adt_op_pay_start, ref date adt_op_pay_end);
//----------------- OP Create Date checks
/* do an accepttext on the OP create date criteria DW */
dw_op_create_date.AcceptText()

// grab the OP date range
adt_op_start = dw_op_create_date.GetItemDate(1,'date_range_start')
adt_op_end   = dw_op_create_date.GetItemDate(1,'date_range_end')

/* couple of checks here */
IF isnull(adt_op_start) THEN
	adt_op_start	= DATE('1900-01-01')
ELSE
	ib_criteria_chosen = TRUE
END IF

IF isnull(adt_op_end) 	THEN
	adt_op_end		= DATE('2079-06-06')
ELSE
	ib_criteria_chosen = TRUE
END IF

/* do a couple of checks */
IF adt_op_start > Date('2079-06-06') OR adt_op_start < Date('1900-01-01') THEN 
	messagebox('Validation Error', 'The Overpayment Start Date if entered, must be a valid date between 1900-01-01 and 2079-06-06')
	RETURN -1
END IF 

IF adt_op_end > Date('2079-06-06') OR adt_op_end < Date('1900-01-01') THEN 
	messagebox('Validation Error', 'The Overpayment End Date if entered, must be a valid date between 1900-01-01 and 2079-06-06')
	RETURN -1
END IF 

IF adt_op_start > adt_op_end THEN 
	messagebox('Validation Error', 'The Overpayment End Date if entered, cannot be greater then the Overpayment Start Date')
	RETURN -1
END IF 

// populate O/P date criteria string for report
IF adt_op_start = 1900-01-01 THEN
	IF adt_op_end = 2079-06-06 THEN
		// no range entered
		is_op_date_range = ''
	ELSE
		is_op_date_range = 'Ending at ' +String(adt_op_end)
	END IF
ELSE
	IF adt_op_end = 2079-06-06 THEN
		is_op_date_range = 'Starting at ' +String(adt_op_start)
	ELSE
		is_op_date_range = String(adt_op_start)+ ' to ' +String(adt_op_end)
	END IF
END IF
IF is_op_date_range <> '' THEN is_op_date_range = is_op_date_range + '~r~n'


//----------------- OP Finance Payment Date checks
/* do an accepttext on the OP finance payment date criteria DW */
dw_op_fin_pay_date.AcceptText()

// grab the OP date range
adt_fin_start = dw_op_fin_pay_date.GetItemDate(1,'date_range_start')
adt_fin_end   = dw_op_fin_pay_date.GetItemDate(1,'date_range_end')

/* couple of checks here */
IF isnull(adt_fin_start) THEN
	adt_fin_start = DATE('1900-01-01')
ELSE
	ib_criteria_chosen = TRUE
END IF

IF isnull(adt_fin_end)   THEN
	adt_fin_end   = DATE('2079-06-06')
ELSE
	ib_criteria_chosen = TRUE
END IF

/* do a couple of checks */
IF adt_fin_start > Date('2079-06-06') OR adt_fin_start < Date('1900-01-01') THEN 
	messagebox('Validation Error', 'The Finance Payment Start Date if entered, must be a valid date between 1900-01-01 and 2079-06-06')
	RETURN -1
END IF 

IF adt_fin_end > Date('2079-06-06') OR adt_fin_end < Date('1900-01-01') THEN 
	messagebox('Validation Error', 'The Finance Payment End Date if entered, must be a valid date between 1900-01-01 and 2079-06-06')
	RETURN -1
END IF 

IF adt_fin_start > adt_fin_end THEN 
	messagebox('Validation Error', 'The Finance Payment End Date if entered, cannot be greater then the Finance Payment Start Date')
	RETURN -1
END IF

// populate criteria string for report
IF adt_fin_start = 1900-01-01 THEN
	IF adt_fin_end = 2079-06-06 THEN
		// no range entered
		is_op_fin_pay_date_range = ''
	ELSE
		is_op_fin_pay_date_range = 'Ending at ' +String(adt_fin_end)
	END IF
ELSE
	IF adt_op_end = 2079-06-06 THEN
		is_op_fin_pay_date_range = 'Starting at ' +String(adt_fin_start)
	ELSE
		is_op_fin_pay_date_range = String(adt_fin_start)+ ' to ' +String(adt_fin_end)
	END IF
END IF
IF is_op_fin_pay_date_range <> '' THEN is_op_fin_pay_date_range = is_op_fin_pay_date_range + '~r~n'






//----------------- OP Last Payment Date checks
/* do an accepttext on the OP finance payment date criteria DW */
dw_op_last_payment_date.AcceptText()

// grab the OP date range
adt_op_pay_start = dw_op_last_payment_date.GetItemDate(1,'date_range_start')
adt_op_pay_end   = dw_op_last_payment_date.GetItemDate(1,'date_range_end')

/* couple of checks here */
IF isnull(adt_op_pay_start) THEN
	adt_op_pay_start = DATE('1900-01-01')
ELSE
	ib_criteria_chosen = TRUE
END IF

IF isnull(adt_op_pay_end)   THEN
	adt_op_pay_end   = DATE('2079-06-06')
ELSE
	ib_criteria_chosen = TRUE
END IF

/* do a couple of checks */
IF adt_op_pay_start > Date('2079-06-06') OR adt_op_pay_start < Date('1900-01-01') THEN 
	messagebox('Validation Error', 'The Last O/P Payment Start Date if entered, must be a valid date between 1900-01-01 and 2079-06-06')
	RETURN -1
END IF 

IF adt_op_pay_end > Date('2079-06-06') OR adt_op_pay_end < Date('1900-01-01') THEN 
	messagebox('Validation Error', 'The Last O/P Payment End Date if entered, must be a valid date between 1900-01-01 and 2079-06-06')
	RETURN -1
END IF 

IF adt_op_pay_start > adt_op_pay_end THEN 
	messagebox('Validation Error', 'The Last O/P Payment End Date if entered, cannot be greater then the Last O/P Payment Start Date')
	RETURN -1
END IF

// populate criteria string for report
IF adt_op_pay_start = 1900-01-01 THEN
	IF adt_op_pay_end = 2079-06-06 THEN
		// no range entered
		is_op_last_pay_date_range = ''
	ELSE
		is_op_last_pay_date_range = 'Ending at ' +String(adt_op_pay_end)
	END IF
ELSE
	IF adt_op_pay_end = 2079-06-06 THEN
		is_op_last_pay_date_range = 'Starting at ' +String(adt_op_pay_start)
	ELSE
		is_op_last_pay_date_range = String(adt_op_pay_start)+ ' to ' +String(adt_op_pay_end)
	END IF
END IF
IF is_op_last_pay_date_range <> '' THEN is_op_last_pay_date_range = is_op_last_pay_date_range + '~r~n'




RETURN 0


end function

public subroutine wf_populate_report_results_table (date adt_op_create_start, date adt_op_create_end, date adt_op_fin_pay_start, date adt_op_fin_pay_end, date adt_op_last_pay_start, date adt_op_last_pay_end, string as_ignore_op_status, string as_ignore_op_type, string as_ignore_claim_manager, string as_ignore_claim_status, string as_ignore_admin_region);/*

populate temp table (#REPORT_RESULTS) that serves as data source for dw_report

*/


/* populate initial data */
INSERT #REPORT_RESULTS
(      admin_region_code      ,
       recipient_no           ,
       claim_no               ,
       claim_manager_user_id  ,
       claim_status           ,
       last_name              ,
       given_names            ,
       overpayment_type_code  ,
       balance_amount         ,
       first_op_record_date   ,
       op_status_desc         ,
       last_payment_date      ,
       op_status_comment      )
SELECT d.admin_region_code                                             ,
       a.recipient_no                                                  ,
       a.claim_no                                                      ,
       d.claim_manager_user_id                                         ,
       CASE WHEN g.claim_status_type_desc = ''
            THEN f.claim_status_desc
            ELSE
                 f.claim_status_desc +' / '+ g.claim_status_type_desc
       END                                                             ,
       e.last_name                                                     ,
       e.given_names                                                   ,
       a.overpayment_type_code                                         ,
       a.balance_amount                                                ,
       CONVERT(VARCHAR(10),a.op_create_date,121) AS 'op_create_date'   ,
       c.op_status_desc                                                ,
       CONVERT(VARCHAR(10),b.create_date,121) AS 'last_payment_date'   ,
       a.comment
FROM   OVERPAYMENT_BALANCE   a
LEFT JOIN OVERPAYMENT_DETAIL b ON a.claim_no                = b.claim_no
                              AND a.overpayment_type_code   = b.overpayment_type_code
                              AND a.recipient_no            = b.recipient_no
                              AND a.recipient_type_code     = b.recipient_type_code 
                              AND a.recipient_sub_type_code = b.recipient_sub_type_code
										AND b.detail_amount           < 0.00
										AND b.create_date            >= :adt_op_last_pay_start
										AND b.create_date            <= :adt_op_last_pay_end
JOIN   Op_Status             c ON a.op_status_code = c.op_status_code
JOIN   CLAIM                 d ON a.claim_no = d.claim_no
JOIN   INDIVIDUAL            e ON a.recipient_no = e.individual_no
JOIN   Claim_Status          f ON d.claim_status_code = f.claim_status_code
LEFT JOIN Claim_Status_Type  g ON d.claim_status_type_code = g.claim_status_type_code
                                               AND d.claim_status_code = g.claim_status_code
WHERE  a.balance_amount <> 0.00
AND NOT EXISTS ( SELECT *
                 FROM   OVERPAYMENT_DETAIL f
                 WHERE  f.claim_no                = a.claim_no
                 AND    f.overpayment_type_code   = a.overpayment_type_code
                 AND    f.recipient_no            = a.recipient_no
                 AND    f.recipient_type_code     = a.recipient_type_code
                 AND    f.recipient_sub_type_code = a.recipient_sub_type_code
                 AND    f.overpayment_no         >  b.overpayment_no
                 AND    f.create_date            >= b.create_date
                 AND    f.detail_amount          <  0.00 )

AND   a.create_date    >= :adt_op_create_start
AND   a.create_date    <= :adt_op_create_end



AND ( EXISTS  ( SELECT *
                FROM   #OVERPAYMENT_STATUS g
					 WHERE  g.op_status_code = a.op_status_code )                 /* temp table populated using wf_populate_overpayment_status */
			OR   ( :as_ignore_op_status         = 'Y' ) ) 

AND ( EXISTS  ( SELECT *
                FROM   #OVERPAYMENT_TYPE h
					 WHERE  h.overpayment_type_code = a.overpayment_type_code )   /* temp table populated using wf_populate_overpayment_type */
			OR   ( :as_ignore_op_type           = 'Y' ) ) 

AND ( EXISTS  ( SELECT *
                FROM   #CLAIM_MANAGER i
					 WHERE  i.user_id = d.claim_manager_user_id )                 /* temp table populated using wf_populate_claim_manager_table */
			OR   ( :as_ignore_claim_manager     = 'Y' ) )

AND ( EXISTS  ( SELECT *
                FROM   #CLAIM_STATUS j
					 WHERE  j.claim_status_code      = d.claim_status_code )      /* temp table populated using wf_populate_claim_status_table */
			OR   ( :as_ignore_claim_status      = 'Y' ) )

AND ( EXISTS  ( SELECT *
                FROM   #OVERPAYMENT_CLAIM_REGION k
					 WHERE  k.admin_region_code = d.admin_region_code )           /* temp table populated using wf_populate_overpayment_claim_region */
			OR   ( :as_ignore_admin_region      = 'Y' ) )
USING SQLCA;
SQLCA.nf_handle_error('w_overpayment_report','embedded SQL: #REPORT_RESULTS','wf_populate_report_results_table')


// add index to improve retrieval
EXECUTE IMMEDIATE 'CREATE UNIQUE CLUSTERED INDEX idx1 ON #REPORT_RESULTS ( claim_no, recipient_no, overpayment_type_code )'
USING SQLCA;
SQLCA.nf_handle_error('w_overpayment_report','embedded SQL: CREATE UNIQUE CLUSTERED INDEX idx1','wf_populate_report_results_table')

//int li_table_count
//
//SELECT COUNT(*)
//INTO   :li_table_count
//FROM   #REPORT_RESULTS
//USING SQLCA;
//SQLCA.nf_handle_error('w_overpayment_report','embedded SQL: count #REPORT_RESULTS...','wf_populate_report_results_table')
//
//messagebox('#REPORT_RESULTS record count',li_table_count)



/* determine the latest correspondence associated with overpayments for records in results table */
INSERT #MAX_CORRESPONDENCE
(      claim_no,
       last_letter_date,
       template_desc )
SELECT a.claim_no,
       CONVERT(VARCHAR(10),a.create_date,121),
       b.template_desc
FROM   CORRESPONDENCE          a
JOIN   Correspondence_Template b ON a.template_code          = b.template_code
                                AND a.template_language_code = Left(b.template_version_no,1)
WHERE  a.template_code          IN ('OVRPYMT1','OVRPYMT2','OVRPYMT3','OVRPYMT6','OVRPYMT7')
AND    a.correspond_status_code = 'S'
AND     EXISTS ( SELECT *
                 FROM   #REPORT_RESULTS c
                 WHERE  c.claim_no = a.claim_no )
AND NOT EXISTS ( SELECT *
                 FROM   CORRESPONDENCE d
                 WHERE  d.claim_no               = a.claim_no
                 AND    d.template_code          IN ('OVRPYMT1','OVRPYMT2','OVRPYMT3','OVRPYMT6','OVRPYMT7')
					  AND    d.correspond_status_code = 'S'
                 AND    d.create_date            > a.create_date )
USING SQLCA;
SQLCA.nf_handle_error('w_overpayment_report','embedded SQL: INSERT #MAX_CORRESPONDENCE','wf_populate_report_results_table')

/* update last letter date & type */
UPDATE a
SET    a.last_letter_date = b.last_letter_date,
       a.last_letter_type = b.template_desc
FROM   #REPORT_RESULTS     a
JOIN   #MAX_CORRESPONDENCE b ON a.claim_no = b.claim_no
USING SQLCA;
SQLCA.nf_handle_error('w_overpayment_report','embedded SQL: UPDATE #REPORT_RESULTS from #MAX_CORRESPONDENCE','wf_populate_report_results_table')


/* determine max processed date of overpayment adjustment txn associated with claims in results table */
INSERT #MAX_OP_ADJ_TXN
(      claim_no             ,
       recipient_no         ,
       last_op_adj_txn_date )
SELECT a.claim_no,
       a.recipient_no,
       CONVERT(VARCHAR(10),a.processed_date,121)
FROM   APPLIED_CLAIM_TXN a
WHERE  a.txn_type_code       = 'J'
AND    a.txn_sub_type_code   = '3'
AND    a.recipient_type_code = 'I'
AND    convert(varchar(10),a.processed_date,121) >= :adt_op_fin_pay_start
AND    convert(varchar(10),a.processed_date,121) <= :adt_op_fin_pay_end  
AND     EXISTS ( SELECT *
                 FROM   #REPORT_RESULTS b
                 WHERE  b.claim_no     = a.claim_no
					  AND    b.recipient_no = a.recipient_no)
AND NOT EXISTS ( SELECT *
                 FROM   APPLIED_CLAIM_TXN c
                 WHERE  c.claim_no            = a.claim_no
					  AND    c.recipient_no        = a.recipient_no
                 AND    c.processed_date     >= a.processed_date 
                 AND    c.txn_no              > a.txn_no
                 AND    c.txn_type_code       = 'J'
                 AND    c.txn_sub_type_code   = '3'
					  AND    c.recipient_type_code = 'I' )
USING SQLCA;
SQLCA.nf_handle_error('w_overpayment_report','embedded SQL: INSERT #MAX_OP_ADJ_TXN','wf_populate_report_results_table')


/* determine max processed date of overpayment-related payments associated with claims in results table */
INSERT #MAX_OP_PAY
(      claim_no            ,
       recipient_no        ,
       last_op_pay_date    )
SELECT a.claim_no,
       b.recipient_no,
       CONVERT(VARCHAR(10),a.processed_date,121)
FROM   PAYMENT           a
JOIN   APPLIED_CLAIM_TXN b ON a.payment_no = b.payment_no
WHERE  a.payment_type_code     = '97'
AND    a.payment_sub_type_code IN ('CO','DO','OD')
AND    convert(varchar(10),a.processed_date,121) >= :adt_op_fin_pay_start
AND    convert(varchar(10),a.processed_date,121) <= :adt_op_fin_pay_end
AND    b.recipient_type_code   = 'I'
AND     EXISTS ( SELECT *
                 FROM   #REPORT_RESULTS c
                 WHERE  c.claim_no     = a.claim_no
					  AND    c.recipient_no = b.recipient_no )
AND NOT EXISTS ( SELECT *
                 FROM   PAYMENT           d
					  JOIN   APPLIED_CLAIM_TXN e ON d.payment_no = e.payment_no
                 WHERE  d.claim_no              = a.claim_no
                 and    d.processed_date       >= a.processed_date 
                 AND    d.payment_no            > a.payment_no
                 AND    d.payment_type_code     = '97'
                 AND    d.payment_sub_type_code IN ('CO','DO','OD')
					  AND    e.recipient_type_code   = 'I'
					  AND    e.recipient_no          = b.recipient_no )
USING SQLCA;
SQLCA.nf_handle_error('w_overpayment_report','embedded SQL: INSERT #MAX_OP_PAY','wf_populate_report_results_table')

/* determine the max date between overpayment adjustment txns and overpayment-related payments associated with claims in results table */
INSERT #MAX_OP_FINANCE_DATE
(      claim_no,
       recipient_no,
       last_op_finance_date )
SELECT a.claim_no,
       a.recipient_no,
       a.last_op_adj_txn_date
FROM   #MAX_OP_ADJ_TXN a
JOIN   #MAX_OP_PAY     b ON a.claim_no             = b.claim_no
                        AND a.recipient_no         = b.recipient_no
                        AND a.last_op_adj_txn_date > b.last_op_pay_date
USING SQLCA;
SQLCA.nf_handle_error('w_overpayment_report','embedded SQL: INSERT #MAX_OP_FINANCE_DATE(1)','wf_populate_report_results_table')

INSERT #MAX_OP_FINANCE_DATE
(      claim_no,
       recipient_no,
       last_op_finance_date )
SELECT b.claim_no,
       b.recipient_no,
       b.last_op_pay_date
FROM   #MAX_OP_ADJ_TXN a
JOIN   #MAX_OP_PAY     b ON a.claim_no              = b.claim_no
                        AND a.recipient_no          = b.recipient_no
                        AND a.last_op_adj_txn_date <= b.last_op_pay_date
USING SQLCA;
SQLCA.nf_handle_error('w_overpayment_report','embedded SQL: INSERT #MAX_OP_FINANCE_DATE(2)','wf_populate_report_results_table')

INSERT #MAX_OP_FINANCE_DATE
(      claim_no,
       recipient_no,
       last_op_finance_date )
SELECT a.claim_no,
       a.recipient_no,
       a.last_op_adj_txn_date
FROM   #MAX_OP_ADJ_TXN a
WHERE NOT EXISTS ( SELECT *
                   FROM   #MAX_OP_PAY b
                   WHERE  b.claim_no     = a.claim_no
						 AND    b.recipient_no = a.recipient_no )
USING SQLCA;
SQLCA.nf_handle_error('w_overpayment_report','embedded SQL: INSERT #MAX_OP_FINANCE_DATE(3)','wf_populate_report_results_table')

INSERT #MAX_OP_FINANCE_DATE
(      claim_no,
       recipient_no,
       last_op_finance_date )
SELECT a.claim_no,
       a.recipient_no,
       a.last_op_pay_date
FROM   #MAX_OP_PAY a
WHERE NOT EXISTS ( SELECT *
                   FROM   #MAX_OP_ADJ_TXN b
                   WHERE  b.claim_no     = a.claim_no
						 AND    b.recipient_no = a.recipient_no )
USING SQLCA;
SQLCA.nf_handle_error('w_overpayment_report','embedded SQL: INSERT #MAX_OP_FINANCE_DATE(4)','wf_populate_report_results_table')


/* update the max 'payment by finance' date in results table */
UPDATE a
SET    a.date_last_pay_app_by_finance = b.last_op_finance_date
FROM   #REPORT_RESULTS      a
JOIN   #MAX_OP_FINANCE_DATE b ON a.claim_no     = b.claim_no
                             AND a.recipient_no = b.recipient_no
USING SQLCA;
SQLCA.nf_handle_error('w_overpayment_report','embedded SQL: UPDATE #REPORT_RESULTS from #MAX_OP_FINANCE_DATE','wf_populate_report_results_table')



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

on w_overpayment_report.create
int iCurrent
call super::create
this.cb_print=create cb_print
this.cb_close=create cb_close
this.uo_report_selector_tabs=create uo_report_selector_tabs
this.cb_retrieve=create cb_retrieve
this.dw_op_create_date=create dw_op_create_date
this.dw_op_fin_pay_date=create dw_op_fin_pay_date
this.cb_clear=create cb_clear
this.cb_extract=create cb_extract
this.p_expand=create p_expand
this.dw_op_last_payment_date=create dw_op_last_payment_date
this.st_1=create st_1
this.st_2=create st_2
this.st_3=create st_3
this.st_4=create st_4
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_print
this.Control[iCurrent+2]=this.cb_close
this.Control[iCurrent+3]=this.uo_report_selector_tabs
this.Control[iCurrent+4]=this.cb_retrieve
this.Control[iCurrent+5]=this.dw_op_create_date
this.Control[iCurrent+6]=this.dw_op_fin_pay_date
this.Control[iCurrent+7]=this.cb_clear
this.Control[iCurrent+8]=this.cb_extract
this.Control[iCurrent+9]=this.p_expand
this.Control[iCurrent+10]=this.dw_op_last_payment_date
this.Control[iCurrent+11]=this.st_1
this.Control[iCurrent+12]=this.st_2
this.Control[iCurrent+13]=this.st_3
this.Control[iCurrent+14]=this.st_4
end on

on w_overpayment_report.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_print)
destroy(this.cb_close)
destroy(this.uo_report_selector_tabs)
destroy(this.cb_retrieve)
destroy(this.dw_op_create_date)
destroy(this.dw_op_fin_pay_date)
destroy(this.cb_clear)
destroy(this.cb_extract)
destroy(this.p_expand)
destroy(this.dw_op_last_payment_date)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.st_4)
end on

event open;call super::open;


dw_op_create_date.SetTransObject(SQLCA)
dw_op_create_date.InsertRow(0)

dw_op_fin_pay_date.SetTransObject(SQLCA)
dw_op_fin_pay_date.InsertRow(0)

dw_op_last_payment_date.SetTransObject(SQLCA)
dw_op_last_payment_date.InsertRow(0)


il_design_time_width  = 3100
il_design_time_height = 2500

This.wf_SetResize(True)

THIS.inv_resize.of_register(dw_report,'ScaleToRight&Bottom')
THIS.inv_resize.of_register(uo_report_selector_tabs,0,0,100,0)
THIS.inv_resize.of_register(cb_print,100,100,0,0)
THIS.inv_resize.of_register(cb_close,100,100,0,0)
THIS.inv_resize.of_register(cb_extract,'FixedToBottom')
THIS.inv_resize.of_register(p_expand,'FixedToBottom')



wf_set_up_tab_pages()



/* for testing purposes */

//DATETIME ldtm_start, ldtm_end
//
//ldtm_start = datetime('2010-01-01')
//ldtm_end   = datetime(today())
//
//dw_op_create_date.SetItem(1,'date_range_start', ldtm_start)
//dw_op_create_date.SetItem(1,'date_range_end',   ldtm_end)
//dw_op_create_date.AcceptText()
//
//dw_op_fin_pay_date.SetItem(1,'date_range_start', ldtm_start)
//dw_op_fin_pay_date.SetItem(1,'date_range_end',   ldtm_end)
//dw_op_fin_pay_date.AcceptText()

iw_win = wf_get_window_reference()


wf_setup_tabpg_tooltips()
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

type dw_report from w_a_report`dw_report within w_overpayment_report
integer x = 50
integer y = 1196
integer width = 2994
integer height = 1176
integer taborder = 0
string title = "Overpayment Summary Report"
string dataobject = "d_overpayment_summary"
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


lm_popup.m_options.PopMenu(iw_win.PointerX( ), iw_win.PointerY( ))

Destroy lm_popup
end event

event dw_report::resize;call super::resize;// move p_expand

p_expand.Y = THIS.Y
p_expand.X = THIS.Width - 55
end event

type cb_print from commandbutton within w_overpayment_report
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

type cb_close from commandbutton within w_overpayment_report
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

type uo_report_selector_tabs from uo_report_criteria_selector within w_overpayment_report
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

type cb_retrieve from commandbutton within w_overpayment_report
integer x = 2912
integer y = 540
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

event clicked;DATE			ldt_op_start, ldt_op_end, ldt_fin_start, ldt_fin_end, ldt_last_pay_start, ldt_last_pay_end
INTEGER		li_rtn, li_rowcount

STRING      ls_claim_manager_array[], ls_ignore_claim_manager
STRING      ls_claim_status_array[], ls_ignore_claim_status, ls_claim_status_type_array[], ls_ignore_claim_status_type
STRING      ls_op_status_array[], ls_ignore_op_status
STRING      ls_overpayment_type_array[], ls_ignore_overpayment_type
STRING      ls_admin_region_array[], ls_ignore_admin_region



SetPointer(HourGlass!)

// validate date criteria
li_rtn = wf_validate_date_criteria(ldt_op_start, ldt_op_end, ldt_fin_start, ldt_fin_end, ldt_last_pay_start, ldt_last_pay_end)
IF li_rtn < 0 THEN RETURN


dw_report.SetRedraw(FALSE)

// process the selected values on the tabpages
li_rtn = wf_process_tabpage_criteria(ls_claim_manager_array,     ls_ignore_claim_manager    , &
                                     ls_claim_status_array,      ls_ignore_claim_status     , &
												 ls_op_status_array,         ls_ignore_op_status        , &
												 ls_overpayment_type_array,  ls_ignore_overpayment_type , &
												 ls_admin_region_array,      ls_ignore_admin_region )
IF li_rtn < 0 THEN RETURN

// function processes temp tables to populate results table
wf_populate_report_results_table(ldt_op_start, ldt_op_end, ldt_fin_start, ldt_fin_end, ldt_last_pay_start, ldt_last_pay_end, &
                              +  ls_ignore_op_status,         &
										+ 	ls_ignore_overpayment_type,  &
										+  ls_ignore_claim_manager,     &
										+  ls_ignore_claim_status,      &
										+ 	ls_ignore_admin_region)

li_rowcount = dw_report.Retrieve(is_op_date_range, is_op_fin_pay_date_range, is_op_last_pay_date_range, is_op_type_code, is_admin_region, is_op_status_filter, is_claim_status_filter, is_claim_manager_filter)
SQLCA.nf_handle_error('w_overpayment_report','dw_report.Retrieve','cb_retrieve.clicked')

//messagebox('after retrieval rowcount',li_rowcount)

IF li_rowcount = 0 THEN
	messagebox('No records','The selected criteria did not return any records.',Exclamation!)
END IF

// reset temp tables, variables after retrieval
wf_post_retrieval_reset()


dw_report.SETREDRAW(TRUE)

end event

type dw_op_create_date from u_dw_online within w_overpayment_report
string tag = "this is a test"
integer x = 2226
integer y = 228
integer width = 663
integer height = 260
integer taborder = 10
boolean bringtotop = true
string dataobject = "d_op_create_date_search_criteria"
borderstyle borderstyle = stylelowered!
end type

type dw_op_fin_pay_date from u_dw_online within w_overpayment_report
integer x = 2226
integer y = 556
integer width = 663
integer height = 260
integer taborder = 20
boolean bringtotop = true
string dataobject = "d_op_fin_pay_search_criteria"
borderstyle borderstyle = stylelowered!
end type

type cb_clear from commandbutton within w_overpayment_report
integer x = 2912
integer y = 704
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
dw_op_create_date.reset()
dw_op_create_date.insertrow(0)

dw_op_fin_pay_date.reset()
dw_op_fin_pay_date.insertrow(0)

dw_op_last_payment_date.reset()
dw_op_last_payment_date.insertrow(0)


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

wf_create_temp_tables()

li_rowcount = dw_report.Retrieve(is_op_date_range, is_op_fin_pay_date_range, is_op_last_pay_date_range, is_op_type_code, is_admin_region, is_op_status_filter, is_claim_status_filter, is_claim_manager_filter)
SQLCA.nf_handle_error('w_overpayment_report','dw_report.Retrieve','cb_retrieve.clicked')

wf_drop_temp_tables()

THIS.SetRedraw(TRUE)
end event

type cb_extract from commandbutton within w_overpayment_report
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

type p_expand from picture within w_overpayment_report
integer x = 2935
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

type dw_op_last_payment_date from u_dw_online within w_overpayment_report
integer x = 2226
integer y = 884
integer width = 663
integer height = 260
integer taborder = 30
boolean bringtotop = true
string dataobject = "d_op_last_payment_search_criteria"
borderstyle borderstyle = stylelowered!
end type

type st_1 from statictext within w_overpayment_report
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

type st_2 from statictext within w_overpayment_report
integer x = 69
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

type st_3 from statictext within w_overpayment_report
integer x = 1454
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

type st_4 from statictext within w_overpayment_report
integer x = 2354
integer y = 48
integer width = 827
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
string text = "Optional Filter by Dates"
boolean focusrectangle = false
end type

