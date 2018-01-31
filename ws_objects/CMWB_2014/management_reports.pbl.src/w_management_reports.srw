$PBExportHeader$w_management_reports.srw
forward
global type w_management_reports from w_a_tool
end type
type dw_details from u_dw_online within w_management_reports
end type
type dw_search from u_dw_online within w_management_reports
end type
type uo_tab_container from uo_report_criteria_selector within w_management_reports
end type
type ddlb_report_type from dropdownlistbox within w_management_reports
end type
type cb_retrieve from commandbutton within w_management_reports
end type
type cb_clear from commandbutton within w_management_reports
end type
type cb_1 from commandbutton within w_management_reports
end type
type cb_2 from commandbutton within w_management_reports
end type
type st_1 from statictext within w_management_reports
end type
type cb_filter from commandbutton within w_management_reports
end type
type st_2 from statictext within w_management_reports
end type
type st_3 from statictext within w_management_reports
end type
type cb_print from commandbutton within w_management_reports
end type
type rb_detail from radiobutton within w_management_reports
end type
type rb_summary from radiobutton within w_management_reports
end type
type p_expand from picture within w_management_reports
end type
type cb_extract from commandbutton within w_management_reports
end type
type cb_current_filter from commandbutton within w_management_reports
end type
type rb_totals from radiobutton within w_management_reports
end type
type cb_add_note from commandbutton within w_management_reports
end type
type gb_1 from groupbox within w_management_reports
end type
end forward

global type w_management_reports from w_a_tool
integer width = 3200
integer height = 1804
dw_details dw_details
dw_search dw_search
uo_tab_container uo_tab_container
ddlb_report_type ddlb_report_type
cb_retrieve cb_retrieve
cb_clear cb_clear
cb_1 cb_1
cb_2 cb_2
st_1 st_1
cb_filter cb_filter
st_2 st_2
st_3 st_3
cb_print cb_print
rb_detail rb_detail
rb_summary rb_summary
p_expand p_expand
cb_extract cb_extract
cb_current_filter cb_current_filter
rb_totals rb_totals
cb_add_note cb_add_note
gb_1 gb_1
end type
global w_management_reports w_management_reports

type variables
STRING		is_current_filter, is_dw_retrieve_action, is_report_syntax
LONG			il_normal_y, il_normal_height

end variables

forward prototypes
public subroutine wf_set_sort (string as_report_type)
public subroutine wf_set_filter (string as_filter)
public subroutine wf_select_first_row ()
public subroutine wf_set_filter_on_report (string as_filter)
public subroutine wf_shrink_enlarge (string as_arg)
public function string wf_get_current_filter ()
public subroutine wf_rb_button_click (string as_button)
public function string wf_create_where_clause (string as_additional_where, string as_source)
end prototypes

public subroutine wf_set_sort (string as_report_type);STRING	ls_sort

//specfic to whatever the report is and the sort you want to use.
CHOOSE CASE as_report_type
	CASE 'Return To Work (RTW)'
		
		ls_sort = 'group_order, claim_no, inital_assessment_report_no '
		
	CASE 'Volumes and Cost'
		
		ls_sort = 'claim_no, inital_assessment_report_no '
			
END CHOOSE

//this needs to be here to recalc the groups
dw_details.SetRedraw(FALSE)
dw_details.SetSort(ls_sort)
dw_details.Sort()
dw_details.GroupCalc()
dw_details.SetRedraw(TRUE)
end subroutine

public subroutine wf_set_filter (string as_filter);//set filter to nothing
dw_details.SetRedraw(false)
dw_details.setfilter('')
dw_details.Filter()
dw_details.SetRedraw(true)


/* only filter if there are rows */
IF TRIM(as_filter) > ''  THEN
	
	dw_details.SetRedraw(false)
	
	// filter
	dw_details.setfilter(as_filter)
	dw_details.Filter()
	dw_details.SetRedraw(true)
	
	is_current_filter = as_filter
	
END IF 
end subroutine

public subroutine wf_select_first_row ();LONG 	ll_claim_no

/* select the first row in the list */
IF dw_details.rowcount() > 0 THEN
	
	dw_details.selectrow(0, FALSE)	
	dw_details.selectrow(1, TRUE)	
	
	//set the filter on report if applicable
	CHOOSE CASE ddlb_report_type.TEXT
		CASE "Volumes and Cost"	
				
		CASE "Return To Work (RTW)"
			
				ll_claim_no = dw_details.getitemnumber(1,'claim_no')
		
				/* for testing bring back the claim tobstone and the documents */
				IF ll_claim_no > 0 THEN 
					iw_active_sheet.wf_set_claim(ll_claim_no)
				END IF 
			
		CASE "Therapist By Clinic"
										
		CASE	 ELSE
	END CHOOSE
	
END IF 
end subroutine

public subroutine wf_set_filter_on_report (string as_filter);STRING  ls_report

ls_report = TRIM(ddlb_report_type.text)

CHOOSE CASE ls_report
	CASE "Return To Work (RTW)", "Volumes and Cost"	, "Therapists By Clinic"
		
		dw_details.setredraw(false)
		dw_details.Modify("t_filter_info.Text= '" + as_filter + "'")
		dw_details.setredraw(true)
						
	CASE	 ELSE
END CHOOSE

end subroutine

public subroutine wf_shrink_enlarge (string as_arg);
LONG			li_row

IF as_arg = "ENLARGE" THEN

	il_normal_y 			= dw_details.y
	il_normal_height 	= dw_details.height
	
	dw_details.y			= st_1.y
	dw_details.height 	= il_normal_height + (il_normal_y - st_1.y)
	
ELSE
	dw_details.y 		= uo_tab_container.y + uo_tab_container.height + 10
  	dw_details.height 	= p_expand.y - ( uo_tab_container.y + uo_tab_container.height) - 30

END IF 

li_row = dw_details.getrow()
IF li_row > 0 THEN 
	dw_details.scrolltorow(li_row)
	//check to see if I need to highlaight this row
END IF 
end subroutine

public function string wf_get_current_filter ();STRING ls_filter

IF dw_details.ROWCOUNT() > 0 THEN
	ls_filter = dw_details.object.t_filter_info.Text
	IF isnull(ls_filter) THEN ls_filter = ''
END IF 

RETURN ls_filter
end function

public subroutine wf_rb_button_click (string as_button);DATE			ldt_start, ldt_end
STRING		ls_filter
INTEGER		li_rowcount

// no report selected
IF ddlb_report_type.text = '' THEN RETURN
 
// casse for specific code
CHOOSE CASE as_button
		
	CASE 'rb_detail'
		
			CHOOSE CASE ddlb_report_type.TEXT
				CASE "Volumes and Cost"		
					
					// set the dataobject
					dw_details.dataobject = 'd_mr_diagnostic_group_report'
				CASE "Return To Work (RTW)"
									
					// set the dataobject
					dw_details.dataobject = 'd_mr_return_to_work_sp_detail'
							
				CASE	 ELSE
			END CHOOSE
		
	CASE 'rb_summary'
		
			CHOOSE CASE ddlb_report_type.TEXT
				CASE "Volumes and Cost"		
					
						// set the dataobject
					dw_details.dataobject = 'd_mr_diagnostic_group_report_summary'
				CASE "Return To Work (RTW)"
					
					// set the dataobject
					dw_details.dataobject = 'd_mr_return_to_work_sp_summary'
							
				CASE	 ELSE
			END CHOOSE
	

							
	CASE 'rb_totals'
					
			dw_details.dataobject = "d_mr_return_to_work_sp_counts"

		
END CHOOSE

cb_retrieve.triggerevent('clicked')

end subroutine

public function string wf_create_where_clause (string as_additional_where, string as_source);STRING	 	ls_original_select, ls_group_by, ls_sql_groupby, ls_modified_sql, ls_sql_orderby
INT			li_where_pos, li_groupby_pos, li_orderby_pos

//NOTE: for some stupid reason the where clause is case sensitive

//grab the data syntax from the datawindow
ls_original_select = as_source

// need to replace single quotes with dbls
long start_pos=1
string old_str, new_str, mystring

mystring 		= ls_original_select
old_str 		= "'"
new_str 		= '"'

// Find the first occurrence of old_str.
start_pos = Pos(mystring, old_str, start_pos)

// Only enter the loop if you find old_str.

DO WHILE start_pos > 0

    // Replace old_str with new_str.
    mystring = Replace(mystring, start_pos, Len(old_str), new_str)

    // Find the next occurrence of old_str.
    start_pos = Pos(mystring, old_str,  start_pos+Len(new_str))

LOOP

ls_original_select = lower(mystring)


//find the where position if there is one
li_where_pos 	= pos( ls_original_select  , "where" )
li_groupby_pos = pos( ls_original_select  , "group by" )
li_orderby_pos = pos( ls_original_select  , "order by" )

// group by information & select
IF li_groupby_pos > 0 THEN 
	ls_sql_groupby = MID(ls_original_select, li_groupby_pos )
	ls_original_select = left(ls_original_select, li_groupby_pos -1)
ELSE
	ls_sql_groupby = ''
END IF 

// order by information
IF li_orderby_pos > 0 THEN 
	ls_sql_orderby = MID(ls_original_select, li_orderby_pos )
	
	IF li_groupby_pos = 0  THEN 
		ls_original_select = left(ls_original_select, li_orderby_pos -1)
	END IF 
	
ELSE
	ls_sql_orderby = ''
END IF 

IF len(ls_original_select) > 0  THEN 	
	ls_modified_sql = ls_original_select + ' '  + as_additional_where + '  ' +  ls_sql_groupby	+ '  ' +  ls_sql_orderby	
END IF 

RETURN ls_modified_sql
end function

on w_management_reports.create
int iCurrent
call super::create
this.dw_details=create dw_details
this.dw_search=create dw_search
this.uo_tab_container=create uo_tab_container
this.ddlb_report_type=create ddlb_report_type
this.cb_retrieve=create cb_retrieve
this.cb_clear=create cb_clear
this.cb_1=create cb_1
this.cb_2=create cb_2
this.st_1=create st_1
this.cb_filter=create cb_filter
this.st_2=create st_2
this.st_3=create st_3
this.cb_print=create cb_print
this.rb_detail=create rb_detail
this.rb_summary=create rb_summary
this.p_expand=create p_expand
this.cb_extract=create cb_extract
this.cb_current_filter=create cb_current_filter
this.rb_totals=create rb_totals
this.cb_add_note=create cb_add_note
this.gb_1=create gb_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_details
this.Control[iCurrent+2]=this.dw_search
this.Control[iCurrent+3]=this.uo_tab_container
this.Control[iCurrent+4]=this.ddlb_report_type
this.Control[iCurrent+5]=this.cb_retrieve
this.Control[iCurrent+6]=this.cb_clear
this.Control[iCurrent+7]=this.cb_1
this.Control[iCurrent+8]=this.cb_2
this.Control[iCurrent+9]=this.st_1
this.Control[iCurrent+10]=this.cb_filter
this.Control[iCurrent+11]=this.st_2
this.Control[iCurrent+12]=this.st_3
this.Control[iCurrent+13]=this.cb_print
this.Control[iCurrent+14]=this.rb_detail
this.Control[iCurrent+15]=this.rb_summary
this.Control[iCurrent+16]=this.p_expand
this.Control[iCurrent+17]=this.cb_extract
this.Control[iCurrent+18]=this.cb_current_filter
this.Control[iCurrent+19]=this.rb_totals
this.Control[iCurrent+20]=this.cb_add_note
this.Control[iCurrent+21]=this.gb_1
end on

on w_management_reports.destroy
call super::destroy
destroy(this.dw_details)
destroy(this.dw_search)
destroy(this.uo_tab_container)
destroy(this.ddlb_report_type)
destroy(this.cb_retrieve)
destroy(this.cb_clear)
destroy(this.cb_1)
destroy(this.cb_2)
destroy(this.st_1)
destroy(this.cb_filter)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.cb_print)
destroy(this.rb_detail)
destroy(this.rb_summary)
destroy(this.p_expand)
destroy(this.cb_extract)
destroy(this.cb_current_filter)
destroy(this.rb_totals)
destroy(this.cb_add_note)
destroy(this.gb_1)
end on

event open;call super::open;INTEGER		 li_counter, li_upperbound_check

dw_search.SetTransObject(sqlca)
dw_search.InsertRow(0)

li_upperbound_check = upperbound( uo_tab_container.Control[])

object winobjecttype[]
FOR li_counter = 1 to  upperbound( uo_tab_container.Control[])	
	 winobjecttype[li_counter] = TypeOf(Control[li_counter])	
NEXT

IF IsNull(inv_resize) OR NOT IsValid (inv_resize) THEN
	inv_resize = CREATE n_resize
	inv_resize.of_SetOrigSize (3000,1792)
END IF

THIS.inv_resize.of_register(dw_details,0,0,100,100)
THIS.inv_resize.of_register(cb_close,'FixedToBottom')
THIS.inv_resize.of_register(cb_print,'FixedToBottom')
THIS.inv_resize.of_register(gb_1,'FixedToBottom')
THIS.inv_resize.of_register(rb_detail,'FixedToBottom')
THIS.inv_resize.of_register(rb_summary,'FixedToBottom')
THIS.inv_resize.of_register(rb_totals,'FixedToBottom')
THIS.inv_resize.of_register(uo_tab_container,0,0,100,0)
THIS.inv_resize.of_register(p_expand,'FixedToBottom')
THIS.inv_resize.of_register(cb_extract,'FixedToBottom')
THIS.inv_resize.of_register(cb_filter,'FixedToBottom')

/*defaults for reports */
rb_detail.visible 			= FALSE
rb_summary.visible 		= FALSE
rb_totals.visible 			= FALSE
gb_1.visible 					= FALSE
cb_current_filter.visible 	= FALSE

end event

event rbuttondown;call super::rbuttondown;return 1
end event

type st_title from w_a_tool`st_title within w_management_reports
boolean visible = false
end type

type cb_close from w_a_tool`cb_close within w_management_reports
integer x = 2711
integer y = 1688
integer width = 434
integer height = 80
end type

type dw_details from u_dw_online within w_management_reports
integer y = 960
integer width = 2994
integer height = 704
integer taborder = 10
boolean bringtotop = true
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = false
end type

event constructor;call super::constructor;this.settransobject(sqlca)
uf_setselect(1)
end event

event rowfocuschanged;call super::rowfocuschanged;
LONG			ll_claim_no
INTEGER 	li_getrow

IF currentrow <= 0 THEN RETURN

//set the filter on report if applicable
CHOOSE CASE ddlb_report_type.TEXT
	CASE "Volumes and Cost"	
			
	CASE "Return To Work (RTW)"
		
		li_getrow 	= getrow()
		ll_claim_no 	= THIS.getitemnumber(currentrow,'claim_no')


		/* for testing bring back the claim tobstone and the documents */
		IF ll_claim_no > 0 THEN 
			
			iw_active_sheet.wf_set_claim(ll_claim_no)
			
			//need to reset the documents on a claim change
			 cb_filter.text = 'Show All Docs'
			 cb_filter.triggerevent('clicked')
		END IF
		
	CASE "Therapist By Clinic"
									
	CASE	 ELSE
END CHOOSE

end event

event resize;call super::resize;this.scrolltorow(this.getrow())
end event

event rbuttondown;M_DW_ONLINE_RMB_POPUP lm_popup

	/*	 create the menu*/
	lm_popup = Create m_dw_online_rmb_popup
	lm_popup.mf_set_datawindow(This)
	lm_popup.m_options.m_sort.visible = TRUE
	lm_popup.m_options.PopMenu(w_frame.PointerX( ), w_frame.PointerY( ))
	Destroy lm_popup
	
end event

type dw_search from u_dw_online within w_management_reports
integer y = 224
integer width = 997
integer height = 244
integer taborder = 10
boolean bringtotop = true
string dataobject = "d_mr_search_criteria"
boolean livescroll = false
end type

event itemchanged;call super::itemchanged;
DATE			ldt_date_range

/* make sure they have a report picked */
IF trim(ddlb_report_type.text) = '' THEN 
	messagebox('Report?', 'Please pick a report')
	RETURN
END IF 

end event

event clicked;call super::clicked;
STRING		ls_recipient_type, ls_find, ls_provider_name
INTEGER		li_row_found, li_rows

DATAWINDOWCHILD	   	ldwc_dddw_ephysio_providers
S_WINDOW_MESSAGE	lstr_message

CHOOSE CASE dwo.name
	CASE 'b_clinic' 
		
		ls_recipient_type = 'M'	
			
		OpenWithParm(w_service_provider_search, ls_recipient_type)
		lstr_message = Message.PowerObjectParm

		IF lstr_message.al_doubleparm[1] > 0 THEN
			
			dw_search.getchild('clinic', ldwc_dddw_ephysio_providers)
			li_rows = ldwc_dddw_ephysio_providers.rowcount()
			
			ls_find = 'provider_no = ' +  string(lstr_message.al_doubleparm[1])
			li_row_found = ldwc_dddw_ephysio_providers.Find(ls_find, 1, li_rows)
			
			IF li_row_found > 0 THEN
				ldwc_dddw_ephysio_providers.scrolltorow(li_row_found)
				ls_provider_name = ldwc_dddw_ephysio_providers.getitemstring( li_row_found, 'sort_name')
				dw_search.setitem(1, 'clinic', lstr_message.al_doubleparm[1])
				dw_search.settext( ls_provider_name)
				
			ELSE
				messagebox('Sorry', 'Bad luck! No rows found')
			END IF				
		END IF		
END CHOOSE

end event

type uo_tab_container from uo_report_criteria_selector within w_management_reports
integer x = 1001
integer y = 4
integer width = 2112
integer height = 940
integer taborder = 20
boolean bringtotop = true
end type

on uo_tab_container.destroy
call uo_report_criteria_selector::destroy
end on

type ddlb_report_type from dropdownlistbox within w_management_reports
integer y = 108
integer width = 997
integer height = 368
integer taborder = 20
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12639424
string item[] = {"Return To Work (RTW)","Volumes and Cost","Therapists By Clinic"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;/* Steps to add a new report
	1. add report name to DDLB
	2. create your dataobjects -  include a tag which will be the tabpage name
	3. create the array - hardcoded here in string format
	4. pass array to uo - tab to create the tab pages based on the dataobjects
*/
STRING  ls_array_of_dataobjects[], ls_check

ls_check = TRIM(THIS.text)

/*defaults for reports */
rb_detail.visible 			= FALSE
rb_summary.visible 		= FALSE
rb_totals.visible 			= FALSE
gb_1.visible 					= FALSE
cb_current_filter.visible 	= FALSE
is_report_syntax 			= ''
dw_details.RESET()


CHOOSE CASE ls_check
	CASE "Volumes and Cost"
		
		/*		Provider
				Therapist
				Diagnostic Group
				Admin Region
				--- POB   --  (could be a useful filter if not too difficult)  ???? (need to include on report?)	For Billable items
				Program
		*/
		ls_array_of_dataobjects[1] = "d_mr_provider"
		ls_array_of_dataobjects[2] = "d_mr_admin_region"
		ls_array_of_dataobjects[3] = "d_mr_rehab_program"
		ls_array_of_dataobjects[4] = "d_mr_billable_items"
		
		// set the dataobject 
		dw_details.dataobject = 'd_mr_diagnostic_group_report_summary'
		
		//use filter or where
		is_dw_retrieve_action = 'WHERE'
		
		//need to pass this into our object that controls the where creation
		is_report_syntax	= dw_details.Describe("DataWindow.Table.Select")
		
		//set some defaults on
		rb_detail.visible 			= TRUE
		rb_summary.visible 		= TRUE
		gb_1.visible 					= TRUE
		gb_1.bringtotop 			= FALSE
		cb_current_filter.visible 	= TRUE
		rb_summary.checked      = TRUE
		
	CASE "Return To Work (RTW)"
		
		/*
			For the following parameters, the user should be able to select one or many for filtering.  
			No selection would indicate no filtering by the criteria so all would be returned:
				•	Clinic
				•	Therapist
				•	Treatment Goal  **
				•	Diagnostic Group **
				•	Discharge Disposition
				•	Part of Body
		*/
		ls_array_of_dataobjects[1] = "d_mr_provider"
		ls_array_of_dataobjects[2] = "d_mr_therapist"
		ls_array_of_dataobjects[3] = "d_mr_treatment_goal"
		ls_array_of_dataobjects[4] = "d_mr_diagnostic_group"
		ls_array_of_dataobjects[5] = "d_mr_treatment_discharge_disposition"
		ls_array_of_dataobjects[6] = "d_mr_admin_region"
		ls_array_of_dataobjects[7] = "d_mr_pob_treated"
		ls_array_of_dataobjects[8] = "d_mr_rehab_program"
		
		// set the dataobject
		dw_details.dataobject = 'd_mr_return_to_work_sp_summary'
		
		//set some defaults on
		rb_detail.visible 		= TRUE
		rb_summary.visible 	= TRUE
		gb_1.visible 				= TRUE
		gb_1.bringtotop 		= FALSE
		rb_summary.checked = TRUE
		rb_totals.visible 		= TRUE
		rb_summary.checked = TRUE
		
		//use filter or where
		is_dw_retrieve_action = 'FILTER'
		
	CASE "Therapists By Clinic"
		
		ls_array_of_dataobjects[1] = "d_mr_provider"
		
		// set the dataobject
		dw_details.dataobject = 'd_mr_therapists_by_clinic_report'
		
		//use filter or where
		is_dw_retrieve_action = 'FILTER'
		
		cb_current_filter.visible 	= TRUE
							
	CASE	 ELSE
		
		ls_array_of_dataobjects[1] = "RESET"	
		
		// set the dataobject
		dw_details.dataobject = ''
		
		is_dw_retrieve_action = ''
								
END CHOOSE

//setup the tabpages
uo_tab_container.tab_tabpage_container.of_add_tabpages(ls_array_of_dataobjects)

end event

type cb_retrieve from commandbutton within w_management_reports
integer y = 520
integer width = 329
integer height = 80
integer taborder = 20
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Retrieve"
end type

event clicked;DATE			ldt_start, ldt_end
STRING		ls_filter, ls_where, ls_return,  ls_created_where
INTEGER		li_rowcount, li_filtered_count

/* Make sure there is dataobject */
IF dw_details.dataobject = ''  THEN RETURN

/* make sure they have a report picked */
IF trim(ddlb_report_type.text) = '' THEN 
	messagebox('Report?', 'Please pick a report')
	RETURN
END IF 

/* do an accepttext on the search window */
dw_search.accepttext()

// grab the date range
ldt_start	= dw_search.getitemdate(1,'date_range_start')
ldt_end	= dw_search.getitemdate(1,'date_range_end')

/* couple of checks here */
IF isnull(ldt_start) 	THEN 	ldt_start 	= DATE('1900-01-01')
IF isnull(ldt_end) 	THEN 	ldt_end 	= DATE('2079-01-01')

/* do a couple of checks */
IF  ldt_start > Date('2079-06-06') OR ldt_start < Date('1900-01-01') THEN 
	messagebox("Validation Error", 'The Start Date if entered, must be a valid date between 1900-01-01 and 2079-06-06')
	RETURN 1
END IF 

IF  ldt_end > Date('2079-06-06') OR ldt_end < Date('1900-01-01') THEN 
	messagebox("Validation Error", 'The End Date if entered, must be a valid date between 1900-01-01 and 2079-06-06')
	RETURN 1
END IF 

IF  ldt_start > ldt_end  THEN 
	messagebox("Validation Error", 'The End Date if entered, cannot be greater then the Start Date')
	RETURN 1
END IF 

dw_details.settransobject(sqlca)

CHOOSE CASE is_dw_retrieve_action
	CASE 'FILTER'
		
		ls_filter	           	= uo_tab_container.tab_tabpage_container.of_get_filter()
		is_current_filter 	= ls_filter
		
	CASE 'WHERE'	
		/* grab the where clause created from the tab control */
		ls_where              = uo_tab_container.tab_tabpage_container.of_get_where()
			
		ls_created_where = wf_create_where_clause(ls_where, is_report_syntax )

		ls_return = dw_details.Modify("DataWindow.Table.Select='" + ls_created_where + "'")
		
	CASE ''
END CHOOSE

dw_details.SETREDRAW(FALSE)
	
li_rowcount = dw_details.retrieve(ldt_start, ldt_end)
SQLCA.nf_handle_error("w_management_report", "cb_retrieve", "dw_details.retrieve()")

// set the filter -- should we check and see if null?
wf_set_filter(ls_filter)

//specfic to whatever the report is and the sort you want to use.
wf_set_sort(ddlb_report_type.text)

//set the filter on report if applicable
CHOOSE CASE ddlb_report_type.text
	CASE "Volumes and Cost"	
		
		ls_filter = string(ldt_start,'yyyy-mm-dd') + "/" + string(ldt_end,'yyyy-mm-dd') + " - " + ls_where
		
		wf_set_filter_on_report(ls_filter)
		
		dw_details.SetRedraw(FALSE)
		dw_details.SetSort( " rehab_program_desc_e asc  name asc  claim_no asc  billable_item_desc_e asc service_date desc" )//rehab_program_desc_e, name, claim_no, billable_item_desc_e,service_date
		//dw_details.SetSort( "#6 A #4 A, #1 A, #10 A, #18 D") //rehab_program_desc_e, name, claim_no, billable_item_desc_e,service_date
		dw_details.sort( )
		dw_details.GroupCalc()
		dw_details.SetRedraw(true)
		
	CASE "Return To Work (RTW)"
		
		ls_filter = string(ldt_start,'yyyy-mm-dd') + "/" + string(ldt_end,'yyyy-mm-dd') + " - " + ls_filter
		
		wf_set_filter_on_report(ls_filter)
		
	CASE "Therapists By Clinic"
		
		wf_set_filter_on_report(ls_filter)
							
	CASE	 ELSE
END CHOOSE

//set the first row
wf_select_first_row()

dw_details.SETREDRAW(TRUE)

end event

type cb_clear from commandbutton within w_management_reports
integer x = 663
integer y = 520
integer width = 329
integer height = 80
integer taborder = 30
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Clear"
end type

event clicked;

IF (isnull(dw_details.rowcount()) or dw_details.rowcount() < 1) AND trim(is_current_filter) = '' THEN RETURN 

IF messagebox('Confirm Clear', 'Are you sure you want to clear your search criteria?',Question!, YesNo! , 2) = 2 THEN RETURN 

/* reset everything */
dw_search.reset()
dw_search.insertrow(0)

/* set the details to nothing */
dw_details.reset()

/* set the ddlb to the reset version */
ddlb_report_type.SelectItem(0)

/* need to trigger the changed event to call the code */
ddlb_report_type.triggerevent('selectionchanged')

st_3.text = '0'

end event

type cb_1 from commandbutton within w_management_reports
boolean visible = false
integer x = 498
integer y = 704
integer width = 155
integer height = 76
integer taborder = 30
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "? syn"
end type

event clicked;

STRING				ls_origional_select

ls_origional_select = dw_details.Describe("DataWindow.Table.Select")

MESSAGEBOX('SYNTAX',ls_origional_select)

end event

type cb_2 from commandbutton within w_management_reports
boolean visible = false
integer x = 338
integer y = 704
integer width = 155
integer height = 76
integer taborder = 30
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "? Fil"
end type

event clicked;MESSAGEBOX('The current Filter is:', is_current_filter)

end event

type st_1 from statictext within w_management_reports
integer x = 5
integer y = 24
integer width = 992
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 255
long backcolor = 67108864
string text = "Pick A Report Type"
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cb_filter from commandbutton within w_management_reports
integer x = 471
integer y = 1688
integer width = 535
integer height = 80
integer taborder = 40
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Show Physio Docs"
end type

event clicked;

LONG			ll_rowsindw, ll_claim_no
INTEGER		li_cnt, li_counter
STRING		ls_filter_constant, ls_filter
u_ds 			lds_docid
W_SHEET	lw_active_sheet

/* Get the name of the active work sheet. */
lw_active_sheet = w_frame.GetActiveSheet()

/* If valid sheet name found then continue, else abort process.*/
IF NOT IsValid(lw_active_sheet) = TRUE THEN RETURN

/* Check to make sure there are documents to select.*/
ll_rowsindw = lw_active_sheet.dw_documents.RowCount()
IF ll_rowsindw <= 0 THEN 	RETURN

/* Unhighlight everything to start first. */
lw_active_sheet.dw_documents.SelectRow(0,FALSE)

IF cb_filter.text = 'Show All Docs' THEN 
	lw_active_sheet.dw_documents.SetFilter("")
	lw_active_sheet.dw_documents.Filter()
	cb_filter.text = 'Show Physio Docs' 
	RETURN
ELSE
	cb_filter.text = 'Show All Docs' 	
END IF 

ll_claim_no = lw_active_sheet.dw_documents.getitemnumber(1, 'claimmaster_claim')

/* need to grab all of the related documents and create the filter */
lds_docid 					= CREATE u_ds
lds_docid.DataObject 		= 'd_physio_docid_for_filter'
lds_docid.SetTransObject(SQLCA)
li_cnt = lds_docid.Retrieve(ll_claim_no)

ls_filter_constant 	= 'ref_docid = ' 
ls_filter 				= ''
FOR li_counter = 1 TO li_cnt

	IF li_counter <> li_cnt THEN 
		ls_filter = ls_filter + '( ' + ls_filter_constant + string(lds_docid.getitemnumber(li_counter,'doc_id')) + ') or '
		
	ELSE
		ls_filter = ls_filter +  '( ' + ls_filter_constant + string(lds_docid.getitemnumber(li_counter,'doc_id')) + ')'
	END IF 
	
NEXT

/* Now go through the list of selected document types and see highlight the documents accordingly.*/
lw_active_sheet.dw_documents.SetFilter(ls_filter)
lw_active_sheet.dw_documents.Filter()

end event

type st_2 from statictext within w_management_reports
boolean visible = false
integer x = 558
integer y = 804
integer width = 402
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
string text = "filtered/total"
boolean focusrectangle = false
end type

type st_3 from statictext within w_management_reports
boolean visible = false
integer x = 631
integer y = 872
integer width = 352
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
boolean focusrectangle = false
end type

type cb_print from commandbutton within w_management_reports
integer x = 2258
integer y = 1688
integer width = 434
integer height = 80
integer taborder = 20
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Print"
end type

event clicked;
IF dw_details.rowcount() > 0 THEN 
	dw_details.object.datawindow.print.orientation = 1  //Landscape

	dw_details.PRINT()
END IF 

end event

type rb_detail from radiobutton within w_management_reports
integer x = 1157
integer y = 1680
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
string text = "Detailed"
end type

event clicked;wf_rb_button_click('rb_detail')

end event

type rb_summary from radiobutton within w_management_reports
integer x = 1477
integer y = 1680
integer width = 334
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

event clicked;wf_rb_button_click('rb_summary')
end event

type p_expand from picture within w_management_reports
integer x = 9
integer y = 1680
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

dw_details.bringtotop = TRUE

end event

type cb_extract from commandbutton within w_management_reports
integer x = 137
integer y = 1688
integer width = 320
integer height = 80
integer taborder = 11
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
IF dw_details.RowCount() = 0 THEN
	MessageBox('No rows','There is no Report Data to extract.')
	RETURN
END IF

dw_details.Saveas('',Excel!,TRUE)

end event

type cb_current_filter from commandbutton within w_management_reports
integer x = 421
integer y = 520
integer width = 146
integer height = 80
integer taborder = 40
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "?"
end type

event clicked;STRING 		ls_filter, ls_describe, ls_get_sql_select
S_WINDOW_MESSAGE  lstr_window_message
 

ls_describe 			= dw_details.Describe("DataWindow.Table.Select")
ls_get_sql_select 	= dw_details.GetSQLSelect()

IF dw_details.rowcount() > 0  THEN 

	lstr_window_message.as_stringparm[1] =  dw_details.GetSQLSelect()
		
	IF trim(lstr_window_message.as_stringparm[1] ) = '' THEN //TRY THIS WAY...
		lstr_window_message.as_stringparm[1] =  is_current_filter     //= dw_details.Describe("DataWindow.Table.Select")
	END IF 
	
	IF 	TRIM(lstr_window_message.as_stringparm[1])  <> '' THEN
		OpenWithParm(w_management_report_sql,  lstr_window_message)
	END IF 
END IF 

end event

type rb_totals from radiobutton within w_management_reports
integer x = 1838
integer y = 1680
integer width = 270
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
string text = "Totals"
end type

event clicked;wf_rb_button_click('rb_totals')
end event

type cb_add_note from commandbutton within w_management_reports
integer y = 836
integer width = 475
integer height = 80
integer taborder = 30
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Add Report Note"
end type

event clicked;STRING 						ls_describe, ls_message



IF dw_details.rowcount() < 1 THEN 
	messagebox('No Report Information', 'Please return a report before adding a report note')
	RETURN
END IF 

ls_describe = dw_details.Object.t_user_entered_text.Text
IF ISNULL(ls_describe) THEN ls_describe = ''
 
Openwithparm(w_management_report_add_text, ls_describe)

/*	grab the passed value into the datastore */
ls_message 	= Message.stringparm

//IF trim(ls_message) <> 'CLOSE' THEN
	dw_details.setredraw(false)
	dw_details.Modify("t_user_entered_text.Text= '" + ls_message + "'")
	dw_details.setredraw(true)
//END IF 

end event

type gb_1 from groupbox within w_management_reports
integer x = 1125
integer y = 1648
integer width = 1010
integer height = 116
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

