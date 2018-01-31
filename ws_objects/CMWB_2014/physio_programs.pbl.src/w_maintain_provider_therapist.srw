$PBExportHeader$w_maintain_provider_therapist.srw
forward
global type w_maintain_provider_therapist from w_ancestor
end type
type tab_maintain from tab within w_maintain_provider_therapist
end type
type tabpage_request from userobject within tab_maintain
end type
type cb_refresh from commandbutton within tabpage_request
end type
type dw_therapist_info from u_dw_online within tabpage_request
end type
type cb_reject from commandbutton within tabpage_request
end type
type cb_approve from commandbutton within tabpage_request
end type
type dw_clinics from u_dw_online within tabpage_request
end type
type cb_select from commandbutton within tabpage_request
end type
type dw_select_therapist from u_dw_online within tabpage_request
end type
type tabpage_request from userobject within tab_maintain
cb_refresh cb_refresh
dw_therapist_info dw_therapist_info
cb_reject cb_reject
cb_approve cb_approve
dw_clinics dw_clinics
cb_select cb_select
dw_select_therapist dw_select_therapist
end type
type tabpage_provider from userobject within tab_maintain
end type
type st_save_required from statictext within tabpage_provider
end type
type st_1 from statictext within tabpage_provider
end type
type cb_one_back from commandbutton within tabpage_provider
end type
type cb_all_back from commandbutton within tabpage_provider
end type
type cb_one_to from commandbutton within tabpage_provider
end type
type cb_all_to from commandbutton within tabpage_provider
end type
type dw_rehab_program from u_dw_online within tabpage_provider
end type
type cb_save from commandbutton within tabpage_provider
end type
type cb_cancel from commandbutton within tabpage_provider
end type
type dw_rehab_programs from u_dw_online within tabpage_provider
end type
type dw_clinic from u_dw_online within tabpage_provider
end type
type dw_provider from u_dw_online within tabpage_provider
end type
type st_provider_name from statictext within tabpage_provider
end type
type cb_provider_search from commandbutton within tabpage_provider
end type
type r_1 from rectangle within tabpage_provider
end type
type tabpage_provider from userobject within tab_maintain
st_save_required st_save_required
st_1 st_1
cb_one_back cb_one_back
cb_all_back cb_all_back
cb_one_to cb_one_to
cb_all_to cb_all_to
dw_rehab_program dw_rehab_program
cb_save cb_save
cb_cancel cb_cancel
dw_rehab_programs dw_rehab_programs
dw_clinic dw_clinic
dw_provider dw_provider
st_provider_name st_provider_name
cb_provider_search cb_provider_search
r_1 r_1
end type
type tabpage_therapist from userobject within tab_maintain
end type
type cb_5 from commandbutton within tabpage_therapist
end type
type cb_cancel_therapist from commandbutton within tabpage_therapist
end type
type st_2 from statictext within tabpage_therapist
end type
type cb_4 from commandbutton within tabpage_therapist
end type
type cb_3 from commandbutton within tabpage_therapist
end type
type cb_2 from commandbutton within tabpage_therapist
end type
type cb_1 from commandbutton within tabpage_therapist
end type
type dw_eligible_programs from u_dw_online within tabpage_therapist
end type
type dw_therapist_program from u_dw_online within tabpage_therapist
end type
type dw_provider_therapist from u_dw_online within tabpage_therapist
end type
type dw_search_results from u_dw_online within tabpage_therapist
end type
type cb_therapist_search from commandbutton within tabpage_therapist
end type
type cb_save_therapist from commandbutton within tabpage_therapist
end type
type dw_therapist_license from u_dw_online within tabpage_therapist
end type
type tabpage_therapist from userobject within tab_maintain
cb_5 cb_5
cb_cancel_therapist cb_cancel_therapist
st_2 st_2
cb_4 cb_4
cb_3 cb_3
cb_2 cb_2
cb_1 cb_1
dw_eligible_programs dw_eligible_programs
dw_therapist_program dw_therapist_program
dw_provider_therapist dw_provider_therapist
dw_search_results dw_search_results
cb_therapist_search cb_therapist_search
cb_save_therapist cb_save_therapist
dw_therapist_license dw_therapist_license
end type
type tab_maintain from tab within w_maintain_provider_therapist
tabpage_request tabpage_request
tabpage_provider tabpage_provider
tabpage_therapist tabpage_therapist
end type
end forward

global type w_maintain_provider_therapist from w_ancestor
integer y = 48
integer width = 3301
integer height = 2788
string title = "Maintain Clinics And Therapists"
string menuname = "m_cmwb_notools"
windowtype windowtype = main!
long backcolor = 67108864
tab_maintain tab_maintain
end type
global w_maintain_provider_therapist w_maintain_provider_therapist

type prototypes

end prototypes

type variables
n_provider_therapist inv_provider_therapist

BOOLEAN	ib_pending_therapist




end variables

forward prototypes
public function string wf_get_provider_name (string as_type, long al_no)
public subroutine wf_retrieve_dws_for_provider (long al_provider_no)
public subroutine wf_drag_programs (integer ai_drag_type)
public subroutine wf_drag_therapist_programs (integer ai_drag_type)
public subroutine wf_retrieve_dws_for_therapist ()
public subroutine wf_reset_dws_for_provider ()
public subroutine wf_reset_dws_for_therapist ()
public subroutine wf_set_save_required_on (boolean ab_on)
public subroutine wf_default_programs_for_therapist ()
public function integer wf_find_rehab_program (string as_rehab_program, integer ai_case)
public subroutine wf_manage_therapist (long al_therapist_no)
public function integer wf_program_already_there (string as_rehab_program_code, integer ai_case)
public subroutine wf_set_therapist_program_active_filter (string as_active)
public subroutine wf_set_provider_program_active_filter (string as_active)
public function boolean wf_delisted_date_exists (long al_therapist_no)
public function boolean wf_check_rejected_exists (long al_principal_id)
public function boolean wf_check_therapist_deactivated (string as_license_no, string as_license_type_code, string as_license_prov)
public function boolean wf_check_reassign_exists (string as_license_no, string as_license_prov_state_code, string as_license_type_code)
end prototypes

public function string wf_get_provider_name (string as_type, long al_no);STRING			ls_provider_name

CHOOSE CASE as_type
	CASE 'I'
		
		SELECT  given_names + ' ' + last_name
		INTO		:ls_provider_name
		FROM 	INDIVIDUAL
		WHERE individual_no = :al_no
		USING 	SQLCA;
		SQLCA.nf_handle_error("w_maintain_provider_therapist", "wf_get_provider_name", "SELECT given_names + ' ' + last_name")
	
	CASE ELSE

		SELECT name
		INTO		:ls_provider_name                                     			
		FROM 	PROVIDER
		WHERE provider_no 				= :al_no
		AND		provider_type_code 	= :as_type
		USING 	SQLCA;
		SQLCA.nf_handle_error("w_maintain_provider_therapist", "wf_get_provider_name()","SELECT name")	
		
END CHOOSE

IF ISNULL(ls_provider_name) THEN ls_provider_name = ''

RETURN ls_provider_name

end function

public subroutine wf_retrieve_dws_for_provider (long al_provider_no);INTEGER		li_rows
STRING		ls_provider_type_code

ls_provider_type_code ='M'

/* do the retrieve for the users */
tab_maintain.tabpage_provider.dw_provider.retrieve(al_provider_no,ls_provider_type_code)
SQLCA.nf_handle_error("w_maintain_provider_therapist","wf_retrieve_dws_for_provider",".dw_provider.retrieve(al_provider_no,ls_provider_type_code)")

tab_maintain.tabpage_provider.dw_rehab_program.retrieve(al_provider_no)
SQLCA.nf_handle_error("w_maintain_provider_therapist","wf_retrieve_dws_for_provider","dw_rehab_program.retrieve(ll_provider_no)")

tab_maintain.tabpage_provider.dw_rehab_programs.retrieve(al_provider_no)
SQLCA.nf_handle_error("w_maintain_provider_therapist","wf_retrieve_dws_for_provider","dw_rehab_programs.retrieve(ll_provider_no)")



end subroutine

public subroutine wf_drag_programs (integer ai_drag_type);//grabs the highlighted row checks to see if it already exists - if it doesn't add it to the DW
INTEGER 	li_counter, li_rowcount, li_row, li_selected_row, li_deleted, li_selectedcount, li_found
LONG		ll_provider_no
STRING	ls_item_key, ls_table_name, ls_rehab_program, ls_rehab_program_text,  DWfilter2


li_rowcount 		=  tab_maintain.tabpage_provider.dw_provider.rowcount()

// make sure there is a row
IF ISNULL(li_rowcount) OR li_rowcount <= 0 THEN RETURN
ll_provider_no 	= tab_maintain.tabpage_provider.dw_provider.getitemnumber(1,'provider_no')

// 
IF ai_drag_type = 2 THEN  
	// make sure program can bedeleted
	FOR li_counter = 1 TO tab_maintain.tabpage_provider.dw_rehab_programs.rowcount()	
		
		IF tab_maintain.tabpage_provider.dw_rehab_programs.isselected(li_counter) THEN
				
				ls_rehab_program 		= tab_maintain.tabpage_provider.dw_rehab_programs.getitemstring(li_counter, 'rehab_program_code')
				ls_rehab_program_text 	= tab_maintain.tabpage_provider.dw_rehab_programs.getitemstring(li_counter, 'rehab_program_desc_e')
						
//				IF inv_provider_therapist.nf_can_provider_program_be_deleted(ll_provider_no , ls_rehab_program) = FALSE THEN 
//					MESSAGEBOX('Error', 'The ' + ls_rehab_program_text + ' Program cant be deleted. It is associated with a Physio Report',EXCLAMATION!)
//					tab_maintain.tabpage_provider.dw_rehab_programs.selectrow(li_counter, false)
//				END IF 
		END IF 
			
	NEXT
END IF 


IF ai_drag_type = 1 THEN  
	// make sure program can bedeleted
	FOR li_counter = 1 TO tab_maintain.tabpage_provider.dw_rehab_program.rowcount()	
		
		IF tab_maintain.tabpage_provider.dw_rehab_program.isselected(li_counter) THEN
				
				ls_rehab_program 		= tab_maintain.tabpage_provider.dw_rehab_program.getitemstring(li_counter, 'rehab_program_code')
				ls_rehab_program_text 	= tab_maintain.tabpage_provider.dw_rehab_program.getitemstring(li_counter, 'rehab_program_desc_e')
						
				IF inv_provider_therapist.nf_is_program_a_rehab_program( ls_rehab_program) = FALSE THEN 
					MESSAGEBOX('Error', 'The ' + ls_rehab_program_text + ' Program is not valid and cannot be saved to the Provider',EXCLAMATION!)
					tab_maintain.tabpage_provider.dw_rehab_program.selectrow(li_counter, false)
				END IF 
		END IF 
			
	NEXT
END IF 


CHOOSE CASE  ai_drag_type
		
	CASE 1 // TO Users Programs
		
			FOR li_counter = 1 TO tab_maintain.tabpage_provider.dw_rehab_program.rowcount()
				IF  tab_maintain.tabpage_provider.dw_rehab_program.IsSelected(li_counter) = TRUE THEN 		
					li_selectedcount ++		
				END IF 	
			NEXT
		
			li_selected_row = tab_maintain.tabpage_provider.dw_rehab_program.GetSelectedRow(0)
			IF li_selected_row = 0 THEN RETURN
			
			//set the redraw off
			 tab_maintain.tabpage_provider.dw_rehab_programs.setredraw(false)
			//set the filter off
			wf_set_provider_program_active_filter("")
			
			DO WHILE li_selected_row > 0
						
					ls_rehab_program	 		= tab_maintain.tabpage_provider.dw_rehab_program.getitemstring(li_selected_row, 'rehab_program_code')
					ls_rehab_program_text 	= tab_maintain.tabpage_provider.dw_rehab_program.getitemstring(li_selected_row, 'rehab_program_desc_e')
					
					//dont put dupes in
					li_found = wf_program_already_there(ls_rehab_program,1) 
					IF li_found > 0 THEN 
					
						tab_maintain.tabpage_provider.dw_rehab_programs.setitem(li_found, 'active_flag', 'Y')
						
					ELSE
		
						li_row = tab_maintain.tabpage_provider.dw_rehab_programs.insertrow(0)
							
						tab_maintain.tabpage_provider.dw_rehab_programs.setitem(li_row, 'rehab_program_code', ls_rehab_program)
						tab_maintain.tabpage_provider.dw_rehab_programs.setitem(li_row, 'rehab_program_desc_e', ls_rehab_program_text)
						tab_maintain.tabpage_provider.dw_rehab_programs.setitem(li_row, 'provider_no', ll_provider_no)
						tab_maintain.tabpage_provider.dw_rehab_programs.setitem(li_row, 'active_flag', 'Y')
					END IF 
					
					tab_maintain.tabpage_provider.dw_rehab_program.deleterow(li_selected_row)		
					li_deleted ++
										
					IF li_deleted = li_selectedcount  THEN EXIT
			
					/* Get the next selected row.*/
					li_selected_row = tab_maintain.tabpage_provider.dw_rehab_program.GetSelectedRow(0)
							
			LOOP
			
			tab_maintain.tabpage_provider.dw_rehab_programs.sort()
			tab_maintain.tabpage_provider.dw_rehab_programs.setredraw(true)
			
					
	CASE 2 // from users programs
		
			FOR li_counter = 1 TO tab_maintain.tabpage_provider.dw_rehab_programs.rowcount()
				IF  tab_maintain.tabpage_provider.dw_rehab_programs.IsSelected(li_counter) = TRUE THEN 		
					li_selectedcount ++		
				END IF 	
			NEXT
			
			li_selected_row = tab_maintain.tabpage_provider.dw_rehab_programs.GetSelectedRow(0)
			IF li_selected_row = 0 THEN RETURN
			
			/* Get the category id for selected rows. */
			DO WHILE li_selected_row > 0
				
				ls_rehab_program 		= tab_maintain.tabpage_provider.dw_rehab_programs.getitemstring(li_selected_row, 'rehab_program_code')
				ls_rehab_program_text 	= tab_maintain.tabpage_provider.dw_rehab_programs.getitemstring(li_selected_row, 'rehab_program_desc_e')
			                                                                                                                                                                	
				tab_maintain.tabpage_provider.dw_rehab_programs.setitem(li_selected_row, 'active_flag', 'N')
				tab_maintain.tabpage_provider.dw_rehab_programs.selectrow(li_selected_row, false)
				li_deleted ++
				
				li_row = tab_maintain.tabpage_provider.dw_rehab_program.insertrow(0)
				
				tab_maintain.tabpage_provider.dw_rehab_program.setitem(li_row, 'rehab_program_code', ls_rehab_program)
				tab_maintain.tabpage_provider.dw_rehab_program.setitem(li_row, 'rehab_program_desc_e', ls_rehab_program_text)
				tab_maintain.tabpage_provider.dw_rehab_program.setitem(li_row, 'active_flag', 'Y')
				
				IF li_deleted = li_selectedcount  THEN EXIT
			
				/* Get the next selected row.*/
				li_selected_row = tab_maintain.tabpage_provider.dw_rehab_programs.GetSelectedRow(0)
				
			LOOP
					
		wf_set_provider_program_active_filter("Y")
		tab_maintain.tabpage_provider.dw_rehab_programs.sort()
						
	
END CHOOSE

wf_set_save_required_on(TRUE)


end subroutine

public subroutine wf_drag_therapist_programs (integer ai_drag_type);//grabs the highlighted row checks to see if it already exists - if it doesn't add it to the DW

INTEGER 	li_counter, li_rowcount, li_row, li_selected_row, li_deleted, li_selectedcount, li_search_row, li_found, li_msg_return
LONG			ll_therapist_no
STRING		ls_item_key, ls_table_name, ls_rehab_program, ls_rehab_program_text


li_rowcount 		=  tab_maintain.tabpage_therapist.dw_eligible_programs.rowcount()

li_search_row = 	tab_maintain.tabpage_therapist.dw_search_results.getrow()

IF isnull(li_search_row) OR li_search_row < 1 THEN RETURN 
ll_therapist_no 	= 	tab_maintain.tabpage_therapist.dw_search_results.getitemnumber(li_search_row,'therapist_no')

// no therapist no go
IF isnull(ll_therapist_no) OR ll_therapist_no < 1  THEN RETURN 

/*
I think that it should warn the user if the therapist has draft reports and allow for the program to be removed if they want.
Otherwise  allow for program removal.
*/
IF ai_drag_type = 2 THEN  
	FOR li_counter = 1 TO tab_maintain.tabpage_therapist.dw_therapist_program.rowcount()	
		
		IF tab_maintain.tabpage_therapist.dw_therapist_program.isselected(li_counter) THEN
				
				ls_rehab_program 		= tab_maintain.tabpage_therapist.dw_therapist_program.getitemstring(li_counter, 'rehab_program_code')
				ls_rehab_program_text 	= tab_maintain.tabpage_therapist.dw_therapist_program.getitemstring(li_counter, 'rehab_program_desc_e')
						
				IF inv_provider_therapist.nf_can_therapist_program_be_deleted(ll_therapist_no, ls_rehab_program) = FALSE THEN 
					li_msg_return = MESSAGEBOX('Associated Draft Report', 'The ' + ls_rehab_program_text + 'This Program is associated with a Draft Report are you sure you want to remove it?', question!, yesno!)
					IF li_msg_return = 2  THEN 
						tab_maintain.tabpage_therapist.dw_therapist_program.selectrow(li_counter, false)
					END IF 
				END IF 
		END IF 		
	NEXT
END IF 

/* Rehab programs cannot be moved if any of the delisted dates are set */
IF wf_delisted_date_exists(ll_therapist_no) = TRUE THEN 
	 MESSAGEBOX('Therapist Delisted', 'This therapist has a license that has been delisted. You cannot give the therapist a rehab program if any licenses have been delisted.', stopsign!)
	 RETURN
END IF 

CHOOSE CASE  ai_drag_type
		
	CASE 1 // TO Users Programs
		
			FOR li_counter = 1 TO tab_maintain.tabpage_therapist.dw_eligible_programs.rowcount()
				IF  tab_maintain.tabpage_therapist.dw_eligible_programs.IsSelected(li_counter) = TRUE THEN 		
					li_selectedcount ++		
				END IF 	
			NEXT
		
			li_selected_row = tab_maintain.tabpage_therapist.dw_eligible_programs.GetSelectedRow(0)
			IF li_selected_row = 0 THEN RETURN
			
			//set the redraw off
			 tab_maintain.tabpage_therapist.dw_therapist_program.setredraw(false)
			//set the filter off
			wf_set_therapist_program_active_filter("")
			
			DO WHILE li_selected_row > 0
						
					ls_rehab_program	 		= tab_maintain.tabpage_therapist.dw_eligible_programs.getitemstring(li_selected_row, 'rehab_program_code')
					ls_rehab_program_text 	= tab_maintain.tabpage_therapist.dw_eligible_programs.getitemstring(li_selected_row, 'rehab_program_desc_e')
					
					//dont put dupes in
					li_found =  wf_program_already_there(ls_rehab_program,2) 
					
					tab_maintain.tabpage_therapist.dw_eligible_programs.deleterow(li_selected_row)		
					li_deleted ++
					
					IF li_found > 0 THEN //just make it active
							tab_maintain.tabpage_therapist.dw_therapist_program.setitem(li_found, 'active_flag', 'Y')
					ELSE
																		
							li_row = tab_maintain.tabpage_therapist.dw_therapist_program.insertrow(0)
								
							tab_maintain.tabpage_therapist.dw_therapist_program.setitem(li_row, 'rehab_program_code', ls_rehab_program)
							tab_maintain.tabpage_therapist.dw_therapist_program.setitem(li_row, 'rehab_program_desc_e', ls_rehab_program_text)
							tab_maintain.tabpage_therapist.dw_therapist_program.setitem(li_row, 'therapist_no', ll_therapist_no)
							tab_maintain.tabpage_therapist.dw_therapist_program.setitem(li_row, 'active_flag', 'Y')
	
					END IF 
						
					IF li_deleted = li_selectedcount  THEN EXIT
			
					/* Get the next selected row.*/
					li_selected_row = tab_maintain.tabpage_therapist.dw_eligible_programs.GetSelectedRow(0)
							
			LOOP
			 tab_maintain.tabpage_therapist.dw_therapist_program.sort()
			 tab_maintain.tabpage_therapist.dw_therapist_program.setredraw(true)
			
					
	CASE 2 // from users programs
		
			FOR li_counter = 1 TO tab_maintain.tabpage_therapist.dw_therapist_program.rowcount()
				IF  tab_maintain.tabpage_therapist.dw_therapist_program.IsSelected(li_counter) = TRUE THEN 		
					li_selectedcount ++		
				END IF 	
			NEXT
			
			li_selected_row = tab_maintain.tabpage_therapist.dw_therapist_program.GetSelectedRow(0)
			IF li_selected_row = 0 THEN RETURN

			/* Get the category id for selected rows. */
			DO WHILE li_selected_row > 0
				
				ls_rehab_program 		= tab_maintain.tabpage_therapist.dw_therapist_program.getitemstring(li_selected_row, 'rehab_program_code')
				ls_rehab_program_text 	= tab_maintain.tabpage_therapist.dw_therapist_program.getitemstring(li_selected_row, 'rehab_program_desc_e')
			                                                                                                                                                                
				tab_maintain.tabpage_therapist.dw_therapist_program.setitem(li_selected_row, 'active_flag', 'N')
				tab_maintain.tabpage_therapist.dw_therapist_program.selectrow(li_selected_row, false)
				//tab_maintain.tabpage_therapist.dw_therapist_program.deleterow(li_selected_row)		
				li_deleted ++
				
				li_row = tab_maintain.tabpage_therapist.dw_eligible_programs.insertrow(0)
				
				tab_maintain.tabpage_therapist.dw_eligible_programs.setitem(li_row, 'rehab_program_code', ls_rehab_program)
				tab_maintain.tabpage_therapist.dw_eligible_programs.setitem(li_row, 'rehab_program_desc_e', ls_rehab_program_text)
				tab_maintain.tabpage_therapist.dw_eligible_programs.setitem(li_row, 'active_flag', 'Y')
				
				IF li_deleted = li_selectedcount  THEN EXIT
			
				/* Get the next selected row.*/
				li_selected_row = tab_maintain.tabpage_therapist.dw_therapist_program.GetSelectedRow(0)
				
			LOOP
			
			wf_set_therapist_program_active_filter("Y")			
			tab_maintain.tabpage_therapist.dw_therapist_program.sort()
			
							
END CHOOSE



















end subroutine

public subroutine wf_retrieve_dws_for_therapist ();INTEGER	li_rows
LONG		ll_therapist_no

//in this case a therapist exists already simply re-retrieve them
IF tab_maintain.tabpage_therapist.dw_search_results.rowcount() = 1 THEN
	
	ll_therapist_no = tab_maintain.tabpage_therapist.dw_search_results.GETITEMNUMBER(1,'therapist_no')
	
	tab_maintain.tabpage_therapist.dw_search_results.retrieve(ll_therapist_no )
	SQLCA.nf_handle_error("w_maintain_provider_therapist", "wf_retrieve_dws_for_therapist", "tab_maintain.tabpage_therapist.dw_search_results.retrieve()")
	
END IF 


end subroutine

public subroutine wf_reset_dws_for_provider ();/* do the retrieve for the users */
tab_maintain.tabpage_provider.dw_provider.reset()
tab_maintain.tabpage_provider.dw_rehab_programs.reset()
tab_maintain.tabpage_provider.dw_rehab_program.reset()

end subroutine

public subroutine wf_reset_dws_for_therapist ();
/* do the retrieve for the users */
tab_maintain.tabpage_therapist.dw_eligible_programs.reset()
tab_maintain.tabpage_therapist.dw_provider_therapist.reset()
tab_maintain.tabpage_therapist.dw_therapist_license.reset()
tab_maintain.tabpage_therapist.dw_therapist_program.reset()


end subroutine

public subroutine wf_set_save_required_on (boolean ab_on);tab_maintain.tabpage_provider.st_save_required.visible = ab_on
end subroutine

public subroutine wf_default_programs_for_therapist ();STRING		ls_rehab_program_code
INTEGER		li_counter

//grab the primary and P&A from dw_eligible_programs and highlght them 

FOR li_counter = 1 TO  tab_maintain.tabpage_therapist.dw_eligible_programs.rowcount()
	
	ls_rehab_program_code = tab_maintain.tabpage_therapist.dw_eligible_programs.getitemstring(li_counter,'rehab_program_code')
	
	CHOOSE CASE ls_rehab_program_code
		CASE 'P001', 'P007'
			
			 tab_maintain.tabpage_therapist.dw_eligible_programs.SelectRow(li_counter, true)
			
		CASE ELSE
			
	END CHOOSE
		
NEXT
end subroutine

public function integer wf_find_rehab_program (string as_rehab_program, integer ai_case);INTEGER		li_found

li_found =	0

CHOOSE CASE ai_case
	CASE  1//provider
		li_found = 	tab_maintain.tabpage_provider.dw_rehab_program.Find("rehab_program_code = '" + as_rehab_program + "'"  ,   1 , tab_maintain.tabpage_provider.dw_rehab_program.RowCount()) 
			
	CASE 2//therapist
		li_found = 	tab_maintain.tabpage_therapist.dw_eligible_programs.Find("rehab_program_code = '" + as_rehab_program + "'",   1, tab_maintain.tabpage_therapist.dw_eligible_programs.RowCount()) 
		
	CASE 3 	
			li_found = 	tab_maintain.tabpage_provider.dw_rehab_programs.Find("rehab_program_code = '" + as_rehab_program + "'"  ,   1 , tab_maintain.tabpage_provider.dw_rehab_programs.RowCount()) 
					
END CHOOSE
					
IF ISNULL(li_found) OR li_found < 1 THEN RETURN 0

RETURN li_found
end function

public subroutine wf_manage_therapist (long al_therapist_no);INTEGER			li_current_row

IF isnull(al_therapist_no) OR al_therapist_no < 1 THEN 
	messagebox('Therapist?','Please select a valid therapist')
END IF 

tab_maintain.tabpage_therapist.dw_search_results.settransobject(sqlca)
		
tab_maintain.tabpage_therapist.dw_search_results.retrieve(al_therapist_no)
SQLCA.nf_handle_error("w_maintain_provider_therapist", "tabpage_request", "cb_select_clicked")

wf_reset_dws_for_therapist()

IF tab_maintain.tabpage_therapist.dw_search_results.rowcount() > 0 THEN 
	tab_maintain.tabpage_therapist.dw_search_results.scrolltorow(1)
	tab_maintain.tabpage_therapist.dw_search_results.triggerevent('rowfocuschanged')
END IF 

tab_maintain.SelectTab(3)
end subroutine

public function integer wf_program_already_there (string as_rehab_program_code, integer ai_case);INTEGER		li_counter, li_rowcount
STRING		ls_key

CHOOSE CASE ai_case
	CASE 1	//provider
		
		li_rowcount = tab_maintain.tabpage_provider.dw_rehab_programs.rowcount()

		IF isnull(li_rowcount) OR li_rowcount < 1 THEN RETURN 0

		FOR li_counter = 1 TO  li_rowcount
			
			ls_key = tab_maintain.tabpage_provider.dw_rehab_programs.getitemstring(li_counter, 'rehab_program_code')
		
			IF trim(ls_key) = trim(as_rehab_program_code) THEN RETURN li_counter
		
		NEXT
		
		RETURN 0
		
	CASE 2	//therapist
		
		li_rowcount = tab_maintain.tabpage_therapist.dw_therapist_program.rowcount()

		IF isnull(li_rowcount) OR li_rowcount < 1 THEN RETURN 0

		FOR li_counter = 1 TO  li_rowcount
			
			ls_key = tab_maintain.tabpage_therapist.dw_therapist_program.getitemstring(li_counter, 'rehab_program_code')
		
			IF trim(ls_key) = trim(as_rehab_program_code) THEN RETURN li_counter
		
		NEXT
		
		RETURN 0
			
END CHOOSE

RETURN 0


end function

public subroutine wf_set_therapist_program_active_filter (string as_active);STRING			DWfilter2

IF as_active > '' THEN 
	
	DWfilter2 =	 "REHAB_PROGRAM_xref_THERAPIST.active_flag = 'Y'"
ELSE	
	DWfilter2 = ''
END IF 

tab_maintain.tabpage_therapist.dw_therapist_program.SetFilter(DWfilter2)
tab_maintain.tabpage_therapist.dw_therapist_program.SetRedraw(false)
tab_maintain.tabpage_therapist.dw_therapist_program.Filter( )
tab_maintain.tabpage_therapist.dw_therapist_program.SetRedraw(true)	
end subroutine

public subroutine wf_set_provider_program_active_filter (string as_active);STRING			DWfilter2

IF as_active > '' THEN 
	
	DWfilter2 =	 "REHAB_PROGRAM_xref_PROGRAM.active_flag = 'Y'"
ELSE	
	DWfilter2 = ''
END IF 

tab_maintain.tabpage_therapist.dw_therapist_program.SetFilter(DWfilter2)
tab_maintain.tabpage_therapist.dw_therapist_program.SetRedraw(false)
tab_maintain.tabpage_therapist.dw_therapist_program.Filter( )
tab_maintain.tabpage_therapist.dw_therapist_program.SetRedraw(true)	
end subroutine

public function boolean wf_delisted_date_exists (long al_therapist_no);INTEGER				li_count

SELECT 	count(*) 
INTO 		:li_count
FROM 		therapist_license  
WHERE 	therapist_no = :al_therapist_no 
AND  		delisted_date IS NOT NULL;
SQLCA.nf_handle_error("w_maintain_provider_therapist", "wf_delisted_date_exists()", "SELECT count(*) ")
	
IF li_count > 0  THEN  RETURN TRUE


RETURN FALSE

end function

public function boolean wf_check_rejected_exists (long al_principal_id);LONG				li_count

SELECT   COUNT(*)
INTO		:li_count               			
FROM 	   workflow_therapist_approval
WHERE 	therapist_wif_principal_id 	= :al_principal_id
AND		approval_status_code = '2'
USING 	SQLCA;
SQLCA.nf_handle_error("w_maintain_provider_therapist", "wf_check_rejected_exists()","SELECT COUNT(*)")	

IF li_count > 0 THEN RETURN TRUE

RETURN FALSE
end function

public function boolean wf_check_therapist_deactivated (string as_license_no, string as_license_type_code, string as_license_prov);LONG				li_count

SELECT 	count(*)
INTO		:li_count
FROM  	THERAPIST_LICENSE a 
JOIN  		THERAPIST  b ON a.therapist_no 	= b.therapist_no
WHERE 	a.license_no 							= :as_license_no 
AND 		a.license_prov_state_code 			= :as_license_prov
AND 		a.license_type_code 					= :as_license_type_code
AND 		b.wif_principal_id 						= 0
USING 	SQLCA;
SQLCA.nf_handle_error("w_maintain_provider_therapist", "wf_check_therapist_deactivated()","SELECT COUNT(*)")	


IF li_count > 0 THEN RETURN TRUE


RETURN FALSE
end function

public function boolean wf_check_reassign_exists (string as_license_no, string as_license_prov_state_code, string as_license_type_code);INTEGER li_count
	
SELECT count(*)
INTO   :li_count
FROM   THERAPIST_LICENSE          a
JOIN   THERAPIST                  b ON a.therapist_no     = b.therapist_no 
JOIN   v_WIF_CUSTOM_PRINCIPAL_ALL x ON b.wif_principal_id = x.wif_principal_id
WHERE  a.license_no              = :as_license_no
AND    a.license_prov_state_code = :as_license_prov_state_code
AND    a.license_type_code       = :as_license_type_code
AND    NOT EXISTS ( SELECT * 
                    FROM WIF_CUSTOM_PRINCIPAL c WHERE c.wif_principal_id = b.wif_principal_id ) 
USING SQLCA;
							  
IF 	li_count > 0  THEN RETURN TRUE
		
		
RETURN FALSE 
end function

on w_maintain_provider_therapist.create
int iCurrent
call super::create
if IsValid(this.MenuID) then destroy(this.MenuID)
if this.MenuName = "m_cmwb_notools" then this.MenuID = create m_cmwb_notools
this.tab_maintain=create tab_maintain
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.tab_maintain
end on

on w_maintain_provider_therapist.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.tab_maintain)
end on

event open;call super::open;INTEGER			li_row
INT 				li_trancount
BOOLEAN		lb_rtn

/* APPLICATION SECURITY CODE. */
lb_rtn = G_PFSecurity.UOF_Check_Access(This)
IF lb_rtn = FALSE THEN
	Messagebox("Access Denied", "You do not have proper security priveleges to open this window.~r~r" +&
				  "If you need to open this window, Please call the Helpdesk to get the proper security priveleges.", Exclamation!)
	Close(This)
	RETURN -1
END IF

THIS.I_Authorized_Access = TRUE	

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')

tab_maintain.tabpage_provider.dw_clinic.settransobject(sqlca)
tab_maintain.tabpage_provider.dw_provider.settransobject(SQLCA)
tab_maintain.tabpage_provider.dw_rehab_programs.settransobject(SQLCA)
tab_maintain.tabpage_provider.dw_rehab_program.settransobject(SQLCA)
li_row = tab_maintain.tabpage_provider.dw_clinic.insertrow(0)

tab_maintain.tabpage_provider.dw_clinic.setitem(li_row,'name','Please select a Provider...')

tab_maintain.tabpage_provider.dw_clinic.scrolltorow(li_row)

IF IsNull(inv_resize) OR NOT IsValid (inv_resize) THEN
	inv_resize = CREATE n_resize
	inv_resize.of_SetOrigSize (2734,2608)
END IF

// PROVIDER
THIS.inv_resize.of_register(tab_maintain,0,0,100,100)
THIS.inv_resize.of_register(tab_maintain.tabpage_provider.cb_save,0,0,0,0)
THIS.inv_resize.of_register(tab_maintain.tabpage_provider.cb_cancel,0,0,0,0)

/* create the NVO */
inv_provider_therapist = Create n_provider_therapist

//set set save required off
wf_set_save_required_on(false)

/* RETRIEVE THE PENDING REQESTS */
tab_maintain.tabpage_request.dw_select_therapist.retrieve()
SQLCA.nf_handle_error('w_maintain_provider_therapist','open','dw_select_therapist.retrieve()')


end event

type tab_maintain from tab within w_maintain_provider_therapist
integer x = 23
integer y = 76
integer width = 3218
integer height = 2504
integer taborder = 10
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 67108864
boolean raggedright = true
boolean focusonbuttondown = true
boolean boldselectedtext = true
integer selectedtab = 1
tabpage_request tabpage_request
tabpage_provider tabpage_provider
tabpage_therapist tabpage_therapist
end type

on tab_maintain.create
this.tabpage_request=create tabpage_request
this.tabpage_provider=create tabpage_provider
this.tabpage_therapist=create tabpage_therapist
this.Control[]={this.tabpage_request,&
this.tabpage_provider,&
this.tabpage_therapist}
end on

on tab_maintain.destroy
destroy(this.tabpage_request)
destroy(this.tabpage_provider)
destroy(this.tabpage_therapist)
end on

event selectionchanged;IF newindex > 0 THEN
 THIS.Control[newindex].TabBackColor = rgb(160,204,231)
END IF 

IF oldindex > 0 THEN
 THIS.Control[oldindex].TabBackColor = 79741120
END IF 

end event

type tabpage_request from userobject within tab_maintain
integer x = 18
integer y = 100
integer width = 3182
integer height = 2388
long backcolor = 67108864
string text = "Pending Approval"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
cb_refresh cb_refresh
dw_therapist_info dw_therapist_info
cb_reject cb_reject
cb_approve cb_approve
dw_clinics dw_clinics
cb_select cb_select
dw_select_therapist dw_select_therapist
end type

on tabpage_request.create
this.cb_refresh=create cb_refresh
this.dw_therapist_info=create dw_therapist_info
this.cb_reject=create cb_reject
this.cb_approve=create cb_approve
this.dw_clinics=create dw_clinics
this.cb_select=create cb_select
this.dw_select_therapist=create dw_select_therapist
this.Control[]={this.cb_refresh,&
this.dw_therapist_info,&
this.cb_reject,&
this.cb_approve,&
this.dw_clinics,&
this.cb_select,&
this.dw_select_therapist}
end on

on tabpage_request.destroy
destroy(this.cb_refresh)
destroy(this.dw_therapist_info)
destroy(this.cb_reject)
destroy(this.cb_approve)
destroy(this.dw_clinics)
destroy(this.cb_select)
destroy(this.dw_select_therapist)
end on

type cb_refresh from commandbutton within tabpage_request
integer x = 18
integer y = 2056
integer width = 759
integer height = 104
integer taborder = 41
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Refresh Pending Approvals"
end type

event clicked;//set set save required off
wf_set_save_required_on(false)

dw_therapist_info.reset()
dw_clinics.reset()

/* RETRIEVE THE PENDING REQESTS */
tab_maintain.tabpage_request.dw_select_therapist.retrieve()
SQLCA.nf_handle_error('w_maintain_provider_therapist','open','dw_select_therapist.retrieve()')

dw_select_therapist.triggerevent('rowfocuschanged')
end event

type dw_therapist_info from u_dw_online within tabpage_request
integer x = 18
integer y = 972
integer width = 3131
integer height = 472
integer taborder = 21
string dataobject = "d_therapist_for_workflow"
boolean vscrollbar = true
end type

event constructor;call super::constructor;settransobject(sqlca)
end event

type cb_reject from commandbutton within tabpage_request
integer x = 2510
integer y = 2056
integer width = 306
integer height = 92
integer taborder = 31
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Reject"
boolean default = true
end type

event clicked;/*
	•	Display the pending requests from the THERAPIST_APPROVAL_REQUEST_LOG
	•	If approved then the approver can select the programs
	•	Must have primary physiotherapy if approved
	•	If rejected then the programs are not available for selection
	•	Confirm rejection --- are you sure…?
	•	Update the request status based on the decision
	•	Set the decision date to the current date
	•	If accepted insert the therapist information into the THERAPIST and THERAPIST_LICENSE tables
*/

LONG			ll_ReturnCode, ll_wif_principal_id, ll_count
STRING		ls_therapist, ls_license_type_code, ls_license_prov_state_code, ls_name, ls_license_type_desc_e
STRING 		ls_describe, ls_message
INTEGER		li_token_row, li_row


IF dw_select_therapist.rowcount() < 1 THEN RETURN 

li_token_row 		= dw_select_therapist.getrow()
IF isnull(li_token_row) OR li_token_row < 1 THEN RETURN 

ll_wif_principal_id					=	dw_select_therapist.getitemnumber(li_token_row ,'therapist_wif_principal_id')
ls_license_type_code					=	dw_select_therapist.getitemstring(li_token_row ,'license_type_code')
ls_license_prov_state_code			=	dw_select_therapist.getitemstring(li_token_row ,'license_prov_state_code')
ls_name 									= 	dw_select_therapist.getitemstring(li_token_row ,'name')
ls_license_type_desc_e 				= 	dw_select_therapist.getitemstring(li_token_row ,'license_type_desc_e')

// check to see if this individual is already a therapist
//IF  inv_provider_therapist.nf_check_therapist_license_exists(ll_wif_principal_id, ls_license_type_code, ls_license_prov_state_code) = TRUE THEN 
//	 MessageBox("Therapist Already approved", "Please use the manage therapist screen to maintain this therapist license", Information! )
//	RETURN 1	
//END IF  
//
/*
Warning 

You are about to reject the request for approval for Kelly Stone.  This means that Kelly Stone will not be able to treat WorkSafeNB clients and submit physiotherapy reports.
Do you want to continue?
*/
ll_ReturnCode = MessageBox("Warning!","You are about to reject the request for approval for " + ls_name+ "  This means that " + ls_name + " will not be able to treat WorkSafeNB clients and submit physiotherapy reports. " +&
"~rDo you want to continue?",Question!,YesNo!)
IF ll_ReturnCode = 2 THEN RETURN


ls_describe = ''

Openwithparm(w_add_rejected_comment, ls_describe)

/*	grab the passed value into the datastore */
ls_message 	= Message.stringparm
ls_message = trim(ls_message)

// Second thoughts on rejecting
IF ls_message = '' THEN RETURN 

/* WORKFLOW_THERAPIST_APPROVAL - UPDATE */
/* 4.110 The workflow token must be marked as used if the request for approval has been rejected. */
dw_select_therapist.setitem(li_token_row, 'approval_status_code', '2') 
dw_select_therapist.setitem(li_token_row, 'decision_date', Date(f_server_datetime()) )
dw_select_therapist.setitem(li_token_row, 'rejected_comment', ls_message)
dw_select_therapist.setitem(li_token_row, 'approver_rejecter_user_id',  vgst_user_profile.user_id)
	
/* updates and commits */
SQLCA.nf_begin_transaction()

dw_select_therapist.update()
SQLCA.nf_handle_error("w_maintain_provider_therapist","cb_approve-clicked","dw_select_therapist.update()")

SQLCA.nf_commit_transaction()


/* re-retrieve the token information */
dw_select_therapist.retrieve()
SQLCA.nf_handle_error("w_maintain_provider_therapist","cb_approve-clicked","dw_select_therapist.retrieve()")

/* scroll to the first row */
dw_select_therapist.ScrollToRow(1)
dw_select_therapist.TriggerEvent(RowFocusChanged!)
dw_select_therapist.selectrow(1, true)

end event

type cb_approve from commandbutton within tabpage_request
integer x = 2181
integer y = 2056
integer width = 306
integer height = 92
integer taborder = 21
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Approve"
end type

event clicked;/*
•	Display the pending requests from the THERAPIST_APPROVAL_REQUEST_LOG
•	If approved then the approver can select the programs
•	Must have primary physiotherapy if approved
•	If rejected then the programs are not available for selection
•	Confirm rejection --- are you sure…?
•	Update the request status based on the decision
•	Set the decision date to the current date
•	If accepted insert the therapist information into the THERAPIST and THERAPIST_LICENSE tables

*/

LONG			ll_ReturnCode, ll_wif_principal_id, ll_record_no,  ll_therapist_no
STRING		ls_license_type_code, ls_license_prov_state_code, ls_delisted_comment, ls_approval_request_status_code, ls_license_no
STRING      ls_program_array[], ls_name, ls_license_type_desc_e, ls_prov_state
INTEGER		li_token_row, li_therapist_row, li_therapist_license_row, li_row, li_counter
DATE	  		ldt_approval_date, ldt_decision_date
u_ds   		lds_therapist_license, lds_program_therapist


IF dw_select_therapist.rowcount() < 1 THEN RETURN 

li_token_row 		= dw_select_therapist.getrow()
IF isnull(li_token_row) OR li_token_row < 1 THEN RETURN 

lds_therapist_license 						= CREATE u_ds
lds_therapist_license.dataobject 		= 'd_therapist_license_insert'
lds_therapist_license.SetTransObject(sqlca)

lds_program_therapist 					= CREATE u_ds
lds_program_therapist.dataobject 		= 'd_program_therapist_xref'
lds_program_therapist.SetTransObject(sqlca)


/* grab the informationwe need from the WORKFLOW_THERAPIST_APPROVAL table */
//ll_token_id	 							= 	dw_select_therapist.getitemnumber(li_token_row ,'token_id' ) cant be used
ll_record_no 							= 	dw_select_therapist.getitemnumber(li_token_row ,'record_no')
ll_wif_principal_id						=	dw_select_therapist.getitemnumber(li_token_row ,'therapist_wif_principal_id')
ls_license_no 							= 	dw_select_therapist.getitemstring(li_token_row ,'license_no')
ls_license_type_code					=	dw_select_therapist.getitemstring(li_token_row ,'license_type_code')
ls_license_prov_state_code			=	dw_select_therapist.getitemstring(li_token_row ,'license_prov_state_code')
ldt_approval_date 						= 	date(dw_select_therapist.getitemdatetime(li_token_row ,'approval_request_date'))
ls_approval_request_status_code	=	dw_select_therapist.getitemstring(li_token_row ,'approval_status_code')
ldt_decision_date 						= 	date(dw_select_therapist.getitemdatetime(li_token_row ,'decision_date'))
ls_name 									= 	dw_select_therapist.getitemstring(li_token_row ,'name')
ls_license_type_desc_e 				= 	dw_select_therapist.getitemstring(li_token_row ,'license_type_desc_e')


/* requesting user portal account must still be active */
IF inv_provider_therapist.nf_portal_account_is_active(ll_wif_principal_id) = FALSE THEN
	MessageBox("Therapist Not Active", "The Requesting User Portal Account must be active.", Information! )
	RETURN -1		
END IF 

IF  inv_provider_therapist.nf_check_therapist_license_exists(ll_wif_principal_id, ls_license_type_code, ls_license_prov_state_code) = TRUE THEN 
	 MessageBox("Therapist Already approved", "Please use the manage therapist screen to add or remove programs.", Information! )
	RETURN -1	
END IF 

/*
 token_status_code 		token_status_desc_e                               
----------------- 				--------------------    
0                 				Available           
1                 				Used                
2                 				Expired             
3                 				Cancelled          
99                				Error    
*/


/*  4.20		A request for approval must not be available for approval if the workflow token has expired. -- this should never happen*/
IF inv_provider_therapist.nf_get_token_status(ll_record_no, ll_wif_principal_id, ls_license_no) = 2  THEN 
	 MessageBox("Token Expired", "A request for approval must not be available for approval if the workflow token has expired.", Information! )
	RETURN -1	
END IF 

/* 4.30		A request for approval must not be available for approval if the workflow token is cancelled. */
IF inv_provider_therapist.nf_get_token_status(ll_record_no, ll_wif_principal_id, ls_license_no) = 3  THEN 
	 MessageBox("Token Cancelled", "A request for approval must not be available for approval if the workflow token is cancelled.", Information! )
	RETURN -1	
END IF 

/* 4.40		A rejected therapist must not be assigned a physiotherapy program. */
IF inv_provider_therapist.nf_has_therapist_been_rejected(ll_record_no, ll_wif_principal_id, ls_license_no) > 0 THEN 
	 MessageBox("Therapist Rejected", "A rejected therapist must not be assigned a physiotherapy program. ", Information! )
	RETURN 1	
END IF

/* •	The license number must be unique for the license type and the licensing  province 
		incorrectbased on this idx.... Cannot insert duplicate key row in object 'dbo.THERAPIST_LICENSE' with unique index 'idx2'. The duplicate key value is (999).
		No changes made to database. -- this will need to be changed or the index will need to be changed
*/
IF inv_provider_therapist.nf_is_license_unique(ls_license_no, ls_license_type_code, ls_license_prov_state_code) = FALSE THEN
	MessageBox('Error','The license number must be unique for the license type and the licensing  province',Exclamation!)
	RETURN -1	
END IF 

/* this idx is wrong - what if a therapist had the same license number in a different province? it would fail on this but it shouldn't
   Cannot insert duplicate key row in object 'dbo.THERAPIST_LICENSE' with unique index 'idx1'. The duplicate key value is (1294).
*/
IF inv_provider_therapist.nf_is_license_unique_key_idx1(ls_license_type_code, ls_license_no) = FALSE THEN
	MessageBox('Error','The license number must be unique for the license type code and license no. ',Exclamation!)
	RETURN -1	
END IF

IF inv_provider_therapist.nf_check_therapist_exists(ll_wif_principal_id) THEN 
	
		ll_therapist_no = inv_provider_therapist.nf_get_therapist_no(ll_wif_principal_id)

		/* paul has the Primary key for license table as therapist_no,license_type and license_no which isnot the same as above */
		IF inv_provider_therapist.nf_is_license_unique_key(ll_therapist_no, ls_license_type_code, ls_license_no) = FALSE THEN
			MessageBox('Error','The license number must be unique for the therapist number, license type code and license no ',Exclamation!)
			RETURN -1	
		END IF
		
		/* Rehab programs cannot be moved if any of the delisted dates are set */
		IF wf_delisted_date_exists(ll_therapist_no) = TRUE THEN 
			 MESSAGEBOX('Therapist Delisted', 'This therapist has a license that has been delisted. You must un de-list this therapist in order to approve this license.', stopsign!)
			 RETURN -1
		END IF 
			
ELSE 
	ll_therapist_no = 0
END IF 

/*  You are about to approve Justin Parsons as a physiotherapist in the province of: QC (or if you have the spelling – spell it out).
	This will mean that the therapist can treat clients at clinics in the licensed province and submit electronic reports (provided the clinic submits electronically).
	Do you want to continue?
*/
ls_prov_state = inv_provider_therapist.nf_get_location_description(ls_license_prov_state_code)
ll_ReturnCode = MessageBox("Question!","You are about to approve " + ls_name+ " as a "+ ls_license_type_desc_e + " in the Province of: " + ls_prov_state +&
" This will mean that the therapist can treat clients at clinics in the licensed province and submit electronic reports (provided the clinic submits electronically)." +&
" Do you want to continue?",Question!,YesNo!)

IF ll_ReturnCode = 2 THEN RETURN

/* LAST_THERAPIST_NO - SELECT/UPDATE  */
IF ll_therapist_no = 0 THEN 
			
	UPDATE Last_therapist_No SET last_therapist_no = last_therapist_no + 1 USING SQLCA;
	SQLCA.nf_handle_error("w_maintain_provider_therapist","cb_approve-clicked","Embedded SQL: Update Last_therapist_No")
		
	SELECT last_therapist_no INTO :ll_therapist_no FROM Last_therapist_No USING SQLCA;
	SQLCA.nf_handle_error("w_maintain_provider_therapist","cb_approve-clicked","SELECT last_therapist_no INTO ")
	
	
	/* THERAPIST - insert */
	li_therapist_row = dw_therapist_info.insertrow(0)
		
	dw_therapist_info.setitem( li_therapist_row, 'wif_principal_id', ll_wif_principal_id )
	dw_therapist_info.setitem( li_therapist_row, 'therapist_no', ll_therapist_no )
	dw_therapist_info.setitem( li_therapist_row, 'Comments', '')

END IF 
				
IF IsNull(ll_therapist_no ) or ll_therapist_no = 0 THEN 
	SignalError(-666,'Crashing... crashing... crash.')
END IF 

/* THERAPIST_LICENSE - insert 

  SELECT therapist_no,   
         license_type_code,   
         license_no,   
         license_prov_state_code,   
         listed_date,   
         delisted_date,   
         delisted_comment,   
    FROM therapist_license   
*/

li_therapist_license_row = lds_therapist_license.insertrow(0)
lds_therapist_license.setitem( li_therapist_license_row, 'therapist_no', ll_therapist_no)
lds_therapist_license.setitem( li_therapist_license_row, 'license_type_code', ls_license_type_code)
lds_therapist_license.setitem( li_therapist_license_row, 'license_no', ls_license_no)
lds_therapist_license.setitem( li_therapist_license_row, 'license_prov_state_code',ls_license_prov_state_code)
lds_therapist_license.setitem( li_therapist_license_row, 'listed_date', date(f_server_datetime()))
lds_therapist_license.setitem( li_therapist_license_row, 'delisted_comment', ls_delisted_comment)

	
/*    REHAB_PROGRAM_xref_THERAPIST - insert */
/*
	When Pam ‘Approves’ a therapist, the module should automatically approve the therapist for primary phsyio 
	and p&a – save this with the therapist information. – She can add additional programs after the save (or when you change tabs).	
*/

// set up the program array -- PRIMARY & P&A
/* 4.80 		A physiotherapist must be assigned to the primary physiotherapy program if approved.*/
/* 4.90 		A physiotherapist must be assigned to the P&A physiotherapy program if approved. REMOVED 2016-02-11*/
/* 4.160    A physiotherapist must be assigned to the Functional Outcome Scores program if approved.. ADDED 2016-02-11*/
 ls_program_array[1] = 'P001'
 ls_program_array[2] = 'P007'
 
// insert a row in for primary and P&A
FOR li_counter = 1 TO upperbound(ls_program_array)	
	IF inv_provider_therapist.nf_check_license_program_exists(ll_wif_principal_id, ls_program_array[li_counter]) = FALSE THEN 
		li_row = lds_program_therapist.insertrow(0)
		lds_program_therapist.setitem(li_row, 'rehab_program_code',ls_program_array[li_counter])
		lds_program_therapist.setitem(li_row, 'therapist_no',ll_therapist_no)
		lds_program_therapist.setitem(li_row, 'active_flag','Y')
	END IF 	
NEXT

/* WORKFLOW_THERAPIST_APPROVAL - UPDATE  */
/* 4.50		The decision date must be set to the current date if a therapist is approved. */
/* 4.100 	The workflow token must be marked as used if the request for approval has been approved. */
dw_select_therapist.setitem(li_token_row, 'approval_status_code', '1') 
dw_select_therapist.setitem(li_token_row, 'decision_date', Date(f_server_datetime()) )
dw_select_therapist.setitem(li_token_row, 'approver_rejecter_user_id',  vgst_user_profile.user_id)



/* BR'S NOT CODED */
/* 
4.40		A rejected therapist must not be assigned a physiotherapy program.
4.60		A therapist must be approved for each license submitted for approval.
4.70 		A therapist must only be approved or rejected by the approver.
*/

/* updates and commits */
SQLCA.nf_begin_transaction()

//token info
dw_select_therapist.update()
SQLCA.nf_handle_error("w_maintain_provider_therapist","cb_approve-clicked","dw_select_therapist.update()")

//therapist info
dw_therapist_info.update()
SQLCA.nf_handle_error("w_maintain_provider_therapist","cb_approve-clicked","dw_therapist_info.update()")

//therapist license
lds_therapist_license.update()
SQLCA.nf_handle_error("w_maintain_provider_therapist","cb_approve-clicked","lds_therapist_license.update()")

//programs
lds_program_therapist.update()
SQLCA.nf_handle_error("w_maintain_provider_therapist","cb_approve-clicked","dw_program_therapist.update()")

SQLCA.nf_commit_transaction()

/* re-retrieve the token information */
dw_select_therapist.retrieve()
SQLCA.nf_handle_error("w_maintain_provider_therapist","cb_approve-clicked","dw_select_therapist.retrieve()")

/* scroll to the first row */
cb_refresh.triggerevent('clicked!')
dw_select_therapist.ScrollToRow(1)
dw_select_therapist.TriggerEvent(RowFocusChanged!)
dw_select_therapist.selectrow(1, true)

// ASK IF THE THERAPIST NEEDS TOBE MANAGED
ll_ReturnCode = MessageBox("Question!","You have approved " + ls_name+ " as a "+ ls_license_type_desc_e + ". Do you want to Manage this Therapist?",Question!,YesNo!)
IF ll_ReturnCode = 1 THEN 
	wf_manage_therapist(ll_therapist_no)	
END IF 







end event

type dw_clinics from u_dw_online within tabpage_request
integer x = 18
integer y = 1460
integer width = 3131
integer height = 556
integer taborder = 21
string dataobject = "d_providers_therapist_workflow"
boolean vscrollbar = true
end type

event constructor;call super::constructor;settransobject(sqlca)
end event

type cb_select from commandbutton within tabpage_request
integer x = 2839
integer y = 2056
integer width = 306
integer height = 92
integer taborder = 21
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Manage..."
end type

event clicked;LONG				ll_therapist_no
INTEGER			li_current_row

/*
	Token_id
	Record_no
	Therapist_wif_principal_id
	license_no
	license_type_code
	license_prov_state_code
	approval_request_date
	approval_request_status_code
	decision_date --- for rejected or approved
	Approver/rejecter id – internal id (user_id)
*/

li_current_row = dw_therapist_info.getrow()
IF isnull(li_current_row) OR li_current_row < 1 THEN RETURN 
	
ll_therapist_no = dw_therapist_info.getitemnumber(1,'therapist_no')
IF isnull(ll_therapist_no) OR ll_therapist_no < 1 THEN 
	messagebox('Therapist?','Please select a valid therapist')
END IF 

wf_manage_therapist(ll_therapist_no)	





end event

type dw_select_therapist from u_dw_online within tabpage_request
integer x = 18
integer y = 16
integer width = 3131
integer height = 940
integer taborder = 21
string dataobject = "d_workflow_therapist_approval"
boolean hscrollbar = true
boolean vscrollbar = true
end type

event constructor;call super::constructor;this.uf_setselect(1)
this.settransobject(sqlca)
end event

event rowfocuschanged;call super::rowfocuschanged;/* dw_select_therapist - Populates the providers based on the therapist	
*/

LONG			ll_wif_principal_id
INTEGER		li_rows

li_rows = dw_select_therapist.rowcount()

IF li_rows > 0 THEN 
	ll_wif_principal_id 	= dw_select_therapist.getitemnumber(this.getrow(),'therapist_wif_principal_id')
	
	dw_therapist_info.retrieve(ll_wif_principal_id)
	SQLCA.nf_handle_error("w_maintain_provider_therapist", "rowfocuschanged", "dw_therapist_info.retrieve(ll_wif_principal_id)")
	
	dw_clinics.retrieve(ll_wif_principal_id)
	SQLCA.nf_handle_error("w_maintain_provider_therapist", "rowfocuschanged", "dw_clinics.retrieve(ll_wif_principal_id)")
	
END IF 

end event

event retrieveend;call super::retrieveend;LONG			ll_wif_principal_id
INTEGER		li_counter
BOOLEAN	   lb_rejected, lb_previously_deleted, lb_reassign
STRING		ls_license_no, ls_license_type_code, ls_license_prov_state_code


FOR li_counter= 1 TO rowcount
	
	ll_wif_principal_id        = this.getitemnumber(li_counter,'therapist_wif_principal_id')
	ls_license_no              = this.getitemstring(li_counter,'license_no')
	ls_license_type_code       = this.getitemstring(li_counter,'license_type_code')
	ls_license_prov_state_code = this.getitemstring(li_counter,'license_prov_state_code')
	
	// GIVE THE USER INFO ON IF THERE ARE REJECTED LICENSES FOR RELATED TO THIS THERAPIST
	lb_rejected = wf_check_rejected_exists(ll_wif_principal_id)
	
	IF lb_rejected = TRUE THEN
		THIS.setitem(li_counter,'license_status', 'R')	
	END IF 
	
	lb_reassign = wf_check_reassign_exists(ls_license_no, ls_license_prov_state_code, ls_license_type_code)
	
	IF lb_reassign = TRUE THEN
		THIS.setitem(li_counter, 'reassign','1')
	ELSE
		THIS.setitem(li_counter, 'reassign','0')
	END IF 
	
NEXT 

/* If there is a rowcount > 0 check to see if the first one is a reasign - if it it unselect it so the button is visible
this is only applicable on the first retrieve once they user invokes the rowfocuschanged - all bets are off */

IF rowcount > 0  THEN
	IF THIS.getitemstring(1,'reassign') = '1'  THEN 
		this.selectrow(0,false)
	ELSE
		this.selectrow(1,TRUE)
	END IF 
END IF 



end event

event doubleclicked;call super::doubleclicked;LONG			ll_wif_principal_id
STRING		ls_license_status
s_window_message    lstr_message


IF this.rowcount() < 1 then return 


ll_wif_principal_id	=	THIS.getitemnumber(THIS.GETROW() ,'therapist_wif_principal_id')
ls_license_status	=	THIS.getitemstring(THIS.GETROW() ,'license_status')


IF TRIM(ls_license_status) <> '' THEN 
	IF ll_wif_principal_id > 0 THEN
		lstr_message.al_doubleparm[1] = ll_wif_principal_id
		Openwithparm(w_rejected_license_info , lstr_message)
	END IF 
END IF 









end event

event buttonclicked;call super::buttonclicked;LONG			ll_wif_principal_id, ll_record_no,  ll_therapist_no
STRING		ls_license_type_code, ls_license_prov_state_code, ls_approval_request_status_code, ls_license_no
STRING      ls_name, ls_license_type_desc_e, ls_token_id
DATE			ldt_approval_date, ldt_decision_date

S_WINDOW_MESSAGE lstr_message

/*    there is currently only one button on this datawindow

		SELECT a.token_id,   a.record_no,   a.therapist_wif_principal_id,   a.license_no,   
				 a.license_type_code,   a.license_prov_state_code,   a.approval_request_date,   
				 a.approval_status_code,  a.decision_date,   a.approver_rejecter_user_id,
				 b.first_name + ' ' + b.last_name as 'Name',	 c.approval_status_desc_e,
				 d.license_type_desc_e,		b.last_name, b.first_name,         a.rejected_comment,
				 '' AS 'license_status',         '' AS 'previously_deleted',       '' AS 'reassign'
		FROM workflow_therapist_approval  	a
		join WIF_CUSTOM_PRINCIPAL 				b on a.therapist_wif_principal_id = b.wif_principal_id
		join Approval_Status 					c on c.approval_status_code = a.approval_status_code 
		join License_Type 						d on d.license_type_code = a.license_type_code
		join WORKFLOW_TOKEN  					e ON a.token_id = e.token_id
		where c.approval_status_code 		= '0'
		and e.token_status_code 			= '0'
	 
*/

/* grab the informationwe need from the WORKFLOW_THERAPIST_APPROVAL table */
ls_token_id                      = 	dw_select_therapist.getitemstring(row ,'token_id')
ll_record_no 							= 	dw_select_therapist.getitemnumber(row ,'record_no')
ll_wif_principal_id					=	dw_select_therapist.getitemnumber(row ,'therapist_wif_principal_id')
ls_license_no 							= 	dw_select_therapist.getitemstring(row ,'license_no')
ls_license_type_code					=	dw_select_therapist.getitemstring(row ,'license_type_code')
ls_license_prov_state_code			=	dw_select_therapist.getitemstring(row ,'license_prov_state_code')
ldt_approval_date 					= 	date(dw_select_therapist.getitemdatetime(row ,'approval_request_date'))
ls_approval_request_status_code	=	dw_select_therapist.getitemstring(row ,'approval_status_code')
ldt_decision_date 					= 	date(dw_select_therapist.getitemdatetime(row ,'decision_date'))
ls_name 									= 	dw_select_therapist.getitemstring(row ,'name')
ls_license_type_desc_e 				= 	dw_select_therapist.getitemstring(row ,'license_type_desc_e')

// just a final check just in case
IF wf_check_reassign_exists(ls_license_no, ls_license_prov_state_code , ls_license_type_code )= FALSE THEN RETURN 

/* Do Some basic checks to make sure everything will turn out good */

/* requesting user portal account must still be active */
IF inv_provider_therapist.nf_portal_account_is_active(ll_wif_principal_id) = FALSE THEN
	MessageBox("Therapist Not Active", "The Requesting User Portal Account must be active.", Information! )
	RETURN -1		
END IF 

IF  inv_provider_therapist.nf_check_therapist_license_exists(ll_wif_principal_id, ls_license_type_code, ls_license_prov_state_code) = TRUE THEN 
	 MessageBox("Therapist Already approved", "Please use the manage therapist screen to add or remove programs.", Information! )
	RETURN -1	
END IF 

/*
 token_status_code 		token_status_desc_e                               
----------------- 				--------------------    
0                 				Available           
1                 				Used                
2                 				Expired             
3                 				Cancelled          
99                				Error    
*/


/*  4.20		A request for approval must not be available for approval if the workflow token has expired. -- this should never happen*/
IF inv_provider_therapist.nf_get_token_status(ll_record_no, ll_wif_principal_id, ls_license_no) = 2  THEN 
	 MessageBox("Token Expired", "A request for approval must not be available for approval if the workflow token has expired.", Information! )
	RETURN -1	
END IF 

/* 4.30		A request for approval must not be available for approval if the workflow token is cancelled. */
IF inv_provider_therapist.nf_get_token_status(ll_record_no, ll_wif_principal_id, ls_license_no) = 3  THEN 
	 MessageBox("Token Cancelled", "A request for approval must not be available for approval if the workflow token is cancelled.", Information! )
	RETURN -1	
END IF 

/* 4.40		A rejected therapist must not be assigned a physiotherapy program. */
IF inv_provider_therapist.nf_has_therapist_been_rejected(ll_record_no, ll_wif_principal_id, ls_license_no) > 0 THEN 
	 MessageBox("Therapist Rejected", "A rejected therapist must not be assigned a physiotherapy program. ", Information! )
	RETURN 1	
END IF

/* •	The license number must be unique for the license type and the licensing  province 
		incorrectbased on this idx.... Cannot insert duplicate key row in object 'dbo.THERAPIST_LICENSE' with unique index 'idx2'. The duplicate key value is (999).
		No changes made to database. -- this will need to be changed or the index will need to be changed
		
		NOT Applicable - we are not creating a new therapist - we are reassigning a therapist
*/
//IF inv_provider_therapist.nf_is_license_unique(ls_license_no, ls_license_type_code, ls_license_prov_state_code) = FALSE THEN
//	MessageBox('Error','The license number must be unique for the license type and the licensing  province',Exclamation!)
//	RETURN -1	
//END IF 

IF inv_provider_therapist.nf_check_therapist_exists(ll_wif_principal_id) THEN 
	
	ll_therapist_no = inv_provider_therapist.nf_get_therapist_no(ll_wif_principal_id)

	/* paul has the Primary key for license table as therapist_no,license_type and license_no which isnot the same as above */
	IF inv_provider_therapist.nf_is_license_unique_key(ll_therapist_no, ls_license_type_code, ls_license_no) = FALSE THEN
		MessageBox('Error','The license number must be unique for the therapist number, license type code and license no ',Exclamation!)
		RETURN -1	
	END IF
		
	/* Rehab programs cannot be moved if any of the delisted dates are set */
	IF wf_delisted_date_exists(ll_therapist_no) = TRUE THEN 
		 MESSAGEBOX('Therapist Delisted', 'This therapist has a license that has been delisted. You must un de-list this therapist in order to approve this license.', stopsign!)
		 RETURN -1
	END IF 		
END IF 


/* if it made it to here everything passed open the screen and allow them to re-assign */
lstr_message.al_doubleparm[1] = ll_wif_principal_id
lstr_message.as_stringparm[1] = ls_license_no
lstr_message.as_stringparm[2] = ls_license_type_code
lstr_message.as_stringparm[3] = ls_license_prov_state_code

/* open the window */
OpenWithParm(w_reassign_therapist, lstr_message)

// refresh everything
cb_refresh.triggerevent(clicked!)

//scroll back to row you were on
IF dw_select_therapist.rowcount() >= row  THEN 
	dw_select_therapist.selectrow(0, false)
	dw_select_therapist.selectrow(row, true)
	
END IF 







end event

type tabpage_provider from userobject within tab_maintain
integer x = 18
integer y = 100
integer width = 3182
integer height = 2388
long backcolor = 67108864
string text = "Maintain Provider"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
st_save_required st_save_required
st_1 st_1
cb_one_back cb_one_back
cb_all_back cb_all_back
cb_one_to cb_one_to
cb_all_to cb_all_to
dw_rehab_program dw_rehab_program
cb_save cb_save
cb_cancel cb_cancel
dw_rehab_programs dw_rehab_programs
dw_clinic dw_clinic
dw_provider dw_provider
st_provider_name st_provider_name
cb_provider_search cb_provider_search
r_1 r_1
end type

on tabpage_provider.create
this.st_save_required=create st_save_required
this.st_1=create st_1
this.cb_one_back=create cb_one_back
this.cb_all_back=create cb_all_back
this.cb_one_to=create cb_one_to
this.cb_all_to=create cb_all_to
this.dw_rehab_program=create dw_rehab_program
this.cb_save=create cb_save
this.cb_cancel=create cb_cancel
this.dw_rehab_programs=create dw_rehab_programs
this.dw_clinic=create dw_clinic
this.dw_provider=create dw_provider
this.st_provider_name=create st_provider_name
this.cb_provider_search=create cb_provider_search
this.r_1=create r_1
this.Control[]={this.st_save_required,&
this.st_1,&
this.cb_one_back,&
this.cb_all_back,&
this.cb_one_to,&
this.cb_all_to,&
this.dw_rehab_program,&
this.cb_save,&
this.cb_cancel,&
this.dw_rehab_programs,&
this.dw_clinic,&
this.dw_provider,&
this.st_provider_name,&
this.cb_provider_search,&
this.r_1}
end on

on tabpage_provider.destroy
destroy(this.st_save_required)
destroy(this.st_1)
destroy(this.cb_one_back)
destroy(this.cb_all_back)
destroy(this.cb_one_to)
destroy(this.cb_all_to)
destroy(this.dw_rehab_program)
destroy(this.cb_save)
destroy(this.cb_cancel)
destroy(this.dw_rehab_programs)
destroy(this.dw_clinic)
destroy(this.dw_provider)
destroy(this.st_provider_name)
destroy(this.cb_provider_search)
destroy(this.r_1)
end on

type st_save_required from statictext within tabpage_provider
integer x = 1723
integer y = 2188
integer width = 814
integer height = 88
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 255
long backcolor = 67108864
string text = "Save / Cancel Required"
alignment alignment = right!
boolean focusrectangle = false
end type

type st_1 from statictext within tabpage_provider
integer x = 1810
integer y = 1396
integer width = 763
integer height = 72
integer textsize = -12
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 134217857
long backcolor = 67108864
string text = "Current Programs"
boolean focusrectangle = false
end type

type cb_one_back from commandbutton within tabpage_provider
integer x = 1472
integer y = 1784
integer width = 151
integer height = 92
integer taborder = 80
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "<"
end type

event clicked;IF dw_rehab_programs.rowcount() < 1 THEN RETURN 

wf_drag_programs(2)
end event

type cb_all_back from commandbutton within tabpage_provider
integer x = 1472
integer y = 1680
integer width = 151
integer height = 92
integer taborder = 80
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "<<"
end type

event clicked;INTEGER			li_counter

IF dw_rehab_programs.rowcount() < 1 THEN RETURN


FOR li_counter = 1 TO dw_rehab_programs.rowcount()
	dw_rehab_programs.selectrow(li_counter,true)
NEXT

wf_drag_programs(2)
end event

type cb_one_to from commandbutton within tabpage_provider
integer x = 1472
integer y = 1888
integer width = 151
integer height = 92
integer taborder = 70
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = ">"
end type

event clicked;IF dw_rehab_program.rowcount() < 1 THEN RETURN 

wf_drag_programs(1)
end event

type cb_all_to from commandbutton within tabpage_provider
integer x = 1472
integer y = 1992
integer width = 151
integer height = 92
integer taborder = 60
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = ">>"
end type

event clicked;INTEGER			li_counter


IF dw_rehab_program.rowcount() < 1 THEN RETURN 

FOR li_counter = 1 TO dw_rehab_program.rowcount()
	
	dw_rehab_program.selectrow(li_counter, true)
	
NEXT 

wf_drag_programs(1)
end event

type dw_rehab_program from u_dw_online within tabpage_provider
integer x = 5
integer y = 1484
integer width = 1271
integer height = 668
integer taborder = 70
string dataobject = "d_rehab_program"
end type

event constructor;call super::constructor;this.uf_setselect(3)
end event

type cb_save from commandbutton within tabpage_provider
integer x = 2574
integer y = 2184
integer width = 274
integer height = 88
integer taborder = 50
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Save"
end type

event clicked;INTEGER			li_program_count, li_counter, li_primary = 0, li_pa = 0,li_x
LONG				ll_provider_no  , ll_len, ll_count_at,ll_count_space
STRING         	ls_provider_type_code, ls_provider_sub_type_code, ls_name, ls_ephysio_flag, ls_physio_contract_flag, ls_program_active_flag
STRING			ls_provider_sub_type, ls_active_flag, ls_rehab_program_code, ls_prov_code, ls_email, ls_hours, ls_string

/*

//ck_provider_27
//([physio_contract_flag]='Y' AND [ephysio_flag]='Y' AND ([prov_state_code]='NS' OR [prov_state_code]='NB') 
//AND [provider_sub_type_code]='35' AND [provider_type_code]='M' AND [active_flag]='Y' OR [physio_contract_flag]='N' AND [ephysio_flag]='Y'
//AND ([prov_state_code]='NS' OR [prov_state_code]='NB') AND [provider_sub_type_code]='35' AND [provider_type_code]='M' 
//AND [active_flag]='Y' OR [physio_contract_flag]='N' AND [ephysio_flag]='N')

*/

/* SECTION 3 OF BR_CMWB_PROVIDER_MAINTENANCE_V11 
Business Rules for the Physio Contract and ePhysio Flags:
Refer to Section 3	Physio Providers in the BR_CMWB_Provider_Maintenance document

3	Physio Providers (THE FOLLOWING RULES ARE NOT APPLICABLE TO THIS MODULE - THE REST OF THE RULES ARE REFERENCED IN THE CODE BELOW )							

3.50	All planned rehab tasks assigned to the provider must be cancelled if the provider is set to inactive (active flag = ‘N’) and the provider is a Physio Clinic.
3.60 	All ‘in progress’ rehab tasks assigned to the provider must be closed if the provider is set to inactive (active end date set) and the provider is a Physio Clinic.
3.70	The actual completion date of the ‘closed’ task must be set to the active end date (inactive date) of the provider.  (See Rationale)

*/
/*
##2.10	A Physiotherapy provider must be approved for at least one Physiotherapy program (e.g. Work Conditioning, Shoulder program, Back program) if the provider is active.
##2.20 	A Physiotherapy provider must have an active approval for the Primary Physiotherapy program if the provider is active.  
##2.30 	A Provider must be a Physiotherapy provider (medical aid provider/physio clinic) to be approved for a Physiotherapy program (e.g. Work Conditioning, Shoulder program, Back program)	
##2.40	A Physiotherapy provider must only be approved for a rehab program that is a Physiotherapy type of program.
##2.50	A Physiotherapy provider must have an active approval for the P & A program if the provider is active
##2.60	The electronic physiotherapy billing and reporting (ephysio flag) must be ‘No’ if the provider is not a physiotherapy clinic.
##2.70	The electronic physiotherapy billing and reporting (ephysio flag) must be ‘Yes’ or ‘No’ if the provider is a physiotherapy clinic.
##2.80	The electronic physiotherapy billing and reporting (ephysio flag) should be ‘Yes’ if the provider is a physiotherapy clinic and the provider is active and the provider’s province/state is New Brunswick.
##2.90   The electronic physiotherapy billing and reporting (ephysio flag) should be ‘No’ if the provider is a physiotherapy clinic, the provider is active and the provider’s province/state is not New Brunswick.
##2.100 The physio contract must not be ‘Yes’ if the provider is not set up for electronic billing (ephysio flag = ‘N’)
##2.110 The physio contract must be ‘No’ if provider is not a physiotherapy clinic.
*/

dw_provider.accepttext()
dw_rehab_programs.accepttext()

IF dw_provider.rowcount() < 1 THEN RETURN

//IF st_save_required.visible = FALSE THEN RETURN 

// dw_provider
ls_active_flag 						= dw_provider.getitemstring(1,'active_flag')
ll_provider_no 						= dw_provider.getitemnumber(1, 'provider_no')
ls_provider_type_code 			= dw_provider.getitemstring(1, 'provider_type_code')
ls_provider_sub_type_code 		= dw_provider.getitemstring(1, 'provider_sub_type_code')
ls_name 								= dw_provider.getitemstring(1, 'name')
ls_ephysio_flag 					= dw_provider.getitemstring(1, 'ephysio_flag')
ls_physio_contract_flag 			= dw_provider.getitemstring(1, 'physio_contract_flag')
ls_prov_code						= dw_provider.getitemstring(1, 'prov_state_code')
ls_email								= dw_provider.getitemstring(1, 'email_address')
ls_hours								= dw_provider.getitemstring(1, 'hours_of_operation')
 

IF isnull(ls_active_flag) THEN ls_active_flag = ''

/* This functionality is only applicable for a Provider that is has a sub-type of Physiotherapy Clinic – 
	it does not matter whether the physiotherapy clinic is under the Physio contract or whether the physiotherapy clinic is set up for ePhysio.
*/
 IF inv_provider_therapist.nf_get_provider_subtype(ll_provider_no) <> '35' THEN //physio_clinic
	messagebox('Information', 'This functionality is only applicable for a Provider that  has a sub-type of Physiotherapy Clinic ')
	RETURN -1
 END IF 

li_program_count = dw_rehab_programs.rowcount()
IF ISNULL(li_program_count) THEN li_program_count = 0

//2.30 	A Provider must be a Physiotherapy provider (medical aid provider/physio clinic) to be approved for a Physiotherapy program (e.g. Work Conditioning, Shoulder program, Back program)
IF ls_provider_type_code <> 'M' OR ls_provider_sub_type_code <> '35' THEN
	messagebox('Information', 'This module is only applicable for Physio Clinics')
	RETURN -1	
END IF 

IF ls_active_flag = 'Y' AND li_program_count = 0  THEN
	
	// 2.10	A Physiotherapy provider must be approved for at least one Physiotherapy program (e.g. Work Conditioning, Shoulder program, Back program) if the provider is active.
	messagebox('Information', 'A Physiotherapy provider must be approved for at least one Physiotherapy program (e.g. Work Conditioning, Shoulder program, Back program) if the provider is active.')
	RETURN -1	
END IF 
		
//dw_rehab_programs
FOR li_counter = 1 TO li_program_count
	ls_program_active_flag 		= dw_rehab_programs.getitemstring( li_counter, 'active_flag')
	ls_rehab_program_code 		= dw_rehab_programs.getitemstring( li_counter, 'rehab_program_code')
	
	IF ls_rehab_program_code = 'P001'   AND ls_program_active_flag = 'Y' THEN li_primary ++
	IF ls_rehab_program_code = 'P007'  AND ls_program_active_flag = 'Y'  THEN li_pa ++
	
	/* 2.40		A Physiotherapy provider must only be approved for a rehab program that is a Physiotherapy type of program. */
					// should never happen taken from dropdown
	IF inv_provider_therapist.nf_is_program_a_rehab_program( ls_rehab_program_code) = FALSE THEN 
		messagebox('Error', 'A Physiotherapy provider must only be approved for a rehab program that is a Physiotherapy type of program.')
		RETURN -1
	END IF 	
NEXT

IF  ls_active_flag = 'Y' THEN 
	
	// 2.20	A Physiotherapy provider must have an active approval for the Primary Physiotherapy program if the provider is active.  
	IF li_primary = 0 THEN 
		messagebox('Information', 'A Physiotherapy provider must be approved for the Primary Physiotherapy program if the provider is active.')
		RETURN -1
	END IF 
	
	// 2.50	A Physiotherapy provider must have an active approval for the P & A program if the provider is active REMOVED
	// 2.130	A Physiotherapy provider must have an active approval for the Functional Outcome test Scores program if the provider is active.  
	IF li_pa = 0 THEN 
		messagebox('Information', 'A Physiotherapy provider must have an active approval for the Functional Outcome test Scores if the provider is active')
		RETURN -1
	END IF 
	
	
END IF 

// 2.60	The electronic physiotherapy billing and reporting (ephysio flag) must be ‘No’ if the provider is not a physiotherapy clinic.
// should never happen
IF  ls_provider_sub_type_code <> '35' AND ls_ephysio_flag = 'N' THEN
	messagebox('Information', 'The electronic physiotherapy billing and reporting (ephysio flag) must be ‘No’ if the provider is not a physiotherapy clinic.')
	RETURN -1
END IF 

//Validate the ePhysio flag
IF ls_provider_type_code = 'M' and ls_provider_sub_type_code = '35' THEN
	
	// 2.70	The electronic physiotherapy billing and reporting (ephysio flag) must be ‘Yes’ or ‘No’ if the provider is a physiotherapy clinic.
	IF ls_ephysio_flag <> 'Y' AND ls_ephysio_flag <> 'N' THEN
		MessageBox('Error','The electronic physiotherapy billing and reporting (ephysio flag) must be ‘Yes’ or ‘No’ if the provider is a physiotherapy clinic..',exclamation!)
		RETURN -1
	END IF
	
	// 2.80	The electronic physiotherapy billing and reporting (ephysio flag) should be ‘Yes’ if the provider is a physiotherapy clinic and the provider is active and the provider’s province/state is New Brunswick.
	IF ls_ephysio_flag = 'N' and ls_prov_code = 'NB' and ls_active_flag = 'Y' THEN
		MessageBox('Warning','The electronic physiotherapy billing and reporting (ephysio flag) should be ‘Yes’ if the provider is a physiotherapy clinic and the provider is active and the provider’s province/state is New Brunswick.',information!) 
	END IF
	
	//2.90   The electronic physiotherapy billing and reporting (ephysio flag) should be ‘No’ if the provider is a physiotherapy clinic, the provider is active and the provider’s province/state is not New Brunswick.
	IF ls_ephysio_flag = 'Y' and ls_prov_code <> 'NB' and ls_active_flag = 'Y' THEN
		MessageBox('Warning','The electronic physiotherapy billing and reporting (ephysio flag) should be ‘No’ if the provider is a physiotherapy clinic, the provider is active and the provider’s province/state is not New Brunswick.',information!) 
	END IF
END IF

IF ls_ephysio_flag = 'Y' OR ls_physio_contract_flag = 'Y' THEN
	
	// 3.90	The physio contract must be ‘No’ if provider is not a physiotherapy clinic.
	IF ls_provider_type_code <> 'M' OR (ls_provider_type_code = 'M' and ls_provider_sub_type_code <> '35') THEN
		MessageBox('Error','The provider must be a Physio provider when the ePhysio Billing flag or Contract flag are on.',Exclamation!)
		RETURN -1
	END IF
	
	// NOT SURE WHAT RULE NUMBER THIS IS
	IF ls_prov_code <> 'NB' AND ls_prov_code <> 'NS' THEN
		MessageBox('Error','The provider must be a NB or NS provider when the ePhysio Billing flag or Contract flag are on.',Exclamation!)
		RETURN -1
	END IF	
	
	// /2.100 The physio contract must not be ‘Yes’ if the provider is not set up for electronic billing (ephysio flag = ‘N’)
	IF  ls_ephysio_flag = 'N' and ls_physio_contract_flag = 'Y' THEN
		MessageBox('Error','The physio contract must not be ‘Yes’ if the provider is not set up for electronic billing (ephysio flag = ‘N’)',Exclamation!)
		RETURN -1
	END IF	
	
	IF ls_active_flag = 'N' THEN
		MessageBox('Error','The provider must be active when the ePhysio Billing and Contract flags are on.',Exclamation!)
		RETURN -1
	END IF			
END IF

IF ls_hours <> '' AND dw_provider.GetItemStatus(1,'hours_of_operation',Primary!)= DataModified! THEN	
	ls_hours  =TRIM(ls_hours)
	dw_provider.SetItem(1,"hours_of_operation",ls_hours)
END IF	


IF NOT IsNull(ls_email) and ls_email <> '' THEN
	
	IF LEN(ls_email) > 120 THEN
		MessageBox('Error Invalid Email','The email address must be valid and not more than 120 characters.',Exclamation!)
		Return -1
	END IF		
	
	IF LEN(ls_email) < 5  OR Pos(ls_email, "@") < 2  OR Pos(ls_email, ".") < 3 THEN
		MessageBox('Error Invalid Email','The email address must be in proper format ex. name@nb.ca .',Exclamation!)
		Return -1
	END IF
	IF Pos(ls_email,'.-') <> 0 OR   Pos(ls_email,'-.') <> 0 OR   Pos(ls_email,'..') <> 0 THEN
		MessageBox('Error Invalid Email',"The email address must not contain '.- 'or '-.' or '..' .",Exclamation!)
		Return -1
	END IF
	IF Pos(ls_email,'@@') <> 0 OR Pos(ls_email,'--') <> 0 OR  Pos(ls_email,'@.') <> 0 THEN
		MessageBox('Error Invalid Email',"The email address must not contain '@@ 'or '--' or '@.' .",Exclamation!)
		Return -1
	END IF
	IF Pos(ls_email,'.@') <> 0 THEN
		MessageBox('Error Invalid Email',"The email address must not contain '.@ '.",Exclamation!)
		Return -1
	END IF
	IF Pos(ls_email,' ') <> 0 THEN
		MessageBox('Error Invalid Email',"The email address must not contain a space.",Exclamation!)
		Return -1
	END IF	
	IF Pos(ls_email, "@") = 1  OR Pos(ls_email, ".") = 1 OR  Pos(ls_email, " ") = 1 THEN
		MessageBox('Error Invalid Email','The email address must be more than 5 characters.',Exclamation!)
		Return -1
	END IF
	ll_len = LEN(ls_email)
	
	FOR li_x = 1 to ll_len
		ls_string = left(ls_email,li_x)
		IF MID(ls_email,li_x,1) = '@'  then
			ll_count_at = ll_count_at + 1
		END IF
		IF MID(ls_email,li_x,1) = ' ' then
			ll_count_space = ll_count_space + 1
		END IF
	NEXT
	IF ll_count_at > 1 THEN
		MessageBox('Error Invalid Email',"The email address can not have more than one '@' character.",Exclamation!)
		Return -1
	END IF
	IF ll_count_space > 0 THEN
		MessageBox('Error Invalid Email',"The email address can not have a space in it.",Exclamation!)
		Return -1
	END IF
	
END IF

/*********************** UPDATE SECTION ***********************/
/*
3.2.2	Database Updates
This will be used to populate the following database tables:
•	PROVIDER 
•	REHAB_PROGRAM_XREF_PROVIDER 
*/

SQLCA.nf_begin_transaction()

dw_provider.update()
SQLCA.nf_handle_error("w_maintain_provider_therapist", "cb_save - clicked", "dw_provider.update()")

dw_rehab_programs.update()
SQLCA.nf_handle_error("w_maintain_provider_therapist", "cb_save - clicked", "dw_rehab_programs.update()")

SQLCA.nf_commit_transaction()

wf_set_save_required_on(false)

messagebox('Success','Save Completed')
end event

type cb_cancel from commandbutton within tabpage_provider
integer x = 2871
integer y = 2184
integer width = 274
integer height = 88
integer taborder = 50
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cancel"
boolean default = true
end type

event clicked;LONG			ll_provider_no
STRING		ls_name, ls_provider_type
INTEGER		li_row


IF  dw_provider.rowcount() > 0  THEN 
	ll_provider_no = dw_provider.getitemnumber(1,'provider_no')
END IF 
		
dw_provider.reset()
dw_rehab_programs.reset()
dw_rehab_programs.insertrow(0)

IF ll_provider_no > 0 THEN 
	ls_provider_type = 'M'
	ls_name = wf_get_provider_name(ls_provider_type,ll_provider_no)	

	st_provider_name.text = ls_name
		
	//reset the search dw
	li_row = tab_maintain.tabpage_provider.dw_clinic.insertrow(0)
	tab_maintain.tabpage_provider.dw_clinic.setitem(li_row,'name','Please select a Provider...')
	tab_maintain.tabpage_provider.dw_clinic.scrolltorow(li_row)
		
	wf_reset_dws_for_provider()
	
	wf_retrieve_dws_for_provider(ll_provider_no)
	
	wf_set_save_required_on(false)
	
END IF 
end event

type dw_rehab_programs from u_dw_online within tabpage_provider
integer x = 1806
integer y = 1484
integer width = 1339
integer height = 668
integer taborder = 40
string dataobject = "d_provider_program"
end type

event constructor;call super::constructor;this.uf_setselect(3)
end event

event dragdrop;call super::dragdrop;

end event

event retrieveend;call super::retrieveend;LONG		ll_rows, ll_item
STRING	ls_program_code

IF rowcount <= 0 OR isnull(rowcount) THEN
	
	ls_program_code = 'P001'

	//CHECK AND SEE WHAT PROGRAMS ARE THERE IF APPLICABLE ADD THE PHYSIO
	ll_rows = dw_rehab_program.RowCount()
	
	IF ll_rows > 0 THEN 
	
		ll_item = dw_rehab_program.Find("rehab_program_code = '" + TRIM(ls_program_code) + "'", 1, ll_rows)
		
		dw_rehab_program.SelectRow(0, false)
		dw_rehab_program.SelectRow(ll_item, true)
		
		cb_one_to.triggerevent('clicked')
		
	END IF 
	
END IF 
end event

type dw_clinic from u_dw_online within tabpage_provider
integer x = 32
integer y = 64
integer width = 2537
integer height = 88
integer taborder = 30
string dataobject = "d_ephysio_provider_controller_clinic"
boolean border = false
boolean livescroll = false
end type

event constructor;call super::constructor;Integer li_row

THIS.uf_setselect(3)



end event

event ue_itemchangeaccepted;call super::ue_itemchangeaccepted;/* 
Populates the users based on the Provider	

SELECT provider_no,            	provider_type_code,            provider_sub_type_code,   
         	name,            			sort_name,            			email_address,   
         	ephysio_flag  
FROM 	PROVIDER   
WHERE 	ephysio_flag 	= 'Y'
AND 		active_flag 		= 'Y'
*/
datawindowchild 	ldwc_provider
INTEGER				li_rows,  li_provider_row
LONG					ll_provider_no
STRING				ls_provider_type_code 

THIS.getchild('name', ldwc_provider)


li_rows 						= ldwc_provider.rowcount()
li_provider_row 			= ldwc_provider.getrow()
ls_provider_type_code 	= 'M'

IF li_rows > 0 THEN 
	ll_provider_no 	= ldwc_provider.getitemnumber(li_provider_row,'provider_no')
END IF 

st_provider_name.text = ''

wf_reset_dws_for_provider()

wf_retrieve_dws_for_provider(ll_provider_no)

wf_set_save_required_on(FALSE)



end event

type dw_provider from u_dw_online within tabpage_provider
integer x = 5
integer y = 364
integer width = 3141
integer height = 1028
integer taborder = 30
string dataobject = "d_provider_for_update"
end type

event ue_itemchangeaccepted;call super::ue_itemchangeaccepted;//CHOOSE CASE as_column_name
//	CASE 'physio_contract_flag'
//
//		wf_set_save_required_on(true)
//		
//	CASE 'ephysio_flag'
//		
//		wf_set_save_required_on(true)
//		
//	CASE 'service_offered_language_code'
//		
//		wf_set_save_required_on(true)
//		
//	CASE 'preferred_correspond_language_code'
//		
//		wf_set_save_required_on(true)
//		
//	CASE 'hours_of_operation'
//				
//		wf_set_save_required_on(true)
//				
//	CASE 'email_address'
//						
//		wf_set_save_required_on(true)
//		
//		
//END CHOOSE
end event

type st_provider_name from statictext within tabpage_provider
integer x = 73
integer y = 216
integer width = 2144
integer height = 92
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
boolean focusrectangle = false
end type

type cb_provider_search from commandbutton within tabpage_provider
integer x = 2683
integer y = 60
integer width = 320
integer height = 92
integer taborder = 20
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Search"
end type

event clicked;STRING			ls_provider_type,  ls_name
LONG				ll_provider_no
INTEGER			li_row

S_WINDOW_MESSAGE		lstr_message
	
/*	get the type to search for */
ls_provider_type 	= 'M'
		
IF ls_provider_type = 'V' OR ls_provider_type =  'M' OR ls_provider_type =  'O' THEN
	OpenWithParm(w_clinic_search, 'X')
	lstr_message = Message.PowerObjectParm
	IF lstr_message.al_doubleparm[1] > 0 THEN
				
		ll_provider_no =  lstr_message.al_doubleparm[1]
					
		ls_name = wf_get_provider_name(ls_provider_type,ll_provider_no)	

		st_provider_name.text = ls_name
		
		//reset the search dw
		li_row = tab_maintain.tabpage_provider.dw_clinic.insertrow(0)
		tab_maintain.tabpage_provider.dw_clinic.setitem(li_row,'name','Please select a Provider...')
		tab_maintain.tabpage_provider.dw_clinic.scrolltorow(li_row)
		
		wf_reset_dws_for_provider()
		wf_retrieve_dws_for_provider(ll_provider_no)
				
	END IF
END IF 
end event

type r_1 from rectangle within tabpage_provider
long linecolor = 33554432
integer linethickness = 4
long fillcolor = 67108864
integer x = 9
integer y = 40
integer width = 3154
integer height = 296
end type

type tabpage_therapist from userobject within tab_maintain
integer x = 18
integer y = 100
integer width = 3182
integer height = 2388
long backcolor = 67108864
string text = "Maintain Therapist"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
cb_5 cb_5
cb_cancel_therapist cb_cancel_therapist
st_2 st_2
cb_4 cb_4
cb_3 cb_3
cb_2 cb_2
cb_1 cb_1
dw_eligible_programs dw_eligible_programs
dw_therapist_program dw_therapist_program
dw_provider_therapist dw_provider_therapist
dw_search_results dw_search_results
cb_therapist_search cb_therapist_search
cb_save_therapist cb_save_therapist
dw_therapist_license dw_therapist_license
end type

on tabpage_therapist.create
this.cb_5=create cb_5
this.cb_cancel_therapist=create cb_cancel_therapist
this.st_2=create st_2
this.cb_4=create cb_4
this.cb_3=create cb_3
this.cb_2=create cb_2
this.cb_1=create cb_1
this.dw_eligible_programs=create dw_eligible_programs
this.dw_therapist_program=create dw_therapist_program
this.dw_provider_therapist=create dw_provider_therapist
this.dw_search_results=create dw_search_results
this.cb_therapist_search=create cb_therapist_search
this.cb_save_therapist=create cb_save_therapist
this.dw_therapist_license=create dw_therapist_license
this.Control[]={this.cb_5,&
this.cb_cancel_therapist,&
this.st_2,&
this.cb_4,&
this.cb_3,&
this.cb_2,&
this.cb_1,&
this.dw_eligible_programs,&
this.dw_therapist_program,&
this.dw_provider_therapist,&
this.dw_search_results,&
this.cb_therapist_search,&
this.cb_save_therapist,&
this.dw_therapist_license}
end on

on tabpage_therapist.destroy
destroy(this.cb_5)
destroy(this.cb_cancel_therapist)
destroy(this.st_2)
destroy(this.cb_4)
destroy(this.cb_3)
destroy(this.cb_2)
destroy(this.cb_1)
destroy(this.dw_eligible_programs)
destroy(this.dw_therapist_program)
destroy(this.dw_provider_therapist)
destroy(this.dw_search_results)
destroy(this.cb_therapist_search)
destroy(this.cb_save_therapist)
destroy(this.dw_therapist_license)
end on

type cb_5 from commandbutton within tabpage_therapist
integer y = 304
integer width = 855
integer height = 92
integer taborder = 30
integer textsize = -12
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = roman!
string facename = "Times New Roman"
string text = "View Rejected History"
end type

event clicked;LONG							ll_wif_principal_id, ll_row
				
s_window_message    	lstr_message

IF dw_search_results.rowcount() <= 0  THEN RETURN
      
ll_row = dw_search_results.getrow()

IF ll_row <= 0 THEN RETURN 

ll_wif_principal_id	=	dw_search_results.getitemnumber(ll_row ,'wif_principal_id')

IF ll_wif_principal_id > 0 THEN
	lstr_message.al_doubleparm[1] = ll_wif_principal_id
	Openwithparm(w_rejected_license_info , lstr_message)
END IF 

end event

type cb_cancel_therapist from commandbutton within tabpage_therapist
integer x = 2907
integer y = 2176
integer width = 265
integer height = 92
integer taborder = 60
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "&Cancel"
boolean default = true
end type

event clicked;/* reset the dw's */
wf_reset_dws_for_therapist()
wf_retrieve_dws_for_therapist()

IF dw_search_results.rowcount() > 0 THEN 
	
	dw_search_results.triggerevent('rowfocuschanged')
	
END IF 
end event

type st_2 from statictext within tabpage_therapist
integer x = 1678
integer y = 1416
integer width = 763
integer height = 72
integer textsize = -12
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 134217857
long backcolor = 67108864
string text = "Current Programs"
boolean focusrectangle = false
end type

type cb_4 from commandbutton within tabpage_therapist
integer x = 1394
integer y = 1616
integer width = 155
integer height = 92
integer taborder = 90
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "<<"
end type

event clicked;INTEGER			li_counter

IF dw_therapist_program.rowcount() < 1 THEN RETURN

FOR li_counter = 1 TO dw_therapist_program.rowcount()
	dw_therapist_program.selectrow(li_counter,true)
NEXT

wf_drag_therapist_programs(2)
end event

type cb_3 from commandbutton within tabpage_therapist
integer x = 1394
integer y = 1724
integer width = 155
integer height = 92
integer taborder = 90
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "<"
end type

event clicked;LONG				ll_therapist_no
STRING			ls_program_code
INTEGER			li_counter

IF dw_therapist_program.rowcount() < 1 THEN RETURN

wf_drag_therapist_programs(2)


end event

type cb_2 from commandbutton within tabpage_therapist
integer x = 1394
integer y = 1832
integer width = 155
integer height = 92
integer taborder = 80
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = ">"
end type

event clicked;IF dw_eligible_programs.rowcount() < 1 THEN RETURN

wf_drag_therapist_programs(1)
end event

type cb_1 from commandbutton within tabpage_therapist
integer x = 1394
integer y = 1940
integer width = 155
integer height = 92
integer taborder = 70
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = ">>"
end type

event clicked;INTEGER			li_counter

IF dw_eligible_programs.rowcount() < 1 THEN RETURN 

FOR li_counter = 1 TO dw_eligible_programs.rowcount()
	
	dw_eligible_programs.selectrow(li_counter, true)
	
NEXT 

wf_drag_therapist_programs(1)
end event

type dw_eligible_programs from u_dw_online within tabpage_therapist
integer x = 5
integer y = 1500
integer width = 1257
integer height = 664
integer taborder = 11
string dataobject = "d_rehab_program_therapist"
end type

event constructor;call super::constructor;settransobject(sqlca)

uf_setselect(3)
end event

type dw_therapist_program from u_dw_online within tabpage_therapist
integer x = 1687
integer y = 1500
integer width = 1339
integer height = 664
integer taborder = 11
string dataobject = "d_program_therapist_xref"
end type

event constructor;call super::constructor;settransobject(sqlca)

uf_setselect(3)
end event

type dw_provider_therapist from u_dw_online within tabpage_therapist
integer x = 5
integer y = 1060
integer width = 3173
integer height = 332
integer taborder = 11
string dataobject = "d_providers_therapist"
boolean vscrollbar = true
end type

event constructor;call super::constructor;settransobject(sqlca)
end event

type dw_search_results from u_dw_online within tabpage_therapist
integer y = 32
integer width = 3182
integer height = 268
integer taborder = 11
string dataobject = "d_therapists_returned_from_search"
boolean hscrollbar = true
boolean livescroll = false
end type

event rowfocuschanged;call super::rowfocuschanged;/* 
Populates the providers based on the therapist	
*/
LONG					ll_therapist_no, ll_principal_id
INTEGER				li_rows

li_rows= dw_search_results.rowcount()

IF li_rows > 0 THEN 
	ll_therapist_no 		= 	dw_search_results.getitemnumber(this.getrow(),'therapist_no')
	ll_principal_id 		=  dw_search_results.getitemnumber(this.getrow(),'wif_principal_id')
	
	dw_provider_therapist.retrieve(ll_therapist_no)
	SQLCA.nf_handle_error("w_maintain_provider_therapist", "rowfocuschanged", "dw_provider_therapist.retrieve(ll_therapist_no)")
	
	dw_therapist_license.retrieve(ll_therapist_no)
	SQLCA.nf_handle_error("w_maintain_provider_therapist", "rowfocuschanged", "dw_therapist_license.retrieve(ll_therapist_no)")
	
	dw_therapist_program.retrieve(ll_therapist_no)
	SQLCA.nf_handle_error("w_maintain_provider_therapist", "rowfocuschanged", "dw_therapist_program.retrieve(ll_therapist_no)")
	
	dw_eligible_programs.retrieve(ll_therapist_no)
	SQLCA.nf_handle_error("w_maintain_provider_therapist", "rowfocuschanged", "dw_eligible_programs.retrieve(ll_therapist_no)")
	
END IF 

end event

event constructor;call super::constructor;//this.uf_setselect(1)
end event

type cb_therapist_search from commandbutton within tabpage_therapist
integer x = 2592
integer y = 304
integer width = 585
integer height = 84
integer taborder = 20
integer textsize = -12
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = roman!
string facename = "Times New Roman"
string text = "Therapist Search"
end type

event clicked;STRING			ls_provider_type,  ls_name, ls_search_type
LONG				ll_therapists_no
INTEGER			li_row

s_window_message    lstr_message


/*	get the type to search for */


Open(w_therapist_search)

lstr_message = Message.PowerObjectParm

dw_search_results.settransobject(sqlca)

ll_therapists_no = lstr_message.al_doubleparm[1]

IF ll_therapists_no > 0  THEN 
	
	/* 3.20		A therapist must be an active portal user (have a user account) to be approved for a physiotherapy program.    */
	// therapist returned based on the therapist_no and wif_principal_id 
	
	//do the array retrieve
	dw_search_results.retrieve(ll_therapists_no)
	SQLCA.nf_handle_error("w_maintain_provider_therapist", "clicked", "dw_search_results.retrieve()")
	
	wf_reset_dws_for_therapist()
				
END IF

IF dw_search_results.rowcount() > 0 THEN 
	dw_search_results.scrolltorow(1)
	dw_search_results.triggerevent('rowfocuschanged')
END IF 

end event

type cb_save_therapist from commandbutton within tabpage_therapist
integer x = 2615
integer y = 2176
integer width = 265
integer height = 92
integer taborder = 21
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Save"
end type

event clicked;INTEGER		li_counter, li_license_counter, li_license_count, li_inner_counter, li_found_count, li_search_result, li_delisted
LONG			ll_therapist_no,  ll_find, ll_selected_row, ll_license_therapist_no, ll_next_therapist_no, ll_wif_principal_id, ll_therapist_no_license
STRING 		ls_last_name,  ls_active_flag,   ls_license_no, ls_license_type_license, ls_license_prov_state_code_license, ls_license_no_license, ls_license_type_desc_e
STRING 		ls_rehab_program_code, ls_program_active_flag, ls_delisted_comment_license,    ls_country_code,  ls_email_address, ls_first_name, ls_full_name
STRING		ls_delisted_comment_license_org, ls_therapist_comments, ls_therapist_comments_orig
DATE			 ldt_delisted_date_orig,  ldt_listed_date_license, ldt_delisted_date_license, ldt_delisted_date_license_org
BOOLEAN	lb_delisted

DATAWINDOWCHILD	ldwc_child
dwitemstatus		ldwis_rowstatus

/*
unconfirmed constraints

[ck_THERAPIST_04]
([listed_date]<=[create_date] AND [listed_date]>='1990-01-01')

[ck_THERAPIST_05]
([delisted_date] IS NULL OR [delisted_date]<=dateadd(month,(60),getdate()))

  SELECT therapist_no,
 			license_type_code, 
			license_no,      
			license_prov_state_code,
			listed_date,            
			 delisted_date,           
			delisted_comment   
    FROM therapist_license  
   WHERE therapist_no = :al_therapist_no    

*/

dw_therapist_license.accepttext()
dw_therapist_program.accepttext()
dw_search_results.accepttext()

IF dw_search_results.rowcount() < 1 OR dw_therapist_license.rowcount() < 1 THEN RETURN

//dw_search_results
li_search_result 					= dw_search_results.getrow()
ll_therapist_no 						= dw_search_results.getitemnumber(li_search_result, 'therapist_no')
ll_wif_principal_id 					= dw_search_results.getitemnumber(li_search_result, 'wif_principal_id')
ls_email_address 					= dw_search_results.getitemstring(li_search_result, 'Wif_principal_name')
ls_first_name 						= dw_search_results.getitemstring(li_search_result, 'first_name')
ls_last_name 						= dw_search_results.getitemstring(li_search_result, 'last_name')
ls_full_name                  		= dw_search_results.getitemstring(li_search_result,'compute_1')
ls_therapist_comments			= dw_search_results.getitemstring(li_search_result,'comments')
ls_therapist_comments_orig		= dw_search_results.getitemstring(li_search_result,'comments', primary!,true)


///* 4.40		A rejected therapist must not be assigned a physiotherapy program. */
//IF inv_provider_therapist.nf_has_therapist_been_rejected(ll_record_no, ll_wif_principal_id, ls_license_no) > 0 THEN 
//	 MessageBox("Therapist Rejected", "A rejected therapist must not be assigned a physiotherapy program. ", Information! )
//	RETURN 1	
//END IF 

li_license_count = dw_therapist_license.rowcount()
IF isnull(li_license_count)  THEN li_license_count = 0
		
// • A therapist must have at least one license 
IF li_license_count = 0 THEN 
	MessageBox('Error','A therapist must have at least one license.',Exclamation!)
	RETURN -1	
END IF 

// check the therapist comments for len
IF len(trim(ls_therapist_comments)) < 3 AND len(trim(ls_therapist_comments)) > 0 THEN
	MessageBox('Error','The Comment entered for the Therapist must be greater then 2 characters',Exclamation!)
	RETURN -1	
END IF 

//if added or modified trim the comment
IF ls_therapist_comments <> ls_therapist_comments_orig THEN 
	 dw_search_results.setitem(1,'comments',Trim(dw_search_results.GetItemString(1,'comments')))	
END IF 

li_delisted = 0
/* If there is a delisted license and there is now a license that is not delisted then all licenses must be delisted or listed cant have some of each */
FOR li_counter = 1 TO dw_therapist_license.rowcount()
		IF  date(dw_therapist_license.getitemdatetime(li_counter, 'delisted_date')) > date('1900-01-01') THEN 
			li_delisted ++
		END IF 	
NEXT

IF li_delisted > 0 AND (li_delisted <> dw_therapist_license.rowcount())  THEN 
	MessageBox('Delisted Licenses ','All licenses for this Therapist must be delisted if any licenses are delisted',Exclamation!)
	RETURN -1	
END IF 
	
FOR li_license_counter = 1 TO dw_therapist_license.rowcount()
	
		ldwis_rowstatus = dw_therapist_license.GetItemStatus(li_license_counter,0,Primary!)
		IF ldwis_rowstatus <> DATAMODIFIED! THEN CONTINUE

		//dw_therapist_license
//		li_license_row 								= dw_therapist_license.getrow()
		ll_therapist_no_license 					= dw_therapist_license.getitemnumber(li_license_counter, 'therapist_no')
		ls_license_type_license 					= dw_therapist_license.getitemstring(li_license_counter, 'license_type_code')
		ls_license_no_license 					= dw_therapist_license.getitemstring(li_license_counter, 'license_no')
		ls_license_prov_state_code_license	= dw_therapist_license.getitemstring(li_license_counter, 'license_prov_state_code')
		ldt_listed_date_license 					= date(dw_therapist_license.getitemdatetime(li_license_counter, 'listed_date'))
		ldt_delisted_date_license 				= date(dw_therapist_license.getitemdatetime(li_license_counter, 'delisted_date'))
		ls_delisted_comment_license 			= dw_therapist_license.getitemstring(li_license_counter, 'delisted_comment')
		ldt_delisted_date_license_org  			= date(dw_therapist_license.getitemdatetime(li_license_counter, 'delisted_date',primary!,true))
		ls_license_type_desc_e 					= dw_therapist_license.getitemstring(li_license_counter, 'license_type_desc_e')   
		
		ls_delisted_comment_license_org		= dw_therapist_license.getitemstring(li_license_counter, 'delisted_comment', primary!, true)
		
		IF isnull(ldt_delisted_date_license_org) THEN ldt_delisted_date_license_org = DATE('1900-01-01')
		
		
		// basic checks for constraints
		//IF LEFT(ls_first_name,1) = ' ' OR RIGHT(ls_first_name,1) = ' ' THEN dw_therapist_license.setitem(1,'first_name',Trim(dw_therapist_license.GetItemString(1,'first_name')))	
		//IF LEFT(ls_last_name,1) 	= ' ' OR RIGHT(ls_last_name,1)  = ' ' THEN dw_therapist_license.setitem(1,'last_name',Trim(dw_therapist_license.GetItemString(1,'last_name')))	
		IF ISNULL(ls_delisted_comment_license_org) THEN ls_delisted_comment_license_org =''
		IF trim(ls_delisted_comment_license) > '' AND  ls_delisted_comment_license <> ls_delisted_comment_license_org THEN dw_therapist_license.setitem(li_license_counter,'delisted_comment',Trim(dw_therapist_license.GetItemString(li_license_counter,'delisted_comment')))	
		
		
		//•	A therapist must have a last name and a given name
		IF TRIM(ls_last_name) = '' OR TRIM(ls_first_name) = '' THEN 
			MessageBox('Error','A therapist must have a last name and a given name.',Exclamation!)
			RETURN -1
		END IF 
		
		// •The Listed date must not be null (defaults to the current date when created)
		IF ISNULL(ldt_listed_date_license) THEN 
			MessageBox('Error','The Listed date must not be null.',Exclamation!)
			RETURN -1
		END IF 
		
		IF ldt_listed_date_license < date('1990-01-01') THEN 
			MessageBox('Error','The Listed date must be greater then 1990-01-01.',Exclamation!)
			RETURN -1
		END IF 
		
		//•	The Therapist must have an Inactive status if the therapist is delisted  (however, the therapist may have either an active status or an inactive status if the therapist is not delisted)
		IF NOT ISNULL(ldt_delisted_date_license)	AND ls_active_flag = 'Y' THEN 
			MessageBox('Error','The Therapist must have an Inactive status if the therapist is delisted.',Exclamation!)
			RETURN -1	
		END IF 
		
		//•	If the therapist is delisted, the delisted date that is entered must be greater than the listed date
		IF NOT ISNULL(ldt_delisted_date_license)	AND ldt_delisted_date_license <  ldt_listed_date_license THEN 
			MessageBox('Error','The delisted date that must be greater than the listed date.',Exclamation!)
			RETURN -1	
		END IF 
		
		/* do a couple of checks */
		IF  ldt_listed_date_license > Date('2079-06-06') OR ldt_listed_date_license < Date('1900-01-01') THEN 
			messagebox("Validation Error", 'The Listed Date if entered, must be a valid date between 1900-01-01 and 2079-06-06')
			RETURN -1
		END IF 
		
		IF  ldt_delisted_date_license > Date('2079-06-06') OR ldt_delisted_date_license < Date('1900-01-01') THEN 
			messagebox("Validation Error", 'The Delisted Date if entered, must be a valid date between 1900-01-01 and 2079-06-06')
			RETURN -1
		END IF 
		
		// if there isnt a delisted date there probably shouldn't be a comment
		IF trim(ls_delisted_comment_license) > '' AND ( isnull(ldt_delisted_date_license) OR ldt_delisted_date_license = Date('1900-01-01')) THEN
			messagebox("Validation Error", 'The delisted comment is only applicable if the delisted date is supplied.')
			RETURN -1
		END IF 
		
		// makesure delitedcomment is three chars
		IF trim(ls_delisted_comment_license) > '' AND len(ls_delisted_comment_license) < 3 THEN
			messagebox("Validation Error", 'The delisted comment must be greater then 3 characters.')
			RETURN -1
		END IF
		
		//based on [ck_THERAPIST_05]
		IF (ldt_delisted_date_license <> ldt_delisted_date_license_org) AND NOT ISNULL(ldt_delisted_date_license) AND ldt_delisted_date_license > date('1900-01-01')  THEN

			IF ldt_delisted_date_license < RelativeDate( Date( f_server_datetime()), -180 ) THEN
				MessageBox( "Date Warning", "The delisted date is more than 6 months in the past.", Information! )
				RETURN -1
			END IF
		END IF 
		
		// therapist has becom delisted send an email
		IF  ISNULL(ldt_delisted_date_license_org) OR ldt_delisted_date_license_org =  date('1900-01-01') AND ldt_delisted_date_license_org <> ldt_delisted_date_license  THEN 
			lb_delisted = TRUE //therapist has been delisted - send an email	
		END IF 
		
		// basic checks for constraints
		IF LEFT(ls_license_no_license,1) 		= ' ' OR RIGHT(ls_license_no_license,1) = ' ' THEN dw_therapist_license.setitem(1,'license_no',Trim(dw_therapist_license.GetItemString(1,'license_no')))	
		ls_license_no = trim(ls_license_no)
		
					
		IF LEN(ls_license_no_license) < 3  THEN 
			MessageBox('Error','The license must have a minimum of 3 characters',Exclamation!)
			RETURN -1	
		END IF 
		
		// •	The license type must be entered and must be a license type from the list of valid license types
		IF ISNULL(ls_license_type_license) OR TRIM(ls_license_type_license) = '' THEN 
			MessageBox('Error','The license type must be entered and must be a license type from the list of valid license types.',Exclamation!)
			RETURN -1	
		END IF 
		
		//•	The license number must be entered (and pass the standard character data type validations)
		IF ISNULL(ls_license_no_license) OR TRIM(ls_license_no_license) = '' THEN 
			MessageBox('Error','	The license number must be entered.',Exclamation!)
			RETURN -1	
		END IF 
	
		//•	The licensing  province must be entered and must be a Canadian province
		IF ISNULL(ls_license_prov_state_code_license) OR TRIM(ls_license_prov_state_code_license) = '' THEN 
			MessageBox('Error','The licensing  province must be entered and must be a Canadian province',Exclamation!)
			RETURN -1	
		END IF 
	
		dw_therapist_license.GetChild('license_prov_state_code', ldwc_child)
		
		ll_selected_row		=  ldwc_child.Find('location_code = "' + ls_license_prov_state_code_license +  '"',0, ldwc_child.RowCount())  	
		ls_country_code 	= Trim(ldwc_child.GetItemString(ll_selected_row,"country_code"))
	
		// The licensing  province must be entered and must be a Canadian province
		IF UPPER(ls_country_code) <> 'CAN' THEN 
			MessageBox('Error','The licensing  province must be entered and must be a Canadian province',Exclamation!)
			RETURN -1	
		END IF 
		
		/* •	The license number must be unique for the license type and the licensing  province 
//				incorrectbased on this idx.... Cannot insert duplicate key row in object 'dbo.THERAPIST_LICENSE' with unique index 'idx2'. The duplicate key value is (999).
//				No changes made to database. -- this will need to be changed or the index will need to be changed
//		*/
//		IF inv_provider_therapist.nf_is_license_unique(ls_license_no_license, ls_license_type_license, ls_license_prov_state_code_license) = FALSE THEN
//			MessageBox('Error','The license number must be unique for the license type and the licensing  province',Exclamation!)
//			RETURN -1	
//		END IF 
//	
//		// make sure the combo is unique in the dw
//		li_found_count = 0
//		FOR li_inner_counter = 1 TO li_license_count	
//			ll_find = dw_therapist_license.Find( 'license_no  = "' + ls_license_no_license + '" and license_type_code = "' + ls_license_type_license + '" and license_prov_state_code = "'  + ls_license_prov_state_code_license + '"'	, li_inner_counter, dw_therapist_license.RowCount())
//			IF  ll_find > 0 THEN li_found_count ++
//		NEXT
//		
//		IF li_found_count > 1 THEN 
//			MessageBox('Error','The license number must be unique for the license type and the licensing  province',Exclamation!)
//			RETURN -1	
//		END IF 
		
		/* if everything goes allright send off the emails to applicable providers to 
//			say if the therapist has been delisted - obviously don't send if they haven't been de-listed
//			
//			remove for now until translation is complete
//		*/
//		IF lb_delisted = TRUE THEN 
//			inv_provider_therapist.nf_create_email(ll_wif_principal_id, ls_license_no_license, ls_license_type_desc_e, ls_license_prov_state_code_license, ls_full_name)
//			lb_delisted = FALSE
//		END IF 
					
NEXT

/*********************** UPDATE SECTION ***********************/
///* 3.3.3	Database Updates
//This will be used to populate the following database tables:
////•	THERAPIST
////•	THERAPIST_LICENSE
////•	REHAB_PROGRAM_XREF_THERAPIST
//*/
	
//new programs
//FOR li_counter = 1 to dw_therapist_program.rowcount()
//		IF dw_therapist_program.getitemstatus(li_counter,0,primary!) = newmodified! THEN 
//			ls_rehab_program_code 	= dw_therapist_program.getitemstring(li_counter, 'rehab_program_code')
//			ls_program_active_flag 	= dw_therapist_program.getitemstring(li_counter, 'active_flag')	
//			dw_therapist_program.setitem(li_counter, 'therapist_no',ll_therapist_no_license)	
//		END IF 	
//NEXT
	

// start the transaction stuff....
SQLCA.nf_begin_transaction()

dw_therapist_license.update()
SQLCA.nf_handle_error("w_maintain_provider_therapist", "cb_save - clicked", "dw_therapist_license.update()")

dw_therapist_program.update()
SQLCA.nf_handle_error("w_maintain_provider_therapist", "cb_save - clicked", "dw_therapist_program.update()")

dw_search_results.update()  
SQLCA.nf_handle_error("w_maintain_provider_therapist", "cb_save - clicked", "dw_search_results.update()")

SQLCA.nf_commit_transaction()

messagebox('Success','Save Completed')
	
end event

type dw_therapist_license from u_dw_online within tabpage_therapist
integer y = 436
integer width = 3182
integer height = 592
integer taborder = 11
string dataobject = "d_therapist_license_update"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = false
end type

event constructor;call super::constructor;settransobject(sqlca)
end event

event buttonclicked;call super::buttonclicked;//CHOOSE CASE dwo.name
//	CASE 'b_delete'
//		IF THIS.modifiedcount() > 0 OR THIS.DeletedCount() > 0 THEN 
//			PARENT.cb_save_therapist.triggerevent('clicked')	
//		END IF 
//		
//	CASE ELSE	
//END CHOOSE
end event

