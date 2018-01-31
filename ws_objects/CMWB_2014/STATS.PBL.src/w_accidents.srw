$PBExportHeader$w_accidents.srw
$PBExportComments$Accident Statistics screen. Inherited from w_a_tool.
forward
global type w_accidents from w_a_tool
end type
type dw_stats from u_dw_online within w_accidents
end type
type dw_claim from u_dw_online within w_accidents
end type
type cb_save from commandbutton within w_accidents
end type
type cb_next from commandbutton within w_accidents
end type
type cb_cancel from commandbutton within w_accidents
end type
type cb_previous from commandbutton within w_accidents
end type
type uo_image_append from u_image_append within w_accidents
end type
end forward

global type w_accidents from w_a_tool
integer width = 3182
integer height = 1992
boolean resizable = false
dw_stats dw_stats
dw_claim dw_claim
cb_save cb_save
cb_next cb_next
cb_cancel cb_cancel
cb_previous cb_previous
uo_image_append uo_image_append
end type
global w_accidents w_accidents

type variables
BOOLEAN     ib_needtosave
W_SHEET    iw_wsheet
LONG           il_claimno
INTEGER      il_rowsindw
INTEGER      il_currentrow

end variables

forward prototypes
public function boolean wf_check_ni_ee (string as_ni_code, string as_ee_code)
public function integer wf_invalid_code_message (string as_field)
public function integer wf_invalid_combination_message (string field_1, string field_2)
public function boolean wf_check_ni_pb (string as_ni_code, string as_pb_code)
public function boolean wf_check_si_ee (string as_si_code, string as_ee_code)
public function boolean wf_check_ssi_ee (string as_ssi_code, string as_ee_code)
public subroutine wf_display_docs ()
public function long wf_get_claim_no_to_work_on ()
public subroutine wf_initialize_window ()
public subroutine wf_setup_claim ()
public subroutine wf_move_record_prompt (string as_direction)
public function integer wf_save_record_prompt (string as_direction)
public function integer wf_validate (integer ai_column)
public subroutine wf_reset_to_original ()
public subroutine wf_move_to_record (integer ai_movetorecord)
public subroutine wf_retrieve_accident (long al_claim_no)
public function integer wf_save (string as_direction)
public function integer wf_filter_side_of_body (string as_part_of_body_code, string as_mode)
public function boolean wf_check_pb_sb (string as_pb_code, string as_sb_code)
end prototypes

public function boolean wf_check_ni_ee (string as_ni_code, string as_ee_code);/*	Arguments:		as_ni_code		(nature_of_injury_code)
						as_ee_code		(event_exposure_code)
	Return			Boolean			(indicates whether the two codes can be combined in an ACCIDENT record)
*/

STRING	ls_flag
INTEGER	li_error_status

SELECT	valid_combination_flag
INTO		:ls_flag
FROM		Nature_Event_Combination
WHERE	((Nature_Event_Combination.nature_of_injury_code = :as_ni_code)
AND		(Nature_Event_Combination.event_exposure_code = :as_ee_code))
USING SQLCA;

li_error_status = SQLCA.nf_handle_error("w_accidents","","wf_check_ni_ee: 'SELECT valid_combination_flag'")

IF Not ls_flag = "Y" THEN
	wf_invalid_combination_message("Nature of Injury","Event or Exposure")
	RETURN FALSE
ELSE
	RETURN TRUE
END IF

end function

public function integer wf_invalid_code_message (string as_field);RETURN MessageBox("Accident Validation","The " + as_field + " code is invalid",exclamation!)

end function

public function integer wf_invalid_combination_message (string field_1, string field_2);RETURN MessageBox("Accident Validation","The specified " + field_1 + " and " + field_2 + " combination is invalid.",exclamation!)

end function

public function boolean wf_check_ni_pb (string as_ni_code, string as_pb_code);/*	Arguments:		as_ni_code		(nature_of_injury_code)
						as_pb_code		(part_of_body_code)
	Return			Boolean			(indicates whether the two codes can be combined in an ACCIDENT record)
*/

STRING	ls_flag
INTEGER	li_error_status

SELECT	valid_combination_flag
INTO		:ls_flag
FROM		Nature_PartOfBody_Combination
WHERE	((Nature_PartOfBody_Combination.nature_of_injury_code = :as_ni_code)
AND		(Nature_PartOfBody_Combination.part_of_body_code = :as_pb_code))
USING SQLCA ;

li_error_status = SQLCA.nf_handle_error("w_accidents","","wf_check_ni_pb: 'SELECT valid_combination_flag'")

IF Not ls_flag = "Y" THEN
	wf_invalid_combination_message("Nature of Injury","Part of Body")
	RETURN FALSE
ELSE
	RETURN TRUE
END IF

end function

public function boolean wf_check_si_ee (string as_si_code, string as_ee_code);/*	Arguments:		as_si_code		(source_of_injury_code)
						as_ee_code		(event_exposure_code)
	Return			Boolean			(indicates whether the two codes can be combined in an ACCIDENT record)
*/

STRING	ls_flag
INTEGER	li_error_status

SELECT	valid_combination_flag
INTO		:ls_flag
FROM		Source_Event_Combination
WHERE	((Source_Event_Combination.source_of_injury_code = :as_si_code)
AND		(Source_Event_Combination.event_exposure_code = :as_ee_code))
USING SQLCA ;

li_error_status = SQLCA.nf_handle_error("w_accidents","","wf_check_si_ee: 'SELECT valid_combination_flag'")

IF Not ls_flag = "Y" THEN
	wf_invalid_combination_message("Event or Exposure","Source of Injury")
	RETURN FALSE
ELSE
	RETURN TRUE
END IF

end function

public function boolean wf_check_ssi_ee (string as_ssi_code, string as_ee_code);/*	Arguments:		as_ssi_code	(second_source_of_injury_code)
						as_ee_code		(event_exposure_code)
	Return			Boolean			(indicates whether the two codes can be combined in an ACCIDENT record)
*/

STRING	ls_flag
INTEGER	li_error_status

SELECT	valid_combination_flag
INTO		:ls_flag
FROM		Source_Event_Combination
WHERE	((Source_Event_Combination.source_of_injury_code = :as_ssi_code)
AND		(Source_Event_Combination.event_exposure_code = :as_ee_code))
USING SQLCA;

li_error_status = SQLCA.nf_handle_error("w_accidents","","wf_check_ssi_ee: 'SELECT valid_combination_flag'")

IF Not ls_flag = "Y" THEN
	wf_invalid_combination_message("Event or Exposure","Second Source of Injury")
	RETURN FALSE
ELSE
	RETURN TRUE
END IF

end function

public subroutine wf_display_docs ();LONG		ll_doc_id, ll_result
INTEGER	li_doc_count, li_doc_list_row, li_error_status
STRING	ls_doc_type
integer  li_rtn

/* close all imaged documents currently being displayed
*/

do
	ll_result = f_close_viewer()
loop while ll_result <> 0


/* Call on ImaraViewer to display all claim related documents of set types.
*/
	li_doc_count = iw_wsheet.dw_documents.RowCount()
	li_doc_list_row = 1
	DO WHILE li_doc_list_row <= li_doc_count
		ls_doc_type = Left(iw_wsheet.dw_documents.GetItemString(li_doc_list_row,"docindex_type"),2)
		IF ls_doc_type = "SD" OR ls_doc_type = "SE" THEN

/*	Get the document id for selected row
	View the document
*/
			ll_doc_id = iw_wsheet.dw_documents.GetItemNumber(li_doc_list_row,"ref_docid")
			
			
		
				
			if uo_image_append.of_init(ll_doc_id)	<= 0 then
				RETURN
			end if
				
				
			ls_doc_type =  uo_image_append.of_get_file_type()
				
			
			CHOOSE CASE ls_doc_type
				/*  Imaged document */ 
				CASE 'IMA', 'TIF'
					li_rtn = uo_image_append.of_append_image(ll_doc_id)
					if li_rtn < 0 then
						RETURN
					end if
				case else
					iw_wsheet.iu_dw_document_path.f_manage_document(ll_doc_id,"V","NORMAL")
			end choose
					

	

		END IF
		li_doc_list_row ++
	LOOP

end subroutine

public function long wf_get_claim_no_to_work_on ();/*	This function is used to grab the claim_no off the datawindow dw_basic_claim
	located on the worksheet (iw_wsheet) and returns it to the instance variable
	il_claimno. It contains no arguments and returns 1 value.
*/

LONG ll_claimno

/*	Grab the claim_no from the dw_basic_claim & determine the currently selected row in the list.
*/
	ll_claimno = iw_wsheet.dw_basic_claim.GetitemNumber(1,'claim_no')
	il_currentrow = iw_wsheet.uo_claim_search.dw_search_list.GetSelectedRow(0)

/* Set title to indicate current record.
*/
	IF il_rowsindw > 0 THEN
	   st_title.Text = 'Maintain Accident Statistics - currently on record ' + Trim(String(il_currentrow)) + ' of ' + Trim(String(il_rowsindw))
	END IF

	RETURN ll_claimno

end function

public subroutine wf_initialize_window ();/* This function initializes the Accident Statistics screen by obtaining a reference to the 
	parent window and by initializing the datawindows. It also determines how many records are
	in the search list on the parent windows user-object.
	It contains no arguments and returns no values.
*/

/*	Obtain a reference to the current instance of w_sheet.
*/
	iw_wsheet = ParentWindow()

/*	Initialize the datawindows.
*/
	dw_stats.SetTransObject(SQLCA)
	dw_claim.SetTransObject(SQLCA)

/* Determine number of records in list.
*/
	il_rowsindw = iw_wsheet.uo_claim_search.dw_search_list.RowCount()
 
end subroutine

public subroutine wf_setup_claim ();/*	This function will retrieve the CLAIM & ACCIDENT records for the value of il_claimno.
	It will also open up all associated imaged documents required.
	It contains no arguments & returns no values.
*/

/*	View the claim and open all appropriate documents.
*/
	wf_retrieve_accident(il_claimno)

/*	Disabled the call to the function wf_display_docs() as the users decided
	that they wish to open them up themselves. If it needs to be automatic
	later, then uncomment the line directly below.

	wf_display_docs()	*** THIS LINE.
*/
	dw_stats.SetColumn('nature_of_injury_code')
	dw_stats.SetFocus()

end subroutine

public subroutine wf_move_record_prompt (string as_direction);INTEGER li_nextrecord, li_promptvalue, li_err_col
BOOLEAN lb_proceedtomove

IF ib_needtosave THEN
	li_promptvalue = wf_save_record_prompt(as_direction)
	CHOOSE CASE li_promptvalue
		CASE 1
			li_err_col = wf_save(as_direction)

/*	Always set lb_proceedtomove to FALSE because if record saved, then the save handles the record movement. And
	if an error occurs, then don't move anyway. */
			lb_proceedtomove = FALSE
		CASE 2

/*	Aborted save of record, however still want to move.
*/
			lb_proceedtomove = TRUE
		CASE ELSE

/* Aborted record movement.
*/
			lb_proceedtomove = FALSE
		END CHOOSE
ELSE

/* Just a move.
*/
	lb_proceedtomove = TRUE
END IF

IF lb_proceedtomove THEN
	IF as_direction = 'next' THEN
		li_nextrecord = il_currentrow + 1
	ELSE
		li_nextrecord = il_currentrow - 1
	END IF
	wf_move_to_record(li_nextrecord)
END IF

end subroutine

public function integer wf_save_record_prompt (string as_direction);INTEGER li_mess_ret

li_mess_ret = MessageBox("Accident Statistics Maintenance","Current record changed, save changes before moving to the " + as_direction + " record?",Question!,YesNoCancel!,3)

RETURN li_mess_ret
end function

public function integer wf_validate (integer ai_column);/*	This function validates one or all columns in dw_stats 
	Argument: 		ai_column,			Indicates which column to validate  (0 for all columns)
	Return Value:	Negative:			Invalid code (dw_stats.itemfocuschanged only sets focus on negative values)
						Zero:					Valid code and combination(s) 
	Variables:		lb_check_ni_ee:	Enables checking of related codes for NATURE OF INJURY and EVENT OR EXPOSURE		
						lb_check_si_ee:	Enables checking of related codes for SOURCE OF INJURY and EVENT OR EXPOSURE		
						lb_check_ssi_ee:	Enables checking of related codes for SECONDARY SOURCE OF INJURY and EVENT OR EXPOSURE		
						lb_check_ni_pb:	Enables checking of related codes for NATURE OF INJURY and PART OF BODY		
						ls_current_value:		Used with GetItemString (contains injury codes)
						ls_code_message:	Contains error message text
						ls_rel_code_1:	Contains injury code argument for 1 of the 4 code relationship functions
						ls_rel_code_2:	Contains injury code argument for 1 of the 4 code relationship functions
						li_column:			Contains the current column number of dw_stats being validated
*/

STRING          ls_current_value = "", ls_code_message = ""
STRING          ls_rel_code_1, ls_rel_code_2, ls_isactive, ls_original_value, ls_findfield
INTEGER         li_column, li_rowsindw
BOOLEAN         lb_check_ni_ee, lb_check_si_ee, lb_check_ssi_ee, lb_check_ni_pb, lb_check_pb_sb
LONG            ll_entryfoundat
DATAWINDOWCHILD ldwc_child
DWITEMSTATUS    ldwis_columnstatus, ldwis_rowstatus

	IF ai_column > 0 THEN
		li_column = ai_column
	ELSE
		li_column = 1
	END IF

/*	Set related codes checking variables.
*/
	lb_check_ni_ee  = FALSE
	lb_check_si_ee  = FALSE
	lb_check_ssi_ee = FALSE
	lb_check_ni_pb  = FALSE

	DO WHILE li_column <= 7

/*	Set value for MessageBox message if missing data.
*/
		CHOOSE CASE li_column
			CASE 1
				ls_code_message = "Event or Exposure"
				ls_findfield = "event_exposure_code = '"
			CASE 2
				ls_code_message = "Nature Of Injury"
				ls_findfield = "nature_of_injury_code = '"
			CASE 3
				ls_code_message = "Source Of Injury"
				ls_findfield = "source_of_injury_code = '"
			CASE 4
				ls_code_message = "Second Source Of Injury"
				ls_findfield = "source_of_injury_code = '"
			CASE 5
				ls_code_message = "Occupation"
				ls_findfield = "occupation_code = '"
			CASE 6
				ls_code_message = "Part of Body"
				ls_findfield = "part_of_body_code = '"
			CASE 7
				ls_code_message = "Side of Body"
				ls_findfield = "side_of_body_code = '"
		END CHOOSE

/*	Determine if the value for the column has changed and if so, perform validation.
*/
		dw_stats.AcceptText()
/*	ldwis_columnstatus = dw_stats.GetItemStatus(1,li_column,Primary!) */
/*	IF ldwis_columnstatus <> NotModified! THEN     // Commented out to allow check for non-active values. */

/*	Get the current & original value for the columns.
*/
		ls_current_value  = dw_stats.GetItemString(1,li_column)
		ls_original_value = dw_stats.GetItemString(1,li_column,Primary!,TRUE)

/*	Check for a value in the field.
*/
		IF IsNull(ls_current_value) or ls_current_value = "" THEN
			MessageBox('Required Data Missing','A value for ' + ls_code_message + ' is required before you can continue.')
			RETURN (-1 * li_column)
		END IF

/*	Based on the column, perform the appropriate code which determines the child datawindow and
	sets any of the check variables.
*/
		CHOOSE CASE li_column
			CASE 1     // event_exposure_code.
				lb_check_ni_ee  = TRUE
				lb_check_si_ee  = TRUE
				lb_check_ssi_ee = TRUE
				dw_stats.GetChild('event_exposure_code',ldwc_child)

			CASE 2     // nature_of_injury_code.
				lb_check_ni_pb = TRUE
				lb_check_ni_ee = TRUE
				dw_stats.GetChild('nature_of_injury_code',ldwc_child)

			CASE 3     // source_of_injury_code.
				lb_check_si_ee = TRUE
				dw_stats.GetChild('source_of_injury_code',ldwc_child)

			CASE 4     // second_source_of_injury_code.
				lb_check_ssi_ee = TRUE
				dw_stats.GetChild('second_source_of_injury_code',ldwc_child)

			CASE 5     // occupation_code.
				dw_stats.GetChild('occupation_code',ldwc_child)

			CASE 6     // part_of_body.
				lb_check_ni_pb = TRUE
				dw_stats.GetChild('part_of_body_code',ldwc_child)
			
			CASE 7     // side_of_body.
				lb_check_pb_sb = TRUE
				dw_stats.GetChild('side_of_body_code',ldwc_child)
		END CHOOSE

/*	Determine if a value typed in is actually a valid entry.
*/
		li_rowsindw = ldwc_child.RowCount()
		ll_entryfoundat = ldwc_child.Find(ls_findfield + ls_current_value + "'",1,li_rowsindw)
		IF ll_entryfoundat = 0 THEN
			MessageBox('Invalid Data','The value selected/entered for ' + ls_code_message + ' is invalid.')
			RETURN (-1 * li_column)
		END IF

/*	Get out of the loop if only checking one column, otherwise check next one.
*/
		IF ai_column > 0 THEN
			EXIT
		ELSE
			li_column = li_column + 1
		END IF
	LOOP

/*	Validate related code combinations if the row changed.
*/
	ldwis_rowstatus = dw_stats.GetItemStatus(1,0,Primary!)
	IF ldwis_rowstatus <> NotModified! THEN

/*	Nature of Injury and Part of Body.
*/
		IF lb_check_ni_pb THEN
			ls_rel_code_1 = dw_stats.GetItemString(1,"nature_of_injury_code")
			ls_rel_code_2 = dw_stats.GetItemString(1,"part_of_body_code")
			
			//perform the filtering on the side of body dropdown list when the part of body changes
			if wf_filter_side_of_body(ls_rel_code_2, 'set') < 0 THEN RETURN -7
			
			IF not (IsNull(ls_rel_code_1) or IsNull(ls_rel_code_2) or ls_rel_code_1 = "" or ls_rel_code_2 = "") THEN
				IF not wf_check_ni_pb(ls_rel_code_1,ls_rel_code_2) THEN
					wf_filter_side_of_body('', 'reset')					
					RETURN (-1 * li_column)
				END IF
			END IF
		END IF
		
	/*	Part of Body to Side of Body.
*/
		IF lb_check_pb_sb THEN
			ls_rel_code_1 = dw_stats.GetItemString(1,"part_of_body_code")
			ls_rel_code_2 = dw_stats.GetItemString(1,"side_of_body_code")
			IF not (IsNull(ls_rel_code_1) or IsNull(ls_rel_code_2) or ls_rel_code_1 = "" or ls_rel_code_2 = "") THEN
				IF not wf_check_pb_sb(ls_rel_code_1,ls_rel_code_2) THEN
					RETURN (-1 * li_column)
				END IF
			END IF
		END IF

/*	Nature of Injury and Injury Event/Exposure combination.
*/
		IF lb_check_ni_ee THEN
			ls_rel_code_1 = dw_stats.GetItemString(1,"nature_of_injury_code")
			ls_rel_code_2 = dw_stats.GetItemString(1,"event_exposure_code")
			IF not (IsNull(ls_rel_code_1) or IsNull(ls_rel_code_2) or ls_rel_code_1 = "" or ls_rel_code_2 = "") THEN
				IF not wf_check_ni_ee(ls_rel_code_1,ls_rel_code_2) THEN
					RETURN (-1 * li_column)
				END IF
			END IF
		END IF
	
/*	Source of Injury and Injury Event/Exposure combination.
*/
		IF lb_check_si_ee THEN
			ls_rel_code_1 = dw_stats.GetItemString(1,"source_of_injury_code")
			ls_rel_code_2 = dw_stats.GetItemString(1,"event_exposure_code")
			IF not (IsNull(ls_rel_code_1) or IsNull(ls_rel_code_2) or ls_rel_code_1 = "" or ls_rel_code_2 = "") THEN
				IF not wf_check_si_ee(ls_rel_code_1,ls_rel_code_2) THEN
					RETURN (-1 * li_column)
				END IF
			END IF
		END IF

/*	Per StatsCan 1996/01/09 (Horst Stiebert)
	Priority of Source of Injury and Second Source of Injury is still being clarified.
	At this point, Secondary Source of Injury 'depends' on Injury Event or Exposure.
	Therefore, cannot validate combinations of codes for these two fields.
	Leave code in program; commented out for future use!
*/
////	Validate Second Source of Injury and Injury Event/Exposure combination
//		IF lb_check_ssi_ee THEN
//			ls_rel_code_1 = dw_stats.GetItemString(1,"second_source_of_injury_code")
//			ls_rel_code_2 = dw_stats.GetItemString(1,"event_exposure_code")
//			IF not (IsNull(ls_rel_code_1) or IsNull(ls_rel_code_2) or ls_rel_code_1 = "" or ls_rel_code_2 = "") THEN
//				IF not wf_check_ssi_ee(ls_rel_code_1,ls_rel_code_2) THEN
//					RETURN (-1 * li_column)
//				END IF
//			END IF
//		END IF
	END IF

	RETURN 0

end function

public subroutine wf_reset_to_original ();/*	Get original values of columns, then set them on record.
*/

	dw_stats.SetItem(1,'nature_of_injury_code',dw_stats.GetItemString(1,'nature_of_injury_code',Primary!,TRUE))
	dw_stats.SetItem(1,'part_of_body_code',dw_stats.GetItemString(1,'part_of_body_code',Primary!,TRUE))
	dw_stats.SetItem(1,'side_of_body_code',dw_stats.GetItemString(1,'side_of_body_code',Primary!,TRUE))
	dw_stats.SetItem(1,'source_of_injury_code',dw_stats.GetItemString(1,'source_of_injury_code',Primary!,TRUE))
	dw_stats.SetItem(1,'second_source_of_injury_code',dw_stats.GetItemString(1,'second_source_of_injury_code',Primary!,TRUE))
	dw_stats.SetItem(1,'occupation_code',dw_stats.GetItemString(1,'occupation_code',Primary!,TRUE))
	dw_stats.SetItem(1,'event_exposure_code',dw_stats.GetItemString(1,'event_exposure_code',Primary!,TRUE))
	dw_stats.AcceptText()
	
	wf_filter_side_of_body('', 'cancel')

/*	Set status of row to NotModified! to indicate that no changes have taken place.
*/
	dw_stats.SetItemStatus(1,0,Primary!,NotModified!)

/*	Reset the need to save flag.
*/
	ib_needtosave = FALSE

/*	Disable buttons.
*/
	cb_save.enabled = FALSE
	cb_cancel.enabled = FALSE

/*	Set the focus on dw_stats.
*/
	dw_stats.setfocus()
end subroutine

public subroutine wf_move_to_record (integer ai_movetorecord);INTEGER li_scrollresult

/*	This function is used to move to either the next or previous record in the list
	within the user-object on the worksheet (iw_wsheet.uo_claim_search.dw_search_list).
*/

	IF ai_movetorecord > il_rowsindw or ai_movetorecord = 0 or ai_movetorecord < 0 THEN
		IF il_rowsindw > 1 THEN
			IF ai_movetorecord > il_rowsindw THEN
				MessageBox("Bottom Of List","You have reached the bottom of the list.")
			ELSE
				MessageBox("Top Of List","You have reached the top of the list.")
			END IF
		ELSE
			// top & bottom of list are the same when there is only one record
		END IF
	ELSE
		li_scrollresult = iw_wsheet.uo_claim_search.dw_search_list.ScrollToRow(ai_movetorecord)
		IF li_scrollresult < 0 THEN
			MessageBox("Scroll Error","An error occured while trying to scroll to the next record.")
		END IF
	END IF

/*	After moving to the next record, trigger the doubleclicked event to populate the screen w_sheet.
*/
// No longer necessary.  Rowfocuschanged event refreshes data.  Rob Head 98/08/19.
//	iw_wsheet.uo_claim_search.dw_search_list.TriggerEvent(DoubleClicked!)

/*	Get the claim_no of the claim to work on.
*/
	il_claimno = wf_get_claim_no_to_work_on()

/*	Retrieve the information for the claim and open the imaged documents.
*/
	wf_setup_claim()

/*	Turn off save & cancel button.
*/
	cb_save.enabled = FALSE
	cb_cancel.enabled = FALSE

end subroutine

public subroutine wf_retrieve_accident (long al_claim_no);Integer li_error_status

//	Retrieve the record from ACCIDENT.
li_error_status = dw_stats.Retrieve(al_claim_no)
li_error_status = SQLCA.nf_handle_error("w_accidents","dw_stats","wf_retrieve_accident: 'dw_stats.Retrieve(al_claim_no)'")
IF li_error_status = 0 THEN
	//	Retrieve the record from CLAIM.
	li_error_status = dw_claim.Retrieve(al_claim_no)
	li_error_status = SQLCA.nf_handle_error("w_accidents","dw_claim","wf_retrieve_accident: 'dw_claim.Retrieve(al_claim_no)'")
	IF li_error_status = 0 THEN
		dw_stats.SetColumn("nature_of_injury_code")
		dw_stats.SetFocus() 
	ELSE
		//	Clear the screen and close window.
		dw_claim.Reset()
		dw_stats.Reset()
		ib_needtosave = FALSE
		cb_close.PostEvent(Clicked!)
	END IF
ELSE
	//	Clear the screen and close window.
	dw_claim.Reset()
	dw_stats.Reset()
	ib_needtosave = FALSE
	cb_close.PostEvent(Clicked!)
END IF

end subroutine

public function integer wf_save (string as_direction);// wf_save
//
Long    ll_currentclaim, ll_last_event_no
Integer li_ret, li_error_status, li_scrollresult, li_answer, li_msg_rtn
String  ls_noic, ls_pobc, ls_soic, ls_eec, ls_ssoic, ls_occupation, ls_orig_nature_of_injury_code_desc, ls_sobc
String  ls_nature_of_injury_code, ls_orig_nature_of_injury_code, ls_nature_of_injury_code_desc, ls_event_comment
Datetime ldt_server_datetime 
DWItemStatus ldwis_datawindowstatus

SetPointer(HourGlass!)

//	Validate all fields (argument 0) and continue if passed.
dw_stats.AcceptText()
li_ret = wf_validate(0)

IF li_ret = 0 THEN
	// Attempt save. If all codes set, set coding_complete_flag to TRUE.
	ls_noic         = Trim(dw_stats.GetItemString(1,'nature_of_injury_code'))
	ls_pobc         = Trim(dw_stats.GetItemString(1,'part_of_body_code'))
	ls_sobc         = TRIM(dw_stats.GetItemString(1,'side_of_body_code'))
	ls_soic         = Trim(dw_stats.GetItemString(1,'source_of_injury_code'))
	ls_eec          = Trim(dw_stats.GetItemString(1,'event_exposure_code'))
	ls_ssoic        = Trim(dw_stats.GetItemString(1,'second_source_of_injury_code'))
	ls_occupation   = Trim(dw_stats.GetItemString(1,'occupation_code'))
	ll_currentclaim = dw_stats.GetItemNumber(1,'claim_no')
	IF ls_noic <> '' and ls_pobc <> '' and ls_soic <> '' and ls_eec <> '' and ls_ssoic <> '' and ls_occupation <> ''  and ls_sobc <> '' and ls_sobc <> 'N' THEN
		
		IF ls_sobc = 'U' THEN
			IF dw_claim.GetItemString(1,'claim_coding_complete_flag') = 'N' THEN
				li_msg_rtn = MessageBox('Stats Fully Coded?','The side of body for this claim is "Unknown". Do you still want to update this claim to be "Fully Coded"?',Exclamation!,YesNo!,2)
				IF li_msg_rtn = 2 THEN
					RETURN -1
				END IF
			END IF
		END IF
		
		dw_claim.SetItem(1,"claim_coding_complete_flag","Y")
		dw_claim.SetItem(1,"part_of_body_code",ls_pobc)
		dw_claim.SetItem(1,'side_of_body_code', ls_sobc)

		//	Update CLAIM & ACCIDENT.
		//	But first need to check to see if adding (claim_no = NULL) or updating (claim_no <> NULL). IF adding, need to 
		//	set the itemstatus indicator on the datawindow so that it thinks it's an insert. This is because of the way
		//	the datawindow is written. (It always returns a claim record, therefore it always thinks it's a update.)
		If dw_stats.GetItemString(1,'nature_of_injury_code',Primary!,True) = ''  Then
			dw_stats.SetItem(1,'accident_original_nature_of_injury_code',ls_noic)
		End if

		// Create an event when Nature of Injury changes from original Coding of NOI
		ls_orig_nature_of_injury_code = dw_stats.GetItemString(1, 'nature_of_injury_code', Primary!, TRUE)
		ls_nature_of_injury_code = dw_stats.GetItemString(1, 'nature_of_injury_code', Primary!, FALSE)
		IF IsNull(ls_orig_nature_of_injury_code) = TRUE THEN ls_orig_nature_of_injury_code = ""
		IF IsNull(ls_nature_of_injury_code) = TRUE THEN ls_nature_of_injury_code = ""
		
			
		SQLCA.nf_begin_transaction()
		
		IF ls_nature_of_injury_code <> ls_orig_nature_of_injury_code AND ls_nature_of_injury_code <> "" AND ls_orig_nature_of_injury_code <> "" THEN
			SELECT isnull(MAX(event_no), 0) + 1
			  INTO :ll_last_event_no 
			  FROM CLAIM_EVENT 
			 WHERE claim_no = :ll_currentclaim ;

			li_error_status = SQLCA.nf_handle_error("w_accidents", "", "wf_save: SELECT MAX(event_no) + 1 FROM CLAIM_EVENT ")
			
			ldt_server_datetime = Datetime(Date(f_server_datetime()), Time("0:00:00"))

			SELECT ISNULL(nature_of_injury_code_desc, '') 
			  INTO :ls_orig_nature_of_injury_code_desc 
			  FROM Nature_Of_Injury 
			 WHERE nature_of_injury_code = :ls_orig_nature_of_injury_code ;

			li_error_status = SQLCA.nf_handle_error("w_accidents", "", "wf_save: SELECT nature_of_injury_code_desc FROM Nature_Of_Injury WHERE nature_of_injury_code = :ls_orig_nature_of_injury_code")

			SELECT ISNULL(nature_of_injury_code_desc, '') 
			  INTO :ls_nature_of_injury_code_desc 
			  FROM Nature_Of_Injury 
			 WHERE nature_of_injury_code = :ls_nature_of_injury_code ;

			li_error_status = SQLCA.nf_handle_error("w_accidents", "", "wf_save: SELECT nature_of_injury_code_desc FROM Nature_Of_Injury WHERE nature_of_injury_code = :ls_nature_of_injury_code")
			
			ls_event_comment = "Nature of Injury changed from original coding: " + ls_orig_nature_of_injury_code_desc + " to: " + ls_nature_of_injury_code_desc
  			ls_event_comment = f_clean_string_4(ls_event_comment)
			  
			INSERT CLAIM_EVENT (claim_no, event_no, event_date, event_type_code, 
									  event_comment,event_specific_code) 
			VALUES (:ll_currentclaim, :ll_last_event_no, :ldt_server_datetime, '027', :ls_event_comment, 'NOI') ;

			li_error_status = SQLCA.nf_handle_error("w_accidents", "", "wf_save: INSERT CLAIM_EVENT")
		END IF
				
		li_error_status = dw_claim.Update(TRUE)
	
		// After update of CLAIM record, check to see if it had been changed by another user. If so, need to retrieve data.
		IF SQLCA.SQLDBCode = 532 THEN			
			SQLCA.nf_rollback_transaction()
			
			ib_needtosave = FALSE
			cb_save.Enabled = FALSE
			cb_cancel.Enabled = FALSE
			SQLCA.SQLDBCode = 0
			SQLCA.SQLErrText = ""
			MessageBox("Record Update Error","The data for this claim has changed since being retrieved. Please close the screen and perform the search again.",Exclamation!)
			RETURN -1
		END IF

		li_error_status = SQLCA.nf_handle_error("w_accidents","dw_claim","wf_save: 'dw_claim.update()'")
		IF li_error_status = 0 THEN
			li_error_status = dw_stats.Update(TRUE)
			li_error_status = SQLCA.nf_handle_error("w_accidents","dw_stats","wf_save: 'dw_stats.update()'")
			IF li_error_status = 0 THEN

				SQLCA.nf_commit_transaction()

				// Reset the need to save flag.
				ib_needtosave = FALSE

				// After the record has been saved, move to the next/previous record in the user object on the w_sheet
				// based on the argument as_direction.
				IF as_direction = 'next' THEN
					cb_next.TriggerEvent(Clicked!)
				ELSE
					IF as_direction = 'previous' THEN
						cb_previous.TriggerEvent(Clicked!)
					END IF
				END IF
			END IF
		END IF
	ELSE
		IF ls_noic <> '' THEN 
			MessageBox('Required Data Missing','Before the record can be saved, nature_of_injury_code must be filled in.')
			dw_stats.SetColumn(3)
		ELSE
			IF ls_pobc <> '' THEN
				MessageBox('Required Data Missing','Before the record can be saved, part_of_body_code must be filled in.')
				dw_stats.SetColumn(1)
			ELSE
				IF ls_sobc <> '' THEN
					MessageBox('Required Data Missing','Before the record can be saved, side_of_body_code must be filled in.')
					dw_stats.SetColumn(7)
				ELSE
					IF ls_soic <> '' THEN
						MessageBox('Required Data Missing','Before the record can be saved, source_of_injury_code must be filled in.')
						dw_stats.SetColumn(4)
					ELSE
						IF ls_eec <> '' THEN
							MessageBox('Required Data Missing','Before the record can be saved, event_exposure_code must be filled in.')
							dw_stats.SetColumn(2)
						ELSE
							IF ls_ssoic <> '' THEN
								MessageBox('Required Data Missing','Before the record can be saved, second_source_of_body_code must be filled in.')
								dw_stats.SetColumn(5)
							ELSE
								MessageBox('Required Data Missing','Before the record can be saved, occupation_code must be filled in.')
								dw_stats.SetColumn(6)
							END IF
						END IF
					END IF
				END IF
			END IF
		END IF
		dw_stats.SetFocus()
	END IF
ELSE
	li_ret = Abs(li_ret)
	dw_stats.SetColumn(li_ret)
	dw_stats.SetFocus()
END IF

RETURN li_ret

end function

public function integer wf_filter_side_of_body (string as_part_of_body_code, string as_mode);
datawindowchild ldw_child
integer li_rtn
String  ls_part_of_body_code, ls_side_of_body_code
long ll_row, ll_rowcount

ll_row = dw_stats.getRow()

li_rtn = dw_stats.getChild('side_of_body_code', ldw_child)

IF li_rtn <> 1 THEN
	MessageBox('Error', 'Not a datawindow child')
	return -1
END IF

IF as_mode = 'set' THEN
	ls_part_of_body_code = as_part_of_body_code
ELSEIF as_mode = 'reset' THEN
	ls_part_of_body_code = dw_stats.GetItemString(ll_row,'part_of_body_code',Primary!,TRUE)	
ELSEIF as_mode = 'cancel' THEN
	ls_part_of_body_code = dw_stats.GetItemString(ll_row,'part_of_body_code',Primary!,TRUE)	
END IF
ldw_child.settransObject(SQLCA)
ldw_child.retrieve(ls_part_of_body_code)

ll_rowcount = ldw_child.RowCount()
IF ll_rowcount = 1 THEN
	ls_side_of_body_code = ldw_child.getItemString(1, 'side_of_body_code')
ELSE
	ls_side_of_body_code = dw_stats.getItemString(ll_row, 'side_of_body_code')
END IF

IF as_mode = 'set'  THEN
	dw_stats.setItem(ll_row, 'side_of_body_description', ls_side_of_body_code)
ELSEIF as_mode = 'reset' THEN
	dw_stats.setItem(ll_row, 'side_of_body_description','')
ELSEIF as_mode = 'cancel' THEN
	dw_stats.setItem(ll_row, 'side_of_body_description', ls_side_of_body_code)
END IF

return 1
end function

public function boolean wf_check_pb_sb (string as_pb_code, string as_sb_code);/*	Arguments:		
						as_pb_code		(part_of_body_code)
						as_sb_code		(side_of_body_code)
	Return			Boolean			(indicates whether the two codes can be combined in an ACCIDENT record)
*/

STRING	ls_flag
INTEGER	li_error_status

SELECT	active_flag
INTO		:ls_flag
FROM		Part_Of_Body_xref_Side_Of_Body
WHERE	(part_of_body_code = :as_pb_code
AND		 side_of_body_code = :as_sb_code)
USING SQLCA ;

li_error_status = SQLCA.nf_handle_error("w_accidents","","wf_check_pb_sb: 'SELECT active_flag'")

IF ls_flag <> "Y" THEN
	wf_invalid_combination_message("Part of Body","Side of Body")
	RETURN FALSE
ELSE
	RETURN TRUE
END IF

end function

on open;call w_a_tool::open;SetPointer(HOURGLASS!)

/*	Call wf_initialize_window to initialize the screen.
*/
	wf_initialize_window()

/*	Get the claim_no of the claim to work on.
*/
	il_claimno = wf_get_claim_no_to_work_on()

/*	Retrieve the information for the claim and open the imaged documents.
*/
	wf_setup_claim()

end on

event closequery;INTEGER li_mess_ret, li_err_col, li_error_status

dw_stats.AcceptText()

/*	Enable the closing of the window.
*/
	Message.ReturnValue = 0

	IF ib_needtosave THEN
		li_mess_ret = MessageBox("Accident Statistics Maintenance","Do you wish to save changes before closing?",Question!,YesNoCancel!,3)
		CHOOSE CASE li_mess_ret
			CASE 1
				li_err_col = wf_save('exit')
			CASE 3
				li_err_col = dw_stats.getcolumn()
				IF li_err_col < 1 THEN li_err_col = 2
		END CHOOSE
	END IF

/*	If save error or cancel, then do not close window.
*/
	IF li_err_col <> 0 THEN
		li_err_col = abs(li_err_col)
		dw_stats.SetColumn(li_err_col)
		dw_stats.SetFocus()
		Message.ReturnValue = 1
	END IF

end event

on w_accidents.create
int iCurrent
call super::create
this.dw_stats=create dw_stats
this.dw_claim=create dw_claim
this.cb_save=create cb_save
this.cb_next=create cb_next
this.cb_cancel=create cb_cancel
this.cb_previous=create cb_previous
this.uo_image_append=create uo_image_append
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_stats
this.Control[iCurrent+2]=this.dw_claim
this.Control[iCurrent+3]=this.cb_save
this.Control[iCurrent+4]=this.cb_next
this.Control[iCurrent+5]=this.cb_cancel
this.Control[iCurrent+6]=this.cb_previous
this.Control[iCurrent+7]=this.uo_image_append
end on

on w_accidents.destroy
call super::destroy
destroy(this.dw_stats)
destroy(this.dw_claim)
destroy(this.cb_save)
destroy(this.cb_next)
destroy(this.cb_cancel)
destroy(this.cb_previous)
destroy(this.uo_image_append)
end on

type st_title from w_a_tool`st_title within w_accidents
string text = "Maintain Accident Statistics"
end type

type cb_close from w_a_tool`cb_close within w_accidents
integer x = 2057
integer y = 1660
integer width = 329
integer height = 108
integer taborder = 10
end type

type dw_stats from u_dw_online within w_accidents
integer x = 23
integer y = 108
integer width = 3122
integer height = 1276
integer taborder = 40
string dataobject = "d_accident_stats_accident"
borderstyle borderstyle = styleraised!
end type

event retrieveend;call super::retrieveend;
STRING ls_part_of_body_code
datawindowchild ldw_child
Integer li_rtn


/*	Reset the need to save flag.
*/
ib_needtosave = FALSE

ls_part_of_body_code = getItemString(getrow(),'part_of_body_code')
if isnull(ls_part_of_body_code) or ls_part_of_body_code = '' THEN ls_part_of_body_code = '99990'  //should never happen, but put 'unknown - 99990' part of body in for now, it will be checked on save
	
wf_filter_side_of_body(ls_part_of_body_code, 'set')
	


end event

on doubleclicked;call u_dw_online::doubleclicked;/*	Select the whole line in the ddlb.
*/
	SelectText(1,Len(textline()))

end on

on editchanged;call u_dw_online::editchanged;/*	Turn on save & cancel button.
*/
	IF cb_save.enabled = FALSE THEN cb_save.enabled = TRUE
	IF cb_cancel.enabled = FALSE THEN cb_cancel.enabled = TRUE

/*	Reset the need to save flag.
*/
	ib_needtosave = TRUE

end on

event itemchanged;call super::itemchanged;INTEGER           li_err_col, li_columnis, li_count
STRING            ls_side_of_body_code, ls_part_of_body_code, ls_new_side_of_body_code, ls_new_side_of_body_desc_e
DataWindowChild   ldw_child


/*	Select and highlight the line.
*/
SelectText(1,Len(textline()))

/*
side_of_body_code can be 'U' for unknown but only for display on existing claims with no known value, 
'Unknown' cannot be one of the selectable items in the dropdown because its not active. Thats why we have a
plain edit field linked to a dddw that includes 'unknown' to display the description of unknown when required.
*/
IF dwo.name = 'side_of_body_code' THEN
	//side_of_body_code_description lies on top so set it so it looks th same as what was picked, for consistency
	dw_stats.setItem(1, 'side_of_body_description', data)
END IF


/*	Turn on save & cancel button.
*/
IF cb_save.enabled = FALSE THEN cb_save.enabled = TRUE
IF cb_cancel.enabled = FALSE THEN cb_cancel.enabled = TRUE


/*	Reset the need to save flag.
*/
ib_needtosave = TRUE


/*	Validate selected/entered value.
*/
li_columnis = dw_stats.getcolumn()
li_err_col = wf_validate(li_columnis)

/*	IF an error occured (li_err_col < 0), then set focus to the column it was in.
*/
IF li_err_col < 0 THEN
	dw_stats.SetColumn(li_columnis)
	dw_stats.SetFocus()
END IF



IF dwo.name = 'part_of_body_code' THEN
	ls_part_of_body_code = data
	ls_side_of_body_code = THIS.GetItemString(1,'side_of_body_code')
	
	
	// get number of 'side of body' related to newly selected part of body
	SELECT COUNT(*)
	INTO   :li_count
	FROM   Part_Of_Body_xref_Side_Of_Body
	WHERE  part_of_body_code = :ls_part_of_body_code
	AND    active_flag       = 'Y'
	USING SQLCA;
	SQLCA.nf_handle_error('w_accidents','embedded SQL: SELECT COUNT(*) FROM Part_Of_Body_xref_Side_Of_Body...','dw_stats.itemchanged')
	
	IF li_count = 1 THEN
		// if there is only one related side of body, then set side of body to that one
		SELECT b.side_of_body_code, 
		       b.side_of_body_desc_e
		INTO   :ls_new_side_of_body_code,
		       :ls_new_side_of_body_desc_e
		FROM   Part_Of_Body_xref_Side_Of_Body a
		JOIN   Side_Of_Body                   b ON a.side_of_body_code = b.side_of_body_code
		WHERE  a.part_of_body_code = :ls_part_of_body_code
	   AND    a.active_flag       = 'Y'
		USING SQLCA;
		SQLCA.nf_handle_error('w_accidents','embedded SQL: SELECT COUNT(*) FROM Part_Of_Body_xref_Side_Of_Body...','dw_stats.itemchanged')
		
		THIS.SetItem(1,'side_of_body_code',ls_new_side_of_body_code)
		THIS.SetItem(1,'side_of_body_description',ls_new_side_of_body_desc_e)
		
	ELSE
		// otherwise, if the current side of body does not have an associated record for the new part of body,
		// then clear the side of body for a new selection
		SELECT COUNT(*)
		INTO   :li_count
		FROM   Part_Of_Body_xref_Side_Of_Body
		WHERE  part_of_body_code = :ls_part_of_body_code
		AND    side_of_body_code = :ls_side_of_body_code
		AND    active_flag       = 'Y'
		USING SQLCA;
		SQLCA.nf_handle_error('w_accidents','embedded SQL: SELECT COUNT(*) FROM Part_Of_Body_xref_Side_Of_Body...','dw_stats.itemchanged')
		
		IF li_count = 0 THEN
			THIS.SetItem(1,'side_of_body_code','')
			THIS.SetItem(1,'side_of_body_description','')
		END IF
	END IF
	
END IF

end event

event itemerror;/*	Allow the focus to change so that 'wf_validate' can handle the ItemError.
*/
RETURN 2

end event

type dw_claim from u_dw_online within w_accidents
boolean visible = false
integer x = 27
integer y = 1408
integer width = 2615
integer height = 108
integer taborder = 70
string dataobject = "d_accident_stats_claim"
borderstyle borderstyle = styleraised!
end type

on retrieveend;call u_dw_online::retrieveend;IF THIS.RowCount() = 0 THEN
   MessageBox("DW_CLAIM","No records returned.")
END IF

end on

type cb_save from commandbutton within w_accidents
integer x = 265
integer y = 1660
integer width = 329
integer height = 108
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Save"
end type

event clicked;INTEGER	li_err_col, li_rtn
N_PROCESS_RUN_STATUS ln_process_run_status

/******************************************************************************************
P10275 - Daytime Payment Processing
- added new function call to prevent updating of tables used by PRODSVCS Payment Processing
- new object N_PROCESS_RUN_STATUS is used to determine status of processes (in progress = Y/N)
- '023' refers to the Accident Stats module, '044' refers to the Payment Processing module
******************************************************************************************/
ln_process_run_status = Create N_PROCESS_RUN_STATUS

li_rtn = ln_process_run_status.nf_in_progress('023','044','save',SQLCA)

IF li_rtn = 1 THEN
	// module is blocked
	return
END IF
/******************************************************************************************/
/******************************************************************************************
PR 9576 - ABCC Eligibility Extract processing
- added function call to prevent updating of tables used by PRODSVCS ABCC Eligibility Extract process
- N_PROCESS_RUN_STATUS is used to determine status of process (in progress = Y/N)
- '023' refers to the Accident Stats module, '046' refers to the ABCC Eligibility Extract
******************************************************************************************/

//clear variable
li_rtn = 0
li_rtn = ln_process_run_status.nf_in_progress('023','046','save accident stats',SQLCA)

IF li_rtn = 1 THEN
	// module is blocked
	return
END IF

/******************************************************************************************/

SetPointer(HourGlass!)

li_err_col = wf_save('next')
IF li_err_col <> 0 THEN
	li_err_col = Abs(li_err_col)
	dw_stats.SetColumn(li_err_col)
ELSE
	dw_stats.SetColumn('nature_of_injury_code')
END IF

dw_stats.SetFocus()

iw_active_sheet.wf_set_claim(il_claimno)
end event

type cb_next from commandbutton within w_accidents
integer x = 1161
integer y = 1660
integer width = 329
integer height = 108
integer taborder = 60
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Next"
end type

on clicked;wf_move_record_prompt('next')

end on

type cb_cancel from commandbutton within w_accidents
integer x = 713
integer y = 1660
integer width = 329
integer height = 108
integer taborder = 30
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Cance&l"
end type

event clicked;wf_reset_to_original()
dw_stats.setColumn('nature_of_injury_code')
dw_stats.setFocus()

end event

type cb_previous from commandbutton within w_accidents
integer x = 1609
integer y = 1660
integer width = 329
integer height = 108
integer taborder = 50
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Prev"
end type

on clicked;wf_move_record_prompt('previous')
end on

type uo_image_append from u_image_append within w_accidents
boolean visible = false
integer x = 754
integer y = 1404
integer height = 240
integer taborder = 70
end type

on uo_image_append.destroy
call u_image_append::destroy
end on

