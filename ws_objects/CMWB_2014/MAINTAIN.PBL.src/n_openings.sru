$PBExportHeader$n_openings.sru
$PBExportComments$user object for openings
forward
global type n_openings from n_pdc
end type
end forward

global type n_openings from n_pdc
end type
global n_openings n_openings

type variables
DATETIME idt_accident_date
LONG il_claim_no
STRING is_status_code, is_status_type_code
end variables

forward prototypes
public function integer nf_retrieve (long al_claim_no)
public function integer nf_get_next_identifier (long al_claim_no)
public function integer nf_check_mandatory ()
public function integer nf_set_defaults ()
public function integer nf_insert (long al_row)
public function integer nf_check_date_overlap (long al_row, datetime adt_date, string al_type)
public function integer nf_change_item (long al_datawindow, string as_column_name)
public function string nf_pen_or_sv_open ()
public function integer nf_check_accident_recurr (integer al_row, datetime adtm_recurrence_date, string as_recurrence_type)
public function integer nf_check_benefit_end_future (string as_type)
public function integer nf_check_open_type (string as_type)
public function boolean nf_all_openings_ended ()
public function boolean nf_end_before_due (datetime adtm_review_due)
public function boolean nf_more_than_one_opened ()
public function boolean nf_check_for_awards (long al_claim_no, long al_opening_no)
public function boolean nf_check_for_ben_calcs (long al_claim_no, long al_opening_no, datetime adtm_accident_recurrence_date)
public function boolean nf_check_for_payments (long al_claim_no, long al_opening_no, datetime adtm_accident_recurrence_date)
public function boolean nf_check_payment_delete (long al_claim_no, long al_opening_no)
public function boolean nf_check_ben_calc_delete (long al_claim_no, integer al_opening_no)
public function integer nf_set_unused_fields ()
public function datetime nf_get_min_recurrence_date ()
public function integer nf_check_bus_rule ()
public function boolean nf_check_for_benefit_entitlement (long al_claim_no, long al_opening_no)
public function integer nf_check_for_reminders (long al_claim_no)
end prototypes

public function integer nf_retrieve (long al_claim_no);LONG             ll_rows
DATAWINDOWCHILD  ldwc_child
STRING           ls_filter
// dw 1 = openings


ll_rows = idw_dw[1].Retrieve(al_claim_no)


Return ll_rows
end function

public function integer nf_get_next_identifier (long al_claim_no);LONG  ll_opening_no

// get the next opening number - the max + 1 for the claim

// need to know the claim number - set in controller
SELECT IsNull(Max(opening_no),0)
  INTO :ll_opening_no
  FROM OPENING
 WHERE claim_no = :il_claim_no;

IF SQLCA.nf_handle_error('n_openings', 'nf_get_next_identifier', '') < 0 THEN
   Return -1
ELSE
   Return (ll_opening_no + 1)
END IF

end function

public function integer nf_check_mandatory ();LONG  ll_current_row, ll_max_row


IF idw_dw[1].AcceptText() < 0 THEN Return -1


ll_current_row = 1
ll_max_row = idw_dw[1].RowCount()
DO WHILE ll_current_row <= ll_max_row

   IF IsNull(idw_dw[1].GetItemString(ll_current_row,'opening_type_code')) OR Trim(idw_dw[1].GetItemString(ll_current_row,'opening_type_code')) = '' THEN
      MessageBox("Missing Opening Type","The opening type must be entered.  Please enter.")
      idw_dw[1].SetColumn('opening_type_code')
      Return -1
   END IF

   IF IsNull(idw_dw[1].GetItemDateTime(ll_current_row,'benefit_start_date')) THEN
      MessageBox("Missing Start Date","The benefit start date must be entered.  Please enter.")
      idw_dw[1].SetColumn('benefit_start_date')
      Return -1
   END IF

   IF IsNull(idw_dw[1].GetItemDateTime(ll_current_row,'accident_recurrence_date')) THEN
      MessageBox("Missing Start Date","The accident recurrence date must be entered.  Please enter.")
      idw_dw[1].SetColumn('accident_recurrence_date')
      Return -1
   END IF

   IF IsNull(idw_dw[1].GetItemString(ll_current_row, 'recurrence_type_code')) OR Trim(idw_dw[1].GetItemString(ll_current_row, 'recurrence_type_code') ) = '' THEN
		MessageBox('Missing Recurrence Type', 'The recurrence type code must be entered.  Please enter.')
		idw_dw[1].SetColumn('recurrence_type_code')
		Return -1
	END IF

   IF NOT IsNull(idw_dw[1].GetItemDateTime(ll_current_row,'benefit_end_date')) THEN
      IF IsNull(idw_dw[1].GetItemString(ll_current_row, 'claim_disposition_code')) OR Trim(idw_dw[1].GetItemString(ll_current_row, 'claim_disposition_code')) = '' THEN
         MessageBox("Missing Disposition","Claim disposition code must be entered if end date specified.  Please enter.")
         idw_dw[1].SetColumn('claim_disposition_code')
         Return -1
      END IF
   ELSE
      IF is_status_code = 'F' THEN
         MessageBox('Error', 'All end dates must be specified for Finalled claims.')
         Return -1
      END IF
   END IF

   ll_current_row = ll_current_row + 1
LOOP

Return 0
end function

public function integer nf_set_defaults ();DATAWINDOWCHILD  	ldwc_child
STRING           	ls_opening
LONG             	ll_row, ll_loop, ll_max
DATETIME				ldtm_max_end, ldtm_max_recurrence
/*	this function only runs on an insert
*/

	ll_row = idw_dw[1].GetRow()
	idw_dw[1].SetItem(ll_row,'receiving_ltd_code', 'I')
	idw_dw[1].SetItem(ll_row,'claimant_working_flag', 'I')
	idw_dw[1].SetItem(ll_row,'work_restriction_flag', 'I')
	idw_dw[1].SetItem(ll_row,'claim_disposition_code', ' ')
	idw_dw[1].SetItem(ll_row,'three_day_exempt_code', '.')
	IF ll_row = 1 THEN
/*	assuming that is only one row then it is the first opening since all rows are inserted
	at the end
*/
		idw_dw[1].SetItem(ll_row,'accident_recurrence_date', idt_accident_date)
		idw_dw[1].SetItem(ll_row,'benefit_start_date', idt_accident_date)
		idw_dw[1].SetItem(ll_row,'recurrence_type_code','R')
		idw_dw[1].SetItem(ll_row,'opening_type_code', 'RLOE')
	ELSE
		idw_dw[1].SetItem(ll_row,'recurrence_type_code', 'A')
		ll_loop = 1
		ll_max = idw_dw[1].RowCount()
		ldtm_max_end =  DateTime(Date('19000101'),Time('00:00:00'))
		ldtm_max_recurrence = DateTime(Date('19000101'), Time('00:00:00'))
		ls_opening = ' '
		DO WHILE ll_loop <= ll_max
			IF idw_dw[1].GetItemDateTime(ll_loop,'benefit_end_date') > ldtm_max_end THEN
				ldtm_max_end = idw_dw[1].GetItemDateTime(ll_loop,'benefit_end_date')
				ls_opening = idw_dw[1].GetItemString(ll_loop,'opening_type_code')
			END IF
			IF idw_dw[1].GetItemDateTime(ll_loop,'accident_recurrence_date') > ldtm_max_recurrence THEN
				ldtm_max_recurrence = idw_dw[1].GetItemDateTime(ll_loop,'accident_recurrence_date')
			END IF
			ll_loop = ll_loop + 1
		LOOP
		idw_dw[1].SetItem(ll_row,'opening_type_code', ls_opening)
		idw_dw[1].SetItem(ll_row,'accident_recurrence_date',ldtm_max_recurrence)
	END IF
	

Return 0
end function

public function integer nf_insert (long al_row);LONG ll_row


// if status ok then insert row

IF is_status_code = 'A' OR (is_status_code = 'F' AND (is_status_type_code = '01' OR is_status_type_code = '02')) THEN
   ll_row = idw_dw[1].InsertRow(al_row) 
   IF ll_row > 0 THEN
      idw_dw[1].ScrollToRow(ll_row)
   END IF
ELSE
   MessageBox('Error', 'The status code does not allow for openings to be entered.')
   Return -1
END IF
IF nf_set_defaults() < 0 THEN Return -1
Return 0


end function

public function integer nf_check_date_overlap (long al_row, datetime adt_date, string al_type);LONG  ll_loop, ll_max

ll_loop = 1
ll_max = idw_dw[1].RowCount()

DO WHILE ll_loop <= ll_max
   IF al_type = idw_dw[1].GetItemString(ll_loop, 'opening_type_code') AND ll_loop <> al_row THEN
   // the rows are of the same type and not the same row
   // now check to see if the date past is between the dates on the row
      IF adt_date >= idw_dw[1].GetItemDateTime(ll_loop, 'benefit_start_date') AND ( adt_date < idw_dw[1].GetItemDateTime(ll_loop, 'benefit_end_date') OR ISNULL(idw_dw[1].GetItemDateTime(ll_loop, 'benefit_end_date'))) THEN
         Return -1
      END IF
   END IF
   ll_loop = ll_loop + 1
LOOP
Return 0
end function

public function integer nf_change_item (long al_datawindow, string as_column_name);/*	the code for this function is in the window
*/

Return 0
end function

public function string nf_pen_or_sv_open ();LONG		ll_loop, ll_max
STRING	ls_opening_type_code
/*	Determine if a pension benefit or survior benefit is open
	Return 'PEN' - pension
			 'SV'  - survior
			 'S1'  - SURVIOR 85% ELECTED
			 'S2'  - SURVIOR 60% ELECTED
			 
			 ''    - neither
*/

	ll_loop = 1
	ll_max = idw_dw[1].RowCount()
	DO WHILE ll_loop <= ll_max	
		ls_opening_type_code = idw_dw[1].GetItemString(ll_loop, 'opening_type_code')
		IF IsNull(idw_dw[1].GetItemDateTime(ll_loop, 'benefit_end_date')) THEN
			IF ls_opening_type_code = 'PEN' THEN
				Return 'PEN'
			END IF
			IF ls_opening_type_code = 'SV' THEN
				Return 'SV'
			END IF
			IF ls_opening_type_code = 'S1' THEN
				Return 'S1'
			END IF
			IF ls_opening_type_code = 'S2' THEN
				Return 'S2'
			END IF
		END IF
		ll_loop = ll_loop + 1
	LOOP

Return ''
end function

public function integer nf_check_accident_recurr (integer al_row, datetime adtm_recurrence_date, string as_recurrence_type);LONG  ll_loop, ll_max

	ll_loop = 1
	ll_max = idw_dw[1].RowCount()

	DO WHILE ll_loop <= ll_max
		CHOOSE CASE as_recurrence_type
			CASE 'A'
/*			if type is A then it must match an accident recurrence date for an opening of type R
*/
   			IF idw_dw[1].GetItemString(ll_loop, 'recurrence_type_code') = 'R' AND ll_loop <> al_row THEN
  			    	IF adtm_recurrence_date = idw_dw[1].GetItemDateTime(ll_loop, 'accident_recurrence_date') THEN
         			Return 0
			      END IF
   			END IF
			CASE 'R'
   			IF idw_dw[1].GetItemString(ll_loop, 'recurrence_type_code') = 'R' AND ll_loop <> al_row THEN
  			    	IF adtm_recurrence_date = idw_dw[1].GetItemDateTime(ll_loop, 'accident_recurrence_date') THEN
         			Return -1
			      END IF
   			END IF
		END CHOOSE	
	   ll_loop = ll_loop + 1
	LOOP
	IF as_recurrence_type = 'A' THEN
		Return -1
	ELSE
		Return 0
	END IF
end function

public function integer nf_check_benefit_end_future (string as_type);LONG		ll_loop, ll_max
DATETIME	ldtm_today

/*	this functions checks for an opening that is still open
	it passed back information to the parent so that it can then
	be passed to the claim business rule checker
*/
	ldtm_today = f_server_datetime()
	ll_max = idw_dw[1].RowCount()
	ll_loop = 1
	DO WHILE ll_loop <= ll_max
   	IF idw_dw[1].GetItemString(ll_loop, 'opening_type_code') = as_type THEN
/*		check to see if end date is in the future
*/
      	IF idw_dw[1].GetItemDateTime(ll_loop, 'benefit_end_date') > ldtm_today THEN
         	Return ll_loop
	      END IF
   	END IF
	   ll_loop = ll_loop + 1
	LOOP
Return -1
end function

public function integer nf_check_open_type (string as_type);LONG ll_loop, ll_max

// this functions checks for a RLOE opening that is still open
// it passed back information to the parent so that it can then
// be passed to the claim business rule checker

ll_max = idw_dw[1].RowCount()
ll_loop = 1
DO WHILE ll_loop <= ll_max
   IF idw_dw[1].GetItemString(ll_loop, 'opening_type_code') = as_type THEN
   // check to see if it is still open
      IF IsNull(idw_dw[1].GetItemDateTime(ll_loop, 'benefit_end_date')) THEN
         Return ll_loop
      END IF
   END IF
   ll_loop = ll_loop + 1
LOOP
Return -1
end function

public function boolean nf_all_openings_ended ();LONG		ll_loop, ll_max
DATETIME	ldtm_today

	ll_max = idw_dw[1].RowCount()
	ll_loop = 1
	DO WHILE ll_loop <= ll_max

     	IF IsNull(idw_dw[1].GetItemDateTime(ll_loop, 'benefit_end_date')) THEN
        	Return FALSE
      END IF
	   ll_loop = ll_loop + 1
	LOOP
Return TRUE
end function

public function boolean nf_end_before_due (datetime adtm_review_due);LONG		ll_loop, ll_max
DATETIME	ldtm_today

	ll_max = idw_dw[1].RowCount()
	ll_loop = 1
	DO WHILE ll_loop <= ll_max
     	IF idw_dw[1].GetItemDateTime(ll_loop, 'benefit_end_date') >= adtm_review_due THEN
        	Return FALSE
      END IF
	   ll_loop = ll_loop + 1
	LOOP
Return TRUE
end function

public function boolean nf_more_than_one_opened ();LONG		ll_loop, ll_max, ll_count
DATETIME	ldtm_today

	ll_count = 0
	ll_max = idw_dw[1].RowCount()
	ll_loop = 1
	DO WHILE ll_loop <= ll_max
     	IF IsNull(idw_dw[1].GetItemDateTime(ll_loop, 'benefit_end_date')) THEN
        	ll_count = ll_count + 1
      END IF
	   ll_loop = ll_loop + 1
	LOOP
	IF ll_count <= 1 THEN
		Return FALSE
	END IF
Return TRUE
end function

public function boolean nf_check_for_awards (long al_claim_no, long al_opening_no);LONG	ll_count

	IF NOT IsNull(al_opening_no) THEN
		SELECT Count(*)
		  INTO :ll_count
		  FROM PERIODIC_AWARD
		 WHERE claim_no = :al_claim_no
	   	AND opening_no = :al_opening_no;

		IF SQLCA.nf_handle_error('Embedded SQL: Select from PERIODIC_AWARD', 'n_openings', 'nf_check_for_awards') < 0 THEN
			Return FALSE
		END IF
		IF ll_count > 0 THEN
			Return TRUE
		END IF
	END IF
Return FALSE
end function

public function boolean nf_check_for_ben_calcs (long al_claim_no, long al_opening_no, datetime adtm_accident_recurrence_date);LONG	ll_count

	IF NOT IsNull(al_opening_no) THEN
		SELECT Count(*)
		  INTO :ll_count
	  	  FROM BENEFIT_CALCULATION
		 WHERE claim_no = :al_claim_no
	   	AND opening_no = :al_opening_no
			AND effective_from_date < :adtm_accident_recurrence_date;

		IF SQLCA.nf_handle_error('Embedded SQL: Select from BENEFIT_CALCULATION', 'n_openings', 'nf_check_for_ben_calcs') < 0 THEN
			Return FALSE
		END IF
		IF ll_count > 0 THEN
			Return TRUE
		END IF
	END IF
Return FALSE
end function

public function boolean nf_check_for_payments (long al_claim_no, long al_opening_no, datetime adtm_accident_recurrence_date);LONG	ll_count

	IF NOT IsNull(al_opening_no) THEN
		SELECT Count(*)
		  INTO :ll_count
		  FROM PAYMENT
		 WHERE claim_no = :al_claim_no
	   	AND opening_no = :al_opening_no
			AND paid_from_date < :adtm_accident_recurrence_date;

		IF SQLCA.nf_handle_error('Embedded SQL: Select from PAYMENT', 'n_openings', 'nf_check_for_payments') < 0 THEN
			Return FALSE
		END IF
		IF ll_count > 0 THEN
			Return TRUE
		END IF
	END IF
Return FALSE
end function

public function boolean nf_check_payment_delete (long al_claim_no, long al_opening_no);LONG	ll_count

	IF NOT IsNull(al_opening_no) THEN
		SELECT Count(*)
		  INTO :ll_count
		  FROM PAYMENT
		 WHERE claim_no = :al_claim_no
	   	AND opening_no = :al_opening_no;

		IF SQLCA.nf_handle_error('Embedded SQL: Select from PAYMENT', 'n_openings', 'nf_check_for_payments') < 0 THEN
			Return FALSE
		END IF
		IF ll_count > 0 THEN
			Return TRUE
		END IF
	END IF
Return FALSE
end function

public function boolean nf_check_ben_calc_delete (long al_claim_no, integer al_opening_no);LONG	ll_count

	IF NOT IsNull(al_opening_no) THEN
		SELECT Count(*)
		  INTO :ll_count
	  	  FROM BENEFIT_CALCULATION
		 WHERE claim_no = :al_claim_no
	   	AND opening_no = :al_opening_no;

		IF SQLCA.nf_handle_error('Embedded SQL: Select from BENEFIT_CALCULATION', 'n_openings', 'nf_check_for_ben_calcs') < 0 THEN
			Return FALSE
		END IF
		IF ll_count > 0 THEN
			Return TRUE
		END IF
	END IF
Return FALSE
end function

public function integer nf_set_unused_fields ();LONG 		ll_current_row, ll_max, ll_loop
STRING	ls_3day_flag_setting, ls_comp_setting, ls_3day_exempt_setting
STRING	ls_3day_flag_setting_new, ls_comp_setting_new, ls_3day_exempt_setting_new
DATE		ldt_accident_recurrence_date

ll_max = idw_dw[1].RowCount()
ll_current_row = 1

DO WHILE ll_current_row <= ll_max
	
	IF IsNull(idw_dw[1].GetItemString(ll_current_row, 'receiving_ltd_code')) THEN
		idw_dw[1].SetItem(ll_current_row, 'receiving_ltd_code', 'I')
	END IF
	IF IsNull(idw_dw[1].GetItemString(ll_current_row, 'claimant_working_flag')) OR (idw_dw[1].GetItemString(ll_current_row, 'claimant_working_flag') <> 'N' AND idw_dw[1].GetItemString(ll_current_row, 'claimant_working_flag') <> 'Y') THEN
		idw_dw[1].SetItem(ll_current_row, 'claimant_working_flag', 'I')
	END IF
	IF IsNull(idw_dw[1].GetItemString(ll_current_row, 'work_restriction_flag')) OR (idw_dw[1].GetItemString(ll_current_row, 'work_restriction_flag') <> 'N' AND idw_dw[1].GetItemString(ll_current_row, 'work_restriction_flag') <> 'Y') THEN
		idw_dw[1].SetItem(ll_current_row, 'work_restriction_flag', 'I')
	END IF

	IF IsNull(idw_dw[1].GetItemString(ll_current_row, 'recurrence_type_code')) THEN
		idw_dw[1].SetItem(ll_current_row, 'opening_type', 'R')
	END IF
	
	IF idw_dw[1].GetItemStatus(ll_current_row,0,Primary!) <> NotModified! THEN	
		/* For new Recurrence Openings: Set the Three Day Paid, the 85% Comp Paid flags and the three day 
			exemption code fields. (the three day paid and the 85% comp paid flag columns are not entered by the
			user: these are defaulted on the creation of an opening and then set on through the processing of payments)
		*/
		ldt_accident_recurrence_date = Date(idw_dw[1].GetItemDateTime(ll_current_row, 'accident_recurrence_date')) 
		IF (IsNull(idw_dw[1].GetItemString(ll_current_row, 'three_day_paid_flag'))  	OR &
			Trim(idw_dw[1].GetItemString(ll_current_row, 'three_day_paid_flag')) = '') AND &
			idw_dw[1].GetItemString(ll_current_row,'recurrence_type_code') = 'R'  		THEN
			/*	For Accident/Recurrences prior to 1993/01/01, the three day paid flag , 85% Comp Paid Flag and
				the three day exemption code are always inapplicable 
			*/
			IF ldt_accident_recurrence_date < Date('1993-01-01') THEN
				idw_dw[1].SetItem(ll_current_row, 'three_day_paid_flag', 'I')
				idw_dw[1].SetItem(ll_current_row, 'comp_85percent_flag', 'I')
				idw_dw[1].SetItem(ll_current_row, 'three_day_exempt_code', '.')
			ELSE
				/* The 85% comp paid flag is Inapplicable if the Accident/Recurrence is on or after 
					January 01, 1998; otherwise it is set to No.
				*/
				IF ldt_accident_recurrence_date < Date ('1998/01/01') THEN
					idw_dw[1].SetItem(ll_current_row, 'comp_85percent_flag', 'N')
				ELSE
					idw_dw[1].SetItem(ll_current_row, 'comp_85percent_flag', 'I')
				END IF
				/* The value for the 3 day paid flag depends on the three day exemption code 
				*/
				IF idw_dw[1].GetItemString(ll_current_row, 'three_day_exempt_code') = '.' THEN
					idw_dw[1].SetItem(ll_current_row, 'three_day_paid_flag', 'N')
				ELSE
					idw_dw[1].SetItem(ll_current_row, 'three_day_paid_flag', 'I')
				END IF
			END IF
		END IF
	END IF
   ll_current_row = ll_current_row + 1
LOOP


/*	For all related administrative openings,the three day paid flag, the 85% comp paid flag and the three day
	exemption code are set to the same value as the matching recurrence opening. 
	So find the matching Opening recurrence...
*/
ll_current_row = 1
								
DO WHILE ll_current_row <= ll_max
	IF idw_dw[1].GetItemString(ll_current_row,'recurrence_type_code') = 'A' THEN
		ll_loop = 1
		DO WHILE ll_loop <= ll_max
			IF idw_dw[1].GetItemDateTime(ll_current_row, 'accident_recurrence_date') = &
				idw_dw[1].GetItemDateTime(ll_loop, 'accident_recurrence_date') 	AND &
				idw_dw[1].GetItemString(ll_loop,'recurrence_type_code') = 'R' 	THEN
				ls_3day_flag_setting_new = idw_dw[1].GetItemString(ll_loop,'three_day_paid_flag')
				ls_comp_setting_new = idw_dw[1].GetItemString(ll_loop, 'comp_85percent_flag')
				ls_3day_exempt_setting_new = idw_dw[1].GetItemString(ll_loop, 'three_day_exempt_code')
				Exit			/* Found the matching recurrence */
			END IF
			ll_loop = ll_loop + 1
		LOOP
		
		ls_3day_flag_setting = idw_dw[1].GetItemString(ll_current_row,'three_day_paid_flag')
		IF IsNull(ls_3day_flag_setting) OR ls_3day_flag_setting <> ls_3day_flag_setting_new THEN
			idw_dw[1].SetItem(ll_current_row, 'three_day_paid_flag', ls_3day_flag_setting_new)
		END IF
		ls_comp_setting = idw_dw[1].GetItemString(ll_current_row, 'comp_85percent_flag')
		IF IsNull(ls_comp_setting) OR ls_comp_setting <> ls_comp_setting_new THEN
			idw_dw[1].SetItem(ll_current_row, 'comp_85percent_flag', ls_comp_setting_new)
		END IF
		ls_3day_exempt_setting = idw_dw[1].GetItemString(ll_current_row, 'three_day_exempt_code')
		IF IsNull(ls_3day_exempt_setting) OR ls_3day_exempt_setting <> ls_3day_exempt_setting_new THEN
			idw_dw[1].SetItem(ll_current_row, 'three_day_exempt_code', ls_3day_exempt_setting_new)		
		END IF
	END IF
	
	ll_current_row = ll_current_row + 1
	
LOOP
			
Return 0

end function

public function datetime nf_get_min_recurrence_date ();LONG     ll_cntr
DATETIME ldtm_acc_rec_date
STRING   ls_opening_type_code
	
ldtm_acc_rec_date       = idw_dw[1].GetItemDateTime(idw_dw[1].GetRow(),'accident_recurrence_date')

FOR ll_Cntr = idw_dw[1].RowCount() to 1 STEP -1
	ls_opening_type_code    = idw_dw[1].GetItemString(ll_cntr,'opening_type_code')
	IF ls_opening_type_code = 'RLOE' THEN
		IF idw_dw[1].GetItemDateTime(ll_cntr,'accident_recurrence_date') <= ldtm_acc_rec_date THEN
			ldtm_acc_rec_date = idw_dw[1].GetItemDateTime(ll_cntr,'accident_recurrence_date')
		END IF
	END IF
NEXT 

RETURN ldtm_acc_rec_date
end function

public function integer nf_check_bus_rule ();BOOLEAN  			lb_all_closed , lb_ltd_open, lb_pen_open, lb_rloe_open, lb_benefits, lb_sv_open, lb_payments, lb_awards, lb_benefit_entitlement
LONG     				ll_max, ll_loop, ll_count, ll_opening_no, ll_selected_opening_no
STRING  				ls_string, ls_string2, ls_string3, ls_string4, ls_string5, ls_string6
STRING				ls_opening_type_code, ls_comment_req_flag,  ls_selected_opening_type,  ls_opening_no_message
DATETIME			ldtm_server_date, ldtm_date, ldtm_min_date, ldtm_employment, ldtm_benefit_end_date, ldtm_award_end_date, ldtm_start_date
DATAWINDOWCHILD	ldwc_child

/*	loop through all rows
*/
	ll_max = idw_dw[1].RowCount()
	ll_loop = 1

	lb_all_closed 		= TRUE
	lb_ltd_open 			= FALSE
	lb_pen_open 		= FALSE
	lb_rloe_open 		= FALSE
	lb_sv_open 			= FALSE
	ldtm_server_date 	= f_server_datetime()
	ll_opening_no 		= 0
	
	/* 6.10		Openings must only be added if the claim status and type are:
		Status	Type
		Active		N/A
		Finalled	Final
		Finalled	First and Final
	*/
	IF is_status_code = 'A' OR (is_status_code = 'F' AND (is_status_type_code = '01' OR is_status_type_code = '02')) THEN
		
   	DO WHILE ll_loop <= ll_max

			lb_payments 				= FALSE
			lb_benefits 					= FALSE
			lb_awards 					= FALSE
			lb_benefit_entitlement 	= FALSE
			
			IF idw_dw[1].GetItemStatus(ll_loop,0,Primary!) <> NewModified! THEN
				ll_opening_no 				= idw_dw[1].GetItemNumber(ll_loop,'opening_no')
				lb_payments                  = nf_check_for_payments(il_claim_no,ll_opening_no, idw_dw[1].GetItemDateTime(1,'accident_recurrence_date')) 
				lb_benefits                   	= nf_check_for_ben_calcs(il_claim_no,ll_opening_no, idw_dw[1].GetItemDateTime(1,'accident_recurrence_date'))
				lb_benefit_entitlement    = nf_check_for_benefit_entitlement(il_claim_no,ll_opening_no)
				/* 	it is not necessary to check awards if the check on bencals is done correctly since awards cannot exist before any bencalc*/
			ELSE//make sure the opening number is NOT the previous rows number **PR found during P10237 ANNUITIES**
				ll_opening_no = idw_dw[1].GetItemNumber(ll_loop,'opening_no')
				
				IF ISNULL(ll_opening_no) THEN ll_opening_no = 0
				
			END IF
			
			IF ISNULL(ll_opening_no) OR ll_opening_no = 0 THEN
				ls_opening_no_message = ' New Opening'
			ELSE
				ls_opening_no_message = string(ll_opening_no)
			END IF 
			
/*		    check opening type
*/
      	    ls_opening_type_code = idw_dw[1].GetItemString(ll_loop,'opening_type_code')
			IF idw_dw[1].GetChild('opening_type_code', ldwc_child) < 1 THEN
				MessageBox('Error Retrieveing List', 'Unable to retrieve list of valid opening type codes.  Contact help desk.')
				RETURN -1
			END IF
			
			/* 6.15		The type of opening must be a valid, active entry in the opening type look-up table. (Refer to Rationale) */
			IF ldwc_child.Find('opening_type_code = "' + ls_opening_type_code + '" AND active_flag = "Y"',0, ldwc_child.RowCount()) < 0 THEN
      	         MessageBox('Invalid Opening - Opening No: ' + ls_opening_no_message ,'The opening type is not valid.')
         	    RETURN -1
	        END IF
			  
			/* 6.370	Once benefit calculations, benefit entitlement or payments have been made using the opening, the opening type must not be changed.*/
			IF ls_opening_type_code <> idw_dw[1].GetItemString(ll_loop,'opening_type_code',Primary!,TRUE) AND (lb_payments OR lb_benefits OR lb_benefit_entitlement) THEN
				MessageBox('Unable to modify Opening Type Opening No: ' + ls_opening_no_message ,'The opening type cannot be modified because payments, benefit calculations, or benefit entitlement exist.')
				RETURN -1
			END IF		
         
/*			benefit start date
*/
             // 6.30 The benefit start date of an opening must be on or after the accident date of the claim.
  			IF idw_dw[1].GetItemDateTime(ll_loop, 'benefit_start_date') < idw_dw[1].GetItemDateTime(ll_loop, 'accident_recurrence_date') THEN
         		MessageBox('Error Opening No: ' + ls_opening_no_message , 'Benefit start date cannot be before the accident date.')
	        		 RETURN -1
   	   		END IF
			
			
			/* 6.20		The start date and end date for an opening must not overlap with or equal any other benefit period for an opening 
							of the same benefit category. */
	         IF nf_check_date_overlap(ll_loop, idw_dw[1].GetItemDateTime(ll_loop, 'benefit_start_date'), ls_opening_type_code) < 0 THEN
   	             MessageBox('Error Opening No: ' + ls_opening_no_message , 'Openings of the same category can not have overlapping dates.')
      	        RETURN -1
	        END IF
			  
		  /* 6.387	The benefit start date must be on or before the payment from date of any processed or scheduled payments, 
			              associated with the opening, which have not been fully adjusted or cancelled. */
		  IF idw_dw[1].GetItemStatus(ll_loop,0,Primary!) = DataModified! THEN
			
				ll_opening_no = idw_dw[1].GetItemNumber(ll_loop,'opening_no')
				
				SELECT 	Min(paid_from_date)
				INTO 		:ldtm_date
				FROM 	PAYMENT
				WHERE 	claim_no 		= :il_claim_no
				AND 		opening_no 		= :ll_opening_no
				AND 		zeroed_flag 		<> 'Y' ;
				
				SQLCA.nf_handle_error('Embedded SQL: select from PAYMENT', 'n_openings', 'nf_check_bus_rule') 
				
				IF NOT IsNull(ldtm_date) THEN
					IF idw_dw[1].GetItemDateTime(ll_loop, 'benefit_start_date') > ldtm_date THEN
						MessageBox('Invalid Benefit Start - Opening No: ' + ls_opening_no_message , 'The start date cannot be after payments that have not been fully cancelled or adjusted.')
						RETURN -1
					END IF
				END IF
                 
			    /*if ok above check out the awards*/
				SELECT 	Min(award_start_date)
				INTO 		:ldtm_date
				FROM 	PERIODIC_AWARD
				WHERE 	claim_no 			= :il_claim_no
				AND 		opening_no 			= :ll_opening_no
				AND 		award_type_code <> 'CA' 
				AND  		award_type_code <> 'CL';
				
				SQLCA.nf_handle_error('Embedded SQL: select from PERIODIC_AWARD', 'n_openings', 'nf_check_bus_rule') 
			
			    /* 6.380	The benefit start date must not be greater than the start date of any awards associated with the opening, 
				              unless the award is a Clothing or Care Allowance.
				*/
				IF NOT IsNull(ldtm_date) THEN
					IF idw_dw[1].GetItemDateTime(ll_loop, 'benefit_start_date') > ldtm_date THEN
						MessageBox('Invalid Benefit Start - Opening No: ' + ls_opening_no_message , 'The benefit start date cannot be after the start of awards.')
						RETURN -1
					END IF
				END IF
				
				//benefit start date must be after the associated RTW Incentive employment start date.
				SetNull(ldtm_employment)		
				
				Select 	employment_start_date
				Into    	:ldtm_employment
				From  	RTW_INCENTIVE_QUALIFICATION
				Where 	claim_no 		= :il_claim_no
				And     	opening_no 		= :ll_opening_no
				Using 		SQLCA;
		
				SQLCA.nf_handle_error("w_claim", "itemchanged", "dw_opening_details - Select employment_start_date From RTW_INCENTIVE_QUALIFCATION")

                  /* 15.45	The employment start date must be on or after the benefit start date of the associated opening. */
				IF NOT IsNull( ldtm_employment) AND DATE( ldtm_employment) <> 1900-01-01 THEN
			 		IF idw_dw[1].GetItemDateTime(ll_loop, 'benefit_start_date') > ldtm_employment THEN
         				MessageBox('Error Opening No: ' + ls_opening_no_message , 'Benefit start date cannot be after the associated RTW Incentive employment start date.')
	        				 RETURN -1
   	   				END IF
				END IF			
			END IF

		/* P10273 Annuities - 6.385 The benefit start date must be on or before the start date of any active benefit entitlement associated with the opening.
		    for annuity purposes - The benefit start date must on or before any active benefit entitlement associated with the opening.
		*/
			IF  idw_dw[1].GetItemStatus(ll_loop,0,Primary!) = DataModified! THEN 
				ldtm_start_date = idw_dw[1].GetItemDateTime(ll_loop, 'benefit_start_date')
				
				Select     count(*)
				Into    	:ll_count
				From   	BENEFIT_ENTITLEMENT
				Where  	claim_no 							= :il_claim_no
				And    	opening_no 							= :ll_opening_no
				And    	benefit_entitlement_from_date < :ldtm_start_date
				And    	active_flag 							= 'Y'
				Using 		SQLCA;
				
				SQLCA.nf_handle_error("n_openings", "nf_check_bus_rule", "Select count From BENEFIT_ENTITLEMENT")
				
				/* 6.385	The benefit start date must be on or before the start date of any active benefit entitlement associated with the opening.
				*/
				IF ll_count > 0 THEN 
					MessageBox('For annuity purposes - Opening No: ' + ls_opening_no_message ,"The benefit start date must be on or before any active benefit entitlement associated with the opening.")                                                                                                          
					RETURN -1
				END IF
			END IF 
			
			/* 6.340 The 12-week review date must only be entered if the opening is a regular loss of earnings type.
			*/
			IF idw_dw[1].GetItemString(ll_loop,'opening_type_code') <> 'RLOE' AND NOT IsNull(idw_dw[1].GetItemDateTime(ll_loop,'review_12_week_date')) THEN	
				MessageBox('Warning - Opening No: ' + ls_opening_no_message , '12 week review only valid for RLOE - blanking out date.')
				SetNull(ldtm_date)
				idw_dw[1].SetItem(ll_loop,'review_12_week_date',ldtm_date)
			END IF
			
			IF NOT IsNull(idw_dw[1].GetItemDateTime(ll_loop,'review_12_week_date')) THEN	
				IF Date(idw_dw[1].GetItemDateTime(ll_loop,'review_12_week_date')) < Date('1900/01/01') THEN	
					MessageBox('Invalid 12 Week Review - Opening No: ' + ls_opening_no_message,'The 12 week review date is invalid.')
					RETURN -1
				END IF
			END IF
			
			/* 6.350 The 12-week review date must be after the accident/recurrence date of the first RLOE opening.*/
			IF NOT IsNull(idw_dw[1].GetItemDateTime(ll_loop,'review_12_week_date')) THEN	
				ldtm_min_date = nf_get_min_recurrence_date()
				IF Date(idw_dw[1].GetItemDateTime(ll_loop,'review_12_week_date')) <= Date(ldtm_min_date) THEN	
					MessageBox('Invalid 12 Week Review - Opening No: ' + ls_opening_no_message,'The 12 week review date must be greater than the first RLOE accident/recurrence date.')
					RETURN -1
				END IF
			END IF
						
/*			end date
*/
	  IF NOT IsNull(idw_dw[1].GetItemDateTime(ll_loop, 'benefit_end_date')) THEN
   	      	IF idw_dw[1].GetItemDateTime(ll_loop, 'benefit_end_date') < idt_accident_date THEN
      	      	MessageBox('Error - Opening No: ' + ls_opening_no_message, 'Benefit end date cannot be before the accident date.')
         	   	RETURN -1
	     	END IF
		
		ldtm_benefit_end_date = idw_dw[1].GetItemDateTime(ll_loop, 'benefit_end_date')
		
		// 6.90 The end date must not be before the end date of any periodic awards for that opening.
		IF ls_opening_type_code <> 'RLOE' THEN
			SetNull(ldtm_award_end_date)
			
			Select 	Max(award_end_date)
			into    	:ldtm_award_end_date
			From  	PERIODIC_AWARD 
			Where 	claim_no 			= 	:il_claim_no
			And   	 	opening_no 			=  :ll_opening_no
			And    	(award_type_code <> 'CA' AND award_type_code <> 'CL')
			Using 		SQLCA;

			SQLCA.nf_handle_error("n_openings","nf_check_bus_rule","Select from PERIODIC_AWARD")
		
		    /* 6.90 	The end date must not be before the end date of any periodic awards for that opening. */
			IF NOT IsNull(ldtm_award_end_date) THEN
				IF ldtm_award_end_date > ldtm_benefit_end_date THEN
      	      		MessageBox('Error - Opening No: ' + ls_opening_no_message , 'The benefit end date can not be before a corresponding award end date.')
         	   		RETURN -1
	         	END IF
			END IF
		END IF	
		
		/* 6.100 	The following information must be entered only if a benefit is ended:
				·	Work restriction indicator
				·	Claimant working indicator
				·	Receiving ltd indicator
				·	Disposition 
				·	Disposition reason (depends upon the disposition chosen).
					(Refer to Rationale)
		*/
  	      IF IsNull(idw_dw[1].GetItemString(ll_loop, 'receiving_ltd_code')) OR  Trim(idw_dw[1].GetItemString(ll_loop, 'receiving_ltd_code')) = '' THEN
      	      MessageBox('Error - Opening No: ' + ls_opening_no_message , 'Receiving LTD must have a value when end date specified.')
         	   RETURN -1
	      END IF
   	      IF IsNull(idw_dw[1].GetItemString(ll_loop, 'claimant_working_flag')) OR  Trim(idw_dw[1].GetItemString(ll_loop, 'claimant_working_flag')) = '' THEN
      	      MessageBox('Error - Opening No: ' + ls_opening_no_message , 'Claimant Working must have a value when end date specified.')
         	   RETURN -1
	      END IF
   	      IF IsNull(idw_dw[1].GetItemString(ll_loop, 'work_restriction_flag')) OR  Trim(idw_dw[1].GetItemString(ll_loop, 'work_restriction_flag')) = '' THEN
      	      MessageBox('Error - Opening No: ' + ls_opening_no_message , ' Work restriction must have a value when end date specified.')
         	   RETURN -1
	      END IF
				
		// 6.70 The end date of an opening should not be more than seven weeks in the future.	
		 IF Date(idw_dw[1].GetItemDateTime(ll_loop, 'benefit_end_date')) > RelativeDate(Date(ldtm_server_date),49) and Date(idw_dw[1].GetItemDateTime(ll_loop, 'benefit_end_date')) <= RelativeDate(Date(ldtm_server_date),90) THEN
      	      IF MessageBox('Warning - Opening No: ' + ls_opening_no_message, 'Benefit end date is more than 7 weeks in the future.~r~nWould you like to Continue?',question!,yesno!) = 2 THEN
				 RETURN -1
			 END IF
		 //6.80 The end date of an opening must not be more than 3 months in the future.
        	 ELSEIF Date(idw_dw[1].GetItemDateTime(ll_loop, 'benefit_end_date')) > RelativeDate(Date(ldtm_server_date),90) THEN
			MessageBox('Error - Opening No: ' + ls_opening_no_message, 'Benefit end date cannot be more than 3 months in the future.')
             RETURN -1
	      END IF

		/* 6.95	The end date of an opening must be on or after the end date of any active benefit entitlement associated with the opening. */
		IF  idw_dw[1].GetItemStatus(ll_loop,0,Primary!) = DataModified! THEN  
			
			Select 	count(*)
			Into    	:ll_count
			From   	BENEFIT_ENTITLEMENT
			Where  	claim_no 						= :il_claim_no
			And    	opening_no 						= :ll_opening_no
			And    	benefit_entitlement_to_date > :ldtm_benefit_end_date
			And    	active_flag 						= 'Y'
			Using 		SQLCA;
			
			SQLCA.nf_handle_error("n_openings", "nf_check_bus_rule", "Select count From BENEFIT_ENTITLEMENT")
			
			IF ll_count > 0 THEN   //The BE must before the End Date of the opening
				MessageBox('Invalid End Date - Opening No: ' + ls_opening_no_message,"The end date of the opening must be on or after the end date of any active benefit entitlement associated with the opening.")
				RETURN -1
			END IF
		END IF 
		

/*		 end date cannot be before any payments for the opening that are not fully cancelled or adjusted	
		6.60	The benefit end date must be on or after the paid to date of any processed or scheduled payments, associated with 
				the opening, which have not been fully adjusted or cancelled.
*/
		 IF idw_dw[1].GetItemStatus(ll_loop,0,Primary!) = DataModified! THEN
			
				SELECT 	Max(paid_to_date)
				INTO 		:ldtm_date
				FROM 	PAYMENT
				WHERE 	claim_no 	   			= :il_claim_no
				AND 		opening_no            	= :ll_opening_no
				AND 		zeroed_flag            	<> 'Y'
				AND       payment_type_code 	NOT IN ('R1', 'R2', 'R3');
				
				SQLCA.nf_handle_error('Embedded SQL: select from PAYMENT', 'n_openings', 'nf_check_bus_rule') 
					
				IF NOT IsNull(ldtm_date) THEN
					IF idw_dw[1].GetItemDateTime(ll_loop, 'benefit_end_date') < ldtm_date THEN
						MessageBox('Invalid Benefit End - Opening No: ' + ls_opening_no_message , 'The end date cannot be before non return to work payments that have not been fully cancelled or adjusted.  Last payment = ' + String(ldtm_date, 'YYYY/MM/DD') + '.')
						RETURN -1
					END IF
				END IF
				
				/*	check out the awards for not equal to RLOE
					6.90 	The end date must not be before the end date of any periodic awards for that opening.
				*/
 				 	IF ls_opening_type_code <> 'RLOE' THEN
						
						SELECT 	Max(award_end_date)
						INTO 		:ldtm_date
						FROM 	PERIODIC_AWARD
						WHERE	claim_no 			= :il_claim_no
						AND 		opening_no 			= :ll_opening_no
						AND 		award_type_code <> 'CA' 
						AND  		award_type_code <> 'CL'
						USING 	SQLCA;
						
						SQLCA.nf_handle_error('Embedded SQL: select from PERIODIC_AWARDS', 'n_openings', 'nf_check_bus_rule') 
						
						IF NOT IsNull(ldtm_date) THEN
							IF idw_dw[1].GetItemDateTime(ll_loop, 'benefit_end_date') < ldtm_date THEN
								MessageBox('Invalid Benefit End - Opening No: ' + ls_opening_no_message , 'The end date cannot be before any award end dates.')
								RETURN -1
							END IF
						END IF
					END IF
				END IF
				
/*				validate the dispostion code
*/
				ls_string 		= idw_dw[1].GetItemString(ll_loop, 'claim_disposition_code')
				ls_string2 	= idw_dw[1].GetItemString(ll_loop, 'receiving_ltd_code')
				ls_string3 	= idw_dw[1].GetItemString(ll_loop, 'claimant_working_flag')
				ls_string4 	= idw_dw[1].GetItemString(ll_loop, 'work_restriction_flag')
				ls_string5 	= idw_dw[1].GetItemString(ll_loop, 'opening_type_code')
	
				SELECT a.disposition_code, b.comment_required_flag
				INTO 		:ls_string6, :ls_comment_req_flag
				FROM 	Disposition_Combination a, Claim_Disposition_Type b
				WHERE 	a.opening_type_code 		= :ls_string5
				AND 		a.work_restriction_flag 	= :ls_string4
				AND 		a.working_flag 				= :ls_string3
				AND 		a.receiving_ltd_code 		= :ls_string2
				AND 		a.disposition_code 		= :ls_string
				AND 		a.disposition_code 		= b.claim_disposition_code;

				SQLCA.nf_handle_error('Embedded SQL: Select from Dispostion_Combination', 'n_openings', 'nf_check_bus_rule') 
				
				/* 6.105	The disposition must be appropriate for the opening type, work restriction indicator, claimant working indicator
				              and receiving LTD indicator . (Refer to Rationale) */
				IF IsNull(ls_string6) THEN
					MessageBox('Invalid Dispostion Code - Opening No: ' + ls_opening_no_message , 'The dispostion code is not valid for the criteria specified.')
					RETURN -1
				END IF
				
				/* 6.100 	The following information must be entered only if a benefit is ended: Last Bulleted
					·	Work restriction indicator
					·	Claimant working indicator
					·	Receiving ltd indicator
					·	Disposition 
					·	Disposition reason (depends upon the disposition chosen) *******
				*/
				IF idw_dw[1].GetItemStatus(ll_loop, 'claim_disposition_code', Primary!) <> NotModified! OR idw_dw[1].GetItemStatus(ll_loop, 'disposition_comment', Primary!) <> NotModified! THEN
					IF ls_comment_req_flag = 'Y' AND ( IsNull(idw_dw[1].GetItemString(ll_loop, 'disposition_comment')) OR Trim(idw_dw[1].GetItemString(ll_loop, 'disposition_comment')) = '') THEN
						MessageBox('Missing Comment - Opening No: ' + ls_opening_no_message , 'The disposition comment must be filled in for this disposition code.')
						RETURN -1
					END IF
				END IF
  
  				/* 6.20		The start date and end date for an opening must not overlap with or equal any other benefit period for 
				                  an opening of the same benefit category.
				 */
   	      		IF nf_check_date_overlap(ll_loop, idw_dw[1].GetItemDateTime(ll_loop, 'benefit_end_date'), ls_string) < 0 THEN
      	      		MessageBox('Error - Opening No: ' + ls_opening_no_message, 'Openings of the same category can not have overlapping dates.')
         	   		RETURN -1
	        		END IF
   	   ELSE

/*			end date is null - ie not closed
*/
  		      lb_all_closed = FALSE
				
				IF idw_dw[1].GetItemString(ll_loop, 'receiving_ltd_code') <> 'I' THEN 
					idw_dw[1].SetItem(ll_loop, 'receiving_ltd_code', 'I')
				END IF
				IF idw_dw[1].GetItemString(ll_loop, 'claimant_working_flag') <> 'I' THEN
					idw_dw[1].SetItem(ll_loop, 'claimant_working_flag', 'I')
				END IF
				IF idw_dw[1].GetItemString(ll_loop,'work_restriction_flag') <> 'I' THEN
					idw_dw[1].SetItem(ll_loop, 'work_restriction_flag', 'I')
				END IF
				IF TRIM(idw_dw[1].GetItemString(ll_loop,'claim_disposition_code')) <> '' THEN
					idw_dw[1].SetItem(ll_loop, 'claim_disposition_code', ' ')
				END IF			
		
		 /* 6.40 	There must be only one of each benefit type opened at any given time.  See Rationale. */	
   	      IF lb_ltd_open AND idw_dw[1].GetItemString(ll_loop, 'opening_type_code') = 'LTD' THEN

				/*already another open with ltd - can't have more than one*/
  		         MessageBox('Error - Opening No: ' + ls_opening_no_message , 'There is more than one opening of type LTD active. Please Correct.')
      	      	RETURN -1
          ELSE
            	IF idw_dw[1].GetItemString(ll_loop, 'opening_type_code') = 'LTD' THEN
               	lb_ltd_open = TRUE
	         END IF
   	     END IF
		
		// 6.40 		There must be only one of each benefit type opened at any given time.  See Rationale.
   	      IF lb_rloe_open AND idw_dw[1].GetItemString(ll_loop, 'opening_type_code') = 'RLOE' THEN
      	      	MessageBox('Error - Opening No: '+ ls_opening_no_message , 'There is more than one opening of type RLOE active. Please Correct.')
         	   	RETURN -1
	      ELSE
   	         IF idw_dw[1].GetItemString(ll_loop, 'opening_type_code') = 'RLOE' THEN
      	         lb_rloe_open = TRUE
         	END IF
	      END IF
		
		// 6.40 		There must be only one of each benefit type opened at any given time.  See Rationale.
	   	 IF lb_pen_open AND idw_dw[1].GetItemString(ll_loop, 'opening_type_code') = 'PEN' THEN
   	   	      MessageBox('Error - Opening No: ' + ls_opening_no_message , 'There is more than one opening of type PEN active. Please Correct.')
      	   	  RETURN -1
		 ELSE
   		      IF idw_dw[1].GetItemString(ll_loop, 'opening_type_code') = 'PEN' THEN
      		      lb_pen_open = TRUE
         	 END IF
		 END IF
		 
		 // 6.40 		There must be only one of each benefit type opened at any given time.  See Rationale.
   	      IF lb_sv_open AND idw_dw[1].GetItemString(ll_loop, 'opening_type_code') = 'SV' THEN
      	      MessageBox('Error - Opening No: ' + ls_opening_no_message , 'There is more than one opening of type SV active. Please Correct.')
         	   RETURN -1
	      ELSE
   	         IF idw_dw[1].GetItemString(ll_loop, 'opening_type_code') = 'SV' THEN
      	         lb_sv_open = TRUE
         	END IF
	      END IF
			
			// 6.40 		There must be only one of each benefit type opened at any given time.  See Rationale.
			IF lb_sv_open AND idw_dw[1].GetItemString(ll_loop, 'opening_type_code') = 'S1' THEN
      	      	MessageBox('Error - Opening No: ' + ls_opening_no_message , 'There is more than one opening of type S1 active. Please Correct.')
         	   	RETURN -1
	         ELSE
   	         	IF idw_dw[1].GetItemString(ll_loop, 'opening_type_code') = 'S1' THEN
      	         	lb_sv_open = TRUE
         	   	END IF
	         END IF
				
				// 6.40 		There must be only one of each benefit type opened at any given time.  See Rationale.
				IF lb_sv_open AND idw_dw[1].GetItemString(ll_loop, 'opening_type_code') = 'S2' THEN
      	      		MessageBox('Error - Opening No: ' + ls_opening_no_message , 'There is more than one opening of type S2 active. Please Correct.')
         	  		RETURN -1
	         	ELSE
   	         		IF idw_dw[1].GetItemString(ll_loop, 'opening_type_code') = 'S2' THEN
      	         		lb_sv_open = TRUE
         	   		END IF
	         	END IF
   	   		END IF

/*			recurrence type code
*/
  		   ls_string = idw_dw[1].GetItemString(ll_loop, 'recurrence_type_code')
			  
	       SELECT 	Count(*)
   	 	   INTO 		:ll_count
      	   FROM 		Recurrence_Type
            WHERE 	recurrence_type_code 	= :ls_string
		   AND 		active_flag 					= 'Y';
		   
		   SQLCA.nf_handle_error('Embedded SQL: Select from Recurrence_Type', 'n_openings', 'nf_check_bus_rule') 
           
		 /* 6.270 An opening must specify whether the benefit period is for administrative purposes or for a recurrence of injury
		              resulting from the accident.
		  */
		   IF ll_count < 1 THEN
	           MessageBox('Error - Opening No: ' + ls_opening_no_message , 'Invalid recurrence type code.  Please correct.')
   	           RETURN -1
      	   END IF
				
		   IF ls_string = 'R' AND ls_string <> idw_dw[1].GetItemString(ll_loop, 'recurrence_type_code',Primary!, TRUE) AND (lb_payments OR lb_benefits) THEN
				/* 	don't need to check if new because the two flags will be false */
				MessageBox('Cannot Change Recurrence Type - Opening No: ' + ls_opening_no_message, 'The recurrence type code cannot be changed from administrative to recurrence.')
				RETURN -1
		   END IF

/*			accident recurrence date if the date is changed to after the original date then cause an error if
			payment/awards/bencals exist
*/
		   ldtm_date = idw_dw[1].GetItemDateTime(ll_loop, 'accident_recurrence_date')
		   IF ldtm_date > idw_dw[1].GetItemDateTime(ll_loop, 'accident_recurrence_date', Primary!, TRUE) AND (lb_payments OR lb_benefits) THEN
				MessageBox('Cannot Change Recurrence Date - Opening No: ' + ls_opening_no_message , 'The accident recurrence date cannot be changed because payments or benefit calculations exist.')
				RETURN -1
		   END IF
			
			// P10261 An accident recurrence date can not be after an associated Job Search RTW Incentive Qualification employment start date.
			ll_selected_opening_no = idw_dw[1].GetItemNumber(ll_loop,'opening_no')
			
			IF ISNULL(ll_selected_opening_no) THEN ll_selected_opening_no = 0
			
			SetNull(ldtm_employment)
			
			Select employment_start_date
			Into    :ldtm_employment
			From  RTW_INCENTIVE_QUALIFICATION
			Where claim_no 		= :il_claim_no
			And     opening_no 	= :ll_selected_opening_no
			Using SQLCA;
		
			SQLCA.nf_handle_error("n_openings", "nf_check_bus_rules()", "dw_opening_details - Select employment_start_date From RTW_INCENTIVE_QUALIFCATION")
		
		    /* 6.400	An openings accident/ recurrence date must not be after an associated Return To Work incentive 
			             qualification employment start date. */
			IF NOT IsNull( ldtm_employment) AND DATE( ldtm_employment) <> 1900-01-01 THEN
				IF ldtm_date > ldtm_employment THEN 
					MessageBox('Cannot Change Recurrence Date - Opening No: ' + String(ll_selected_opening_no) , 'The accident recurrence date cannot be after the employment start date of the associated RTW Incentive qualification.')
					RETURN -1
				END IF
			END IF
			
			/* 6.300 The accident/recurrence date of an administrative opening, must be the same date as at least one
			             accident/recurrence date of a non-administrative opening (i.e. an accident/recurrence type opening) */
			IF nf_check_accident_recurr(ll_loop, ldtm_date, ls_string) < 0 THEN
				IF ls_string = 'A' THEN
					MessageBox('Invalid Accident Recurrence Date - Opening No: ' + String(ll_selected_opening_no) , 'There is no accident/recurrence opening with the same accident/recurrence date as this administrative opening.')
					RETURN -1
				ELSEIF ls_string = 'R' THEN /* 6.271	There must be only one accident/recurrence opening for each accident/recurrence date.*/
					MessageBox('Invalid Accident Recurrence Date - Opening No: ' + String(ll_selected_opening_no) , 'There is already a recurrence entered with the same accident/recurrence date.')
					RETURN -1
				END IF
			END IF
					
			/*	Check the three day exemption code to determine if it affects the Three Day Paid flag. The
				three day exemption code can be modified for claims with accident/recurrences on or after
				January 01, 1998 having an RLOE opening of type "Accident/Recurrence" ("R").
				The three day paid flag is NOT modified if a three day reimbursement was already issued 
				(i.e. the three day paid flag = "Y") or if the three day paid flag is blank (it is blank
				for new openings and gets set in the function nf_set_unused_fields).
				
					Three Day Exemption Code			Three Day Paid Flag
					------------------------			-------------------
					"."	Not Applicable					"N" 			3Day not Paid
					"."	Not Applicable					"Y"			3Day Paid
					"A"	Admitted to Hospital			"I" 			3Day is Inapplicable
					"R"	Recurrence within 20 days	"I"			3Day is Inapplicable
					
					6.220 	The three-day paid flag must be inapplicable if all of the following are true: 
								·	The opening is an RLOE opening
								·	The accident recurrence date is on or after January 1, 1998
								·	The three-day exemption reason is not ‘Inapplicable’

					6.171	The three-day paid flag must be inapplicable if the opening is not an RLOE opening type.

					6.230	The three-day paid flag must be inapplicable if all of the following are true:
					·	The opening is an RLOE opening 
					·	The accident recurrence date is prior to January 1, 1993 

					6.240	The three-day paid flag must not be inapplicable if all of the following are true: 
					·	The opening is an RLOE opening
					·	The accident recurrence date is on or after January 1,1995
					·	The accident recurrence date is prior to January 1, 1998

					6.253 The three-day paid flag must be ‘No’ if all of the following are true:  ???
					·	The opening is an RLOE opening
					·	The accident recurrence date is on or after January 1, 1993
					·	The accident recurrence date is prior to January 1, 1995 
					·	There is no three day payment for on opening with the same accident/recurrence date
					·	There has been no payment for any opening with the same accident/recurrence date where the benefit calculation has ‘Top Up’ in effect

					6.254	The three-day paid flag must be inapplicable if all of the following are true: 
					·	The opening is an RLOE opening
					·	The accident recurrence date is on or after January 1, 1998
					·	The exemption reason is not ‘Inapplicable’
					
					6.250 The three-day paid flag must be inapplicable if all of the following are true:   
					·	The opening is an RLOE opening
					·	The accident recurrence date is on or after January 1, 1993
					·	The accident recurrence date is prior to January 1, 1995 
					·	There is no three day payment for any opening with the same accident/recurrence date
					·	There has been a payment for an opening with the same accident/recurrence date where the benefit calculation has ‘Top Up’ in effect
			*/
			IF date(idw_dw[1].GetItemDateTime(ll_loop, 'accident_recurrence_date')) >= Date('1998/01/01') AND &
			   idw_dw[1].GetItemString(ll_loop,'recurrence_type_code') = 'R'   	AND &
				idw_dw[1].GetItemString(ll_loop,'opening_type_code') = 'RLOE' 		AND &
				(idw_dw[1].GetItemString(ll_loop, 'three_day_paid_flag') = 'I' 	OR  &
				idw_dw[1].GetItemString(ll_loop, 'three_day_paid_flag') = 'N' )	THEN
				
				IF idw_dw[1].GetItemString(ll_loop, 'three_day_exempt_code') = '.' THEN
					IF idw_dw[1].GetItemString(ll_loop,'three_day_paid_flag') <> 'N' THEN
						idw_dw[1].SetItem(ll_loop,'three_day_paid_flag','N')
					END IF
				ELSE
					IF idw_dw[1].GetItemString(ll_loop,'three_day_paid_flag') <> 'I' THEN
						idw_dw[1].SetItem(ll_loop,'three_day_paid_flag','I')
					END IF
				END IF
			END IF
	
			/* NOTE: This code does an update shouldn't be here  */
			//CPPD Reminders
			ls_selected_opening_type = idw_dw[1].GetItemString(ll_loop,'opening_type_code')			
             	IF ls_selected_opening_type = 'LTD'  THEN
				/* 6.420	All planned CPP Disability reminders for the claim must be cancelled if an LTD opening is created or Re-opened */
				/* REVISED TO: 6.420	All planned regular loss of earnings CPP Disability reminders for the claim must be cancelled,
				             if a long-term disability opening is created. (Refer to Rationale)
					: This only applies when an LTD opening is created and not when an LTD opening is re-opened by removing the opening end date.
	                  RLOE CPP Disability reminders are currently only sub-types of 6M and 24M. Follow-up reminders are not cancelled.

				*/
				// 	IF idw_dw[1].GetItemStatus(ll_loop,"opening_type_code",Primary!) = DataModified! THEN (changed from)
				IF idw_dw[1].GetItemStatus(ll_loop,0,Primary!) = NewModified! THEN
					IF nf_check_for_reminders(il_claim_no ) < 0 THEN
						RETURN -1
					END IF	
				END IF
			END IF
			
			
			ll_loop = ll_loop + 1
   	LOOP

/*		6.130 	There must be at least one open benefit if the claim status is active.
*/
  	IF is_status_code = 'A' AND lb_all_closed THEN
      	MessageBox('Error ' , 'If the claim is active there must be an opening without a benefit end date.')
	     RETURN -1
   	END IF

/*		6.140 	There must not be any benefits open if the claim status is finalled (they must all have an end date).
*/
  	IF is_status_code = 'F' and NOT lb_all_closed THEN
      	MessageBox('Error', 'All openings must be closed for the status of Final.')
	     RETURN -1
   	END IF

/*		survior  and pension openings cannot exist at the same time
		6.150 	There must only be a survivor benefit opened if the claimant is deceased.
*/	
		IF lb_pen_open AND lb_sv_open AND lb_rloe_open AND lb_ltd_open THEN
			MessageBox('Invalid Openings','No other openings can be active with Survior Benefits.')
			RETURN -1
		END IF
/*		if survior opening then there must be a death date on the claimant
*/
		SetNull(ldtm_date)
		SELECT 	death_date
		INTO 		:ldtm_date
		FROM 	INDIVIDUAL, CLAIM_PARTICIPANT
		WHERE 	CLAIM_PARTICIPANT.individual_no 			= INDIVIDUAL.individual_no
		AND 		CLAIM_PARTICIPANT.claim_no 				= :il_claim_no
		AND 		CLAIM_PARTICIPANT.claim_role_code 		= 'C'
		AND 		CLAIM_PARTICIPANT.claimant_active_flag 	= 'Y';
		
		SQLCA.nf_handle_error('Embedded SQL: Select from INDIVIDUAL, CLAIM_PARTICIPANT', 'n_openings', 'nf_check_bus_rule') 
		
		IF IsNull(ldtm_date) AND lb_sv_open THEN	
			MessageBox('Invalid Opening','A survior opening cannot be entered unless there is a death date registered for the claimant.')
			RETURN -1
		ELSE

/*		if there is a death date then there can only be sv openings
         6.150 	There must only be a survivor benefit opened if the claimant is deceased.
*/
			IF NOT IsNull(ldtm_date) AND (lb_pen_open OR lb_rloe_open OR lb_ltd_open) THEN
				MessageBox('Invalid Opening','Only openings of type SV can be entered since the claimaint is deceased.')
				RETURN -1
			END IF
		END IF

/*		check for deletes and see if ok to delete
		can't delete if benefit calculations or payments
		6.160 	An opening must not be deleted if there are benefit calculations, benefit entitlement, payments or awards created using the opening.  
		6.170 	An opening must not be deleted if the opening has action items associated with it.
*/
  		ll_max = idw_dw[1].DeletedCount()
	   ll_loop = 1
   	DO WHILE ll_loop <= ll_max
      	IF idw_dw[1].GetItemStatus(ll_loop,0, Delete!) <> New! AND idw_dw[1].GetItemStatus(ll_loop,0, Delete!) <> NewModified! THEN
	      // check the other tables
   	      ll_opening_no = idw_dw[1].GetItemNumber(ll_loop,'opening_no', DELETE!, TRUE)
	         IF nf_check_ben_calc_delete(il_claim_no, ll_opening_no) THEN
   	         	MessageBox('Error', 'Opening ' + String(ll_opening_no) + ' cannot be deleted.  Benefit calculations exist.')
      	      	RETURN -1
         	END IF
		    IF nf_check_payment_delete(il_claim_no, ll_opening_no) THEN
      	      	MessageBox('Error', 'Opening ' + String(ll_opening_no) + ' cannot be deleted.  Payments exist.')
         	   	RETURN -1
	         END IF
		    IF nf_check_for_awards(il_claim_no, ll_opening_no) THEN
      	      	MessageBox('Error', 'Opening ' + String(ll_opening_no) + ' cannot be deleted.  Awards exist.')
         	   	RETURN -1
	         END IF
		    IF nf_check_for_benefit_entitlement(il_claim_no, ll_opening_no) THEN
      	       	MessageBox('Error', 'Opening ' + String(ll_opening_no) + ' cannot be deleted.  Benefit Entitlement exists.')
         	   	RETURN -1
	         END IF
   	   END IF
      	ll_loop = ll_loop + 1
	   LOOP
	ELSE
/*		can't be any new openings if not the correct status
		all existing openings must be closed
		no changes can be made to openings - they can be deleted
*/
	 lb_benefits 			= FALSE
    	 ll_max 				= idw_dw[1].DeletedCount()
	 ll_loop 				= 1
      DO WHILE ll_loop <= ll_max
     	   IF idw_dw[1].GetItemStatus(ll_loop,0, Delete!) <> New! AND idw_dw[1].GetItemStatus(ll_loop,0, Delete!) <> NewModified! THEN
/*			check the other tables
*/
           	ll_opening_no = idw_dw[1].GetItemNumber(ll_loop,'opening_no', DELETE!, TRUE)
	         IF nf_check_ben_calc_delete(il_claim_no, ll_opening_no) OR nf_check_payment_delete(il_claim_no, ll_opening_no) OR nf_check_for_awards(il_claim_no, ll_opening_no) THEN
      	      	MessageBox('Error', 'Opening ' + String(ll_opening_no) + ' cannot be deleted.  Awards, payments or benefit calculations exist.')
         	   	RETURN -1
     		END IF
        	END IF
	      ll_loop = ll_loop + 1
      LOOP
/*		now check the main buffer
*/
	   	ll_max = idw_dw[1].RowCount()
     	ll_loop = 1
     	DO WHILE ll_loop <= ll_max
       	IF idw_dw[1].GetItemStatus(ll_loop,0, Primary!) = New! OR idw_dw[1].GetItemStatus(ll_loop,0, Primary!) = NewModified! THEN
/*			no new additions allowed
*/			
				MessageBox('Error','No openings can be added under the current status.')
				RETURN -1
			END IF
			IF idw_dw[1].GetItemStatus(ll_loop,0, Primary!) = DataModified! THEN
				MessageBox('Error','No modifications to openings allowed under the current status.')
				RETURN -1
			END IF
	      ll_loop = ll_loop + 1
      LOOP
	END IF

RETURN 0
end function

public function boolean nf_check_for_benefit_entitlement (long al_claim_no, long al_opening_no);LONG	ll_count

	IF NOT IsNull(al_opening_no) THEN
		SELECT Count(*)
		  INTO :ll_count
		  FROM BENEFIT_ENTITLEMENT
		 WHERE claim_no = :al_claim_no
	   	AND opening_no = :al_opening_no;

		IF SQLCA.nf_handle_error('Embedded SQL: Select from BENEFIT_ENTITLEMENT', 'n_openings', 'nf_check_for_benefit_entitlement') < 0 THEN
			Return FALSE
		END IF
		IF ll_count > 0 THEN
			Return TRUE
		END IF
	END IF
Return FALSE
end function

public function integer nf_check_for_reminders (long al_claim_no);LONG	ll_count
Date  ldt_date
/*
 6.420	All planned CPP Disability reminders for the claim must be cancelled if an LTD opening is created or Re-opened 
REVISED TO: 6.420	All planned regular loss of earnings CPP Disability reminders for the claim must be cancelled,
				  if a long-term disability opening is created. (Refer to Rationale)
: This only applies when an LTD opening is created and not when an LTD opening is re-opened by removing the opening end date.
   RLOE CPP Disability reminders are currently only sub-types of 6M and 24M. Follow-up reminders are not cancelled.

#	Scenario	Expected Results	Pass/Fail	Note
1	Select a claim with planned CPPD reminders. Create an LTD opening, and save.	All planned CPPD Reminders are cancelled.		
2	Select a claim with completed or cancelled reminders. Create an LTD opening and save.	No change, reminder statuses remain the same.		
3	Select a claim with a planned CPPD reminder. Open a closed LTD opening.	No change, reminder statuses remain the same.	
4	Select a claim with a planned CPPD reminder. Close a RLOE opening and save.	No change. 		
*/

// Set RLOE reminders to cancelled if a new LTD opening is created - need to see where this is called from 
// it appears to only be applicable for a newly created one as per the BR spec.
SELECT	Count(*)
INTO 		:ll_count
FROM 	CLAIM_REMINDER
WHERE   claim_no 						= :al_claim_no
AND  	   	reminder_type_code 			= 'CPPD'
AND  	   	reminder_sub_type_code 	IN ('6M','24M')
AND        reminder_status_code 		= 'P'
USING 	SQLCA;
	
SQLCA.nf_handle_error('Embedded SQL: Select CLAIM_REMINDER', 'n_openings', 'nf_check_for_reminder') 

IF ll_count <= 0 THEN  RETURN 0

//DO THE UPDATE WHERE APPLICABLE
ldt_date = DATE(f_server_datetime())	
		
UPDATE  CLAIM_REMINDER
SET 	    reminder_status_code = 'X', closed_date = :ldt_date 
WHERE   claim_no 						= :al_claim_no
AND  	   	reminder_type_code 			= 'CPPD'
AND  	   	reminder_sub_type_code 	IN ('6M','24M')
AND        reminder_status_code 		= 'P'
USING SQLCA;
	
SQLCA.nf_handle_error('Embedded SQL: Update CLAIM REMINDER', 'n_openings', 'nf_check_for_reminder')

RETURN 0
end function

on n_openings.create
call super::create
end on

on n_openings.destroy
call super::destroy
end on

