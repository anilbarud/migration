$PBExportHeader$n_goals_objectives.sru
forward
global type n_goals_objectives from n_pdc
end type
end forward

global type n_goals_objectives from n_pdc
end type
global n_goals_objectives n_goals_objectives

type variables
LONG il_claim_no
end variables

forward prototypes
public subroutine nf_set_claim_no (long al_claim_no)
public function integer nf_retrieve_goal (integer ai_goal_no)
public function integer nf_retrieve_objective (integer ai_goal_no, integer ai_objective_no)
public function integer nf_change_item (long al_datawindow)
public function long nf_set_identifiers ()
public function integer nf_check_bus_rule ()
public function integer nf_insert (long al_row)
public function integer nf_insert_objective (long al_row)
public function integer nf_check_mandatory ()
public function integer nf_get_next_goal_no ()
public function integer nf_get_next_objective_no (integer ai_goal_no)
public function integer nf_set_defaults (integer ai_datawindow)
public function integer nf_delete_goal_objectives ()
public function integer nf_check_outcomes ()
end prototypes

public subroutine nf_set_claim_no (long al_claim_no);	
il_claim_no = al_claim_no

end subroutine

public function integer nf_retrieve_goal (integer ai_goal_no);/* Retrieve goal
*/

LONG  ll_rows

ll_rows = idw_dw[1].Retrieve(il_claim_no, ai_goal_no)
SQLCA.nf_handle_error('Retrieve Goal','n_goal_objective','nf_retrieve_goal') 

IF ll_rows < 1 THEN
	MessageBox('Retrieval Error','Unable to retrieve requested goal.')
	RETURN -1
END IF

IF ll_rows = 0 THEN
	idw_dw[1].InsertRow(0)
	idw_dw[2].InsertRow(0)
END IF
   
RETURN ll_rows

end function

public function integer nf_retrieve_objective (integer ai_goal_no, integer ai_objective_no);/* Retrieve objective
*/

LONG  ll_rows

ll_rows = idw_dw[2].Retrieve(il_claim_no, ai_goal_no, ai_objective_no)
SQLCA.nf_handle_error('Retrieve Objective','n_goal_objective','nf_retrieve_objective') 

IF ll_rows < 1 THEN
	MessageBox('Retrieval Error','Unable to retrieve requested objective.')
	RETURN -1
END IF

IF ll_rows = 0 THEN
	idw_dw[2].InsertRow(0)
END IF

IF NOT(IsNull(idw_dw[2].GetItemDateTime(1,'revised_objective_date'))) THEN
	idw_dw[2].setItem(1,'revised_objective_flag','Y')
END IF
   
RETURN ll_rows

end function

public function integer nf_change_item (long al_datawindow);/*	This function runs the code for the itemchanged event for the appropriate datawindow.
*/
	LONG		ll_current_row
	DATETIME	ldt_null_datetime
	STRING	ls_revised_objective_flag
	
	SetNull(ldt_null_datetime)
		  
	ll_current_row = idw_dw[al_datawindow].GetRow()
	CHOOSE CASE	al_datawindow
		CASE	1

/*	This is for the datawindow idw_dw[1] - which is the goal
*/
   		CHOOSE CASE idw_dw[al_datawindow].GetColumnName()				
				CASE	''
					
			END CHOOSE
			
/*	This is for the datawindow idw_dw[2] - which is the objective
*/
			
		CASE	2
			CHOOSE CASE idw_dw[al_datawindow].GetColumnName()	
				CASE ''
				
			END CHOOSE
	END CHOOSE
RETURN 0
end function

public function long nf_set_identifiers ();/*	This function is used to set all the identifiers for the updateable datawindows.
*/
	LONG		ll_rowsindw, ll_loopcount, ll_count
	INTEGER	li_goal_no, li_objective_no
	
/*	Call function to determine next goal number if the  REHAB_GOAL record is new.
*/
	IF	idw_dw[1].RowCount() > 0 THEN
		IF idw_dw[1].GetItemNumber(1,'goal_no') = 0 OR IsNull(idw_dw[1].GetItemNumber(1,'goal_no')) THEN
			li_goal_no = nf_get_next_goal_no()
			IF li_goal_no < 0 THEN
				RETURN -1
			ELSE
				idw_dw[1].SetItem(1,'goal_no',li_goal_no)
				idw_dw[1].Setitem(1,'claim_no',il_claim_no)
			END IF
		ELSE
			li_goal_no = idw_dw[1].GetItemNumber(1,'goal_no')
		END IF
	END IF

/*	Call function to determine next objective number if the REHAB_OBJECTIVE record is new.
*/
	IF	idw_dw[2].RowCount() > 0 THEN
		IF idw_dw[2].GetItemNumber(1,'objective_no') = 0 OR IsNull(idw_dw[2].GetItemNumber(1,'objective_no')) THEN
			li_objective_no = nf_get_next_objective_no(li_goal_no)
			IF li_objective_no < 0 THEN
				RETURN -1
			ELSE
				idw_dw[2].SetItem(1,'objective_no',li_objective_no)
				
/*	Also set the goal and claim numbers for the new record.
*/
				idw_dw[2].SetItem(1,'goal_no',li_goal_no)
				idw_dw[2].Setitem(1,'claim_no',il_claim_no)
			END IF
		ELSE
			li_objective_no = idw_dw[2].GetItemNumber(1,'objective_no')
		END IF
	END IF

RETURN 0
end function

public function integer nf_check_bus_rule ();DATETIME ldt_null_datetime
DATE ld_today

SetNull(ldt_null_datetime)
ld_today = Date(f_server_datetime())


/*	Revised Objective Flag is not applicable when the outcome is  "Achieved/Successful" 
*/
IF idw_dw[2].GetItemString(1,'revised_objective_flag') ='Y' AND idw_dw[2].GetItemString(1,'outcome_code') = '01' THEN
	MessageBox('Revised Objective Error',"Can't revise an objective when the outcome has been achieved/successful")
	RETURN -1
END IF

/*	Outcome date is set to current date if an outcome is entered
*/
IF idw_dw[2].GetItemString(1,'outcome_code') <> idw_dw[2].GetItemString(1,'outcome_code',Primary!,TRUE) THEN 
	IF idw_dw[2].GetItemString(1,'outcome_code')='' THEN
		idw_dw[2].SetItem(1,'outcome_date',ldt_null_datetime)
	ELSE
		idw_dw[2].SetItem(1,'outcome_date',ld_today)
	END IF
ELSE
//  Test if outcome date set for a new goal with new objective & outcome code.  If date still null,
//  set outcome date to server date - EMcDermott, April 7/'98
	IF idw_dw[2].GetItemString(1,'outcome_code') <>'' THEN
		idw_dw[2].SetItem(1,'outcome_date',ld_today)
	END IF
END IF

/* Set the revised objective date to the current date when the Objective has been revised 
*/
IF idw_dw[2].GetItemString(1,'revised_objective_flag') <> idw_dw[2].GetItemString(1,'revised_objective_flag',Primary!,TRUE) THEN
	IF idw_dw[2].GetItemString(1,'revised_objective_flag') ='N' THEN
		idw_dw[2].SetItem(1,'revised_objective_date',ldt_null_datetime)
	ELSE
		idw_dw[2].SetItem(1,'revised_objective_date',ld_today)	
	END IF
END IF


RETURN 0

end function

public function integer nf_insert (long al_row);/* When inserting a new goal, reset both the goal and objective datawindows
	to ensure that at least one objective is entered for the new goal
*/
	
	idw_dw[1].Reset()
	idw_dw[1].InsertRow(al_row)
	nf_set_defaults(1)
	
	nf_insert_objective(al_row)
	nf_set_defaults(2)
	
	RETURN 0
	
	
end function

public function integer nf_insert_objective (long al_row);
	idw_dw[2].Reset()
	idw_dw[2].InsertRow(al_row)

	nf_set_defaults(2)

	RETURN 0
end function

public function integer nf_check_mandatory ();IF IsNull(idw_dw[1].GetItemString(1,'goal_code')) THEN
	MessageBox('Goal Error','Goal code must be entered')
	RETURN -1
END IF

IF IsNull(idw_dw[1].GetItemString(1,'rehab_type_code')) THEN
	MessageBox('Goal Error','Rehab type code must be entered')
	RETURN -1
END IF

IF IsNull(idw_dw[2].GetItemString(1,'objective_code')) THEN
	MessageBox('Objective Error','Objective code must be entered')
	RETURN -1
END IF

RETURN 0
end function

public function integer nf_get_next_goal_no ();/* This function is used to determine the next goal number for a claim. 
	Arguments:	none
	Return:		integer (goal_no)
*/
	INTEGER	li_next_goal_no

	SELECT Max(goal_no)
	  INTO :li_next_goal_no
	  FROM REHAB_GOAL
	 WHERE claim_no = :il_claim_no
	 USING SQLCA;
	
	SQLCA.nf_handle_error("Embedded SQL: SELECT Max(goal_no)","n_goals_objectives","nf_get_next_goal_no().")

	IF IsNull(li_next_goal_no) THEN
		li_next_goal_no = 1
	ELSE
		li_next_goal_no ++
	END IF

	RETURN li_next_goal_no

end function

public function integer nf_get_next_objective_no (integer ai_goal_no);/* This function is used to determine the next objective number for a goal within a claim.
	Argument:	ai_goal_no (goal number for the claim)
	Return:		integer (objective_no)
*/
	INTEGER	li_next_objective_no

	SELECT Max(objective_no)
	  INTO :li_next_objective_no
	  FROM REHAB_OBJECTIVE
	 WHERE claim_no = :il_claim_no
	 	AND goal_no = :ai_goal_no
	 USING SQLCA;
	
	SQLCA.nf_handle_error("Embedded SQL: SELECT Max(objective_no)","n_goals_objectives","nf_get_next_objective_no().") 

	IF IsNull(li_next_objective_no) THEN
		li_next_objective_no = 1
	ELSE
		li_next_objective_no ++
	END IF

	RETURN li_next_objective_no

end function

public function integer nf_set_defaults (integer ai_datawindow);/* Set defaults for the datawindow specified
	Arguments:	ai_datawindow 
	Return:		integer
*/
	DATETIME	ldtm_null_date
	
	SetNull(ldtm_null_date)
	CHOOSE CASE ai_datawindow
		CASE 1
			
		CASE 2
			idw_dw[2].Setitem(1,'objective_no',0)
			idw_dw[2].Setitem(1,'outcome_code','')
			idw_dw[2].Setitem(1,'outcome_date',ldtm_null_date)
			idw_dw[2].Setitem(1,'revised_objective_date',ldtm_null_date)
			idw_dw[2].Setitem(1,'revised_objective_flag','N')
			
		CASE ELSE
			

	END CHOOSE

	RETURN 0
end function

public function integer nf_delete_goal_objectives ();RETURN 0
end function

public function integer nf_check_outcomes ();/*	If a goal already exists for the claim, ensure that all objectives 
	for other goals have been completed/cancelled
*/

LONG ll_count

	SELECT count(*) 
	INTO :ll_count
	FROM REHAB_OBJECTIVE
	WHERE claim_no = :il_claim_no
	AND	outcome_code = ' '
	USING SQLCA;
	
	SQLCA.nf_handle_error('Embedded SQL: SELECT from REHAB_OBJECTIVE','n_goals_objectives','nf_check_outcomes') 

	IF ll_count > 0 THEN
		MessageBox ("Rehab Goal","All objectives for all goals must be completed/cancelled before adding a goal")
		RETURN -1
	END IF
	
	RETURN 0
	
end function

on n_goals_objectives.create
call super::create
end on

on n_goals_objectives.destroy
call super::destroy
end on

