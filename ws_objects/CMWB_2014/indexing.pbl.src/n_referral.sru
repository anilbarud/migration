$PBExportHeader$n_referral.sru
forward
global type n_referral from nonvisualobject
end type
end forward

global type n_referral from nonvisualobject
end type
global n_referral n_referral

type variables

N_GOALS_OBJECTIVES 	inv_goals_objectives
N_AUTHORIZATIONS inv_authorizations

DATASTORE  ids_rehab_task

end variables

forward prototypes
public function long nf_get_next_referral_no ()
public function long nf_update_referral (long al_claim_no, long al_provider_no, string as_provider_type_code, long al_docid, datetime adtm_referred_on_date, string as_service_code, long al_task_no)
public function integer nf_check_for_task (long al_claim_no, string as_service)
public function integer nf_check_referral_date (long al_claim_no, string as_service, date ldt_accident_date)
public function integer nf_create_rehab_plan (long al_claim_no, string as_claim_status_code, string as_claim_status_type_code)
public function integer nf_create_rehab_tasks (long al_claim_no, string as_service_code, string as_program_code, string as_task_type, string as_task_sub_type, integer al_opening_no, date adt_start_date, ref integer al_task_no)
public function integer nf_check_for_auto_task (long al_claim_no, string as_service_code)
public function integer nf_check_rehab_task (long al_claim_no, string as_service, string as_program)
public function integer nf_check_for_a_valid_referral (long al_claim_no, string as_service, ref long al_referral_no)
public function integer nf_check_other_services (long al_claim_no)
public function integer nf_delete_referral (long al_claim_no, long al_docid)
public function integer nf_validate_task_status (long al_referral_no)
public function integer nf_create_rehab_task_auths (integer al_task_no, long al_claim_no, string as_service_code, string as_program_code)
public function integer nf_update_task_on_referral (long al_rehab_referral_no, integer al_task_no)
public function integer nf_check_physio_task_not_assigned (long al_claim_no)
public function integer nf_update_provider_on_direct_referral (long al_claim_no, integer ai_task_no, long al_provider_no, string as_provider_type)
public function long nf_insert_referral (long al_claim_no, long al_provider_no, string as_provider_type_code, long al_docid, datetime adtm_referred_on_date, string as_service_code, long al_task_no, ref long al_referral_no)
public function integer nf_create_direct_referral (long al_claim_no, long al_provider_no, string as_provider_type_code, long al_docid, datetime adtm_referred_on_date, string as_service_code, long al_task_no, long al_direct_referral_no)
end prototypes

public function long nf_get_next_referral_no ();// This function gets the next/last referral no from the "Last_Referral_No" Table.
//
// Returns referral no if successful, -1 if not successful


long	ll_last_referral_no

SQLCA.nf_begin_transaction()

UPDATE	Last_Rehab_Referral_No
SET		last_rehab_referral_no = last_rehab_referral_no + 1
USING	SQLCA;

If SQLCA.nf_handle_error("Embedded SQL Update","nf_get_next_referral_no","updating Last_Referral_No") < 0 	or SQLCA.SQLnRows <> 1 then
	SQLCA.nf_rollback_transaction()
	MessageBox("APPLICATION ERROR","Error updating last_referral_no." &
	+"~n~r~n~r CONTACT THE HELP DESK",StopSign!)
	Return -1
END IF
		
SELECT	last_rehab_referral_no
INTO		:ll_last_referral_no
FROM		Last_Rehab_Referral_No
USING    SQLCA;


If SQLCA.nf_handle_error("Embedded SQL Select","nf_get_next_referral_no","On select Of Last_Referral_No")  <> 0 THEN	
	MessageBox("APPLICATION ERROR","Error Retrieving last referral no." &
					+"~n~r~n~rCONTACT THE HELP DESK",StopSign!)
	Return -1
END IF


If SQLCA.nf_commit_transaction()  <> 0 THEN Return -1

RETURN ll_last_referral_no
end function

public function long nf_update_referral (long al_claim_no, long al_provider_no, string as_provider_type_code, long al_docid, datetime adtm_referred_on_date, string as_service_code, long al_task_no);long ll_claim_no, ll_rehab_referral_no, ll_referring_provider_no, ll_docid, ll_task_no, ll_next_referral_no, ll_count
string ls_referring_provider_type_code, ls_rehab_service_code, ls_task_status_code
datetime	ldtm_referred_on_date


//Check to see if the claim no changed if it did then we have to check to see if there are tasks associated with that referral.

Select claim_no, task_no
Into    :ll_claim_no, :ll_task_no
From REHAB_REFERRAL
Where doc_id =  :al_docid
Using SQLCA;

SQLCA.nf_handle_error('n_referral','nf_update_referral','Select From REHAB_REFERRAL')

IF ll_claim_no <> al_claim_no THEN // the claim no changed on the document check for an associated task
	IF ll_task_no > 0 THEN // there is a task
        //check for the status on the task.
		SELECT task_status_code
		INTO    :ls_task_status_code
		FROM   REHAB_TASK
		WHERE claim_no = :al_claim_no
		and       task_no = :al_task_no
		USING   SQLCA;
		
		SQLCA.nf_handle_error('n_referral','nf_update_referral','Select From REHAB_TASK')
		
		// if the task status is pending change the status to cancelled and then delete it.
		  IF ls_task_status_code = '01' THEN
			
			UPDATE REHAB_TASK
			SET       task_status_code = '03', comment = 'Cancelled - Referral in error.'
			WHERE  claim_no = :al_claim_no
			AND       task_no =  :al_task_no
			USING    SQLCA;
			
			SQLCA.nf_handle_error('n_referral','nf_update_referral','UPDATE From REHAB_TASK')
    	END IF
	END IF
END IF


// Update the REHAB_REFERRAL table if the referral flag is checked on.

UPDATE REHAB_REFERRAL
Set referred_on_date = :adtm_referred_on_date, rehab_service_code = :as_service_code ,referring_provider_no = :al_provider_no,referring_provider_type_code = :as_provider_type_code, claim_no = :al_claim_no
Where doc_id = :al_docid
Using SQLCA;

SQLCA.nf_handle_error('n_referral','nf_update_referral','Insert From REHAB_REFERRAL')


Return 1
end function

public function integer nf_check_for_task (long al_claim_no, string as_service);long ll_count

// Returns -1 if a task has already been created.


Select count(*)
Into    :ll_count
From  REHAB_REFERRAL
Where claim_no = :al_claim_no
and     rehab_service_code = :as_service
and     task_no <> 0 
Using SQLCA;

SQLCA.nf_handle_error('n_referral','nf_check_for_task','Select from REHAB_REFERRAL')



IF ll_count > 0 THEN 
	RETURN -1
ELSE
	RETURN 1
END IF 


end function

public function integer nf_check_referral_date (long al_claim_no, string as_service, date ldt_accident_date);long ll_max, ll_loop
Boolean lb_date
Date ldt_referral_date

DATASTORE  lds_rehab_referral

lds_rehab_referral = CREATE DATASTORE
lds_rehab_referral.DataObject = 'ds_rehab_referral'
lds_rehab_referral.SetTransObject(SQLCA)


lds_rehab_referral.Retrieve(al_claim_no, as_service)
SQLCA.nf_handle_error('n_referral','nf_check_referral_date','lds_rehab_referal.Retrieve')

ll_max = lds_rehab_referral.RowCount()
ll_loop = 1
lb_date = FALSE
DO WHILE ll_loop <= ll_max
	ldt_referral_date = Date(lds_rehab_referral.getitemdatetime(ll_loop,'referred_on_date'))
	IF DaysAfter(ldt_referral_date,ldt_accident_date) <= 42 and  ldt_referral_date >= ldt_accident_date THEN
		lb_date = TRUE
	END IF
	ll_loop = ll_loop + 1
LOOP


IF lb_date = TRUE THEN
	Return 1 // there is a referral that is  
ELSE
	Return -1
END IF
	

end function

public function integer nf_create_rehab_plan (long al_claim_no, string as_claim_status_code, string as_claim_status_type_code);long ll_count
int li_objective_no, li_goal_no
string ls_goal_code, ls_objective_code, ls_rehab_type_code

inv_goals_objectives = Create n_goals_objectives


//Check for a Rehab Plan if there isn't one then create it.

Select count(*)
Into    :ll_count
From REHAB_GOAL
Where claim_no = :al_claim_no
Using SQLCA;

SQLCA.nf_handle_error('n_referral','nf_create_rehab_plan','Select from REHAB_GOAL')


IF ll_count > 0 THEN
	Return -1 //already has a goal created
END IF

inv_goals_objectives.nf_set_claim_no(al_claim_no)
li_goal_no = inv_goals_objectives.nf_get_next_goal_no()
li_objective_no = 1


IF as_claim_status_code = 'F' and as_claim_status_type_code = '04' Then   //Final No Lost Time
	ls_goal_code= '04' //to stay at work
	ls_objective_code = '09'  //by minimizing the impact of injury
ELSE
	ls_goal_code = '01' //return to same occupation
	ls_objective_code = '06'	//by achieving ability to perform all job	
END IF

ls_rehab_type_code = '01' //new claim

Insert REHAB_GOAL
(claim_no, goal_no, goal_code, rehab_type_code)
Values(:al_claim_no, :li_goal_no, :ls_goal_code, :ls_rehab_type_code)
Using SQLCA;

SQLCA.nf_handle_error('n_referral','nf_create_rehab_plan','Insert into REHAB_GOAL')

Insert REHAB_OBJECTIVE
(claim_no, goal_no, objective_no, objective_code, outcome_code)
Values(:al_claim_no, :li_goal_no, :li_objective_no, :ls_objective_code, '')
Using SQLCA;

SQLCA.nf_handle_error('n_referral','nf_create_rehab_plan','Insert into REHAB_OBJECTIVE')


Return 1
end function

public function integer nf_create_rehab_tasks (long al_claim_no, string as_service_code, string as_program_code, string as_task_type, string as_task_sub_type, integer al_opening_no, date adt_start_date, ref integer al_task_no);long ll_task_no,ll_row, ll_claim_no
string ls_service, ls_program_code, ls_task_success_code_required, ls_task_success_code
Date ldt_completion_date

ids_rehab_task = CREATE DATASTORE
ids_rehab_task.DataObject = 'ds_rehab_task2'
ids_rehab_task.SetTransObject(SQLCA)


ll_claim_no = al_claim_no

//get the next task_no
Select max(task_no)
Into    :ll_task_no
From  REHAB_TASK
Where claim_no = :al_claim_no
Using SQLCA;

SQLCA.nf_handle_error('n_referral','nf_create_rehab_tasks','Select max(task_no) from REHAB_TASK')


SELECT success_code_required
INTO :ls_task_success_code_required
FROM Task_Type
WHERE task_type_code = :as_task_type
USING SQLCA;

SQLCA.nf_handle_error('n_referral','nf_create_rehab_tasks','Select success_code_required from Task_Type')

IF ls_task_success_code_required = 'N' THEN
	ls_task_success_code = 'I'
ELSE
	ls_task_success_code = 'X'
END IF

If IsNull(ll_task_no) Then ll_task_no= 0

ll_task_no = ll_task_no + 1

ldt_completion_date = RelativeDate(adt_start_date, 56)

ll_row = ids_rehab_task.InsertRow(0)
ids_rehab_task.setrow(ll_row)

ids_rehab_task.setitem(ll_row,'claim_no',ll_claim_no)
ids_rehab_task.setitem(ll_row,'task_no',ll_task_no)

ids_rehab_task.setitem(ll_row,'task_type_code',as_task_type)
ids_rehab_task.setitem(ll_row,'task_sub_type_code',as_task_sub_type)
ids_rehab_task.setitem(ll_row,'task_specific_code','.')

ids_rehab_task.setitem(ll_row,'task_status_code','01')
ids_rehab_task.setitem(ll_row,'task_success_code',ls_task_success_code) // 'I' when task success required flag is No, and 'X' when the flag is yes

ids_rehab_task.setitem(ll_row,'responsible_user_id', vgst_user_profile.user_id)
ids_rehab_task.setitem(ll_row,'comment',"Initial Set")
ids_rehab_task.setitem(ll_row,'auto_created_flag','Y')

ids_rehab_task.setitem(ll_row,'planned_start_date',adt_start_date)
ids_rehab_task.setitem(ll_row,'planned_completion_date',RelativeDate(adt_start_date, 56))  //8 weeks from the accident date

ids_rehab_task.setitem(ll_row,'opening_no',al_opening_no)

ids_rehab_task.setitem(ll_row,'rehab_service_code',as_service_code)
ids_rehab_task.setitem(ll_row,'rehab_program_code',as_program_code)
ids_rehab_task.setitem(ll_row,'expedited_service_flag','N')

ids_rehab_task.Update()
SQLCA.nf_handle_error('n_referral','nf_create_rehab_tasks','lds_rehab_task.Update()')


al_task_no = ll_task_no //passed by ref


Return 1
end function

public function integer nf_check_for_auto_task (long al_claim_no, string as_service_code);long ll_count


Select count(*)
Into  :ll_count
From REHAB_TASK a
Join REHAB_REFERRAL b on a.claim_no = b.claim_no and a.task_no = b.task_no
Where a.auto_created_flag = 'Y'
and   b.rehab_service_code = :as_service_code
And   b.claim_no = :al_claim_no
Using SQLCA;

SQLCA.nf_handle_error('n_referral','nf_check_for_auto_task','Select from REHAB_TASK 01')

IF ll_count > 0 THEN 
	Return 1
Else
	Return 0
END IF
end function

public function integer nf_check_rehab_task (long al_claim_no, string as_service, string as_program);long ll_count

// Returns -1 if a task has already been created.


Select count(*)
Into    :ll_count
From REHAB_TASK
Where claim_no = :al_claim_no
and    rehab_service_code = :as_service
and    rehab_program_code = :as_program
Using SQLCA;

SQLCA.nf_handle_error('n_referral','nf_check_for_task','Select from REHAB_REFERRAL')



IF ll_count > 0 THEN 
	RETURN -1
ELSE
	RETURN 1
END IF 


end function

public function integer nf_check_for_a_valid_referral (long al_claim_no, string as_service, ref long al_referral_no);

//Check to see if this claim has a physio referral

long ll_count, ll_task_no, ll_referral_no
datetime ldtm_referred_on_date


Select count(*)
Into    :ll_count
From  REHAB_REFERRAL a
Where a.rehab_service_code = :as_service
And  a.claim_no = :al_claim_no
And exists (Select *
                From CLAIM c
                Where c.claim_no = a.claim_no 
                And   c.accident_date <= a.referred_on_date)
And DATEDIFF(day, a.referred_on_date, getdate()) <= 42
Using SQLCA;

SQLCA.nf_handle_error('n_referral','nf_check_for_referral','Select from REHAB_REFERRAL')

IF ll_count  < 1 Then
	 al_referral_no = 0
	Return -1
Else
	//the physiotherapy referral date is greater than or equal to the accident date and less than or equal to six weeks from the current date
	
	Select Min(a.rehab_referral_no)
	Into   :ll_referral_no
	From REHAB_REFERRAL a
	Where a.rehab_service_code =  :as_service
	And  claim_no = :al_claim_no
	and exists (Select *
					From CLAIM c
					Where c.claim_no = a.claim_no 
					And   c.accident_date <= a.referred_on_date)
	And DATEDIFF(day, a.referred_on_date, getdate()) <= 42
	Using SQLCA;

    SQLCA.nf_handle_error('n_referral','nf_check_for_referral','Select from REHAB_REFERRAL')
	 
	IF IsNull(ll_referral_no) THEN
		 al_referral_no = 0
	ELSE
		 al_referral_no = ll_referral_no
	END IF
	
	Return 1
End If


end function

public function integer nf_check_other_services (long al_claim_no);long ll_count

// the claim does not have a rehab task for Acupuncture , Athletic Therapy, Back management, Chiropractic, Massage Therapy, Occ Rehab Level 1, Occ Rehab Level 2,
// Work Hardening, Work Recovery, and Spinal Cord Rehabilitation


Select count(*)
Into    :ll_count
From  REHAB_TASK
Where claim_no = :al_claim_no
And   (rehab_service_code IN ('S001','S004','S006','S010') OR 
task_sub_type_code  IN ('088','146','147','096','097','094')) 
And task_status_code IN('01','02') 
Using SQLCA;

SQLCA.nf_handle_error('n_referral','nf_check_other_services','Select from REHAB_TASK')

IF ll_count > 0 THEN  // a rehab task exists
	Return -1
ELSE
	Return 1
END IF
end function

public function integer nf_delete_referral (long al_claim_no, long al_docid);long ll_claim_no, ll_rehab_referral_no, ll_referring_provider_no, ll_docid, ll_task_no, ll_next_referral_no, ll_count
string ls_referring_provider_type_code, ls_rehab_service_code, ls_task_status_code
datetime	ldtm_referred_on_date


SELECT task_no , rehab_referral_no
INTO    :ll_task_no, :ll_rehab_referral_no
From    REHAB_REFERRAL
WHERE doc_id = :al_docid
and      claim_no = :al_claim_no
USING  SQLCA;

SQLCA.nf_handle_error('n_referral','nf_update_referral','Select From REHAB_REFERRAL')

IF ll_task_no <> 0 THEN
	
	SELECT task_status_code
	INTO    :ls_task_status_code
	FROM   REHAB_TASK
	WHERE claim_no = :al_claim_no
		AND  task_no = :ll_task_no
	USING  SQLCA;
	
	SQLCA.nf_handle_error('n_referral','nf_update_referral','Select From REHAB_TASK')
	
	// if the task status is pending change the status to cancelled and then delete it.
     IF ls_task_status_code = '01' THEN
		
		UPDATE REHAB_TASK
		SET       task_status_code = '03', comment = 'Cancelled - Referral in error.'
		WHERE  claim_no = :al_claim_no
		AND       task_no = :ll_task_no
		USING  SQLCA;
		
		SQLCA.nf_handle_error('n_referral','nf_delete_referral','UPDATE From REHAB_TASK')
		
	END IF
	
END IF

// Delete the REHAB_REFERRAL table.

DELETE FROM REHAB_REFERRAL
WHERE doc_id = :al_docid
USING  SQLCA;

SQLCA.nf_handle_error('n_referral','nf_update_referral','Insert From REHAB_REFERRAL')

// A trigger inserts the REHAB_REFERRAL record into the REHAB_REFERRAL_DELETED table when it's deleted.


Return 1
end function

public function integer nf_validate_task_status (long al_referral_no);long ll_task_no, ll_claim_no
string ls_task_status_code

//	Check the associated rehab task status is in Inprogress or Completed

Select task_no, claim_no
Into   :ll_task_no, :ll_claim_no
From  REHAB_REFERRAL
Where rehab_referral_no = :al_referral_no
Using  SQLCA;

SQLCA.nf_handle_error('n_referral','nf_validate_task_status','Select From REHAB_REFERRAL')

//check for the status on the task.
SELECT task_status_code
INTO    :ls_task_status_code
FROM   REHAB_TASK
WHERE claim_no = :ll_claim_no
and       task_no = :ll_task_no
USING   SQLCA;
	
SQLCA.nf_handle_error('n_referral','nf_validate_task_status','Select From REHAB_TASK')
	
 IF  ls_task_status_code = '02' OR ls_task_status_code = '04' THEN
	//error cannot change for 	02 - in progress or 04 - completed statuses
	Return -1
END IF

RETURN 0
end function

public function integer nf_create_rehab_task_auths (integer al_task_no, long al_claim_no, string as_service_code, string as_program_code);long ll_rows, ll_auth_no, ll_billable_xref_no, ll_count, li_row_no
int li_row
string ls_flag, ls_authorization_comment
decimal ldec_max_authorized_amount, ldec_auth_qty
datetime ldtm_today

ldtm_today = f_server_datetime()

inv_authorizations = Create n_authorizations


DATASTORE  lds_rehab_task_auth
DATASTORE  lds_billable_item_xref

lds_rehab_task_auth = CREATE DATASTORE
lds_rehab_task_auth.DataObject = 'ds_rehab_task_authorization2'
lds_rehab_task_auth.SetTransObject(SQLCA)

lds_billable_item_xref = CREATE DATASTORE
lds_billable_item_xref.DataObject = 'ds_billable_item_rehab_task_xref'
lds_billable_item_xref.SetTransObject(SQLCA)

lds_billable_item_xref.Retrieve(as_service_code, as_program_code)   // 'S022', 'P001' fpr primary physio referral, and 'S022', 'P006' for direct referral
SQLCA.nf_handle_error('n_referral','nf_create_rehab_task_auths','lds_billable_item_xref.Retrieve')

ll_rows = lds_billable_item_xref.rowcount()

FOR li_row = 1 to ll_rows

	//get a new authorization no each time
 	ll_auth_no = inv_authorizations.nf_get_next_authorization_no()
	 
    ll_billable_xref_no = lds_billable_item_xref.getitemnumber(li_row,'billable_xref_no')
	ldec_max_authorized_amount =   lds_billable_item_xref.getitemnumber(li_row,'max_authorized_amount')
	 
	
	//Change the authorized_qty for Physio Trestment it should be qty less 1
	IF  lds_billable_item_xref.getitemstring(li_row,'task_type_code') = 'TR' and lds_billable_item_xref.getitemstring(li_row,'task_sub_type_code') = '031' &
	   AND  lds_billable_item_xref.getitemnumber(li_row,'billable_item_no') = 173 AND Date(ldtm_today) >= Date(gdtm_ephysio_implementation_date)  THEN
			ldec_auth_qty = lds_billable_item_xref.getitemnumber(li_row,'default_authorized_qty' ) - 1
	ELSE
    		ldec_auth_qty = lds_billable_item_xref.getitemnumber(li_row,'default_authorized_qty' )	
	END IF
	
	IF  lds_billable_item_xref.getitemstring(li_row,'task_type_code') = 'TR' and lds_billable_item_xref.getitemstring(li_row,'task_sub_type_code') = '031' &
	   AND  lds_billable_item_xref.getitemnumber(li_row,'billable_item_no') = 173  THEN
		ls_authorization_comment = 'Initial Set'
	Else
		ls_authorization_comment = ''
	End If
	
	//get the fixed_fee_flag from the Billable_Goods_And_Services_Fee table
	Select count(*)
	into    :ll_count
	From Billable_Item_Fee
	Where billable_xref_no = :ll_billable_xref_no
	Using SQLCA;
	
	SQLCA.nf_handle_error('n_referral','nf_create_rehab_task_auths','Select count form Billable_Item_Fee' )
	
	IF ll_count > 0 THEN 
		ls_flag = 'Y'
	ELSE 
		ls_flag = 'N'
	END IF
	
	li_row_no = 	lds_rehab_task_auth.insertrow(0)
	
	lds_rehab_task_auth.setitem(li_row_no,'authorization_no',ll_auth_no)
	lds_rehab_task_auth.setitem(li_row_no,'task_no',al_task_no)
	lds_rehab_task_auth.setitem(li_row_no,'claim_no',al_claim_no)
	lds_rehab_task_auth.setitem(li_row_no,'billable_xref_no',ll_billable_xref_no)
	lds_rehab_task_auth.setitem(li_row_no,'authorized_quantity',ldec_auth_qty)	
	lds_rehab_task_auth.setitem(li_row_no,'authorized_amount',ldec_max_authorized_amount )		
	lds_rehab_task_auth.setitem(li_row_no,'paid_quantity',0)
	lds_rehab_task_auth.setitem(li_row_no,'paid_amount',0)
	lds_rehab_task_auth.setitem(li_row_no,'authorized_provider_type_code','')
	lds_rehab_task_auth.setitem(li_row_no,'authorized_provider_no',0)
	lds_rehab_task_auth.setitem(li_row_no,'authorized_date',date(f_server_datetime()))
	lds_rehab_task_auth.setitem(li_row_no,'authorized_by_user_id',vgst_user_profile.user_id)
	lds_rehab_task_auth.setitem(li_row_no,'authorization_comment',ls_authorization_comment)
	lds_rehab_task_auth.setitem(li_row_no,'expedited_billing_flag','N')
	lds_rehab_task_auth.setitem(li_row_no,'fixed_fee_flag',ls_flag)
	lds_rehab_task_auth.setitem(li_row_no,'auto_created_flag','Y')
	
NEXT

lds_rehab_task_auth.Update()
SQLCA.nf_handle_error('n_referral','nf_create_rehab_task_auths','lds_rehab_task_auth.Update()')


Return 1
end function

public function integer nf_update_task_on_referral (long al_rehab_referral_no, integer al_task_no);// Update the task_no on the REHAB_REFERRAL table when the REHAB_TASK is automatically created

UPDATE REHAB_REFERRAL
Set task_no = :al_task_no
Where rehab_referral_no = :al_rehab_referral_no
Using SQLCA;

SQLCA.nf_handle_error('n_referral','nf_update_task_on_referral','Insert From REHAB_REFERRAL')


Return 1
end function

public function integer nf_check_physio_task_not_assigned (long al_claim_no);
int li_task_no = 0

// checks for existence of a task (ie Primary physio) where a provider has not been assigned)

/* If a primary physio task that does not have a provider (clinic) assigned, exists for this claim, 
    and the status of that task is 'pending' or 'in progress', then that task_no is returned, 
	otherwise, ll_task will be 0. Calling code will test and decide to procede or quit.
*/

Select top 1 task_no
Into    :li_task_no
From   REHAB_TASK
Where claim_no = :al_claim_no
and     rehab_service_code = 'S022'
and     rehab_program_code = 'P001'
and     provider_no = 0
and     task_status_code in ('01', '02')
order by task_no desc
Using  SQLCA;

SQLCA.nf_handle_error('n_referral','nf_check_for_task','Select from REHAB_REFERRAL')


return li_task_no

end function

public function integer nf_update_provider_on_direct_referral (long al_claim_no, integer ai_task_no, long al_provider_no, string as_provider_type);// Update the provider_no on the REHAB_TASK and REHAB_TASK_AUTHORIZATION tables when the REHAB_TASK is automatically created as a direct referral

UPDATE  REHAB_TASK
SET       provider_no            = :al_provider_no, 
            provider_type_code = :as_provider_type
WHERE   claim_no = :al_claim_no
AND       task_no  = :ai_task_no
Using     SQLCA;

SQLCA.nf_handle_error('n_referral','nf_update_provider_on_direct_referral','Update REHAB_TASK')

UPDATE  REHAB_TASK_AUTHORIZATION
SET       authorized_provider_no            = :al_provider_no, 
            authorized_provider_type_code = :as_provider_type
WHERE   claim_no = :al_claim_no
AND       task_no  = :ai_task_no
Using     SQLCA;

SQLCA.nf_handle_error('n_referral','nf_update_provider_on_direct_referral','Update REHAB_TASK_AUTHORIZATION')

Return 1
end function

public function long nf_insert_referral (long al_claim_no, long al_provider_no, string as_provider_type_code, long al_docid, datetime adtm_referred_on_date, string as_service_code, long al_task_no, ref long al_referral_no);
long ll_next_referral_no

ll_next_referral_no = nf_get_next_referral_no()

IF ll_next_referral_no = -1 THEN RETURN -1

// Update the REHAB_REFERRAL table if the referral flag is checked on.

INSERT INTO REHAB_REFERRAL
(rehab_referral_no,referred_on_date,rehab_service_code,referring_provider_no,referring_provider_type_code,claim_no,task_no,doc_id)
VALUES
(:ll_next_referral_no,
:adtm_referred_on_date,
:as_service_code,
:al_provider_no,
:as_provider_type_code,
:al_claim_no,
:al_task_no,
:al_docid)
USING SQLCA;

SQLCA.nf_handle_error('n_referral','nf_insert_referral','Insert From REHAB_REFERRAL')

al_referral_no = ll_next_referral_no  //passed by ref

Return 1
end function

public function integer nf_create_direct_referral (long al_claim_no, long al_provider_no, string as_provider_type_code, long al_docid, datetime adtm_referred_on_date, string as_service_code, long al_task_no, long al_direct_referral_no);/* 
In document indexing, if a document is received and indexed as a type ‘SP’ then 
it may be necessary to create a direct referral physiotherapy rehab task. 

Create a direct referral rehab task only if all the following conditions are met:

•	The SP document has the referral for physiotherapy services checked on
•	The physiotherapy referral date is greater than or equal to the accident date and less than or equal to six weeks from the current date
•	The claim is a  ‘recent accident’ – the accident date is less than or equal to six weeks from the current date
•	The claim status is one of the following: Active with Regular Loss of Earnings, Final-Final, Final-No Lost Time, Final Lost Time Medical Aid Only, or Final First & Final
•	The claim does not have a rehab task for Acupuncture , Athletic Therapy, Back management, Chiropractic, Massage Therapy, Occ Rehab Level 1, Occ Rehab Level 2, Work Hardening, Work Recovery, and Spinal Cord Rehabilitation
•	The claim has an existing rehab plan  ( this is redundant in that if the next condition (below) exists, then a rehab plan must be in existence)
•	The claim has a task for primary physiotherapy which does not have a clinic assigned to the task
*/

//  we only get here if its an 'SP' document being indexed, and the referral flag has been checked on, 
//  so we don't need to check for those conditions


STRING    ls_claim_status_code, ls_claim_status_type
boolean    lb_active
long         ll_max, ll_loop, ll_opening_no, ll_rtn, ll_referral_no, ll_days_since_accident 
INT          li_task_no, li_existing_physio_task
u_ds        lds_claim_info, lds_openings
datetime  ldtm_accident_date, ldtm_today 



ldtm_today = f_server_datetime()

lds_claim_info = create u_ds
lds_claim_info.dataobject = 'd_claim_maintain'
lds_claim_info.settransObject(SQLCA)
lds_claim_info.retrieve(al_claim_no)

SQLCA.nf_handle_error("w_document_imdexing","n_referral.nf_create_direct_referral", "retrieve datastore d_claim_maintain")

lds_openings = create u_ds
lds_openings.dataobject = 'd_openings'
lds_openings.settransObject(SQLCA)
lds_openings.retrieve(al_claim_no)

SQLCA.nf_handle_error("w_document_indexing","n_referral.nf_create_direct_referral", "retrieve datastore d_openings")

// ---- CREATE AN AUTO REHAB PLAN, TASKS AND AUTHORIZATIONS -----

ldtm_accident_date = lds_claim_info.GetItemDateTime (1,'accident_date')
ls_claim_status_code = lds_claim_info.GetItemString(1,'claim_status_code')
ls_claim_status_type = lds_claim_info.GetItemString(1,'claim_status_type_code')
lb_active = FALSE
//
IF ls_claim_status_code = 'A' THEN //check for an RLOE opening for a claim status of Active
//	//loop through the openings to ensure there is an open-ended RLOE opening.
	ll_max = lds_openings.RowCount()
	ll_loop = 1
	DO WHILE ll_loop <= ll_max
      IF lds_openings.GetItemString(ll_loop, 'opening_type_code') = 'RLOE' AND IsNull(lds_openings.GetItemDateTime(ll_loop, 'benefit_end_date')) THEN
		 ll_opening_no = lds_openings.GetItemNumber(ll_loop, 'opening_no')
		 IF IsNull(ll_opening_no) then ll_opening_no = 1
       	 lb_active = TRUE
      END IF
	   ll_loop = ll_loop + 1
	LOOP
END IF
//
//the claim is a new accident within 6 weeks (42 days) of the current date	
ll_days_since_accident =  DaysAfter(Date(ldtm_accident_date),Date(ldtm_today))

// if accident date is within the 42 day period AND the referral date is on or after the accident date 
// then we will know that the referral date is on or after the accident date AND is also within that 42 day window, so 2 BR rules are satisfied here
IF ll_days_since_accident <= 42 AND DATE(adtm_referred_on_date) >= Date(ldtm_accident_date) THEN // 1

//check the claim status
	  IF (ls_claim_status_code = 'F' AND (ls_claim_status_type = '01' OR ls_claim_status_type = '02' OR ls_claim_status_type = '03' OR ls_claim_status_type = '04' )) OR (lb_active = TRUE) THEN  // 3 
		IF IsNull(ll_opening_no) then ll_opening_no = 1

			//check the referral date
			IF this.nf_check_referral_date(al_claim_no,"S022",Date(ldtm_accident_date))	= 1 THEN	  // 4
				  
				IF this.nf_check_other_services(al_claim_no ) = 1 THEN  // 5
				
					// check for existence of primary physio task with no provider assigned 

					li_existing_physio_task = this.nf_check_physio_task_not_assigned(al_claim_no)
					IF li_existing_physio_task > 0  Then //6
				 
						//  A GOAL and OBJECTIVE already exists if a promary physio task exists (checked above), so we only need to create 
						//  the new direct referral task and authorizations and cancel the original primary physio task
							IF this.nf_create_rehab_tasks(al_claim_no,"S022","P006", "AS","168",ll_opening_no,Date(ldtm_accident_date),li_task_no) = 1 THEN //7
								IF this.nf_create_rehab_task_auths(li_task_no, al_claim_no,'S022', 'P006') = 1 THEN  //8
										MessageBox('Rehab Tasks and Authorizations','Rehab Tasks and Authorizations have been automatically created for the claim.',Information!)
								
									// this is a direct referral, so we can assign the provider no and provider type code on both the REHAB_TASK and REHAB_TASK_AUTHORIZATION
									this.nf_update_provider_on_direct_referral(al_claim_no, li_task_no, al_provider_no,as_provider_type_code )
									
									IF this.nf_update_task_on_referral(al_direct_referral_no, li_task_no) < 0 THEN 
											Return -1
										END IF 
								END IF //8
								
							// now 'cancel' the existing primary physio task, which was determined to be in existence at #6 branch above
							UPDATE REHAB_TASK
							SET      task_status_code = '03'
							WHERE  claim_no = :al_claim_no
							and      task_no = :li_existing_physio_task
							USING SQLCA;
							
							SQLCA.nf_handle_error("document_indexing - n_referral","nf_create_direct_referral","UPDATE REHAB_TASK..set task_status_code..")
							
						END IF //6
					END IF //5
				END IF  //4					
			 END IF  //3
	END IF  //2
 END IF  //1


destroy lds_claim_info
destroy lds_openings

return 1
end function

on n_referral.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_referral.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

