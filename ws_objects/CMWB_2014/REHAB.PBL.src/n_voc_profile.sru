$PBExportHeader$n_voc_profile.sru
$PBExportComments$Vocational Profile business rules
forward
global type n_voc_profile from n_pdc
end type
end forward

global type n_voc_profile from n_pdc
end type
global n_voc_profile n_voc_profile

type variables
LONG                           	il_claim_no


end variables

forward prototypes
public function integer nf_check_bus_rule ()
public function integer nf_check_mandatory ()
public function integer nf_set_claim_no (long al_claim_no)
public function integer nf_set_defaults ()
public function integer nf_set_unused_fields ()
public function integer nf_retrieve_voc_profile ()
end prototypes

public function integer nf_check_bus_rule ();STRING	ls_string
/* ----  VALIDATE if data entered for Voc Profile ----
*/

	IF idw_dw[1].GetItemStatus(1,0,Primary!) = New! THEN
/*		New row but no data was entered
*/
		idw_dw[1].DeleteRow(1)
		Return 0	
	END IF

Return 0


end function

public function integer nf_check_mandatory ();
// accept the data windows 
IF idw_dw[1].AcceptText() < 0 THEN Return -1
	
Return 0
end function

public function integer nf_set_claim_no (long al_claim_no);/* Set the claim number retrieved for the rehab plan */
	
il_claim_no = al_claim_no
 
Return 0
end function

public function integer nf_set_defaults ();LONG  ll_row

/* The flags default to "NO":
   	Post-Secondary Education
		Literacy
		Bondable
		Union Affilication
	The codes default to UNKNOWN:
		School Grade
		Language Skill
*/
ll_row = idw_dw[1].GetRow()
IF ll_row > 0 THEN
	idw_dw[1].SetItem(ll_row, "claim_no", il_claim_no)	
	idw_dw[1].SetItem(ll_row, 'post_secondary_education_flag', 'N')
   idw_dw[1].SetItem(ll_row, 'literacy_flag', 'N')
	idw_dw[1].SetItem(ll_row, 'bondable_flag', 'N')
	idw_dw[1].SetItem(ll_row, 'union_affiliation_flag', 'N')
	idw_dw[1].SetItem(ll_row, 'school_grade_completed_code', 'U')
	idw_dw[1].SetItem(ll_row, 'language_skill_code', 'U')
END IF

Return 0
end function

public function integer nf_set_unused_fields ();
LONG  ll_row

/* Determine which fields are null and fill them in with a default value
 	at some point in time any of the following fields may be unused
*/


ll_row = idw_dw[1].GetRow()
IF ll_row > 0 THEN
   IF Isnull(idw_dw[1].GetItemString(ll_row,'school_grade_completed_code')) or &
	   idw_dw[1].GetItemString(ll_row,'school_grade_completed_code') = '' THEN
		idw_dw[1].SetItem(ll_row, 'school_grade_completed_code', 'U')
	END IF
	
	IF Isnull(idw_dw[1].GetItemString(ll_row,'language_skill_code')) or &
	   idw_dw[1].GetItemString(ll_row,'language_skill_code') = '' THEN
		idw_dw[1].SetItem(ll_row, 'language_skill_code', 'U')
	END IF
	
	IF Isnull(idw_dw[1].GetItemString(ll_row,'pre_unit_group_code')) or &
	   idw_dw[1].GetItemString(ll_row,'pre_unit_group_code') = '' THEN
		idw_dw[1].SetItem(ll_row, 'pre_unit_group_code', '')
	END IF
	
	IF Isnull(idw_dw[1].GetItemString(ll_row,'post_unit_group_code')) or &
	   idw_dw[1].GetItemString(ll_row,'post_unit_group_code') = '' THEN
		idw_dw[1].SetItem(ll_row, 'post_unit_group_code', '')
	END IF
	
	IF Isnull(idw_dw[1].GetItemNumber(ll_row,'pre_unit_group_occupation_no')) THEN
		idw_dw[1].SetItem(ll_row, 'pre_unit_group_occupation_no', 0)
	END IF
	
	IF Isnull(idw_dw[1].GetItemNumber(ll_row,'post_unit_group_occupation_no')) THEN
		idw_dw[1].SetItem(ll_row, 'post_unit_group_occupation_no', 0)
	END IF
	
	
END IF

Return 0
end function

public function integer nf_retrieve_voc_profile ();LONG	ll_rows

/* Re-Retrieve the Voc Profile Details */
ll_rows = idw_dw[1].Retrieve(il_claim_no)
SQLCA.nf_handle_error('Retrieve of dw','n_voc_profile','nf_retrieve_voc_profile') 

return 0
end function

on n_voc_profile.create
call super::create
end on

on n_voc_profile.destroy
call super::destroy
end on

