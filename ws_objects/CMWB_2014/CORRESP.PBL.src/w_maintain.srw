$PBExportHeader$w_maintain.srw
$PBExportComments$Create correspondence
forward
global type w_maintain from window
end type
type dw_doc_type from u_dw_online within w_maintain
end type
type dw_document_type_list from u_dw_online within w_maintain
end type
type st_man_entry from statictext within w_maintain
end type
type dw_manual_field_entry from u_dw_online within w_maintain
end type
type cb_next from commandbutton within w_maintain
end type
type cb_prev from commandbutton within w_maintain
end type
type mle_ccs from multilineedit within w_maintain
end type
type cbx_preview from checkbox within w_maintain
end type
type dw_manual_field_list from u_dw_online within w_maintain
end type
type cb_man_ok from commandbutton within w_maintain
end type
type st_to from statictext within w_maintain
end type
type st_2 from statictext within w_maintain
end type
type st_1 from statictext within w_maintain
end type
type dw_template_fields from u_dw_online within w_maintain
end type
type cb_recipients from commandbutton within w_maintain
end type
type cb_ok from commandbutton within w_maintain
end type
type cb_cancel from commandbutton within w_maintain
end type
type gb_1 from groupbox within w_maintain
end type
end forward

global type w_maintain from window
integer x = 91
integer y = 716
integer width = 1915
integer height = 1172
boolean titlebar = true
string title = "Compose "
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 67108864
boolean righttoleft = true
event ue_post_open pbm_custom01
dw_doc_type dw_doc_type
dw_document_type_list dw_document_type_list
st_man_entry st_man_entry
dw_manual_field_entry dw_manual_field_entry
cb_next cb_next
cb_prev cb_prev
mle_ccs mle_ccs
cbx_preview cbx_preview
dw_manual_field_list dw_manual_field_list
cb_man_ok cb_man_ok
st_to st_to
st_2 st_2
st_1 st_1
dw_template_fields dw_template_fields
cb_recipients cb_recipients
cb_ok cb_ok
cb_cancel cb_cancel
gb_1 gb_1
end type
global w_maintain w_maintain

type prototypes

end prototypes

type variables
s_correspond_claim   vistr_correspond_claim
datawindowChild	   idw_ddw_document_types	
int		   vii_return_code
string		   physical_file_name,vis_sgnt_file
long		   doc_id
w_sheet		   viw_sheet	
w_correspond	   viw_correspond
int		   vii_row_nbr, vii_max_rcpnt

// Variables used to process template
string		   vis_field[30,2]
long		   vil_actual_fields, vil_usage
string		   vis_word_command
int		   vii_nbr_rows
string		   vis_file_name, vis_doc_path
uint		   vii_win_handle
// Varible used to access Imara functions
n_imaging	   vinv_imfunction

u_word ioo_word
end variables

forward prototypes
public function integer wf_determine_file_name ()
public function integer wf_create_corr_rcpnt (string addressee_role, long addressee_id, string addressee_type_code)
public function integer wf_process_recipients ()
public function string wf_determine_field_values (string label)
public function long wf_process_template ()
public function integer wf_create_imara_index ()
public function integer wf_vldte_file_name (long al_docid)
end prototypes

event ue_post_open;long ll_handle

dw_document_type_list.Setfocus()
this.parentwindow().SetMicroHelp("Ready")

end event

public function integer wf_determine_file_name ();// ************************************************************************************************
// AUTHOR:	MW Georgev
// Modified:	94.10.24

// ************************************************************************************************
// THIS FUNCTION DETERMINES THE PHYSICAL FILE NAME OF GENERATED
// CORRESPONDENCE ALONG WITH THE DOC_ID FOR IMARA INDEXING PURPOSES

// ************************************************************************************************
// DECLARATIONS

int				rc=-1
long				folder_id,ldoc_id,temp
string			year_month, ls_folder_text, ls_doc_text
/* window			w_correspond */
w_correspond	vlw_window

N_OBJECTHELPER lnv_object_helper

f_populate_app_log(gs_appname,lnv_object_helper.get_app_component_type(THIS),lnv_object_helper.getpath(THIS),'wf_determine_file_name-begin')


	SetPointer(HourGlass!)
	IF ISvalid (vinv_imfunction) = False THEN
		// signal error if n_imaging is not valid
		Error.Text        = ImageTrans.sqlerrtext
		IF Error.Text = '' THEN
			Error.Text        = 'Imara object n_imaging is not valid' + &
			                    '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.' + &
			                    '~r~nDoc ID: ' + String(vistr_correspond_claim.corr.doc_id) + '.' + &
			                    '~r~nTemplate Type: ' + vistr_correspond_claim.corr.template_type + '.'
		ELSE
			Error.Text        = Error.Text + '.~r~nImara object n_imaging is not valid' + &
			                    '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.'  + &
			                    '~r~nDoc ID: ' + String(vistr_correspond_claim.corr.doc_id) + '.' + &
			                    '~r~nTemplate Type: ' + vistr_correspond_claim.corr.template_type + '.'
		END IF
		Error.Object      = 'w_maintain'
		Error.is_database = 'IMARA_DB'
		Error.il_dbcode   = ImageTrans.SQLDBCode
		Error.ObjectEvent = 'wf_determine_filename'
		SignalError()
	END IF

	ImageTrans.nf_begin_transaction()
	
	rc = vinv_imfunction.nf_create_claimsmaster("W_maintain",vistr_correspond_claim.claim_no,vistr_correspond_claim.image_status)
	Choose Case RC
		Case 0  // CLAIM and Folder exists Do nothing
			ImageTrans.nf_rollback_transaction()
		Case 1 //  CLAIM and Folder created
			ImageTrans.nf_commit_transaction()
		Case Else
			ImageTrans.nf_rollback_transaction()
	End Choose		
/********************************************************************************
	get a folder id
*********************************************************************************/
	SELECT folderid  
   	INTO :folder_id  
   	FROM CLAIM_MASTER  
 		WHERE claim_no = :vistr_correspond_claim.claim_no
   	USING ImageTrans ;
	vii_return_code = ImageTrans.nf_handle_error("Embedded SQL: Select from CLAIM_MASTER ","w_maintain","on wf_determine_file_name")
	
	IF vii_return_code= 100 then
		// signal error if no folder id for claim.
		IF IsNull(folder_id) THEN 
			ls_folder_text = 'null'
		ELSE
			ls_folder_text = String(folder_id)
		END IF
		Error.Text        = ImageTrans.sqlerrtext
		IF Error.Text = '' THEN
			Error.Text        = 'Invalid folder ID: ' + ls_folder_text + &
			                    '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.' + &
			                    '~r~nDoc ID: ' + String(vistr_correspond_claim.corr.doc_id) + '.' + &
			                    '~r~nTemplate Type: ' + vistr_correspond_claim.corr.template_type + '.' 
		ELSE
			Error.Text        = Error.Text + '.~r~nInvalid folder ID: ' + ls_folder_text + &
			                    '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.'  + &
			                    '~r~nDoc ID: ' + String(vistr_correspond_claim.corr.doc_id) + '.' + &
			                    '~r~nTemplate Type: ' + vistr_correspond_claim.corr.template_type + '.'
		END IF
		Error.Object      = 'w_maintain'
		Error.is_database = 'IMARA_DB'
		Error.il_dbcode   = ImageTrans.SQLDBCode
		Error.ObjectEvent = 'wf_determine_filename'
		SignalError()
	END IF
	

f_populate_app_log(gs_appname,lnv_object_helper.get_app_component_type(THIS),lnv_object_helper.getpath(THIS),'wf_determine_file_name-between txns')

/*******************************************************************************************
	Get a docid
*****************************************************************************************/
// Retrieve the docid all files must follow IMARA standard of uppercase for filename and extension


	// this function begins & commits two IMARA txns, then begins another. This txn must be committed by calling object
	ldoc_id = vinv_imfunction.nf_create_template_document('w_maintain',folder_id,vistr_correspond_claim.corr.template_extension,physical_file_name)
	
	IF ldoc_id <= 0 THEN
		// signal error if the doc id is not greater than zero.
		Error.Text        = ImageTrans.sqlerrtext
		IF Error.Text = '' THEN
			Error.Text        = 'Invalid doc ID: ' + String(ldoc_id) + &
			                    '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.' + &
			                    '~r~nFolder ID: ' + String(folder_id) + '.' + &
			                    '~r~nTemplate Type: ' + String(vistr_correspond_claim.corr.template_type) + '.'
		ELSE
			Error.Text        = Error.Text + '.~r~nInvalid doc ID: ' + String(ldoc_id) + &
			                    '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.' + &
			                    '~r~nFolder ID: ' + String(folder_id) + '.' + &
			                    '~r~nTemplate Type: ' + String(vistr_correspond_claim.corr.template_type) + '.'
		END IF
		Error.Object      = 'w_maintain'
		Error.is_database = 'IMARA_DB'
		Error.il_dbcode   = ImageTrans.SQLDBCode
		Error.ObjectEvent = 'wf_determine_filename'
		SignalError()
	end if
	
	wf_vldte_file_name(ldoc_id)
	
	
	vistr_correspond_claim.corr.document_name = physical_file_name
	vistr_correspond_claim.corr.doc_id = ldoc_id
	
RETURN 0

end function

public function integer wf_create_corr_rcpnt (string addressee_role, long addressee_id, string addressee_type_code);// ********************************************************************************************
// RECORD DID NOT EXIST SO CREATE ONE
// NOTE: RECIPIENT_TYPE_CD IS BLANK SINCE THE FIELD IS NO LONGER NEEDED
// FOR RELEASE 1, IT IS NOT IMPORTANT
// Validate parameters

	IF ISNULL(addressee_id) or addressee_id < 0 then RETURN -1
	TRIM (addressee_role)
	IF addressee_role = " " then Return -1
	TRIM (addressee_type_code)
	IF addressee_type_code = " " then RETURN -1 

	INSERT into CORRESPONDENCE_RECIPIENT
			(claim_no, correspond_no, correspond_recipient_id, addressee_role_code,
			correspond_recipient_type_cd)
	VALUES
			(:vistr_correspond_claim.claim_no, :vistr_correspond_claim.corr.corr_no, :addressee_id, 
			:addressee_role, :addressee_type_code)
	USING	SQLCA;

	SQLCA.nf_handle_error("Insert into Correspondence Recipient ","w_recipients","on wf_create_corr_rcpnt")


RETURN 0
end function

public function integer wf_process_recipients ();// ************************************************************************************************
//	This function will process the final recipient list
//
// ************************************************************************************************
// DECLARATIONS

string	vls_recipient_type_code, addressee_role
long		addressee_id,vli_recipient_id
int		vli_rc,  i = 1

// Get addressee in first position
addressee_id = gs_rcpnt_lst[i].ws_rcpnt_id
vls_recipient_type_code = gs_rcpnt_lst[i].ws_rcpnt_type_cd
addressee_role = gs_rcpnt_lst[i].ws_addressee_role
vli_rc = wf_create_corr_rcpnt(addressee_role, addressee_id, vls_recipient_type_code)
IF vli_rc < 0 THEN
	Return -1
END IF

// Get CC's starting in the second position
FOR i = 2 to vii_max_rcpnt
	vli_recipient_id = gs_rcpnt_lst[i].ws_rcpnt_id
	vls_recipient_type_code = gs_rcpnt_lst[i].ws_rcpnt_type_cd
	addressee_role = gs_rcpnt_lst[i].ws_addressee_role
	vli_rc = wf_create_corr_rcpnt(addressee_role, vli_recipient_id, vls_recipient_type_code)
	IF vli_rc < 0 THEN
		Return -1
	END IF
NEXT

Return 0
end function

public function string wf_determine_field_values (string label);// AUTHOR:	MW George
// Modified:	94.10.10

DATETIME	ldtm_annuity_start_date
DECIMAL		ldec_annuity_set_aside_percent
LONG   		ll_ext_id, ll_rcpnt_id, ll_accident_employer_operation_no
STRING 		ls_value, ls_employer_type_code
STRING 		ls_lang, ls_rcpnt_type_code, ls_clmt_phone_no, ls_empl_phone_no
STRING 		ls_acode, ls_name, ls_oname, ls_aline1, ls_aline2, ls_aline3, ls_tcity
STRING 		ls_prov, ls_tcountry, ls_pc, ls_line_1, ls_line_2, ls_line_3, ls_line_4, ls_region_code
STRING 		ls_toll_free_no, ls_line_5, ls_line_6, ls_vrfyd_pc, ls_card_file_flag, ls_given_name, ls_last_name

SetPointer(HourGlass!)

// Determine language
ls_lang = Left(vistr_correspond_claim.corr.version_no,1)

ls_value = ""

Choose Case label
Case "{claim_number}"
	ls_value = String(vistr_correspond_claim.claim_no)

Case "{date_of_letter}"
	IF ls_lang = 'E' THEN
		ls_value = String(f_server_datetime(), "mmmm dd, yyyy")
	ELSE
		ls_value = "Le " + f_cnvrt_month(Date(f_server_datetime()))
	END IF

Case "{addressee}"
	
	IF vistr_correspond_claim.corr.template_type = 'ANNPAY01' OR vistr_correspond_claim.corr.template_type = 'ANNPAY02' THEN		
		//WHAT TO DO WHAT TO DO!!!!!!
		SELECT  last_name, given_names, address_line1, address_line2,
						city, prov_state_code, country_code, postal_code
		INTO    	 :ls_last_name, :ls_given_name, :ls_aline1, :ls_aline2,
						:ls_tcity, :ls_prov,:ls_tcountry, :ls_pc
		FROM    	INDIVIDUAL
		WHERE   individual_no = :vistr_correspond_claim.annuity_payout_individual_no
		USING 	SQLCA;

		vii_return_code = SQLCA.nf_handle_error("Correspondence Recipient - w_maintain ","wf_determine_field_values","select from INDIVIDUAL..(A)")

		ls_name = trim(ls_given_name) + " " + trim(ls_last_name)
		
	ELSE	
		// FIND OUT WHERE THE ADDRESS IS STORED
		ll_rcpnt_id = gs_rcpnt_lst[1].ws_rcpnt_id
		
		SELECT 	recipient_no, address_location_code,correspond_recipient_subtyp_cd, card_file_flag
		INTO 		:ll_ext_id, :ls_acode, :ls_rcpnt_type_code, :ls_card_file_flag
		FROM 	RECIPIENT_LIST
		WHERE 	claim_no 					= :vistr_correspond_claim.claim_no
		AND 		correspond_recipient_id 	= :ll_rcpnt_id 
		USING 	SQLCA ;
		
		vii_return_code = SQLCA.nf_handle_error(" ","w_maintain","on wf_determine_field_values")
		if vii_return_code = 0 then
			if ls_acode = "C" then
				// ADDRESS IS STORED LOCALLY
				SELECT 	name1, name2, address_line1, address_line2, address_line3, 
							city, province, country, postal_code
				INTO 		:ls_name, :ls_oname, :ls_aline1, :ls_aline2, :ls_aline3, :ls_tcity, :ls_prov,
							:ls_tcountry, :ls_pc
				FROM 	RECIPIENT_ADDRESS
				WHERE 	correspond_recipient_id 	= :ll_rcpnt_id 
				AND 		active_flag 					= 'Y' 
				USING 	SQLCA;
	
			elseif ls_acode = "P" then
				// ADDRESS IS STORED WITH THE SERVICE PROVIDER DATA
				SELECT	name, address_line1, address_line2, 
							city, prov_state_code, country_code, postal_code, contact_name
				INTO 		:ls_name, :ls_aline1, :ls_aline2, 
							:ls_tcity, :ls_prov, :ls_tcountry, :ls_pc, :ls_oname
				FROM 	PROVIDER
				WHERE 	provider_no 			= :ll_ext_id 
				AND 		provider_type_code 	= :ls_rcpnt_type_code
				USING 	SQLCA ;
	
			elseif ls_acode = "E" then
				
				// ADDRESS IS STORED WITH THE EMPLOYER DATA
				SELECT 	C.accident_employer_operation_no, E.employer_type_code  
				INTO 		:ll_accident_employer_operation_no, :ls_employer_type_code 
				FROM 	CLAIM C, EMPLOYER E   
				WHERE 	C.claim_no 					= :vistr_correspond_claim.claim_no 
				AND 		C.accident_employer_no = E.employer_no ;
	
				vii_return_code = SQLCA.nf_handle_error(" ","w_maintain","on wf_determine_field_values")
				
				IF ls_employer_type_code = "S" THEN
					
					SELECT	E.employer_legal_name, O.operation_name, EA.address_line1, EA.address_line2, EA.address_line3,   
								EA.city, EA.prov_state_code, EA.postal_code, EA.country_code
					INTO 		:ls_name, :ls_oname, :ls_aline1, :ls_aline2, :ls_aline3, :ls_tcity, :ls_prov, :ls_pc, :ls_tcountry
					FROM 	EMPLOYER_ADDRESS EA, EMPLOYER E, OPERATION O  
					WHERE 	EA.employer_no 			= :ll_ext_id
					AND 		EA.address_type_code 	= "BA" 
					AND 		O.operation_no          = :ll_accident_employer_operation_no 
					AND 		EA.employer_no 			= E.employer_no 
					AND 		EA.employer_no 			= O.employer_no 
					USING 	SQLCA ; 
				ELSE
					
					SELECT 	E.employer_legal_name, EA.address_line1, EA.address_line2, EA.address_line3,   
								EA.city, EA.prov_state_code, EA.postal_code, EA.country_code
					INTO 		:ls_name, :ls_aline1, :ls_aline2, :ls_aline3, :ls_tcity, :ls_prov, :ls_pc, :ls_tcountry
					FROM 	EMPLOYER_ADDRESS EA, EMPLOYER E  
					WHERE 	EA.employer_no 			= :ll_ext_id
					AND 		EA.address_type_code 	= "BA" 
					AND 		EA.employer_no 			= E.employer_no 
					USING SQLCA ; 
					
				END IF
			end if
		end if	
		vii_return_code = SQLCA.nf_handle_error(" ","w_maintain","on wf_determine_field_values")
			
	END IF // end of else for annuity stuff
		
	if vii_return_code = 0 then
		ls_vrfyd_pc = f_vrfy_pstl_cd(ls_pc, ls_tcountry)	// Verify postal code format

		// FORMAT THE ADDRESS
		ls_line_1 = UPPER(ls_name) 
		ls_line_2 = UPPER(ls_oname)
		ls_line_3 = UPPER(ls_aline1)
		ls_line_4 = UPPER(ls_aline2)
		ls_line_5 = UPPER(ls_aline3)
		ls_line_6 = Trim(UPPER(ls_tcity))+ " "+ trim(ls_prov) + "  " + trim(ls_vrfyd_pc)	

		// The following result as - "line1" + "^p"  + "line2" + "^p"  + "line3"+ "^p" + "line4" ....
		IF Trim(ls_line_1) <> "" THEN ls_value = ls_line_1 + "^p"
		IF Trim(ls_line_2) <> "" THEN ls_value = ls_value + ls_line_2 + "^p"
		IF Trim(ls_line_3) <> "" THEN ls_value = ls_value + ls_line_3 + "^p" 
		IF Trim(ls_line_4) <> "" THEN ls_value = ls_value + ls_line_4 + "^p" 
		IF Trim(ls_line_5) <> "" THEN ls_value = ls_value + ls_line_5 + "^p" 
		ls_value = ls_value + ls_line_6 // + "^p"  PB8 upgrade: unnecessary extra line // Added on Oct 15, 1996 some users complained there is no new line
		
		//PR6924, Add the country to the address if its not a Canadian address, as per Canada Post standards
		ls_tcountry = UPPER(TRIM(ls_tcountry))
		IF  ls_tcountry <> 'CANADA' AND ls_tcountry <> 'CAN' THEN
			SELECT 	location_desc1
			INTO    	:ls_tcountry
			FROM   	Location
			WHERE 	location_type_code 							= 'C'
			AND      	UPPER(RTRIM(LTRIM(location_code))) 	= :ls_tcountry  
			USING 	SQLCA;
			
			IF SQLCA.nf_handle_error("Correspondence Recipient - w_maintain ","wf_determine_field_values","select location_desc1 from Location ") = 0 Then
				ls_value = ls_value + "^p" + ls_tcountry
			END IF
		END IF		
	end if
	
Case "{claimant_name}"
	
		ls_value = trim(vistr_correspond_claim.first_name)+" "+trim(vistr_correspond_claim.last_name)
	
//PR7420 - 2008-06-02 - r.s. - New template field used in M19 correspondence letters, used in place of {addressee}
Case "{claimant_address}"

	SELECT  	address_line1, address_line2,
			    	city, prov_state_code, country_code, postal_code
	INTO    	:ls_aline1, :ls_aline2,
	           	:ls_tcity, :ls_prov,:ls_tcountry, :ls_pc
	FROM    	INDIVIDUAL
	WHERE   individual_no = 
				( select individual_no from CLAIM where claim_no = :vistr_correspond_claim.claim_no )
	USING 	SQLCA;
	
	SQLCA.nf_handle_error("Correspondence Recipient - w_maintain ","wf_determine_field_values","select from INDIVIDUAL..")
	
	ls_vrfyd_pc = f_vrfy_pstl_cd(ls_pc, ls_tcountry)	// Verify postal code format
	ls_line_3 = Trim(UPPER(ls_tcity))+ " "+ trim(ls_prov) + "  " + trim(ls_vrfyd_pc)
	
	IF Trim(ls_aline1) <> "" THEN ls_value = ls_value + ls_aline1 + "^p"
	IF Trim(ls_aline2) <> "" THEN ls_value = ls_value + ls_aline2 + "^p"	
	ls_value = ls_value + ls_line_3 
	
	ls_tcountry = UPPER(TRIM(ls_tcountry))
	IF  ls_tcountry <> 'CANADA' AND ls_tcountry <> 'CAN' THEN
		SELECT 	location_desc1
		INTO    	:ls_tcountry
		FROM   	Location
		WHERE 	location_type_code 							= 'C'
		AND      	UPPER(RTRIM(LTRIM(location_code))) 	= :ls_tcountry  
		USING 	SQLCA;
				
		IF SQLCA.nf_handle_error("Correspondence Recipient - w_maintain ","wf_determine_field_values","select location_desc1 from Location ") = 0 Then
			ls_value = ls_value + "^p" + ls_tcountry
		END IF
	END IF	

Case "{accident_year}"
	ls_value = String(Year(vistr_correspond_claim.accident_date))

Case "{date_of_accident}"
	IF ls_lang = 'E' THEN
		ls_value = String(vistr_correspond_claim.accident_date, "mmmm dd, yyyy")
	ELSE
		ls_value = f_cnvrt_month(vistr_correspond_claim.accident_date)
	END IF

Case "{accident_employer_name}", "{employer_name}"
	ls_value = Trim(vistr_correspond_claim.accident_employer_name)

Case "{date_of_birth}"
	IF ls_lang = 'E' THEN
		ls_value = String(vistr_correspond_claim.birth_date, "mmmm dd yyyy")
	ELSE
		ls_value = f_cnvrt_month(vistr_correspond_claim.birth_date)
	END IF
	
Case "{toll_free_phone_no}"
	ls_region_code = vgst_user_profile.default_admin_region_code
	SELECT 	Admin_Region.toll_free_telephone_no  
     INTO 		:ls_toll_free_no  
     FROM 	Admin_Region  
    WHERE 	Admin_Region.admin_region_code = :ls_region_code   ;

	vii_return_code = SQLCA.nf_handle_error(" ","w_maintain","on wf_determine_field_values")
	CHOOSE CASE Len(trim(ls_toll_free_no))
		CASE 10
			ls_value = "(" + Left(trim(ls_toll_free_no), 3) + ") " + Mid(trim(ls_toll_free_no),4,3) + "-" + Right(trim(ls_toll_free_no),4)
		CASE 7
			ls_value = Left(trim(ls_toll_free_no), 3) + "-" + Right(trim(ls_toll_free_no),4)
		CASE 14  // P10151-100 add this case for numbers that are 14 chars long (including the dashes) and reformat telephone number to x xxx xxx-xxxx
			ls_value = Left(ls_toll_free_no, 1) + ' ' + Mid(trim(ls_toll_free_no), 3,3) + ' ' + Mid(trim(ls_toll_free_no),7,3) + "-" + Right(trim(ls_toll_free_no),4)
		CASE ELSE
			ls_value = trim(ls_toll_free_no)
	END CHOOSE

Case "{claimant_telephone_number}"
	ls_clmt_phone_no = vistr_correspond_claim.claimant_phone_no
	
	CHOOSE CASE Len(trim(ls_clmt_phone_no)) //2015-05-13 T014496 - David Worboys - copied and adapted this block from toll numbers
		CASE 14  
			ls_value = Left(ls_clmt_phone_no, 1) + ' ' + Mid(trim(ls_clmt_phone_no), 3,3) + ' ' + Mid(trim(ls_clmt_phone_no),7,3) + "-" + Right(trim(ls_clmt_phone_no),4)
		CASE 10
			ls_value = Left(trim(ls_clmt_phone_no), 3)+ " "  + Mid(trim(ls_clmt_phone_no),4,3) + "-" + Right(trim(ls_clmt_phone_no),4)
		CASE 7
			ls_value = Left(trim(ls_clmt_phone_no), 3) + " " + Right(trim(ls_clmt_phone_no),4)
		CASE 0
			ls_value = 'N/A'
		CASE ELSE
			ls_value = trim(ls_clmt_phone_no)
	END CHOOSE
	
Case "{claimant_area_code}"
	ls_clmt_phone_no = vistr_correspond_claim.claimant_phone_no
	CHOOSE CASE Len(trim(vistr_correspond_claim.claimant_phone_no))
		CASE 10
			ls_value = Trim(Left(ls_clmt_phone_no, 3))
		CASE ELSE
			ls_value = Trim(ls_clmt_phone_no)
	END CHOOSE

Case "{employer_telephone_number}"
	ls_empl_phone_no = vistr_correspond_claim.accident_employer_phone_no
	CHOOSE CASE Len(trim(ls_empl_phone_no))
		CASE 10
			ls_value = Mid(trim(ls_empl_phone_no),4,3) + "-" + Right(trim(ls_empl_phone_no),4)
		CASE 7
			ls_value = Left(trim(ls_empl_phone_no), 3) + "-" + Right(trim(ls_empl_phone_no),4)
		CASE ELSE
			ls_value = Trim(ls_empl_phone_no)
	END CHOOSE

Case "{employer_area_code}"
	// RETRIEVE FROM EMPLOYER_ADDRESS
	ls_empl_phone_no = vistr_correspond_claim.accident_employer_phone_no
	CHOOSE CASE Len(trim(ls_empl_phone_no))
		CASE 10
			ls_value = Trim(Left(ls_empl_phone_no, 3))
		CASE ELSE
			ls_value = Trim(ls_empl_phone_no)
	END CHOOSE

Case "{case_manager}"
	ls_value = vistr_correspond_claim.case_manager

Case "{medicare_number}"
	ls_value = vistr_correspond_claim.medicare_no

Case "{social_insurance_number}"
	ls_value = vistr_correspond_claim.sin_no

Case "{preferred_language}"
	ls_value = vistr_correspond_claim.language

Case "{sender_position}"
	IF ls_lang = 'E' THEN
		ls_value = vgst_user_profile.position_english_desc
	ELSE
		ls_value = vgst_user_profile.position_french_desc
	END IF

Case "{sender_name}"
	ls_value = vgst_user_profile.user_name

Case "{whscc_local_phone_no}"
	IF vgst_user_profile.phone_no <> "" THEN
		ls_value = "506 " + vgst_user_profile.phone_no  //  P10151-100 reformat this number to remove the parenthesis
	END IF

Case "{full_address}"
	ls_aline1 = Trim(vistr_correspond_claim.street)
	ls_tcity = Trim(vistr_correspond_claim.city)
	ls_prov = Trim(vistr_correspond_claim.province)
	ls_value = ls_aline1 + ", " + ls_tcity + ", " + ls_prov

Case "{annuity_percentage}"
	
	IF vistr_correspond_claim.corr.template_type = 'ANNPAY01' OR vistr_correspond_claim.corr.template_type = 'ANNPAY02' THEN
		
		//at this point we have given them warnings for everything so just grab the percentage
		// use the saved claim_role_code: if they are a 'SS' use that individual for this claim_no, otherwise its an IW, so claim_no is 0
		// (the same individual could have multiple annuity accounts: one as an 'SS' and one as a 'C' so there could be more than one percentage  - we only want the one as it pertains to this claim if individual is an SS
		SELECT	a.annuity_set_aside_percent
		INTO		:ldec_annuity_set_aside_percent
		FROM		ANNUITY_ELIGIBILITY	a  
        	JOIN		ANNUITY_ACCOUNT      b on a.annuity_account_no = b.annuity_account_no
		WHERE	a.annuity_eligibility_status_code 	= 'A'
		AND		b.individual_no 						= :vistr_correspond_claim.annuity_payout_individual_no
		AND		b.claim_no 							= CASE :vistr_correspond_claim.annuity_individual_claim_role
		                                                 			WHEN 'SS' then :vistr_correspond_claim.claim_no 
																ELSE 0 END
		USING    SQLCA;
		
		SQLCA.nf_handle_error('w_maintain', 'wf_determine_field_values()', 'SELECT annuity_set_aside_percentage FROM ANNUITY_ELIGIBILITY...')
		
		IF ls_lang = 'E' THEN
			ls_value = String(ldec_annuity_set_aside_percent,'##')
		ELSE
			ls_value = String(ldec_annuity_set_aside_percent,'##')		
		END IF
		
	ELSE	
		
		SELECT	a.annuity_set_aside_percent
		INTO		:ldec_annuity_set_aside_percent
		FROM		ANNUITY_ELIGIBILITY	a
		JOIN		ANNUITY_ACCOUNT	b ON	a.annuity_account_no 	= b.annuity_account_no
		JOIN		CLAIM					c ON	b.individual_no 				= c.individual_no
		JOIN		CLAIM_PARTICIPANT	d ON	c.claim_no 					= d.claim_no
												AND	c.individual_no 				= d.individual_no
		JOIN		INDIVIDUAL				e ON	d.individual_no 				= e.individual_no
		WHERE	a.annuity_eligibility_status_code 	= 'A'
		AND		c.claim_no 								= :vistr_correspond_claim.claim_no
		AND		c.individual_no 							= :vistr_correspond_claim.individual_no
		AND		d.claimant_active_flag 				= 'Y'
		AND		e.death_date 							is null
		AND NOT EXISTS	(	SELECT	*
									FROM		ANNUITY_ELIGIBILITY d
									WHERE	d.annuity_account_no 				= a.annuity_account_no
									AND		d.annuity_eligibility_status_code 	= 'P')
		USING SQLCA;
		SQLCA.nf_handle_error('w_maintain', 'wf_determine_field_values', 'SELECT annuity_set_aside_percentage FROM ANNUITY_ELIGIBILITY...')
			
		IF ls_lang = 'E' THEN
			ls_value = String(ldec_annuity_set_aside_percent,'##')
		ELSE
			ls_value = String(ldec_annuity_set_aside_percent,'##')		
		END IF
	END IF 

Case "{annuity_start_date}"
	
	SELECT	a.annuity_start_date
	INTO		:ldtm_annuity_start_date
	FROM		ANNUITY_ELIGIBILITY	a
	JOIN		ANNUITY_ACCOUNT	b ON	a.annuity_account_no 	= b.annuity_account_no
	JOIN		CLAIM					c ON	b.individual_no 				= c.individual_no
	JOIN		CLAIM_PARTICIPANT	d ON	c.claim_no 					= d.claim_no
											AND	c.individual_no 				= d.individual_no
	JOIN		INDIVIDUAL				e ON	d.individual_no 				= e.individual_no
	WHERE	a.annuity_eligibility_status_code 	= 'A'
	AND		c.claim_no 								= :vistr_correspond_claim.claim_no
	AND		c.individual_no 							= :vistr_correspond_claim.individual_no
	AND		d.claimant_active_flag 				= 'Y'
	AND		e.death_date 							is null
	AND NOT EXISTS	(	SELECT	*
								FROM		ANNUITY_ELIGIBILITY d
								WHERE	d.annuity_account_no 				= a.annuity_account_no
								AND		d.annuity_eligibility_status_code 	= 'P')
	USING SQLCA;
	SQLCA.nf_handle_error('w_maintain', 'wf_determine_field_values', 'SELECT annuity_set_aside_percentage FROM ANNUITY_ELIGIBILITY...')
	
	IF IsNull(ldtm_annuity_start_date) OR String(ldtm_annuity_start_date,'yyyy-mm-dd') = '1900-01-01' THEN
		ls_value = ''
	ELSE
		IF ls_lang = 'E' THEN
			ls_value = String(Date(ldtm_annuity_start_date), "mmmm dd, yyyy")
		ELSE
			ls_value = f_cnvrt_month(Date(ldtm_annuity_start_date))
		END IF
	END IF
Case '{claimant_cellphone_number}' //2015-05-07 T014496 - David Worboys
	SELECT cellphone_no  
		INTO    	 :ls_value
		FROM    	INDIVIDUAL
		WHERE   individual_no = :vistr_correspond_claim.individual_no
		USING 	SQLCA;

	CHOOSE CASE Len(trim(ls_value)) //copied and adapted this block from toll numbers
		CASE 14  
			ls_value = Left(ls_value, 1) + ' ' + Mid(trim(ls_value), 3,3) + ' ' + Mid(trim(ls_value),7,3) + "-" + Right(trim(ls_value),4)
		CASE 10
			ls_value = Left(trim(ls_value), 3)+ " "  + Mid(trim(ls_value),4,3) + "-" + Right(trim(ls_value),4)
		CASE 7
			ls_value = Left(trim(ls_value), 3) + " " + Right(trim(ls_value),4)
		CASE 0
			ls_value = 'N/A'
		CASE ELSE
			ls_value = trim(ls_value)
	END CHOOSE

Case Else
	ls_value = ""
End Choose

RETURN ls_value
end function

public function long wf_process_template ();//// AUTHOR:	MW George
//// Modified:	94.10.28
Date    ldt_tdate
Long    i, j, ll_usage, ll_field_ctr, ll_rcpnt_id, ll_ext_id, ll_format_type
String  ls_sdate, ls_rcpnt_type_code
String  ls_label, ls_physical_file_path
String  ls_msg, ls_card_file_flag
String  ls_acode, ls_name, ls_oname
Int     li_rc, li_return
Ulong   lul_handle
Boolean lb_file_exists, lb_found_field, lb_found

N_OBJECTHELPER lnv_object_helper



ioo_word = Create u_word

SetPointer(HourGlass!)

ls_msg = "Composing " + Trim(vistr_correspond_claim.corr.template_type) + ", Please wait..."

// Disable main window, so user loses control of the application,
// when communcation with word is timing out. (user can't quit until
// the error is displayed after 60 sec.)
w_frame.enabled = False

// ***************************************************************************************************************
// SET THE DOCUMENT PATHS AND PHYSICAL FILE NAME
// NOTE:
// TO PROPERLY FORMAT THE FILEOPEN COMMAND, THE DOC_PATH BELOW IS ENCLOSED WITHIN
// SINGLE QUOTES TO ALLOW THE STRING ITSELF TO CONTAIN DOUBLE QUOTES WHICH IS WHAT
// WORDBASIC REQUIRES FOR THE FILE NAME.

//  NEED TO FIGURE OUT HOW TO GET THE COMMENT SELECTED FROM THE DATAWINDOW - INSTANCE VARIABLE ?
//vistr_correspond_claim.corr.comments = dw_doc_type.Retrieve(

// ** STARTS IMARA TXN ** STARTS IMARA TXN ** STARTS IMARA TXN ** STARTS IMARA TXN ** STARTS IMARA TXN ** 
li_rc = wf_determine_file_name()
if li_rc < 0 THEN Return -1

SetPointer(HourGlass!)
// Setup template path and name
vis_file_name = vistr_correspond_claim.corr.template_type + '.' + vistr_correspond_claim.corr.template_extension

//IF creating a macro-enabled document, need to save in a different format than a regular word doc
IF vistr_correspond_claim.corr.template_extension = 'docm' THEN
	ll_format_type = 13
ELSE
	ll_format_type = 12
END IF

// Determine language
If Left(vistr_correspond_claim.corr.version_no,1) = 'E' THEN
	vis_doc_path =  vistr_correspond_claim.corr.engl_tmplt_path + vis_file_name
ELSE
	vis_doc_path =  vistr_correspond_claim.corr.french_tmplt_path + vis_file_name
End If

ls_physical_file_path = vis_doc_path

// CONTINUE WITH THE CREATION OF THE CORRESPONDENCE.
// SET UP THE VARIABLES+DATA TO MERGE INTO THE SELECTED TEMPLATE
ldt_tdate = today()
ls_sdate = String(ldt_tdate)
	
if vil_actual_fields > 0 then
	for i = 1 to vil_actual_fields
		if upper(dw_template_fields.GetItemString(i, "access_code")) = 'S' then
			vis_field[i,1]	= dw_template_fields.GetItemString(i, "template_field_id")	// Process system fields
			ls_label		= vis_field[i,1]
			vis_field[i,2] 	= wf_determine_field_values(ls_label)
			SetPointer(HourGlass!)
		end if
	next
end if

// DETERMINE THE CC NAMES
ll_field_ctr = vil_actual_fields + 1
j = ll_field_ctr
vis_field[j,1] = "{cc}"
vis_field[j,2] = " "	

// If the template doesn't allow CC's then you should bypass this
// CC recipients start in position 2, addressee is in position 1
vii_max_rcpnt = UPPERBOUND(gs_rcpnt_lst)
FOR i = 2 to vii_max_rcpnt
	ls_name = ""
	ll_rcpnt_id = gs_rcpnt_lst[i].ws_rcpnt_id

	SELECT recipient_no, address_location_code, correspond_recipient_subtyp_cd, card_file_flag
	  INTO :ll_ext_id, :ls_acode, :ls_rcpnt_type_code, :ls_card_file_flag
	  FROM RECIPIENT_LIST
	 WHERE claim_no = :vistr_correspond_claim.claim_no
	   AND correspond_recipient_id = :ll_rcpnt_id 
	USING SQLCA ;
	vii_return_code = SQLCA.nf_handle_error("Select from Recipient_List ","w_maintain","on cb_ok clicked")		

	if vii_return_code = 0 then
		if ls_acode = "C" then
			// NAME IS STORED LOCALLY
			SELECT name1, name2
			  INTO :ls_name, :ls_oname
			  FROM RECIPIENT_ADDRESS
			 WHERE correspond_recipient_id	= :ll_rcpnt_id
			   AND active_flag = 'Y' 
			 USING SQLCA ;
		elseif ls_acode = "P" then
			// NAME IS STORED WITH THE SERVICE PROVIDER DATA
			SELECT name
			  INTO :ls_name
			  FROM PROVIDER
			 WHERE provider_no = :ll_ext_id 
			   AND provider_type_code = :ls_rcpnt_type_code
			 USING SQLCA ;
		elseif ls_acode = "E" then
			// NAME IS STORED WITH THE EMPLOYER DATA
			SELECT employer_name
			  INTO :ls_name
			  FROM EMPLOYER_NAME
			 WHERE employer_no = :ll_ext_id 
			   AND employer_name_type_code = 'L'
			 USING SQLCA ;	
		end if
		vii_return_code = SQLCA.nf_handle_error("Select from Employer name ","w_maintain","on cb_ok clicked")			
		if vii_return_code = 0 then
			/* FORMAT THE NAME */
			UPPER ( Trim(ls_name) )
			if vis_field[j,2] = " " then
				vis_field[j,2] = "c.c.~t"+ls_name
			else
				vis_field[j,2] = vis_field[j,2]+"^p~t"+ls_name
			end if
		else
			// Unable to find address associated with at least one individual
			Error.Text        = 'Unable to find address associated with at least one individual.' + &
									  '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.' + &
									  '~r~nDoc ID: ' + String(vistr_correspond_claim.corr.doc_id) + '.' + &
									  '~r~nTemplate Type: ' + vistr_correspond_claim.corr.template_type + '.' 
			Error.Object      = 'w_maintain'
			Error.ObjectEvent = 'wf_process_template'
			SignalError()
		end if
	else
		// Unable to find carbon copy recipient
		Error.Text        = 'Unable to find carbon copy recipient.' + &
		                    '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.' + &
		                    '~r~nDoc ID: ' + String(vistr_correspond_claim.corr.doc_id) + '.' + &
								  '~r~nTemplate Type: ' + vistr_correspond_claim.corr.template_type + '.' 
		Error.Object      = 'w_maintain'
		Error.ObjectEvent = 'wf_process_template'
		SignalError()
	end if
	ll_field_ctr++
NEXT

// Connect to Word
li_rc = ioo_word.uf_connect()
IF li_rc < 0 THEN
	Return -1
END IF

SetPointer(HourGlass!)

// open word as not read-only and make it not visible and open in normal state
li_rc = ioo_word.uf_file_open(ls_physical_file_path,false,false,0)
IF li_rc = OLE_ERROR THEN
	f_populate_ole_error('w_maintain','w_maintain','wf_process_template','OLE',143,'','')
	closeWithReturn(this, 'signal')
	RETURN -1
ELSEIF li_rc < 0 THEN
	// Unable to open MS-Word file
	Error.Text        = 'Unable to open MS-Word file.' + &
							  '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.' + &
							  '~r~nDoc ID: ' + String(vistr_correspond_claim.corr.doc_id) + '.' + &
							  '~r~nPhysical file path: ' + ls_physical_file_path + '.'  + &
							  '~r~nTemplate Type: ' + vistr_correspond_claim.corr.template_type + '.' 
	Error.Object      = 'w_maintain'
	Error.ObjectEvent = 'wf_process_template'
	SignalError()	
END IF

// ACTUAL_FIELDS IS INCREMENTED TO ALLOW FOR THE {ccs} FIELD THAT SHOULD BE PLACED ON EVERY CORRESPONDENCE.
vil_actual_fields += 1

for i = 1 to vil_actual_fields
	// IF no cc's replace {cc} with spaces
	IF vis_field[i,1] = "{cc}" AND Trim(vis_field[i,2]) = ""  AND UPPER(vistr_correspond_claim.corr.cc_allowed_yn) = "Y" THEN
		li_return = ioo_word.uf_end_of_line()
		IF li_return = OLE_ERROR THEN
			f_populate_ole_error('w_maintain','w_maintain','wf_process_template','OLE',158,'','')
			closeWithReturn(this, 'signal')
			RETURN -1
		END IF
		li_return = ioo_word.uf_find_replace(vis_field[i,1],vis_field[i,2],true)
		IF li_return = OLE_ERROR THEN 
			f_populate_ole_error('w_maintain','w_maintain','wf_process_template','OLE',164,'','')
			closeWithReturn(this, 'signal')
			RETURN -1
		ELSEIF li_Return = 0 THEN
			f_populate_ole_error('w_maintain','w_maintain','wf_process_template','FIELD',164,vis_field[i,1],vistr_correspond_claim.corr.template_type)
			closeWithReturn(this, 'signal')
			RETURN -1
		END IF
		li_return = ioo_word.uf_end_of_line()
		IF li_return = OLE_ERROR THEN
			f_populate_ole_error('w_maintain','w_maintain','wf_process_template','OLE',174,'','')
			closeWithReturn(this, 'signal')
			RETURN -1
		END IF
		li_return = ioo_word.uf_add_bookmark("endcc")
		IF li_return = OLE_ERROR THEN
			f_populate_ole_error('w_maintain','w_maintain','wf_process_template','OLE',180,'','')
			closeWithReturn(this, 'signal')
			RETURN -1
		END IF
	END IF

	IF Trim(vis_field[i,2]) <> ""	THEN		// Default value for manual and automated fields is " ", the CC is "".
				
		li_return = ioo_word.uf_find_replace(vis_field[i,1],vis_field[i,2],true)
		IF li_return = OLE_ERROR THEN 
			f_populate_ole_error('w_maintain','w_maintain','wf_process_template','OLE',190,'','')
			closeWithReturn(this, 'signal')
			RETURN -1
		ELSEIF li_Return = 0 THEN
			f_populate_ole_error('w_maintain','w_maintain','wf_process_template','FIELD',190,vis_field[i,1], vistr_correspond_claim.corr.template_type)
			closeWithReturn(this, 'signal')
			RETURN -1
		END IF
		
		do while li_return = 1
			li_return = ioo_word.uf_find_replace(vis_field[i,1],vis_field[i,2],true)
			IF li_return = OLE_ERROR THEN 
				f_populate_ole_error('w_maintain','w_maintain','wf_process_template','OLE',202,'','')
				closeWithReturn(this, 'signal')
				RETURN -1
			END IF
		loop
		
		/* Position the cursor to start searching from the where the startaddress bookmark 
			Add the code to insert an endbookmark for end of cc's */
		IF vis_field[i,1] = "{cc}" and UPPER(vistr_correspond_claim.corr.cc_allowed_yn) = "Y" THEN
			li_return = ioo_word.uf_find(vis_field[i,2])
			IF li_return = OLE_ERROR THEN
				f_populate_ole_error('w_maintain','w_maintain','wf_process_template','OLE',213,'','')
				closeWithReturn(this, 'signal')
				RETURN -1
			END IF
			li_return = ioo_word.uf_end_of_line()
			IF li_return = OLE_ERROR THEN
				f_populate_ole_error('w_maintain','w_maintain','wf_process_template','OLE',219,'','')
				closeWithReturn(this, 'signal')
				RETURN -1
			END IF
			li_return = ioo_word.uf_add_bookmark("endcc")
			IF li_return = OLE_ERROR THEN
				f_populate_ole_error('w_maintain','w_maintain','wf_process_template','OLE',225,'','')
				closeWithReturn(this, 'signal')
				RETURN -1
			END IF
		END IF
	END IF	
next

/* Insert Electronic signature */
IF UPPER(Trim(vistr_correspond_claim.corr.signature_required_yn)) = "Y" THEN
	vis_sgnt_file 	= vistr_correspond_claim.corr.signature_path + vgst_user_profile.user_id + '.bmp'

	// Check if signature exists
	lb_file_exists = FileExists(vis_sgnt_file)

	IF lb_file_exists THEN		
		li_return = ioo_word.uf_go_to_bookmark("signature")
		IF li_return = OLE_ERROR THEN 
			f_populate_ole_error('w_maintain','w_maintain','wf_process_template','OLE',243,'','')
			closeWithReturn(this, 'signal')
			RETURN -1
		ELSEIF li_return = 0 THEN
			f_populate_ole_error('w_maintain','w_maintain','wf_process_template','BKMK',243,'signature',vistr_correspond_claim.corr.template_type)
			closeWithReturn(this, 'signal')
			RETURN -1 // didn't find 'signature' bookmark
		END IF

		vis_sgnt_file 	= vistr_correspond_claim.corr.signature_path + vgst_user_profile.user_id + '.bmp'

		// This sets Selection.ExtendMode to True
		li_return = ioo_word.uf_extend(True)
		IF li_return = OLE_ERROR THEN
			f_populate_ole_error('w_maintain','w_maintain','wf_process_template','OLE',254,'','')
			closeWithReturn(this, 'signal')
			RETURN -1
		END IF

		li_return = ioo_word.uf_new_paragraph(1,13)
		IF li_return = OLE_ERROR THEN
			f_populate_ole_error('w_maintain','w_maintain','wf_process_template','OLE',260,'','')
			closeWithReturn(this, 'signal')
			RETURN -1
		END IF

		// uf_add_pic() - calls activeDocument.InLineShapes.AddPicture(as_file_name, false, true, i_ooword.Selection.Range)
		//                   as_file_name -     The path and file name of the picture.
 		//                   LinkToFile -       False means to make the picture an independent copy of the file. The default value is False.
 		//                   SaveWithDocument - True means to save the linked picture with the document. The default value is False.
 		//                   Range -            The location where the picture will be placed in the text. If the range isn't collapsed, the picture replaces the range; otherwise, the picture is inserted. 
		//                                      If this argument is omitted, the picture is placed automatically.

		li_return = ioo_word.uf_add_pic(vis_sgnt_file)
		IF li_return = OLE_ERROR THEN
			f_populate_ole_error('w_maintain','w_maintain','wf_process_template','OLE',266,'','')
			closeWithReturn(this, 'signal')
			RETURN -1
		END IF

		// Now set Selection.ExtendMode to False so whole document is not selected (next command uf_homekey which calls: Selection.HomeKey(6) <- 6 = A story)  
		li_return = ioo_word.uf_extend(FALSE)
		IF li_return = OLE_ERROR THEN
			f_populate_ole_error('w_maintain','w_maintain','wf_process_template','OLE',254,'','')
			closeWithReturn(this, 'signal')
			RETURN -1
		END IF
	END IF
END IF

li_return = ioo_word.uf_homekey()
IF li_return = OLE_ERROR THEN
	f_populate_ole_error('w_maintain','w_maintain','wf_process_template','OLE',275,'','')
	closeWithReturn(this, 'signal')
	RETURN -1
END IF

IF IsValid(ioo_word) THEN
	ioo_word.uf_visible(true)
	ioo_word.uf_win_state('normal')
ELSE
	f_populate_ole_error('w_maintain','w_maintain','wf_process_template','OLE',282,'','')
	closeWithReturn(this, 'signal')
	RETURN -1
END IF

li_return = ioo_word.uf_file_save_as(physical_file_name, ll_format_type)
IF li_return = OLE_ERROR THEN 
	f_populate_ole_error('w_maintain','w_maintain','wf_process_template','OLE',291,'','')
	closeWithReturn(this, 'signal')
	RETURN -1
ELSEIF li_return < 0 then
	// Could not save MS-Word file
	Error.Text        = 'Could not save MS-Word file.' + &
							  '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.' + &
							  '~r~nDoc ID: ' + String(vistr_correspond_claim.corr.doc_id) + '.' + &
							  '~r~nPhysical file path: ' + ls_physical_file_path + '.' + &
							  '~r~nTemplate Type: ' + vistr_correspond_claim.corr.template_type + '.' 
	Error.Object      = 'w_maintain'
	Error.ObjectEvent = 'wf_process_template'
	SignalError()
END IF

// Check if filename exists in specified directory
// NOTE - Fileexists can cause a sharing violation until maintenance 4.03
//	if the file your checking is open. Because the
// fileexists function will open a file.
// New Imara Function checks for existence of files in a given mode, returns int 0 if true
//-1 is file not found in mode.

IF NOT FileExists(physical_file_name) THEN
	// MS-Word file does not exist after saving
	Error.Text        = 'MS-Word file does not exist after saving.' + &
							  '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.' + &
							  '~r~nDoc ID: ' + String(vistr_correspond_claim.corr.doc_id) + '.' + &
							  '~r~nPhysical file path: ' + physical_file_name + '.' + &
							  '~r~nTemplate Type: ' + vistr_correspond_claim.corr.template_type + '.'  
	Error.Object      = 'w_maintain'
	Error.ObjectEvent = 'wf_process_template'
	SignalError()
END IF

SQLCA.nf_begin_transaction()

// CREATE A "CORRESPONDENCE" RECORD DERIVED FROM THE TEMPLATE IF ACTION IS "Generate"
INSERT into CORRESPONDENCE
			(claim_no, correspond_no, template_code, user_comments, template_language_code, physical_file_name, doc_id)
VALUES
		(:vistr_correspond_claim.claim_no, :vistr_correspond_claim.corr.corr_no, :vistr_correspond_claim.corr.template_type,
		 :vistr_correspond_claim.corr.comments, :vistr_correspond_claim.corr.version_no, :vistr_correspond_claim.corr.document_name, :vistr_correspond_claim.corr.doc_id)
USING	SQLCA;
SQLCA.nf_handle_error("Embedded SQL INSERT on CORRESPONDENCE ","w_maintain","on wf_process_template for cb_ok clicked")	


vii_return_code = wf_process_recipients()
if vii_return_code < 0 then 	Return -1

li_rc = wf_create_imara_index()
if li_rc < 0 then
	SQLCA.nf_rollback_transaction()	
	Return -1
end if

SetPointer(HourGlass!)

// commit both txns
SQLCA.nf_commit_transaction()
ImageTrans.nf_commit_transaction()


// write to the application log
f_populate_app_log(gs_appname,lnv_object_helper.get_app_component_type(THIS),lnv_object_helper.getpath(THIS),'wf_process_template-end')

This.BringToTop = True

Return 0
end function

public function integer wf_create_imara_index ();// ************************************************************************************************
// THIS FUNCTION WILL CREATE AN INDEX RECORD IN IMARA FOR THE GENERATED
// CORRESPONDENCE


	date ld_date
	int	rc	
	SetPointer(HourGlass!)
	ld_date = Date(f_server_datetime())
	

	dw_doc_type.InsertRow(0)
	
	dw_doc_type.SetItem(1,"docid",vistr_correspond_claim.corr.doc_id)
	dw_doc_type.SetItem(1,"date_on_document",ld_date)
	dw_doc_type.SetItem(1,"source_code","I")
	dw_doc_type.SetItem(1,"type_code",vistr_correspond_claim.corr.document_type)
	dw_doc_type.SetItem(1,"imaged_document_flag","N")
	dw_doc_type.SetItem(1,"comment",vistr_correspond_claim.corr.comments)
	dw_doc_type.SetItem(1,"sent_flag","N")
	dw_doc_type.SetItem(1,"service_provider_no",0)
	dw_doc_type.SetItem(1,"service_provider_type_code"," ")
	dw_doc_type.SetItem(1,"english_flag","Y")
	IF Mid(Trim(vistr_correspond_claim.language),1,1) = "F" THEN &
		  	dw_doc_type.SetItem(1,"english_flag","N")
	dw_doc_type.SetItem(1,"reference_no",0)
	dw_doc_type.SetItem(1,"claim_no",vistr_correspond_claim.claim_no)
	dw_doc_type.SetItem(1,"date_received",ld_date)
	
	dw_doc_type.Update()
	ImageTrans.nf_handle_error("dw_doc_type", "w_maintain", "on wf_create_imara_index")
	
	RETURN 0


end function

public function integer wf_vldte_file_name (long al_docid);////////////////////////////////////////////////////////////////////
////
// This function will validate the  file
//	name to see if it already exists 
//
//

STRING ls_doc_text

IF FileExists(physical_file_name) THEN
	// signal error if the file already exists
	IF IsNull(al_docid) THEN 
		ls_doc_text = 'null'
	ELSE
		ls_doc_text = String(al_docid)
	END IF
	Error.Text        = ImageTrans.sqlerrtext
	IF Error.Text = '' THEN
		Error.Text        = 'The document ' + physical_file_name + ' already exists.' + &
		                    '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.' + &
		                    '~r~nDoc ID: ' + ls_doc_text + '.' + &
		                    '~r~nTemplate Type: ' + vistr_correspond_claim.corr.template_type + '.'
	ELSE
		Error.Text        = Error.Text + '.~r~nThe document ' + physical_file_name + ' already exists.' + &
		                    '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.' + &
		                    '~r~nDoc ID: ' + ls_doc_text + '.' + &
		                    '~r~nTemplate Type: ' + vistr_correspond_claim.corr.template_type + '.'
	END IF
	Error.Object      = 'w_maintain'
	Error.is_database = 'IMARA_DB'
	Error.il_dbcode   = ImageTrans.SQLDBCode
	Error.ObjectEvent = 'wf_vldte_file_name'
	SignalError()	
END IF

Return 0
end function

event open;// ************************************************************************************************
// AUTHOR:	MW George
// Modified:	94.10.06
//
// Steps:	1. Set default claimant by using w_recipients (auto - cb_recipients)
//				2. User can modify recipient list by using w_recipients (manual - cb_recipients)
//				3. Produce list of manual fields for users to complete (w_maintain - invisible)
//				4. Process manual and auto fields to produce the piece of correspondence.
//				5. Check that correspondence (file path and name) exists
//				6. Insert data into correspondence and correspondence_recipient tables
//				7. create entry in Imaging
///
//

// ************************************************************************************************
// SET WINDOW VALUES & INSTANCE VARIABLES
long					vrc
int					vli_rc,vli_row_count,vli_row_no
Window 			lw_frame

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')

	
	vistr_correspond_claim = Message.PowerObjectParm
	This.Visible = FALSE
	
// Set database object for drop down datawindow

	dw_document_type_list.SetTransObject(ImageTrans)
	dw_document_type_list.InsertRow(0)

// Get the child datawindw name
	dw_document_type_list.GetChild("type_desc", idw_ddw_document_types)
	viw_sheet = w_frame.GetActiveSheet()
	viw_correspond = vistr_correspond_claim.parent_window
	vistr_correspond_claim.set_default_addressee = False

// Set default values
	vistr_correspond_claim.corr.document_type= "LC"  // HARD CODED FOR NOW
		

	vli_row_no =	idw_ddw_document_types.Find("type_code = '"+vistr_correspond_claim.corr.document_type+"'",1,idw_ddw_document_types.Rowcount())

	vli_rc =	idw_ddw_document_types.Find("type_code = '"+vistr_correspond_claim.corr.document_type+"'",1,idw_ddw_document_types.Rowcount())
	
	If vli_row_no < 0	 OR vli_row_no = 0  then
			MessageBox(this.title,"Unable to set document type you must set it yourself!",INFORMATION!)
		Else
	
//			vistr_correspond_claim.corr.comments =	idw_ddw_document_types.GetItemString(vli_row_no,"description")
			vistr_correspond_claim.corr.document_type =	idw_ddw_document_types.GetItemString(vli_row_no,"type_code")
			dw_document_type_list.SetItem(1,"type_desc",idw_ddw_document_types.GetItemString(vli_row_no,"type_desc"))
			dw_document_type_list.SetItem(1,"comment",vistr_correspond_claim.corr.comments)
			
			
	end if	

// set default claimant automaticaly by emulating the sequence
	vistr_correspond_claim.set_default_addressee = True
	
	cb_recipients.TriggerEvent(Clicked!)
	
	If UPPER(Left(vistr_correspond_claim.corr.version_no,1)) = "F" then this.title = "Composez" 	
	this.title				= this.title + " (#"+String(vistr_correspond_claim.claim_no)+")"
	vinv_imfunction = create n_imaging
	
	// center the window over w_correspond to imitate child / parent window behaviour
	IF IsValid(viw_correspond) THEN
		lw_frame = viw_correspond.ParentWindow().ParentWindow()  // w_frame
		THIS.x = lw_frame.x + (lw_frame.width - THIS.Width)/2
		THIS.y = lw_frame.y + (lw_frame.height - THIS.Height)/2
	END IF
	This.Visible = TRUE
	
	this.Postevent("ue_post_open")

end event

event closequery;IF IsValid (viw_correspond) then
	viw_correspond.Postevent("correspondactivate")
	viw_correspond.wf_reset_buttons(True, "")
END IF

IF ISValid(vinv_imfunction) then destroy vinv_imfunction

w_frame.enabled = True



end event

on w_maintain.create
this.dw_doc_type=create dw_doc_type
this.dw_document_type_list=create dw_document_type_list
this.st_man_entry=create st_man_entry
this.dw_manual_field_entry=create dw_manual_field_entry
this.cb_next=create cb_next
this.cb_prev=create cb_prev
this.mle_ccs=create mle_ccs
this.cbx_preview=create cbx_preview
this.dw_manual_field_list=create dw_manual_field_list
this.cb_man_ok=create cb_man_ok
this.st_to=create st_to
this.st_2=create st_2
this.st_1=create st_1
this.dw_template_fields=create dw_template_fields
this.cb_recipients=create cb_recipients
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
this.gb_1=create gb_1
this.Control[]={this.dw_doc_type,&
this.dw_document_type_list,&
this.st_man_entry,&
this.dw_manual_field_entry,&
this.cb_next,&
this.cb_prev,&
this.mle_ccs,&
this.cbx_preview,&
this.dw_manual_field_list,&
this.cb_man_ok,&
this.st_to,&
this.st_2,&
this.st_1,&
this.dw_template_fields,&
this.cb_recipients,&
this.cb_ok,&
this.cb_cancel,&
this.gb_1}
end on

on w_maintain.destroy
destroy(this.dw_doc_type)
destroy(this.dw_document_type_list)
destroy(this.st_man_entry)
destroy(this.dw_manual_field_entry)
destroy(this.cb_next)
destroy(this.cb_prev)
destroy(this.mle_ccs)
destroy(this.cbx_preview)
destroy(this.dw_manual_field_list)
destroy(this.cb_man_ok)
destroy(this.st_to)
destroy(this.st_2)
destroy(this.st_1)
destroy(this.dw_template_fields)
destroy(this.cb_recipients)
destroy(this.cb_ok)
destroy(this.cb_cancel)
destroy(this.gb_1)
end on

type dw_doc_type from u_dw_online within w_maintain
boolean visible = false
integer x = 384
integer y = 1248
integer height = 360
integer taborder = 80
string dataobject = "d_doc_type"
end type

type dw_document_type_list from u_dw_online within w_maintain
integer x = 101
integer y = 456
integer width = 1646
integer height = 336
integer taborder = 60
string dataobject = "d_document_type_list"
boolean border = false
end type

on itemchanged;// Declare local variables and 
string	ls_col_value,ls_col_selected
string ls_debug
int li_row_nbr	
//// Get the row number and values
//	li_row_nbr = dw_document_type_list.GetRow()
//	ls_col_value = Gettext()
//	ls_col_selected = GetcolumnName()
//	
//	Choose Case ls_col_selected
//		Case "type_code"
///*		changed 1996/10/16 - wrong document type
//			vistr_correspond_claim.corr.document_type = Trim(dw_document_type_list.GetItemString(li_row_nbr,"type_code"))
//*/	
//			vistr_correspond_claim.corr.comments =dw_document_type_list.GetItemString(dw_document_type_list.GetRow(),"comment")
//			dw_document_type_list.SetItem(1,"type_desc",vistr_correspond_claim.corr.document_type)
//			dw_document_type_list.SetItem(1,"comment",vistr_correspond_claim.corr.comments)
//	End Choose
// 
// Get the row number and values
	li_row_nbr = idw_ddw_document_types.GetRow()
	ls_col_value = Gettext()
	ls_col_selected = GetcolumnName()
	
	Choose Case ls_col_selected
		Case "type_desc"
		
			vistr_correspond_claim.corr.document_type =	idw_ddw_document_types.GetItemString(li_row_nbr,"type_code")
//			vistr_correspond_claim.corr.comments =	idw_ddw_document_types.GetItemString(li_row_nbr,"description")
			
			dw_document_type_list.SetItem(1,"type_desc",idw_ddw_document_types.GetItemString(li_row_nbr,"type_code"))
//			dw_document_type_list.SetItem(1,"comment",vistr_correspond_claim.corr.comments)

		Case "comment"

			vistr_correspond_claim.corr.comments = ls_col_value
			dw_document_type_list.SetItem(1,"comment",ls_col_value)	
	End Choose 


end on

type st_man_entry from statictext within w_maintain
boolean visible = false
integer x = 55
integer width = 375
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = " Manual Entry "
alignment alignment = center!
boolean focusrectangle = false
end type

type dw_manual_field_entry from u_dw_online within w_maintain
boolean visible = false
integer x = 27
integer y = 24
integer width = 1783
integer height = 936
integer taborder = 50
string dataobject = "d_manual_field_entry"
boolean vscrollbar = true
end type

on itemchanged;string	vls_value
int		vli_len, vli_curr_row

vli_curr_row = this.GetRow()

vli_len = Len(this.GetText())
IF vli_len = 40 THEN
	MessageBox(this.title,"This field entered has 40 characters. If additional" &
		+ "~n~r information is required it can be entered in the word document")
END IF 
end on

type cb_next from commandbutton within w_maintain
boolean visible = false
integer x = 73
integer y = 976
integer width = 389
integer height = 96
integer taborder = 70
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Page Down"
end type

on clicked;dw_manual_field_entry.ScrollNextPage()


end on

type cb_prev from commandbutton within w_maintain
boolean visible = false
integer x = 475
integer y = 976
integer width = 389
integer height = 96
integer taborder = 90
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "P&age Up"
end type

on clicked;dw_manual_field_entry.ScrollPriorPage()
end on

type mle_ccs from multilineedit within w_maintain
integer x = 439
integer y = 312
integer width = 1234
integer height = 96
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean autovscroll = true
boolean displayonly = true
end type

type cbx_preview from checkbox within w_maintain
boolean visible = false
integer x = 37
integer y = 784
integer width = 334
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "&Preview"
end type

type dw_manual_field_list from u_dw_online within w_maintain
boolean visible = false
integer x = 73
integer y = 1648
integer height = 360
string dataobject = "d_manual_field_list"
end type

type cb_man_ok from commandbutton within w_maintain
boolean visible = false
integer x = 969
integer y = 976
integer width = 384
integer height = 96
integer taborder = 100
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Done"
end type

event clicked;// The following will store all the field id and values in an array, so word 
// can use it to replace the fields in the template - process_template
int		vli_rc, vli_row_cntr
long		vll_row_nbr
string	vls_msg

SetPointer(HourGlass!)

IF dw_manual_field_entry.AcceptText() < 0 THEN
	dw_manual_field_entry.SetFocus()
	Return
END IF

// Disable screen, so the user doesn't re-trigger
cb_cancel.enabled = False
cb_man_ok.enabled = False
cb_prev.enabled = False
cb_next.enabled = False

// Process all entries
FOR vli_row_cntr = 1 TO vii_nbr_rows
	vis_field[vli_row_cntr, 1] = dw_manual_field_entry.GetItemString(vli_row_cntr, "field_id")
	vis_field[vli_row_cntr, 2] = dw_manual_field_entry.GetItemString(vli_row_cntr, "field_value")  	// will default to one blank space
NEXT

vli_rc = wf_process_template()

IF vli_rc < 0 THEN
	Return
END IF

SetPointer(HourGlass!)

// REFRESH THE "CORRESPOND" WINDOW AND THE "MAINTAIN" WINDOW

w_frame.BringToTop = True

Close(parent)

end event

type st_to from statictext within w_maintain
integer x = 439
integer y = 168
integer width = 1234
integer height = 96
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Claimant"
boolean border = true
boolean focusrectangle = false
end type

type st_2 from statictext within w_maintain
integer x = 142
integer y = 332
integer width = 146
integer height = 64
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "CCs:"
boolean focusrectangle = false
end type

type st_1 from statictext within w_maintain
integer x = 142
integer y = 188
integer width = 110
integer height = 64
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "To:"
boolean focusrectangle = false
end type

type dw_template_fields from u_dw_online within w_maintain
boolean visible = false
integer x = 37
integer y = 880
integer width = 1682
integer height = 592
integer taborder = 30
boolean titlebar = true
string title = "Template Fields"
string dataobject = "d_template_fields"
boolean hscrollbar = true
boolean vscrollbar = true
end type

event dberror;SQLCA.SQLDBCode = sqldbcode
SQLCA.SQLErrText = sqlerrtext
RETURN 1
end event

type cb_recipients from commandbutton within w_maintain
integer x = 215
integer y = 888
integer width = 384
integer height = 96
integer taborder = 10
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&To/CCs..."
end type

event clicked;// AUTHOR:	MW George
// Modified:	94.09.28

w_correspond	vlw_correspond
w_recipients		vlw_recipients

// Keep the window invisible but not disabled
// OPEN THE RECIPIENTS CHILD WINDOW, its the Only window that will make this window visible again.
parent.SetRedraw(False)
parent.Visible=False 

// Save parent window name
vlw_correspond = vistr_correspond_claim.parent_window

// Set Current Window to parent window
vistr_correspond_claim.parent_window = parent

OpenWithParm(w_recipients, vistr_correspond_claim, viw_sheet)

// Validate, if no recipients an error occured
IF gs_rcpnt_lst[1].ws_rcpnt_id = 0 THEN
	Close(parent)
	Return
END IF

// Reset flag
vistr_correspond_claim.set_default_addressee = False

// Set Current Window to previous parent window. Because it's used in the process_template event
vistr_correspond_claim.parent_window = vlw_correspond

end event

type cb_ok from commandbutton within w_maintain
event activatecorrespond pbm_custom01
integer x = 754
integer y = 888
integer width = 384
integer height = 96
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
boolean default = true
end type

event clicked;//***********************************************************************************
// The following will display the document when entering manual fields if the
// user so chooses. It will also setup the manual field entry screen
//***********************************************************************************

int vli_rc, i, vli_curr_row, li_count
STRING ls_claim_role, ls_recipient

/* BR 1.280 The correspondence recipient must not be of type claimant, if the annuity payout letter is being created for a surviving spouse

 IF letter is annuity related, and annuity payout individual <> individual_no, and the To: field is still 'Claimant'
 then make the user replace the addressee with someone other than Claimant. (usually the Surviving Spouse)
*/

IF vistr_correspond_claim.corr.template_type = 'ANNPAY01'  OR &
    vistr_correspond_claim.corr.template_type = 'ANNPAY02'  THEN
	
	SELECT c.claim_role_desc, i.given_names + ' ' + i.last_name
	INTO   :ls_claim_role, :ls_recipient
	FROM   CLAIM_PARTICIPANT b
	JOIN     INDIVIDUAL i on b.individual_no = i.individual_no
	JOIN     Claim_Role c on b.claim_role_code = c.claim_role_code
	WHERE  b.claim_no = :vistr_correspond_claim.claim_no
	AND      i.individual_no = :vistr_correspond_claim.annuity_payout_individual_no
	USING SQLCA;

	SQLCA.nf_Handle_Error("w_maintain","cb_ok.clicked","SELECT from CLAIM_PARTICIPANT...")
	
	IF vistr_correspond_claim.annuity_payout_individual_no <> vistr_correspond_claim.individual_no &
		AND  st_to.text = 'Claimant' THEN
				 
		MESSAGEBOX("Incorrect Correspondence Recipient", "The default recipient for this Annuity Payout Letter is the Claimant.  This Annuity Payout Letter will be created for the " &
		                    	+ "~r~n" +  ls_claim_role + ", "+ ls_recipient + ". Please click the To/CCs... button to  'Replace' the claimant with the correct addressee." &
							+ "~r~n~r~nIf the adressee is not in the list of recipients, please add the new recipient by returning to the main Correspondence window and open the Recipients..window.", Exclamation!)
		                                           
		RETURN
	END IF
	
/* Part of BR 1.180 (see rationale)
If more than one account for the claim, message the user: The Annuity Payout Letter will be created for the (claim role), individual's name.
If you require the letter to be created for a different person/annuity account, please start over.
*/
	SELECT 	count(*)
	INTO     :li_count
	FROM 	ANNUITY_ACCOUNT	a	
	  JOIN    CLAIM_PARTICIPANT  b  on b.individual_no = a.individual_no and b.claim_role_code = a.claim_role_code
	WHERE  	b.claim_no   = :vistr_correspond_claim.claim_no
	AND   	b.claim_role_code in ('C','SS')
	USING SQLCA;
	SQLCA.nf_Handle_Error("w_maintain","cb_ok.clicked","SELECT count(*) from ANNUITY_ACCOUNT...")
	
	IF li_count > 1 THEN		
		IF MESSAGEBOX("Multiple Annuity Accounts", "The Annuity Payout Letter will be created for the " +  ls_claim_role + ", " + ls_recipient + &
							"~r~nIf you require the letter to be created for a different person/annuity account, please click Cancel and start over.", Exclamation!,OKCancel!,2) = 2 THEN
			RETURN
		END IF		
	END IF
	
END IF

SetPointer(HourGlass!)

//  Housekeeping section
dw_template_fields.SetTransObject(SQLCA)
dw_manual_field_entry.SetTransObject(SQLCA)
dw_manual_field_list.SetTransObject(SQLCA)
dw_doc_type.SetTransObject(ImageTrans)
cb_recipients.enabled = FALSE
cb_ok.enabled = FALSE

//	Get the latest document type that was chosen by the user 
IF dw_document_type_list.Accepttext() = -1 THEN
	MessageBox (title,"Unable to accept the document type or comment"&
					+"~n~rTry a different value or limit the comment to 44 chars.!",Information!)
	dw_document_type_list.Setcolumn(dw_document_type_list.getcolumn())
	dw_document_type_list.Setfocus()
	RETurn
END IF

vistr_correspond_claim.corr.comments = dw_document_type_list.GETITEMSTRING(1,"comment")

// Setup template path and name
vis_file_name = vistr_correspond_claim.corr.template_type + vistr_correspond_claim.corr.template_extension

// Determine language
IF Left(vistr_correspond_claim.corr.version_no,1) = 'E' THEN
	vis_doc_path =  vistr_correspond_claim.corr.engl_tmplt_path + vis_file_name
ELSE
	vis_doc_path =  vistr_correspond_claim.corr.french_tmplt_path + vis_file_name
END IF

// Retrieve all fields associated with the template
vil_actual_fields = dw_template_fields.Retrieve(vistr_correspond_claim.corr.template_type, vistr_correspond_claim.corr.version_no)	
SQLCA.nf_handle_error("dw_template_fields","w_maintain","on cb_ok clicked")

// Retrieve all manual fields 
vii_nbr_rows = dw_manual_field_list.Retrieve(vistr_correspond_claim.corr.template_type, vistr_correspond_claim.corr.version_no)
SQLCA.nf_handle_error("dw_manual_field_list","w_manual_entry","on open event")

// Processing Section Begins
IF vii_nbr_rows > 0 THEN // At least 1 manual prompts

// Populate the external datawindow (manual fields) - dw_manual_field_entry, with the field id's
	FOR i = 1 to vii_nbr_rows
		vli_curr_row = dw_manual_field_entry.InsertRow(0)
		dw_manual_field_entry.SetItem(vli_curr_row, "field_id", dw_manual_field_list.GetItemString(i,"template_field_id"))
	NEXT

	// Setup next and previous button if applied
	IF vii_nbr_rows > 3 THEN
		cb_next.visible = True
		cb_prev.visible = True
	END IF

	// Setup screen for manual entry, by making it visible
	this.default	=	False
	mle_ccs.visible = False
	dw_doc_type.visible = False
	st_to.visible = False
	cb_recipients.visible = False
	cb_ok.visible = False
	cb_ok.default = False
	cb_cancel.X = 1372
	cb_cancel.Y = 977
	cb_man_ok.visible = True
	cb_man_ok.default = True
	dw_manual_field_entry.visible = True
	st_man_entry.visible = True
	parent.height = 1197
	dw_manual_field_entry.SetFocus()
ELSE // No manual fields on the template or no fields at all		
	vli_rc = wf_process_template()
	IF vli_rc < 0 THEN
		Return	
	END IF

	Close(parent)
END IF


end event

type cb_cancel from commandbutton within w_maintain
integer x = 1294
integer y = 888
integer width = 384
integer height = 96
integer taborder = 40
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cance&l"
boolean cancel = true
end type

event clicked;close (parent)

end event

type gb_1 from groupbox within w_maintain
integer x = 27
integer y = 24
integer width = 1755
integer height = 840
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Correspondence"
end type

