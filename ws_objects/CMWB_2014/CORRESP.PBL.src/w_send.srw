$PBExportHeader$w_send.srw
$PBExportComments$Entry of method for sending correspondence (i.e. print, fax, or e-mail). Also, creates an entry on the imaging system for imaged claims
forward
global type w_send from window
end type
type dw_unselected_recipients from u_dw_online within w_send
end type
type cb_done from commandbutton within w_send
end type
type cb_add_recipient from commandbutton within w_send
end type
type cbx_remote from checkbox within w_send
end type
type dw_mixcase from uo_mixcase within w_send
end type
type dw_employer from u_dw_online within w_send
end type
type dw_provider from u_dw_online within w_send
end type
type dw_get_address from u_dw_online within w_send
end type
type dw_corr_recipients from u_dw_online within w_send
end type
type cbx_email from checkbox within w_send
end type
type cbx_fax from checkbox within w_send
end type
type cbx_print from checkbox within w_send
end type
type cb_cancel from commandbutton within w_send
end type
type cb_ok from commandbutton within w_send
end type
type dw_list_recipients from u_dw_online within w_send
end type
type gb_2 from groupbox within w_send
end type
type gb_1 from groupbox within w_send
end type
type cb_addtocc from commandbutton within w_send
end type
type gb_recipient_list from groupbox within w_send
end type
type lbx_processes from listbox within w_send
end type
end forward

global type w_send from window
integer x = 46
integer y = 200
integer width = 1445
integer height = 1268
boolean titlebar = true
string title = "Send"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 67108864
boolean righttoleft = true
event ue_postopen pbm_custom01
dw_unselected_recipients dw_unselected_recipients
cb_done cb_done
cb_add_recipient cb_add_recipient
cbx_remote cbx_remote
dw_mixcase dw_mixcase
dw_employer dw_employer
dw_provider dw_provider
dw_get_address dw_get_address
dw_corr_recipients dw_corr_recipients
cbx_email cbx_email
cbx_fax cbx_fax
cbx_print cbx_print
cb_cancel cb_cancel
cb_ok cb_ok
dw_list_recipients dw_list_recipients
gb_2 gb_2
gb_1 gb_1
cb_addtocc cb_addtocc
gb_recipient_list gb_recipient_list
lbx_processes lbx_processes
end type
global w_send w_send

type prototypes
Function ulong CreateToolhelp32Snapshot (ulong dwFlags, ulong th32ProcessID) Library "KERNEL32.DLL"
Function boolean Process32First (ulong hSnapshot, ref PROCESSENTRY32 lppe) Library "KERNEL32.DLL" alias for "Process32First;Ansi"
Function boolean Process32Next (ulong hSnapshot, ref PROCESSENTRY32 lppe) Library "KERNEL32.DLL" alias for "Process32Next;Ansi"
Function boolean CloseHandle (ref ulong hObject) Library "KERNEL32.DLL"
Function boolean TerminateProcess (ulong hProcess, uint uExitCode) LIBRARY "KERNEL32.DLL"
Function boolean GetExitCodeProcess (ulong hProcess, Ref uint lpExitCode) LIBRARY "KERNEL32.DLL"
Function ulong OpenProcess(ulong dwdesiredaccess, boolean binheritHandle,ulong dwprocessid) Library "kernel32.dll"
Function long GetLastError() Library "kernel32.dll"
Function Boolean EnumProcesses(REF processEntry Process, long cb, REF long cbNeeded ) Library "PSAPI.DLL" alias for "EnumProcesses;Ansi"
Function boolean EnumProcessModules( ulong hProcess, REF ModuleEntry Module, long cb, REF long lpcbNeeded ) LIBRARY "PSAPI.DLL" alias for "EnumProcessModules;Ansi"
Function long GetModuleBaseNameA(ulong hProcess, ulong hModule, REF string lpBaseName,long nSize) LIBRARY "PSAPI.DLL" alias for "GetModuleBaseNameA;Ansi"
Function long GetModuleFileNameExA(ulong hProcess, ulong hModule, REF string lpBaseName,long nSize) LIBRARY "PSAPI.DLL" alias for "GetModuleFileNameExA;Ansi"

end prototypes

type variables
s_correspond_claim   vistr_correspond_claim
int  		vii_return_code, vii_total_rows,vii_total_recipients
int		vii_recipients, vii_correspondence
ulong	iul_handle, iul_win_handle
w_correspond	viw_correspond

int		vii_row_cntr
string		is_rcpnt_addr
string		vis_cc_addr[6]
int		vis_total_addr_lines
datetime		vidt_date_time
long		vil_recipient_id
string		vis_rcpnt_type_code, vis_lang
n_imaging	in_imfunction
n_event_log	invo_event_log
n_auto_letter	invo_n_auto_letter
DATE		id_doc_create_date
STRING	is_status
STRING	is_CPPD_template = 'CPPDINFO' 	// This is the CPPD Reminder template

Datastore	ids_reminder		//from the Maintain.pbl
Datastore	ids_reminder_up
N_REMINDERS inv_reminders

n_mail  inv_email
INTEGER  ii_reason
INTEGER  ii_module = 007
STRING   is_app = 'CMWB'

Private:

int	il_row_no, il_row_count
string	is_cc_name[]
window	iw_parent_win
boolean ib_action_on_buttonup = false

long  il_LastClickedRow

u_word ioo_word, ioo_cc_word
long il_send_doc_handle[]
BOOLEAN ib_stop, ib_kill
ulong iul_processid
Environment ie_op_sys

PROCESSENTRY32 ipe_processentry[]
constant ulong TH32CS_SNAPHEAPLIST = 1 //0x00000001
constant ulong TH32CS_SNAPPROCESS  = 2 //0x00000002
constant ulong TH32CS_SNAPTHREAD   = 4 //0x00000004
constant ulong TH32CS_SNAPMODULE   = 8 //0x00000008
constant ulong TH32CS_SNAPALL      = TH32CS_SNAPHEAPLIST + TH32CS_SNAPPROCESS + TH32CS_SNAPTHREAD + TH32CS_SNAPMODULE
constant ulong TH32CS_INHERIT      = 2147483648 //0x80000000
constant ulong  PROCESS_TERMINATE = 1
constant ulong  PROCESS_CREATE_THREAD = 2
constant ulong  PROCESS_SET_SESSIONID = 4
constant ulong  PROCESS_VM_OPERATION = 8
constant ulong  PROCESS_VM_READ = 16
constant ulong  PROCESS_VM_WRITE = 32
constant ulong  PROCESS_DUP_HANDLE = 64
constant ulong  PROCESS_CREATE_PROCESS = 128
constant ulong  PROCESS_SET_QUOTA = 256
constant ulong  PROCESS_SET_INFORMATION = 512
constant ulong  PROCESS_QUERY_INFORMATION = 1024
constant ulong  PROCESS_ALL_ACCESS = 2035711
end variables

forward prototypes
public function integer wf_update_event ()
public function integer wf_insert_cc (string as_addressee_role, long al_addressee_id, string as_addressee_type_code)
public function string wf_get_cc_names (long al_rcpnt_id, string as_addr_loc_code, string as_rcpnt_type_code)
public function integer wf_shift_highlight (long al_aclickedrow)
private function integer wf_add_cc ()
public subroutine wf_get_handle (string as_file_path)
public function integer wf_remote_print ()
public function integer wf_ole_cc (string as_name[])
public function string wf_error (integer ai_error)
public function boolean wf_destroyprocess (unsignedlong aul_processid)
public subroutine wf_kill_word ()
public subroutine wf_2000_close_winword ()
public subroutine wf_nt4_close_winword ()
public function integer wf_update_claim_reminders ()
public function integer wf_update_foe_flag (long al_claim_no, ref string as_flag)
public function integer wf_execute_word (string doc_path)
public function integer wf_store_address (string vls_location_code, integer vli_row_cntr, string vls_role_code)
public function integer wf_process_address ()
end prototypes

event ue_postopen;int	vli_row_cntr,vli_rc, li_msg
long 	row_no, ll_handle
string remote_flag, ls_null
w_send lw_this
DATETIME	ldt_doc_create_date

inv_reminders = create n_reminders

SetNull(ls_null)

If (dw_unselected_recipients.SetTransObject(SQLCA)) <> 1 then &		
		SQLCA.nf_handle_error("w_add_recipient","dw_unselected_recipients","In open event doing SetTransobject")

If vistr_correspond_claim.claim_no <= 0 then 
	MessageBox(title, "A system error has occurred. ~n~r Claim number "+String(vistr_correspond_claim.claim_no)+ "is invalid" &
	+"~n~r Please report this to the Help Desk!",Stopsign!)
	THIS.Triggerevent("close")
END IF	

/*	PR 3532 - When letters are sent after the date on which they were created, the date on the letter is the date the
	letter was created instead of the date the letter was sent. This could cause problems for appeals since the time
	limit for an appeal is based on the date in the letter. If the letter is dated earlier than the date it is actully
	sent, the appeal deadline would be too early. If a letter is being sent more than 2 days after it was created, ask
	user if they want to verify that the date in the letter is today's date.
*/
	SELECT create_date
	INTO   :ldt_doc_create_date
	FROM   DOCUMENT_INDEX
	WHERE  docid = :vistr_correspond_claim.corr.doc_id
	USING  IMAGETRANS;

	IF IMAGETRANS.nf_handle_error("Select from DOCUMENT_INDEX ","w_send","ue_post_open event") < 0 THEN
		RETURN -1
	END IF

	id_doc_create_date = Date(ldt_doc_create_date)
	IF IsValid(viw_correspond.dw_template_list) THEN
		is_status = viw_correspond.dw_generated_list.GetItemString(viw_correspond.dw_generated_list.GetRow(),"status")
	ELSE
		Error.Text        = SQLCA.sqlerrtext
		IF Error.Text = '' THEN
			Error.Text        = 'An error occurred while trying to determine the status of the selected letter.'+ &
									  '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.' + &
			       	   	     '~r~nDoc ID: ' + String(vistr_correspond_claim.corr.doc_id) + '.' + &
		            	        '~r~nTemplate Type: ' + vistr_correspond_claim.corr.template_type + '.'
		ELSE
			Error.Text        = Error.Text + 'An error occurred while trying to determine the status of the selected letter.'+ &
									  '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.' + &
			       	   	     '~r~nDoc ID: ' + String(vistr_correspond_claim.corr.doc_id) + '.' + &
		            	        '~r~nTemplate Type: ' + vistr_correspond_claim.corr.template_type + '.'
		END IF
		Error.is_database = 'CLAIM'
		Error.Object      = 'w_send'
		Error.ObjectEvent = 'ue_postopen'
		SignalError()
	END IF

/*	Letters that are Mailed, Sent or Deleted cannot have their indexed date changed. The below message is
	only to be triggered if the status is unsent (status = ''). (Added for PR 3853, March 2004)
*/
	IF DaysAfter(id_doc_create_date, Date(f_server_datetime())) > 2 AND is_status = '' THEN
		li_msg = MessageBox('Date Verification', 'This letter was created more than two days ago.  Do you want to '+ &
								+ 'edit the date in the letter?', Information!, YesNo!)
		IF li_msg = 1 THEN
			Close(THIS)
			Return
		ELSE
		//	Continue processing
		END IF
	END IF

/*Check the number of recipients vs the number already selected, if it's equal then you've selected all   */

	SELECT count(*)
	INTO	:vii_total_recipients 
	FROM	RECIPIENT_LIST
	WHERE claim_no					=	:vistr_correspond_claim.claim_no
	USING	SQLCA;
	IF SQLCA.nf_handle_error("Select from RECIPIENT_LIST ","w_send","in ue_post_open event") < 0 THEN RETURN -1 

	vii_recipients = dw_corr_recipients.Retrieve(vistr_correspond_claim.claim_no, vistr_correspond_claim.corr.corr_no)
	IF SQLCA.nf_handle_error("dw_corr_recipients","w_send","in ue_post_open") < 0 THEN RETURN -1

	vii_total_rows = dw_list_recipients.Retrieve(vistr_correspond_claim.claim_no, vistr_correspond_claim.corr.corr_no)
	vli_rc = SQLCA.nf_handle_error("dw_list_recipients","w_send","on ue_post_open event")
	IF vli_rc < 0 THEN RETURN -1

	dw_unselected_recipients.uf_setselect(3)
	// get ready to bring in the imaging functions
	in_imfunction = create n_imaging
	invo_event_log = create n_event_log
	invo_n_auto_letter = Create N_auto_letter
	
	
	IF vistr_correspond_claim.rcpnt.action ="" then SETNULL(vistr_correspond_claim.rcpnt.action )
	IF ISNULL(vistr_correspond_claim.rcpnt.action) then vistr_correspond_claim.rcpnt.action =" " 
	IF vistr_correspond_claim.rcpnt.action = " " AND vii_total_recipients > vii_total_rows  then
	 	cb_add_recipient.enabled = True
	 Else 
		cb_add_recipient.enabled	=	False
	END IF

	IF isvalid(viw_correspond.dw_template_list) then
			
		row_no =viw_correspond.dw_template_list.find&
		("template_code = '"+ vistr_correspond_claim.corr.template_type+"'",1,viw_correspond.dw_template_list.Rowcount())
		If row_no > 0 then 
			remote_flag = viw_correspond.dw_template_list.GetItemString(row_no,"remote_mail_allowed_yn")
			vistr_correspond_claim.corr.remote_mail_yn = UPPER(remote_flag)
			IF	(vistr_correspond_claim.corr.remote_mail_yn) = 'Y' then
				cbx_remote.Enabled = TRUE
			ELSE
				cbx_remote.Enabled = FALSE
			END IF	
		END IF
	END IF
	
	lw_this = this

	IF f_check_word_doc_open(vistr_correspond_claim.corr.document_name,lw_this) = -99 THEN  // doc is already open
		close(this)
		return
	END IF
	
end event

public function integer wf_update_event ();Long	  ll_event_no,ll_result, ll_count_template, ll_count
Integer li_rc, li_trancount
String  ls_event_specific_code,ls_doc_type,ls_sent_flag, ls_claim_imaged_flag
Datetime	ltdm_today


N_OBJECTHELPER lnv_object_helper

f_populate_app_log(gs_appname,lnv_object_helper.get_app_component_type(THIS),lnv_object_helper.getpath(THIS),'wf_update_event-begin')



ltdm_today = f_server_datetime()
		
// Figure out the event specific code and determine if the document is already sent
SELECT type_code, sent_flag 
  INTO :ls_doc_type, :ls_sent_flag
  FROM DOCUMENT_INDEX
 WHERE claim_no = :vistr_correspond_claim.claim_no 
   AND docid = :vistr_correspond_claim.corr.doc_id	
using IMAGETRANS ;
ImageTrans.nf_handle_error('w_send', 'embedded SQL: SELECT DOCUMENT_INDEX', 'wf_update_event')

IF IMAGETRANS.SQLCODE <> 0 then return -2

IF ls_sent_flag	= 'Y' then return 1 
ls_event_specific_code = in_imfunction.nf_choose_event_specific(ls_doc_type)


f_populate_app_log(gs_appname,lnv_object_helper.get_app_component_type(THIS),lnv_object_helper.getpath(THIS),'wf_update_event - 1')

SQLCA.nf_begin_transaction()

// CREATE A CLAIM EVENT  RECORD FOR FOR THE CORRESPONDENCE BEING SENT.  Event log table is an independent commit.
//	Correspondence and Document table are dependent commits.
ll_event_no = invo_event_log.nf_next_claim_event_no(vistr_correspond_claim.claim_no)
IF ll_event_no > 0 THEN
	li_rc = invo_event_log.nf_create_auto_event(vistr_correspond_claim.claim_no,ll_event_no,'005',&
				ls_doc_type+" "+vistr_correspond_claim.corr.comments,ls_event_specific_code)

// Return zero only if successful
	IF li_rc = 0 THEN
		SQLCA.nf_commit_transaction()
	END IF	
ELSEIF ll_event_no < 0 or ll_event_no = 0 then
	SQLCA.nf_rollback_transaction()
	RETURN -1
END IF

f_populate_app_log(gs_appname,lnv_object_helper.get_app_component_type(THIS),lnv_object_helper.getpath(THIS),'wf_update_event - 2')

SQLCA.nf_begin_transaction()

//	UPDATE CORRESPONDECE in CLAIM_DB and DOUMENT_INDEX in IMARA_DB
UPDATE CORRESPONDENCE 
	SET correspond_status_code = "S",
		 event_no =	:ll_event_no
 WHERE claim_no = :vistr_correspond_claim.claim_no 
   AND correspond_no = :vistr_correspond_claim.corr.corr_no	
 USING SQLCA ;

// expect one row to be updated
vii_return_code = SQLCA.nf_handle_error('w_send', 'embedded SQL: UPDATE CORRESPONDENCE', 'wf_update_event', 1)


SELECT imaged_flag
  INTO :ls_claim_imaged_flag
  FROM CLAIM
 WHERE claim_no = :vistr_correspond_claim.claim_no
 USING SQLCA ;

// if an IMARA txn has not been begun by the calling function, then begin one here
IMAGETRANS.nf_transaction_count(li_trancount,0,'','','',FALSE)
IF li_trancount = 0 THEN
	IMAGETRANS.nf_begin_transaction()
END IF

IF ls_claim_imaged_flag = "Y" THEN
	UPDATE DOCUMENT_INDEX 
		SET sent_flag = "Y", imaged_document_flag = "Y"
	 WHERE claim_no = :vistr_correspond_claim.claim_no 
	   AND docid = :vistr_correspond_claim.corr.doc_id
	USING IMAGETRANS ;
	// expect one record to be updated
	vii_return_code = ImageTrans.nf_handle_error('w_send', 'embedded SQL: UPDATE DOCUMENT_INDEX', 'wf_update_event', 1)
ELSE
	UPDATE DOCUMENT_INDEX 
		SET sent_flag = "Y"
	 WHERE claim_no = :vistr_correspond_claim.claim_no 
	   AND docid = :vistr_correspond_claim.corr.doc_id
	USING IMAGETRANS ;
	// expect one record to be updated
	vii_return_code = ImageTrans.nf_handle_error('w_send', 'embedded SQL: UPDATE DOCUMENT_INDEX', 'wf_update_event', 1)
END IF


SELECT count(*)
INTO    :ll_count_template
FROM   CORRESPONDENCE 
WHERE claim_no = :vistr_correspond_claim.claim_no 
    AND  correspond_no = :vistr_correspond_claim.corr.corr_no
	AND event_no = :ll_event_no
	AND template_code = :is_CPPD_template
 USING SQLCA ;
 
SQLCA.nf_handle_error("ERROR","w_send","wf_update_event - CORRESPONDENCE")

IF ll_count_template > 0 THEN 
	//Create / Update claim reminders
	IF wf_update_claim_reminders() >= 0 THEN
		
		// Send CPPD Claim Reminder Auto Event to CPPD Reminders Bucket IF Active Claim with an LTD opening.
		SELECT  Count(*)
		INTO		:ll_count
		FROM 	CLAIM , OPENING
		WHERE 	CLAIM.claim_no = OPENING.claim_no
		AND		CLAIM.claim_no = :vistr_correspond_claim.claim_no 
		AND		CLAIM.claim_status_code = 'A'
		AND		OPENING.opening_type_code = 'LTD'
		AND		(OPENING.benefit_end_date IS NULL OR OPENING.benefit_end_date > :ltdm_today)
		USING	SQLCA;

		SQLCA.nf_handle_error("ERROR","w_send","wf_update_event - CLAIM & OPENING")

		IF ll_count > 0 THEN
			// this function commits imagetrans
			invo_n_auto_letter.uf_send_doc(vistr_correspond_claim.claim_no, ll_event_no, 'CPPD')
		END IF	
		
	END IF	
END IF

// commit for wf_update_claim_reminders above
SQLCA.nf_commit_transaction()

IMAGETRANS.nf_commit_transaction()


f_populate_app_log(gs_appname,lnv_object_helper.get_app_component_type(THIS),lnv_object_helper.getpath(THIS),'wf_update_event-end')


Return 0		
end function

public function integer wf_insert_cc (string as_addressee_role, long al_addressee_id, string as_addressee_type_code);Datetime	ldt_insert_date

ldt_insert_date = f_server_datetime()

/* Validate parameters  */
IF ISNULL(al_addressee_id) OR al_addressee_id < 0 THEN
	RETURN -1
END IF

IF as_addressee_role = "" THEN
	SETNULL(as_addressee_role)
END IF

IF TRIM(as_addressee_role) = " " OR ISNULL(as_addressee_role) THEN
	Return -1
END IF

IF as_addressee_type_code = "" THEN
	SETNULL(as_addressee_type_code)
END IF

IF TRIM(as_addressee_type_code) = " " OR ISNULL(as_addressee_type_code) THEN
	RETURN -1 
END IF

INSERT CORRESPONDENCE_RECIPIENT (claim_no, correspond_no, correspond_recipient_id, addressee_role_code,
											correspond_recipient_type_cd)
VALUES (:vistr_correspond_claim.claim_no, :vistr_correspond_claim.corr.corr_no, :al_addressee_id, 
		  :as_addressee_role, :as_addressee_type_code)
USING	SQLCA ;

IF	(SQLCA.nf_handle_error("Insert into Correspondence Recipient ", "w_send","in wf_insert_cc_recipients")) < 0 THEN
	Return -1
END IF

RETURN 0
end function

public function string wf_get_cc_names (long al_rcpnt_id, string as_addr_loc_code, string as_rcpnt_type_code);String  ls_name, ls_oname
Integer li_rtn

Choose Case (as_addr_loc_code)
	Case("C")  // NAME IS STORED LOCALLY
		SELECT name1, name2
		  INTO :ls_name, :ls_oname
		  FROM RECIPIENT_ADDRESS
		 WHERE correspond_recipient_id = :al_rcpnt_id 
		   AND active_flag = 'Y'
		USING SQLCA ;
	Case "P"   // NAME IS STORED WITH THE SERVICE PROVIDER DATA
		SELECT name
		  INTO :ls_name
		  FROM PROVIDER
		 WHERE provider_no = :al_rcpnt_id 
		   AND provider_type_code = :as_rcpnt_type_code
		 USING SQLCA ;
	Case "E"    // NAME IS STORED WITH THE EMPLOYER DATA
		SELECT employer_legal_name
		  INTO :ls_name
		  FROM EMPLOYER
		 WHERE employer_no = :al_rcpnt_id
		 USING SQLCA ;	
END CHOOSE

li_rtn = SQLCA.nf_handle_error("SQL Select","w_send","in wf_get_cc_name")	

IF ls_oname = "" THEN 
	SETNULL(ls_oname)	
END IF

IF NOT ISNULL(ls_oname) THEN 
	ls_name = ls_name + ls_oname 
END IF

IF li_rtn <> 0 THEN
	ls_name = "" 
END IF

Return(ls_name)
end function

public function integer wf_shift_highlight (long al_aclickedrow);//	This function will verify that there is a prior selected row and
//	then highlight all Rows between the two.  If there is no previously
//	Selected row then it will highlight only the row clicked.  
//	This function will not unhighlight any other rows to allow for a 
//	mix of shift and Control key inter mingling.  This will have to be
//	aware of the relation between the rows to know which way to 
//	highlight.
//
//	The arguement passed will be the currently clicked row.  This 
//	function will use the existing DataWindow and the instance variable
//	iLastClickedRow to perform it's scrolling.

Integer li_Idx

// File manager functionality ... turn off all rows then select new range
dw_unselected_recipients.setredraw(false)
dw_unselected_recipients.selectrow(0,false)

If il_lastclickedrow = 0 then
//	dw_unselected_recipients.SelectRow(al_aclickedrow,TRUE)
	dw_unselected_recipients.setredraw(true)
	Return 1
end if

// Selection moving backward
if il_lastclickedrow > al_aclickedrow then
	For li_Idx = il_lastclickedrow to al_aclickedrow STEP -1
	dw_unselected_recipients.selectrow(li_Idx,TRUE)	
	end for	
else
// Selection moving forward
	For li_Idx = il_lastclickedrow to al_aclickedrow 
		dw_unselected_recipients.selectrow(li_Idx,TRUE)	
	next	
end if

dw_unselected_recipients.setredraw(true)
Return 1
end function

private function integer wf_add_cc ();String  ls_label, ls_rcpnt_type_code, ls_address_location_code, ls_rcpnt_sub_type_code
Boolean lb_error = False
Long    ll_recipient_id, ll_recipient_no, ll_select_row_no, ll_row_no, ll_delete_row[]
Integer li_rc, i=1

// Get selected rows to start the loop
ll_select_row_no=dw_unselected_recipients.GetSelectedRow(0)

// il_row_count is the number of overall recipients that haven't been selected
Do UNTIL (ll_select_row_no = 0 or lb_error)
	// Get the selected items 
	ls_label = dw_unselected_recipients.GetItemString(ll_select_row_no,"label")
	ls_rcpnt_sub_type_code = dw_unselected_recipients.GetItemString(ll_select_row_no,"rl_recipient_subtyp_cd")
	ls_rcpnt_type_code = dw_unselected_recipients.GetItemString(ll_select_row_no,"rl_recipient_type_cd")
	ll_recipient_id = dw_unselected_recipients.GetItemNumber(ll_select_row_no,"rl_recipient_id")
	ls_address_location_code = dw_unselected_recipients.GetItemString(ll_select_row_no,"address_location_code")
	ll_recipient_no = dw_unselected_recipients.GetItemNumber(ll_select_row_no,"rl_recipient_no")
	
	If ls_address_location_code = "P" or ls_address_location_code =	"E" then
		is_cc_name[i] = wf_get_cc_names(ll_recipient_no, ls_address_location_code, ls_rcpnt_sub_type_code) 
	ELSE
		is_cc_name[i] = wf_get_cc_names(ll_recipient_id, ls_address_location_code, ls_rcpnt_sub_type_code)
	END IF
			 
	If is_cc_name[i] = "" then Return -1

	// Insert into carbon copy recipients into Correspondence Recipient Table
	IF wf_insert_cc("C", ll_recipient_id, ls_rcpnt_type_code) < 0 then Return -2 // Database rollback 

	// Get the next item selected
	ll_delete_row[i] = ll_select_row_no
	il_row_count --
	ll_select_row_no = dw_unselected_recipients.GetSelectedRow(ll_select_row_no)
 
	i++
Loop

// Add the cc list to the letter  which should be opened
li_rc = wf_ole_cc(is_cc_name[])

IF li_rc < 0 and li_rc <> -99 then
	Return -1
elseif li_Rc = -99 then // file is open
	return li_rc
end if

// Delete the rows that have been added to recipients
ll_row_no = UpperBound(ll_delete_row)
For i = 1 to ll_row_no
	dw_unselected_recipients.DeleteRow(ll_delete_row[i])
next		

il_row_count = dw_unselected_recipients.Retrieve(vistr_correspond_claim.claim_no,vistr_correspond_claim.corr.corr_no)
IF SQLCA.nf_handle_error("w_add_recipient","dw_unselected_recipients","In open event doing retrieve") < 0 THEN RETURN -1

Return(li_rc)

end function

public subroutine wf_get_handle (string as_file_path);String ls_reverse_file_name, ls_file_name
Integer li_next_element

// get handle of window just opened
// 1st: cut name of document out of full path
ls_reverse_file_name = Reverse(as_file_path)
ls_file_name = Reverse(Left(ls_reverse_file_name, (Pos(ls_reverse_file_name,'\') - 1) ))

ls_file_name = ls_file_name + ' - Microsoft Word'

li_next_element = UpperBound(il_send_doc_handle) + 1
il_send_doc_handle[li_next_element] = FindWindowA('OpusApp',ls_file_name)
end subroutine

public function integer wf_remote_print ();String  ls_errmsg, ls_rc
Integer li_rc
window  lw_parent_window

lw_parent_window = w_send

If Not isValid(invo_n_auto_letter) then 
	Error.Text        = SQLCA.sqlerrtext
	IF Error.Text = '' THEN
		Error.Text        = 'The remote print object is not valid.'+ &
								  '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.'  + &
			                 '~r~nDoc ID: ' + String(vistr_correspond_claim.corr.doc_id) + '.' + &
		                    '~r~nTemplate Type: ' + vistr_correspond_claim.corr.template_type + '.'
	ELSE
		Error.Text        = Error.Text + 'The remote print object is not valid.'+ &
								  '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.'   + &
			                 '~r~nDoc ID: ' + String(vistr_correspond_claim.corr.doc_id) + '.' + &
		                    '~r~nTemplate Type: ' + vistr_correspond_claim.corr.template_type + '.'
	END IF
	Error.is_database = 'CLAIM'
	Error.Object      = 'w_send'
	Error.ObjectEvent = 'wf_remote_print'
	SignalError()
END IF

// this function may ultimately begin & commit txn on SQLCA
li_rc =	invo_n_auto_letter.uf_begin_letter(lw_parent_window)

If li_rc <> 0 then
	IF li_rc <> -99 THEN // doc was not already open
		vistr_correspond_claim.corr.corr_action ='Error'
		ls_rc	=	String(ABS(li_rc))
		SELECT mail_error_code_desc 
		  INTO :ls_errmsg
		  FROM Mail_Error
		 WHERE mail_error_code= :ls_rc 
		 USING SQLCA ;
		SQLCA.nf_handle_error("w_send","Embedded SQL","in clicked for remote print") 
		
		Error.Text        = SQLCA.sqlerrtext
		IF Error.Text = '' THEN
			Error.Text        = 'A remote print error occurred.'+ &
									  '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.' + &
			   	              '~r~nDoc ID: ' + String(vistr_correspond_claim.corr.doc_id) + '.' + &
		     		              '~r~nTemplate Type: ' + vistr_correspond_claim.corr.template_type + '.'
		ELSE
			Error.Text        = Error.Text + 'A remote print error occurred.'+ &
									  '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.' + &
			       	   	     '~r~nDoc ID: ' + String(vistr_correspond_claim.corr.doc_id) + '.' + &
		            	        '~r~nTemplate Type: ' + vistr_correspond_claim.corr.template_type + '.'
		END IF
		Error.is_database = 'CLAIM'
		Error.Object      = 'w_send'
		Error.ObjectEvent = 'wf_remote_print'
		SignalError()
	ELSE
		return li_rc
	END IF
END IF

// Update the action value to Sent
SQLCA.nf_begin_transaction()

//	UPDATE CORRESPONDECE in CLAIM_DB 
UPDATE CORRESPONDENCE 
	SET correspond_status_code = "M" 
 WHERE claim_no = :vistr_correspond_claim.claim_no 
   AND correspond_no = :vistr_correspond_claim.corr.corr_no	
 USING SQLCA;

// expect one record to be updated
SQLCA.nf_handle_error('w_send','embedded SQL: UPDATE CORRESPONDENCE','wf_remote_print', 1)

SQLCA.nf_commit_transaction()

	
// Refresh generated list to show that document has been sent
viw_correspond.Triggerevent("correspondactivate")
vistr_correspond_claim.corr.corr_action =	"MAILED"

Return(0)

end function

public function integer wf_ole_cc (string as_name[]);Long    ll_usage
Integer i, li_return
ulong   lul_handle, lul_win_handle
String  ls_word_command, ls_file_name, ls_cc_names
String  ls_tab, ls_line_ret
w_sheet lw_sheet
boolean lb_found_cc, lb_found
w_send lw_this

ls_tab = Char(9)

ls_file_name = vistr_correspond_claim.corr.document_name
ll_usage = upperbound(as_name)
lw_sheet = w_frame.GetActiveSheet()
iw_parent_win = Parentwindow(lw_sheet)

// Check if word is running, if so close and restart
ioo_cc_word = Create u_word

li_return = ioo_cc_word.uf_connect()
IF li_return < 0 THEN
	Error.Text        = SQLCA.sqlerrtext
	IF Error.Text = '' THEN
		Error.Text        = 'Could not start MS WORD.'+ &
								  '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.' + &
								  '~r~nDoc ID: ' + String(vistr_correspond_claim.corr.doc_id) + '.' + &
		                    '~r~nTemplate Type: ' + vistr_correspond_claim.corr.template_type + '.'
	ELSE
		Error.Text        = Error.Text + 'Could not start MS WORD.'+ &
								  '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.' + &
								  '~r~nDoc ID: ' + String(vistr_correspond_claim.corr.doc_id) + '.' + &
		                    '~r~nTemplate Type: ' + vistr_correspond_claim.corr.template_type + '.'
	END IF
	Error.is_database = 'CLAIM'
	Error.Object      = 'w_send'
	Error.ObjectEvent = 'wf_ole_cc'
	SignalError()
END IF
	
SetPointer(HourGlass!)
If isValid(iw_parent_win) then iw_parent_win.SetMicrohelp("One moment please, while I replace the CC in your document ") 

lw_this = this
li_return = f_check_word_doc_open(ls_file_name,lw_this)

IF li_return < 0 THEN RETURN li_return

li_return = ioo_cc_word.uf_file_open(ls_file_name,false,true,2)
IF li_return = OLE_ERROR THEN 
	f_populate_ole_error('w_send','w_send','wf_ole_cc','OLE',33,'','')
	RETURN -1
ELSEIF li_return < 0 THEN
	Error.Text        = SQLCA.sqlerrtext
	IF Error.Text = '' THEN
		Error.Text        = 'Could not open file "'+ ls_file_name + '"' + &
								  '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.'  + &
								  '~r~nDoc ID: ' + String(vistr_correspond_claim.corr.doc_id) + '.' + &
		                    '~r~nTemplate Type: ' + vistr_correspond_claim.corr.template_type + '.'
	ELSE
		Error.Text        = Error.Text + 'Could not open file "'+ ls_file_name + '"' + &
								  '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.'  + &
								  '~r~nDoc ID: ' + String(vistr_correspond_claim.corr.doc_id) + '.' + &
		                    '~r~nTemplate Type: ' + vistr_correspond_claim.corr.template_type + '.'
	END IF
	Error.is_database = 'CLAIM'
	Error.Object      = 'w_send'
	Error.ObjectEvent = 'wf_ole_cc'
	SignalError()
END IF

// find out if cc's have already been added.
// P10151-100 change the format of cc from cc. to c.c.
li_return = ioo_cc_word.uf_find_replace('c.c.','c.c.',false)  // no replacements, just a find!!
IF li_return = OLE_ERROR THEN 
	f_populate_ole_error('w_send','w_send','wf_ole_cc','OLE',43,'','')
	RETURN -1
END IF
// as an interim measure for the change above, check for original value of: cc. also. NOTE: This code can be removed after a a few weeks from 2008-06-10. 
// Is just for transition period i.e., when a document was created with cc. but later, after this change is migrated, user does a send and new recipients are 
// added to cc list, new code change above is looking for c.c.
//***** remove the block of code below
IF li_return = 0 THEN   // look for previous version of: 'cc.'
	li_return = ioo_cc_word.uf_find_replace('cc.','cc.',false)  // no replacements, just a find!!
	IF li_return = OLE_ERROR THEN 
		f_populate_ole_error('w_send','w_send','wf_ole_cc','OLE',55,'','')
		RETURN -1
	END IF
END IF
// ***** end of removable code

// Assign all the names to a single string variable
For i =  1 to ll_usage
	IF li_return = 1 THEN
		ls_cc_names = char(13) + ls_tab + as_name[i]
	ELSE
		ls_cc_names = 'c.c.' + ls_tab + as_name[i]
	END IF
Next

li_return = ioo_cc_word.uf_go_to_bookmark('endcc')  // endcc
IF li_return = OLE_ERROR THEN 
	f_populate_ole_error('w_send','w_send','wf_ole_cc','OLE',61,'','')
	RETURN -1
ELSEIF li_return = 0 THEN
	f_populate_ole_error('w_send','w_send','wf_ole_cc','BKMK',61,'endcc',vistr_correspond_claim.corr.template_type)
	RETURN -1
END IF

li_return = ioo_cc_word.uf_end_of_line()
IF li_return = OLE_ERROR THEN
	f_populate_ole_error('w_send','w_send','wf_ole_cc','OLE',70,'','')
	RETURN -1
END IF
li_return = ioo_cc_word.uf_extend(TRUE)
IF li_return = OLE_ERROR THEN 
	f_populate_ole_error('w_send','w_send','wf_ole_cc','OLE',75,'','')
	RETURN -1
END IF
li_return = ioo_cc_word.uf_insert_before(ls_cc_names)
IF li_return = OLE_ERROR THEN
	f_populate_ole_error('w_send','w_send','wf_ole_cc','OLE',80,'','')
	RETURN -1
END IF
li_return = ioo_cc_word.uf_del_bookmark('endcc')
IF li_return = OLE_ERROR THEN
	f_populate_ole_error('w_send','w_send','wf_ole_cc','OLE',85,'endcc','')
	RETURN -1
END IF
li_return = ioo_cc_word.uf_extend(FALSE)
IF li_return = OLE_ERROR THEN
	f_populate_ole_error('w_send','w_send','wf_ole_cc','OLE',90,'','')
	RETURN -1
END IF
li_return = ioo_cc_word.uf_end_of_line()
IF li_return = OLE_ERROR THEN
	f_populate_ole_error('w_send','w_send','wf_ole_cc','OLE',95,'','')
	RETURN -1
END IF
li_return = ioo_cc_word.uf_add_bookmark('endcc')
IF li_return = OLE_ERROR THEN
	f_populate_ole_error('w_send','w_send','wf_ole_cc','OLE',100,'','')
	RETURN -1
END IF
// script above ascertains that endcc bookmark is after added cc's

li_return = ioo_cc_word.uf_file_save(ls_file_name)
IF li_return = OLE_ERROR THEN
	f_populate_ole_error('w_send','w_send','wf_ole_cc','OLE',107,'','')
	RETURN -1
END IF
// Close sheet
li_return = ioo_cc_word.uf_file_close()
IF li_return = OLE_ERROR THEN 
	f_populate_ole_error('w_send','w_send','wf_ole_cc','OLE',113,'','')
	RETURN -1
END IF

li_return = ioo_cc_word.uf_disconnect()
IF li_return < 0 then
	Error.Text        = SQLCA.sqlerrtext
	IF Error.Text = '' THEN
		Error.Text        = 'Could not close MS-Word.' + &
								  '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.'  + &
								  '~r~nDoc ID: ' + String(vistr_correspond_claim.corr.doc_id) + '.' + &
		                    '~r~nTemplate Type: ' + vistr_correspond_claim.corr.template_type + '.'
	ELSE
		Error.Text        = Error.Text + 'Could not close MS-Word.' + &
								  '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.'  + &
								  '~r~nDoc ID: ' + String(vistr_correspond_claim.corr.doc_id) + '.' + &
		                    '~r~nTemplate Type: ' + vistr_correspond_claim.corr.template_type + '.'
	END IF
	Error.is_database = 'CLAIM'
	Error.Object      = 'w_send'
	Error.ObjectEvent = 'wf_ole_cc'
	SignalError()
	MessageBox("Close MS Word Error","Could not communicate with MS Word" &
	+ "~n~r~n~rPLEASE CONTACT YOUR SYSTEM ADMINISTRATOR",StopSign!)
	Return -1
END IF


If isValid(iw_parent_win) then iw_parent_win.SetMicrohelp("CC replacement completed... ") 

destroy ioo_cc_word

Return(0)

end function

public function string wf_error (integer ai_error);choose case ai_error
case 0
 Return "The operation completed successfully." //  ERROR_SUCCESS
case 1
 Return "Incorrect function." //  ERROR_INVALID_FUNCTION
case 2
 Return "The system cannot find the file specified." //ERROR_FILE_NOT_FOUND
case 3
 Return "The system cannot find the path specified." // ERROR_PATH_NOT_FOUND
case 4
 Return "The system cannot open the file." //  ERROR_TOO_MANY_OPEN_FILES
case 5
 Return "Access is denied." //  ERROR_ACCESS_DENIED
case 6
 Return "The handle is invalid." //  ERROR_INVALID_HANDLE
case 7
 Return "The storage control blocks were destroyed." //  ERROR_ARENA_TRASHED
case 8
 Return "Not enough storage is available to process this command." // ERROR_NOT_ENOUGH_MEMORY
case 9
 Return "The storage control block address is invalid." // ERROR_INVALID_BLOCK
case 10
 Return "The environment is incorrect." //  ERROR_BAD_ENVIRONMENT
case 11
 Return "An attempt was made to load a program with an incorrect format." // ERROR_BAD_FORMAT
case 12
 Return "The access code is invalid." //  ERROR_INVALID_ACCESS
case 13
 Return "The data is invalid." //  ERROR_INVALID_DATA
case 14
 Return "Not enough storage is available to complete this operation." //ERROR_OUTOFMEMORY
case 15
 Return " The system cannot find the drive specified." //ERROR_INVALID_DRIVE
case 16
 Return "The directory cannot be removed." //  ERROR_CURRENT_DIRECTORY
end choose
Return "Unknown Error " + String(ai_error) + "." //  ERROR_CURRENT_DIRECTORY
end function

public function boolean wf_destroyprocess (unsignedlong aul_processid);uint lui_exitcode
boolean  lb_return
ulong hProcess
any external_return
hprocess = OpenProcess(process_all_access,false,aul_processid)
if hprocess < 1 then
	MessageBox('Process Termination',wf_error(GetLastError()),StopSign!)
	return lb_return
end if
external_return = GetExitCodeProcess(hprocess, lui_exitcode)
if TerminateProcess(hprocess, lui_exitcode) then
	 return true
else
	MessageBox('Process Termination',wf_error(GetLastError()),StopSign!)
end if
return lb_return
end function

public subroutine wf_kill_word ();Choose Case ie_op_sys.OSType
	Case Windows!
		CHOOSE Case ie_op_sys.OSMajorRevision
			Case 4,5,6
				// Windows 95, 98, ME
				wf_2000_close_winword()
			Case Else
				Return  // API calls won't work, so exit
		End Choose
	Case WindowsNT!
		CHOOSE Case ie_op_sys.OSMajorRevision
			Case 4
				// Windows NT4
				wf_nt4_close_winword()
			Case is > 4
				// Windows 2000 or greater
				wf_2000_close_winword()
			Case Else
				Return // API calls won't work, so exit
		End Choose
	Case Else
		Return
END CHOOSE
end subroutine

public subroutine wf_2000_close_winword ();// clean up any OLE Word 2000 processes in Windows 2000
LONG						ll_rows
ULONG					lul_hSnapshot, lul_th32ProcessID, lul_word_proc
INTEGER					li_counter = 1
PROCESSENTRY32		lpe_processentry[]
STRING					ls_exe
BOOLEAN				lb_return

//when using the TH32CS_SNAPPROCESS flag, the lul_th32ProcessID argument is ignored

SetNull (lul_th32ProcessID)
lul_hSnapshot = CreateToolhelp32Snapshot (TH32CS_SNAPPROCESS,lul_th32ProcessID)

// load structure lpe_processentry with values
if lul_hSnapshot > 0 then
	lpe_processentry[li_counter].dwSize = 296
	if Process32First (lul_hSnapshot, lpe_processentry[li_counter]) then
		do
			li_counter = li_counter + 1
			lpe_processentry[li_counter].dwSize = 296
		loop while Process32Next (lul_hSnapshot, lpe_processentry[li_counter])
	end if
	CloseHandle (lul_hSnapshot)
end if

// check structure for 'winword.exe'
ll_rows = UpperBound(lpe_processentry)
FOR li_counter = 1 to ll_rows
	ls_exe = Lower(lpe_processentry[li_counter].szexefile)
	IF ls_exe = 'winword.exe' AND lpe_processentry[li_counter].th32processid <> iul_processid THEN  // a process by this name parents all winwords created using OLE in correspondence
		lul_word_proc = lpe_processentry[li_counter].th32processid
		lb_return = wf_destroyprocess(lul_word_proc)
		iul_processid = lul_word_proc
	END IF
	lbx_processes.AddItem(lpe_processentry[li_counter].szexefile + '~tprocessID: ' + string(lpe_processentry[li_counter].th32processid) + '~tParent processID: ' + string(lpe_processentry[li_counter].th32parentprocessid))
NEXT
end subroutine

public subroutine wf_nt4_close_winword ();/** NT 4 version **/
// This function will shut down all Word processes.
// It searches structure ModuleEntry for those with winword.exe as part of file path
// and closes them.

ProcessEntry lpe_Process // Window structure (array of 500 ulong)...arbitrary value !
ModuleEntry lme_Module // Window structure (array of 100 long)... idem
long ll_proc_size = 2000 // Size of Process
long ll_mod_size = 400 // Size of Module
long ll_proc_needed // returned size of Process
long ll_mod_needed // returned size of Module
long ll_mod_name // size of returned name of the module
long ll_counter_nt4 // for looping through processes
ulong lul_proc_handle // Handle of the process
boolean lb_proc, lb_mod // return codes for EnumProcesses and EnumProcessModules
string ls_mod_name // Name of the module
string ls_mod_base_name
long ll_mod_base_name
uint	lui_exitcode
any la_external_return

lb_proc = EnumProcesses(lpe_Process, ll_proc_size, ll_proc_needed)
if lb_proc then
	for ll_counter_nt4 = 1 to integer(ll_proc_needed/ 4)
		lul_proc_Handle = OpenProcess(PROCESS_ALL_ACCESS,false,lpe_Process.lpIdProcess[ll_counter_nt4])
		lb_mod = EnumProcessModules(lul_proc_handle, lme_Module, ll_mod_size, ll_mod_needed)
		if ll_mod_needed >= 4 then
			ls_mod_name = space(254)
			ll_mod_Name=GetModuleFileNameExA(lul_proc_handle, lme_Module.lpidmodule[1], ls_mod_name ,254)
			if ll_mod_Name > 0 then
				if Pos(Lower(String(ls_mod_name)), 'winword.exe') <> 0 then
					la_external_return = GetExitCodeProcess(lul_proc_handle, lui_exitcode)
					if TerminateProcess(lul_proc_handle, lui_exitcode) then
						 //OK
					else
						MessageBox('Process Termination',wf_error(GetLastError()),StopSign!)
					end if
				end if
			end if
		end if
		CloseHandle(lul_proc_Handle)
	next
end if


end subroutine

public function integer wf_update_claim_reminders ();Long	ll_return, ll_count


ids_reminder = create datastore
ids_reminder.dataobject = 'ds_claim_reminder'
ids_reminder.settransobject(sqlca)

ids_reminder_up = create datastore
ids_reminder_up.dataobject = 'ds_claim_reminder_update'
ids_reminder_up.settransobject(sqlca)


Select Count(*)
Into    :ll_count
From  CLAIM_REMINDER
Where claim_no = :vistr_correspond_claim.claim_no
And    reminder_status_code = 'P'
And    reminder_sub_type_code <> 'FU'
Using  SQLCA;

SQLCA.nf_handle_error("ERROR - w_send","wf_update_claim_reminders","Select CLAIM_REMINDER")

IF ll_count > 0 THEN //Update the Claim Reminder table

	ll_return = inv_reminders.nf_update_reminder( vistr_correspond_claim.claim_no,"C","P")
ELSE
	//Messagebox("Warning","A planned reminder does not exist for this claim and template choosen.",Information!,ok!)
END IF

//Create new reminder one month in the future if one does not already exist.

Select Count(*)
Into    :ll_count
From CLAIM_REMINDER
Where claim_no = :vistr_correspond_claim.claim_no
And    reminder_status_code = 'P'
And    reminder_sub_type_code = 'FU'
Using  SQLCA;

SQLCA.nf_handle_error("ERROR - w_send","wf_update_claim_reminders","Select CLAIM_REMINDER")

IF ll_count < 1 THEN
	ll_return = inv_reminders.nf_create_auto_reminder(vistr_correspond_claim.claim_no,"CPPD","FU")
END IF

Return ll_return




end function

public function integer wf_update_foe_flag (long al_claim_no, ref string as_flag);STRING  ls_flag
INTEGER li_rtn
LONG    ll_sqlcode

SELECT form_of_election_sent_flag
INTO     :as_flag
FROM    DIFFICULT_CLAIM
WHERE  claim_no = :al_claim_no
USING   SQLCA;
SQLCA.nf_handle_error( "w_send","wf_update_foe_flag","SELECT form_of_election_sent_flag")

ll_sqlcode = SQLCA.SQLCode


SQLCA.nf_begin_transaction()

IF ll_sqlcode = 100 THEN
	INSERT DIFFICULT_CLAIM(claim_no,form_of_election_sent_flag,appeal_decision_flag,fatality_flag,complex_adjudication_code)
	VALUES (:al_claim_no, 'Y', 'N', 'N', '')
	USING SQLCA;
	SQLCA.nf_handle_error( "w_send","wf_update_foe_flag","INSERT DIFFICULT_CLAIM")
	
	UPDATE CLAIM
	SET       difficult_claim_flag = 'Y'
	WHERE  claim_no               = :al_claim_no
	USING SQLCA;	
	SQLCA.nf_handle_error( "w_send","wf_update_foe_flag","UPDATE CLAIM")

ELSEIF as_flag <> 'Y' THEN
	
	UPDATE DIFFICULT_CLAIM
	SET       form_of_election_sent_flag = 'Y'
	WHERE  claim_no         = :al_claim_no
	USING   SQLCA;
	SQLCA.nf_handle_error( "w_send","wf_update_foe_flag","UPDATE DIFFICULT_CLAIM")
	
END IF

SQLCA.nf_commit_transaction()
	
RETURN 0
end function

public function integer wf_execute_word (string doc_path);//*******************************************************************************************************************
// The following function will:
//			For every recipient:
//				1. Store the address in an array. (Recipient array for addressee and CC array for CC's)
//				2. Insert the id and name in the correspondence_distribution table
//				3. Open the correspondence in word 
//				4. Process the address information if CC is being processed
//					NOTE - No need to recreate addressee document to print, just
//							 open. Addressee's address is the default copy. 
//				5. Print the document
Integer li_rc = 1, li_printing_count
String  ls_location_code
String  ls_addr_role_code, ls_rcpnt_subtyp_code
string ls_reverse_file_name, ls_file_name, ls_title_bar
Long ll_handle

ioo_word = Create u_word

SetPointer(HourGlass!)

// Determine language used to create correspondence 
vis_lang = vistr_correspond_claim.corr.language_code

li_rc = ioo_word.uf_connect()
IF li_rc < 0 THEN
	Return -1
END IF

FOR vii_row_cntr = 1 TO vii_total_rows
	// Store the address information - The first should always be the addressee
	ls_location_code = dw_list_recipients.GetItemString(vii_row_cntr, "recipient_list_address_locat")
	ls_addr_role_code = dw_list_recipients.GetItemString(vii_row_cntr,"correspondence_recipient_addressee_role_")
	ls_rcpnt_subtyp_code = dw_list_recipients.GetItemString(vii_row_cntr,"correspond_recipient_subtyp_cd")

	li_rc = wf_store_address(ls_location_code, vii_row_cntr, ls_addr_role_code)
	IF li_rc = -1 THEN Continue	// No address found

	// If you don't want to print this guy then skip him
	IF dw_list_recipients.GetItemString(vii_row_cntr,"select_flag") = "N" then CONTINUE
	
	// Open correspondence in word
	li_rc = ioo_word.uf_file_open(doc_path, false,true,2) // path, not readonly, visible, minimized
	if li_rc = OLE_ERROR THEN
		f_populate_ole_error('w_send','w_send','wf_execute_word','OLE',63,'','')
	ELSEIF li_rc < 0 then
		Error.Text        = SQLCA.sqlerrtext
		IF Error.Text = '' THEN
			Error.Text        = 'Could not open "' + doc_path + '" in MS WORD.'+ &
									  '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.' + &
			                    '~r~nDoc ID: ' + String(vistr_correspond_claim.corr.doc_id) + '.' + &
			                    '~r~nTemplate Type: ' + vistr_correspond_claim.corr.template_type + '.'
									  
		ELSE
			Error.Text        = Error.Text + 'Could not open "' + doc_path + '" in MS WORD.'+ &
									  '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.'  + &
			                    '~r~nDoc ID: ' + String(vistr_correspond_claim.corr.doc_id) + '.' + &
			                    '~r~nTemplate Type: ' + vistr_correspond_claim.corr.template_type + '.'
		END IF
		Error.is_database = 'CLAIM'
		Error.Object      = 'w_send'
		Error.ObjectEvent = 'wf_execute_word'
		SignalError()
	end if
	
	wf_get_handle(doc_path) // get handle of newly opened word doc & store in instance array.
		
	// Builds blocks of addresses for each carbon-copie not the addressee - "A"
	IF UPPER(ls_addr_role_code) <> "A" THEN	
		li_rc = wf_process_address()
		IF li_rc < 0 THEN
			Return -1
		END IF
	END IF

	// Print the correspondence
	li_rc = ioo_word.Uf_print()
	IF li_rc = OLE_ERROR THEN f_populate_ole_error('w_send','w_send','wf_execute_word','OLE',83,'','')
	
	// Close correspondence without saving, bacause changes might have occurr by adding the two blocks of addresses
	li_rc = ioo_word.uf_file_close()
	if li_rc = OLE_ERROR THEN
		f_populate_ole_error('w_send','w_send','wf_execute_word','OLE',87,'','')
	ELSEIF li_rc < 0 then
		Error.Text        = SQLCA.sqlerrtext
		IF Error.Text = '' THEN
			Error.Text        = 'Could not close "' + doc_path + '" in MS WORD.'+ &
									  '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.' + &
			                    '~r~nDoc ID: ' + String(vistr_correspond_claim.corr.doc_id) + '.' + &
			                    '~r~nTemplate Type: ' + vistr_correspond_claim.corr.template_type + '.' 
		ELSE
			Error.Text        = Error.Text + 'Could not close "' + doc_path + '" in MS WORD.'+ &
									  '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.'  + &
			                    '~r~nDoc ID: ' + String(vistr_correspond_claim.corr.doc_id) + '.' + &
			                    '~r~nTemplate Type: ' + vistr_correspond_claim.corr.template_type + '.'
		END IF
		Error.is_database = 'CLAIM'
		Error.Object      = 'w_send'
		Error.ObjectEvent = 'wf_execute_word'
		SignalError()
	end if
NEXT

li_rc = ioo_word.uf_disconnect()
IF li_rc < 0 THEN
	Return -1
END IF

RETURN li_rc
end function

public function integer wf_store_address (string vls_location_code, integer vli_row_cntr, string vls_role_code);// This function will store the addresses of the addressee and it's carbon-copies to be used 
//	by wf_process_address
Long    ll_recipient_no, ll_claim_no, ll_accident_employer_operation_no
Integer li_rc, li_line_cntr, li_addr_cntr
String  ls_oname, ls_name, ls_aline1, ls_aline2, ls_aline3, ls_tcity
String  ls_prov, ls_tcountry, ls_pc, ls_addr[6], ls_rcpnt_subtyp_code, ls_vrfyd_pc
String  ls_employer_type_code

SetPointer(HourGlass!)

dw_get_address.SetTransObject(SQLCA)
dw_provider.SetTransObject(SQLCA)
dw_employer.SetTransObject(SQLCA)

// Get recipient id to be used by correspondence_distribution table
vil_recipient_id = dw_list_recipients.GetItemNumber(vli_row_cntr,"correspond_recipient_id")
vis_rcpnt_type_code = dw_list_recipients.GetItemString(vli_row_cntr,"correspond_recipient_type_code")

CHOOSE CASE vls_location_code
	CASE 'C'
		vli_row_cntr = dw_get_address.Retrieve(vil_recipient_id)
		li_rc = SQLCA.nf_handle_error("dw_get_address","w_send","on wf_store_address")
		
		IF vli_row_cntr = 0 THEN
			Error.Text        = SQLCA.sqlerrtext
			IF Error.Text = '' THEN
				Error.Text        = 'Unable to find the address associated with recipient ' + String(vil_recipient_id) + '.' + &
										  '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.' + &
			   		              '~r~nDoc ID: ' + String(vistr_correspond_claim.corr.doc_id) + '.' + &
		     		   	           '~r~nTemplate Type: ' + vistr_correspond_claim.corr.template_type + '.'
			ELSE
				Error.Text        = Error.Text + 'Unable to find the address associated with recipient ' + String(vil_recipient_id) + '.' + &
										  '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.' + &
			   	      	        '~r~nDoc ID: ' + String(vistr_correspond_claim.corr.doc_id) + '.' + &
		     		         	     '~r~nTemplate Type: ' + vistr_correspond_claim.corr.template_type + '.'
			END IF
			Error.is_database = 'CLAIM'
			Error.Object      = 'w_send'
			Error.ObjectEvent = 'wf_store_address'
			SignalError()
		END IF

		// Retrieve address data
		ls_aline1 = dw_get_address.GetItemString(vli_row_cntr, "address_line1")
		ls_aline2 =	dw_get_address.GetItemString(vli_row_cntr,"address_line2")
		ls_aline3 =	dw_get_address.GetItemString(vli_row_cntr,"address_line3")
		ls_tcity	= dw_get_address.GetItemString(vli_row_cntr, "city")
		ls_tcountry = dw_get_address.GetItemString(vli_row_cntr,"country")
		ls_name =	dw_get_address.GetItemString(vli_row_cntr,"name1")
		ls_oname = dw_get_address.GetItemString(vli_row_cntr,"name2")	
		ls_pc =	dw_get_address.GetItemString(vli_row_cntr,"postal_code")	
		ls_prov = dw_get_address.GetItemString(vli_row_cntr,"province")	
	CASE 'P'
		ls_rcpnt_subtyp_code = dw_list_recipients.GetItemString(vli_row_cntr,"correspond_recipient_subtyp_cd")
		ll_recipient_no = dw_list_recipients.GetItemNumber(vli_row_cntr,"recipient_list_recipient_no")
		vli_row_cntr = dw_provider.Retrieve(ll_recipient_no, ls_rcpnt_subtyp_code)
		li_rc = SQLCA.nf_handle_error("dw_employer","w_send","on wf_store_address")

		IF vli_row_cntr = 0 THEN
			Error.Text        = SQLCA.sqlerrtext
			IF Error.Text = '' THEN
				Error.Text        = 'Unable to find the address associated with service provider ' + String(ll_recipient_no) + '.' + &
										  '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.' + &
			   		              '~r~nDoc ID: ' + String(vistr_correspond_claim.corr.doc_id) + '.' + &
		     		   	           '~r~nTemplate Type: ' + vistr_correspond_claim.corr.template_type + '.'
			ELSE
				Error.Text        = Error.Text + 'Unable to find the address associated with service provider ' + String(ll_recipient_no) + '.' + &
										  '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.' + &
			   		              '~r~nDoc ID: ' + String(vistr_correspond_claim.corr.doc_id) + '.' + &
		     		   	           '~r~nTemplate Type: ' + vistr_correspond_claim.corr.template_type + '.'
			END IF
			Error.is_database = 'CLAIM'
			Error.Object      = 'w_send'
			Error.ObjectEvent = 'wf_store_address'
			SignalError()
		END IF

		// Retrieve address data
		ls_aline1 = dw_provider.GetItemString(vli_row_cntr, "address_line1")
		ls_aline2 =	dw_provider.GetItemString(vli_row_cntr,"address_line2")
		
		ls_tcity	= dw_provider.GetItemString(vli_row_cntr, "city")
		ls_tcountry = dw_provider.GetItemString(vli_row_cntr,"country_code")
		ls_name = dw_provider.GetItemString(vli_row_cntr,"name")	
		ls_pc =	dw_provider.GetItemString(vli_row_cntr,"postal_code")	
		ls_prov = dw_provider.GetItemString(vli_row_cntr,"prov_state_code")
		ls_oname = dw_provider.GetItemString(vli_row_cntr,"contact_name")
	CASE 'E'
		ll_recipient_no = dw_list_recipients.GetItemNumber(vli_row_cntr,"recipient_list_recipient_no")
		vli_row_cntr = dw_employer.Retrieve(ll_recipient_no)
		li_rc = SQLCA.nf_handle_error("dw_employer","w_send","on wf_store_address")
		IF vli_row_cntr = 0 THEN
			Error.Text        = SQLCA.sqlerrtext
			IF Error.Text = '' THEN
				Error.Text        = 'Unable to find the address associated with employer ' + String(ll_recipient_no) + '.' + &
										  '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.' + &
			   		              '~r~nDoc ID: ' + String(vistr_correspond_claim.corr.doc_id) + '.' + &
		     		   	           '~r~nTemplate Type: ' + vistr_correspond_claim.corr.template_type + '.'
			ELSE
				Error.Text        = Error.Text + 'Unable to find the address associated with employer ' + String(ll_recipient_no) + '.' + &
										  '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.' + &
			   		              '~r~nDoc ID: ' + String(vistr_correspond_claim.corr.doc_id) + '.' + &
		     		   	           '~r~nTemplate Type: ' + vistr_correspond_claim.corr.template_type + '.'
			END IF
			Error.is_database = 'CLAIM'
			Error.Object      = 'w_send'
			Error.ObjectEvent = 'wf_store_address'
			SignalError()
		END IF

		// Get the Accident Employer/Operation Number
		ll_claim_no = dw_list_recipients.GetItemNumber(vli_row_cntr,"claim_no")

		SELECT accident_employer_operation_no 
		  INTO :ll_accident_employer_operation_no
		  FROM CLAIM
		 WHERE claim_no = :ll_claim_no ; 
		SQLCA.nf_handle_error("SQL select on CLAIM","w_send","in wf_store_address")

		// Get the type of Employer
		SELECT employer_type_code 
		  INTO :ls_employer_type_code 
		  FROM EMPLOYER
		 WHERE employer_no = :ll_recipient_no ;

		// Operation name should appear in address for Self-Insured Employers
		IF ls_employer_type_code = "S" THEN
			SELECT operation_name 
			  INTO :ls_oname 
			  FROM OPERATION 
			 WHERE employer_no = :ll_recipient_no 
				AND operation_no = :ll_accident_employer_operation_no ;

			SQLCA.nf_handle_error("SQL select on Employer","w_send","in wf_store_address")
		ELSE
			ls_oname = dw_employer.GetItemString(vli_row_cntr,"employer_address_contact_name")
			IF ls_oname <> "" THEN
				ls_oname = ""			// edythe (99/09/22, pr 817) - Employer contact name shouldn't show (until
			END IF						//										operation/claim address types become available)
		END IF

		// Retrieve address data
		ls_aline1 = dw_employer.GetItemString(vli_row_cntr, "employer_address_address_line1")
		ls_aline2 =	dw_employer.GetItemString(vli_row_cntr,"employer_address_address_line2")
		ls_aline3 =	dw_employer.GetItemString(vli_row_cntr,"employer_address_address_line3")
		ls_tcity	= dw_employer.GetItemString(vli_row_cntr, "employer_address_city")
		ls_tcountry = dw_employer.GetItemString(vli_row_cntr,"employer_address_country")
		ls_name = dw_employer.GetItemString(vli_row_cntr,"employer_name_employer_name")	
		ls_pc = dw_employer.GetItemString(vli_row_cntr,"employer_address_postal_code")	
		ls_prov = dw_employer.GetItemString(vli_row_cntr,"employer_address_province")	
END CHOOSE	

// FORMAT THE ADDRESS
// Clear array
FOR li_line_cntr = 1 TO 6
	vis_cc_addr[li_line_cntr] = ""
NEXT

ls_vrfyd_pc = f_vrfy_pstl_cd(ls_pc, ls_tcountry)	// Verify postal code format

ls_addr[1] = Trim(UPPER(ls_name))
ls_addr[2] = Trim(UPPER(ls_oname))
ls_addr[3] = Trim(UPPER(ls_aline1))
ls_addr[4] = Trim(UPPER(ls_aline2))
ls_addr[5] = Trim(UPPER(ls_aline3))
ls_addr[6] = Trim(UPPER(ls_tcity)+", "+trim(ls_prov)) + "  " + trim(ls_vrfyd_pc)

li_line_cntr = 1		// Initialize counter

// Store the address in the proper array (i.e. address is either the addressee or CC)
IF UPPER(vls_role_code) = 'A'  THEN		// Addressee
	is_rcpnt_addr = ls_addr[1]	// Recipient name
ELSE												// Carbon-copy
	li_line_cntr = 1														// Initialize counter, used to suppress blank lines
	FOR li_addr_cntr = 1 TO 6
		IF Trim(ls_addr[li_addr_cntr]) = "" THEN				// Bypass blank lines in ls_addr array
			CONTINUE		
		END IF
		vis_cc_addr[li_line_cntr] =  ls_addr[li_addr_cntr]
		li_line_cntr = li_line_cntr + 1
	NEXT
END IF

Return 0
end function

public function integer wf_process_address ();// This function applies only to Carbon-Copies (CC's). It will replace the addressee's address
// on the correspondence with the CC's address, then skip a line and put "Original to: addressee's name" 
Integer li_rtn, li_line_cntr, li_counter,li_upper, li_return
String  ls_value


li_return = ioo_word.uf_go_to_bookmark('startaddress')
If li_return = 0 THEN
	f_populate_ole_error('w_send','w_send','wf_process_address','BKMK', 7, 'startaddress',vistr_correspond_claim.corr.template_type)
	RETURN -1
ELSEIF li_Return = OLE_ERROR THEN
	f_populate_ole_error('w_send','w_send','wf_process_address','OLE', 7,'','')
	return -1
END IF

li_Return = ioo_word.uf_new_paragraph(1,13)
IF li_Return = OLE_ERROR THEN
	f_populate_ole_error('w_send','w_send','wf_process_address','OLE', 16,'','')
	return -1
END IF
li_return = ioo_word.uf_Extend_line()
IF li_Return = OLE_ERROR THEN
	f_populate_ole_error('w_send','w_send','wf_process_address','OLE', 21,'','')
	return -1
END IF

li_upper = UpperBound(vis_cc_addr) // maximum of six lines of address

For li_line_cntr = 1 to li_upper 
	If vis_cc_addr[li_line_cntr] = "" then 	SETNULL(vis_cc_addr[li_line_cntr])
	If NOT ISNULL(vis_cc_addr[li_line_cntr])  THEN	/* SKIP IF BLANK LINES  */
		li_return = ioo_word.uf_set_font("Times New Roman",11)
		IF li_Return = OLE_ERROR THEN
			f_populate_ole_error('w_send','w_send','wf_process_address','OLE', 32,'','')
			return -1
		END IF
		li_return = ioo_word.uf_del_bookmark('endaddress')
		IF li_Return = OLE_ERROR THEN
			f_populate_ole_error('w_send','w_send','wf_process_address','BKMK', 37,'endaddress','')
			return -1
		ELSEIF li_Return < 0 THEN 
			f_populate_ole_error('w_send','w_send','wf_process_address','OLE', 37,'',vistr_correspond_claim.corr.template_type)
			return -1
		END IF
		
		IF li_line_cntr = 1 THEN
			li_return = ioo_word.uf_type(vis_cc_addr[1])
			IF li_return = OLE_ERROR THEN
				f_populate_ole_error('w_send','w_send','wf_process_address','OLE', 47,'','')
 				RETURN -1
			END IF
		ELSE
			li_return = ioo_word.uf_insert(vis_cc_addr[li_line_cntr])
			IF li_return = OLE_ERROR THEN
				f_populate_ole_error('w_send','w_send','wf_process_address','OLE', 53,'','')
 				RETURN -1
			END IF
			li_return = ioo_word.uf_set_font("Times New Roman",11) // sets selection to proper font
			IF li_return = OLE_ERROR THEN
				f_populate_ole_error('w_send','w_send','wf_process_address','OLE', 58,'','')
 				RETURN -1
			END IF
		END IF
		li_return = ioo_word.uf_end_of_line()
		IF li_return = OLE_ERROR THEN
			f_populate_ole_error('w_send','w_send','wf_process_address','OLE', 66,'','')
			RETURN -1
		END IF
		li_return = ioo_word.uf_add_bookmark('endaddress')
		IF li_return = OLE_ERROR THEN
			f_populate_ole_error('w_send','w_send','wf_process_address','OLE', 69,'','')
			RETURN -1
		END IF
		li_return = ioo_word.uf_new_paragraph(1,13) // Go down to the next line
		IF li_return = OLE_ERROR THEN
			f_populate_ole_error('w_send','w_send','wf_process_address','OLE', 74,'','')
			RETURN -1
		END IF
	END if
Next

ioo_word.uf_new_paragraph(1,13) // Go down to the next paragraph
ls_value = 'COPY - ORIGINAL TO:'
If UPPER(vistr_correspond_claim.corr.language_code) = 'F' then ls_value = 'COPIE - ORIGINAL ENVOYÉ À :'
ioo_word.uf_extend(true)
IF li_return = OLE_ERROR THEN
	f_populate_ole_error('w_send','w_send','wf_process_address','OLE', 85,'','')
	RETURN -1
END IF
ioo_word.uf_set_font("Times New Roman",11)
IF li_return = OLE_ERROR THEN
	f_populate_ole_error('w_send','w_send','wf_process_address','OLE', 90,'','')
	RETURN -1
END IF
ioo_word.uf_insert(ls_value)
IF li_return = OLE_ERROR THEN
	f_populate_ole_error('w_send','w_send','wf_process_address','OLE', 86,'','')
	RETURN -1
END IF
li_return = ioo_word.uf_extend(true)
IF li_return = OLE_ERROR THEN
	f_populate_ole_error('w_send','w_send','wf_process_address','OLE', 91,'','')
	RETURN -1
END IF
li_return = ioo_word.uf_new_paragraph(1,9) // Go tab 
IF li_return = OLE_ERROR THEN
	f_populate_ole_error('w_send','w_send','wf_process_address','OLE', 96,'','')
	RETURN -1
END IF
li_return = ioo_word.uf_insert(is_rcpnt_addr)
IF li_return = OLE_ERROR THEN
	f_populate_ole_error('w_send','w_send','wf_process_address','OLE', 101,'','')
	RETURN -1
END IF

Return 0
end function

on closequery;
iF IsValid(viw_correspond) then 
	viw_correspond.wf_reset_buttons(True, "")
	viw_correspond.wf_reset_buttonss(viw_correspond.dw_generated_list)
//	viw_correspond.cb_send.SetFocus()
END IF
w_frame.enabled = True

IF IsValid (in_imfunction) then   destroy in_imfunction
IF IsValid (invo_event_log) then   destroy invo_event_log
If IsValid	(invo_n_auto_letter) then Destroy invo_n_auto_letter

end on

event open;// ************************************************************************************************
// AUTHOR:	MW George
// Modified:	94.10.18
//

int		vli_rc,  vli_row_cntr
Window lw_frame
Long ll_handle

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


vistr_correspond_claim = Message.PowerObjectParm

This.Visible = FALSE

dw_corr_recipients.SetTransObject(SQLCA)
If (dw_unselected_recipients.SetTransObject(SQLCA)) <> 1 then &		
		SQLCA.nf_handle_error("w_send","dw_unselected_recipients","In open event doing SetTransobject")
If (dw_list_recipients.SetTransObject(SQLCA)) <> 1 then &		
		SQLCA.nf_handle_error("w_send","dw_list_recipients","In open event doing SetTransobject")		
dw_mixcase.SetTransObject(SQLCA)

viw_correspond = vistr_correspond_claim.parent_window

//
// Produce list of recipients for the correspondence
//
dw_mixcase.InsertRow(0)

IF IsValid(viw_correspond) THEN
	lw_frame = viw_correspond.ParentWindow().ParentWindow()  // w_frame
	THIS.x = lw_frame.x + (lw_frame.width - THIS.Width)/2
	THIS.y = lw_frame.y + (lw_frame.height - THIS.Height)/2
END IF
This.Visible = TRUE

GetEnvironment(ie_op_sys)

this.postevent("ue_postopen")
end event

on w_send.create
this.dw_unselected_recipients=create dw_unselected_recipients
this.cb_done=create cb_done
this.cb_add_recipient=create cb_add_recipient
this.cbx_remote=create cbx_remote
this.dw_mixcase=create dw_mixcase
this.dw_employer=create dw_employer
this.dw_provider=create dw_provider
this.dw_get_address=create dw_get_address
this.dw_corr_recipients=create dw_corr_recipients
this.cbx_email=create cbx_email
this.cbx_fax=create cbx_fax
this.cbx_print=create cbx_print
this.cb_cancel=create cb_cancel
this.cb_ok=create cb_ok
this.dw_list_recipients=create dw_list_recipients
this.gb_2=create gb_2
this.gb_1=create gb_1
this.cb_addtocc=create cb_addtocc
this.gb_recipient_list=create gb_recipient_list
this.lbx_processes=create lbx_processes
this.Control[]={this.dw_unselected_recipients,&
this.cb_done,&
this.cb_add_recipient,&
this.cbx_remote,&
this.dw_mixcase,&
this.dw_employer,&
this.dw_provider,&
this.dw_get_address,&
this.dw_corr_recipients,&
this.cbx_email,&
this.cbx_fax,&
this.cbx_print,&
this.cb_cancel,&
this.cb_ok,&
this.dw_list_recipients,&
this.gb_2,&
this.gb_1,&
this.cb_addtocc,&
this.gb_recipient_list,&
this.lbx_processes}
end on

on w_send.destroy
destroy(this.dw_unselected_recipients)
destroy(this.cb_done)
destroy(this.cb_add_recipient)
destroy(this.cbx_remote)
destroy(this.dw_mixcase)
destroy(this.dw_employer)
destroy(this.dw_provider)
destroy(this.dw_get_address)
destroy(this.dw_corr_recipients)
destroy(this.cbx_email)
destroy(this.cbx_fax)
destroy(this.cbx_print)
destroy(this.cb_cancel)
destroy(this.cb_ok)
destroy(this.dw_list_recipients)
destroy(this.gb_2)
destroy(this.gb_1)
destroy(this.cb_addtocc)
destroy(this.gb_recipient_list)
destroy(this.lbx_processes)
end on

event key;
//IF dw_unselected_recipients.GetClickedColumn() > 0 then dw_unselected_recipients.triggerevent("we_key_down")
end event

event close;Integer li_upper, li_counter

IF NOT ib_kill THEN
	li_upper = UpperBound(il_send_doc_handle)
	
	FOR li_counter = 1 TO li_upper
		IF IsWindow(il_send_doc_handle[li_counter]) <> 0 THEN
			ioo_word.uf_disconnect()
		ELSE
			continue
		END IF
	NEXT
END IF
end event

type dw_unselected_recipients from u_dw_online within w_send
event ue_lbuttonup pbm_dwnlbuttonup
event we_key_down pbm_syskeydown
boolean visible = false
integer x = 1504
integer y = 100
integer width = 1001
integer height = 836
integer taborder = 100
string dataobject = "d_unselected_recipients"
boolean vscrollbar = true
boolean livescroll = false
end type

event ue_lbuttonup;call super::ue_lbuttonup;//////////////////////////////////////////////////////////////////////////////////////////////////
// ue_LButtonUp script for dw_highlight
//////////////////////////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////////////////////////
//		This event will be the controlling event for all of the types of
//		Highlighting that will be done.
//////////////////////////////////////////////////////////////////////////////////////////////////
long	  ll_ClickedRow
string	  ls_KeyDownType


//////////////////////////////////////////////////////////////////////////////////////////////////
//		First make sure the user clicked on a Row.  Clicking on WhiteSpace
//		or in the header will return a clicked row value of 0.  If that 
//		occurs, just leave this event.
//////////////////////////////////////////////////////////////////////////////////////////////////


//In filemanager style, the the click and ctrl-click events on a selected row take place
//on the button up event.
If ib_action_on_buttonup Then
	ib_action_on_buttonup = false

	// (CTRL KEY) keep other rows highlighted and highlight a new row or
	// turn off the currint row highlight
	If Keydown(KeyControl!) then
		this.selectrow(il_lastclickedrow,FALSE)
	Else
		this.SelectRow(0,FALSE)
		this.SelectRow(il_lastclickedrow,TRUE)
	End If

	//last action was deselecting a row , an anchor row is no longer defined
	il_lastclickedrow = 0
End If
end event

event clicked;//
///*	If the current row is the same as the row the user just clicked,
//	a rowfocuschanges will not happen.  Therefore, force one.
//*/
//	long	row_no
//	row_no = this.GetRow()
//
//	If row_no > 0 and row_no = row	 THEN
//		This.TriggerEvent(rowfocuschanged!)
//	End IF
//////////////////////////////////////////////////////////////////////////////////////////////////
// Clicked script for dw_highlight
//////////////////////////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////////////////////////
//		This event will be the controlling event for all of the types of
//		Highlighting that will be done.
//////////////////////////////////////////////////////////////////////////////////////////////////
string	  ls_KeyDownType


//////////////////////////////////////////////////////////////////////////////////////////////////

//		First make sure the user clicked on a Row.  Clicking on WhiteSpace
//		or in the header will return a clicked row value of 0.  If that 
//		occurs, just leave this event.
//////////////////////////////////////////////////////////////////////////////////////////////////

//check for a valid row
If row = 0 then Return

//case of select multiple rows range using the shift key
If Keydown(KeyShift!) then
	wf_Shift_Highlight(row)
	//In filemanager style, the click and ctrl-click events on a selected row take 
	//place on the button up event.

ElseIf this.IsSelected(row) Then
	il_LastClickedRow = row
	ib_action_on_buttonup = true
	
ElseIf Keydown(KeyControl!) then
	// (CTRL KEY) keep other rows highlighted and highlight a new row or
	// turn off the currint row highlight
	il_LastClickedRow = row
	this.SelectRow(row,TRUE)
	
Else
	il_LastClickedRow = row
	this.SelectRow(0,FALSE)
	this.SelectRow(row,TRUE)
END IF	
end event

type cb_done from commandbutton within w_send
boolean visible = false
integer x = 2121
integer y = 1020
integer width = 347
integer height = 96
integer taborder = 140
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Do&ne"
boolean cancel = true
end type

event clicked;int rc
Window lw_frame

SetPointer(HourGlass!)

this.visible=False
gb_recipient_list.Visible=False
cb_addtocc.Visible=False
dw_unselected_recipients.Visible=False
dw_list_recipients.Reset()
vii_total_rows = dw_list_recipients.Retrieve(vistr_correspond_claim.claim_no, vistr_correspond_claim.corr.corr_no)
rc =dw_list_recipients.SetSort("correspondence_recipient_addressee_role_,A,recipient_list_claim_recipie,A")
rc =dw_list_recipients.Sort()
rc =dw_list_recipients.GroupCalc()
cb_ok.Enabled=True
cb_cancel.Enabled=True
If il_row_count <= 0 then cb_add_recipient.Enabled=False Else cb_add_recipient.Enabled=True

rc =Parent.Resize(1473,1261)  // Resize to old width
IF IsValid(viw_correspond) THEN
	lw_frame = viw_correspond.ParentWindow().ParentWindow()  // w_frame
	PARENT.x = lw_frame.x + (lw_frame.width - 1473)/2
	PARENT.y = lw_frame.y + (lw_frame.height - 1261)/2
END IF

cb_ok.SetFocus()

If isValid(iw_parent_win) then iw_parent_win.SetMicrohelp("Ready to Print Letters")



end event

type cb_add_recipient from commandbutton within w_send
integer x = 146
integer y = 1012
integer width = 425
integer height = 96
integer taborder = 60
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Add  Recipient"
end type

event clicked;
int	j
Window lw_frame

SetPointer(HourGlass!)

// ******************************************************************************************************************************
// ENSURE THAT THE TEMPLATE ALLOWS CARBON-COPIES. IF NOT, DON'T ALLOW USER TO CONTINUE
IF UPPER(Trim(vistr_correspond_claim.corr.cc_allowed_yn)) = "N" THEN
	MessageBox(parent.title,"This template type - " + vistr_correspond_claim.corr.template_type + " does not allow carbon-copies." &
	,inFORMATION!)
	Return
END IF
	
	
il_row_count = dw_unselected_recipients.Retrieve(vistr_correspond_claim.claim_no,vistr_correspond_claim.corr.corr_no)
IF SQLCA.nf_handle_error("w_add_recipient","dw_unselected_recipients","In open event doing retrieve") < 0 THEN RETURN -1

If il_row_count <= 0 then
	this.enabled=False
	Return
END IF

Parent.SetReDraw(False)
this.enabled=False
cb_addtocc.Visible=True
cb_addtocc.enabled=True
cb_done.Visible=True
cb_done.Enabled=true
cb_ok.enabled=False
cb_cancel.enabled=False
dw_unselected_recipients.Visible=True

Parent.Resize(2634,1269)
IF IsValid(viw_correspond) THEN
	lw_frame = viw_correspond.ParentWindow().ParentWindow()  // w_frame
	Parent.x = lw_frame.x + (lw_frame.width - 2634)/2
	Parent.y = lw_frame.y + (lw_frame.height - 1269)/2
END IF

Parent.SetReDraw(True)

end event

type cbx_remote from checkbox within w_send
integer x = 992
integer y = 108
integer width = 375
integer height = 72
integer taborder = 30
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Mail Room"
end type

event clicked;int row_no
string	remote_flag
/* You are not allowed to send the to Remote Mail if already Sent or Sent to Remote Mail  */
CHOOSE CASE Trim(vistr_correspond_claim.rcpnt.action)
	CASE " ",""
		IF isvalid(viw_correspond.dw_template_list) then
			
				row_no =viw_correspond.dw_template_list.find&
				("template_code = '"+ vistr_correspond_claim.corr.template_type+"'",1,viw_correspond.dw_template_list.Rowcount())
				If row_no > 0 then remote_flag = viw_correspond.dw_template_list.GetItemString(row_no,"remote_mail_allowed_yn")
		END IF
		vistr_correspond_claim.corr.remote_mail_yn = UPPER(remote_flag)
		IF	(vistr_correspond_claim.corr.remote_mail_yn) <> 'Y' then
			MessageBox("SEND MESSAGE: 511:009","The template "+vistr_correspond_claim.corr.template_type+ " ~n~r is not setup for Remote Correspondence" &
			+"~n~r and cannot be sent to the Mail Room!",None!)
			this.checked=False
			Return
		END IF
	Case Else
		IF NOT ISNULL(vistr_correspond_claim.rcpnt.action) then
			MessageBox("SEND MESSAGE: 511:009","The Correspondence has already been acted on.~n~r "&
			+ " You cannot send to the Mail Room, if its been Sent" &
			+ "~n~r or is already in the mail room!",None!)
			this.checked=False
			Return
		END IF
END CHOOSE

IF this.Checked = True then
		cbx_email.checked	=	False
		cbx_print.checked	=	False
		cbx_fax.checked	=	False

END IF

end event

type dw_mixcase from uo_mixcase within w_send
boolean visible = false
integer x = 302
integer y = 336
integer height = 360
integer taborder = 170
end type

type dw_employer from u_dw_online within w_send
boolean visible = false
integer x = 713
integer y = 312
integer height = 360
integer taborder = 80
string dataobject = "d_employer"
end type

event dberror;SQLCA.SQLDBCode = sqldbcode
SQLCA.SQLErrText = sqlerrtext
RETURN 1

end event

type dw_provider from u_dw_online within w_send
boolean visible = false
integer y = 288
integer height = 360
integer taborder = 150
string dataobject = "d_service_provider"
end type

event dberror;SQLCA.SQLDBCode = sqldbcode
SQLCA.SQLErrText = sqlerrtext
RETURN 1

end event

type dw_get_address from u_dw_online within w_send
boolean visible = false
integer x = 55
integer y = 432
integer height = 360
integer taborder = 130
string dataobject = "d_get_address"
end type

event dberror;SQLCA.SQLDBCode = sqldbcode
SQLCA.SQLErrText = sqlerrtext
RETURN 1

end event

type dw_corr_recipients from u_dw_online within w_send
boolean visible = false
integer x = 91
integer y = 300
integer width = 1408
integer height = 480
integer taborder = 160
string dataobject = "d_corr_ccs"
boolean vscrollbar = true
end type

event dberror;SQLCA.SQLDBCode = sqldbcode
SQLCA.SQLErrText = sqlerrtext
RETURN 1


end event

type cbx_email from checkbox within w_send
integer x = 690
integer y = 108
integer width = 238
integer height = 80
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "eMail"
end type

type cbx_fax from checkbox within w_send
integer x = 411
integer y = 116
integer width = 247
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Fax"
end type

event clicked;IF this.Checked = True then
		cbx_email.checked	=	False
		cbx_fax.checked	=	False
		cbx_print.checked	=	False
END IF
end event

type cbx_print from checkbox within w_send
integer x = 114
integer y = 112
integer width = 261
integer height = 72
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Printer"
boolean checked = true
end type

on clicked;IF this.Checked = True then
		cbx_email.checked	=	False
		cbx_fax.checked	=	False
		cbx_remote.checked	=	False

END IF

end on

type cb_cancel from commandbutton within w_send
integer x = 1019
integer y = 1012
integer width = 347
integer height = 96
integer taborder = 70
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cance&l"
boolean cancel = true
end type

on clicked;

close(parent)
end on

type cb_ok from commandbutton within w_send
integer x = 622
integer y = 1012
integer width = 347
integer height = 96
integer taborder = 50
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
boolean default = true
end type

event clicked;// AUTHOR:	MW George
// Modified:	94.10.24

Integer rc, rowcount, li_rtn
String  ls_physical_file_name, ls_msg
w_sheet vlw_sheet
long  i, ll_handle
STRING	ls_imaged_doc_flag, ls_type_code, ls_comment, ls_source, ls_sent_flag, ls_reviewed_flag, ls_serv_prov_type
STRING	ls_english_flag, ls_flag
INT		li_serv_prov_no, li_trancount
LONG		ll_result, ll_ref_no, ll_count
DATE		ld_server_date
s_email_setup lstr_email_setup

N_PROCESS_RUN_STATUS ln_process_run_status


N_OBJECTHELPER lnv_object_helper

f_populate_app_log(gs_appname,lnv_object_helper.get_app_component_type(THIS),lnv_object_helper.getpath(THIS),'clicked event')


/******************************************************************************************
P10275 - Daytime Payment Processing
- added new function call to prevent updating of tables used by PRODSVCS Payment Processing
- new object N_PROCESS_RUN_STATUS is used to determine status of processes (in progress = Y/N)
- '007' refers to the Correspondence module, '044' refers to the Payment Processing module
******************************************************************************************/
ln_process_run_status = Create N_PROCESS_RUN_STATUS

li_rtn = ln_process_run_status.nf_in_progress('007','044','send',SQLCA)

IF li_rtn = 1 THEN
	// module is blocked
	return
END IF
/******************************************************************************************/

inv_email = CREATE n_mail

SetPointer(HourGlass!)

vidt_date_time = f_server_datetime()
vlw_sheet = w_frame.GetActiveSheet()
If dw_list_recipients.AcceptText() < 1 then
	MessageBox("SEND MESSAGE: 511:009","Error in the list of recipients. ~n~r Please report this to the Help Desk!",Stopsign!) 
 	Return
END IF

// Remote print processing
IF Not cbx_remote.checked AND not cbx_print.checked then 
	MessageBox("SEND MESSAGE: 511:009","Please select a distribution method!",None!) 
	Return
END IF

// Disable Ok button so we don't run this function more than once.
cb_ok.enabled = FALSE
cb_cancel.enabled = FALSE

/*	PR 3532 - When letters are sent after the date on which they were created, the date on the letter is the date the
	letter was created instead of the date the letter was sent. This could cause problems for appeals since the time
	limit for an appeal is based on the date in the letter. If the letter is dated earlier than the date it is actully
	sent, the appeal deadline would be too early. If a letter is being sent more than 2 days after it was created,
	a message will inform the user that the index date will be changed. The index date is the
	DOCUMENT_INDEX.create date. Since the create_date cannot be changed, the record in DOCUMENT_INDEX must be deleted
	and a new record inserted that has the same values as the original.
*/
	ld_server_date = Date(f_server_datetime())
	
/*	Letters that are Mailed, Sent or Deleted cannot have their indexed date changed. The below message is only to
	be triggered if the status is unsent (status = ''). (Added for PR 3853, March 2004)
*/
IF DaysAfter(id_doc_create_date, ld_server_date) > 2 AND is_status = '' THEN
	MessageBox(	'Indexed Date Change', 'This letter was created more than two days ago. The indexed date will '+ & 
					'be changed to the current date.', Information! )

	SELECT	imaged_document_flag,
				type_code,
				comment,
				source_code,
				sent_flag,
				reviewed_flag,
				service_provider_no,
				service_provider_type_code,
				english_flag,
				reference_no
	INTO		:ls_imaged_doc_flag,
				:ls_type_code,
				:ls_comment,
				:ls_source,
				:ls_sent_flag,
				:ls_reviewed_flag,
				:li_serv_prov_no,
				:ls_serv_prov_type,
				:ls_english_flag,
				:ll_ref_no
	FROM		DOCUMENT_INDEX
	WHERE		docid = :vistr_correspond_claim.corr.doc_id
	USING		IMAGETRANS ;
	
	IMAGETRANS.nf_handle_error('w_send','SELECT DOCUMENT_INDEX','cb_ok.clicked')


	IMAGETRANS.nf_begin_transaction()

	DELETE
	FROM	 DOCUMENT_INDEX
	WHERE	 docid = :vistr_correspond_claim.corr.doc_id
	USING  IMAGETRANS ;
	
	// expect one record to be deleted
	IMAGETRANS.nf_handle_error('w_send','DELETE DOCUMENT_INDEX','cb_ok.clicked',1)

	

	INSERT INTO DOCUMENT_INDEX (	docid,
											claim_no,
											imaged_document_flag,
											type_code,
											date_on_document,
											comment,
											source_code,
											sent_flag,
											reviewed_flag,
											service_provider_no,
											service_provider_type_code,
											english_flag,
											date_received,
											reference_no		)
	VALUES ( :vistr_correspond_claim.corr.doc_id,
				:vistr_correspond_claim.claim_no,
				:ls_imaged_doc_flag,
				:ls_type_code,
				:ld_server_date,
				:ls_comment,
				:ls_source,
				:ls_sent_flag,
				:ls_reviewed_flag,
				:li_serv_prov_no,
				:ls_serv_prov_type,
				:ls_english_flag,
				:ld_server_date,
				:ll_ref_no		)
	USING IMAGETRANS ;
		
	// expect one record to be deleted
	IMAGETRANS.nf_handle_error('w_send','INSERT DOCUMENT_INDEX','cb_ok.clicked',1)

END IF

SetPointer(HourGlass!)	
If cbx_remote.checked = True then 
	// For remote mail at least one record must be selected.
	rowcount = dw_list_recipients.Rowcount()
	IF dw_list_recipients.Find("select_flag = 'Y'",1,rowcount) <= 0 Then
		// there may be a txn open for IMARA_DB, so roll it back
		ImageTrans.nf_rollback_transaction()
		MessageBox("SEND MESSAGE: 511:009","At least one recipient must be selected when ~n~r Correspondence is being sent to the Mail Room!",None!) 
		dw_list_recipients.Setcolumn("select_flag")
		cb_ok.enabled = TRUE
		cb_cancel.enabled = TRUE
		Return	
	END IF
	
	If ISvalid(vlw_sheet) then vlw_sheet.SetMicrohelp("One moment please, while I send your document to remote mail!")
	
	
	// BEGINS & COMMITS SQLCA ** BEGINS & COMMITS SQLCA ** BEGINS & COMMITS SQLCA ** BEGINS & COMMITS SQLCA 
	rc = wf_remote_print()

	If rc < 0 then
		IF rc <> -99 THEN // if doc was not already open
			vlw_sheet.SetMicrohelp("An error occurred sending your document to remote mail!")
		ELSE
			// message is taken care of already
		END IF
		Close(parent)
		Return
		
	Else
		IMAGETRANS.nf_transaction_count(li_trancount,0,'','','',FALSE)
		IF li_trancount > 0 THEN
			// commit txn before closing window
			IMAGETRANS.nf_commit_transaction()
		END IF
			
		vlw_sheet.SetMicrohelp("Your document was sent to remote mail successfully!")
		cbx_remote.checked=False
		Close(parent)
		Return
	END IF
	
END IF

// End of Remote Print
cb_add_recipient.enabled=False

// Regular Processing

// Make sure the DOCUMENT is Valid 

ls_physical_file_name = vistr_correspond_claim.corr.document_name

// Check if document exists, before sending
// NOTE - Fileexists can cause a sharing violation until maintenance patch 4.03
//	if the file your checking is open. Because the
// fileexists function will open a file.
// Use new IMARA FUNCTION. Returns a 0 if file exists in the mode specified
//  Modes:  00  Check for existence only
//  			02 	Check for write permission
//				04		Check for read permission
//				06		Check for r/w permission

IF NOT FileExists(ls_physical_file_name) THEN
	Error.Text        = SQLCA.sqlerrtext
	IF Error.Text = '' THEN
		Error.Text        = 'The document "' + ls_physical_file_name + '" cannot be found.' + &
								  '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.' + &
								  '~r~nDoc ID: ' + String(vistr_correspond_claim.corr.doc_id) + '.' + &
     		   	           '~r~nTemplate Type: ' + vistr_correspond_claim.corr.template_type + '.'
	ELSE
		Error.Text        = Error.Text + 'The document "' + ls_physical_file_name + '" cannot be found.' + &
								  '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.' + &
								  '~r~nDoc ID: ' + String(vistr_correspond_claim.corr.doc_id) + '.' + &
     		   	           '~r~nTemplate Type: ' + vistr_correspond_claim.corr.template_type + '.'
	END IF
	Error.is_database = 'CLAIM'
	Error.Object      = 'w_send'
	Error.ObjectEvent = 'cb_ok.clicked'
	SignalError()
END IF

// May not need the window handle anymore.  Used to open a temporary window which caused 
// the current window to loose focus and screwed up the DDE calls. Was passing
//	the window handle with the DDE function calls. Refer to
//	PowerSoft technical tip #4239 
iul_win_handle = Handle(Parent)

ls_msg = "Sending " + Trim(vistr_correspond_claim.corr.template_type) + ", Please wait..."

// Disable main window, so user loses control of the application,
// when communcation with word is timing out. (user can't quit until
// the error is displayed after 60 sec.)
w_frame.enabled = False
cb_add_recipient.enabled=False
	
//	Retrieve the status of the Correspondence 
SELECT correspond_status_code
  INTO :vistr_correspond_claim.corr.corr_action
  FROM CORRESPONDENCE 
 WHERE claim_no = :vistr_correspond_claim.claim_no 
   AND correspond_no = :vistr_correspond_claim.corr.corr_no	
 USING SQLCA;

SQLCA.nf_handle_error("SQL Update","w_send","in wf_update_event")
	
//	Action  Values:	S -	Sent Letter has been printed once and already imaged if an imaged claim. You can only print
//							M -  MAIL ROOM - Letters sent to Mail Room
//							BLANK -	Never Sent. If an imaged claim,
//										1. update status to S
//										2. imaged,print and
//										3. log event
//										4. set action variable = "SENT"
//										If a non-image claim
//											1. Assign Action variable to value "PRINT"			
//											2. Update status to S
//											3. print and
//											4. log event. 	
//										
//
// OPEN AND PRINT WORD DOCUMENT
SetPointer(HourGlass!)
rc = wf_execute_word(ls_physical_file_name)
ll_handle = Handle(parent)
BringWindowToTop(ll_handle)
	
/****************************************************************************************/
/* Add routine here to check to see if wf_execute_word() routine worked correctly!!!!!! */
/****************************************************************************************/
if rc < 0 and rc <> -99 then
	 MessageBox(parent.title,"Error in printing document. Please try again",STOPSIGN!)
ElseIf rc = -99 then  // the doc was already open
	// do nothing
else
	//P10151-66 - J. Hawker, 2007.10.15 - Check if any ELECTION letters have been sent
	IF vistr_correspond_claim.corr.template_type = 'ELECTION' THEN			
		SELECT Count(*)
		INTO     :ll_count
		FROM   CORRESPONDENCE 
		WHERE claim_no                      = :vistr_correspond_claim.claim_no
		AND     template_code               = 'ELECTION'
		AND     correspond_status_code = 'S'
		USING   SQLCA;
	
		SQLCA.nf_handle_error('w_send','SELECT Count(*) from CORRESPONDENCE','cb_ok.clicked')
	
		IF ll_count = 0 THEN
			ii_reason = 001
		ELSEIF ll_count > 0 THEN
			ii_reason = 005	
		END IF
	END IF	
	
	IF vistr_correspond_claim.corr.template_type = 'ELECTION' OR vistr_correspond_claim.corr.template_type = 'FBLETTER' THEN
		//Update the flag in the DIFFICULT_CLAIM table if needed
		
		
		// BEGINS & COMMITS SQLCA ** BEGINS & COMMITS SQLCA ** BEGINS & COMMITS SQLCA ** BEGINS & COMMITS SQLCA 
		
		wf_update_foe_flag(vistr_correspond_claim.claim_no, ls_flag)

		IF ii_reason = 005 AND ls_flag <> 'Y' THEN ii_reason = 001
		
		IF ii_reason = 005 THEN ii_reason = 000		
	END IF
	
	IF vistr_correspond_claim.corr.corr_action = "" then SetNULL(vistr_correspond_claim.corr.corr_action)
	If ISNULL(vistr_correspond_claim.corr.corr_action) or   vistr_correspond_claim.corr.corr_action = " " then
		
		// BEGINS & COMMITS SQLCA & IMARA_DB ** BEGINS & COMMITS SQLCA & IMARA_DB ** BEGINS & COMMITS SQLCA & IMARA_DB ** 		
		rc = wf_update_event() 
		Choose Case (rc)
			Case -1
				Error.Text        = SQLCA.sqlerrtext
				IF Error.Text = '' THEN
					Error.Text        = 'Unable to transfer to imaging due to CLAIM database error.' + &
											  '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.' + &
											  '~r~nDoc ID: ' + String(vistr_correspond_claim.corr.doc_id) + '.' + &
											  '~r~nTemplate Type: ' + vistr_correspond_claim.corr.template_type + '.'
				ELSE
					Error.Text        = Error.Text + 'Unable to transfer to imaging due to CLAIM database error.' + &
											  '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.' + &
											  '~r~nDoc ID: ' + String(vistr_correspond_claim.corr.doc_id) + '.' + &
											  '~r~nTemplate Type: ' + vistr_correspond_claim.corr.template_type + '.' 
				END IF
				Error.is_database = 'CLAIM'
				Error.Object      = 'w_send'
				Error.ObjectEvent = 'cb_ok.clicked'
				SignalError()
			Case -2
				Error.Text        = SQLCA.sqlerrtext
				IF Error.Text = '' THEN
					Error.Text        = 'Unable to transfer to imaging due to IMARA database error.' + &
											  '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.' + &
											  '~r~nDoc ID: ' + String(vistr_correspond_claim.corr.doc_id) + '.'  + &
											  '~r~nTemplate Type: ' + vistr_correspond_claim.corr.template_type + '.'
				ELSE
					Error.Text        = Error.Text + 'Unable to transfer to imaging due to IMARA database error.' + &
											  '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.' + &
											  '~r~nDoc ID: ' + String(vistr_correspond_claim.corr.doc_id) + '.' + &
											  '~r~nTemplate Type: ' + vistr_correspond_claim.corr.template_type + '.'
				END IF
				Error.is_database = 'CLAIM'
				Error.Object      = 'w_send'
				Error.ObjectEvent = 'cb_ok.clicked'
				SignalError()
			Case Else
			// Postive return codes indicate no error and a 1 indicates it's already been logged and indexed
		End Choose
	END IF
	
	// If template requires auto email AND it is not the first FOE letter being sent, set up the email and send
	IF 	vistr_correspond_claim.corr.auto_email_yn = 'Y' AND ii_reason > 0 THEN		
		lstr_email_setup.as_application_code = is_app
		lstr_email_setup.ai_app_module_code = ii_module
		lstr_email_setup.as_template_code = vistr_correspond_claim.corr.template_type
		lstr_email_setup.ai_email_reason_code = ii_reason
		lstr_email_setup.al_claim_no = vistr_correspond_claim.claim_no
		lstr_email_setup.as_subject_type = 'FOE'
		
		IF inv_email.nf_init_values(lstr_email_setup) < 0 THEN
			RETURN -1
		END IF
		IF inv_email.nf_auto_email()	< 0 THEN
			RETURN -1
		END IF	
	END IF
	
/* Refresh generated list to show that document has been sent */
	viw_correspond.Triggerevent("correspondactivate")
// Refresh imara document list by activating refresh button on w_sheet	

	IF IsValid(vlw_sheet) THEN
		 vlw_sheet.cb_refresh_document_list.TriggerEvent(Clicked!)
	/* Turn off the highlighting of the document */
	ELSE
		 MessageBox(parent.title, "Error getting active sheet, Please refresh document viewer manually")
	END IF
	If ISvalid(vlw_sheet) then vlw_sheet.SetMicrohelp("Sending/Printing document completed!")
end if

Close(parent)
end event

type dw_list_recipients from u_dw_online within w_send
integer x = 73
integer y = 360
integer width = 1243
integer height = 552
integer taborder = 110
string dataobject = "d_list_recipients"
boolean vscrollbar = true
end type

event dberror;SQLCA.SQLDBCode = sqldbcode
SQLCA.SQLErrText = sqlerrtext
RETURN 1

end event

event itemerror;RETURN 1
end event

type gb_2 from groupbox within w_send
integer x = 37
integer y = 292
integer width = 1349
integer height = 696
integer taborder = 40
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "SetUp"
end type

type gb_1 from groupbox within w_send
integer x = 37
integer y = 32
integer width = 1349
integer height = 208
integer taborder = 10
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "By"
end type

type cb_addtocc from commandbutton within w_send
boolean visible = false
integer x = 1637
integer y = 1020
integer width = 347
integer height = 96
integer taborder = 120
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "A&ddCc"
end type

event clicked;Integer sel[], i

N_OBJECTHELPER lnv_object_helper


THIS.ENABLED = FALSE
SetPointer(HourGlass!)

FOR i = 1 TO il_row_count
	IF dw_unselected_recipients.ISSELECTED(i) THEN
		sel[i] = i

	END IF
NEXT
vii_return_code = 0

// SetRedaw didn't seem to work in cd_done
IF dw_unselected_recipients.GetSelectedRow(0) = 0 then
	MessageBox("ADD MESSAGE: 513-ADD","PLEASE SELECT AN ITEM OR HIT CANCEL TO CANCEL!", Stopsign!)
	dw_unselected_recipients.Setfocus()
	THIS.ENABLED = TRUE
	RETURN
END IF

f_populate_app_log(gs_appname,lnv_object_helper.get_app_component_type(THIS),lnv_object_helper.getpath(THIS),'clicked event')

SQLCA.nf_begin_transaction()

vii_return_code = wf_add_cc()
SetPointer(Arrow!)
IF vii_return_code <> 0 THEN
	IF vii_return_code = -2 THEN
		SQLCA.nf_rollback_transaction()
	ELSEIF vii_return_code = -99 THEN
		ImageTrans.nf_rollback_transaction()
		SQLCA.nf_rollback_transaction()
		Close(parent)
		RETURN
	END IF
	
	Error.Text        = SQLCA.sqlerrtext
	IF Error.Text = '' THEN
		Error.Text        = 'Errors occurred while adding new recipients.' + &
								  '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.' + &
								  '~r~nDoc ID: ' + String(vistr_correspond_claim.corr.doc_id) + '.' + &
     		   	           '~r~nTemplate Type: ' + vistr_correspond_claim.corr.template_type + '.'
	ELSE
		Error.Text        = Error.Text + 'Errors occurred while adding new recipients.' + &
								  '~r~nClaim Number: ' + String(vistr_correspond_claim.claim_no) + '.' + &
								  '~r~nDoc ID: ' + String(vistr_correspond_claim.corr.doc_id) + '.' + &
     		   	           '~r~nTemplate Type: ' + vistr_correspond_claim.corr.template_type + '.'
	END IF
	Error.is_database = 'CLAIM'
	Error.Object      = 'w_send'
	Error.ObjectEvent = 'cb_ok.clicked'
	SignalError()
ELSE
	SQLCA.nf_commit_transaction()
	IF isValid(iw_parent_win) THEN iw_parent_win.SetMicrohelp("Completed adding CC")
END IF

IF il_row_count <= 0 THEN 
	cb_done.postevent("clicked")
ELSE
	dw_list_recipients.Reset()
	dw_list_recipients.Retrieve(vistr_correspond_claim.claim_no, vistr_correspond_claim.corr.corr_no)
	dw_list_recipients.SetSort("correspondence_recipient_addressee_role_,A,recipient_list_claim_recipie,A")
	dw_list_recipients.Sort()
	dw_list_recipients.GroupCalc()
END IF

THIS.ENABLED = TRUE
end event

type gb_recipient_list from groupbox within w_send
boolean visible = false
integer x = 1440
integer y = 28
integer width = 1134
integer height = 960
integer taborder = 90
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Recipient List"
end type

type lbx_processes from listbox within w_send
boolean visible = false
integer x = 9
integer y = 32
integer width = 1394
integer height = 1140
integer taborder = 150
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

