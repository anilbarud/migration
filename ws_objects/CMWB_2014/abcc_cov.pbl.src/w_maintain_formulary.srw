$PBExportHeader$w_maintain_formulary.srw
$PBExportComments$used to maintain formulary information based on claim information for EPAY project P10236
forward
global type w_maintain_formulary from w_a_tool
end type
type dw_formulary from u_dw_online within w_maintain_formulary
end type
type cb_save from commandbutton within w_maintain_formulary
end type
type cb_cancel from commandbutton within w_maintain_formulary
end type
type uo_image_append from u_image_append within w_maintain_formulary
end type
type cb_switch from commandbutton within w_maintain_formulary
end type
type cb_delete from commandbutton within w_maintain_formulary
end type
type cb_log from commandbutton within w_maintain_formulary
end type
type cb_build from commandbutton within w_maintain_formulary
end type
type cb_add from commandbutton within w_maintain_formulary
end type
type cb_delete_elig from commandbutton within w_maintain_formulary
end type
type dw_eligibility from u_dw_online within w_maintain_formulary
end type
type tab_pages from tab within w_maintain_formulary
end type
type tabpage_rx_din from userobject within tab_pages
end type
type dw_rx from u_dw_online within tabpage_rx_din
end type
type tabpage_rx_din from userobject within tab_pages
dw_rx dw_rx
end type
type tabpage_rxdin_individ from userobject within tab_pages
end type
type dw_formulary_pres_individual from u_dw_online within tabpage_rxdin_individ
end type
type tabpage_rxdin_individ from userobject within tab_pages
dw_formulary_pres_individual dw_formulary_pres_individual
end type
type tabpage_event from userobject within tab_pages
end type
type dw_event from u_dw_online within tabpage_event
end type
type tabpage_event from userobject within tab_pages
dw_event dw_event
end type
type tabpage_sa from userobject within tab_pages
end type
type dw_sa_restriction from u_dw_online within tabpage_sa
end type
type dw_rx_special_auth from u_dw_online within tabpage_sa
end type
type tabpage_sa from userobject within tab_pages
dw_sa_restriction dw_sa_restriction
dw_rx_special_auth dw_rx_special_auth
end type
type tabpage_sa_history from userobject within tab_pages
end type
type dw_restriction_history from u_dw_online within tabpage_sa_history
end type
type dw_rx_special_auth_history from u_dw_online within tabpage_sa_history
end type
type tabpage_sa_history from userobject within tab_pages
dw_restriction_history dw_restriction_history
dw_rx_special_auth_history dw_rx_special_auth_history
end type
type tabpage_elig_hist from userobject within tab_pages
end type
type dw_eligibility_history from u_dw_online within tabpage_elig_hist
end type
type tabpage_elig_hist from userobject within tab_pages
dw_eligibility_history dw_eligibility_history
end type
type tabpage_form_hist from userobject within tab_pages
end type
type dw_formulary_history from u_dw_online within tabpage_form_hist
end type
type tabpage_form_hist from userobject within tab_pages
dw_formulary_history dw_formulary_history
end type
type tabpage_alerts from userobject within tab_pages
end type
type dw_drug_alert from u_dw_online within tabpage_alerts
end type
type tabpage_alerts from userobject within tab_pages
dw_drug_alert dw_drug_alert
end type
type tab_pages from tab within w_maintain_formulary
tabpage_rx_din tabpage_rx_din
tabpage_rxdin_individ tabpage_rxdin_individ
tabpage_event tabpage_event
tabpage_sa tabpage_sa
tabpage_sa_history tabpage_sa_history
tabpage_elig_hist tabpage_elig_hist
tabpage_form_hist tabpage_form_hist
tabpage_alerts tabpage_alerts
end type
type p_1 from picture within w_maintain_formulary
end type
type st_1 from statictext within w_maintain_formulary
end type
type sb_splitbar_top from u_splitbar_horizontal within w_maintain_formulary
end type
type uo_filter from u_filter_control within w_maintain_formulary
end type
type cb_drug_alerts from commandbutton within w_maintain_formulary
end type
type st_alerts from statictext within w_maintain_formulary
end type
type cb_terminate from commandbutton within w_maintain_formulary
end type
type cb_add_eligibility from commandbutton within w_maintain_formulary
end type
type str_generate_report from structure within w_maintain_formulary
end type
end forward

type str_generate_report from structure
	long		eligibility
end type

global type w_maintain_formulary from w_a_tool
integer width = 2693
integer height = 1840
boolean resizable = false
boolean clientedge = true
long il_design_time_height = 1824
long il_design_time_width = 2674
event ue_postopen ( )
dw_formulary dw_formulary
cb_save cb_save
cb_cancel cb_cancel
uo_image_append uo_image_append
cb_switch cb_switch
cb_delete cb_delete
cb_log cb_log
cb_build cb_build
cb_add cb_add
cb_delete_elig cb_delete_elig
dw_eligibility dw_eligibility
tab_pages tab_pages
p_1 p_1
st_1 st_1
sb_splitbar_top sb_splitbar_top
uo_filter uo_filter
cb_drug_alerts cb_drug_alerts
st_alerts st_alerts
cb_terminate cb_terminate
cb_add_eligibility cb_add_eligibility
end type
global w_maintain_formulary w_maintain_formulary

type variables
n_resize_splitter			inv_resize_splitter
n_filter						inv_filter
n_maintain_formulary 	inv_maintain_formulary 
m_rx_popup					im_popup
s_window_message		istr_window_message
w_sheet						iw_sheet
BOOLEAN              		ib_Retrieving, ib_cancel
String							is_mode

long   il_claim_no, il_datawindow_delete = 0

Integer 						ii_event

DATE idt_current_date

end variables

forward prototypes
public function integer wf_view_formulary_history ()
public function boolean wf_archived_document (long al_docid)
public function integer wf_generate_report ()
public function integer wf_full_event_comments ()
public function integer wf_not_registered (long al_claim_no)
public function integer wf_enable_buttons ()
public subroutine wf_send_message ()
public function integer wf_call_function (string as_action)
public function integer wf_read_only ()
public function integer wf_view_restriction_history ()
public function integer wf_full_drug_alert_comments (string as_type)
end prototypes

event ue_postopen();
// had to do this here to get the tab to be the first one selected, if coming from the inbasket

long ll_found

IF ii_event > 0 THEN 
	ll_found = tab_pages.tabpage_event.dw_event.Find("event_no = " + string(ii_event), 1, tab_pages.tabpage_event.dw_event.RowCount())
	tab_pages.SelectTab(3)
		
	IF ll_found > 0  THEN
		tab_pages.tabpage_event.dw_event.SETROW(ll_found)
		tab_pages.tabpage_event.dw_event.scrolltorow(ll_found)
	END IF 

END IF

IF tab_pages.tabpage_alerts.dw_drug_alert.RowCount() > 0 THEN
	st_alerts.Visible = TRUE
END IF
end event

public function integer wf_view_formulary_history ();//open the formulary history window
IF isnull(il_claim_no) OR il_claim_no < 0  THEN RETURN 1

openwithparm(w_formulary_history,il_claim_no)

RETURN 1
end function

public function boolean wf_archived_document (long al_docid);INTEGER		li_count

SELECT Count(*)
  INTO :li_count
  FROM DOCUMENT_INDEX_ARCHIVE
 WHERE docid = :al_docid
 USING ImageTrans;

IF li_count > 0 THEN
	MessageBox('Archived', 'This document has been archived.',Information!)
	RETURN TRUE
ELSE
	RETURN FALSE
END IF
end function

public function integer wf_generate_report ();/* check that the users have selected information in which to view 
   this information will be generated as a report in the report viewer
	put all the PK's into arrays which will be sent to the report viewer
*/
INTEGER li_counter, li_selected
LONG    ll_eligibility[], ll_formulary[], ll_rx[], ll_event[], ll_rx_special_auth[], ll_formulary_pres_individual[]
LONG    ll_eligibility_no, ll_formulary_no, ll_rx_no, ll_event_no, ll_rx_special_auth_no, ll_formulary_pres_individual_no
s_generate_formulary_report lstr_report


//grab the formulary information
li_selected = 1
FOR li_counter = 1 TO dw_formulary.rowcount()
	IF dw_formulary.isselected(li_counter) THEN 
		
		//grab the key value from the row and put it in the array
		ll_formulary_no = dw_formulary.getitemnumber(li_counter,"formulary_record_no")
		
		//check the value
		IF isnull(ll_formulary_no) OR ll_formulary_no < 1 THEN RETURN -1//MESSAGEBOX
		
		//assign the value to the array
		ll_formulary[li_selected] = ll_formulary_no
		
		//up the array counter
		li_selected ++
	END IF 
NEXT

//grab the eligibility information
li_selected = 1
FOR li_counter = 1 TO dw_eligibility.rowcount()
	IF dw_eligibility.isselected(li_counter) THEN 
		
		//grab the key value from the row and put it in the array
		ll_eligibility_no = dw_eligibility.getitemnumber(li_counter,"eligibility_record_no")
		
		//check the value
		IF isnull(ll_eligibility_no) OR ll_eligibility_no < 1 THEN RETURN -1//MESSAGEBOX
		
		//assign the value to the array
		ll_eligibility[li_selected] = ll_eligibility_no
		
		//up the array counter
		li_selected ++
	END IF 	
NEXT

//grab the prescription rowcount
li_selected = 1
FOR li_counter = 1 TO tab_pages.tabpage_rx_din.dw_rx.rowcount()
	IF  tab_pages.tabpage_rx_din.dw_rx.isselected(li_counter) THEN 
		
		//grab the key value from the row and put it in the array
		ll_rx_no =  tab_pages.tabpage_rx_din.dw_rx.getitemnumber(li_counter,"payment_no")
		
		//check the value
		IF isnull(ll_rx_no) OR ll_rx_no < 1 THEN RETURN -1//MESSAGEBOX
		
		//assign the value to the array
		ll_rx[li_selected] = ll_rx_no
		
		//up the array counter
		li_selected ++
	END IF 
NEXT

//grab the prescription rowcount
li_selected = 1
FOR li_counter = 1 TO tab_pages.tabpage_rxdin_individ.dw_formulary_pres_individual.rowcount()
	IF  tab_pages.tabpage_rxdin_individ.dw_formulary_pres_individual.isselected(li_counter) THEN 
		
		//grab the key value from the row and put it in the array
		ll_formulary_pres_individual_no =  tab_pages.tabpage_rxdin_individ.dw_formulary_pres_individual.getitemnumber(li_counter,"payment_no")
		
		//check the value
		IF isnull(ll_formulary_pres_individual_no) OR ll_formulary_pres_individual_no < 1 THEN RETURN -1//MESSAGEBOX
		
		//assign the value to the array
		ll_formulary_pres_individual[li_selected] = ll_formulary_pres_individual_no
		
		//up the array counter
		li_selected ++
	END IF 
NEXT

//grab the event information
li_selected = 1
FOR li_counter = 1 TO  tab_pages.tabpage_event.dw_event.rowcount()
	IF tab_pages.tabpage_event.dw_event.isselected(li_counter) THEN 
		
		//grab the key value from the row and put it in the array
		ll_event_no = tab_pages.tabpage_event.dw_event.getitemnumber(li_counter,"event_no")
		
		//check the value
		IF isnull(ll_event_no) OR ll_event_no < 1 THEN RETURN -1//MESSAGEBOX
		
		//assign the value to the array
		ll_event[li_selected] = ll_event_no
		
		//up the array counter
		li_selected ++
	END IF 
NEXT

//grab the rx sa information
li_selected = 1
FOR li_counter = 1 TO  tab_pages.tabpage_sa.dw_rx_special_auth.rowcount()
	IF tab_pages.tabpage_sa.dw_rx_special_auth.isselected(li_counter) THEN 
		
		//grab the key value from the row and put it in the array
		ll_rx_special_auth_no = tab_pages.tabpage_sa.dw_rx_special_auth.getitemnumber(li_counter,"rx_special_auth_no")
		
		//check the value
		IF isnull(ll_rx_special_auth_no) OR ll_rx_special_auth_no < 1 THEN RETURN -1//MESSAGEBOX
		
		//assign the value to the array
		ll_rx_special_auth[li_selected] = ll_rx_special_auth_no
		
		//up the array counter
		li_selected ++
	END IF 
NEXT

//If the selected = 0 then no report to generate
IF upperbound(ll_eligibility) + upperbound(ll_formulary) +&
 	+ upperbound(ll_rx) + upperbound(ll_event) + upperbound(ll_rx_special_auth)  + upperbound(ll_formulary_pres_individual)  = 0 THEN 
 	MESSAGEBOX("Generate Report","No information was selected in which to generate a report" +&
	            "~rPlease select some information and try again")
 	RETURN 1
END IF 
 
//grab the claim number + Validate
IF ISNULL(il_claim_no) OR il_claim_no < 1 THEN RETURN -1
 
//send the arguments through to the report window 
lstr_report.al_claim_no    = il_claim_no
lstr_report.aw_formulary   = dw_formulary
lstr_report.aw_eligibility = dw_eligibility
lstr_report.aw_event       = tab_pages.tabpage_event.dw_event
lstr_report.aw_rx          = tab_pages.tabpage_rx_din.dw_rx
lstr_report.aw_rx_by_individual = tab_pages.tabpage_rxdin_individ.dw_formulary_pres_individual
lstr_report.aw_sa          = tab_pages.tabpage_sa.dw_rx_special_auth

//open the report window
OpenWithParm(w_formulary_report_viewer, lstr_report)

RETURN 1
end function

public function integer wf_full_event_comments ();INTEGER li_row
STRING  ls_event_type
LONG    ll_event_no

IF tab_pages.tabpage_event.dw_event.RowCount() = 0 THEN RETURN 1

/* grab the row */
li_row = tab_pages.tabpage_event.dw_event.getrow()

IF ISNULL(li_row) OR li_row < 1  THEN RETURN 1

ls_event_type = tab_pages.tabpage_event.dw_event.GetItemString(li_row,"event_type_code")
ll_event_no   = tab_pages.tabpage_event.dw_event.GetItemNumber(li_row,"event_no")

// make sure everything is valid
IF isnull(ls_event_type) OR trim(ls_event_type) = "" THEN RETURN 1
IF isnull(il_claim_no) THEN RETURN 1
IF isnull(ll_event_no) THEN RETURN 1

//information needed to be sent to the event comments window
istr_window_message.as_stringparm[1] = ls_event_type
istr_window_message.al_doubleparm[1] = il_claim_no
istr_window_message.al_doubleparm[2] = ll_event_no

//open the event comments window
OpenWithParm(w_full_event_comments, istr_window_message)

RETURN 1
end function

public function integer wf_not_registered (long al_claim_no);/*
In order to access the Maintain Formulary Assignment screen, 
the user must first select a claim that is registered for drug coverage. 
This screen can be accessed from the main menu and from the inbasket
so check and see if we should allow it to open
*/
INTEGER li_count

IF ISNULL(al_claim_no) OR al_claim_no < 1 THEN RETURN -1

SELECT count(*) 
  INTO :li_count
  FROM X001_REGISTRATION
 WHERE claim_no = :al_claim_no
 USING SQLCA;
 
SQLCA.nf_handle_error('w_maintain_formulary','wf_not_registered()','SELECT count(*)') 

IF ISNULL(li_count) OR li_count < 1 THEN RETURN -1

RETURN 1
end function

public function integer wf_enable_buttons ();/*
The following Buttons are enabled when the screen is opened:
·	Delete Formulary *
·	Switch Formulary ** 
	
* 	is enabled if there is a coverage record that has not been exported and 
	the manual_entry_flag is Yes ???
** is enabled if the formulary code of the active primary formulary 
   differs from the formulary code that is derived from the claim accident 
	nature of injury code
	
	NOTE - NOT INCLUDED IN THE DD IS THE DELETE OF ELIGIBILITY RECORDS
*/
INTEGER li_counter, li_row
LONG    ll_formulary_count, ll_eligibility_count, ll_count,ll_record_no
DATE    ldt_start_date, ldt_null_date
STRING  ls_formulary_code, 	ls_maintain_formulary_flag

//this is also checked as a rule on the column but put it here as well as the itemchanged event
IF inv_maintain_formulary.nf_check_status() = -1 THEN
	cb_switch.enabled          = FALSE
	cb_delete.enabled          = FALSE
	cb_delete_elig.enabled     = FALSE
	cb_add_eligibility.enabled = FALSE
	cb_add.enabled             = FALSE
	RETURN -1
END IF

IF vgst_user_profile.maintain_formulary_flag = "N" THEN
	dw_formulary.Object.formulary_code.protect = True
	dw_formulary.object.formulary_start_date.protect = True
	dw_formulary.Object.formulary_end_date.protect = True
	dw_formulary.Object.formulary_comment.protect = True
	
	dw_formulary.Object.formulary_type_code.Border = '0'
	dw_formulary.Object.formulary_code.Border = '0'
	dw_formulary.Object.formulary_start_date.Border = '0'
	dw_formulary.Object.formulary_end_date.Border = '0'
	dw_formulary.Object.formulary_comment.Border = '0'
	
	cb_add.enabled = False
	cb_delete.enabled = False
	
END IF	

IF tab_pages.SelectedTab = 8 THEN 
	 tab_pages.tabpage_alerts.dw_drug_alert.Object.DataWindow.HorizontalScrollPosition=1
	 cb_drug_alerts.Enabled = TRUE
	 IF tab_pages.tabpage_alerts.dw_drug_alert.GetRow() > 0 THEN
		 IF IsNull(tab_pages.tabpage_alerts.dw_drug_alert.GetItemDateTime(tab_pages.tabpage_alerts.dw_drug_alert.GetRow(), 'terminated_date')) THEN
			cb_terminate.Enabled = TRUE
		ELSE
			cb_terminate.Enabled = FALSE
		 END IF
	END IF
ELSE 
	 cb_drug_alerts.Enabled = FALSE
	 cb_terminate.Enabled = FALSE
END IF

//set the null date
setnull(ldt_null_date)

//grab the counts
ll_formulary_count   = dw_formulary.rowcount()
ll_eligibility_count = dw_eligibility.rowcount()

IF ISNULL(ll_formulary_count) THEN ll_formulary_count     = 0
IF ISNULL(ll_eligibility_count) THEN ll_eligibility_count = 0

/* make sure the claim_no is valid */
IF ISNULL(il_claim_no) OR il_claim_no < 1 THEN RETURN -1 

// default the count/rownumber
ll_count = 0
li_row   = dw_formulary.getrow()

//see if we need to enable the button
IF ll_formulary_count > 0 AND li_row > 0 THEN 
	ll_record_no = dw_formulary.getitemnumber(li_row,"formulary_record_no")
	IF ll_record_no > 0 THEN 
		IF inv_maintain_formulary.nf_has_record_been_exported(il_claim_no,ll_record_no,"DELETE_FORMULARY_IND") = FALSE THEN 
			ll_count = 1
		END IF 
	END IF 
END IF 

//do the button stuff
IF ll_count > 0 THEN 
	cb_delete.enabled = TRUE
ELSE
	cb_delete.enabled = FALSE
END IF 

// default the count/rownumber
ll_count = 0
li_row   = dw_eligibility.getrow()

//see if we need to enable the button
IF ll_eligibility_count > 0 AND li_row > 0 THEN 
	ll_record_no = dw_eligibility.getitemnumber(li_row,"eligibility_record_no")
	IF ll_record_no > 0 THEN 
		IF inv_maintain_formulary.nf_has_record_been_exported(il_claim_no,ll_record_no,"DELETE_ELIGIBILITY_IND") = FALSE THEN 
			ll_count = 1
		END IF 
	END IF 
END IF  	

//do the button stuff
IF ll_count > 0 THEN 
	cb_delete_elig.enabled = TRUE
ELSE
	cb_delete_elig.enabled = FALSE
END IF 
 
//for the switch formulary button
SELECT COUNT(*) INTO :ll_count
  FROM CLAIM_FORMULARY a
 WHERE a.primary_active_flag = "Y" 
   AND a.formulary_type_code = "P" 
	AND a.claim_no            = :il_claim_no
   AND NOT EXISTS (SELECT * 
                     FROM ACCIDENT b, X001_Noi_Formulary_Xref c 
	                 WHERE a.claim_no              = b.claim_no 
                      AND b.nature_of_injury_code = c.noi_code 
                      AND c.formulary_code        = a.formulary_code)
 USING SQLCA;
 
 SQLCA.nf_handle_error('w_maintain_formulary','wf_enable_buttons()','SELECT COUNT(*) INTO :ll_count - B') 

  //trigger the button on or off
 IF ll_count > 0 THEN 
	cb_switch.enabled = TRUE
 ELSE
	cb_switch.enabled = FALSE
 END IF 

 

 RETURN 1

end function

public subroutine wf_send_message ();S_SEND_DOC_PARAMETERS	ls_send_doc_parameters

ls_send_doc_parameters.msg_mode = TRUE
ls_send_doc_parameters.claim_no = il_claim_no

IF ISNULL(il_claim_no) THEN RETURN

IF tab_pages.tabpage_event.dw_event.RowCount() <= 0 THEN
	MessageBox('Warning','There are no events listed to send.')
	Return
ELSE
	ls_send_doc_parameters.document_list = tab_pages.tabpage_event.dw_event
END IF

OpenWithParm(w_send_folder,ls_send_doc_parameters)
end subroutine

public function integer wf_call_function (string as_action);INTEGER 	li_error, li_trancount
STRING   ls_message, ls_event_type_code, ls_event_specific_code
LONG ll_alerts, ll_individual_no
S_WINDOW_MESSAGE lstr_message

//grab the claim number and check it
IF ISNULL(il_claim_no) OR il_claim_no < 1 THEN RETURN -1

//default li_error
li_error = 1

//one time at band camp
setpointer(hourglass!)

CHOOSE CASE as_action
		
	CASE "FORMULARY_LEGEND"
		   li_error = open(w_formulary_code_legend)
				
	CASE "SAVE_EVERYTHING"
		
			dw_eligibility.accepttext()
			dw_formulary.accepttext()
			
			IF inv_maintain_formulary.nf_check_status() <> 1 THEN 
				messagebox("Invalid Status","Invalid Claim status for Save Operation" )
				RETURN -1 
			END IF
			
			SQLCA.nf_begin_transaction()
			
			IF inv_maintain_formulary.nf_save() <> 1 THEN
				// rollback transaction
				SQLCA.nf_transaction_count(li_trancount,0,'','','',FALSE)
				IF li_trancount > 0 THEN
					SQLCA.nf_rollback_transaction()
				END IF

				li_error = -1
        		ls_message = ""
			ELSE
				SQLCA.nf_commit_transaction()

				//everything went alright we need to re-retrieve everything
				inv_maintain_formulary.nf_retrieve(il_claim_no)
				cb_save.enabled                        = FALSE
				cb_add.enabled                         = TRUE
				cb_add_eligibility.enabled             = TRUE
				cb_cancel.enabled                      = FALSE
				inv_maintain_formulary.ib_switched     = FALSE
				inv_maintain_formulary.ib_auto_created = FALSE
				
				/*  check to see if certain buttons can be enabled */
				wf_enable_buttons()
			END IF 
			
	CASE "CANCEL_EVERYTHING"
	
			IF inv_maintain_formulary.nf_cancel() = -1  THEN RETURN -1// DO NOT CANCEL
			
			//everything went alright we need to re-retrieve everything
			inv_maintain_formulary.nf_retrieve(il_claim_no)
			
			//do the buttons
			cb_cancel.enabled                      = FALSE
			cb_save.enabled                        = FALSE
			cb_add.enabled                         = TRUE
			cb_add_eligibility.enabled             = TRUE
			inv_maintain_formulary.ib_switched     = FALSE
			inv_maintain_formulary.ib_auto_created = FALSE

			/*  check to see if certain buttons can be enabled */
			wf_enable_buttons()
			ib_cancel = FALSE
			
	CASE "GENERATE_REPORT"
			//no need for an error doesn't do anything
			wf_generate_report()
		
	CASE "DELETE_FORMULARY"
			
			//IF the save button is enabled don't allow.
			IF cb_save.enabled = TRUE THEN
				MESSAGEBOX("Delete","Please Save or Cancel current work before proceeding with Delete.")
				RETURN -1
			END IF 
			
			IF inv_maintain_formulary.nf_check_modified_status(0) = TRUE THEN
				MESSAGEBOX("Delete Formulary","Please Save or Cancel current work before proceeding with Delete.")
				RETURN -1
			END IF 
			
			IF inv_maintain_formulary.nf_check_status() <> 1 THEN 
				messagebox("Invalid Status", "Invalid Claim status for a Formulary Delete" )
				RETURN -1 
			END IF
		
			SQLCA.nf_begin_transaction()
			
			IF inv_maintain_formulary.nf_delete_formulary(il_claim_no) <> 1 THEN
				// rollback transaction
				SQLCA.nf_transaction_count(li_trancount,0,'','','',FALSE)
				IF li_trancount > 0 THEN
					SQLCA.nf_rollback_transaction()
				END IF

				li_error = -1
            ls_message = ""
			ELSE
				SQLCA.nf_commit_transaction()

				//everything went alright we need to re-retrieve everything
				inv_maintain_formulary.nf_retrieve(il_claim_no)
				cb_save.enabled                        = FALSE
				cb_add.enabled                         = TRUE
				cb_add_eligibility.enabled             = TRUE
				cb_cancel.enabled                      = FALSE
				inv_maintain_formulary.ib_switched     = FALSE
				inv_maintain_formulary.ib_auto_created = FALSE
				
				/*  check to see if certain buttons can be enabled */
				wf_enable_buttons()
			END IF 
			
	CASE "DELETE_ELIGIBILITY"
			
			//IF the save button is enabled don't allow.
			IF cb_save.enabled = TRUE THEN
				MESSAGEBOX("Delete","Please Save or Cancel current work before proceeding with Delete.")
				RETURN -1
			END IF 
			
			IF inv_maintain_formulary.nf_check_modified_status(0) = TRUE THEN
				MESSAGEBOX("Delete Eligibility","Please Save or Cancel current work before proceeding with Delete.")
				RETURN -1
			END IF 
			
			IF inv_maintain_formulary.nf_check_status() <> 1 THEN 
				messagebox("Invalid Status", "Invalid Claim status for a Eligibility Delete" )
				RETURN -1 
			END IF
		
			SQLCA.nf_begin_transaction()
			
 			IF inv_maintain_formulary.nf_delete_eligibility(il_claim_no) <> 1 THEN
				// rollback transaction
				SQLCA.nf_transaction_count(li_trancount,0,'','','',FALSE)
				IF li_trancount > 0 THEN
					SQLCA.nf_rollback_transaction()
				END IF

				li_error = -1
            ls_message = ""
			ELSE
				SQLCA.nf_commit_transaction()

				//everything went alright we need to re-retrieve everything
				inv_maintain_formulary.nf_retrieve(il_claim_no)
				cb_save.enabled                        = FALSE
				cb_add.enabled                         = TRUE
				cb_add_eligibility.enabled             = TRUE
				cb_cancel.enabled                      = FALSE
				inv_maintain_formulary.ib_switched     = FALSE
				inv_maintain_formulary.ib_auto_created = FALSE
				
				/*  check to see if certain buttons can be enabled */
				wf_enable_buttons()
			END IF 
		
	CASE "ADD_FORMULARY"
		
		   IF inv_maintain_formulary.nf_check_status() <> 1 THEN 
				messagebox("Invalid Status", "Invalid Claim status for a Formulary Add" )
				RETURN -1 
			END IF
		
//			IF cb_add_eligibility.enabled = FALSE THEN
//				MESSAGEBOX("Event Log","Please Save or Cancel current work before proceeding with Event Information.")
//				RETURN -1
//			END IF 
		  
			IF inv_maintain_formulary.nf_check_modified_status(1) = TRUE THEN
				MESSAGEBOX("Add Formulary","Please Save or Cancel current work before proceeding with Add.")
				RETURN -1
			END IF 
			
			li_error = inv_maintain_formulary.nf_add_formulary()
			IF li_error = -1 THEN 
				MESSAGEBOX("Add Formulary","Unable to Add Claim Formulary. Please Cancel current work before proceeding.")
				RETURN -1
			END IF 
			
			//turn it off....turn it off
			cb_add.enabled    = FALSE
			cb_cancel.enabled = TRUE
			//cb_delete.enabled = TRUE
			
	CASE "SWITCH_FORMULARY"
		
			IF inv_maintain_formulary.nf_check_status() <> 1 THEN 
				messagebox("Invalid Status", "Invalid Claim status for a Formulary Switch" )
				RETURN -1 
			END IF 
		
			IF inv_maintain_formulary.nf_check_modified_status(0) = TRUE THEN
				MESSAGEBOX("Delete Formulary","Please Save or Cancel current work before proceeding with Formulary Switch.")
				RETURN -1
			END IF 
			
			cb_switch.enabled = FALSE
			cb_cancel.enabled = TRUE
			
		   IF inv_maintain_formulary.nf_switch_formulary(il_claim_no) = 1 THEN
				cb_save.enabled                    = TRUE
				inv_maintain_formulary.ib_switched = TRUE
			ELSE
				MESSAGEBOX("Switch Formulary Failure","Unable to Switch Claim Formulary. Please Cancel current work before proceeding.")
				RETURN -1
			END IF 
		
	CASE "VIEW_FORMULARY_HISTORY"
		   IF wf_view_formulary_history() = -1 THEN RETURN -1
			
	CASE "VIEW_RESTRICTION_HISTORY"
		   IF wf_view_restriction_history() = -1 THEN RETURN -1
						
	CASE "SEND_MESSAGE"
		   wf_send_message()
		
	CASE "LOG_EVENT"
		
		   //IF the save button is enabled don't allow.
			IF cb_save.enabled = TRUE THEN
				MESSAGEBOX("Event Log","Please Save or Cancel current work before proceeding with Event Information.")
				RETURN -1
			END IF 
			
			IF inv_maintain_formulary.nf_check_modified_status(0) = TRUE THEN
				MESSAGEBOX("Event Log","Please Save or Cancel current work before proceeding with Event Information.")
				RETURN -1
			END IF 
			
		   li_error = inv_maintain_formulary.nf_add_event()
			//IF li_error = -1 THEN RETURN -1
		
	CASE "FULL_EVENT_COMMENTS"
		   li_error = wf_full_event_comments()
			
	CASE "ADD_ELIGIBILITY"
		
		   IF inv_maintain_formulary.nf_check_status() <> 1 THEN 
				messagebox("Invalid Status", "Invalid Claim status for a Add Eligibility")
				RETURN -1 
			END IF 
		
//		   IF cb_add.enabled = FALSE THEN
//				MESSAGEBOX("Event Log","Please Save or Cancel current work before proceeding with Event Information.")
//				RETURN -1
//			END IF 
		
			IF inv_maintain_formulary.nf_check_modified_status(4) = TRUE THEN
				MESSAGEBOX("Delete Formulary","Please Save or Cancel current work before proceeding with Eligibility Add.")
				RETURN -1
			END IF 
			
		   li_error = inv_maintain_formulary.nf_add_eligibility()
			IF li_error = -1 THEN 
				MESSAGEBOX("Add Eligibility Failure","Unable to Add Claim Eligibility. Please Cancel current work before proceeding.")
				RETURN -1
			END IF 
			
			cb_add_eligibility.enabled = FALSE
			cb_cancel.enabled          = TRUE
			
	CASE 'DRUG_ALERT'

			IF inv_maintain_formulary.nf_check_modified_status(12) = TRUE THEN
				MessageBox("Add Alert Failure","Please Save or Cancel your current Drug Alert before you add a new Drug Alert.")
				RETURN -1
			END IF 
			
			tab_pages.tabpage_alerts.dw_drug_alert.Object.drug_alert_type_code.TabSequence = 10
			tab_pages.tabpage_alerts.dw_drug_alert.Object.alert_comment.TabSequence = 20
			tab_pages.tabpage_alerts.dw_drug_alert.Object.terminated_comment.TabSequence = 0

			IF inv_maintain_formulary.nf_add_drug_alert() < 0 THEN
				MessageBox('Add Alert Failure','Unable to add a new Drug Alert', Information! )
				RETURN -1
			END IF
			
			tab_pages.tabpage_alerts.dw_drug_alert.Object.DataWindow.HorizontalScrollPosition=1 
			cb_save.enabled   = TRUE
			cb_cancel.enabled = TRUE
			cb_drug_alerts.Enabled = FALSE
			cb_terminate.Enabled = FALSE
	
	CASE "FULL_ALERT_COMMENTS"
		   li_error = wf_full_drug_alert_comments('N')
		
	CASE 'TERMINATE_ALERT'
			
			IF inv_maintain_formulary.nf_check_modified_status(12) = TRUE THEN
				MessageBox("Terminate Alert Failure","Please Save or Cancel your current work before proceeding with a Drug Alert Termination")
				RETURN -1
			END IF 
		
			tab_pages.tabpage_alerts.dw_drug_alert.Object.drug_alert_type_code.TabSequence = 0
			tab_pages.tabpage_alerts.dw_drug_alert.Object.alert_comment.TabSequence = 0
			tab_pages.tabpage_alerts.dw_drug_alert.Object.terminated_comment.TabSequence = 30

			IF inv_maintain_formulary.nf_terminate_drug_alert(tab_pages.tabpage_alerts.dw_drug_alert.GetRow()) < 0 THEN
				MessageBox('Termination Failure','Termination of the Drug Alert failed', Information!)
				RETURN -1
			END IF
			
			cb_save.Enabled   = TRUE
			cb_cancel.Enabled = TRUE
			cb_drug_alerts.Enabled = FALSE
			cb_terminate.Enabled = FALSE
			
	CASE "FULL_TERMINATED_COMMENTS"
		   	li_error = wf_full_drug_alert_comments('T')
			
END CHOOSE


RETURN 1
end function

public function integer wf_read_only ();/*	protect all columns of visible dw's
*/
	dw_eligibility.uf_protect_allattributes(TRUE)
	dw_formulary.uf_protect_allattributes(TRUE)
//	tab_pages.tabpage_rx_din.dw_rx.uf_protect_allattributes(TRUE)
//	tab_pages.tabpage_rxdin_individ.dw_formulary_pres_individual.uf_protect_allattributes(TRUE)
//	tab_pages.tabpage_event.dw_event.uf_protect_allattributes(TRUE)
//	tab_pages.tabpage_sa.dw_rx_special_auth.uf_protect_allattributes(TRUE)
//	tab_pages.tabpage_sa.dw_sa_restriction.uf_protect_allattributes(TRUE)
//	tab_pages.tabpage_sa_history.dw_rx_special_auth_history.uf_protect_allattributes(TRUE)
//	tab_pages.tabpage_elig_hist.dw_eligibility_history.uf_protect_allattributes(TRUE)
//	tab_pages.tabpage_form_hist.dw_formulary_history.uf_protect_allattributes(TRUE)

	
	dw_eligibility.uf_set_backcolor()
	dw_formulary.uf_set_backcolor()
	tab_pages.tabpage_rx_din.dw_rx.uf_set_backcolor()
	tab_pages.tabpage_rxdin_individ.dw_formulary_pres_individual.uf_set_backcolor()
	tab_pages.tabpage_event.dw_event.uf_set_backcolor()
	tab_pages.tabpage_sa.dw_rx_special_auth.uf_set_backcolor()
	tab_pages.tabpage_sa.dw_sa_restriction.uf_set_backcolor()
	tab_pages.tabpage_sa_history.dw_rx_special_auth_history.uf_set_backcolor()
	tab_pages.tabpage_elig_hist.dw_eligibility_history.uf_set_backcolor()
	tab_pages.tabpage_form_hist.dw_formulary_history.uf_set_backcolor()


	is_mode = 'READ'
	
	dw_formulary.Object.formulary_type_code.Border = '0'
	dw_formulary.Object.formulary_code.Border = '0'
	dw_formulary.Object.formulary_start_date.Border = '0'
	dw_formulary.Object.formulary_end_date.Border = '0'
	dw_formulary.Object.formulary_comment.Border = '0'
	
	dw_eligibility.Object.eligibility_start_date.Border = '0'
	dw_eligibility.Object.eligibility_end_date.Border = '0'
	dw_eligibility.Object.comment.Border = '0'


/*	disable the buttons
*/
	cb_add.enabled = FALSE
	cb_add_eligibility.enabled = FALSE
	cb_cancel.enabled = FALSE
	cb_delete.enabled = FALSE
	cb_delete_elig.enabled = FALSE
	cb_save.enabled = FALSE
	cb_log.enabled = FALSE
	cb_switch.enabled = FALSE

Return 0


end function

public function integer wf_view_restriction_history ();//open the restriction history window
IF isnull(il_claim_no) OR il_claim_no < 0  THEN RETURN 1

openwithparm(w_restriction_history,il_claim_no)

RETURN 1
end function

public function integer wf_full_drug_alert_comments (string as_type);INTEGER li_row
LONG    ll_alert_no

IF tab_pages.tabpage_alerts.dw_drug_alert.RowCount() = 0 THEN RETURN 1

li_row = tab_pages.tabpage_alerts.dw_drug_alert.GetRow()

IF ISNULL(li_row) OR li_row < 1  THEN RETURN 1

ll_alert_no   = tab_pages.tabpage_alerts.dw_drug_alert.GetItemNumber(li_row,"drug_alert_no")

// make sure everything is valid
IF IsNull(as_type) OR TRIM(as_type) = "" THEN RETURN 1
IF IsNull(il_claim_no) THEN RETURN 1
IF IsNull(ll_alert_no) THEN RETURN 1

//information needed to be sent to the event comments window
istr_window_message.as_stringparm[1] = as_type
istr_window_message.al_doubleparm[1] = il_claim_no
istr_window_message.al_doubleparm[2] = ll_alert_no

//open the event comments window
OpenWithParm(w_full_event_comments, istr_window_message)

RETURN 1
end function

event open;call super::open;LONG		ll_found, ll_rx_special_auth_no, ll_count
INTEGER	 li_return
Boolean  lb_rtn
W_SHEET  lw_active_sheet
U_DWA    ldw_dw[]


istr_window_message = Message.PowerObjectParm

//grab the event number if applicable
ii_event = istr_window_message.al_doubleparm[1]

lb_rtn = G_PFSecurity.UOF_Check_Access(This)
IF lb_rtn = FALSE THEN
	Messagebox("Access Denied", "You do not have proper security priveleges to open this window.~r~r" +&
				  "If you need to open this window, Please call the Helpdesk to get the proper security priveleges.", Exclamation!)
	Close(This)
	RETURN -1
END IF

/* grab the claim number and set it to an instance variable */
iw_active_sheet = w_frame.GetActiveSheet()
il_claim_no     = iw_active_sheet.dw_basic_claim.GetItemNumber(1,'claim_no')

/* make sure the claim is registered */
IF wf_not_registered(il_claim_no) = -1 THEN
	MESSAGEBOX("Claim Registration","Claim #: " + string(il_claim_no) + " Is not registered.")
	close(this)
	RETURN -1
END IF 

//register the objects with the splitbar  
sb_splitbar_top.of_register(dw_eligibility)
sb_splitbar_top.of_register(dw_formulary)

/* set up the column select which is a multi select (3) */
dw_formulary.uf_setselect(3)
dw_eligibility.uf_setselect(3)
tab_pages.tabpage_event.dw_event.uf_setselect(3)
tab_pages.tabpage_rx_din.dw_rx.uf_setselect(3)
tab_pages.tabpage_rxdin_individ.dw_formulary_pres_individual.uf_setselect(3)
tab_pages.tabpage_sa.dw_rx_special_auth.uf_setselect(3)

/* create the NVO */
inv_maintain_formulary = Create n_maintain_formulary

/* set up the datawindow array */
ldw_dw[1] = dw_formulary
ldw_dw[2] = tab_pages.tabpage_rx_din.dw_rx
ldw_dw[3] = tab_pages.tabpage_event.dw_event
ldw_dw[4] = dw_eligibility
ldw_dw[5] = tab_pages.tabpage_rxdin_individ.dw_formulary_pres_individual
ldw_dw[6] = tab_pages.tabpage_sa.dw_rx_special_auth
ldw_dw[7] = tab_pages.tabpage_sa.dw_sa_restriction
ldw_dw[8] = tab_pages.tabpage_sa_history.dw_rx_special_auth_history
ldw_dw[9]= tab_pages.tabpage_elig_hist.dw_eligibility_history
ldw_dw[10]= tab_pages.tabpage_form_hist.dw_formulary_history
ldw_dw[11] = tab_pages.tabpage_sa_history.dw_restriction_history
ldw_dw[12] = tab_pages.tabpage_alerts.dw_drug_alert

/* set up the array on the NVO and do the initial retrieves */
inv_maintain_formulary.nf_set_datawindow(ldw_dw[],SQLCA)
inv_maintain_formulary.nf_init()
inv_maintain_formulary.nf_set_commit(TRUE)
inv_maintain_formulary.nf_retrieve(il_claim_no)

/* Set up the other junk */
tab_pages.tabpage_rx_din.dw_rx.uf_setfilter(True)
tab_pages.tabpage_rx_din.dw_rx.setFocus()
tab_pages.tabpage_rx_din.dw_rx.uf_SetSort(True)
tab_pages.tabpage_rx_din.dw_rx.setcolumn(1)

/* for the events */
tab_pages.tabpage_event.dw_event.uf_setfilter(True)
tab_pages.tabpage_event.dw_event.uf_SetSort(True)

/* for the rx din by individual */
tab_pages.tabpage_rxdin_individ.dw_formulary_pres_individual.uf_setfilter(True)
tab_pages.tabpage_rxdin_individ.dw_formulary_pres_individual.uf_SetSort(True)

/* for the special authorizations */
tab_pages.tabpage_sa.dw_rx_special_auth.uf_setfilter(True)
tab_pages.tabpage_sa.dw_rx_special_auth.uf_SetSort(True)
tab_pages.tabpage_sa.dw_rx_special_auth.setFocus()
tab_pages.tabpage_sa.dw_rx_special_auth.setcolumn(1)

/* for the restrictions */
tab_pages.tabpage_sa.dw_sa_restriction.uf_setfilter(True)
tab_pages.tabpage_sa.dw_sa_restriction.uf_SetSort(True)

/* for the sa history */
tab_pages.tabpage_sa_history.dw_rx_special_auth_history.uf_setfilter(True)
tab_pages.tabpage_sa_history.dw_rx_special_auth_history.uf_SetSort(True)
tab_pages.tabpage_sa_history.dw_restriction_history.uf_SetSort(True)
tab_pages.tabpage_sa.dw_rx_special_auth.setcolumn(1)

/* for the eligibility history */
tab_pages.tabpage_elig_hist.dw_eligibility_history.uf_setfilter(True)
tab_pages.tabpage_elig_hist.dw_eligibility_history.uf_SetSort(True)

/* for the formulary history */
tab_pages.tabpage_form_hist.dw_formulary_history.uf_setfilter(True)
tab_pages.tabpage_form_hist.dw_formulary_history.uf_SetSort(True)

/* for the drug alert history */
tab_pages.tabpage_alerts.dw_drug_alert.uf_SetFilter(True)
tab_pages.tabpage_alerts.dw_drug_alert.uf_SetSort(True)


//starts the resize service in the ancestor object
wf_setresize(true)

inv_resize.of_register(tab_pages.tabpage_event.dw_event,'scaletoright&bottom')
inv_resize.of_register(tab_pages.tabpage_rx_din.dw_rx,'scaletoright&bottom')
inv_resize.of_register(tab_pages.tabpage_rxdin_individ.dw_formulary_pres_individual,'scaletoright&bottom')
inv_resize.of_register(tab_pages.tabpage_sa.dw_rx_special_auth,'scaletoright')
inv_resize.of_register(tab_pages.tabpage_sa.dw_sa_restriction,'scaletoright&bottom')
inv_resize.of_register(tab_pages.tabpage_sa_history.dw_rx_special_auth_history,'scaletoright')
inv_resize.of_register(tab_pages.tabpage_elig_hist.dw_eligibility_history,'scaletoright&bottom')
inv_resize.of_register(tab_pages.tabpage_form_hist.dw_formulary_history,'scaletoright&bottom')
inv_resize.of_register(tab_pages.tabpage_sa_history.dw_restriction_history,'scaletoright&bottom')
inv_resize.of_register(dw_formulary,'scaletoright')
inv_resize.of_register(dw_eligibility,'scaletoright')
inv_resize.of_Register(tab_pages,'scaletoright&bottom')
inv_resize.of_register(sb_splitbar_top,'scaletoright')
inv_resize.of_Register(tab_pages.tabpage_alerts.dw_drug_alert,'scaletoright&bottom')

inv_resize.of_Register(cb_build,'FixedToBottom')
inv_resize.of_Register(cb_log,'FixedToBottom')
inv_resize.of_Register(cb_switch,'FixedToBottom')
inv_resize.of_Register(cb_delete,'FixedToBottom')
inv_resize.of_Register(cb_delete_elig,'FixedToBottom')
inv_resize.of_Register(cb_add,'FixedToBottom')
inv_resize.of_Register(cb_add_eligibility,'FixedToBottom')
inv_resize.of_Register(cb_cancel,'FixedToBottom')
inv_resize.of_Register(cb_save,'FixedToBottom')
inv_resize.of_Register(cb_close,'FixedToBottom')
inv_resize.of_Register(cb_drug_alerts,'FixedToBottom')
inv_resize.of_Register(cb_terminate,'FixedToBottom')

/* SET ALL OF THE ROWS OFF TO START WITH */
tab_pages.tabpage_rx_din.dw_rx.selectrow(0,false)
tab_pages.tabpage_event.dw_event.selectrow(0,false)
dw_formulary.selectrow(0,false)
dw_eligibility.selectrow(0,false)

// setup Special Authroizations
ldw_dw[6].ShareData(ldw_dw[7])
tab_pages.tabpage_sa.dw_rx_special_auth.selectrow(0,false)
tab_pages.tabpage_sa.dw_rx_special_auth.uf_setselect(1)
tab_pages.tabpage_sa.dw_sa_restriction.uf_setselect(1)

tab_pages.tabpage_event.dw_event.uf_setselect(1)
tab_pages.tabpage_alerts.dw_drug_alert.uf_setselect(1)

tab_pages.tabpage_sa_history.dw_rx_special_auth_history.uf_setselect(1)
tab_pages.tabpage_elig_hist.dw_eligibility_history.uf_setselect(1)
tab_pages.tabpage_form_hist.dw_formulary_history.uf_setselect(1)

/* set focus to the ELIGIBILITY  datawindow */
dw_eligibility.setfocus()

IF istr_window_message.as_mode = 'READ'  OR  (istr_window_message.as_mode = 'UPDATE' AND cb_add_eligibility.visible = False) THEN
	wf_read_only()
ELSE
	/*  check to see if certain buttons can be enabled */
	wf_enable_buttons()
END IF

/* check general warning rule on open status of formulary & eligibility 
RETURN 1 - CASE 1.160
RETURN 2 - CASE 2.170
RETURN 0 - EVERYTHING EVEN - NO ACTION
*/
li_return = inv_maintain_formulary.nf_get_open_count(il_claim_no)
CHOOSE CASE li_return
	CASE 1 //eligibility
		
			messagebox("Informational Warning","Claim Eligibility coverage has expired and Formulary Coverage " +& 
	                           "is still open",information!)
   		
	CASE 2 //formulary
		
			messagebox("Informational Warning","Claim Formulary coverage has expired and Eligibility Coverage " +& 
	                           "is still open",information!)
	CASE ELSE
		//do nothing
END CHOOSE

PostEvent("ue_postopen")
		







end event

on w_maintain_formulary.create
int iCurrent
call super::create
this.dw_formulary=create dw_formulary
this.cb_save=create cb_save
this.cb_cancel=create cb_cancel
this.uo_image_append=create uo_image_append
this.cb_switch=create cb_switch
this.cb_delete=create cb_delete
this.cb_log=create cb_log
this.cb_build=create cb_build
this.cb_add=create cb_add
this.cb_delete_elig=create cb_delete_elig
this.dw_eligibility=create dw_eligibility
this.tab_pages=create tab_pages
this.p_1=create p_1
this.st_1=create st_1
this.sb_splitbar_top=create sb_splitbar_top
this.uo_filter=create uo_filter
this.cb_drug_alerts=create cb_drug_alerts
this.st_alerts=create st_alerts
this.cb_terminate=create cb_terminate
this.cb_add_eligibility=create cb_add_eligibility
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_formulary
this.Control[iCurrent+2]=this.cb_save
this.Control[iCurrent+3]=this.cb_cancel
this.Control[iCurrent+4]=this.uo_image_append
this.Control[iCurrent+5]=this.cb_switch
this.Control[iCurrent+6]=this.cb_delete
this.Control[iCurrent+7]=this.cb_log
this.Control[iCurrent+8]=this.cb_build
this.Control[iCurrent+9]=this.cb_add
this.Control[iCurrent+10]=this.cb_delete_elig
this.Control[iCurrent+11]=this.dw_eligibility
this.Control[iCurrent+12]=this.tab_pages
this.Control[iCurrent+13]=this.p_1
this.Control[iCurrent+14]=this.st_1
this.Control[iCurrent+15]=this.sb_splitbar_top
this.Control[iCurrent+16]=this.uo_filter
this.Control[iCurrent+17]=this.cb_drug_alerts
this.Control[iCurrent+18]=this.st_alerts
this.Control[iCurrent+19]=this.cb_terminate
this.Control[iCurrent+20]=this.cb_add_eligibility
end on

on w_maintain_formulary.destroy
call super::destroy
destroy(this.dw_formulary)
destroy(this.cb_save)
destroy(this.cb_cancel)
destroy(this.uo_image_append)
destroy(this.cb_switch)
destroy(this.cb_delete)
destroy(this.cb_log)
destroy(this.cb_build)
destroy(this.cb_add)
destroy(this.cb_delete_elig)
destroy(this.dw_eligibility)
destroy(this.tab_pages)
destroy(this.p_1)
destroy(this.st_1)
destroy(this.sb_splitbar_top)
destroy(this.uo_filter)
destroy(this.cb_drug_alerts)
destroy(this.st_alerts)
destroy(this.cb_terminate)
destroy(this.cb_add_eligibility)
end on

event closequery;call super::closequery;IF cb_save.enabled = TRUE THEN 
	IF messagebox("Data has been Modified","Information has been modified. Continue with close?",Question!,yesno!) = 2 THEN RETURN 1
END IF
end event

type st_title from w_a_tool`st_title within w_maintain_formulary
boolean visible = false
integer x = 992
integer y = 0
integer width = 73
integer height = 56
integer textsize = -12
string text = ""
boolean border = false
end type

type cb_close from w_a_tool`cb_close within w_maintain_formulary
integer x = 2423
integer y = 1724
integer width = 242
integer height = 92
integer textsize = -8
fontcharset fontcharset = ansi!
end type

event cb_close::clicked;call super::clicked;/* to close the Maintain Rx Coverage screen */
end event

type dw_formulary from u_dw_online within w_maintain_formulary
integer y = 540
integer width = 2656
integer height = 356
integer taborder = 10
boolean bringtotop = true
string dataobject = "d_formulary_main"
boolean controlmenu = true
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event rbuttondown;// make sure there is a valid row
IF ISNULL(row) OR row = 0 THEN RETURN

IF isvalid(im_popup) THEN DESTROY im_popup

im_popup = CREATE m_rx_popup
im_popup.mf_set_datawindow(THIS)
im_popup.mf_set_window(PARENT)
im_popup.m_options.m_sort.visible                 = TRUE	
	
im_popup.m_options.PopMenu(w_frame.PointerX(), w_frame.PointerY())
end event

event editchanged;call super::editchanged;cb_save.enabled   = TRUE
cb_cancel.enabled = TRUE
end event

event itemchanged;call super::itemchanged;
/*Add Formulary	- to add a new secondary formulary

3.1.6.	Add Secondary Formulary

A secondary formulary can only be added if the claim is Active or Final (01-04).

The formulary type is not enabled. The formulary code that is entered must be a valid formulary code 
and can be selected from the list of valid formulary codes. The effective date must be a valid date. 
The termination date must be either a valid date or to 0000-00-00 which represents a null date. 
The comment field must be entered by the user. Refer to the BRs for the secondary formulary.

Field	            Enterable	Mandatory	Notes
----------------------------------------------------------------------------
Formulary Type	   No	         Yes	      Always has a value of 'Secondary'
Formulary Code	   Yes	      Yes	      Must be a valid formulary code
Effective Date	   Yes	      Yes	      Must be a valid date & cannot be null
Termination Date	Yes	      Yes	      Must be a valid date or is null
Comment	         Yes	      Yes	 

3.1.4.1.	Add Primary Coverage Period
A new coverage period for the active, primary formulary code can be added for a registered claim. 
This would be done by the user as a means of creating a 'gap' in the coverage period for the primary formulary.
If no gap in coverage is required, the user would update the termination date or the effective date of the 
existing coverage record.

The user is restricted from adding a new primary formulary code unless the claim's accident nature of 
injury no longer maps to the same formulary code that is assigned to the active, primary formulary. Refer to 
the BR document for the specific conditions under which a new primary formulary code can be added. 

*/
LONG		ll_row, ll_formulary_record_no
STRING   ls_noi_code, ls_formulary_code, ls_formulary_type, ls_new_formulary_code, ls_primary_noi_code
STRING   ls_old_formulary_code , ls_new_active_flag , ls_old_active_flag
INT		li_count

IF ib_cancel = TRUE THEN RETURN

// make sure we have a valid row
IF ISNULL(row) OR row < 0 THEN RETURN 2
ll_row = row

cb_cancel.enabled         = TRUE

/* check the status */
IF inv_maintain_formulary.nf_check_status() = -1 THEN 
	MESSAGEBOX("Invalid Status","This Claim has an Invalid status and cannot be updated")
	RETURN 2
END IF 

/* we are allowing the users to ADD Primary and Secondary types */
CHOOSE CASE  dwo.name
	CASE "formulary_type_code"
		IF data = "S" THEN
			
			//grab the original value
			ls_formulary_code = THIS.GetItemstring(row,'formulary_code',primary!,TRUE)
			//IF data = ls_formulary_code THEN RETURN 0
				
			ll_formulary_record_no = THIS.GetItemnumber(row,'formulary_record_no')
			//IF ISNULL(ll_eligibility_record_no) THEN RETURN 1 

			IF ll_formulary_record_no > 0 THEN
				IF inv_maintain_formulary.nf_has_record_been_exported(il_claim_no,ll_formulary_record_no,"DELETE_FORMULARY_IND") = TRUE THEN
					messagebox("Validation Error","Cannot switch a Primary to Secondary.")
					RETURN 1
				END IF 
			END IF 
			
			dw_formulary.setitem(ll_row,"manual_entry_flag","Y")
			dw_formulary.setitem(ll_row,"primary_noi_code"," ")
			dw_formulary.setitem(ll_row,"primary_active_flag","I")
			dw_formulary.setitem(ll_row,"formulary_code"," ")
			
		ELSE//P
			
			ll_formulary_record_no = THIS.GetItemnumber(row,'formulary_record_no')

			IF ll_formulary_record_no > 0 THEN
				IF inv_maintain_formulary.nf_has_record_been_exported(il_claim_no,ll_formulary_record_no,"DELETE_FORMULARY_IND") = TRUE THEN
					messagebox("Validation Error","Cannot switch a Secondary to a Primary.")
					RETURN 1
				END IF 
			END IF 
			
			//automatically put the noi/formulary code in and protect the column
			/* grab the Primary_noi_code */
			
			
			SELECT primary_noi_code,formulary_code
			INTO   :ls_primary_noi_code,:ls_formulary_code
			FROM   CLAIM_FORMULARY
			WHERE  claim_no = :il_claim_no
			AND    formulary_type_code = 'P'
			AND    primary_active_flag = 'Y'
			USING  SQLCA;
			
			SQLCA.nf_handle_error('w_maintain_formulary', 'dw_formulary','itemchanged - SELECT primary_noi_code,formulary_code')
			
			
			SELECT nature_of_injury_code 
  			  INTO :ls_noi_code
  			  FROM ACCIDENT   
          WHERE claim_no = :il_claim_no
          USING SQLCA;
 
          SQLCA.nf_handle_error('w_maintain_formulary', 'dw_formulary','itemchanged - SELECT nature_of_injury_code') 

			IF ISNULL(ls_noi_code) OR TRIM(ls_noi_code) = "" THEN
				messagebox("Formulary Switch","Could not determine the Nature of Injury Code")
				RETURN 2
			END IF 

			ls_old_active_flag = inv_maintain_formulary.nf_check_if_active_formulary(ls_formulary_code)
			  
			IF ls_old_active_flag = 'Y' THEN
				dw_formulary.setitem(ll_row,"primary_noi_code",ls_primary_noi_code)
				dw_formulary.setitem(ll_row,"primary_active_flag","Y")
				dw_formulary.setitem(ll_row,"formulary_code",ls_formulary_code)
				dw_formulary.setitem(ll_row,"manual_entry_flag","Y")
			ELSE
				MessageBox('Inactive Formulary', 'The Formulary for the Claim Nature of Injury is inactive.')
				RETURN 0
			END IF

			
		END IF 
		
	CASE "formulary_code"
		/* grab the value from formulary_type_desc
		   FIND OUT IF THE TYPE = "S" - IF IT IS IT CANNOT BE CHANGED 
		   if the record has been exported
		*/
		ls_formulary_type = THIS.GetItemstring(row,'formulary_type_code',primary!,TRUE)
		ls_formulary_code = THIS.GetItemstring(row,'formulary_code',primary!,TRUE)
		//IF data = ls_formulary_code THEN RETURN 0
		
		CHOOSE CASE ls_formulary_type
			CASE "S"
				
				ll_formulary_record_no = THIS.GetItemnumber(row,'formulary_record_no')
				ls_new_formulary_code = String(data)

				IF ll_formulary_record_no > 0 THEN
					IF inv_maintain_formulary.nf_has_record_been_exported(il_claim_no,ll_formulary_record_no,"FORMULARY") = TRUE THEN
						dw_formulary.setitem(ll_row,"formulary_code",ls_formulary_code)
						messagebox("Validation Error","Cannot switch a Secondary to a new code once the record has been exported.")
						RETURN 1
					END IF 
				END IF 
				
				SELECT count(*) 
	  			INTO :li_count
	  			FROM CLAIM_FORMULARY
		 		WHERE formulary_type_code = "P"
				AND claim_no            = :il_claim_no
				AND formulary_code      = :ls_new_formulary_code 
				AND primary_active_flag = "Y"
		 		USING SQLCA;
			 
				SQLCA.nf_handle_error('w_maintain_formulary', 'dw_formulary','itemchanged - SELECT - BR4.30') 
		
				IF ISNULL(li_count) THEN li_count = 0 
		
				IF li_count > 0 THEN
					messagebox("Validation Error BR# 4.30", "The formulary code is already assigned to the active, primary formulary.")
					RETURN 1	
				END IF 

				
			CASE "P"
								
				ll_formulary_record_no = THIS.GetItemnumber(row,'formulary_record_no')
				ls_new_formulary_code = String(data)
				
				IF ll_formulary_record_no > 0 THEN
					IF inv_maintain_formulary.nf_has_record_been_exported(il_claim_no,ll_formulary_record_no,"DELETE_FORMULARY_IND") = TRUE THEN
						messagebox("Validation Error","Cannot switch a Secondary to Primary.")
						RETURN 1
					END IF 
				END IF 
				
				// Check the old Primary NOI Code
				SELECT primary_noi_code,formulary_code
				INTO   :ls_primary_noi_code,:ls_old_formulary_code
				FROM   CLAIM_FORMULARY
				WHERE  claim_no = :il_claim_no
				AND    formulary_type_code = 'P'
				AND    primary_active_flag = 'Y'
				USING  SQLCA;
			
				SQLCA.nf_handle_error('w_maintain_formulary', 'dw_formulary','itemchanged - SELECT primary_noi_code,formulary_code')
	
			
				//automatically put the noi/formulary code in and protect the column
				/* grab the Primary_noi_code */
				SELECT nature_of_injury_code 
				  INTO :ls_noi_code
				  FROM ACCIDENT   
				 WHERE claim_no = :il_claim_no
				 USING SQLCA;
	 
				 SQLCA.nf_handle_error('w_maintain_formulary', 'dw_formulary','itemchanged - SELECT nature_of_injury_code') 
	
				IF ISNULL(ls_noi_code) OR TRIM(ls_noi_code) = "" THEN
					messagebox("Formulary Switch","Could not determine the Nature of Injury Code")
					RETURN 2
				END IF 
				
				IF ls_primary_noi_code <> ls_noi_code THEN 
					MessageBox("Informational","The Nature of Injury has changed, a Primary Formulary Code switch may be required.") 
					RETURN 1
				ELSE
					// NOI still mapped to old formulary code
					ls_old_active_flag = inv_maintain_formulary.nf_check_if_active_formulary(ls_old_formulary_code)
					IF ls_old_active_flag = 'N' THEN
						MessageBox("Informational","The Formulary for the Claim Nature of Injury is inactive. Please change the NOI and use the Switch functionality.") 
						RETURN 1
					END IF
				END IF
	
				/* Formulary Code	- No Yes Formulary code based on the current accident NOI */
//				SELECT formulary_code 
//				  INTO :ls_formulary_code 
//				  FROM X001_Noi_Formulary_Xref
//				 WHERE noi_code = :ls_noi_code
//				 USING SQLCA;
//
//				 SQLCA.nf_handle_error('w_maintain_formulary', 'dw_formulary','itemchanged - SELECT formulary_code ') 
//	
//				IF ISNULL(ls_formulary_code) OR TRIM(ls_formulary_code) = "" THEN
//					messagebox("Formulary Add","Could not determine the Formulary Code")
//					RETURN 2
//				END IF 
// 				
//				IF DATA <> ls_formulary_code THEN 
//					messagebox("Formulary Switch","Formulary Code must match the Accident Formulary Code")
//					RETURN 1
//				END IF 
				
				dw_formulary.setitem(ll_row,"primary_noi_code",ls_primary_noi_code)
				dw_formulary.setitem(ll_row,"primary_active_flag","Y")
				dw_formulary.setitem(ll_row,"formulary_code",ls_old_formulary_code)
				dw_formulary.setitem(ll_row,"manual_entry_flag","Y")
			
		END CHOOSE
END CHOOSE

cb_save.enabled   = TRUE
cb_cancel.enabled = TRUE
end event

event rowfocuschanged;call super::rowfocuschanged;LONG ll_formulary_record_no

IF ib_Retrieving OR currentrow <= 0 THEN RETURN 

// if the claim isnt the right status don't enable the buttons
IF inv_maintain_formulary.nf_check_status() <> 1 THEN RETURN 

/* determine if the record can be deleted */
ll_formulary_record_no = dw_formulary.GetItemnumber(currentrow,'formulary_record_no')
IF ISNULL(ll_formulary_record_no) THEN RETURN -1 

IF ll_formulary_record_no = 0 THEN
	//no need to check - new record
ELSE
	IF inv_maintain_formulary.nf_has_record_been_exported(il_claim_no,ll_formulary_record_no,"DELETE_FORMULARY_IND") = TRUE THEN
		cb_delete.enabled = FALSE
	ELSE
		cb_delete.enabled = TRUE
	END IF 
END IF 
end event

event retrievestart;call super::retrievestart;ib_Retrieving = TRUE
end event

event retrieveend;call super::retrieveend;ib_Retrieving = FALSE
end event

event rowfocuschanging;call super::rowfocuschanging;//IF THIS.modifiedcount() > 0 OR dw_eligibility.modifiedcount() > 0 THEN 
//   THIS.SelectRow(newrow, FALSE)
//   THIS.SelectRow(currentrow, TRUE)
//	RETURN 1
//END IF 
end event

event ue_print;this.Object.DataWindow.Print.orientation = 1
this.print()
end event

type cb_save from commandbutton within w_maintain_formulary
integer x = 2021
integer y = 1724
integer width = 192
integer height = 92
integer taborder = 11
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Save"
end type

event clicked;INTEGER li_rtn
N_PROCESS_RUN_STATUS ln_process_run_status

/******************************************************************************************
P10275 - Daytime Payment Processing
- added new function call to prevent updating of tables used by PRODSVCS Payment Processing
- new object N_PROCESS_RUN_STATUS is used to determine status of processes (in progress = Y/N)
- '027' refers to the Maintain Formulary module, '044' refers to the Payment Processing module
- likewise, '046' refers to the ABCC Eligibility Export module
******************************************************************************************/
ln_process_run_status = Create N_PROCESS_RUN_STATUS

li_rtn = ln_process_run_status.nf_in_progress('027','044','save',SQLCA)

IF li_rtn = 1 THEN
	// module is blocked
	return
END IF

//clear variable
li_rtn = 0
li_rtn  = ln_process_run_status.nf_in_progress('027','046','save',SQLCA)

IF li_rtn = 1 THEN
	// module is blocked
	return
END IF

/******************************************************************************************/


/* - to save the update that has been made */	
RETURN wf_call_function("SAVE_EVERYTHING")

end event

type cb_cancel from commandbutton within w_maintain_formulary
integer x = 2208
integer y = 1724
integer width = 210
integer height = 92
integer taborder = 11
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "C&ancel"
end type

event clicked;/* to cancel the update that has been made */
//IF dw_eligibility.rowcount() < 1  THEN RETURN 
		
ib_cancel = TRUE		
RETURN wf_call_function("CANCEL_EVERYTHING")
end event

type uo_image_append from u_image_append within w_maintain_formulary
boolean visible = false
integer x = 256
integer y = 1276
integer taborder = 21
boolean bringtotop = true
end type

on uo_image_append.destroy
call u_image_append::destroy
end on

type cb_switch from commandbutton within w_maintain_formulary
integer x = 475
integer y = 1724
integer width = 206
integer height = 92
integer taborder = 11
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "S&witch"
end type

event clicked;/* to switch the primary formulary code is enabled if the formulary code 
   of the active primary formulary differs from the ...
*/
IF dw_formulary.rowcount() < 1  THEN RETURN 

RETURN wf_call_function("SWITCH_FORMULARY")



end event

type cb_delete from commandbutton within w_maintain_formulary
integer x = 677
integer y = 1724
integer width = 201
integer height = 92
integer taborder = 21
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "- &Form"
end type

event clicked;/* to delete a coverage that has not been exported 
   is enabled if there is a secondary formulary that has 
	not been exported
*/
/* one delete button to control the delete of ELIGIBILITY & FORMULARY
 INFORM THE USER OF WHAT IS ABOUT TO BE DELETED
*/

IF dw_formulary.rowcount() < 1  THEN RETURN 

RETURN wf_call_function("DELETE_FORMULARY")



	
end event

type cb_log from commandbutton within w_maintain_formulary
integer x = 201
integer y = 1724
integer width = 274
integer height = 92
integer taborder = 21
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Log Event"
end type

event clicked;/* to log an event (also need functionality to  CC: the event to another user)
*/
RETURN wf_call_function("LOG_EVENT")


		
	
end event

type cb_build from commandbutton within w_maintain_formulary
integer x = 9
integer y = 1724
integer width = 197
integer height = 92
integer taborder = 31
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Report"
end type

event clicked;/* - to produce a Claim Summary Report */
RETURN wf_call_function("GENERATE_REPORT")




end event

type cb_add from commandbutton within w_maintain_formulary
integer x = 1262
integer y = 1724
integer width = 206
integer height = 92
integer taborder = 11
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "+ For&m"
end type

event clicked;/* to add a new secondary formulary coverage */
RETURN wf_call_function("ADD_FORMULARY")



end event

type cb_delete_elig from commandbutton within w_maintain_formulary
integer x = 873
integer y = 1724
integer width = 206
integer height = 92
integer taborder = 21
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "- El&ig"
end type

event clicked;/* to delete a ELIGIBILITY coverage that has not been exported 
*/
IF dw_eligibility.rowcount() < 1 THEN RETURN 

RETURN wf_call_function("DELETE_ELIGIBILITY")
end event

type dw_eligibility from u_dw_online within w_maintain_formulary
integer y = 104
integer width = 2656
integer height = 384
integer taborder = 10
string dataobject = "d_eligibility_main"
boolean controlmenu = true
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event rbuttondown;IF ISNULL(row) OR row = 0 THEN RETURN

IF isvalid(im_popup) THEN DESTROY im_popup

im_popup = CREATE m_rx_popup
im_popup.mf_set_datawindow(THIS)
im_popup.mf_set_window(PARENT)
im_popup.m_options.m_sort.visible = TRUE		

im_popup.m_options.PopMenu(w_frame.PointerX(), w_frame.PointerY())
end event

event editchanged;call super::editchanged;cb_save.enabled   = TRUE
cb_cancel.enabled = TRUE
end event

event itemchanged;call super::itemchanged;/*
eligibility_start_date, eligibility_end_date, eligibility_record_no, 
export_no,              claim_no,             comment, 
manual_entry_flag,      export_user_id,       export_date
*/

// make sure we have a valid row
IF ISNULL(row) OR row < 0 THEN RETURN -1

//this is also checked as a rule on the column but put it here as well as the itemchanged event
IF inv_maintain_formulary.nf_check_status() = -1 THEN 
	MESSAGEBOX("Invalid Status","This Claim has an Invalid status and cannot be updated")
	RETURN 2
END IF

/* check and see if the formulary datawindow has been updated if so do not 
   allow the user to change this as they could potential change dates
	and that would screw everything up
*/
//IF inv_maintain_formulary.nf_check_modified_status(1) = TRUE THEN
//	messagebox("Data has been Modified","Formulary information has been modified. " +&
//	           "~rPlease Cancel Or Save before modifying the Eligibility Record") 
//	RETURN 1	
//END IF 

cb_save.enabled   = TRUE
cb_cancel.enabled = TRUE
end event

event rowfocuschanged;call super::rowfocuschanged;LONG ll_eligibility_record_no

IF ib_Retrieving OR currentrow <= 0 THEN RETURN 

// if the claim isnt the right status don't enable the buttons
IF inv_maintain_formulary.nf_check_status() <> 1 THEN RETURN 

/* determine if the record can be deleted */
ll_eligibility_record_no = dw_eligibility.GetItemnumber(currentrow,'eligibility_record_no')
IF ISNULL(ll_eligibility_record_no) THEN RETURN 

IF ll_eligibility_record_no > 0 THEN
	IF inv_maintain_formulary.nf_has_record_been_exported(il_claim_no,ll_eligibility_record_no,"DELETE_ELIGIBILITY_IND") = TRUE THEN
		cb_delete_elig.enabled = FALSE
	ELSE
		cb_delete_elig.enabled = TRUE
	END IF 
END IF 
end event

event rowfocuschanging;call super::rowfocuschanging;//IF THIS.modifiedcount() > 0 OR dw_formulary.modifiedcount() > 0 THEN 
//   THIS.SelectRow(newrow, FALSE)
//   THIS.SelectRow(currentrow, TRUE)
//	RETURN 1
//END IF 


end event

event retrievestart;call super::retrievestart;ib_Retrieving = TRUE
end event

event retrieveend;call super::retrieveend;ib_Retrieving = FALSE
end event

event ue_print;this.Object.DataWindow.Print.orientation = 1
this.print()
end event

type tab_pages from tab within w_maintain_formulary
event create ( )
event destroy ( )
string tag = "Rx Din"
integer y = 896
integer width = 2674
integer height = 832
integer taborder = 20
boolean bringtotop = true
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 67108864
boolean raggedright = true
boolean focusonbuttondown = true
integer selectedtab = 1
tabpage_rx_din tabpage_rx_din
tabpage_rxdin_individ tabpage_rxdin_individ
tabpage_event tabpage_event
tabpage_sa tabpage_sa
tabpage_sa_history tabpage_sa_history
tabpage_elig_hist tabpage_elig_hist
tabpage_form_hist tabpage_form_hist
tabpage_alerts tabpage_alerts
end type

on tab_pages.create
this.tabpage_rx_din=create tabpage_rx_din
this.tabpage_rxdin_individ=create tabpage_rxdin_individ
this.tabpage_event=create tabpage_event
this.tabpage_sa=create tabpage_sa
this.tabpage_sa_history=create tabpage_sa_history
this.tabpage_elig_hist=create tabpage_elig_hist
this.tabpage_form_hist=create tabpage_form_hist
this.tabpage_alerts=create tabpage_alerts
this.Control[]={this.tabpage_rx_din,&
this.tabpage_rxdin_individ,&
this.tabpage_event,&
this.tabpage_sa,&
this.tabpage_sa_history,&
this.tabpage_elig_hist,&
this.tabpage_form_hist,&
this.tabpage_alerts}
end on

on tab_pages.destroy
destroy(this.tabpage_rx_din)
destroy(this.tabpage_rxdin_individ)
destroy(this.tabpage_event)
destroy(this.tabpage_sa)
destroy(this.tabpage_sa_history)
destroy(this.tabpage_elig_hist)
destroy(this.tabpage_form_hist)
destroy(this.tabpage_alerts)
end on

event selectionchanged;long ll_rx_special_auth_no, ll_row, ll_cntr


IF newindex = 4 THEN
	
	ll_row = tab_pages.tabpage_sa.dw_rx_special_auth.getrow()
	IF ll_row > 0 THEN
			tab_pages.tabpage_sa.dw_rx_special_auth.GetSelectedRow(0)
			ll_row = tab_pages.tabpage_sa.dw_rx_special_auth.GetRow()
			tab_pages.tabpage_sa.dw_rx_special_auth.SelectRow(ll_row,True)
	         
			//Set-up restriction window
	         ll_rx_special_auth_no = tab_pages.tabpage_sa.dw_rx_special_auth.getitemnumber(ll_row,"rx_special_auth_no")
	         tab_pages.tabpage_sa.dw_sa_restriction.Retrieve(ll_rx_special_auth_no)
		     tab_pages.tabpage_sa.dw_sa_restriction.uf_protect_allattributes(TRUE)
	END IF
END IF

IF newindex = 5 THEN

	//Set-up restriction history window
	ll_row = tab_pages.tabpage_sa_history.dw_rx_special_auth_history.getrow()
	
	IF ll_row > 0 THEN
			tab_pages.tabpage_sa_history.dw_rx_special_auth_history.GetSelectedRow(0)
			ll_row = tab_pages.tabpage_sa_history.dw_rx_special_auth_history.GetRow()
			tab_pages.tabpage_sa_history.dw_rx_special_auth_history.SelectRow(ll_row,True)
	         
			//Set-up restriction window
	         ll_rx_special_auth_no = tab_pages.tabpage_sa_history.dw_rx_special_auth_history.getitemnumber(ll_row,"rx_special_auth_no")
	         tab_pages.tabpage_sa_history.dw_restriction_history.Retrieve(ll_rx_special_auth_no)
	
		     tab_pages.tabpage_sa_history.dw_restriction_history.uf_protect_allattributes(TRUE)
	
	END IF
END IF

cb_drug_alerts.Enabled = FALSE
cb_terminate.Enabled = FALSE	

IF newindex = 8  THEN
	tab_pages.tabpage_alerts.dw_drug_alert.Object.DataWindow.HorizontalScrollPosition=1
	IF cb_terminate.Visible = FALSE THEN
		tab_pages.tabpage_alerts.dw_drug_alert.Enabled = FALSE
	END IF
	cb_drug_alerts.Enabled = TRUE
	IF tab_pages.tabpage_alerts.dw_drug_alert.GetRow() > 0 THEN
		IF IsNull(tab_pages.tabpage_alerts.dw_drug_alert.GetItemDateTime(tab_pages.tabpage_alerts.dw_drug_alert.GetRow(), 'terminated_date')) THEN
			cb_terminate.Enabled = TRUE
		END IF
	END IF
END IF

RETURN 1
end event

event selectionchanging;
IF oldindex = 8 THEN
	IF inv_maintain_formulary.nf_check_modified_status(12) = TRUE THEN
		MessageBox("Add Alert Failure","Please Save or Cancel your current Drug Alert before you leave the Drug Alert tab page.")
		tab_pages.tabpage_alerts.dw_drug_alert.SetFocus()
		RETURN 1
	END IF 
END IF
end event

type tabpage_rx_din from userobject within tab_pages
event create ( )
event destroy ( )
integer x = 18
integer y = 100
integer width = 2638
integer height = 716
long backcolor = 67108864
string text = "Rx Din"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
dw_rx dw_rx
end type

on tabpage_rx_din.create
this.dw_rx=create dw_rx
this.Control[]={this.dw_rx}
end on

on tabpage_rx_din.destroy
destroy(this.dw_rx)
end on

type dw_rx from u_dw_online within tabpage_rx_din
integer width = 2633
integer height = 716
integer taborder = 11
string dataobject = "d_formulary_prescription"
boolean hscrollbar = true
boolean vscrollbar = true
boolean border = false
end type

event getfocus;call super::getfocus;/* put code in here to use the filter control */
uo_filter.uf_set_Requestor(tab_pages.tabpage_rx_din.dw_rx)
end event

event rbuttondown;/*	Create the menu -	Note that this only gives default options.  If you want 
   additional options, you should override the ancestor and visible the options 
	you desire.
*/

// make sure there is a valid row
IF ISNULL(row) OR row = 0 THEN RETURN

IF isvalid(im_popup) THEN DESTROY im_popup

im_popup = CREATE m_rx_popup
im_popup.mf_set_datawindow(THIS)
im_popup.m_options.m_sort.visible                  = TRUE	
im_popup.m_options.PopMenu(w_frame.PointerX(), w_frame.PointerY())
end event

type tabpage_rxdin_individ from userobject within tab_pages
integer x = 18
integer y = 100
integer width = 2638
integer height = 716
long backcolor = 67108864
string text = "Din/Individ"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
dw_formulary_pres_individual dw_formulary_pres_individual
end type

on tabpage_rxdin_individ.create
this.dw_formulary_pres_individual=create dw_formulary_pres_individual
this.Control[]={this.dw_formulary_pres_individual}
end on

on tabpage_rxdin_individ.destroy
destroy(this.dw_formulary_pres_individual)
end on

type dw_formulary_pres_individual from u_dw_online within tabpage_rxdin_individ
integer width = 2633
integer height = 716
integer taborder = 11
string dataobject = "d_formulary_pres_individual"
boolean maxbox = true
boolean hscrollbar = true
boolean vscrollbar = true
boolean border = false
end type

event getfocus;call super::getfocus;uo_filter.uf_set_Requestor(this)
end event

event rbuttondown;/*	Create the menu -	Note that this only gives default options.  If you want 
   additional options, you should override the ancestor and visible the options 
	you desire.
*/

// make sure there is a valid row
IF ISNULL(row) OR row = 0 THEN RETURN

IF isvalid(im_popup) THEN DESTROY im_popup

im_popup = CREATE m_rx_popup
im_popup.mf_set_datawindow(THIS)
im_popup.m_options.m_sort.visible                  = TRUE	

im_popup.m_options.PopMenu(w_frame.PointerX(), w_frame.PointerY())
end event

type tabpage_event from userobject within tab_pages
event create ( )
event destroy ( )
integer x = 18
integer y = 100
integer width = 2638
integer height = 716
long backcolor = 67108864
string text = "Event"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
dw_event dw_event
end type

on tabpage_event.create
this.dw_event=create dw_event
this.Control[]={this.dw_event}
end on

on tabpage_event.destroy
destroy(this.dw_event)
end on

type dw_event from u_dw_online within tabpage_event
integer width = 2633
integer height = 716
integer taborder = 11
string dataobject = "d_formulary_event"
boolean hscrollbar = true
boolean vscrollbar = true
boolean border = false
end type

event doubleclicked;call super::doubleclicked;THIS.POSTEVENT("ue_more_details")
end event

event ue_more_details;call super::ue_more_details;INT		li_row, li_rtn, li_count
LONG		ll_event_no, ll_docid, ll_fldid
STRING	ls_event_type,	ls_select, ls_doc_type
W_SHEET	lw_active_sheet

IF tab_pages.tabpage_event.dw_event.RowCount() = 0 THEN RETURN

/* grab the row */
li_row = tab_pages.tabpage_event.dw_event.getrow()

IF ISNULL(li_row) OR li_row < 1  THEN RETURN

ls_event_type = tab_pages.tabpage_event.dw_event.GetItemString(li_row,"event_type_code")
ll_event_no   = tab_pages.tabpage_event.dw_event.GetItemNumber(li_row,"event_no")

IF ISNULL(ls_event_type) OR TRIM(ls_event_type) = "" THEN RETURN -1
IF ISNULL(ll_event_no) OR ll_event_no < 0 THEN RETURN -1
IF ISNULL(il_claim_no) OR il_claim_no < 0 THEN RETURN -1

istr_window_message.as_stringparm[1] = ls_event_type
istr_window_message.al_doubleparm[1] = il_claim_no
istr_window_message.al_doubleparm[2] = ll_event_no

/* If the event is automatic THEN display the right datawindow to show the event data
	if there are more details to see
*/
CHOOSE CASE ls_event_type
	CASE '004' // Incoming Correspondence
		
     /* Get the docid and view the document */
		SELECT doc_id 
	 	  INTO :ll_docid
		  FROM CORRESP_EVENT_DETAIL
		 WHERE event_no = :ll_event_no
		 USING SQLCA;

		 SQLCA.nf_handle_error("w_maintain_formulary","tab_pages.tabpage_event.dw_event", "ue_more_details - SELECT doc_id ") 
				
		 IF ll_docid <= 0 OR IsNull(ll_docid) THEN
			 MessageBox("View Document","Unable to view document from Maintain Formulary. Try document list below.")
		 ELSE
				
				IF wf_archived_document(ll_docid) THEN RETURN
				
				IF uo_image_append.of_init(ll_docid) <= 0 THEN RETURN
				
				ls_doc_type =  uo_image_append.of_get_file_type()

				CHOOSE CASE ls_doc_type
					/*  Imaged document */ 
					CASE 'IMA', 'TIF'
						IF uo_image_append.of_append_image(ll_docid) < 0 THEN RETURN
		
					CASE ELSE
						iw_active_sheet.iu_dw_document_path.f_manage_document(ll_docid,"V","NORMAL")
				END CHOOSE
		END IF
			
		CASE '005' // Outgoing Correspondence
			
         /*	Get the docid and view the document */
			SELECT doc_id 
			  INTO :ll_docid
			  FROM CORRESPONDENCE 
			 WHERE event_no = :ll_event_no
			   AND claim_no = :il_claim_no
			 USING SQLCA;

		    SQLCA.nf_handle_error("w_maintain_formulary","tab_pages.tabpage_event.dw_event","ue_more_details - SELECT doc_id ")
				
			IF ll_docid <= 0 OR IsNull(ll_docid) THEN
				MessageBox("View Document","Unable to view document from Maintain Formulary. Try document list below.")
				RETURN
			END IF
			
			IF wf_archived_document(ll_docid) THEN RETURN
															  
			IF uo_image_append.of_init(ll_docid)	<= 0 THEN RETURN
				
			ls_doc_type =  uo_image_append.of_get_file_type()
			
			CHOOSE CASE ls_doc_type
				/*  Imaged document */ 
				CASE 'IMA', 'TIF'
					IF uo_image_append.of_append_image(ll_docid) < 0 THEN RETURN
			
				CASE ELSE
					iw_active_sheet.iu_dw_document_path.f_manage_document(ll_docid,"V","NORMAL")
			END CHOOSE
					
		CASE '010' // Claim Status Change
	
			istr_window_message.as_stringparm[1] = ls_event_type
			istr_window_message.al_doubleparm[1] = il_claim_no
			istr_window_message.al_doubleparm[2] = ll_event_no

			OpenWithParm(w_event_details,istr_window_message)

		CASE '014' // Cost Allocation Change
			istr_window_message.as_stringparm[1] = ls_event_type
			istr_window_message.al_doubleparm[1] = il_claim_no
			istr_window_message.al_doubleparm[2] = ll_event_no

			OpenWithParm(w_event_details,istr_window_message)

		CASE ELSE
			MessageBox("Event Type","There are no further details for this event type")

END CHOOSE
end event

event ue_print;call super::ue_print;this.Object.DataWindow.Print.orientation = 1
this.print()
end event

event getfocus;call super::getfocus;uo_filter.uf_set_Requestor(this)
end event

event rbuttondown;/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/


// make sure there is a valid row
IF ISNULL(row) OR row = 0 THEN RETURN

IF isvalid(im_popup) THEN destroy im_popup
	
im_popup = CREATE m_rx_popup
im_popup.mf_set_datawindow(tab_pages.tabpage_event.dw_event)
im_popup.m_options.m_sort.visible              = TRUE
im_popup.m_options.m_fulleventcomments.visible = TRUE	
im_popup.m_options.m_sendmessage.visible       = TRUE

im_popup.m_options.PopMenu(w_frame.PointerX(), w_frame.PointerY())
end event

type tabpage_sa from userobject within tab_pages
integer x = 18
integer y = 100
integer width = 2638
integer height = 716
long backcolor = 67108864
string text = "Special Auth"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
dw_sa_restriction dw_sa_restriction
dw_rx_special_auth dw_rx_special_auth
end type

on tabpage_sa.create
this.dw_sa_restriction=create dw_sa_restriction
this.dw_rx_special_auth=create dw_rx_special_auth
this.Control[]={this.dw_sa_restriction,&
this.dw_rx_special_auth}
end on

on tabpage_sa.destroy
destroy(this.dw_sa_restriction)
destroy(this.dw_rx_special_auth)
end on

type dw_sa_restriction from u_dw_online within tabpage_sa
integer y = 460
integer width = 2633
integer height = 256
integer taborder = 11
string dataobject = "d_rx_sa_restriction"
boolean hscrollbar = true
boolean vscrollbar = true
boolean border = false
end type

event clicked;call super::clicked;		
//long ll_row		
//
//ll_row =  tab_pages.tabpage_sa.dw_sa_restriction.RowCount()
//
//IF ll_row > 0 THEN
//	this.SelectRow(0,False)
//	this.SelectRow(row,True)
//    tab_pages.tabpage_sa.dw_sa_restriction.GetSelectedRow(0)
////	ll_row = tab_pages.tabpage_sa.dw_sa_restriction.GetRow()
////	tab_pages.tabpage_sa.dw_sa_restriction.SelectRow(ll_row,True)
//END IF
end event

event rbuttondown;// make sure there is a valid row
IF ISNULL(row) OR row = 0 THEN RETURN

IF isvalid(im_popup) THEN DESTROY im_popup

im_popup = CREATE m_rx_popup
im_popup.mf_set_datawindow(THIS)
im_popup.m_options.m_sort.visible                  = TRUE	
im_popup.m_options.m_restriction_history.visible = TRUE
im_popup.m_options.PopMenu(w_frame.PointerX(), w_frame.PointerY())
end event

type dw_rx_special_auth from u_dw_online within tabpage_sa
integer y = -8
integer width = 2647
integer height = 468
integer taborder = 11
string dataobject = "d_rx_special_auth"
boolean hscrollbar = true
boolean vscrollbar = true
boolean border = false
end type

event clicked;call super::clicked;Long  ll_rx_special_auth_no, ll_row
	
	
		IF row > 0 THEN
			tab_pages.tabpage_sa.dw_rx_special_auth.GetSelectedRow(0)
			ll_row = tab_pages.tabpage_sa.dw_rx_special_auth.GetRow()
			tab_pages.tabpage_sa.dw_rx_special_auth.SelectRow(ll_row,True)
	         
			//Set-up restriction window
	         ll_rx_special_auth_no = tab_pages.tabpage_sa.dw_rx_special_auth.getitemnumber(row,"rx_special_auth_no")
	         tab_pages.tabpage_sa.dw_sa_restriction.Retrieve(ll_rx_special_auth_no)
		
		END IF



	
	

end event

event getfocus;call super::getfocus;uo_filter.uf_set_Requestor(this)

end event

event rbuttondown;// make sure there is a valid row
IF ISNULL(row) OR row = 0 THEN RETURN

IF isvalid(im_popup) THEN DESTROY im_popup

im_popup = CREATE m_rx_popup
im_popup.mf_set_datawindow(THIS)
im_popup.m_options.m_sort.visible                  = TRUE	
im_popup.m_options.PopMenu(w_frame.PointerX(), w_frame.PointerY())
end event

type tabpage_sa_history from userobject within tab_pages
integer x = 18
integer y = 100
integer width = 2638
integer height = 716
long backcolor = 67108864
string text = "SA History"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
dw_restriction_history dw_restriction_history
dw_rx_special_auth_history dw_rx_special_auth_history
end type

on tabpage_sa_history.create
this.dw_restriction_history=create dw_restriction_history
this.dw_rx_special_auth_history=create dw_rx_special_auth_history
this.Control[]={this.dw_restriction_history,&
this.dw_rx_special_auth_history}
end on

on tabpage_sa_history.destroy
destroy(this.dw_restriction_history)
destroy(this.dw_rx_special_auth_history)
end on

type dw_restriction_history from u_dw_online within tabpage_sa_history
integer y = 456
integer width = 2633
integer height = 260
integer taborder = 11
string dataobject = "d_rx_sa_restriction_hist"
boolean hscrollbar = true
boolean vscrollbar = true
boolean border = false
end type

type dw_rx_special_auth_history from u_dw_online within tabpage_sa_history
integer width = 2633
integer height = 456
integer taborder = 11
string dataobject = "d_rx_special_auth_history"
boolean hscrollbar = true
boolean vscrollbar = true
boolean border = false
end type

event getfocus;call super::getfocus;uo_filter.uf_set_Requestor(this)
end event

event rbuttondown;// make sure there is a valid row
IF ISNULL(row) OR row = 0 THEN RETURN

IF isvalid(im_popup) THEN DESTROY im_popup

im_popup = CREATE m_rx_popup
im_popup.mf_set_datawindow(THIS)
im_popup.m_options.m_sort.visible                  = TRUE	
im_popup.m_options.m_restriction_history.visible = TRUE
im_popup.m_options.PopMenu(w_frame.PointerX(), w_frame.PointerY())
end event

event clicked;call super::clicked;Long  ll_rx_special_auth_no, ll_row
	
	
		IF row > 0 THEN
			tab_pages.tabpage_sa_history.dw_rx_special_auth_history.GetSelectedRow(0)
			ll_row = tab_pages.tabpage_sa_history.dw_rx_special_auth_history.GetRow()
			tab_pages.tabpage_sa_history.dw_rx_special_auth_history.SelectRow(ll_row,True)
	         
			//Set-up restriction window
	         ll_rx_special_auth_no = tab_pages.tabpage_sa_history.dw_rx_special_auth_history.getitemnumber(row,"rx_special_auth_no")
	         tab_pages.tabpage_sa_history.dw_restriction_history.Retrieve(ll_rx_special_auth_no)
		
		END IF

end event

type tabpage_elig_hist from userobject within tab_pages
integer x = 18
integer y = 100
integer width = 2638
integer height = 716
long backcolor = 67108864
string text = "Elig History"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
dw_eligibility_history dw_eligibility_history
end type

on tabpage_elig_hist.create
this.dw_eligibility_history=create dw_eligibility_history
this.Control[]={this.dw_eligibility_history}
end on

on tabpage_elig_hist.destroy
destroy(this.dw_eligibility_history)
end on

type dw_eligibility_history from u_dw_online within tabpage_elig_hist
integer width = 2633
integer height = 716
integer taborder = 11
string dataobject = "d_eligibility_history"
boolean hscrollbar = true
boolean vscrollbar = true
boolean border = false
end type

event getfocus;call super::getfocus;uo_filter.uf_set_Requestor(this)
end event

type tabpage_form_hist from userobject within tab_pages
integer x = 18
integer y = 100
integer width = 2638
integer height = 716
long backcolor = 67108864
string text = "Form History"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
dw_formulary_history dw_formulary_history
end type

on tabpage_form_hist.create
this.dw_formulary_history=create dw_formulary_history
this.Control[]={this.dw_formulary_history}
end on

on tabpage_form_hist.destroy
destroy(this.dw_formulary_history)
end on

type dw_formulary_history from u_dw_online within tabpage_form_hist
integer width = 2633
integer height = 716
integer taborder = 11
string dataobject = "d_formulary_history"
boolean hscrollbar = true
boolean vscrollbar = true
boolean border = false
end type

event getfocus;call super::getfocus;//uo_filter.uf_set_Requestor(this)
end event

type tabpage_alerts from userobject within tab_pages
integer x = 18
integer y = 100
integer width = 2638
integer height = 716
long backcolor = 67108864
string text = "Drug Alerts"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
dw_drug_alert dw_drug_alert
end type

on tabpage_alerts.create
this.dw_drug_alert=create dw_drug_alert
this.Control[]={this.dw_drug_alert}
end on

on tabpage_alerts.destroy
destroy(this.dw_drug_alert)
end on

type dw_drug_alert from u_dw_online within tabpage_alerts
integer width = 2633
integer height = 716
integer taborder = 11
string dataobject = "d_drug_alerts"
boolean controlmenu = true
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event editchanged;call super::editchanged;cb_save.enabled   = TRUE
cb_cancel.enabled = TRUE
end event

event rowfocuschanged;call super::rowfocuschanged;inv_maintain_formulary.il_current_row = currentrow

tab_pages.tabpage_alerts.dw_drug_alert.ScrollToRow(currentrow)
tab_pages.tabpage_alerts.dw_drug_alert.SelectRow(currentrow, TRUE)

cb_terminate.Enabled = FALSE

IF currentrow > 0  THEN
	IF IsNull(DATE(tab_pages.tabpage_alerts.dw_drug_alert.GetItemDateTime(currentrow, 'terminated_date'))) THEN
		cb_terminate.Enabled = TRUE
	END IF
END IF


end event

event itemchanged;call super::itemchanged;cb_save.enabled   = TRUE
cb_cancel.enabled = TRUE

THIS.SetColumn('alert_comment')
end event

event retrieveend;call super::retrieveend;THIS.SetRow(1)
THIS.ScrollToRow(1)
THIS.SelectRow(1, TRUE)

IF THIS.RowCount() > 0 THEN
	st_alerts.Visible = TRUE
END IF
end event

event buttonclicked;call super::buttonclicked;DATE ldt_terminate

ldt_terminate = DATE(THIS.GetItemDateTime(row, 'terminated_date'))
IF IsNull(ldt_terminate) THEN
	IF cb_terminate.Enabled = TRUE THEN
		cb_terminate.TriggerEvent(Clicked!)
	END IF
END IF
end event

event rbuttondown;// make sure there is a valid row
IF ISNULL(row) OR row = 0 THEN RETURN

IF isvalid(im_popup) THEN DESTROY im_popup

im_popup = CREATE m_rx_popup
im_popup.mf_set_datawindow(THIS)
im_popup.m_options.m_fullalertcomments.Visible = TRUE	
IF row > 0 THEN
	IF NOT IsNull(DATE( THIS.GetItemDateTime(row, 'terminated_date'))) THEN
		IF THIS.GetItemString(row,'terminated_comment') > '' THEN
			im_popup.m_options.m_fullterminationcomments.Visible = TRUE	
		END IF
	ELSE
		IF cb_terminate.Enabled = TRUE THEN
			im_popup.m_options.m_terminatedrugalert.Visible = TRUE	
		END IF
	END IF
END IF
im_popup.m_options.PopMenu(w_frame.PointerX(), w_frame.PointerY())
end event

type p_1 from picture within w_maintain_formulary
integer x = 5
integer y = 4
integer width = 110
integer height = 96
boolean bringtotop = true
string picturename = "plus_blue_24_h.gif"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_1 from statictext within w_maintain_formulary
integer x = 128
integer y = 8
integer width = 695
integer height = 76
boolean bringtotop = true
integer textsize = -14
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 67108864
string text = "Rx Maintenance"
boolean focusrectangle = false
end type

type sb_splitbar_top from u_splitbar_horizontal within w_maintain_formulary
integer y = 484
integer width = 2651
integer height = 52
boolean bringtotop = true
string text = "Formulary"
long il_min_units_from_top = 200
long il_min_units_from_bottom = 200
end type

event lbuttondown;call super::lbuttondown;this.il_min_units_from_bottom = parent.height -  tab_pages.y +200
end event

type uo_filter from u_filter_control within w_maintain_formulary
integer x = 2025
integer height = 100
integer taborder = 40
boolean bringtotop = true
end type

event ue_filter_changed;call super::ue_filter_changed;idw_datawindow.object.st_filter.text = ls_new_filter
end event

on uo_filter.destroy
call u_filter_control::destroy
end on

type cb_drug_alerts from commandbutton within w_maintain_formulary
integer x = 1742
integer y = 1724
integer width = 283
integer height = 92
integer taborder = 21
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "Add Alert"
end type

event clicked;RETURN wf_call_function("DRUG_ALERT")
end event

type st_alerts from statictext within w_maintain_formulary
boolean visible = false
integer x = 1486
integer y = 32
integer width = 485
integer height = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean italic = true
long textcolor = 255
long backcolor = 67108864
string text = "Drug Alert(s) Exist"
boolean focusrectangle = false
end type

type cb_terminate from commandbutton within w_maintain_formulary
integer x = 1463
integer y = 1724
integer width = 283
integer height = 92
integer taborder = 31
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "End Alert"
end type

event clicked;RETURN wf_call_function("TERMINATE_ALERT")
end event

type cb_add_eligibility from commandbutton within w_maintain_formulary
integer x = 1074
integer y = 1724
integer width = 192
integer height = 92
integer taborder = 11
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "+ Eli&g"
end type

event clicked;/* to add a new claim eligibility */
RETURN wf_call_function("ADD_ELIGIBILITY")
end event

