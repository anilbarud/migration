$PBExportHeader$w_bank_reconciliation_extract.srw
$PBExportComments$Bank Reconciliation Extract Issued cheques for transmission
forward
global type w_bank_reconciliation_extract from window
end type
type cb_reprint_reports from commandbutton within w_bank_reconciliation_extract
end type
type cb_print_details from commandbutton within w_bank_reconciliation_extract
end type
type dw_bank_extract_error_report from u_dw_online within w_bank_reconciliation_extract
end type
type dw_bank_extract_control_report from u_dw_online within w_bank_reconciliation_extract
end type
type dw_bank_reconciliation_extract from u_dw_online within w_bank_reconciliation_extract
end type
type cb_extract from commandbutton within w_bank_reconciliation_extract
end type
type cb_close from commandbutton within w_bank_reconciliation_extract
end type
end forward

global type w_bank_reconciliation_extract from window
integer x = 1851
integer width = 2738
integer height = 2092
boolean titlebar = true
string title = "Send Issued Cheques"
string menuname = "m_cmwb_notools"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 67108864
cb_reprint_reports cb_reprint_reports
cb_print_details cb_print_details
dw_bank_extract_error_report dw_bank_extract_error_report
dw_bank_extract_control_report dw_bank_extract_control_report
dw_bank_reconciliation_extract dw_bank_reconciliation_extract
cb_extract cb_extract
cb_close cb_close
end type
global w_bank_reconciliation_extract w_bank_reconciliation_extract

on w_bank_reconciliation_extract.create
if this.MenuName = "m_cmwb_notools" then this.MenuID = create m_cmwb_notools
this.cb_reprint_reports=create cb_reprint_reports
this.cb_print_details=create cb_print_details
this.dw_bank_extract_error_report=create dw_bank_extract_error_report
this.dw_bank_extract_control_report=create dw_bank_extract_control_report
this.dw_bank_reconciliation_extract=create dw_bank_reconciliation_extract
this.cb_extract=create cb_extract
this.cb_close=create cb_close
this.Control[]={this.cb_reprint_reports,&
this.cb_print_details,&
this.dw_bank_extract_error_report,&
this.dw_bank_extract_control_report,&
this.dw_bank_reconciliation_extract,&
this.cb_extract,&
this.cb_close}
end on

on w_bank_reconciliation_extract.destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_reprint_reports)
destroy(this.cb_print_details)
destroy(this.dw_bank_extract_error_report)
destroy(this.dw_bank_extract_control_report)
destroy(this.dw_bank_reconciliation_extract)
destroy(this.cb_extract)
destroy(this.cb_close)
end on

event open;Long    ll_num_rows
Integer li_rtn

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


SetPointer(HourGlass!)

// Retrieve all cheques where processed and printed dates aren't null and transmit and reconcilied dates are null
li_rtn = dw_bank_reconciliation_extract.SetTransObject(SQLCA)
ll_num_rows = dw_bank_reconciliation_extract.Retrieve()
li_rtn = SQLCA.nf_handle_error("w_bank_reconciliation_extract","dw_bank_reconciliation_extract","open w_bank_reconciliation_extract")
IF li_rtn < 0 THEN
	Close(This)
	Return 
END IF





end event

event closequery;Integer li_rtn

IF cb_reprint_reports.Enabled = TRUE THEN
	li_rtn = MessageBox("Print Ok?", "Did the Reports Print Ok?~rBy clicking yes you won't be able to print " +&
							  "them again.", Question!, YesNo!, 2)
	IF li_rtn = 2 THEN
		RETURN 1
	END IF
END IF
end event

type cb_reprint_reports from commandbutton within w_bank_reconciliation_extract
integer x = 850
integer y = 1792
integer width = 462
integer height = 100
integer taborder = 30
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Reprint Reports"
end type

event clicked;IF dw_bank_extract_control_report.RowCount() > 0 THEN
	dw_bank_extract_control_report.Print()
END IF

dw_bank_extract_error_report.Print()
end event

type cb_print_details from commandbutton within w_bank_reconciliation_extract
integer x = 1376
integer y = 1792
integer width = 462
integer height = 100
integer taborder = 40
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Print Details"
end type

event clicked;dw_bank_reconciliation_extract.Print()
end event

type dw_bank_extract_error_report from u_dw_online within w_bank_reconciliation_extract
boolean visible = false
integer x = 146
integer y = 1804
integer width = 69
integer height = 64
integer taborder = 0
string dataobject = "d_bank_extract_error_report"
borderstyle borderstyle = stylelowered!
end type

type dw_bank_extract_control_report from u_dw_online within w_bank_reconciliation_extract
boolean visible = false
integer x = 41
integer y = 1804
integer width = 69
integer height = 64
integer taborder = 0
string dataobject = "d_bank_extract_control_report"
borderstyle borderstyle = stylelowered!
end type

type dw_bank_reconciliation_extract from u_dw_online within w_bank_reconciliation_extract
integer x = 14
integer y = 12
integer width = 2674
integer height = 1764
integer taborder = 10
string dataobject = "d_bank_reconciliation_extract"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

type cb_extract from commandbutton within w_bank_reconciliation_extract
integer x = 325
integer y = 1792
integer width = 462
integer height = 100
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Extract"
end type

event clicked;Long       ll_filenum, ll_last_bank_transfer_no, ll_num_cheques, n, ll_cheque_no, ll_rtn, ll_row
Long       ll_num_cheques_database, ll_num_rows_updated, ll_num_file_headers_created
Long       ll_num_file_details_created, ll_num_file_trailers_created
Integer    li_rtn
Decimal{2} ld_cheque_amount, ld_total_amount
Decimal{2} ld_dollar_value_database, ld_dollar_value_updated, ld_dollar_value_file_details_created
String     ls_whscc_bank_transit_no, ls_whscc_account_no, ls_transaction_code, ls_header
String     ls_file_control_number, ls_finance_network, ls_cheque_no, ls_cheque_amount, ls_issue_date
String     ls_detail, ls_total_record_count, ls_total_amount, ls_trailer, ls_payrec_file, ls_extractbkupdir
String     ls_backup_file, ls_period, ls_title, ls_message
DateTime   ldt_issue_date, ldt_server_datetime
Boolean    lb_error_already

ldt_server_datetime = f_server_datetime()
ldt_server_datetime = Datetime(Date(ldt_server_datetime), Time("00:00:00"))
lb_error_already = FALSE

// Set Values for Control Report
ll_num_cheques_database = dw_bank_reconciliation_extract.RowCount()
IF ll_num_cheques_database > 0 THEN
	ld_dollar_value_database = dw_bank_reconciliation_extract.GetItemDecimal(1, "sum_cheque_amount")
ELSE
	ld_dollar_value_database = 0
END IF
ll_num_rows_updated = 0
ld_dollar_value_updated = 0
ll_num_file_details_created = 0
ld_dollar_value_file_details_created = 0

// If there's no cheques then just print out error and control Report
IF ll_num_cheques_database = 0 OR IsNull(ll_num_cheques_database) = TRUE THEN
	MessageBox("No Cheques","There are no cheques to be transmitted.", Information!)
	ls_message = "There are no cheques to be transmitted."
	lb_error_already = TRUE
	GOTO Error_Recovery
END IF

// Open parrec.p01 in the backup directory.  This will be a working copy.
// When the Extract successfully finishes a copy will be copied to the Finance Directory 
// and another copy to the Backup directory.
ls_extractbkupdir = Upper(ProfileString(vgs_ini_filename, "BANK RECONCILIATION", "ExtractBkUpDir", ""))
IF ls_extractbkupdir = "" THEN
	MessageBox("Profile not found","An Error occurred trying to find the BANK RECONCILIATION - ExtractBkUpDir " +&
				  "profile in the cmwb.ini.~rPhone the HelpDesk.", StopSign!)
	ls_message = "An Error occurred trying to find the BANK RECONCILIATION - ExtractBkUpDir profile in the cmwb.ini.  Phone the HelpDesk."
	lb_error_already = TRUE
	GOTO Error_Recovery
END IF

ls_payrec_file = ls_extractbkupdir + "payrec.p01"
ll_last_bank_transfer_no = 0

ll_filenum = FileOpen(ls_payrec_file, StreamMode!, Write!, LockReadWrite!, Replace!)
IF ll_filenum = -1 OR IsNull(ll_filenum) = TRUE THEN
	MessageBox("Error Opening File","An Error occured while trying to open/create file " + ls_payrec_file + ".~r" +&
				  "Phone the Helpdesk.", StopSign!)
	ls_message = "An Error occured while trying to open/create file " + ls_payrec_file + ".  Phone the Helpdesk."
	lb_error_already = TRUE
	GOTO Error_Recovery
END IF

// Write the File Descriptor (SR 184 - Bank Internet Changes)
li_rtn = FileWrite(ll_filenum, "$$2B01REA12B01$$~n")
IF li_rtn = -1 OR IsNull(li_rtn) = TRUE THEN
	MessageBox("Error Writing to File", "An occurred while writing the File Descriptor record to " + ls_payrec_file + ".~r" +&
				  "Phone the Helpdesk.", StopSign!)
	ls_message = "An occurred while writing the File Descriptor record to " + ls_payrec_file + ".  Phone the Helpdesk."
	lb_error_already = TRUE
	GOTO Error_Recovery
END IF

// Create the Header Record
ls_transaction_code = "000"

// Get the Bank Information, don't need where clause because there is only one row in table.
SELECT bank_transit_no, account_no
  INTO :ls_whscc_bank_transit_no, :ls_whscc_account_no
  FROM Banking_Data ;

li_rtn = SQLCA.nf_handle_error("w_bank_reconciliation_extract","","cb_extract - SELECT bank_transit_no, account_no FROM Banking_Data")
ll_num_file_headers_created = ll_num_file_headers_created + 1




SQLCA.nf_begin_transaction()

// Get the File Control Number
UPDATE Last_Bank_Transfer_No 
   SET last_bank_transfer_no = last_bank_transfer_no + 1 ;

li_rtn = SQLCA.nf_handle_error("w_bank_reconciliation_extract","","cb_extract - UPDATE Last_Bank_Transfer_No")

SELECT last_bank_transfer_no 
  INTO :ll_last_bank_transfer_no 
  FROM Last_Bank_Transfer_No ;

li_rtn = SQLCA.nf_handle_error("w_bank_reconciliation_extract","","cb_extract - SELECT last_bank_transfer_no FROM Last_Bank_Transfer_No")

// Put the File Control Number on the Datawindow
dw_bank_reconciliation_extract.Modify("title2.Text='Extract Issued Cheques - Bank Control Number: " + String(ll_last_bank_transfer_no, "0000") + "'")

ls_file_control_number = String(ll_last_bank_transfer_no, "0000")
ls_header = ls_transaction_code + ls_whscc_bank_transit_no + ls_whscc_account_no + ls_file_control_number + Space(61) + "~n"

li_rtn = FileWrite(ll_filenum, ls_header)
IF li_rtn = -1 OR IsNull(li_rtn) = TRUE THEN
	SQLCA.nf_rollback_transaction()
	MessageBox("Error Writing to File", "An occurred while writing the header record to " + ls_payrec_file + ".~r" +&
				  "Phone the Helpdesk.", StopSign!)
	ls_message = "An occurred while writing the header record to " + ls_payrec_file + ".  Phone the Helpdesk."
	lb_error_already = TRUE
	GOTO Error_Recovery
END IF

// Write the Detail Records and CHEQUE_HEADER.transmit_date
ls_transaction_code = "300"
ll_num_cheques = dw_bank_reconciliation_extract.RowCount()
FOR n = 1 TO ll_num_cheques
	ll_cheque_no = dw_bank_reconciliation_extract.GetItemNumber(n, "cheque_no")
	ld_cheque_amount = dw_bank_reconciliation_extract.GetItemDecimal(n, "cheque_amount")
	ldt_issue_date = dw_bank_reconciliation_extract.GetItemDateTime(n, "cheque_date")
	ld_total_amount = ld_total_amount + ld_cheque_amount

	// Format the data for the output file
	ls_cheque_no = String(ll_cheque_no, "00000000")
	ls_cheque_amount = String(ld_cheque_amount * 100, "0000000000")
	ls_issue_date = String(ldt_issue_date, "MMDDYY")

	ls_detail = ls_transaction_code + ls_whscc_bank_transit_no + ls_whscc_account_no + ls_cheque_no +&
					ls_cheque_amount + Space(19) + Space(8) + ls_issue_date + Space(14) + "~n"

	// Write the Detail Line
	li_rtn = FileWrite(ll_filenum, ls_detail)
	IF li_rtn = -1 OR IsNull(li_rtn) = TRUE THEN
		SQLCA.nf_rollback_transaction()
		MessageBox("Error Writing to File", "An occurred while writing the detail record with cheque number = " +&
					  + String(ll_cheque_no, "#####0") + " and cheque amount = " + String(ld_cheque_amount, "$#,###.00") +&
					  " to the " + ls_payrec_file + ".~rPhone the Helpdesk.", StopSign!)
		ls_message = "An occurred while writing the detail record with cheque number = " +&
						 String(ll_cheque_no, "#####0") + " and cheque amount = " + String(ld_cheque_amount, "$#,###.00") +&
						 " to the " + ls_payrec_file + ".  Phone the Helpdesk."
		lb_error_already = TRUE
		GOTO Error_Recovery
	END IF
	ll_num_file_details_created = ll_num_file_details_created + 1
	ld_dollar_value_file_details_created = ld_dollar_value_file_details_created + ld_cheque_amount
	
	dw_bank_reconciliation_extract.SetItem(n, "transmit_date", ldt_server_datetime)
NEXT

// Update CHEQUE_HEADER table
IF ll_num_cheques > 0 THEN
	dw_bank_reconciliation_extract.AcceptText()
	li_rtn = dw_bank_reconciliation_extract.Update()
	ll_num_rows_updated = ll_num_cheques
	li_rtn = SQLCA.nf_handle_error("w_bank_reconciliation_extract","dw_bank_reconciliation_extract","cb_extract - dw_bank_reconciliation_extract.Update()")
	ld_dollar_value_updated = ld_total_amount
ELSE
	ld_dollar_value_updated = 0
	ll_num_rows_updated = 0
END IF

// Write the Trailing Record
ls_transaction_code = "999"

ls_total_record_count = String(ll_num_cheques + 2, "00000000")
ld_total_amount = ld_total_amount * 100
ls_total_amount = String(ld_total_amount, "0000000000000")

ls_trailer = ls_transaction_code + ls_whscc_bank_transit_no + ls_whscc_account_no + ls_total_record_count +&
				 ls_total_amount + Space(44) + "~n"

li_rtn = FileWrite(ll_filenum, ls_trailer)
IF li_rtn = -1 OR IsNull(li_rtn) = TRUE THEN
	SQLCA.nf_rollback_transaction()
	
	MessageBox("Error Writing to File", "An occurred while writing the trailer record to " + ls_payrec_file +&
				  ".~rPhone the Helpdesk.", StopSign!)
	ls_message = "An occurred while writing the trailer record to " + ls_payrec_file + ".  Phone the Helpdesk."
	lb_error_already = TRUE
	GOTO Error_Recovery
END IF
ll_num_file_trailers_created = ll_num_file_trailers_created + 1

// Close the File
li_rtn = FileClose(ll_filenum)
IF li_rtn = -1 OR IsNull(li_rtn) = TRUE THEN
	SQLCA.nf_rollback_transaction()
	
	MessageBox("Error Closing File", "An occurred while closing " + ls_payrec_file + ".~r" +&
				  "Phone the Helpdesk.", StopSign!)
	ls_message = "An occurred while closing " + ls_payrec_file + ".  Phone the Helpdesk."
	lb_error_already = TRUE
	GOTO Error_Recovery
END IF

SQLCA.nf_commit_transaction()


cb_extract.Enabled = FALSE

// Copy the File to the Finance Network Share
ls_finance_network = Upper(ProfileString(vgs_ini_filename, "BANK RECONCILIATION", "ExtractDir", ""))
IF ls_finance_network = "" THEN
	MessageBox("Profile not found", "An Error occurred trying to find the BANK RECONCILIATION - ExtractDir " +&
				  "profile in the cmwb.ini.  The file " + ls_payrec_file + " needs to be manually copied to " +&
				  "Finance Network so that it will be sent to the bank.", StopSign!)
	ls_message = "An Error occurred trying to find the BANK RECONCILIATION - ExtractDir profile in the cmwb.ini." +&
					 "The file " + ls_payrec_file + " needs to be manually copied to Finance Network so that it will " +&
					 "be sent to the bank."
	lb_error_already = TRUE
	GOTO Error_Recovery
END IF

ls_finance_network = ls_finance_network + "payrec.p01"
li_rtn = f_copyfile(ls_payrec_file, ls_finance_network)
IF li_rtn = -1 THEN
	ls_message = "An error occurred while trying to copy file " + ls_payrec_file + " to " + ls_finance_network + ".  " +&
					 "The file " + ls_payrec_file + " needs to be manually copied to " + ls_finance_network + " so that " +&
					 "it will be sent to the bank."
	lb_error_already = TRUE
	GOTO Error_Recovery
END IF

// Copy the file to the Backup Network Share in the appropriate YYYYMM directory 
// and rename the file to payrecdd.p01
ls_extractbkupdir = Upper(ProfileString(vgs_ini_filename, "BANK RECONCILIATION", "ExtractBkUpDir", ""))
ls_period = String(ldt_server_datetime, "YYYYMM")
ls_extractbkupdir = ls_extractbkupdir + ls_period 

ll_rtn = CanAccess(ls_extractbkupdir, 00)
IF ll_rtn <> 0 THEN
	ll_rtn = CreateDirectory(ls_extractbkupdir)
	IF ll_rtn <> 0 THEN
		MessageBox("Error Creating Directory", "Could not create the directory " + ls_extractbkupdir + ".~r" +&
					  "This directory needs to be created so we can store a backup copy of " + ls_payrec_file + ".",Exclamation!)
		ls_message = "Could not create the directory " + ls_extractbkupdir + ".  This directory needs to be created " +&
						 "so we can store a backup copy of " + ls_payrec_file + ".  This will have to be done manually so " +&
						 "create a directory called " + ls_extractbkupdir + " and copy " + ls_payrec_file +&
						 " to this new directory."
		lb_error_already = TRUE
		GOTO Error_Recovery
	END IF
END IF

ls_backup_file = ls_extractbkupdir + "\PAYREC" + String(ldt_server_datetime, "DD") + ".P01"

li_rtn = f_copyfile(ls_payrec_file, ls_backup_file)
IF li_rtn = -1 THEN
	ls_message = "An error occurred while trying to copy file " + ls_payrec_file + " to " + ls_backup_file + "." +&
					 "This will have to be done manually so copy " + ls_payrec_file + " to the " + ls_backup_file +&
					 " right now so that there will be a backup of this file."
	lb_error_already = TRUE
	GOTO Error_Recovery
END IF

MessageBox("Extract Complete", "The Bank Reconciliation Extract has completed.~rCheck the reports to verify everything ran ok.", Information!)

ls_message = "No Errors."

Error_Recovery:

// Create and Print the Control Report
ls_title = "Control Report for Extract Issued Cheques - Bank Control Number " + String(ll_last_bank_transfer_no)

ll_row = dw_bank_extract_control_report.InsertRow(0)
dw_bank_extract_control_report.SetItem(ll_row, "title", ls_title)

dw_bank_extract_control_report.SetItem(ll_row, "num_cheques_database", ll_num_cheques_database)
dw_bank_extract_control_report.SetItem(ll_row, "dollar_value_database", ld_dollar_value_database)
dw_bank_extract_control_report.SetItem(ll_row, "num_rows_updated", ll_num_rows_updated)
dw_bank_extract_control_report.SetItem(ll_row, "dollar_value_updated", ld_dollar_value_updated)
dw_bank_extract_control_report.SetItem(ll_row, "num_file_headers_created", ll_num_file_headers_created)
dw_bank_extract_control_report.SetItem(ll_row, "num_file_details_created", ll_num_file_details_created)
dw_bank_extract_control_report.SetItem(ll_row, "dollar_value_file_details_created", ld_dollar_value_file_details_created)
dw_bank_extract_control_report.SetItem(ll_row, "num_file_trailers_created", ll_num_file_trailers_created)

dw_bank_extract_control_report.Print()

// Create and Print Error Report
ls_title = "Error Report for Extract Issued Cheques - Bank Control Number " + String(ll_last_bank_transfer_no)

IF ll_num_cheques_database <> ll_num_file_details_created THEN
	IF lb_error_already = FALSE THEN
		ls_message = ""
	ELSE
		ls_message = ls_message + "~r~r"
	END IF
	ls_message = ls_message + "The Number of Cheque Header Records issued but not transmitted in the database is " +&
					 String(ll_num_cheques_database) + " does not match the number of detail records created " +&
					 "in the PAYREC.P01 file: " + String(ll_num_file_details_created) + "."
	lb_error_already = TRUE
END IF

IF ld_dollar_value_database <> ld_dollar_value_file_details_created THEN
	IF lb_error_already = FALSE THEN
		ls_message = ""
	ELSE
		ls_message = ls_message + "~r~r"
	END IF
	ls_message = ls_message + "The Dollar Value of Cheque Header Records issued but not transmitted in the database " +&
					 String(ld_dollar_value_database, "$#,##0.00") + " does not match the Dollar Value of detail records " +&
					 "created in the PAYREC.P01 file: " + String(ld_dollar_value_file_details_created, "$#,##0.00") + "."
	lb_error_already = TRUE
END IF

IF ll_num_file_details_created <> ll_num_rows_updated THEN
	IF lb_error_already = FALSE THEN
		ls_message = ""
	ELSE
		ls_message = ls_message + "~r~r"
	END IF
	ls_message = "The number of detail records created in the PAYREC.P01 file " + String(ll_num_file_details_created) +&
					 " does not match the number of Cheque Header records updated with transmit date in the database: " +&
					 String(ll_num_rows_updated) + "."
	lb_error_already = TRUE
END IF

IF ld_dollar_value_file_details_created <> ld_dollar_value_updated THEN
	IF lb_error_already = FALSE THEN
		ls_message = ""
	ELSE
		ls_message = ls_message + "~r~r"
	END IF
	ls_message = ls_message + "The Dollar Value of detail records created in the PAYREC.P01 file " +&
					 String(ld_dollar_value_file_details_created, "$#,##0.00") + " does not match the Dollar Value of " +&
					 " Cheque Header records updated with transmit date in the database: " +&
					 String(ld_dollar_value_updated, "$#,##0.00") + "."
	lb_error_already = TRUE
END IF

ll_row = dw_bank_extract_error_report.InsertRow(0)
dw_bank_extract_error_report.SetItem(ll_row, "title", ls_title)
dw_bank_extract_error_report.SetItem(ll_row, "message", ls_message)
dw_bank_extract_error_report.Print()

cb_reprint_reports.Enabled = TRUE
cb_print_details.Enabled = TRUE
end event

type cb_close from commandbutton within w_bank_reconciliation_extract
integer x = 1902
integer y = 1792
integer width = 462
integer height = 100
integer taborder = 50
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Close"
end type

event clicked;Close(Parent)
end event

