$PBExportHeader$nvo_verify_bank_reconciliation.sru
$PBExportComments$Non-Visual User Object that contains functions related to the verifying of payment data for Reconciled Cheques
forward
global type nvo_verify_bank_reconciliation from nonvisualobject
end type
end forward

global type nvo_verify_bank_reconciliation from nonvisualobject
end type
global nvo_verify_bank_reconciliation nvo_verify_bank_reconciliation

type variables
string  is_finance_network
string is_payrec_file
integer il_filenum

string is_bankrecbkupdir
string is_backup_file

long il_bank_import_no
long il_no_of_cheques
long il_no_of_headers
long il_no_of_trailers 
long il_no_of_details 

string is_header_bank_transit_no
string is_header_account_no
string is_header_filler	
long   il_header_bank_import_no 


string is_trailer_bank_transit_no
string is_trailer_account_no
string is_trailer_filler
long il_trailer_bank_import_no 

string is_header_bank_create_date
string il_header_filler
string is_trailer_all_record_count

string is_trailer_total_amount 




DATASTORE ids_bank_reconcile_error_report
DATASTORE ids_bank_reconcile_control_report


LONG il_detail_bank_import_no
string is_detail_bank_transit_no
string is_detail_account_no
string is_detail_bank_record_type_code
LONG  il_seq_no

STRING is_detail_filler
STRING is_detail_cheque_no
STRING is_detail_cheque_amount
string is_detail_cheque_date
string is_detail_bank_process_date
string is_detail_bank_ecord_type_code
string is_detail_bank_txn_code
string is_detail_tolerance_ind
string is_detail_tolerance_amount
string is_detail_reconciled_code
long il_no_of_unknown
long il_no_of_records_written_to_suspense
string  is_trans_code
string  is_filler
string is_transit_no
string is_account_no

long il_no_of_records_written_to_header
long il_no_of_records_written_to_details
long il_no_of_records_written_to_trailer
long il_all_record_count
long il_detail_cheque_no


decimal {2} idcm_sum_cheque_amount
decimal {2} idcm_total_cheque_amount
decimal {2} idcm_detail_tolerance_amount
decimal {2} idcm_detail_cheque_amount



long il_no_of_cheques_with_invalid_statuses
long il_count_cheque_header
long il_no_of_cheques_never_issued
long il_no_of_cheques_never_transmitted
long il_no_of_cheques_already_reconciled
long il_no_of_cheques_issue_date_no_match
long il_no_cheques_reconciled
long il_no_of_cheques_with_invalid_tolerance = 0

long il_no_of_cheques_431_00
long il_no_of_cheques_431_09
long il_no_of_cheques_180_00
long il_no_of_cheques_180_09
long il_no_of_cheques_157_07
long il_no_of_cheques_432_16 

decimal {2} idcm_amount_of_cheques_431_00
decimal {2} idcm_amount_of_cheques_431_09
decimal {2} idcm_amount_of_cheques_180_00
decimal {2} idcm_amount_of_cheques_180_09
decimal {2} idcm_amount_of_cheques_157_07
decimal {2} idcm_amount_of_cheques_432_16 
decimal {2} idcm_amount_of_cheques_reconciled
decimal {2} idcm_amount_of_cheques_not_reconciled = 0
end variables

forward prototypes
public function integer of_load_file (string as_file_name)
public function integer of_close_file ()
public function integer of_backup_file ()
public function integer of_get_bank_import_no ()
public function integer of_load_file_to_table ()
public function integer of_get_no_cheques_to_process ()
public subroutine of_create_datastores ()
public subroutine of_destroy_datastores ()
public subroutine of_print_error_report ()
public subroutine of_print_control_report ()
public function integer of_validate_table ()
public function integer of_populate_header ()
public function integer of_populate_trailer ()
public function integer of_populate_details (datawindow adw_dw)
public function integer of_produce_controls ()
public subroutine of_get_processing_information (ref string as_payrec_file, ref string as_backup_file, ref string as_bank_import_no, ref string as_no_of_cheques)
public function long of_select_bank_import_no ()
public subroutine of_produce_control_report ()
public function integer of_remove_imported_file ()
public function integer of_perform_match (datawindow adw_dw)
end prototypes

public function integer of_load_file (string as_file_name);integer li_row

/* Get the Finance Network path where F122S14.001 */
is_finance_network = Upper(ProfileString(vgs_ini_filename,"BANK RECONCILIATION", "PayVerifyDir", ""))
is_payrec_file = is_finance_network + as_file_name
il_filenum = FileOpen(is_payrec_file, LineMode!, Read!, LockReadWrite!, Replace!)
if il_filenum > 0 then
else
	MessageBox("File Open","The file you are trying to open " + is_payrec_file + " does not exist or there is a problem with the file.",StopSign!)
	li_row = ids_bank_reconcile_error_report.insertrow(0)
	ids_bank_reconcile_error_report.setitem(li_row,"message","Problem finding file F122S14.001")
	Return -1
end if



return 1
end function

public function integer of_close_file ();integer li_rtn, li_row

// Close the File
li_rtn = FileClose(il_filenum)
IF li_rtn = -1 OR IsNull(li_rtn) = TRUE THEN
	MessageBox("Close File","Problem closing file " + is_payrec_file  ,StopSign!)
	li_row = ids_bank_reconcile_error_report.insertrow(0)
	ids_bank_reconcile_error_report.setitem(li_row,"message","Problem closing file")
	return -1
END IF

return 1
end function

public function integer of_backup_file ();string ls_bankrecbkupdir
string ls_period
datetime ldt_server_datetime 
integer li_rtn, li_row
long  ll_rtn

ldt_server_datetime = f_server_datetime()

// Copy the file to the Backup Network Share in the appropriate YYYYMM directory  and rename the file */
is_bankrecbkupdir = Upper(ProfileString(vgs_ini_filename, "BANK RECONCILIATION", "PayVerifyBkUpDir", ""))
ls_period = String(ldt_server_datetime, "YYYYMM")
is_bankrecbkupdir = is_bankrecbkupdir + ls_period 


ll_rtn = CanAccess(is_bankrecbkupdir, 00)
IF ll_rtn <> 0 THEN
	ll_rtn = CreateDirectory(is_bankrecbkupdir)
	IF ll_rtn <> 0 THEN
		MessageBox("Creating Backup Directory","Problem creating backup directory " + is_bankrecbkupdir  ,StopSign!)
		li_row = ids_bank_reconcile_error_report.insertrow(0)
		ids_bank_reconcile_error_report.setitem(li_row,"message","Problem creating directory for backup")
		return -1
	END IF
END IF

is_backup_file = is_bankrecbkupdir + "\VERIFY" + String(ldt_server_datetime, "DD") + ".001"

li_rtn = f_copyfile(is_payrec_file, is_backup_file)
IF li_rtn = -1 THEN
	MessageBox("Creating Backup","Problem writing backup file " + is_backup_file,StopSign!)
	li_row = ids_bank_reconcile_error_report.insertrow(0)
	ids_bank_reconcile_error_report.setitem(li_row,"message","Problem coping file for backup")
	return -1
END IF

return 1

end function

public function integer of_get_bank_import_no ();integer li_row

/* get a unique import no */
UPDATE Last_Bank_Import_No  
   SET last_bank_import_no = last_bank_import_no + 1 
 using SQLCA;
IF SQLCA.nf_handle_error('w_bank_reconcile_cheques', 'nvo_verify_bank_reconciliation', 'of_get_bank_import_no - UPDATE Last_Bank_Import_No') < 0 THEN 
	li_row = ids_bank_reconcile_error_report.insertrow(0)
	ids_bank_reconcile_error_report.setitem(li_row,"message","Problem updating Last_Bank_Import_No")	
	Return -1
END IF

SELECT last_bank_import_no 
  INTO :il_bank_import_no 
  FROM Last_Bank_Import_No 
 using SQLCA;
IF SQLCA.nf_handle_error('w_bank_reconcile_cheques', 'nvo_verify_bank_reconciliation', 'of_get_bank_import_no - SELECT last_bank_import_no FROM Last_Bank_Import_No') < 0 THEN 
	li_row = ids_bank_reconcile_error_report.insertrow(0)
	ids_bank_reconcile_error_report.setitem(li_row,"message","Problem selecting Last_Bank_Import_No")	
	Return -1
END IF



return 1
end function

public function integer of_load_file_to_table ();string ls_data_string
integer li_rtn, li_row
string ls_report_code
string ls_transit_no
string ls_account_no
string ls_filler
long ll_no_records_read


li_rtn  = fileread(il_filenum,ls_data_string)
if li_rtn < 1 and  li_rtn <> -100 then 
	li_row = ids_bank_reconcile_error_report.insertrow(0)
	ids_bank_reconcile_error_report.setitem(li_row,"message","Problem reading file F122S14.001")
	return -1
end if


/* Read the file a row at a time */
do while li_rtn <> -100
	ls_report_code = mid(ls_data_string,13,2)
	ls_transit_no 	= mid(ls_data_string,1,5)
	ls_account_no 	= mid(ls_data_string,6,7)
	ls_filler      = mid(ls_data_string,15,65)
	
   INSERT INTO IMPORTED_PROCESSED_CHEQUE 
	(bank_import_no,
	seq_no,
	bank_transit_no,
	account_no,
	bank_record_type_code,
	filler)  
	VALUES ( 
	:il_bank_import_no ,
	:ll_no_records_read + 1 ,
	:ls_transit_no,
	:ls_account_no,
	:ls_report_code,
	:ls_filler)	
	using SQLCA ;
	IF SQLCA.nf_handle_error('INSERTING INTO IMPORTED_PROCESSED_CHEQUE  ', 'w_bank_reconcile_cheques', 'cb_receiving_cheque_data') < 0 THEN 
		li_row = ids_bank_reconcile_error_report.insertrow(0)
		ids_bank_reconcile_error_report.setitem(li_row,"message","Problem inserting into IMPORTED_PROCESSED_CHEQUE")
		Return -1
	end if	
	li_rtn = fileread(il_filenum,ls_data_string)
	
	if li_rtn < 1 and  li_rtn <> -100 then 
		li_row = ids_bank_reconcile_error_report.insertrow(0)
		ids_bank_reconcile_error_report.setitem(li_row,"message","Problem READING FILE F122S14.001")
		return -1
	end if
	ll_no_records_read++
loop




return 1



end function

public function integer of_get_no_cheques_to_process ();integer li_row

/* the following are data integrity validations of the imported file */
select count(*) into :il_no_of_cheques
from IMPORTED_PROCESSED_CHEQUE 
WHERE IMPORTED_PROCESSED_CHEQUE.bank_import_no = :il_bank_import_no 
using SQLCA;
IF SQLCA.nf_handle_error('SELECTING COUNT(*) IMPORTED_PROCESSED_CHEQUE  ', 'w_bank_reconcile_cheques', 'cb_receiving_cheque_data') < 0 THEN 
	li_row = ids_bank_reconcile_error_report.insertrow(0)
	ids_bank_reconcile_error_report.setitem(li_row,"message","Problem selecting from IMPORTED_PROCESSED_CHEQUE")	
	Return -1
END IF



return 1
end function

public subroutine of_create_datastores ();ids_bank_reconcile_error_report = CREATE DATASTORE
ids_bank_reconcile_error_report.DataObject = 'd_bank_reconcile_error_report'
//ids_bank_reconcile_error_report.SetTransObject(SQLCA)

ids_bank_reconcile_control_report = CREATE DATASTORE
ids_bank_reconcile_control_report.DataObject = 'd_bank_reconcile_control_report'
//ids_bank_reconcile_control_report.SetTransObject(SQLCA)




end subroutine

public subroutine of_destroy_datastores ();destroy(ids_bank_reconcile_error_report)
destroy(ids_bank_reconcile_control_report)

end subroutine

public subroutine of_print_error_report ();IF ids_bank_reconcile_error_report.RowCount() > 0 THEN
	ids_bank_reconcile_error_report.Print()
END IF

end subroutine

public subroutine of_print_control_report ();IF ids_bank_reconcile_control_report.RowCount() > 0 THEN 
	ids_bank_reconcile_control_report.print()
END IF

end subroutine

public function integer of_validate_table ();// of_validate_table
//
Integer li_row
String  ls_message, ls_bank_transit_no, ls_account_no

// how many header records were loaded
SELECT COUNT(*) 
  INTO :il_no_of_headers 
  FROM IMPORTED_PROCESSED_CHEQUE 
 WHERE bank_import_no = :il_bank_import_no 
   AND bank_record_type_code = '00' 
 USING SQLCA ; 

IF SQLCA.nf_handle_error('w_bank_reconcile_cheques', 'nvo_verify_bank_reconciliation', 'of_validate_table - SELECTING COUNT(*) IMPORTED_PROCESSED_CHEQUE') < 0 THEN
	li_row = ids_bank_reconcile_error_report.InsertRow(0)
	ids_bank_reconcile_error_report.SetItem(li_row, "message", "Problem selecting FROM IMPORTED_PROCESSED_CHEQUE")
	RETURN -1
END IF	

// how many header records were loaded
SELECT COUNT(*) 
  INTO :il_no_of_cheques
  FROM IMPORTED_PROCESSED_CHEQUE 
 WHERE bank_import_no = :il_bank_import_no 
 USING SQLCA ; 

IF SQLCA.nf_handle_error('w_bank_reconcile_cheques', 'nvo_verify_bank_reconciliation', 'of_validate_table - SELECTING COUNT(*) IMPORTED_PROCESSED_CHEQUE') < 0 THEN
	li_row = ids_bank_reconcile_error_report.InsertRow(0)
	ids_bank_reconcile_error_report.SetItem(li_row, "message", "Problem selecting FROM IMPORTED_PROCESSED_CHEQUE")
	RETURN -1
END IF	

SELECT COUNT(*) 
  INTO :il_no_of_trailers 
  FROM IMPORTED_PROCESSED_CHEQUE 
 WHERE bank_import_no = :il_bank_import_no 
   AND bank_record_type_code = '99' 
 USING SQLCA ; 

IF SQLCA.nf_handle_error('w_bank_reconcile_cheques', 'nvo_verify_bank_reconciliation', 'of_validate_table - SELECTING COUNT(*) IMPORTED_PROCESSED_CHEQUE') < 0 THEN 
	li_row = ids_bank_reconcile_error_report.InsertRow(0)
	ids_bank_reconcile_error_report.SetItem(li_row, "message", "Problem selecting FROM IMPORTED_PROCESSED_CHEQUE")
	RETURN -1
END IF

SELECT COUNT(*) 
  INTO :il_no_of_details 
  FROM IMPORTED_PROCESSED_CHEQUE 
 WHERE bank_import_no = :il_bank_import_no 
   AND bank_record_type_code = '30' 
 USING SQLCA ; 

IF SQLCA.nf_handle_error('w_bank_reconcile_cheques', 'nvo_verify_bank_reconciliation', 'of_validate_table - SELECTING COUNT(*) IMPORTED_PROCESSED_CHEQUE') < 0 THEN 
	li_row = ids_bank_reconcile_error_report.InsertRow(0)
	ids_bank_reconcile_error_report.SetItem(li_row, "message", "Problem selecting FROM IMPORTED_PROCESSED_CHEQUE")
	RETURN -1
END IF

SELECT bank_transit_no, account_no, filler, bank_import_no  
  INTO :is_header_bank_transit_no, :is_header_account_no, :is_header_filler, :il_header_bank_import_no 
  FROM IMPORTED_PROCESSED_CHEQUE 
 WHERE bank_import_no = :il_bank_import_no 
   AND bank_record_type_code = '00' 
 USING SQLCA ; 

IF SQLCA.nf_handle_error('w_bank_reconcile_cheques', 'nvo_verify_bank_reconciliation', 'of_validate_table - SELECTING bank_transit_no & account FROM IMPORTED_PROCESSED_CHEQUE') < 0 THEN 
	li_row = ids_bank_reconcile_error_report.InsertRow(0)
	ids_bank_reconcile_error_report.SetItem(li_row, "message", "Problem selecting FROM IMPORTED_PROCESSED_CHEQUE")
	RETURN -1
END IF

SELECT bank_transit_no,account_no, filler, bank_import_no 
  INTO :is_trailer_bank_transit_no, :is_trailer_account_no, :is_trailer_filler, :il_trailer_bank_import_no 
  FROM IMPORTED_PROCESSED_CHEQUE 
 WHERE bank_import_no = :il_bank_import_no 
   AND bank_record_type_code = '99' 
 USING SQLCA ; 
 
IF SQLCA.nf_handle_error('w_bank_reconcile_cheques', 'nvo_verify_bank_reconciliation', 'of_validate_table - SELECTING COUNT(*) IMPORTED_PROCESSED_CHEQUE') < 0 THEN 
	li_row = ids_bank_reconcile_error_report.InsertRow(0)
	ids_bank_reconcile_error_report.SetItem(li_row, "message", "Problem selecting FROM IMPORTED_PROCESSED_CHEQUE")
	RETURN -1
END IF

IF il_no_of_headers = 0 THEN
	li_row = ids_bank_reconcile_error_report.InsertRow(0)
	ids_bank_reconcile_error_report.SetItem(li_row, "message", "The file F122S14.001 must have at least one HEADER record")
	RETURN -1
ELSEIF il_no_of_headers > 1 THEN
	li_row = ids_bank_reconcile_error_report.InsertRow(0)
	ids_bank_reconcile_error_report.SetItem(li_row, "message", "The file F122S14.001 must have no more than one HEADER record")
	RETURN -1
END IF

IF il_no_of_trailers = 0 THEN
	li_row = ids_bank_reconcile_error_report.InsertRow(0)
	ids_bank_reconcile_error_report.SetItem(li_row, "message", "The file F122S14.001 must have at least one trailer record")
	RETURN -1
ELSEIF il_no_of_trailers > 1 THEN
	li_row = ids_bank_reconcile_error_report.InsertRow(0)
	ids_bank_reconcile_error_report.SetItem(li_row, "message", "The file F122S14.001 must have no more than one trailer record")
	RETURN -1
END IF

// Get the Banking info FROM the Banking_Data table
SELECT bank_transit_no, account_no
  INTO :ls_bank_transit_no, :ls_account_no
  FROM Banking_Data ;

IF SQLCA.nf_handle_error('w_bank_reconcile_cheques', 'nvo_verify_bank_reconciliation', 'of_validate_table - SELECT bank_transit_no, account_no FROM Banking_Data') < 0 THEN 
	li_row = ids_bank_reconcile_error_report.InsertRow(0)
	ids_bank_reconcile_error_report.SetItem(li_row, "message", "Problem selecting FROM IMPORTED_PROCESSED_CHEQUE")
	RETURN -1
END IF

IF is_header_bank_transit_no <> ls_bank_transit_no THEN
	li_row = ids_bank_reconcile_error_report.InsertRow(0)
	ids_bank_reconcile_error_report.SetItem(li_row, "message", "The Header Record in file F122S14.001 does not match bank transit number of '00004'")
	RETURN -1
END IF

IF is_header_account_no <> ls_account_no THEN
	li_row = ids_bank_reconcile_error_report.InsertRow(0)
	ids_bank_reconcile_error_report.SetItem(li_row, "message", "The Header Record in file F122S14.001 does not match account number of '0000026' ")
	RETURN -1
END IF

is_header_bank_create_date 	= mid(is_header_filler,15,6)
is_trailer_all_record_count 	= mid(is_trailer_filler,3,9)
is_trailer_total_amount 		= mid(is_trailer_filler,12,13)

// SINCE THE ROYAL BANK HAS NOT INCLUDED THE CENTURY IN THIER DATE'S THE 80 - 20 RULE WAS DECIDED TO BE USED convert the date
IF LONG(mid(is_header_bank_create_date,1,2)) >= 80 THEN
	is_header_bank_create_date= '19' + mid(is_header_bank_create_date,1,2) + '/' + mid(is_header_bank_create_date,3,2) + '/' +  mid(is_header_bank_create_date,5,2)
ELSE
	is_header_bank_create_date = '20' + mid(is_header_bank_create_date,1,2) + '/' + mid(is_header_bank_create_date,3,2) + '/' +  mid(is_header_bank_create_date,5,2)
END IF

// The Header record - bank creation date MUST BE a valid date 
IF isdate(is_header_bank_create_date) THEN
ELSE
	li_row = ids_bank_reconcile_error_report.InsertRow(0)
	ids_bank_reconcile_error_report.SetItem(li_row, "message", "The Header Record in file F122S14.001 does not have a valid bank create date ")
	RETURN -1
END IF

// The trailer record - all_record_count MUST BE numeric
IF LONG(is_trailer_all_record_count) = 0 THEN
	li_row = ids_bank_reconcile_error_report.InsertRow(0)
	ids_bank_reconcile_error_report.SetItem(li_row, "message", "The Trailer Record in file F122S14.001 record count is not numeric ")
	RETURN -1
END IF

// The trailer record - total amount MUST BE numeric
IF LONG(is_trailer_total_amount) = 0 THEN
	li_row = ids_bank_reconcile_error_report.InsertRow(0)
	ids_bank_reconcile_error_report.SetItem(li_row, "message", "The Trailer Record in file F122S14.001 total cheque amount is not numeric ")
	RETURN -1
END IF

IF LONG(is_trailer_all_record_count) <>  (il_no_of_headers + il_no_of_details + il_no_of_trailers) THEN
	ls_message = "P0011 - The record count: " + is_trailer_all_record_count + " in the Trailer Record in file " +&
					 "F122S14.001 does not match the no of header records (" + STRING(il_no_of_headers) +&
					 ") + detail records (" + STRING(il_no_of_details) + ") + trailer records (" +&
					 STRING(il_no_of_trailers) + ") in file F122S14.001."   
	li_row = ids_bank_reconcile_error_report.InsertRow(0)
	ids_bank_reconcile_error_report.SetItem(li_row, "message", ls_message)
	RETURN -1
END IF

RETURN 1

end function

public function integer of_populate_header ();/* At this point, process has passed column validation - ready to populate the four import tables*/

INSERT INTO PROCESSED_CHEQUE_HEADER  
	( bank_import_no,
	  bank_transit_no,
	  seq_no, 
	  account_no,
	  bank_record_type_code,
	  bank_date_created)
VALUES 
	( 	:il_header_bank_import_no,
		:is_header_bank_transit_no,
		1, 
		:is_header_account_no, 
		'00',
		convert(DATETIME,:is_header_bank_create_date))
USING SQLCA;

IF SQLCA.nf_handle_error('w_bank_reconcile_cheques', 'nvo_verify_bank_reconciliation', 'of_populate_header - INSERT PROCESSED_CHEQUE_HEADER ') < 0 THEN Return -1

return 1
end function

public function integer of_populate_trailer ();Long    ll_max_seq_no
Integer li_rtn

SELECT MAX(seq_no) 
  INTO :ll_max_seq_no 
  FROM IMPORTED_PROCESSED_CHEQUE 
 WHERE bank_import_no = :il_bank_import_no ;

li_rtn = SQLCA.nf_handle_error('w_bank_reconcile_cheques', 'nvo_verify_bankreconcilation', 'of_populate_trailer - SELECT MAX(seq_no) FROM IMPORTED_PROCESSED_CHEQUE')
IF li_rtn < 0 THEN 
	RETURN -1
END IF

INSERT INTO PROCESSED_CHEQUE_TRAILER  
		(bank_import_no, bank_transit_no, seq_no, account_no,
		bank_record_type_code, all_record_count, total_amount)
VALUES 
	  (:il_trailer_bank_import_no, :is_trailer_bank_transit_no, :ll_max_seq_no, :is_trailer_account_no,
		'99', convert(numeric,:is_trailer_all_record_count), ROUND((convert(money,:is_trailer_total_amount) / 100),2) ) 	
using SQLCA  ;
		
li_rtn = SQLCA.nf_handle_error('w_bank_reconcile_cheques', 'nvo_verify_bankreconcilation', 'of_populate_trailer - INSERT PROCESSED_CHEQUE_TRAILER') 
IF li_rtn < 0 THEN 
	RETURN -1
END IF

return 1
end function

public function integer of_populate_details (datawindow adw_dw);Long       ll_rows, ll_row
Integer    li_row, li_rtn
Decimal{2} ld_detail_tolerance_amount, ld_cheque_amount
String     ls_detail_tolerance, ls_orig_cheque_date, ls_orig_process_date
Datetime   ldt_cheque_date, ldt_bank_process_date

ll_rows = adw_dw.retrieve(il_bank_import_no)
li_rtn = SQLCA.nf_handle_error('w_bank_reconcile_cheques', 'adw_dw.retrieve(il_bank_import_no)', 'nvo_verify_bank_reconcilation - of_populate_details')

if ll_rows < 0 OR li_rtn < 0 then
	li_row = ids_bank_reconcile_error_report.insertrow(0)
	ids_bank_reconcile_error_report.setitem(li_row,"message"," Problem with retrieving the detail records")
	RETURN -1
end if

/* NEED TO LOOP THOUGH ALL THE DETAIL TO POPULATE PROCESSED_CHEQUE_DETAIL */
FOR ll_row = 1 TO ll_rows
	il_detail_bank_import_no  = adw_dw.getitemnumber(ll_row,'bank_import_no')
	is_detail_bank_transit_no = adw_dw.getitemSTRING(ll_row,'bank_transit_no')
	is_detail_account_no =  adw_dw.getitemSTRING(ll_row,'account_no')
	is_detail_bank_record_type_code = adw_dw.getitemSTRING(ll_row,'bank_record_type_code')
	is_detail_filler = adw_dw.getitemSTRING(ll_row,'filler')
	il_seq_no = 	adw_dw.getitemnumber(ll_row,'seq_no')

	/* NEED TO PARSE THE COLUMNS FROM THE filler data column */
	is_detail_cheque_no 				= mid(is_detail_filler,3,8)
	is_detail_cheque_amount 		= mid(is_detail_filler,11,11)
	is_detail_cheque_date 			= mid(is_detail_filler,24,6)
	is_detail_bank_process_date 	= mid(is_detail_filler,30,6)
	is_detail_bank_txn_code 		= mid(is_detail_filler,36,3)
	is_detail_reconciled_code 		= mid(is_detail_filler,58,2)
	is_detail_tolerance_amount 	= mid(is_detail_filler,60,3)
	is_detail_tolerance_ind       = mid(is_detail_filler,63,1)
	ls_detail_tolerance = mid(is_detail_filler,60,4)

	// Check for Blank Line in File
	if is_detail_bank_record_type_code <> '30' then
		li_row = ids_bank_reconcile_error_report.insertrow(0)
		ids_bank_reconcile_error_report.setitem(li_row,"message","  The detail Record in file F122S14.001 has a bank record type code <> '30' ") 
		return -1
	end if
	
	/* not an error condition */	
	if isnumber(is_detail_cheque_no) then
	else
		is_detail_cheque_no = '0' 
	end if
	
	// Check for Cheque number = 00000000
	if is_detail_cheque_no = '00000000' then
		il_no_of_unknown++
		il_no_of_records_written_to_suspense++
		
		INSERT INTO PROCESSED_CHEQUE_DETAIL_SUSPEN  
         ( bank_import_no, bank_transit_no, account_no, seq_no, bank_record_type_code,   
           cheque_no, cheque_amount, cheque_date, bank_txn_type_code, bank_process_date,  
           reconciled_code, tolerance_amount)  
  		VALUES 
		  (  :il_detail_bank_import_no, :is_detail_bank_transit_no, :is_detail_account_no, :il_seq_no, '30',
           :is_detail_cheque_no, :is_detail_cheque_amount, :is_detail_cheque_date, :is_detail_bank_txn_code, :is_detail_bank_process_date,    
           :is_detail_reconciled_code, :ls_detail_tolerance)  
		using SQLCA;

		li_rtn = SQLCA.nf_handle_error('w_bank_reconcile_cheques', 'nvo_verify_bank_reconcilation', 'of_populate_details - INSERT INTO PROCESSED_CHEQUE_DETAIL_SUSPEN')
		
		continue
	end if
	
	// Validate Cheque Amount
	if isnumber(is_detail_cheque_amount ) then
		ld_cheque_amount = Dec(is_detail_cheque_amount) / 100
	else
		il_no_of_records_written_to_suspense++
		INSERT INTO PROCESSED_CHEQUE_DETAIL_SUSPEN  
         ( bank_import_no, bank_transit_no, account_no, seq_no, bank_record_type_code,   
           cheque_no, cheque_amount, cheque_date, bank_txn_type_code, bank_process_date,  
           reconciled_code, tolerance_amount)  
  		VALUES 
		   ( :il_detail_bank_import_no, :is_detail_bank_transit_no, :is_detail_account_no, :il_seq_no, '30',
           :is_detail_cheque_no, :is_detail_cheque_amount, :is_detail_cheque_date, :is_detail_bank_txn_code, :is_detail_bank_process_date,    
           :is_detail_reconciled_code, :ls_detail_tolerance)  
		using SQLCA;
		
		li_rtn = SQLCA.nf_handle_error('w_bank_reconcile_cheques', 'nvo_verify_bank_reconcilation', 'of_populate_details - INSERT INTO PROCESSED_CHEQUE_DETAIL_SUSPEN')
		
		continue
	end if
	
	// Validate Cheque Date
	ls_orig_cheque_date = is_detail_cheque_date
	if LONG(mid(is_detail_cheque_date ,1,2)) > 80 then
		is_detail_cheque_date = '19' + mid(is_detail_cheque_date ,1,2) + '/' + mid(is_detail_cheque_date ,3,2) + '/' +  mid(is_detail_cheque_date ,5,2) 
	else
		is_detail_cheque_date  = '20' + mid(is_detail_cheque_date ,1,2) + '/' + mid(is_detail_cheque_date ,3,2) + '/' +  mid(is_detail_cheque_date,5,2) 
	end if
	
	if isdate(is_detail_cheque_date) then
		ldt_cheque_date = DateTime(Date(is_detail_cheque_date), Time("00:00:00"))
	else
		il_no_of_records_written_to_suspense++
		INSERT INTO PROCESSED_CHEQUE_DETAIL_SUSPEN  
         ( bank_import_no, bank_transit_no, account_no, seq_no, bank_record_type_code,   
           cheque_no, cheque_amount, cheque_date, bank_txn_type_code, bank_process_date,  
           reconciled_code, tolerance_amount)  
  		VALUES 
		  (  :il_detail_bank_import_no, :is_detail_bank_transit_no, :is_detail_account_no, :il_seq_no, '30',
           :is_detail_cheque_no, :is_detail_cheque_amount, :ls_orig_cheque_date, :is_detail_bank_txn_code, :is_detail_bank_process_date,    
           :is_detail_reconciled_code, :ls_detail_tolerance)  
		using SQLCA;

		li_rtn = SQLCA.nf_handle_error('w_bank_reconcile_cheques', 'nvo_verify_bank_reconcilation', 'of_populate_details - INSERT INTO PROCESSED_CHEQUE_DETAIL_SUSPEN')

		continue
	end if

	// Validate Process Date
	ls_orig_process_date = is_detail_bank_process_date
	if LONG(mid(is_detail_bank_process_date ,1,2)) > 50 then
		is_detail_bank_process_date   = '19' + mid(is_detail_bank_process_date ,1,2) + '/' + mid(is_detail_bank_process_date  ,3,2) + '/' +  mid(is_detail_bank_process_date  ,5,2) 
	else
		is_detail_bank_process_date   = '20' + mid(is_detail_bank_process_date  ,1,2) + '/' + mid(is_detail_bank_process_date  ,3,2) + '/' +  mid(is_detail_bank_process_date  ,5,2) 
	end if

	/* this would be error situation */
	if isdate(is_detail_bank_process_date) then
		ldt_bank_process_date = DateTime(Date(is_detail_bank_process_date), Time("00:00:00"))
	else
		il_no_of_records_written_to_suspense++
		INSERT INTO PROCESSED_CHEQUE_DETAIL_SUSPEN  
         ( bank_import_no, bank_transit_no, account_no, seq_no, bank_record_type_code,   
           cheque_no, cheque_amount, cheque_date, bank_txn_type_code, bank_process_date,
           reconciled_code, tolerance_amount)  
  		VALUES 
		  (  :il_detail_bank_import_no, :is_detail_bank_transit_no, :is_detail_account_no, :il_seq_no, '30',
           :is_detail_cheque_no, :is_detail_cheque_amount, :ls_orig_cheque_date, :is_detail_bank_txn_code, :ls_orig_process_date,    
           :is_detail_reconciled_code, :ls_detail_tolerance)  
		using SQLCA;
		continue
	end if

	is_trans_code = mid(is_filler,36,3)								
	if not isnumber(is_trans_code) then is_trans_code = '0'
	if not isnumber(is_detail_bank_txn_code) then is_detail_bank_txn_code = '0'
	
	// Validate Tolerance Amount
	if isnumber(is_detail_tolerance_amount) then
		if isnull(is_detail_tolerance_ind) then
			is_detail_tolerance_ind = ''
		end if
		ld_detail_tolerance_amount = Dec(is_detail_tolerance_amount) / 100
		IF is_detail_tolerance_ind = "-" THEN
			ld_detail_tolerance_amount = ld_detail_tolerance_amount * -1
		END IF
	else
		/* THIS WOULD GO TO THE SUSPENSE */
		il_no_of_records_written_to_suspense++
		INSERT INTO PROCESSED_CHEQUE_DETAIL_SUSPEN  
         ( bank_import_no, bank_transit_no, account_no, seq_no, bank_record_type_code,   
           cheque_no, cheque_amount, cheque_date, bank_process_date, bank_txn_type_code,   
           reconciled_code, tolerance_amount)  
  		VALUES 
		  (  :il_detail_bank_import_no, :is_detail_bank_transit_no, :is_detail_account_no, :il_seq_no, '30',
           :is_detail_cheque_no, :is_detail_cheque_amount, :ls_orig_cheque_date, :is_detail_bank_txn_code, :is_detail_bank_process_date,    
           :is_detail_reconciled_code, :ls_detail_tolerance)  
		using SQLCA;
		continue
	end if

  INSERT INTO PROCESSED_CHEQUE_DETAIL  
  		(bank_import_no, bank_transit_no, seq_no, account_no, bank_record_type_code,
		 cheque_no, cheque_amount, cheque_date, bank_process_date, bank_txn_type_code,
		 reconciled_code, tolerance_amount)  
	VALUES 	
		(:il_bank_import_no, :is_transit_no, :il_seq_no, :is_account_no, :is_detail_bank_record_type_code,
		 :is_detail_cheque_no, :ld_cheque_amount, :ldt_cheque_date, :ldt_bank_process_date, :is_detail_bank_txn_code,
		 :is_detail_reconciled_code, :ld_detail_tolerance_amount)
	using sqlca  ;
	IF SQLCA.nf_handle_error('creating PROCESSED_CHEQUE_DETAIL', 'w_bank_reconcile_cheques', 'cb_receiving_cheque_data') < 0 THEN Return -1
NEXT

return 1
end function

public function integer of_produce_controls ();// of_produce_controls
//
Integer li_rtn 

SELECT COUNT(*)  
  INTO :il_no_of_records_written_to_header  
  FROM PROCESSED_CHEQUE_HEADER  
 WHERE bank_import_no = :il_bank_import_no  
 USING SQLCA ; 

li_rtn = SQLCA.nf_handle_error('nvo_verify_bank_reconcilation', '', 'of_produce_controls - SELECT COUNT(*) FROM PROCESSED_CHEQUE_HEADER')
IF li_rtn < 0 THEN 
	RETURN -1
END IF
	
SELECT COUNT(*)  
  INTO :il_no_of_records_written_to_details
  FROM PROCESSED_CHEQUE_DETAIL 
 WHERE bank_import_no = :il_bank_import_no 
 USING SQLCA ; 

li_rtn = SQLCA.nf_handle_error('nvo_verify_bank_reconcilation', '', 'of_produce_controls - SELECT COUNT(*) FROM PROCESSED_CHEQUE_DETAIL')
IF li_rtn < 0 THEN 
	RETURN -1
END IF
	 
SELECT COUNT(*)  
  INTO :il_no_of_records_written_to_trailer 
  FROM PROCESSED_CHEQUE_TRAILER 
 WHERE bank_import_no = :il_bank_import_no 
 USING SQLCA ; 

li_rtn = SQLCA.nf_handle_error('nvo_verify_bank_reconcilation', '', 'of_produce_controls - SELECT COUNT(*) FROM PROCESSED_CHEQUE_TRAILER')
IF li_rtn < 0 THEN 
	RETURN -1
END IF

SELECT SUM(cheque_amount)  
  INTO :idcm_sum_cheque_amount  
  FROM PROCESSED_CHEQUE_DETAIL 
 WHERE bank_import_no = :il_bank_import_no 
 USING SQLCA ; 

li_rtn = SQLCA.nf_handle_error('nvo_verify_bank_reconcilation', '', 'of_produce_controls - SELECT SUM(cheque_amount) FROM PROCESSED_CHEQUE_DETAIL')
IF li_rtn < 0 THEN 
	RETURN -1
END IF

SELECT all_record_count, total_amount 
  INTO :il_all_record_count, :idcm_total_cheque_amount 
  FROM PROCESSED_CHEQUE_TRAILER 
 WHERE bank_import_no = :il_bank_import_no 
 USING SQLCA ; 

li_rtn = SQLCA.nf_handle_error('nvo_verify_bank_reconcilation', '', 'of_produce_controls - SELECT all_record_count, total_amount FROM PROCESSED_CHEQUE_TRAILER')
IF li_rtn < 0 THEN 
	RETURN -1
END IF

RETURN 1


end function

public subroutine of_get_processing_information (ref string as_payrec_file, ref string as_backup_file, ref string as_bank_import_no, ref string as_no_of_cheques);

as_payrec_file = is_payrec_file
as_backup_file = is_backup_file
as_bank_import_no = string(il_bank_import_no)
as_no_of_cheques = string(il_no_of_cheques)
end subroutine

public function long of_select_bank_import_no ();// of_select_bank_import_no
// 
RETURN il_bank_import_no

end function

public subroutine of_produce_control_report ();Long ll_row 

// Print the Control Report
ll_row = ids_bank_reconcile_control_report.InsertRow(0)

ids_bank_reconcile_control_report.SetItem(ll_row, "no_of_cheques_with_invalid_tolerance", il_no_of_cheques_with_invalid_tolerance)
ids_bank_reconcile_control_report.SetItem(ll_row, "no_of_headers_processed", il_no_of_headers)
ids_bank_reconcile_control_report.SetItem(ll_row, "no_of_details_processed", il_no_of_details)
ids_bank_reconcile_control_report.SetItem(ll_row, "no_of_trailers_processed", il_no_of_trailers)
ids_bank_reconcile_control_report.SetItem(ll_row, "no_of_unknown_records_processed", il_no_of_unknown)

ids_bank_reconcile_control_report.SetItem(ll_row, "total_no_of_records_processed", il_no_of_headers + il_no_of_details + il_no_of_trailers + il_no_of_unknown)
ids_bank_reconcile_control_report.SetItem(ll_row, "no_of_records_written_to_header", il_no_of_records_written_to_header)
ids_bank_reconcile_control_report.SetItem(ll_row, "no_of_records_written_to_details", il_no_of_records_written_to_details)
ids_bank_reconcile_control_report.SetItem(ll_row, "no_of_records_written_to_trailer", il_no_of_records_written_to_trailer)
ids_bank_reconcile_control_report.SetItem(ll_row, "no_of_records_written_to_suspense", il_no_of_records_written_to_suspense)

ids_bank_reconcile_control_report.SetItem(ll_row, "all_record_count_from_trailer", il_all_record_count - 2)
ids_bank_reconcile_control_report.SetItem(ll_row, "total_amount_from_trailer", idcm_total_cheque_amount)
ids_bank_reconcile_control_report.SetItem(ll_row, "total_cheque_amounts_from_details", idcm_sum_cheque_amount)
ids_bank_reconcile_control_report.SetItem(ll_row, "no_cheques_reconciled", il_no_cheques_reconciled)
ids_bank_reconcile_control_report.SetItem(ll_row, "amount_of_cheques_reconciled", idcm_amount_of_cheques_reconciled)

ids_bank_reconcile_control_report.SetItem(ll_row, "no_of_cheques_never_issued", il_no_of_cheques_never_issued)
ids_bank_reconcile_control_report.SetItem(ll_row, "no_of_cheques_never_transmitted", il_no_of_cheques_never_transmitted)
ids_bank_reconcile_control_report.SetItem(ll_row, "no_of_cheques_already_reconciled", il_no_of_cheques_already_reconciled)
ids_bank_reconcile_control_report.SetItem(ll_row, "no_of_cheques_issue_date_no_match", il_no_of_cheques_issue_date_no_match)
ids_bank_reconcile_control_report.SetItem(ll_row, "no_of_cheques_with_invalid_statuses", il_no_of_cheques_with_invalid_statuses)

ids_bank_reconcile_control_report.SetItem(ll_row, "no_of_cheques_431_00", il_no_of_cheques_431_00)
ids_bank_reconcile_control_report.SetItem(ll_row, "no_of_cheques_431_09", il_no_of_cheques_431_09)
ids_bank_reconcile_control_report.SetItem(ll_row, "no_of_cheques_180_00", il_no_of_cheques_180_00)
ids_bank_reconcile_control_report.SetItem(ll_row, "no_of_cheques_180_09", il_no_of_cheques_180_09)
ids_bank_reconcile_control_report.SetItem(ll_row, "no_of_cheques_157_07", il_no_of_cheques_157_07)
ids_bank_reconcile_control_report.SetItem(ll_row, "no_of_cheques_432_16", il_no_of_cheques_432_16)

ids_bank_reconcile_control_report.SetItem(ll_row, "amount_of_cheques_157_07", idcm_amount_of_cheques_157_07)
ids_bank_reconcile_control_report.SetItem(ll_row, "amount_of_cheques_180_09", idcm_amount_of_cheques_180_09)
ids_bank_reconcile_control_report.SetItem(ll_row, "amount_of_cheques_180_00", idcm_amount_of_cheques_180_00)
ids_bank_reconcile_control_report.SetItem(ll_row, "amount_of_cheques_431_09", idcm_amount_of_cheques_431_09)
ids_bank_reconcile_control_report.SetItem(ll_row, "amount_of_cheques_431_00", idcm_amount_of_cheques_431_00)
ids_bank_reconcile_control_report.SetItem(ll_row, "amount_of_cheques_432_16", idcm_amount_of_cheques_432_16) 

ids_bank_reconcile_control_report.SetItem(ll_row, "amount_of_cheques_not_reconciled", idcm_amount_of_cheques_not_reconciled) 

end subroutine

public function integer of_remove_imported_file ();// of_remove_imported_file 
//
Boolean lb_rtn

lb_rtn = FileExists(is_backup_file)
IF lb_rtn = TRUE THEN
	// delete imported file that has been backed up: F122S14.001 from cmwb.ini's [BANK RECONCILIATION] PayVerifyDir
	lb_rtn = FileDelete(is_payrec_file) 
	IF lb_rtn = FALSE THEN 
		MessageBox('File Deletion Problem', 'Could not delete the imported file. Contact HELPDESK.')
		RETURN -1
	END IF
END IF

RETURN 1

end function

public function integer of_perform_match (datawindow adw_dw);// of_perform_match
// 
Long	     ll_rows, ll_row, ll_error_row, ll_detail_cheque_no 
Integer    li_rtn
String     ls_detail_bank_txn_type_code, ls_detail_reconciled_code, ls_reconciled_amount_flag
String     ls_message, ls_bank_txn_type_desc, ls_set_reconciled_amount_flag, ls_reconciled_desc
Decimal{2} ldcm_detail_tolerance_amount, ldcm_detail_cheque_amount, ldcm_whscc_cheque_amount
Datetime   ldtm_cheque_header_cheque_date, ldtm_detail_cheque_date, ldtm_server_datetime
Datetime   ldtm_detail_bank_process_date, ldtm_cheque_header_transmit_date  

ldtm_server_datetime = f_server_datetime()

ll_rows = adw_dw.Retrieve(il_bank_import_no)
li_rtn = SQLCA.nf_handle_error('nvo_verify_bank_reconciliation', '', 'of_perform_match - adw_dw.Retrieve(il_bank_import_no)')
IF li_rtn < 0 THEN
	RETURN -1
END IF

FOR ll_row = 1 TO ll_rows
	ls_detail_bank_txn_type_code = adw_dw.GetItemString(ll_row, 'bank_txn_type_code')
	ls_detail_reconciled_code = adw_dw.GetItemString(ll_row, 'reconciled_code')
	ll_detail_cheque_no = Long(adw_dw.GetItemString(ll_row, 'cheque_no'))
	ldtm_detail_cheque_date = adw_dw.GetItemDatetime(ll_row, 'cheque_date')
	ldtm_detail_bank_process_date = adw_dw.GetItemDatetime(ll_row, 'bank_process_date')
	ldcm_detail_tolerance_amount = adw_dw.GetItemNumber(ll_row, 'tolerance_amount')
	ldcm_detail_cheque_amount = adw_dw.GetItemNumber(ll_row, 'cheque_amount')

	// Validate Txn Type
	SELECT bank_txn_type_desc 
	  INTO :ls_bank_txn_type_desc
	  FROM Bank_Txn_Type 
	 WHERE bank_txn_type_code = :ls_detail_bank_txn_type_code ;

	li_rtn = SQLCA.nf_handle_error('nvo_verify_bank_reconciliation', '', 'of_perform_match - SELECT bank_txn_type_desc FROM Bank_Txn_Type')
	IF li_rtn = -1 THEN
		RETURN -1
	ELSEIF li_rtn = 100 THEN
		il_no_of_cheques_with_invalid_statuses = il_no_of_cheques_with_invalid_statuses + 1
		ll_error_row = ids_bank_reconcile_error_report.insertrow(0)
		ls_message = "Invalid bank txn type code: " + ls_detail_bank_txn_type_code + " for cheque no : " + String(ll_detail_cheque_no)
		ids_bank_reconcile_error_report.SetItem(ll_error_row, 'message', ls_message)

		UPDATE CHEQUE_HEADER  
		   SET reconciled_date = :ldtm_detail_bank_process_date,
             disbursement_auditor_date = :ldtm_server_datetime,
             reconciled_user_id = :vgst_user_profile.user_id,
             reconciled_code = :ls_detail_reconciled_code,
             reconciled_amount = 0,
             reconciled_amount_flag = 'N',
             bank_amount = :ldcm_detail_cheque_amount 
		 WHERE cheque_no = :ll_detail_cheque_no 
		 USING SQLCA ; 

		li_rtn = SQLCA.nf_handle_error('nvo_verify_bank_reconciliation', '', 'of_perform_match - UPDATE CHEQUE HEADER')
		IF li_rtn < 0 THEN 
			RETURN -1 
		END IF

		CONTINUE
	END IF

	// Validate Txn Type 157 (void cheque)
	IF ls_detail_bank_txn_type_code = '157' THEN
		IF ls_detail_reconciled_code <> '07' THEN     // If reconciled code is Not 07 (void cheque record) 
			// this would be an error situation
			il_no_of_cheques_with_invalid_statuses = il_no_of_cheques_with_invalid_statuses + 1
			ll_error_row = ids_bank_reconcile_error_report.InsertRow(0) 
			ls_message = "Invalid cheque match status: " + ls_detail_reconciled_code + " for bank type code: " + ls_detail_bank_txn_type_code + " - " + ls_bank_txn_type_desc + " for cheque no: " + string(ll_detail_cheque_no)
			ids_bank_reconcile_error_report.SetItem(ll_error_row,'message', ls_message)

         UPDATE CHEQUE_HEADER  
            SET reconciled_date = :ldtm_detail_bank_process_date,
                disbursement_auditor_date = :ldtm_server_datetime,
                reconciled_user_id = :vgst_user_profile.user_id,
                reconciled_code = :ls_detail_reconciled_code,
                reconciled_amount = 0,
                reconciled_amount_flag = 'N',
                bank_amount = :ldcm_detail_cheque_amount
          WHERE cheque_no = :ll_detail_cheque_no
          USING SQLCA ; 

			li_rtn = SQLCA.nf_handle_error('nvo_verify_bank_reconciliation', '', 'of_perform_match - UPDATE CHEQUE HEADER')
			IF li_rtn < 0 THEN 
				RETURN -1 
			END IF
			
			CONTINUE
		END IF
	END IF

	// Validate Txn Types 180 (adjustment debit) and 431 (disbursement audit debit)
	IF ls_detail_bank_txn_type_code = '180' OR ls_detail_bank_txn_type_code= '431' THEN
		IF ls_detail_reconciled_code = '00' OR ls_detail_reconciled_code = '09' THEN       // If reconciled code is 00 (match cheque) or 09 (matched within $1.00) 
		ELSE
			// This would be error situation
			il_no_of_cheques_with_invalid_statuses = il_no_of_cheques_with_invalid_statuses + 1 	
			ll_error_row = ids_bank_reconcile_error_report.InsertRow(0)
			ls_message = "Invalid cheque match status: " + ls_detail_reconciled_code + " for bank type code: " + ls_detail_bank_txn_type_code + " - " + ls_bank_txn_type_desc + " for cheque no: " + string(ll_detail_cheque_no)
			ids_bank_reconcile_error_report.SetItem(ll_error_row,'message', ls_message)

         UPDATE CHEQUE_HEADER 
            SET reconciled_date = :ldtm_detail_bank_process_date,
                disbursement_auditor_date = :ldtm_server_datetime,
                reconciled_user_id = :vgst_user_profile.user_id,
                reconciled_code = :ls_detail_reconciled_code,
                reconciled_amount = 0,
                reconciled_amount_flag = 'N',
                bank_amount = :ldcm_detail_cheque_amount
          WHERE cheque_no = :ll_detail_cheque_no
          USING SQLCA ; 

			li_rtn = SQLCA.nf_handle_error('nvo_verify_bank_reconciliation', '', 'of_perform_match - UPDATE CHEQUE HEADER')
			IF li_rtn < 0 THEN 
				RETURN -1 
			END IF

			CONTINUE
		END IF
	END IF

	IF ls_detail_reconciled_code = "00" OR ls_detail_reconciled_code = "09" OR ls_detail_reconciled_code = "16" THEN   // If reconciled code is 00 (match cheque) OR 09 (matched within $1.00) OR 16 (exception cheque match) 
		ldcm_detail_cheque_amount = ldcm_detail_cheque_amount
	ELSEIF ls_detail_reconciled_code = "07" THEN     // If reconciled code is 07 (void cheque record) 
		ldcm_detail_cheque_amount = 0
	ELSEIF ls_detail_reconciled_code = "14" THEN     // If reconciled code is 14 (exception cheque dropped (28 days)) 
		// Validate Cheque Number in File
		SELECT COUNT(*) 
		  INTO :il_count_cheque_header 
   	  FROM CHEQUE_HEADER  
	    WHERE cheque_no = :ll_detail_cheque_no  
	    USING SQLCA ; 

		li_rtn = SQLCA.nf_handle_error('nvo_verify_bank_reconciliation', '', 'of_perform_match - SELECT COUNT(*) FROM CHEQUE_HEADER')
		IF li_rtn < 0 THEN 
			RETURN -1 
		END IF

		IF isnull(il_count_cheque_header ) THEN il_count_cheque_header = 0

		IF il_count_cheque_header <> 0 THEN
			// SR 94 - even though marked as an exception cheque by the bank it was still cashed, it was still among our un-reconciled cheques, and therefore was 'reconcileable' 
		ELSE
			ll_error_row = ids_bank_reconcile_error_report.InsertRow(0)

			SELECT reconciled_desc 
			  INTO :ls_reconciled_desc 
			  FROM Reconciled 
			 WHERE reconciled_code = :ls_detail_reconciled_code 
			 USING SQLCA ; 

			li_rtn = SQLCA.nf_handle_error('nvo_verify_bank_reconciliation', '', 'of_perform_match - SELECT reconciled_desc FROM Reconciled')
			IF li_rtn < 0 THEN 
				RETURN -1 
			END IF

			IF IsNull(ls_reconciled_desc) OR ls_reconciled_desc = '' THEN
				ls_message = "Invalid Reconciliation code: " + ls_detail_reconciled_code + " for cheque no: " + string(ll_detail_cheque_no )  
			ELSE
				ls_message = "Invalid Reconciliation code: " + ls_detail_reconciled_code + ' ('+ls_reconciled_desc+')' + " for cheque no: " + string(ll_detail_cheque_no )  
			END IF

			ids_bank_reconcile_error_report.SetItem(ll_error_row,'message', ls_message)		
			il_no_of_cheques_with_invalid_statuses = il_no_of_cheques_with_invalid_statuses + 1

         UPDATE CHEQUE_HEADER 
            SET reconciled_date = :ldtm_detail_bank_process_date, 
                disbursement_auditor_date	= :ldtm_server_datetime,
                reconciled_user_id = :vgst_user_profile.user_id, 
                reconciled_code = :ls_detail_reconciled_code,
                reconciled_amount = 0,
                reconciled_amount_flag = 'N',
                bank_amount = :ldcm_detail_cheque_amount
          WHERE cheque_no = :ll_detail_cheque_no 
          USING SQLCA ; 

			li_rtn = SQLCA.nf_handle_error('nvo_verify_bank_reconciliation', '', 'of_perform_match - UPDATE CHEQUE HEADER')
			IF li_rtn < 0 THEN 
				RETURN -1 
			END IF

			CONTINUE
		END IF
	ELSE // generally this is 11 - unreconcileable
		ll_error_row = ids_bank_reconcile_error_report.InsertRow(0)

		SELECT reconciled_desc
		  INTO :ls_reconciled_desc
		  FROM Reconciled
		 WHERE reconciled_code = :ls_detail_reconciled_code
		 USING SQLCA ; 

		li_rtn = SQLCA.nf_handle_error('nvo_verify_bank_reconciliation', '', 'of_perform_match - SELECT reconciled_desc FROM Reconciled')
		IF li_rtn < 0 THEN 
			RETURN -1 
		END IF

		IF IsNull(ls_reconciled_desc) OR ls_reconciled_desc = '' THEN
			ls_message = "Invalid Reconciliation code: " + ls_detail_reconciled_code + " for cheque no: " + string(ll_detail_cheque_no )  
		ELSE
			ls_message = "Invalid Reconciliation code: " + ls_detail_reconciled_code + ' ('+ls_reconciled_desc+')' + " for cheque no: " + string(ll_detail_cheque_no )  
		END IF

		ids_bank_reconcile_error_report.SetItem(ll_error_row, 'message', ls_message)		
		il_no_of_cheques_with_invalid_statuses = il_no_of_cheques_with_invalid_statuses + 1

      UPDATE CHEQUE_HEADER  
         SET reconciled_date = :ldtm_detail_bank_process_date, 
             disbursement_auditor_date = :ldtm_server_datetime,
             reconciled_user_id = :vgst_user_profile.user_id, 
             reconciled_code = :ls_detail_reconciled_code, 
             reconciled_amount = 0, 
             reconciled_amount_flag = 'N', 
             bank_amount = :ldcm_detail_cheque_amount 
       WHERE cheque_no = :ll_detail_cheque_no 
       USING SQLCA ; 

		li_rtn = SQLCA.nf_handle_error('nvo_verify_bank_reconciliation', '', 'of_perform_match - UPDATE CHEQUE HEADER')
		IF li_rtn < 0 THEN 
			RETURN -1 
		END IF

		CONTINUE
	END IF

	// Validate Cheque Number in File
	SELECT count(*) 
	  INTO :il_count_cheque_header 
     FROM CHEQUE_HEADER  
    WHERE cheque_no = :ll_detail_cheque_no  
	 USING SQLCA ; 

	li_rtn = SQLCA.nf_handle_error('nvo_verify_bank_reconciliation', '', 'of_perform_match - SELECT COUNT(*) FROM CHEQUE_HEADER')
	IF li_rtn < 0 THEN 
		RETURN -1 
	END IF

	IF IsNull(il_count_cheque_header ) THEN il_count_cheque_header = 0

	IF il_count_cheque_header = 0 THEN
		il_no_of_cheques_never_issued = il_no_of_cheques_never_issued + 1
		ll_error_row = ids_bank_reconcile_error_report.InsertRow(0)
		ids_bank_reconcile_error_report.SetItem(ll_error_row,'message',"No matching WorkSafeNB Cheque no found : " + string(ll_detail_cheque_no ) )	
		CONTINUE
	ELSE
		SELECT cheque_amount, cheque_date, transmit_date, reconciled_amount_flag 
        INTO :ldcm_whscc_cheque_amount, :ldtm_cheque_header_cheque_date, :ldtm_cheque_header_transmit_date, :ls_reconciled_amount_flag 
        FROM CHEQUE_HEADER 
       WHERE cheque_no = :ll_detail_cheque_no 
       USING SQLCA ; 

		li_rtn = SQLCA.nf_handle_error('nvo_verify_bank_reconciliation', '', 'of_perform_match - SELECT cheque_amount, cheque_date, transmit_date, reconciled_amount_flag FROM CHEQUE_HEADER')
		IF li_rtn < 0 THEN 
			RETURN -1 
		END IF

		// Validate the Tolerance Amount
		IF ls_detail_reconciled_code <> "07" AND ldcm_whscc_cheque_amount <> (ldcm_detail_cheque_amount + ldcm_detail_tolerance_amount) THEN
			ls_message = "Tolerance Amount, " + String(ldcm_detail_tolerance_amount, "$#,##0.00") + " is invalid for " +&
							 "Cheque Number " + String(ll_detail_cheque_no) + ".  The WorkSafeNB Issued Amount: " +&
							 String(ldcm_whscc_cheque_amount, "$#,##0.00") + " should equal the Bank's Amount " +&
							 String(ldcm_detail_cheque_amount, "$#,##0.00") + " + the tolerance amount: " +&
							 String(ldcm_detail_tolerance_amount, "$#,##0.00") + "."
			ll_error_row = ids_bank_reconcile_error_report.InsertRow(0)
			ids_bank_reconcile_error_report.SetItem(ll_error_row,'message', ls_message)	
			il_no_of_cheques_with_invalid_tolerance = il_no_of_cheques_with_invalid_tolerance + 1

         UPDATE CHEQUE_HEADER 
            SET reconciled_date = :ldtm_detail_bank_process_date, 
                disbursement_auditor_date = :ldtm_server_datetime,
                reconciled_user_id = :vgst_user_profile.user_id, 
                reconciled_code = :ls_detail_reconciled_code, 
                reconciled_amount = 0, 
                reconciled_amount_flag = 'N', 
                bank_amount = :ldcm_detail_cheque_amount 
          WHERE cheque_no = :ll_detail_cheque_no 
          USING SQLCA ; 

			li_rtn = SQLCA.nf_handle_error('nvo_verify_bank_reconciliation', '', 'of_perform_match - UPDATE CHEQUE HEADER')
			IF li_rtn < 0 THEN 
				RETURN -1 
			END IF

			CONTINUE
		END IF

		// Validate Transmit Date
		IF ISNULL(ldtm_cheque_header_transmit_date)  THEN
			ll_error_row = ids_bank_reconcile_error_report.InsertRow(0)
			ids_bank_reconcile_error_report.SetItem(ll_error_row,'message',"Invalid Transmitted Date for cheque no : " + string(ll_detail_cheque_no ) )	
			il_no_of_cheques_never_transmitted = il_no_of_cheques_never_transmitted + 1

         UPDATE CHEQUE_HEADER 
            SET reconciled_date = :ldtm_detail_bank_process_date, 
                disbursement_auditor_date = :ldtm_server_datetime, 
                reconciled_user_id = :vgst_user_profile.user_id, 
                reconciled_code = :ls_detail_reconciled_code, 
                reconciled_amount = 0, 
                reconciled_amount_flag = 'N', 
                bank_amount = :ldcm_detail_cheque_amount 
          WHERE cheque_no = :ll_detail_cheque_no 
          USING SQLCA ; 

			li_rtn = SQLCA.nf_handle_error('nvo_verify_bank_reconciliation', '', 'of_perform_match - UPDATE CHEQUE HEADER')
			IF li_rtn < 0 THEN 
				RETURN -1 
			END IF

			CONTINUE
		END IF

		// See if it's already been reconciled
		IF ls_reconciled_amount_flag = "Y" THEN
			il_no_of_cheques_already_reconciled = il_no_of_cheques_already_reconciled + 1
			ll_error_row = ids_bank_reconcile_error_report.InsertRow(0)
			ids_bank_reconcile_error_report.SetItem(ll_error_row,'message',"Already Reconciled for cheque no : " + string(ll_detail_cheque_no ) )

         UPDATE CHEQUE_HEADER 
            SET reconciled_date = :ldtm_detail_bank_process_date, 
                disbursement_auditor_date = :ldtm_server_datetime, 
                reconciled_user_id = :vgst_user_profile.user_id, 
                reconciled_code = :ls_detail_reconciled_code, 
                reconciled_amount = 0, 
                reconciled_amount_flag = 'N', 
                bank_amount = :ldcm_detail_cheque_amount 
          WHERE cheque_no = :ll_detail_cheque_no 
          USING SQLCA ; 

			li_rtn = SQLCA.nf_handle_error('nvo_verify_bank_reconciliation', '', 'of_perform_match - UPDATE CHEQUE HEADER')
			IF li_rtn < 0 THEN 
				RETURN -1 
			END IF

			CONTINUE
		END IF

		// Validate cheque dates between file and CHEQUE_HEADER table
		IF ldtm_cheque_header_cheque_date = ldtm_detail_cheque_date THEN
		ELSE
			il_no_of_cheques_issue_date_no_match = il_no_of_cheques_issue_date_no_match + 1 
			ll_error_row = ids_bank_reconcile_error_report.InsertRow(0)
			ids_bank_reconcile_error_report.SetItem(ll_error_row,'message',"Issues Dates do not match for cheque no : " + string(ll_detail_cheque_no ) )

         UPDATE CHEQUE_HEADER 
            SET reconciled_date = :ldtm_detail_bank_process_date, 
                disbursement_auditor_date = :ldtm_server_datetime, 
                reconciled_user_id = :vgst_user_profile.user_id,
                reconciled_code = :ls_detail_reconciled_code, 
                reconciled_amount = 0, 
                reconciled_amount_flag = 'N', 
                bank_amount = :ldcm_detail_cheque_amount 
          WHERE cheque_no = :ll_detail_cheque_no 
          USING SQLCA ; 

			li_rtn = SQLCA.nf_handle_error('nvo_verify_bank_reconciliation', '', 'of_perform_match - UPDATE CHEQUE HEADER')
			IF li_rtn < 0 THEN 
				RETURN -1 
			END IF

			CONTINUE
		END IF
	END IF

	// SR 94 - At this point all cheques are considered 'reconciled' - i.e. cashed by the bank.
	ls_set_reconciled_amount_flag = 'Y'

   UPDATE CHEQUE_HEADER 
      SET reconciled_date = :ldtm_detail_bank_process_date, 
          disbursement_auditor_date = :ldtm_server_datetime,
          reconciled_user_id = :vgst_user_profile.user_id, 
          reconciled_code = :ls_detail_reconciled_code, 
          reconciled_amount = :ldcm_detail_cheque_amount, 
          reconciled_amount_flag = :ls_set_reconciled_amount_flag, 
          bank_amount = :ldcm_detail_cheque_amount 
    WHERE cheque_no = :ll_detail_cheque_no 
    USING SQLCA ; 

	li_rtn = SQLCA.nf_handle_error('nvo_verify_bank_reconciliation', '', 'of_perform_match - UPDATE CHEQUE HEADER')
	IF li_rtn < 0 THEN 
		RETURN -1 
	END IF

	// Update Control Report Totals
	IF SQLCA.SQLCode = 0 THEN
		il_no_cheques_reconciled = il_no_cheques_reconciled + 1
		idcm_amount_of_cheques_reconciled = idcm_amount_of_cheques_reconciled + ldcm_detail_cheque_amount
		
		IF ls_detail_bank_txn_type_code= '431' THEN      // 431 = disbursement audit debit 
			IF ls_detail_reconciled_code = '00' THEN      // 00 = match cheque 
				il_no_of_cheques_431_00 = il_no_of_cheques_431_00 + 1 
				idcm_amount_of_cheques_431_00 = idcm_amount_of_cheques_431_00 + ldcm_detail_cheque_amount
			ELSEIF ls_detail_reconciled_code = '09' THEN  // 09 = matched with $1.00
				il_no_of_cheques_431_09 = il_no_of_cheques_431_09 + 1
				idcm_amount_of_cheques_431_09 = idcm_amount_of_cheques_431_09 + ldcm_detail_cheque_amount
			ELSE
				idcm_amount_of_cheques_not_reconciled = idcm_amount_of_cheques_not_reconciled + ldcm_detail_cheque_amount
			END IF
		ELSEIF ls_detail_bank_txn_type_code= '180' THEN  // 180 = adjustment debit 
			IF ls_detail_reconciled_code = '00' THEN      // 00 = match cheque
				il_no_of_cheques_180_00 = il_no_of_cheques_180_00 + 1
				idcm_amount_of_cheques_180_00 = idcm_amount_of_cheques_180_00 + ldcm_detail_cheque_amount
			ELSEIF ls_detail_reconciled_code = '09' THEN  // 09 = matched with $1.00
				il_no_of_cheques_180_09 = il_no_of_cheques_180_09 + 1
				idcm_amount_of_cheques_180_09 = idcm_amount_of_cheques_180_09 + ldcm_detail_cheque_amount
			ELSE
				idcm_amount_of_cheques_not_reconciled = idcm_amount_of_cheques_not_reconciled + ldcm_detail_cheque_amount
			END IF
		ELSEIF ls_detail_bank_txn_type_code= '157' THEN  // 157 = void cheque
			IF ls_detail_reconciled_code = '07' THEN      // 07 = void cheque record 
				il_no_of_cheques_157_07 = il_no_of_cheques_157_07 + 1 
				idcm_amount_of_cheques_157_07 = idcm_amount_of_cheques_157_07 + ldcm_detail_cheque_amount
			ELSE
				idcm_amount_of_cheques_not_reconciled = idcm_amount_of_cheques_not_reconciled + ldcm_detail_cheque_amount
			END IF
		ELSEIF ls_detail_bank_txn_type_code= '432' THEN  // 432 = exception cheque paid 
			IF ls_detail_reconciled_code = '16' THEN      // 16 = exception cheque match 
				il_no_of_cheques_432_16 = il_no_of_cheques_432_16 + 1 
				idcm_amount_of_cheques_432_16 = idcm_amount_of_cheques_432_16 + ldcm_detail_cheque_amount
			ELSE
				idcm_amount_of_cheques_not_reconciled = idcm_amount_of_cheques_not_reconciled + ldcm_detail_cheque_amount
			END IF
		END IF
	END IF	
NEXT

RETURN 1 
end function

on nvo_verify_bank_reconciliation.create
call super::create
TriggerEvent( this, "constructor" )
end on

on nvo_verify_bank_reconciliation.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

