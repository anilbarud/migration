$PBExportHeader$w_bank_reconcile_cheques.srw
$PBExportComments$Bank Reconcile - Verify Payment Data Transmission (Reconciled Cheques)
forward
global type w_bank_reconcile_cheques from w_ancestor
end type
type sle_no_of_cheques from singlelineedit within w_bank_reconcile_cheques
end type
type st_4 from statictext within w_bank_reconcile_cheques
end type
type st_processing_message from statictext within w_bank_reconcile_cheques
end type
type sle_bank_import_no from singlelineedit within w_bank_reconcile_cheques
end type
type st_3 from statictext within w_bank_reconcile_cheques
end type
type sle_backup_file from singlelineedit within w_bank_reconcile_cheques
end type
type sle_input_file from singlelineedit within w_bank_reconcile_cheques
end type
type st_2 from statictext within w_bank_reconcile_cheques
end type
type st_1 from statictext within w_bank_reconcile_cheques
end type
type cb_print from commandbutton within w_bank_reconcile_cheques
end type
type cb_receiving_cheque_data from commandbutton within w_bank_reconcile_cheques
end type
type cb_close from commandbutton within w_bank_reconcile_cheques
end type
type dw_bank_reconcile_control_report from u_dw_online within w_bank_reconcile_cheques
end type
type gb_processed from groupbox within w_bank_reconcile_cheques
end type
type dw_bank_reconcile_error_report from u_dw_online within w_bank_reconcile_cheques
end type
type dw_imported_processed_cheque from u_dw_online within w_bank_reconcile_cheques
end type
end forward

global type w_bank_reconcile_cheques from w_ancestor
integer width = 2825
string title = "BANK RECONCILE CHEQUES"
string menuname = "m_cmwb_notools"
windowtype windowtype = main!
long backcolor = 67108864
event ue_post_open ( )
sle_no_of_cheques sle_no_of_cheques
st_4 st_4
st_processing_message st_processing_message
sle_bank_import_no sle_bank_import_no
st_3 st_3
sle_backup_file sle_backup_file
sle_input_file sle_input_file
st_2 st_2
st_1 st_1
cb_print cb_print
cb_receiving_cheque_data cb_receiving_cheque_data
cb_close cb_close
dw_bank_reconcile_control_report dw_bank_reconcile_control_report
gb_processed gb_processed
dw_bank_reconcile_error_report dw_bank_reconcile_error_report
dw_imported_processed_cheque dw_imported_processed_cheque
end type
global w_bank_reconcile_cheques w_bank_reconcile_cheques

type variables



nvo_verify_bank_reconciliation  invo_verify_bank_reconciliation   
	
end variables

event ue_post_open;call super::ue_post_open;Integer li_rtn
String  ls_payverifydir

SetPointer(HourGlass!)

/*	Create object containing necessary functions to do the processing.*/
invo_verify_bank_reconciliation = CREATE nvo_verify_bank_reconciliation
	
	

/*	Set transactions for the datawindow. */
li_rtn = dw_imported_processed_cheque.SetTransObject(SQLCA)

IF li_rtn = -1 THEN
	Messagebox("FATAL ERROR","Problem setting transobject(SQLCA) for dw_imported_processed_cheque",StopSign!)
END IF

ls_payverifydir = Upper(ProfileString(vgs_ini_filename,"BANK RECONCILIATION", "PayVerifyDir", ""))
sle_input_file.Text = ls_payverifydir + "F122S14.001"
end event

on w_bank_reconcile_cheques.create
int iCurrent
call super::create
if IsValid(this.MenuID) then destroy(this.MenuID)
if this.MenuName = "m_cmwb_notools" then this.MenuID = create m_cmwb_notools
this.sle_no_of_cheques=create sle_no_of_cheques
this.st_4=create st_4
this.st_processing_message=create st_processing_message
this.sle_bank_import_no=create sle_bank_import_no
this.st_3=create st_3
this.sle_backup_file=create sle_backup_file
this.sle_input_file=create sle_input_file
this.st_2=create st_2
this.st_1=create st_1
this.cb_print=create cb_print
this.cb_receiving_cheque_data=create cb_receiving_cheque_data
this.cb_close=create cb_close
this.dw_bank_reconcile_control_report=create dw_bank_reconcile_control_report
this.gb_processed=create gb_processed
this.dw_bank_reconcile_error_report=create dw_bank_reconcile_error_report
this.dw_imported_processed_cheque=create dw_imported_processed_cheque
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.sle_no_of_cheques
this.Control[iCurrent+2]=this.st_4
this.Control[iCurrent+3]=this.st_processing_message
this.Control[iCurrent+4]=this.sle_bank_import_no
this.Control[iCurrent+5]=this.st_3
this.Control[iCurrent+6]=this.sle_backup_file
this.Control[iCurrent+7]=this.sle_input_file
this.Control[iCurrent+8]=this.st_2
this.Control[iCurrent+9]=this.st_1
this.Control[iCurrent+10]=this.cb_print
this.Control[iCurrent+11]=this.cb_receiving_cheque_data
this.Control[iCurrent+12]=this.cb_close
this.Control[iCurrent+13]=this.dw_bank_reconcile_control_report
this.Control[iCurrent+14]=this.gb_processed
this.Control[iCurrent+15]=this.dw_bank_reconcile_error_report
this.Control[iCurrent+16]=this.dw_imported_processed_cheque
end on

on w_bank_reconcile_cheques.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.sle_no_of_cheques)
destroy(this.st_4)
destroy(this.st_processing_message)
destroy(this.sle_bank_import_no)
destroy(this.st_3)
destroy(this.sle_backup_file)
destroy(this.sle_input_file)
destroy(this.st_2)
destroy(this.st_1)
destroy(this.cb_print)
destroy(this.cb_receiving_cheque_data)
destroy(this.cb_close)
destroy(this.dw_bank_reconcile_control_report)
destroy(this.gb_processed)
destroy(this.dw_bank_reconcile_error_report)
destroy(this.dw_imported_processed_cheque)
end on

event open;call super::open;this.triggerevent('ue_post_open')





end event

event closequery;call super::closequery;destroy invo_verify_bank_reconciliation
end event

type sle_no_of_cheques from singlelineedit within w_bank_reconcile_cheques
integer x = 2094
integer y = 336
integer width = 279
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean autohscroll = false
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type st_4 from statictext within w_bank_reconcile_cheques
integer x = 1097
integer y = 340
integer width = 910
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "No of  Rows loaded from imput file:"
boolean focusrectangle = false
end type

type st_processing_message from statictext within w_bank_reconcile_cheques
integer x = 78
integer y = 28
integer width = 2555
integer height = 96
integer textsize = -11
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean italic = true
long textcolor = 255
long backcolor = 79741120
boolean enabled = false
boolean focusrectangle = false
end type

type sle_bank_import_no from singlelineedit within w_bank_reconcile_cheques
integer x = 727
integer y = 336
integer width = 279
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean autohscroll = false
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type st_3 from statictext within w_bank_reconcile_cheques
integer x = 183
integer y = 336
integer width = 425
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Bank Import No:"
boolean focusrectangle = false
end type

type sle_backup_file from singlelineedit within w_bank_reconcile_cheques
integer x = 727
integer y = 424
integer width = 1641
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean autohscroll = false
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type sle_input_file from singlelineedit within w_bank_reconcile_cheques
integer x = 727
integer y = 252
integer width = 1641
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean autohscroll = false
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type st_2 from statictext within w_bank_reconcile_cheques
integer x = 183
integer y = 424
integer width = 338
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Backup File:\"
boolean focusrectangle = false
end type

type st_1 from statictext within w_bank_reconcile_cheques
integer x = 183
integer y = 252
integer width = 270
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Input File:\"
boolean focusrectangle = false
end type

type cb_print from commandbutton within w_bank_reconcile_cheques
integer x = 823
integer y = 2340
integer width = 343
integer height = 104
integer taborder = 40
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Print "
end type

event clicked;if dw_imported_processed_cheque.rowcount() > 0 then
	dw_imported_processed_cheque.print()
end if
end event

type cb_receiving_cheque_data from commandbutton within w_bank_reconcile_cheques
integer x = 32
integer y = 2340
integer width = 686
integer height = 104
integer taborder = 60
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Importing Cheque Data"
end type

event clicked;integer li_result,li_rtn
long    ll_error_row, ll_rows, ll_bank_import_no
string  ls_message

THIS.Enabled = FALSE

cb_close.enabled = false

setpointer(Hourglass!)

invo_verify_bank_reconciliation.of_create_datastores()
 
/*	Call function load import file */
st_processing_message.text = "Loading file from network"
li_result = invo_verify_bank_reconciliation.of_load_file("F122S14.001")
IF li_result < 0 THEN
	goto Error_Recovery
END IF

/* Call function to close file */
st_processing_message.text = "Closing file to perform backup"
li_result =  invo_verify_bank_reconciliation.of_close_file()
IF li_result < 0 THEN
	goto Error_Recovery
END IF

/* call function to backup file */
st_processing_message.text = "Preparing to back up file"
li_result =  invo_verify_bank_reconciliation.of_backup_file()
IF li_result < 0 THEN
	goto Error_Recovery
END IF

/* re-open the file to use */
li_result = invo_verify_bank_reconciliation.of_load_file("F122S14.001")
IF li_result < 0 THEN
	goto Error_Recovery
END IF




SQLCA.nf_begin_transaction()


/* get the bank import no */
li_result = invo_verify_bank_reconciliation.of_get_bank_import_no()
IF li_result < 0 THEN
	goto Error_Recovery
END IF

/* load one record at a time from file 'f122s14.001 to database table */
st_processing_message.text = "Loading import table - IMPORTED_PROCESSED_CHEQUE"
li_result = invo_verify_bank_reconciliation.of_load_file_to_table()
IF li_result < 0 THEN
	goto Error_Recovery
END IF

/* get and display the processing information */
li_result = invo_verify_bank_reconciliation.of_get_no_cheques_to_process()
IF li_result < 0 THEN
	goto Error_Recovery
END IF

/* perform validation on imported TABLE - IMPORTED_PROCESSING_CHEQUE */
st_processing_message.text = "Performing validation on - IMPORTED_PROCESSED_CHEQUE"
li_result = invo_verify_bank_reconciliation.of_validate_table()
IF li_result < 0 THEN	
	goto Error_Recovery
END IF

/* display processing information to screen*/
invo_verify_bank_reconciliation.of_get_processing_information(sle_input_file.text,sle_backup_file.text,sle_bank_import_no.text,sle_no_of_cheques.text)

/* popualte the header record */
st_processing_message.text = "POPULATING DATABASE TABLES"
li_result = invo_verify_bank_reconciliation.of_populate_header()
IF li_result < 0 THEN	
	goto Error_Recovery
END IF

/* write the trailer record */
li_result = invo_verify_bank_reconciliation.of_populate_trailer()
IF li_result < 0 THEN	
	goto Error_Recovery
END IF

/* write the detail records */
li_result = invo_verify_bank_reconciliation.of_populate_details(dw_imported_processed_cheque)
IF li_result < 0 THEN	
	goto Error_Recovery
END IF

/* get record counts and totals  for control report */
st_processing_message.text = "Producing Control Report Totals"
li_result = invo_verify_bank_reconciliation.of_produce_controls()
IF li_result < 0 THEN	
	goto Error_Recovery
END IF

/* swap the datawindow - process through to perform bank reconcilation on these records */
dw_imported_processed_cheque.DataObject  = 'd_processed_cheque_detail'
li_rtn = dw_imported_processed_cheque.SetTransObject(SQLCA)

/* perform match */
st_processing_message.text = "Performing Cheque Matching Process"
li_result = invo_verify_bank_reconciliation.of_perform_match(dw_imported_processed_cheque)
IF li_result < 0 THEN	
	goto Error_Recovery
END IF

SQLCA.nf_commit_transaction()


ll_bank_import_no =  invo_verify_bank_reconciliation.of_select_bank_import_no()

dw_imported_processed_cheque.DataObject  = 'd_bank_reconciled_report'
li_rtn = dw_imported_processed_cheque.SetTransObject(SQLCA)

ll_rows = dw_imported_processed_cheque.retrieve(ll_bank_import_no)
IF SQLCA.nf_handle_error('w_bank_reconcile_cheques', 'dw_imported_processed_cheque', 'cb_receiving_cheque_date - dw_imported_processed_cheque.retrieve(ll_bank_import_no)') < 0 THEN 
	goto Error_Recovery
END IF

if ll_rows > 0 then
	dw_imported_processed_cheque.visible =  true
	cb_print.enabled = true
else
	cb_print.enabled = false
end if

st_processing_message.text = "Printing Control Report"

invo_verify_bank_reconciliation.of_print_error_report()
invo_verify_bank_reconciliation.of_produce_control_report()
invo_verify_bank_reconciliation.of_print_control_report()

invo_verify_bank_reconciliation.of_destroy_datastores()

/* Call function to close file */
st_processing_message.text = "Closing file to perform backup"
li_result =  invo_verify_bank_reconciliation.of_close_file()
IF li_result < 0 THEN
	goto Error_Recovery
END IF

st_processing_message.text = "Removing imported file"
invo_verify_bank_reconciliation.of_remove_imported_file()

st_processing_message.text = "Process Complete - Please check Control Report"

cb_close.enabled = true

return 1


Error_Recovery:
st_processing_message.text = "Printing ERROR report - Please Check Error Report"
invo_verify_bank_reconciliation.of_print_error_report()
SQLCA.nf_rollback_transaction()
cb_close.enabled = true

end event

type cb_close from commandbutton within w_bank_reconcile_cheques
integer x = 2354
integer y = 2340
integer width = 283
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

type dw_bank_reconcile_control_report from u_dw_online within w_bank_reconcile_cheques
boolean visible = false
integer x = 1682
integer y = 2004
integer height = 180
integer taborder = 20
string dataobject = "d_bank_reconcile_control_report"
end type

type gb_processed from groupbox within w_bank_reconcile_cheques
integer x = 46
integer y = 180
integer width = 2624
integer height = 412
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Processing Information"
end type

type dw_bank_reconcile_error_report from u_dw_online within w_bank_reconcile_cheques
boolean visible = false
integer x = 197
integer y = 740
integer width = 2149
integer height = 900
integer taborder = 10
string dataobject = "d_bank_reconcile_error_report"
boolean hscrollbar = true
boolean vscrollbar = true
end type

type dw_imported_processed_cheque from u_dw_online within w_bank_reconcile_cheques
boolean visible = false
integer x = 82
integer y = 632
integer width = 2578
integer height = 1604
integer taborder = 30
string dataobject = "d_imported_processed_cheque"
boolean vscrollbar = true
end type

