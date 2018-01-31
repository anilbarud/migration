$PBExportHeader$u_dw_document_path.sru
$PBExportComments$Common - User object containing datawindow to retrieve document path used for viewing and printing imaged documents
forward
global type u_dw_document_path from u_dw_online
end type
end forward

global type u_dw_document_path from u_dw_online
integer width = 846
integer height = 108
boolean enabled = false
string dataobject = "d_filename"
end type
global u_dw_document_path u_dw_document_path

type prototypes

end prototypes

type variables
LONG il_window_handle

u_word ioo_word
u_excel ioo_excel

LONG il_word_handles[]
LONG il_excel_handles[]

BOOLEAN ib_opened_pdf_files


end variables

forward prototypes
public subroutine uf_set_window_handle (long al_hanlde)
public function integer uf_close_office ()
public function integer f_manage_document (long al_docid, string as_view_or_print, string as_option)
public function long uf_get_doc_handle (string as_classname, string as_doc_id)
public function integer uf_is_open (string as_file_path, string as_host_application_name)
end prototypes

public subroutine uf_set_window_handle (long al_hanlde);il_window_handle = al_hanlde
gl_window_handle = il_window_handle
end subroutine

public function integer uf_close_office ();Integer li_upper, li_counter

li_upper = UpperBound(il_word_handles)

FOR li_counter = 1 TO li_upper
	IF IsWindow(il_word_handles[li_counter]) <> 0 THEN
		ioo_word.uf_disconnect()
	ELSE
		continue
	END IF
NEXT

li_upper = UpperBound(il_excel_handles)

FOR li_counter = 1 TO li_upper
	IF IsWindow(il_excel_handles[li_counter]) <> 0 THEN
		ioo_excel.uf_disconnect()
	ELSE
		continue
	END IF
NEXT

return 0
end function

public function integer f_manage_document (long al_docid, string as_view_or_print, string as_option);Long    ll_numrows, ll_row_number, ll_open, ll_pos, ll_rtn, ll_handle, ll_rows 
Integer li_error_status, li_rtn
String  ls_filename, ls_file_extension, ls_classname, ls_host_application_name
String  ls_document_type_code, ls_allow_update_flag, ls_reverse_file_name, ls_file_name
Boolean lb_read_only = TRUE, lb_rtn
u_ds    lds_allow_update_flag
w_sheet lw_sheet
w_rehab_sheet lw_rehab_sheet
n_pdf   lnv_pdf				

lds_allow_update_flag = CREATE u_ds
lds_allow_update_flag.dataobject = 'd_allow_update_flag'
lds_allow_update_flag.SetTransObject(IMAGETRANS)

SetPointer(Hourglass!)	

// Check il_window_handle to see if it lost its value. -- Stephanie Manzer
IF il_window_handle = 0 THEN	
	il_window_handle = gl_window_handle
END IF

// Get the document's path.
ll_numrows = This.Retrieve(al_docid)
IF ImageTrans.nf_handle_error('u_dw_document_path', '', 'f_manage_document - This.Retrieve(al_docid)') < 0 THEN
	MessageBox("Document Path Error", "Could Not Determine the Document's Path. Docid = " + String(al_docid), StopSign!)
	RETURN -1
END IF

IF ll_numrows > 0 THEN
	ls_filename = This.GetItemString(ll_numrows, "filename")

	// Get the type_code so we can retrieve the allow_update_flag
	ls_document_type_code = This.GetItemString(ll_numrows, "document_index_type_code")
	IF NOT IsNull(ls_document_type_code) THEN
		ll_rows = lds_allow_update_flag.Retrieve(ls_document_type_code)
		IMAGETRANS.nf_handle_error("u_dw_document_path", "", "f_manage_document - lds_allow_update_flag.Retrieve(ls_document_type_code)")
		IF ll_rows <> 1 Then
			SignalError(-666, "Error reading allow_update_flag for type_code '" + ls_document_type_code + "'.")
		END IF
		ls_allow_update_flag = lds_allow_update_flag.GetItemString(1, "allow_update_flag")
		//If the allow_update_flag = "Y" then the document will be opened in normal mode
		//IF the allow_update_flag = "N" then the document will be opened in read-only
		IF ls_allow_update_flag = "Y" THEN
			lb_read_only = FALSE
		END IF
	END IF

	// Make sure file is a valid type
	ll_pos = LastPos(ls_filename, ".")
	IF ll_pos = 0 OR IsNull(ll_pos) = TRUE THEN
		MessageBox("Error Opening File", "Unable to determine file extension of file: " + ls_filename + ".", Exclamation!)
		RETURN -1
	END IF

	ls_file_extension = Upper(Right(ls_filename, Len(ls_filename) - ll_pos))
	
	SELECT Upper(ISNULL(host_application_name, '')) 
	  INTO :ls_host_application_name 
	  FROM File_Type 
	 WHERE Upper(file_type_code) = :ls_file_extension ; 
	
	li_rtn = SQLCA.nf_handle_error("u_dw_document_path", "", "f_manage_document - SELECT host_application_name FROM File_Type")
	
	IF FileExists(ls_filename) = FALSE THEN 
		MessageBox("Error opening file", "Could not open " + ls_host_application_name + " document. File " + ls_filename + " does not exist.", Exclamation!)
		RETURN -1
	END IF

	IF ls_host_application_name = 'MICROSOFT WORD' THEN
		ioo_word = CREATE u_word
		li_rtn = ioo_word.uf_connect()

		IF li_rtn < 0 THEN
			MessageBox("Warning","Could Not connect to Word", Information!) 
		END IF

		ll_open = uf_is_open(ls_filename, ls_host_application_name)
		IF ll_open > 0 THEN
			ls_reverse_file_name = Reverse(ls_filename)
			ls_file_name = Reverse(Left(ls_reverse_file_name, (Pos(ls_reverse_file_name,'\') - 1) ))
			MessageBox('File Open', 'This document: ' + ls_file_name + ' is already open. Please close the document and try again.', Information!)
			RETURN 0
		ELSE							
			li_rtn = ioo_word.uf_file_open(ls_filename, lb_read_only, TRUE, 0)
			IF li_rtn < 0 THEN
				MessageBox("Error Opening Word Document", "Could not open Word document: " + ls_filename, Exclamation!)
			END IF

			IF as_view_or_print = "P" THEN
				ioo_word.uf_print()
				ioo_word.uf_file_close()
			ELSE
				ls_classname = "OpusApp"
				ll_handle = uf_get_doc_handle(ls_classname, String(al_docid)) 
				IF ll_handle > 0 THEN
					ll_rtn = UpperBound(il_word_handles)
					il_word_handles[ll_rtn + 1] = ll_handle
					lb_rtn = BringWindowToTop(ll_handle)
				END IF
			END IF
		END IF
	ELSEIF ls_host_application_name = 'MICROSOFT EXCEL' THEN
		ioo_excel = CREATE u_excel	
		li_rtn = ioo_excel.uf_connect()
		IF li_rtn < 0 THEN
			MessageBox("Error Connecting to Excel", "Could Not connect to Excel.", Exclamation!) 
		END IF

		ll_open = uf_is_open(ls_filename, ls_host_application_name)
		IF ll_open > 0 THEN
			ls_reverse_file_name = Reverse(ls_filename)
			ls_file_name = Reverse(Left(ls_reverse_file_name, (Pos(ls_reverse_file_name, '\') - 1))) 
			MessageBox('File Open','This document: ' + ls_file_name + ' is already open. Please close the document and try again.', Information!)
		ELSE
			li_rtn = ioo_excel.uf_file_open(ls_filename, lb_read_only, TRUE, 0)
			IF li_rtn < 0 THEN
				MessageBox("Error Opening Excel Spreadsheet", "Could not open Excel spreadsheet " + ls_filename, Exclamation!)
			END IF

			IF as_view_or_print = "P" THEN
				//	Print the document.
				ioo_excel.uf_print()
				ioo_excel.uf_file_close()
			ELSE
				ls_classname = "XLMAIN"
				ll_handle = uf_get_doc_handle(ls_classname, String(al_docid)) 
				IF ll_handle > 0 THEN
					ll_rtn = UpperBound(il_excel_handles)
					il_excel_handles[ll_rtn + 1] = ll_handle
					lb_rtn = BringWindowToTop(ll_handle)
				END IF
			END IF
		END IF
	ELSEIF ls_host_application_name = 'ADOBE ACROBAT' THEN 
		lnv_pdf = CREATE n_pdf 
		IF as_view_or_print = 'P' THEN 
			// printing seems to leave one acrobat reader window open 
			ib_opened_pdf_files = TRUE 
			lnv_pdf.nf_print_pdf(ls_filename) 
		ELSE
			li_rtn = lnv_pdf.nf_open_pdf_file(gs_adobe_path, ls_filename)
			IF li_rtn < 0 THEN
				RETURN li_rtn
			ELSE
				ib_opened_pdf_files = TRUE
			END IF
		END IF
	ELSE
		// If it is an Imaged document ('IMA' or TIF). Although, from what I understand, all IMA documents have been converted to TIF.
		IF ls_file_extension = 'IMA' OR ls_file_extension = 'TIF' THEN
			ll_row_number = ll_numrows
	
			// P10151-259 R.S., July 2013 - The original external function PrintIMA() would not work on certain images that had been scanned with the 24 bit colour
			// setting turned on, on the scanner. Now we use an image edit control on the object uo_image_append. It works with those images that wouldn't print before
			// we need a refernce to the object uo_image_append, which will be on either w_sheet, or w_rehab_sheet
			ls_classname = w_frame.GetActiveSheet().ClassName()
	
			IF ls_classname = 'w_sheet' THEN
				// works for doc list in w_sheet as well as when printing a doc from the document indexing module (when you delete a doc, you can print it)
				lw_sheet = w_frame.GetActiveSheet()
			ELSEIF ls_classname = 'w_rehab_sheet' THEN
				// for when printing task attachments from rehab module
				lw_rehab_sheet = w_frame.GetActiveSheet() 
			END IF
			
			DO UNTIL ll_row_number = 0
				ls_filename = This.GetItemString (ll_row_number, "filename")
	
				IF as_view_or_print = "P" THEN	
					IF IsValid(lw_sheet) THEN
						li_error_status = lw_sheet.uo_image_append.of_print_image(ls_filename)	
					ELSEIF IsValid(lw_rehab_sheet) THEN
						li_error_status = lw_rehab_sheet.uo_image_append.of_print_image(ls_filename)
					ELSE
						MessageBox("Document Printing","Document: " + ls_filename + " could not be printed.  Could not aquire a proper printing object from memory" )
						RETURN 0
					END IF
					
					IF li_error_status >= 1 THEN
						MessageBox("Document Printing","Document: " + ls_filename + " could not be printed.  Error No: " + String(li_error_status) + ".~r~n Processing continues.")
					END IF	
				END IF
				
				ll_row_number = ll_row_number - 1
			LOOP
		ELSE
			MessageBox("Unknown File Type", "Could not open file " + ls_filename + ". I do not know how to open a " + ls_file_extension + " type file." , Exclamation!)
			RETURN -1
		END IF
	END IF
END IF

SetPointer(Arrow!)

IF as_view_or_print = 'P' THEN
	uf_close_office()
END IF

RETURN ll_numrows
end function

public function long uf_get_doc_handle (string as_classname, string as_doc_id);// uf_get_doc_handle
//
Long ll_rtn
n_search_window_titlebars ln_search_window_titlebars

ln_search_window_titlebars = CREATE n_search_window_titlebars 
ll_rtn = ln_search_window_titlebars.nf_search_window_titlebars(as_classname, as_doc_id)

RETURN ll_rtn

end function

public function integer uf_is_open (string as_file_path, string as_host_application_name);// uf_is_open
// 
// Returns: 1 - if document is already open
//          0 - if document is Not open 
//
String  ls_reverse_file_name, ls_file_name_with_extension, ls_file_name_without_extension, ls_file_name, ls_classname 
Long    ll_rtn
Integer li_rtn
Boolean lb_found 
n_search_window_titlebars ln_search_window_titlebars 

// Get file name out of full path 
ls_reverse_file_name = Reverse(as_file_path)
ls_file_name = Reverse(Left(ls_reverse_file_name, (Pos(ls_reverse_file_name,'\') - 1) ))
ls_file_name_with_extension = Trim(ls_file_name)
ls_file_name_without_extension = Trim(Left(ls_file_name, (Pos(ls_file_name,'.') - 1) )) 

as_host_application_name = Trim(Upper(as_host_application_name)) 
lb_found = FALSE

IF as_host_application_name = 'MICROSOFT WORD' OR as_host_application_name = 'MICROSOFT EXCEL' THEN
	ln_search_window_titlebars = CREATE n_search_window_titlebars 
	
	IF as_host_application_name = 'MICROSOFT EXCEL' THEN 
		ls_classname = "XLMAIN"
	ELSE
		ls_classname = "OpusApp"
	END IF

	ll_rtn = ln_search_window_titlebars.nf_search_window_titlebars(ls_classname, ls_file_name_without_extension)
	IF ll_rtn > 0 THEN
		lb_found = TRUE
	END IF	
ELSE
	MessageBox("Error", "An unknown file type " + as_host_application_name + " was detected when trying to see if the document was already open.") 
END IF

IF lb_found = TRUE THEN
	li_rtn = 1
ELSE
	li_rtn = 0
END IF

RETURN li_rtn

end function

on constructor;call u_dwa::constructor;this.settransobject(ImageTrans)
end on

on u_dw_document_path.create
call super::create
end on

on u_dw_document_path.destroy
call super::destroy
end on

event destructor;call super::destructor;Integer li_rtn
Long    n, ll_rtn, ll_num_handles

// Close Word Documents opened from this instance
IF IsValid(ioo_word) = TRUE THEN 
	ll_num_handles = UpperBound(il_word_handles)
	FOR n = 1 TO ll_num_handles
		// Send message to Word app to tell it to close
		ll_rtn = Send(il_word_handles[n], 16, 0, 0)
	NEXT
	ioo_word.uf_disconnect()
END IF

// Close Excel Spreadsheets opened from this instance
IF IsValid(ioo_excel) = TRUE THEN 
	ll_num_handles = UpperBound(il_excel_handles)
	FOR n = 1 TO ll_num_handles
		// Send message to Word app to tell it to close
		ll_rtn = Send(il_excel_handles[n], 16, 0, 0)
	NEXT
	ioo_excel.uf_disconnect()
END IF

end event

