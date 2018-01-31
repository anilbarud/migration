$PBExportHeader$n_pdf_converter.sru
forward
global type n_pdf_converter from nonvisualobject
end type
end forward

global type n_pdf_converter from nonvisualobject
end type
global n_pdf_converter n_pdf_converter

forward prototypes
public function integer nf_convert_to_pdf (string as_source, string as_destination, string as_application)
public function boolean nf_confirm_files_converted (string as_source_location, string as_destination_location)
end prototypes

public function integer nf_convert_to_pdf (string as_source, string as_destination, string as_application);INTEGER li_return

//messagebox('nf_converttopdf', as_application + " " + as_source + " " + as_destination)
li_return = run(as_application + " " + as_source + " " + as_destination, minimized!)


RETURN li_return 
end function

public function boolean nf_confirm_files_converted (string as_source_location, string as_destination_location);n_filesys		ln_fsys
STRING 		ls_path, ls_name_source[], ls_source_location, ls_destination_location, ls_name_destination[]
DATETIME 	ldt_write[]
BOOLEAN 	lb_subdir[]
DOUBLE 		ld_size[]
INTEGER 	li_max

// sorce files
ls_path = as_source_location + '\'
li_max = ln_fsys.of_GetFiles(ls_path, False, ls_name_source, ld_size, ldt_write, lb_subdir)

// destination files
ls_path = as_destination_location + '\'
li_max = ln_fsys.of_GetFiles(ls_path, False, ls_name_destination, ld_size, ldt_write, lb_subdir)

IF  upperbound(ls_name_source) = upperbound(ls_name_destination) THEN 
	RETURN TRUE
END IF

RETURN FALSE
end function

on n_pdf_converter.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_pdf_converter.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

