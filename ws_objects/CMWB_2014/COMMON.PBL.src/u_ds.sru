$PBExportHeader$u_ds.sru
$PBExportComments$Datastore object with error handling included.
forward
global type u_ds from datastore
end type
end forward

global type u_ds from datastore
end type
global u_ds u_ds

type variables
Protected:
transaction	vit_trans_object
PUBLIC constant int 	TIFF = -666
end variables

forward prototypes
public function integer settransobject (transaction vat_trans_object)
public function string getprinter ()
public subroutine nf_set_printpreview (boolean lb_preview_on)
public function integer setprinter (string ls_printer_name)
public function integer saveas (string as_file_name, integer ai_file_type)
public function long pagecount ()
end prototypes

public function integer settransobject (transaction vat_trans_object);/* ----------------------------------------------------------------------------------------------------	*/
/*	Function Name: 	settransobject																							   */
/*																																			*/
/*	Purpose:				This function sets the transaction object for the datawindow.  It also stores			*/
/*							the transaction object in an instance variable so that it can be used by the			*/
/*							db_error event.																							*/
/*																																			*/
/*	Arguments:			Transaction	-	vat_trans_object	-	The transaction object for which this data		*/
/*																			window is to be set.										*/
/*																																			*/
/* ----------------------------------------------------------------------------------------------------	*/


Integer	vli_return_code


vli_return_code = Super::SetTransObject(vat_trans_object)

If vli_return_code > 0 Then
	vit_trans_object = vat_trans_object
End If

Return vli_return_code
end function

public function string getprinter ();STRING		ls_default_printer_name
if  RegistryGet(gs_default_printer_key,gs_default_printer_value_name,RegString!,ls_default_printer_name) = -1 then return ""

return ls_default_printer_name
end function

public subroutine nf_set_printpreview (boolean lb_preview_on);this.object.datawindow.print.preview = lb_preview_on
end subroutine

public function integer setprinter (string ls_printer_name);Return RegistrySet(gs_default_printer_key,gs_default_printer_value_name,RegString!,ls_printer_name)
end function

public function integer saveas (string as_file_name, integer ai_file_type);//	Return values -1 = UNKNOWN
//						-2 = File was not created
//						-3 = PRINT DRIVER NOT INSTALLED

STRING		ls_default_printer_name
CONSTANT STRING		cs_tiff_extension = ".TIF"
INT			li_rtn
STRING		ls_return_path_filename
STRING		ls_return_filename
INT			li_print_return

ls_return_path_filename = this.dataobject
if ls_return_path_filename = "" then return -1

If ai_file_type = TIFF then	
	
	ls_default_printer_name = this.getprinter()	
	
	if this.setprinter(gs_tiff_printer_name) = -1 then return -1
	
	If as_file_name = "" Then
		li_rtn =	GetFileSaveName("Save " + this.dataobject,ls_return_path_filename,ls_return_filename,"TIF","Tiff File (*.tif), *.TIF")
		If li_rtn = 1 then
			as_file_name = ls_return_path_filename
		Else
			return li_rtn
		end if
	else
		as_file_name =	as_file_name + cs_tiff_extension
	end if
	
	this.object.datawindow.print.filename = as_file_name
	//Print the datawindow
	li_print_return = this.print(false)
	//Set the printer back to the default
	If this.setprinter(ls_default_printer_name) = -1 then return -1
	//Make sure the print executed properly
	If li_print_return = -1 then return -3
	//Confirm that the file exists
	If not  FileExists(as_file_name) then return -2
else
	return -1
end if

return 1
end function

public function long pagecount ();RETURN INTEGER(Describe("Evaluate('PageCount()',0)"))

end function

on u_ds.create
call super::create
TriggerEvent( this, "constructor" )
end on

on u_ds.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

event dberror;/*	Capture the sqldbcode and the sqlerrtext in the transaction object
	for this datastore (Note: The function uf_SetTransObject must be used 
	instead of setting the transaction object directly) */

	If IsValid(vit_trans_object) Then
		vit_trans_object.SQLDBCode	= sqldbcode
		vit_trans_object.SQLErrText = sqlerrtext
		RETURN 1
	End If
end event

