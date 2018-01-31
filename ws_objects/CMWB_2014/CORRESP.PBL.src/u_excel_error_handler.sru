$PBExportHeader$u_excel_error_handler.sru
forward
global type u_excel_error_handler from oleobject
end type
end forward

global type u_excel_error_handler from oleobject
end type
global u_excel_error_handler u_excel_error_handler

event error;Action = ExceptionSubstituteReturnValue!
ReturnValue = -999
end event

on u_excel_error_handler.create
call super::create
TriggerEvent( this, "constructor" )
end on

on u_excel_error_handler.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

event externalexception;Action = ExceptionSubstituteReturnValue!
ReturnValue = -999
end event

