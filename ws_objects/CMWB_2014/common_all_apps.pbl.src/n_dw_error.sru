$PBExportHeader$n_dw_error.sru
forward
global type n_dw_error from error
end type
end forward

shared variables

end variables

global type n_dw_error from error
end type
global n_dw_error n_dw_error

type variables
STRING vis_database
STRING vis_returndata
STRING vis_type      // database - D or Powerbuilder - P
LONG vil_dbcode

String	is_Database
String	is_ReturnData
String	is_Type		// D=database or P=Powerbuilder
Long	il_DBCode
INTEGER ii_sys_resources


STRING		is_ApplicationName
DATETIME	idt_ErrorDatetime

STRING		is_user_id
STRING		is_user_comment

end variables

on n_dw_error.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_dw_error.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

