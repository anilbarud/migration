$PBExportHeader$n_last_no.sru
forward
global type n_last_no from nonvisualobject
end type
end forward

global type n_last_no from nonvisualobject
end type
global n_last_no n_last_no

type variables
private STRING		is_table_name
private STRING		is_column_name
U_DS		ids_last_no
end variables

forward prototypes
public subroutine nf_set_last_no (long al_increment)
public function long nf_get_last_no ()
public function integer nf_set_source (string ls_table_name, string ls_column_name)
end prototypes

public subroutine nf_set_last_no (long al_increment);STRING			ls_sql_update
STRING			ls_sql_select
LONG				ll_next_no
LONG				ll_rows

//Add 1 to the last number.  This is done first so the table is locked and 
//no one else can udpate the  number before we get a change to use it.
If is_table_name = '' or is_column_name = '' then
	SignalError(666,"Last number table name or column name not supplied.")
end if

ls_sql_update = "UPDATE " + is_table_name &
					+" SET " + is_column_name + " = " + is_column_name + " + " + STRING(al_increment)
			
EXECUTE IMMEDIATE :ls_sql_update USING SQLCA;
SQLCA.nf_handle_error("n_last_no","","nf_set_last_no - UPDATE <table _name>")

end subroutine

public function long nf_get_last_no ();STRING			ls_sql_update
STRING			ls_sql_select
LONG				ll_last_no
LONG				ll_rows

//Add 1 to the last number.  This is done first so the table is locked and 
//no one else can udpate the  number before we get a change to use it.
If is_table_name = '' or is_column_name = '' then
	SignalError(666,"Last number table name or column name not supplied.")
end if

ll_rows = ids_last_no.RETRIEVE()
SQLCA.nf_handle_error("n_last_no","","nf_get_last_no - ids_last_no.RETRIEVE()")

iF ll_rows <> 1 THEN
	SignalError(666,"Last number from table " + is_table_name + " not found.")
end if

ll_last_no = ids_last_no.getitemnumber(1,1)

return ll_last_no
end function

public function integer nf_set_source (string ls_table_name, string ls_column_name);STRING 			ls_sql_select
STRING			ls_syntax
STRING			ls_sql_error
STRING			ls_syntax_error

is_table_name = ls_table_name
is_column_name = ls_column_name

ls_sql_select = "SELECT " + is_column_name + " FROM " + is_table_name + " TABLOCKX"
ls_syntax = SQLCA.SYNTAXfromsql(ls_sql_select,'Style(Type=Form)',ls_sql_error)
if ls_sql_error <> '' Then
	return -1
end if

ids_last_no.create(ls_syntax,ls_syntax_error)
If ls_syntax_error <> '' then
	return -1
end if

ids_last_no.settransobject(SQLCA)

return 1


end function

event constructor;ids_last_no = create u_ds
end event

on n_last_no.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_last_no.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

