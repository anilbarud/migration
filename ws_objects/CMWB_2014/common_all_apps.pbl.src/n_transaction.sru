$PBExportHeader$n_transaction.sru
forward
global type n_transaction from transaction
end type
end forward

global type n_transaction from transaction
end type
global n_transaction n_transaction

type variables
//Protected:

Boolean  ib_connected = False
Boolean  ib_display_message = TRUE
end variables

forward prototypes
public function boolean serviceavailable ()
public subroutine nf_set_display_mess_flag (boolean ab_setting)
public function integer nf_handle_error (string as_window, string as_datawindow, string as_script)
public subroutine nf_connect ()
public function integer nf_handle_error (window a_window, userobject a_userobject, string as_script)
public function integer nf_handle_error (window a_window, datastore a_u_ds, string as_script)
public function integer nf_handle_error (window a_window, datawindow a_u_dw_online, string as_script)
public subroutine nf_transaction_count (ref integer ai_trancount, integer ai_expected_trancount, string as_error_object, string as_error_objectevent, string as_error_text, boolean ab_display_error)
public function integer nf_begin_transaction ()
public function integer nf_commit_transaction ()
public function integer nf_rollback_transaction ()
public function integer nf_handle_error (string as_window, string as_datawindow, string as_script, integer ai_num_rows_expected_updated)
end prototypes

public function boolean serviceavailable ();//	This function returns the value of the instance variable ServiceAvailable


	Return ib_connected
end function

public subroutine nf_set_display_mess_flag (boolean ab_setting);// set the message display flag to whatever the developer wants
// true - default - display messages
// false - don't display message

ib_display_message = ab_setting
end subroutine

public function integer nf_handle_error (string as_window, string as_datawindow, string as_script);/*	=================================================================
	Error Handling Function for SQL Server (96-03-25 / Version 1.2)
	=================================================================
	Outcome									Return Value	Action
	-------									------------	------
	Success											  0		None
	No Data Retrieved for embedded SQL (!)	100		None	
	Time Stamp Error 532							N/A		SignalError (Application System Error Event)
	Deadlock 1205, 1211, or 7112				N/A		SignalError (Application System Error Event)
	Unrecoverable Failure						N/A		SignalError (Application System Error Event)
*/

	STRING						ls_sqlerrtext,	ls_sqlreturndata
	LONG							ll_sqldbcode,	ll_sqlcode
	BOOLEAN						lb_terminate
	n_transaction           ltr_transobj


/* Handle no error and no data retrieved in SQL call
*/
	IF (This.SQLDBCode=0) and (Error.Number=0) AND Error.il_dbcode=0 THEN
		Return(0)
	END IF
	IF This.SQLDBCode = 3 THEN
		This.SQLDBCode = 0
		Return(100)
	END IF

/* Clear/reset dbcode and error text so the error is cleared out for the next call to error
	handling.  
*/
	IF This.SQLDBCode<>0 OR Error.il_dbcode=0 THEN
		ls_sqlerrtext		=	this.SQLErrText
		ls_sqlreturndata	=	this.SQLReturnData
		ll_sqldbcode		=	this.SQLDBCode
		ll_sqlcode			=	this.SQLCode
	ELSEIF Error.il_dbcode<>0 THEN
		ls_sqlerrtext		=	Error.Text
		ls_sqlreturndata	=	Error.is_ReturnData
		ll_sqldbcode		=	Error.il_dbcode
		ll_sqlcode			=	Error.Number
		lb_terminate = TRUE
	END IF
	
	THIS.SQLDBCode 	= 0
	THIS.SQLErrText 	= ""
	Error.is_database    = ""
   Error.il_dbcode      = 0
   Error.Number         = 0
   Error.Text           = ""
   Error.is_ReturnData  = ""
   Error.is_type        = ""
   Error.windowmenu     = ""
   Error.object         = ""
   Error.objectevent    = ""


/* For Remaining Cases: Rollback due to DB-Error, or SQL-Error
*/
	ltr_transobj	=	THIS
	ltr_transobj.nf_rollback_transaction()
	If THIS.SQLCode <> 0 Then
		ls_sqlerrtext = 'Rollback failed ' + String(This.SQLCode) + ' ' + ls_sqlerrtext
		lb_terminate = TRUE
	End If


	IF lb_terminate THEN
		GoTo Terminate
	END IF

/* For Remaining Cases: Handling as Unrecoverable Errors
*/
	Terminate:

	Error.is_database    = THIS.Database
   Error.il_dbcode      = ll_sqldbcode
   Error.Number         = ll_sqlcode
   Error.Text           = ls_sqlerrtext
   Error.is_ReturnData  = ls_sqlreturndata
   Error.is_type        = 'D'
   Error.windowmenu     = as_window
   Error.object         = as_datawindow
   Error.objectevent    = as_script

	SignalError()

end function

public subroutine nf_connect ();n_transaction ltr_transobj
LONG ll_rc

ltr_transobj = This


//	If the system is unavailable, we will continue as minimum services will be allowed


	Connect using ltr_transobj;

	ll_rc = this.nf_handle_error("n_transaction - of_connect","", "Error connecting User " + this.LogId + " to " + this.Database +&
									  " Database on Server " + this.ServerName + ".")

	If ltr_transobj.sqlcode < 0 Then
		ib_connected = False

	ElseIf ltr_transobj.SQLCode = 0 Then
		ib_connected = True
	END IF




end subroutine

public function integer nf_handle_error (window a_window, userobject a_userobject, string as_script);nf_handle_error(a_window.classname(), a_userobject.classname(), as_script )

return 1
end function

public function integer nf_handle_error (window a_window, datastore a_u_ds, string as_script);nf_handle_error(a_window.classname(), a_u_ds.classname() + ' ' + a_u_ds.dataobject, as_script )

RETURN 1
end function

public function integer nf_handle_error (window a_window, datawindow a_u_dw_online, string as_script);nf_handle_error(a_window.classname(), a_u_dw_online.classname() + ' ' + a_u_dw_online.dataobject, as_script )

RETURN 1
end function

public subroutine nf_transaction_count (ref integer ai_trancount, integer ai_expected_trancount, string as_error_object, string as_error_objectevent, string as_error_text, boolean ab_display_error);SELECT top 1 @@trancount
INTO   :ai_trancount
FROM   sysobjects
USING THIS;

IF ab_display_error THEN
	IF ai_trancount <> ai_expected_trancount THEN
		Error.Object      = as_Error_Object
		Error.ObjectEvent = as_Error_ObjectEvent		
		Error.Text        = as_error_text + ': ' + STRING(ai_trancount) + ' transactions open, not ' + STRING(ai_expected_trancount)
		
		IF ai_trancount > 0 THEN
			THIS.nf_rollback_transaction()
			f_populate_app_log(gs_appname,100,as_Error_Object,Error.Text)
		END IF
		
		SignalError()
	END IF
END IF

end subroutine

public function integer nf_begin_transaction ();INTEGER li_rc
STRING  ls_begin

ls_begin = 'BEGIN TRANSACTION'

// begin a transaction
EXECUTE IMMEDIATE :ls_begin USING THIS;
li_rc = THIS.nf_handle_error('n_transaction - nf_begin_transaction','', 'Error beginning transaction for ' + THIS.Database + ' Database on Server ' + THIS.ServerName + '.')
									  
RETURN li_rc
end function

public function integer nf_commit_transaction ();INTEGER li_rc
STRING  ls_commit

ls_commit = 'COMMIT TRANSACTION'

// Commit a transaction
EXECUTE IMMEDIATE :ls_commit USING THIS;
li_rc = THIS.nf_handle_error('n_transaction - nf_commit_transaction','', 'Error committing transaction for ' + THIS.Database + ' Database on Server ' + THIS.ServerName + '.')
									  
RETURN li_rc
end function

public function integer nf_rollback_transaction ();INTEGER li_rc, li_trancount
STRING  ls_rollback

THIS.nf_transaction_count(li_trancount,1,'','','',FALSE)

// ONLY rollback if trancount > 0
IF li_trancount > 0 THEN
	
	ls_rollback = 'ROLLBACK TRANSACTION'
	
	// Rollback a transaction
	EXECUTE IMMEDIATE :ls_rollback USING THIS;
	li_rc = THIS.nf_handle_error('n_transaction - nf_rollback_transaction','', 'Error rolling back transaction for ' + THIS.Database + ' Database on Server ' + THIS.ServerName + '.')
END IF

RETURN li_rc
end function

public function integer nf_handle_error (string as_window, string as_datawindow, string as_script, integer ai_num_rows_expected_updated);/*

overloaded version of nf_handle_error.

to be called if a specific number of rows is expected to be updated.

arguments:

as_window                    - the name of the window or object containing this function call
as_datawindow                - the name of the datawindow, or text of embedded SQL statement
as_script                    - the name of the event or function containing this function call
ai_num_rows_expected_updated - the specific number of rows is expected to be updated

*/

INT  li_rtn
LONG ll_sqlnrows


ll_sqlnrows = THIS.SQLNRows

IF ll_sqlnrows <> ai_num_rows_expected_updated THEN
	Error.is_database    = THIS.Database
   Error.il_dbcode      = THIS.SQLDBCode
   Error.Number         = THIS.SQLCode
   Error.Text           = 'The number of rows that were expected to be affected by the previous SQL statement was ' +String(ai_num_rows_expected_updated)+'.' + &
                          '~r~nThe actual number of rows affected was ' +String(ll_sqlnrows)+ '.'
   Error.is_type        = 'D'
   Error.windowmenu     = as_window
   Error.object         = as_datawindow
   Error.objectevent    = as_script

	SignalError()
ELSE
	li_rtn = THIS.nf_handle_error(as_window,as_datawindow,as_script)
	RETURN li_rtn
END IF
	




end function

on n_transaction.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_transaction.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

