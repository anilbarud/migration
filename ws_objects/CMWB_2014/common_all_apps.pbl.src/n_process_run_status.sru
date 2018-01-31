$PBExportHeader$n_process_run_status.sru
forward
global type n_process_run_status from nonvisualobject
end type
end forward

global type n_process_run_status from nonvisualobject
end type
global n_process_run_status n_process_run_status

forward prototypes
public function integer nf_in_progress (string as_module_code, string as_module_in_progress_code, string as_operation, ref n_transaction an_transaction_object)
public function integer nf_in_progress (n_transaction an_transaction, string as_module_in_progress_code, string as_title, string as_message)
end prototypes

public function integer nf_in_progress (string as_module_code, string as_module_in_progress_code, string as_operation, ref n_transaction an_transaction_object);STRING	ls_in_progress_flag, ls_module_in_progress_desc, ls_module_desc, ls_message, ls_title

/*
There are two versions of this function:
1 - this generic one, where programmer specifies the module code of the 
blocking module and the module code of the module being blocked, and the
operation that is being blocked
2 - a more generic overloaded version
*/
/*
SELECT	in_progress_flag
INTO		:ls_in_progress_flag
FROM		PROCESS_RUN_STATUS
WHERE	module_code = :as_module_in_progress_code
USING an_transaction_object;

an_transaction_object.nf_handle_error('n_process_run_status', 'nf_in_progress', 'select in_progress_flag from PAYMENT_PROCESS_RUN_STATUS')

IF ls_in_progress_flag = 'Y' THEN
	SELECT	module_desc
	INTO		:ls_module_in_progress_desc
	FROM		Module
	WHERE	module_code = :as_module_in_progress_code
	USING an_transaction_object;
	
	an_transaction_object.nf_handle_error('n_process_run_status', 'nf_in_progress', 'SELECT module_desc FROM Module (1)')
	
	SELECT	module_desc
	INTO		:ls_module_desc
	FROM		Module
	WHERE	module_code = :as_module_code
	USING an_transaction_object;
	
	an_transaction_object.nf_handle_error('n_process_run_status', 'nf_in_progress', 'SELECT module_desc FROM Module (2)')
	
	ls_title = ls_module_in_progress_desc + ' is running.'
	ls_message = 'The ' + as_operation + ' for the ' + ls_module_desc + ' module cannot be completed at this time.~r~n'&
					+'Please wait 30 seconds and try again.'
					
	MessageBox(ls_title, ls_message, Information!)
	RETURN 1
END IF
*/
RETURN 0
end function

public function integer nf_in_progress (n_transaction an_transaction, string as_module_in_progress_code, string as_title, string as_message);STRING	ls_in_progress_flag

/*
There are two versions of this function:
1 - this generic one, where programmer specifies the messagebox
title and messagebox message
2 - overloaded version
*/
/*
SELECT	in_progress_flag
INTO		:ls_in_progress_flag
FROM		PROCESS_RUN_STATUS
WHERE	module_code = :as_module_in_progress_code
USING an_transaction_object;

an_transaction_object.nf_handle_error('n_process_run_status', 'nf_in_progress', 'select in_progress_flag from PAYMENT_PROCESS_RUN_STATUS')

IF ls_in_progress_flag = 'Y' THEN
	MessageBox(as_title, as_message, Information!)
	RETURN 1
END IF
*/
RETURN 0
end function

on n_process_run_status.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_process_run_status.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

