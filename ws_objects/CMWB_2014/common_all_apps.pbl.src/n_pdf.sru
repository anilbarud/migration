$PBExportHeader$n_pdf.sru
forward
global type n_pdf from nonvisualobject
end type
end forward

global type n_pdf from nonvisualobject
end type
global n_pdf n_pdf

type prototypes
Function ulong CreateToolhelp32Snapshot (ulong dwFlags, ulong th32ProcessID) Library "KERNEL32.DLL"
Function boolean Process32First (ulong hSnapshot, ref PROCESSENTRY32 lppe) Library "KERNEL32.DLL" alias for "Process32First;Ansi"
Function boolean Process32Next (ulong hSnapshot, ref PROCESSENTRY32 lppe) Library "KERNEL32.DLL" alias for "Process32Next;Ansi"
Function boolean CloseHandle (ref ulong hObject) Library "KERNEL32.DLL"
Function boolean TerminateProcess (ulong hProcess, uint uExitCode) LIBRARY "KERNEL32.DLL"
Function boolean GetExitCodeProcess (ulong hProcess, Ref uint lpExitCode) LIBRARY "KERNEL32.DLL"
Function ulong OpenProcess(ulong dwdesiredaccess, boolean binheritHandle,ulong dwprocessid) Library "kernel32.dll"
Function long GetLastError() Library "kernel32.dll"
Function Boolean EnumProcesses(REF processEntry Process, long cb, REF long cbNeeded ) Library "PSAPI.DLL" alias for "EnumProcesses;Ansi"
Function boolean EnumProcessModules( ulong hProcess, REF ModuleEntry Module, long cb, REF long lpcbNeeded ) LIBRARY "PSAPI.DLL" alias for "EnumProcessModules;Ansi"
Function long GetModuleBaseNameA(ulong hProcess, ulong hModule, REF string lpBaseName,long nSize) LIBRARY "PSAPI.DLL" alias for "GetModuleBaseNameA;Ansi"
Function long GetModuleFileNameExA(ulong hProcess, ulong hModule, REF string lpBaseName,long nSize) LIBRARY "PSAPI.DLL" alias for "GetModuleFileNameExA;Ansi"

end prototypes

type variables


// for potential future use, listed all
private:
PROCESSENTRY32 ipe_processentry[]
constant ulong TH32CS_SNAPHEAPLIST = 1 //0x00000001
constant ulong TH32CS_SNAPPROCESS  = 2 //0x00000002
constant ulong TH32CS_SNAPTHREAD   = 4 //0x00000004
constant ulong TH32CS_SNAPMODULE   = 8 //0x00000008
constant ulong TH32CS_SNAPALL      = TH32CS_SNAPHEAPLIST + TH32CS_SNAPPROCESS + TH32CS_SNAPTHREAD + TH32CS_SNAPMODULE
constant ulong TH32CS_INHERIT      = 2147483648 //0x80000000
constant ulong  PROCESS_TERMINATE = 1
constant ulong  PROCESS_CREATE_THREAD = 2
constant ulong  PROCESS_SET_SESSIONID = 4
constant ulong  PROCESS_VM_OPERATION = 8
constant ulong  PROCESS_VM_READ = 16
constant ulong  PROCESS_VM_WRITE = 32
constant ulong  PROCESS_DUP_HANDLE = 64
constant ulong  PROCESS_CREATE_PROCESS = 128
constant ulong  PROCESS_SET_QUOTA = 256
constant ulong  PROCESS_SET_INFORMATION = 512
constant ulong  PROCESS_QUERY_INFORMATION = 1024
constant ulong  PROCESS_ALL_ACCESS = 2035711


end variables

forward prototypes
public function integer nf_open_pdf_file (string as_adobe_path, string as_document_path)
public function integer nf_close_pdf_files ()
public subroutine nf_set_adobe_path (ref string as_adobe_path)
public subroutine nf_print_pdf (string as_doc_path)
end prototypes

public function integer nf_open_pdf_file (string as_adobe_path, string as_document_path);INTEGER   li_rtn
STRING    ls_run_command


IF as_adobe_path <> '' THEN
	IF FileExists(as_document_path) THEN
		// OK
	ELSE
		MessageBox('Missing File','The document could not be found. Please contact the HELPDESK.',Exclamation!)
		RETURN -1
	END IF
	
	// to the executeable path, add the path to the document
	ls_run_command = as_adobe_path + ' "' + as_document_path + '"'
	
	// run the adobe reader command line
	li_rtn = Run(ls_run_command)
	IF li_rtn <> 1 THEN
		MessageBox('File Open Error','An error occurred opening this document. Please contact the HELPDESK.',Exclamation!)
	END IF
ELSE
	MessageBox('Registry Error','The location of the Abobe Reader application could not be found. Please contact the HELPDESK.',Exclamation!)
END IF

RETURN 0
end function

public function integer nf_close_pdf_files ();


// This function will shut down all Word processes.
// It searches structure ModuleEntry for those with winword.exe as part of file path
// and closes them.

ProcessEntry lpe_Process         // Window structure (array of 500 ulong)...arbitrary value !
ModuleEntry  lme_Module          // Window structure (array of 100 long)... idem
LONG         ll_proc_size = 2000 // Size of Process
LONG         ll_mod_size = 400   // Size of Module
LONG         ll_proc_needed      // returned size of Process
LONG         ll_mod_needed       // returned size of Module
LONG         ll_mod_name         // size of returned name of the module
LONG         ll_counter      // for looping through processes
ULONG        lul_proc_handle     // Handle of the process
BOOLEAN      lb_proc, lb_mod     // return codes for EnumProcesses and EnumProcessModules
STRING       ls_mod_name         // Name of the module
STRING       ls_mod_base_name, ls_adobe_exe, ls_reverse, ls_adobe_exe_filename, ls_lower_mod_name
LONG         ll_mod_base_name, ll_count, ll_pos
UINT     	  lui_exitcode
ANY          la_external_return



// get adobe reader exe's filename, look for process with this name & then terminate it
nf_set_adobe_path(ls_adobe_exe)

// the returned string has quote marks, so strip them off
ls_adobe_exe = Left(ls_adobe_exe,Len(ls_adobe_exe) - 1)
ls_adobe_exe = Right(ls_adobe_exe,Len(ls_adobe_exe) - 1)

ls_reverse = Reverse(ls_adobe_exe)
ll_pos = Pos(ls_reverse,'\')				
ls_adobe_exe_filename = Lower(Reverse(Left(ls_reverse,ll_pos - 1)))


lb_proc = EnumProcesses(lpe_Process, ll_proc_size, ll_proc_needed)
if lb_proc then
	ll_count = integer(ll_proc_needed/ 4)
	for ll_counter = 1 to ll_count
		lul_proc_Handle = OpenProcess(PROCESS_ALL_ACCESS,false,lpe_Process.lpIdProcess[ll_counter])
		lb_mod = EnumProcessModules(lul_proc_handle, lme_Module, ll_mod_size, ll_mod_needed)
		if ll_mod_needed >= 4 then
			ls_mod_name = space(254)
			ll_mod_Name=GetModuleFileNameExA(lul_proc_handle, lme_Module.lpidmodule[1], ls_mod_name ,254)
			if ll_mod_Name > 0 then
				ls_lower_mod_name = Lower(String(ls_mod_name))
				if Pos(ls_lower_mod_name, ls_adobe_exe_filename) <> 0 THEN
					la_external_return = GetExitCodeProcess(lul_proc_handle, lui_exitcode)
					if TerminateProcess(lul_proc_handle, lui_exitcode) then
						 //OK
					else
//						MessageBox('Process Termination',wf_error(GetLastError()),StopSign!)
					end if
				end if
			end if
		end if
		CloseHandle(lul_proc_Handle)
	next
end if

return 0
end function

public subroutine nf_set_adobe_path (ref string as_adobe_path);STRING  ls_registry_path


ls_registry_path = 'HKEY_CLASSES_ROOT\Software\Adobe\Acrobat\exe'

// get the registry key that has the path to the executeable that will open a PDF file
RegistryGet(ls_registry_path, '', RegString!, as_adobe_path)
end subroutine

public subroutine nf_print_pdf (string as_doc_path);// Print a provided pdf doc to the default printer
STRING ls_adobe_exe_path


// get path to adobe reader exe
nf_set_adobe_path(ls_adobe_exe_path)
RUN(ls_adobe_exe_path + ' /N /T ' + as_doc_path)

end subroutine

on n_pdf.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_pdf.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

