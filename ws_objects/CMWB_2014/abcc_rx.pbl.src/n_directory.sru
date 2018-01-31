$PBExportHeader$n_directory.sru
forward
global type n_directory from nonvisualobject
end type
type os_filedatetime from structure within n_directory
end type
type os_finddata from structure within n_directory
end type
type os_securityattributes from structure within n_directory
end type
end forward

type os_filedatetime from structure
	ulong		ul_lowdatetime
	ulong		ul_highdatetime
end type

type os_finddata from structure
	ulong		ul_fileattributes
	os_filedatetime		str_creationtime
	os_filedatetime		str_lastaccesstime
	os_filedatetime		str_lastwritetime
	ulong		ul_filesizehigh
	ulong		ul_filesizelow
	ulong		ul_reserved0
	ulong		ul_reserved1
	character		ch_filename[260]
	character		ch_alternatefilename[14]
end type

type os_securityattributes from structure
	ulong		ul_length
	string		ch_description
	boolean		b_inherit
end type

global type n_directory from nonvisualobject
end type
global n_directory n_directory

type prototypes
Function ulong GetDriveTypeA (string drive) library "KERNEL32.DLL" alias for "GetDriveTypeA;Ansi"
Function boolean CreateDirectoryA (ref string directoryname, ref os_securityattributes secattr) library "KERNEL32.DLL" alias for "CreateDirectoryA;Ansi"
Function boolean RemoveDirectoryA (ref string directoryname) library "KERNEL32.DLL" alias for "RemoveDirectoryA;Ansi"
Function ulong GetCurrentDirectoryA (ulong textlen, ref string dirtext) library "KERNEL32.DLL" alias for "GetCurrentDirectoryA;Ansi"
Function boolean SetCurrentDirectoryA (ref string directoryname ) library "KERNEL32.DLL" alias for "SetCurrentDirectoryA;Ansi"
Function ulong GetFileAttributesA (ref string filename) library "KERNEL32.DLL" alias for "GetFileAttributesA;Ansi"
Function boolean SetFileAttributesA (ref string filename, ulong attrib) library "KERNEL32.DLL" alias for "SetFileAttributesA;Ansi"
Function boolean MoveFileA (ref string oldfile, ref string newfile) library "KERNEL32.DLL" alias for "MoveFileA;Ansi"
Function long FindFirstFileA (ref string filename, ref os_finddata findfiledata) library "KERNEL32.DLL" alias for "FindFirstFileA;Ansi"
Function boolean FindNextFileA (ulong handle, ref os_finddata findfiledata) library "KERNEL32.DLL" alias for "FindNextFileA;Ansi"
Function boolean FindClose (ulong handle) library "KERNEL32.DLL"
Function boolean GetDiskFreeSpaceA (string drive, ref long sectpercluster, ref long bytespersect, ref long freeclusters, ref long totalclusters) library "KERNEL32.DLL" alias for "GetDiskFreeSpaceA;Ansi"
Function ulong GetLastError() library "KERNEL32.DLL"

end prototypes

forward prototypes
public function integer nf_dirlist (string as_path, string as_file_search, long ai_file_type, boolean ab_subdirectory, ref string as_files[])
end prototypes

public function integer nf_dirlist (string as_path, string as_file_search, long ai_file_type, boolean ab_subdirectory, ref string as_files[]);BOOLEAN	lb_Found
INTEGER	li_Cnt, li_Entries
LONG		ll_Handle, ll_count
TIME		lt_Time
STRING	ls_full_filename, ls_files_in_sub_directory[]					
STRING	ls_file_name,ls_sub_directory_path 
CONSTANT UNSIGNEDINTEGER		il_bit = 5
os_finddata				lstr_FindData

ls_full_filename = as_path + "*.*"

IF as_file_search <> "" THEN
	IF len(as_file_search) <> 3 THEN RETURN -1
END IF 

// List the entries in the directory
ll_Handle = FindFirstFileA(ls_full_filename, lstr_FindData)
IF ll_Handle <= 0 THEN RETURN -1
Do	
	//If it's not a subdirectory
	IF lstr_FindData.ch_filename[1] <> "." THEN
		IF Int(Mod(lstr_FindData.ul_FileAttributes / (2 ^(il_bit - 1)), 2)) =  0 THEN
			IF as_file_search = "" OR Right(lstr_FindData.ch_filename,3) = as_file_search THEN
				li_Entries ++
				as_files[li_entries] = as_path + lstr_FindData.ch_filename				
			END IF
		ELSEIF Int(Mod(lstr_FindData.ul_FileAttributes / (2 ^(il_bit - 1)), 2)) >  0 THEN//If it is a subdirectory
			IF ab_subdirectory = TRUE THEN
				ls_sub_directory_path = as_path + lstr_FindData.ch_filename + "\"
				nf_dirlist(ls_sub_directory_path ,as_file_search,ai_file_type,ab_subdirectory,ls_files_in_sub_directory)
				FOR ll_count = 1 TO UpperBOUND(ls_files_in_sub_directory)
					li_Entries++
					as_files[li_entries] = ls_files_in_sub_directory[ll_count]
				NEXT
			END IF
		END IF
	END IF
	lb_Found = FindNextFileA(ll_Handle, lstr_FindData)
LOOP UNTIL NOT lb_Found

FindClose(ll_Handle)

RETURN li_Entries


end function

on n_directory.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_directory.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

