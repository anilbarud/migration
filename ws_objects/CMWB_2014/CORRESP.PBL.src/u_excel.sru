$PBExportHeader$u_excel.sru
$PBExportComments$User Object that has Ole connections to word
forward
global type u_excel from uo_vo_base
end type
end forward

global type u_excel from uo_vo_base
end type
global u_excel u_excel

type variables
OLEObject i_ooExcel

CONSTANT INTEGER OLE_GENERAL_ERROR = -999

end variables

forward prototypes
public function integer uf_disconnect ()
public function integer uf_file_close ()
public function integer uf_print ()
public function integer uf_file_open (string as_file_name, boolean ab_readonly, boolean ab_visible, integer ai_win_state)
public function integer uf_connect ()
end prototypes

public function integer uf_disconnect ();// quits without saving, closes Word application
INTEGER		li_Return

IF IsValid(i_ooExcel) THEN
	i_ooExcel.Quit(0)
	i_ooExcel.DisconnectObject()
	DESTROY i_ooExcel 
ELSE
	li_Return = -540
END IF

RETURN li_Return

end function

public function integer uf_file_close ();INTEGER li_return

IF IsValid(i_ooExcel) THEN
	i_ooExcel.ActiveWorkBook.Close(0)
ELSE
	li_return = -1
END IF

RETURN li_Return






end function

public function integer uf_print ();// prints Excel document - printing is not in background to prevent Excel asking if you want to close doc before it is spooled
INTEGER li_return

IF IsValid(i_ooExcel) THEN
	i_ooExcel.ActiveWorkBook.PrintOut
ELSE
	li_return = OLE_ERROR  // OLE Object not valid
END IF

RETURN li_return

end function

public function integer uf_file_open (string as_file_name, boolean ab_readonly, boolean ab_visible, integer ai_win_state);// uf_file_open
//
// Description: Open the file to work with if it exists.
//              The line 'i_ooExcel.WorkBooks.Application.ActiveWindow.WindowState = 2' maximizes the 
//              current sheet so that the filename will appear in the main title bar for Excel. 
//              When workbench closes it searches the main titlebars for doc id's, it it finds 
//              one then it's closes the document or spreadsheet. 
//
INTEGER li_Return

IF IsValid(i_ooExcel) THEN
	IF FileExists(as_file_name) THEN
		i_ooExcel.Workbooks.Open(as_file_name, 3, ab_readonly)  // file name, update links, open readonly
		i_ooExcel.WorkBooks.Application.ActiveWindow.WindowState = 2  // Maximize the active sheet in Excel so filename (containing docid) will appear in main titlebar so that it can be found
		i_ooExcel.Visible = ab_visible
	ELSE
		li_Return = -1
	END IF
ELSE
	li_Return = OLE_ERROR
END IF

RETURN li_Return

end function

public function integer uf_connect ();INTEGER li_return

// Create an instance link to Ms-Word
i_ooExcel = CREATE u_excel_error_handler

// Connect to Word
li_Return = i_ooExcel.ConnectToNewObject('Excel.Application')

// Check the Word status code for the connection
IF li_Return <> 0 THEN
	li_Return = -1
END IF

RETURN li_Return
end function

event destructor;call super::destructor;INT iReturn

// Destroy the word connection if it exists
If ISVALID(i_ooExcel) = True then
	iReturn = uf_disconnect()
END IF
end event

on u_excel.create
call super::create
end on

on u_excel.destroy
call super::destroy
end on

