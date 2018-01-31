$PBExportHeader$u_word.sru
$PBExportComments$User Object that has Ole connections to word
forward
global type u_word from uo_vo_base
end type
end forward

global type u_word from uo_vo_base
end type
global u_word u_word

type variables
OLEObject		i_ooWord

CONSTANT INTEGER OLE_GENERAL_ERROR = -999

end variables

forward prototypes
public function integer uf_disconnect ()
public function integer uf_replace_fields (string as_replacement[50, 2])
public function integer uf_add_bookmark (string as_name)
public function integer uf_add_pic (string as_file_name)
public function integer uf_del_bookmark (string as_bookmark)
public function integer uf_delete ()
public function integer uf_end_of_line ()
public function integer uf_extend (boolean ab_mode)
public function integer uf_extend_line ()
public function integer uf_file_close ()
public function integer uf_file_save (string as_file_name)
public function integer uf_find (string as_text)
public function integer uf_go_to_bookmark (string as_book)
public function integer uf_insert (string as_text)
public function integer uf_insert_before (string as_text)
public function integer uf_list_bookmarks (ref string as_bookmark[])
public function integer uf_new_paragraph (integer al_nbr_paragraphs, integer as_ascii_char)
public function integer uf_print ()
public function integer uf_set_font (string as_font_name, integer ai_font_size)
public function integer uf_type (string as_text)
public function integer uf_visible (boolean ab_visible)
public function integer uf_win_state (string as_win_state)
public function integer uf_homekey ()
public function integer uf_find_replace (string as_find_text, string as_replace_text, boolean ab_direction)
public function integer uf_file_open (string as_file_name, boolean ab_readonly, boolean ab_visible, integer ai_win_state)
public function integer uf_connect ()
public function integer uf_file_save_as (string as_file_path, long al_format_type)
end prototypes

public function integer uf_disconnect ();// quits without saving, closes Word application
INTEGER		li_Return

IF IsValid(i_ooword) THEN
	i_ooWord.Quit(0)
	i_ooWord.DisconnectObject()
	DESTROY i_ooWord
ELSE
	li_Return = -540
END IF

RETURN li_Return

end function

public function integer uf_replace_fields (string as_replacement[50, 2]);INTEGER				li_return
LONG					ll_counter = 2
LONG					ll_TotElements

IF IsValid(i_ooword) THEN
	ll_TotElements = UpperBound(as_replacement[])
	IF ll_TotElements > 0 THEN

		FOR ll_Counter = 1 TO ll_TotElements
			IF (isNull(as_replacement[ll_Counter,1]) or (Trim(as_replacement[ll_Counter,1]) = '') or isNull(Trim(as_replacement[ll_Counter,2])) or (Trim(as_replacement[ll_Counter,2]) = '') or isNull(as_replacement[ll_Counter,3]) or (Trim(as_replacement[ll_Counter,3]) = '')) Then
				// Not A Valid Replacement
			ELSE
				// Go to the bookmark in the word document
				i_ooword.Selection.GoTo(TRUE, 0, 0, as_replacement[ll_Counter,1])
				
				// Replace the text in the word document
				i_ooword.Selection.InsertAfter(as_replacement[ll_Counter,2])
			END IF
		NEXT
	END IF
ELSE
	li_Return = OLE_ERROR
END IF

RETURN li_Return







end function

public function integer uf_add_bookmark (string as_name);// adds bookmark at insertion point in document
INTEGER li_return

IF IsValid(i_ooword) THEN
	i_ooword.ActiveDocument.Bookmarks.Add(as_name)
ELSE
	li_return = OLE_ERROR
END IF

RETURN li_return
end function

public function integer uf_add_pic (string as_file_name);// adds picture file at insertion point in document
INTEGER li_return

IF IsValid(i_ooword) THEN
	i_ooword.activeDocument.InLineShapes.AddPicture(as_file_name,false,true,i_ooword.Selection.Range)
ELSE
	li_return = OLE_ERROR
END IF

RETURN li_return
end function

public function integer uf_del_bookmark (string as_bookmark);// checks if bookmark exists and removes it from Word document
INTEGER li_counter, li_upper, li_return

IF IsValid(i_ooword) THEN
	IF i_ooword.ActiveDocument.Bookmarks.Exists(as_bookmark) THEN
		li_upper = i_ooword.ActiveDocument.Bookmarks.Count
		FOR li_counter = 1 TO li_upper
			IF i_ooword.ActiveDocument.Bookmarks[li_counter].Name = as_bookmark THEN
				exit
			END IF
		NEXT
		i_ooword.ActiveDocument.Bookmarks[li_counter].Delete()
		RETURN 0
	ELSE
		RETURN -1
	END IF
ELSE
	li_return = OLE_ERROR  // OLE Object not valid
END IF

RETURN li_return
end function

public function integer uf_delete ();/* Delete a line from where you established the insertion point - See uf_extend_selection  */
INTEGER li_return

IF IsValid(i_ooword) THEN
	i_ooword.Selection.Range.Delete()
ELSE
	li_return = OLE_ERROR  // OLE Object not valid
END IF

RETURN li_return
end function

public function integer uf_end_of_line ();// sends insertion point to the end of the line
INTEGER li_return

IF IsValid(i_ooword) THEN
	i_ooword.selection.endkey()
	li_return = 0
ELSE
	li_return = OLE_ERROR
END IF

RETURN li_return
end function

public function integer uf_extend (boolean ab_mode);// if set to True, allows selection of text in Word document
INTEGER li_return

IF IsValid(i_ooword) THEN
	i_ooword.Selection.ExtendMode = ab_mode
	li_return = 0
ELSE
	li_return = OLE_ERROR
END IF

RETURN li_return
end function

public function integer uf_extend_line ();///* Highlight the address section  */
INTEGER li_return

IF IsValid(i_ooword) THEN
	i_ooword.Selection.GoTo(TRUE,0,0,"startaddress")
	i_ooword.Selection.ExtendMode = TRUE
	i_ooword.Selection.GoTo(TRUE,0,0,"endaddress")
	li_return = 0
ELSE
	li_return = OLE_ERROR
END IF

RETURN li_return
end function

public function integer uf_file_close ();INTEGER li_return

IF IsValid(i_ooword) THEN
	i_ooWord.ActiveDocument.Close(0)
ELSE
	li_return = OLE_ERROR  // OLE Object not valid
END IF

RETURN li_Return






end function

public function integer uf_file_save (string as_file_name);INTEGER li_return

IF isValid(i_ooword) THEN
	i_ooword.ActiveDocument.Save()
ELSE
	li_return = OLE_ERROR  // OLE Object not valid
END IF

return li_return
end function

public function integer uf_find (string as_text);// find text in Word document
INTEGER li_return

IF IsValid(i_ooword) THEN	
	i_ooword.Selection.Find.Forward=0
	i_ooword.Selection.Find.MatchWholeWord =1
	i_ooword.Selection.Find.MatchCase=1
	i_ooword.Selection.Find.Format=0
	i_ooword.Selection.Find.Text = as_text
	i_ooword.Selection.Find.Execute()
ELSE
	li_return = OLE_ERROR  // OLE Object not valid
END IF

RETURN li_return

end function

public function integer uf_go_to_bookmark (string as_book);// sets insertion point in Word document to bookmark argument, if it exists
INTEGER li_counter, li_upper, li_return
BOOLEAN lb_found

IF IsValid(i_ooword) THEN
	li_upper = i_ooword.ActiveDocument.Bookmarks.Count
ELSE
	li_return = OLE_ERROR  // OLE Object not valid
END IF

If li_upper >= 1 Then
	For li_counter = 1 to li_upper
		IF String(i_ooword.ActiveDocument.Bookmarks[li_counter].Name) = as_book THEN
			lb_found = TRUE
			EXIT
		END IF
	Next
End If

IF lb_found THEN
	i_ooword.Selection.GoTo(TRUE, 0, 0, as_book)
	li_return = 1
END IF

RETURN li_return
end function

public function integer uf_insert (string as_text);/* Add text argument after the selected text  */
INTEGER li_return

IF IsValid(i_ooword) THEN
	i_ooword.Selection.InsertAfter(as_text)
ELSE
	li_return = OLE_ERROR  // OLE Object not valid
END IF

RETURN li_return
end function

public function integer uf_insert_before (string as_text);// add text argument before selected text
INTEGER li_return

IF IsValid(i_ooword) THEN
	i_ooword.Selection.InsertBefore(as_text)
ELSE
	li_return = OLE_ERROR  // OLE Object not valid
END IF

RETURN li_return
end function

public function integer uf_list_bookmarks (ref string as_bookmark[]);int 	li_counter, li_bookmark_count, li_return
/* Retreive the number of bookmarks */

IF IsValid(i_ooword) THEN
	li_bookmark_count = i_ooword.ActiveDocument.Bookmarks.Count
ELSE
	li_return = OLE_ERROR  // OLE Object not valid
END IF

For li_counter = 1 to li_bookmark_count
	as_bookmark[li_counter]=i_ooword.ActiveDocument.Bookmarks[li_counter].Name
Next	

RETURN li_return
end function

public function integer uf_new_paragraph (integer al_nbr_paragraphs, integer as_ascii_char);// insert new line after selected text

INTEGER	li_counter = 1, li_return

STRING	ls_ascii_char

ls_ascii_char = Char(as_ascii_char)

DO
	IF IsValid(i_ooword) THEN
		i_ooword.selection.InsertAfter(ls_ascii_char)
	ELSE
		li_return = OLE_ERROR  // OLE Object not valid
		EXIT
	END IF
	li_counter++
Loop Until li_counter > al_nbr_paragraphs

RETURN li_return
end function

public function integer uf_print ();// prints Word document - printing is not in background to prevent
//                                    Word asking if you want to close doc before it is spooled
INTEGER li_return

IF IsValid(i_ooword) THEN
	i_ooWord.Options.PrintBackground = false
	i_ooword.ActiveDocument.PrintOut
ELSE
	li_return = OLE_ERROR  // OLE Object not valid
END IF

RETURN li_return

end function

public function integer uf_set_font (string as_font_name, integer ai_font_size);/* Set the font size to whatever */
INTEGER li_return

IF IsValid(i_ooword) THEN
	i_ooword.Selection.Font.Name = as_font_name
	i_ooword.Selection.Font.Size = ai_font_size
ELSE
	li_return = OLE_ERROR  // OLE Object not valid
END IF

RETURN li_return
end function

public function integer uf_type (string as_text);// type text at insertion point - if text in document is selected prior to function call,
// then the selected text is replaced.
INTEGER li_return

IF IsValid(i_ooword) THEN
	i_ooword.Selection.TypeText(as_text)
ELSE
	li_return = OLE_ERROR  // OLE Object not valid
END IF

RETURN li_return
end function

public function integer uf_visible (boolean ab_visible);INTEGER li_return

IF IsValid(i_ooword) THEN
	i_ooword.Visible = ab_visible
ELSE
	li_return = OLE_ERROR  // OLE Object not valid
END IF

RETURN li_return
end function

public function integer uf_win_state (string as_win_state);INTEGER li_return

IF IsValid(i_ooword) THEN
	CHOOSE CASE as_win_state
		CASE 'normal'
			i_ooword.ActiveDocument.ActiveWindow.WindowState = 0
		CASE 'max'
			i_ooword.ActiveDocument.ActiveWindow.WindowState = 1	
		CASE 'min'
			i_ooword.ActiveDocument.ActiveWindow.WindowState = 2	
	END CHOOSE
ELSE
	li_return = OLE_ERROR  // OLE Object not valid
END IF

RETURN li_return
end function

public function integer uf_homekey ();INTEGER li_rtn

IF IsValid(i_ooword) THEN
	i_ooword.Selection.HomeKey(6)
ELSE
	li_rtn = OLE_ERROR
END IF

RETURN li_rtn
end function

public function integer uf_find_replace (string as_find_text, string as_replace_text, boolean ab_direction);// Find and replace text in Word document.

boolean lb_return
INTEGER li_return

IF IsValid(i_ooword) THEN	
	lb_return = i_ooword.Selection.Find.Execute(as_find_text  ,false,false,false,false,false,ab_direction,1,false, as_replace_text   ,1)
ELSE
	li_return = OLE_ERROR  // OLE Object not valid
END IF

IF lb_return THEN
	li_return = 1  // Otherwise, if lb_return = false, return 0 (translates to false) or OLE_ERROR (OLE problem)
END IF

RETURN li_return

end function

public function integer uf_file_open (string as_file_name, boolean ab_readonly, boolean ab_visible, integer ai_win_state);/* Open the file to work with if it exists */
INTEGER		li_Return

IF IsValid(i_ooword) THEN
	IF FileExists(as_file_name) THEN
		i_ooWord.Documents.Open(as_file_name,true,ab_readonly)
		i_ooword.ActiveDocument.ActiveWindow.WindowState = ai_win_state  // 0 = normal, 1 = max, 2 = min
		i_ooword.Visible = ab_visible
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
i_ooWord =	CREATE u_word_error_handler

// Connect to Word
li_Return = i_ooWord.ConnectToNewObject('Word.Application')

// Check the Word status code for the connection
IF li_Return <> 0 THEN
	li_Return = -1
END IF

RETURN li_Return
end function

public function integer uf_file_save_as (string as_file_path, long al_format_type);INTEGER			li_return
STRING			ls_reverse_file_path, ls_directory

ls_reverse_file_path = Reverse(as_file_path)
ls_directory = Reverse(Right(ls_reverse_file_path, (Len(ls_reverse_file_path) - Pos(ls_reverse_file_path,'\') ) ) )

IF NOT DirectoryExists(ls_directory) then
	li_return = -400
end if

IF isValid(i_ooword) THEN
	i_ooword.ActiveDocument.SaveAs(as_file_path, al_format_type)  
ELSE
	li_return = OLE_ERROR  // OLE Object not valid
END IF

IF Not FileExists (as_file_path) THEN
	li_return = -400
END IF

return li_return


end function

event destructor;call super::destructor;INT					iReturn

// Destroy the word connection if it exists
If ISVALID(i_ooWord) = True then
	iReturn = uf_disconnect()
END IF
end event

on u_word.create
call super::create
end on

on u_word.destroy
call super::destroy
end on

