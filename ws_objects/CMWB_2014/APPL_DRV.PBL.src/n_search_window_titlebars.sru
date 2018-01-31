$PBExportHeader$n_search_window_titlebars.sru
forward
global type n_search_window_titlebars from nonvisualobject
end type
end forward

global type n_search_window_titlebars from nonvisualobject
end type
global n_search_window_titlebars n_search_window_titlebars

type prototypes
FUNCTION LONG GetDesktopWindow() LIBRARY "user32" alias for "GetDesktopWindow;Ansi"
FUNCTION LONG GetWindow(long hWnd, long uCmd) LIBRARY "user32" alias for "GetWindow;Ansi"
FUNCTION LONG GetClassNameA(long hWnd, Ref String lpClassName, long nMaxCount) LIBRARY "user32" alias for "GetClassNameA;Ansi"
FUNCTION LONG GetWindowTextA(long hWnd, Ref String lpString, long nMaxCount) LIBRARY "user32" alias for "GetWindowTextA;Ansi"

end prototypes

forward prototypes
public function long nf_search_window_titlebars (string as_classname, string as_text_to_find)
end prototypes

public function long nf_search_window_titlebars (string as_classname, string as_text_to_find);// nf_search_window_titlebars - This function searches through all the window titlebars for the string passed to this function.
//                              The handle of the first window that has a title bar that contains the search string is returned. 
//                              The search is not case sensitive. 
//
// Arguments: as_classname - Classname of application your looking for.  Pass empty string to this argument if you want classname to be ignored. 
//            as_text_to_find - Text you are looking for in Title bar
// 
// Returns: ###### - Returns the handle of the window that contains the search string. 
//               0 - if the search string is NOT found in any window title bar
//
// External Functions used: 
//
// GetDesktopWindow() - Retrieves a handle to the desktop window. The desktop window covers the entire screen. 
//                      The desktop window is the area on top of which other windows are painted. 
//                      The return value is a handle to the desktop window.
//
// GetWindow() - Retrieves a handle to a window that has the specified relationship (Z-Order or owner) to the specified window. 
//               If the function succeeds, the return value is a window handle. If no window exists with the specified relationship to the specified window, the return value is NULL.
//
// GetClassNameA() - Retrieves the name of the class to which the specified window belongs. 
//                   If the function succeeds, the return value is the number of characters copied to the buffer, not including the terminating null character.
//                   If the function fails, the return value is zero.
//
// GetWindowTextA - Copies the text of the specified window's title bar (if it has one) into a buffer. If the specified window is a control, 
//                  the text of the control is copied. However, GetWindowText cannot retrieve the text of a control in another application.
//                  If the function succeeds, the return value is the length, in characters, of the copied string, not including the terminating 
//                  null character. If the window has no title bar or text, if the title bar is empty, or if the window or control handle is invalid, the return value is zero.
//
Long     ll_desktop, ll_child, ll_pos
Integer  ll_rtn 
String   ls_class_name, ls_window_name 
Boolean  lb_found 

Constant Long GW_HWNDFIRST = 0
Constant Long GW_HWNDLAST = 1
Constant Long GW_HWNDNEXT = 2
Constant Long GW_HWNDPREV = 3
Constant Long GW_OWNER = 4
Constant Long GW_CHILD = 5
Constant Long GW_MAX = 5
Constant Long MAX_WIDTH = 255

ll_rtn = 0 
lb_found = FALSE

// Validate arguments
IF IsNull(as_text_to_find) = TRUE OR as_text_to_find = "" THEN
	RETURN ll_rtn
END IF

IF as_classname = "" OR IsNull(as_classname) = TRUE THEN
	as_classname = ""
END IF

ll_desktop = GetDesktopWindow()
ll_child = GetWindow(ll_desktop, GW_CHILD) 

DO WHILE (ll_child > 0 AND lb_found = FALSE)
	ls_class_name = Space(MAX_WIDTH)
	ls_window_name = Space(MAX_WIDTH)

	// Get window classname and window text
	GetClassNameA(ll_child, ls_class_name, MAX_WIDTH) 
	GetWindowTextA(ll_child, ls_window_name, MAX_WIDTH) 

	IF IsNull(ls_window_name) = FALSE AND ls_window_name <> "" THEN 
		// Search window name for text to find
		ll_pos = Pos(Upper(ls_window_name), Upper(as_text_to_find)) 
		IF ll_pos > 0 THEN 
			// Don't check classname if it's blank
			IF as_classname = "" THEN
				ll_rtn = ll_child 
				lb_found = TRUE 
			ELSE
				// Check classname 
				IF Upper(as_classname) = Upper(ls_class_name) THEN
					ll_rtn = ll_child 
					lb_found = TRUE 
				END IF
			END IF
		END IF 
	END IF 

	ll_child = GetWindow(ll_child, GW_HWNDNEXT) 
LOOP 

IF lb_found = FALSE THEN
	ll_rtn = 0 
END IF

RETURN ll_rtn

end function

on n_search_window_titlebars.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_search_window_titlebars.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

