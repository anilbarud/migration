$PBExportHeader$n_coolmenu.sru
forward
global type n_coolmenu from nonvisualobject
end type
type s_menudata from structure within n_coolmenu
end type
end forward

type s_menudata from structure
	string		as_toolbaritemname[]
	string		as_menutext[]
	string		as_image[]
	integer		ai_imageid[]
end type

global type n_coolmenu from nonvisualobject autoinstantiate
end type

type prototypes
Function Integer AddImage( Ref String sImage, Long bckColor, Integer xPixel, Integer yPixel ) Library "coolmenu.dll" alias for "AddImage;Ansi"
Function Integer AddMenuImage( Ref String sImage, uLong hIcon ) Library "coolmenu.dll" alias for "AddMenuImage;Ansi"
Function int SetImageNameId( Ref String iName, uInt iImage ) Library "coolmenu.dll" alias for "SetImageNameId;Ansi"
Subroutine InstallCoolMenu( uLong hWnd ) Library "coolmenu.dll"
Subroutine SetNormalStyle() Library "coolmenu.dll"
Subroutine Set2KStyle() Library "coolmenu.dll"
Subroutine SetXpStyle() Library "coolmenu.dll"
Subroutine Set2K3Style() Library "coolmenu.dll"
Subroutine SetXpSelectedColor( Long colorRef ) Library "coolmenu.dll"
Subroutine SetXpMenuColor( Long colorRef ) Library "coolmenu.dll"
Subroutine SetXpBmpBkColor( Long colorRef ) Library "coolmenu.dll"
Subroutine SetXpCheckedSelectedColor( Long colorRef ) Library "coolmenu.dll"
Subroutine SetXpCheckedColor( Long colorRef ) Library "coolmenu.dll"
Subroutine SetXpPenColor( Long colorRef ) Library "coolmenu.dll"
Subroutine SetXpCheckedDisabledColor( Long colorRef ) Library "coolmenu.dll"
Subroutine SetXpTextColor( Long colorRef ) Library "coolmenu.dll"
Subroutine SetXpHighTextColor( Long colorRef ) Library "coolmenu.dll"
Subroutine SetXpDisabledTextColor( Long colorRef ) Library "coolmenu.dll"
Subroutine SetXpDefaults() Library "coolmenu.dll"
Subroutine Set2K3Gradient( Long colorRefStart, Long colorRefEnd ) Library "coolmenu.dll"

Function Long GetSysColor( uInt uIndex ) Library "user32.dll"
end prototypes

type variables
Private:
Constant Integer	SUCCESS = 1
Constant Integer	NO_ACTION = 0
Constant Integer	FAILURE = -1

Constant Integer	STYLE_2K3    = 4
Constant Integer	STYLE_2K     = 2
Constant Integer	STYLE_NORMAL = 1
Constant Integer	STYLE_XP     = 3

Integer		ii_Index, ii_Style
s_menudata	istr_MenuData
Window		iw_Requestor
Boolean		ib_ShowToolbarVisibleOnly

//	Added to set backgroundcolor of bitmap yourself. You can
//	use the Tag property with one of these values.
Constant Integer	COLOR_SCROLLBAR = 0
Constant Integer	COLOR_BACKGROUND = 1
Constant Integer	COLOR_ACTIVECAPTION = 2
Constant Integer	COLOR_INACTIVECAPTION = 3
Constant Integer	COLOR_MENU = 4
Constant Integer	COLOR_WINDOW = 5
Constant Integer	COLOR_WINDOWFRAME = 6
Constant Integer	COLOR_MENUTEXT = 7
Constant Integer	COLOR_WINDOWTEXT = 8
Constant Integer	COLOR_CAPTIONTEXT = 9
Constant Integer	COLOR_ACTIVEBORDER = 10
Constant Integer	COLOR_INACTIVEBORDER = 11
Constant Integer	COLOR_APPWORKSPACE = 12
Constant Integer	COLOR_HIGHLIGHT = 13
Constant Integer	COLOR_HIGHLIGHTTEXT = 14
Constant Integer	COLOR_BTNFACE = 15
Constant Integer	COLOR_BTNSHADOW = 16
Constant Integer	COLOR_GRAYTEXT = 17
Constant Integer	COLOR_BTNTEXT = 18
Constant Integer	COLOR_INACTIVECAPTIONTEXT = 19
Constant Integer	COLOR_BTNHIGHLIGHT = 20
/* win95 colors */
Constant Integer	COLOR_3DDKSHADOW = 21
Constant Integer	COLOR_3DLIGHT = 22
Constant Integer	COLOR_INFOTEXT = 23
Constant Integer	COLOR_INFOBK = 24
Constant Integer	COLOR_DESKTOP = COLOR_BACKGROUND
Constant Integer	COLOR_3DFACE = COLOR_BTNFACE
Constant Integer	COLOR_3DSHADOW = COLOR_BTNSHADOW
Constant Integer	COLOR_3DHIGHLIGHT = COLOR_BTNHIGHLIGHT
Constant Integer	COLOR_3DHILIGHT = COLOR_BTNHIGHLIGHT
Constant Integer	COLOR_BTNHILIGHT = COLOR_BTNHIGHLIGHT
/* win98 colors */
Constant Integer	COLOR_ALTERNATEBTNFACE = 25  /* undocumented, constant's name unknown */
Constant Integer	COLOR_HOTLIGHT = 26
Constant Integer	COLOR_GRADIENTACTIVECAPTION = 27
Constant Integer	COLOR_GRADIENTINACTIVECAPTION = 28

Constant Integer	COLOR_MIN = 0
Constant Integer	COLOR_MAX = 28

end variables

forward prototypes
public function integer of_setrequestor (readonly window aw_requestor)
public subroutine of_setxpselectedcolor (long al_color)
public subroutine of_SetXpMenuColor (long al_color)
public subroutine of_SetXpBmpBkColor (long al_color)
public subroutine of_SetXpCheckedSelectedColor (long al_color)
public subroutine of_SetXpCheckedColor (long al_color)
public subroutine of_SetXpPenColor (long al_color)
public subroutine of_SetXpCheckedDisabledColor (long al_color)
public subroutine of_SetXpHighTextColor (long al_color)
public subroutine of_SetXpDisabledTextColor (long al_color)
public subroutine of_SetXpTextColor (long al_color)
public subroutine of_setxpdefaults ()
public function integer of_setmenu (string as_menu)
private subroutine of_setimagenameid (string as_menuname, string as_image, integer ai_index)
private function boolean of_isloaded (string as_image, string as_menutext, ref integer ai_index)
public function boolean of_addmenubitmap (string as_menutext, unsignedlong aul_icon)
public function integer of_settoolbarvisibleonly (boolean ab_switch)
public function boolean of_gettoolbarvisibleonly ()
public function integer of_getmenustyle ()
public subroutine of_set2kstyle ()
public subroutine of_set2k3style ()
public subroutine of_setxpstyle ()
public subroutine of_setnormalstyle ()
private function integer of_addimage (string as_image, string as_tag)
private function integer of_getmenunames (menu amnu_menu)
public subroutine of_set2k3gradient (long al_colorstart, long al_colorend)
public function integer of_setstyle (integer ai_style)
end prototypes

public function integer of_setrequestor (readonly window aw_requestor);/*
	Function:		of_SetRequestor
	Arguments:		Window	aw_Requestor	The window which requests the coolmenu-function.
														This must be the first window opened by your application,
														e.g. your MDI-frame.
	Description:	Sets the requesting window and installs coolmenu.dll.
											
*/	

iw_Requestor = aw_Requestor

InstallCoolMenu( Handle( iw_Requestor ) )

Return SUCCESS
end function

public subroutine of_setxpselectedcolor (long al_color);SetXpSelectedColor( al_Color )
end subroutine

public subroutine of_SetXpMenuColor (long al_color);SetXpMenuColor( al_Color )
end subroutine

public subroutine of_SetXpBmpBkColor (long al_color);SetXpBmpBkColor( al_Color )
end subroutine

public subroutine of_SetXpCheckedSelectedColor (long al_color);SetXpCheckedSelectedColor( al_Color )
end subroutine

public subroutine of_SetXpCheckedColor (long al_color);SetXpCheckedColor( al_Color )
end subroutine

public subroutine of_SetXpPenColor (long al_color);SetXpPenColor( al_Color )
end subroutine

public subroutine of_SetXpCheckedDisabledColor (long al_color);SetXpCheckedDisabledColor( al_Color )
end subroutine

public subroutine of_SetXpHighTextColor (long al_color);SetXpHighTextColor( al_Color )
end subroutine

public subroutine of_SetXpDisabledTextColor (long al_color);SetXpDisabledTextColor( al_Color )
end subroutine

public subroutine of_SetXpTextColor (long al_color);SetXpTextColor( al_Color )
end subroutine

public subroutine of_setxpdefaults ();SetXpDefaults()

end subroutine

public function integer of_setmenu (string as_menu);/*
	Function:		of_SetMenu
	Arguments:		String	as_Menu	Menu from which you want to see toolbar-bitmaps.
	Description:	
	Usage:			Call this function for every menu with menuitems for which you want to see
						the corresponding bitmaps. If you have a descendant-menu, call of_setmenu just 
						for the descendant, not for the ancestor. If you have more than one descendant,
						just call it for one descendant, unless you added different menuitems to 
						different descendants. In that case, if the added menuitems have bitmaps you
						want to see, you have to call the function for every descendant.
*/

Menu		lmnu_Menu
Boolean	lb_Created

If IsValid( iw_Requestor ) Then
	If iw_Requestor.MenuName <> as_Menu Then
		lmnu_Menu = Create Using as_Menu
		lb_Created = True
	Else
		lmnu_Menu = iw_Requestor.MenuID
	End If
End If

of_GetMenuNames( lmnu_Menu )

If lb_Created Then Destroy lmnu_Menu

Return SUCCESS
end function

private subroutine of_setimagenameid (string as_menuname, string as_image, integer ai_index);/*
	Function:		of_SetImagenameid
	Arguments:		String	as_menuname	Menu from which you want to see toolbar-bitmaps.
						String	as_Image		Image for menuitem
						Integer	ai_Index		Index of image in imagelist within coolmenu.dll
	Description:	
	Usage:			Internal use only.
*/
ii_Index++
istr_MenuData.as_MenuText[ii_Index] = as_MenuName
istr_MenuData.as_toolbaritemname[ii_Index] = as_Image
istr_MenuData.ai_ImageId[ii_Index] = ai_Index
SetImageNameId( as_MenuName, ai_Index )

end subroutine

private function boolean of_isloaded (string as_image, string as_menutext, ref integer ai_index);/*
	Function:		of_IsLoaded
	Arguments:		String	as_MenuText		Text of current menuitem.
						String	as_Image			Image for this menuitem.
						Integer	ai_Index			Index of image if loaded.
	Description:	Sees if the combination menuitem - image is already loaded.
*/	

Integer	li_Loop, li_Il

ai_Index = -1

For li_Loop = 1 To ii_Index
	If istr_MenuData.as_toolbaritemname[li_Loop] = as_Image And &
		istr_MenuData.as_menutext[li_Loop] = as_MenuText Then
		Return True
	End If
Next

For li_Loop = 1 To ii_Index
	If istr_MenuData.as_toolbaritemname[li_Loop] = as_Image Then
		ai_Index = istr_MenuData.ai_imageid[li_Loop]
		Return False
	End If
Next

Return False
end function

public function boolean of_addmenubitmap (string as_menutext, unsignedlong aul_icon);Return ( AddMenuImage( as_menutext, aul_icon ) <> FAILURE )
end function

public function integer of_settoolbarvisibleonly (boolean ab_switch);/*
	Function:		of_ToolbarVisibleOnly
	Arguments:		Boolean	ab_Switch
	Description:	
	Usage:			Call this function if you just want to show bitmaps that are shown
						on the toolbar.
*/
ib_ShowToolbarVisibleOnly = ab_Switch

Return SUCCESS
end function

public function boolean of_gettoolbarvisibleonly ();/*
	Function:		of_GetToolbarVisibleOnly
	Arguments:		None
	Description:	
	Usage:			
*/
Return ib_ShowToolbarVisibleOnly
end function

public function integer of_getmenustyle ();//	Returns 1 for normal, 2 for Office Xp and 3 for Office 2003

Return ii_Style
end function

public subroutine of_set2kstyle ();/*
	Function:		of_SetXpStyle
	Arguments:		Boolean	ab_Switch	Turn Xp-style drawing on or off.
	Description:	
	Usage:			Call this function to enable or disable Office Xp style drawing.
*/

Set2KStyle()

ii_Style = 2
end subroutine

public subroutine of_set2k3style ();/*
	Function:		of_SetXpStyle
	Arguments:		Boolean	ab_Switch	Turn Office 2003 style drawing on or off.
	Description:	
	Usage:			Call this function to enable or disable Office 2003 style drawing.
*/
Set2K3Style()

ii_Style = 4

end subroutine

public subroutine of_setxpstyle ();/*
	Function:		of_SetXpStyle
	Arguments:		Boolean	ab_Switch	Turn Xp-style drawing on or off.
	Description:	
	Usage:			Call this function to enable or disable Office Xp style drawing.
*/

SetXpStyle()

ii_Style = 3

end subroutine

public subroutine of_setnormalstyle ();/*
	Function:		of_SetNormalStyle
	Arguments:		None
	Description:	
	Usage:			Call this function to disable any Office style like drawing.
*/
SetNormalStyle()

ii_Style = 1

end subroutine

private function integer of_addimage (string as_image, string as_tag);/*
	Function:		of_AddImage
	Arguments:		String	as_Image		Image to add.
						String	as_Tag		Tag to define backgroundcolor yourself. Only to use for .bmp files.
													Values:	-1			Don't use any backgroundcolor at all.
																0 - 28	A Windows defined color
																Empty		Let coolmenu determine the color to use.
																x,y		Pixels to use. At default pixels 0, 0 are
																			used (upperleft corner). If for example
																			the upperright corner has to be used to
																			determine the backcolor, set the tag to 15,0
																			(this depends on the size of your bitmap of course).
	Description:	Adds an image to an ImageList (within coolmenu.dll).
	
*/
Long		ll_Null, ll_Color
Integer	li_XPixel, li_YPixel, li_Pos

If IsNumber( as_Tag ) Then
	ll_Color = Long( as_Tag )
	If ll_Color >= COLOR_MIN And ll_Color <= COLOR_MAX Then
		ll_Color = GetSysColor( ll_Color )
	End If
	Return AddImage( as_Image, ll_Color, 0, 0 )
Else
	li_Pos = Pos( as_Tag, "," )
	If li_Pos > 0 Then
		li_XPixel = Integer( Left( as_Tag, li_Pos - 1 ) )
		li_YPixel = Integer( Mid( as_Tag, li_Pos + 1 ) )
	End If
End If

SetNull( ll_Null )

Return AddImage( as_Image, ll_Null, li_XPixel, li_YPixel )

end function

private function integer of_getmenunames (menu amnu_menu);/*
	Function:		of_GetMenuNames
	Arguments:		Menu	amnu_Menu	Menu from which you want to see toolbar-bitmaps.
	Description:	Walks through the given menu (recursive) and sees if an item has
						a toolbaritem (filled ToolbarItemName ). If so, the function AddImage
						is called, which adds it to an ImageList (within coolmenu.dll).
											
*/	

Integer	li_Loop, li_ItemCount, li_Return, li_Index
String	ls_MenuName
Long		ll_Null

li_ItemCount = UpperBound( amnu_menu.Item )

SetNull( ll_Null )

For li_Loop = 1 To li_ItemCount
	ls_MenuName = amnu_menu.Item[li_Loop].Text
	If ls_MenuName = '-' Then Continue
	If amnu_menu.Item[li_Loop].ToolbarItemName <> '' Then
		If ib_ShowToolbarVisibleOnly Then
			If Not amnu_menu.Item[li_Loop].ToolbarItemVisible Then Continue
		End If
		If Not of_IsLoaded( amnu_menu.Item[li_Loop].ToolbarItemName, ls_MenuName, li_Index ) Then
			If li_Index >= 0 Then
				of_SetImageNameId( ls_MenuName, amnu_menu.Item[li_Loop].ToolbarItemName, li_Index )
			Else
				li_Return = of_AddImage( amnu_menu.Item[li_Loop].ToolbarItemName, amnu_menu.Item[li_Loop].Tag )
				If li_Return >= 0 Then
					of_SetImageNameId( ls_MenuName, amnu_menu.Item[li_Loop].ToolbarItemName, li_Return )
				Else
					Continue
				End If
			End If
		End If
	End If
	of_GetMenuNames( amnu_menu.Item[li_Loop] )
Next

Return SUCCESS
end function

public subroutine of_set2k3gradient (long al_colorstart, long al_colorend);Set2K3Gradient( al_ColorStart, al_ColorEnd )
end subroutine

public function integer of_setstyle (integer ai_style);

CHOOSE CASE ai_Style
		
	CASE STYLE_XP
		of_SetXpStyle()
	CASE STYLE_2K3
		of_Set2K3Style()
	CASE STYLE_2K
		of_Set2kStyle()
	CASE STYLE_NORMAL
		of_SetNormalStyle()
	CASE ELSE
		RETURN -1
		
END CHOOSE

RETURN 1
end function

on n_coolmenu.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_coolmenu.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

