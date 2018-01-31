$PBExportHeader$u_top_bar.sru
$PBExportComments$User Object - Folder Control
forward
global type u_top_bar from userobject
end type
type pb_1 from picturebutton within u_top_bar
end type
type uo_5 from u_top_tab within u_top_bar
end type
type uo_4 from u_top_tab within u_top_bar
end type
type uo_3 from u_top_tab within u_top_bar
end type
type uo_2 from u_top_tab within u_top_bar
end type
type uo_1 from u_top_tab within u_top_bar
end type
end forward

global type u_top_bar from userobject
integer width = 2587
integer height = 1052
boolean border = true
long backcolor = 67108864
event ue_bar_clicked pbm_custom01
pb_1 pb_1
uo_5 uo_5
uo_4 uo_4
uo_3 uo_3
uo_2 uo_2
uo_1 uo_1
end type
global u_top_bar u_top_bar

type variables
//
//      These variables are used to keep track of
//      which tab has focus and which tab is losing focus

Integer          ii_lastindex, ii_index
end variables

forward prototypes
public subroutine uf_settabs (string asa_tabnames[5])
public function userobject uf_getobject (integer ai_index)
public subroutine uf_display_tab (integer ai_index)
public subroutine uf_getindex (ref integer ai_index)
public subroutine uf_setindex (integer ai_index)
end prototypes

on ue_bar_clicked;//	create reference variable for the individual tabs

u_top_tab vlu_curtab, vlu_lasttab

//	get the last tab and the current tab

vlu_curtab	=	uf_getobject (ii_index)
vlu_lasttab = uf_getobject  (ii_lastindex)


//	set the last tab to normal

vlu_lasttab.p_tab.picturename = "tab_down.bmp"

//	set the current tab to bold

vlu_curtab.p_tab.picturename = "tab_up.bmp"


end on

public subroutine uf_settabs (string asa_tabnames[5]);//
// This function is used to set the number of tabs and the text into the tabs from the window using the tab control
//

//	declare variables for counters

integer	vli,	vli_numberoftabs

//	declare variable to be a reference to the user object individual tabs

u_top_tab	vlu_tab



vli_numberoftabs = UpperBound (asa_tabnames)

//	loop through array
//	set the static text display in the tabs

For vli = 1 to vli_numberoftabs

	//	get a reference into the local variable

	vlu_tab = uf_getobject (vli)

	// update the static text object in the tab object

	vlu_tab.st_tab.text = asa_tabnames [vli]

Next
end subroutine

public function userobject uf_getobject (integer ai_index);//
//	This function is designed to accept an integer as an argument for use as an 
//	index into the tab bar.  It returns a reference to a user object of type u_top_tab
//

u_top_tab	vlu_retval

choose case ai_index
	case 1
		vlu_retval = uo_1
	case 2
		vlu_retval = uo_2
	case 3
		vlu_retval = uo_3
	case 4
		vlu_retval = uo_4
	case 5
		vlu_retval = uo_5
end choose

return vlu_retval
end function

public subroutine uf_display_tab (integer ai_index);/* ----------------------------------------------------------------------------------------------------	*/
/*	Function Name: 	uf_display_tab																								*/
/*																																			*/
/*	Purpose:				This function will bring a specified tab to the front and move the current tab back.*/
/*																																			*/
/*																																			*/
/*	Arguments:			Integer	-	ai_index	-	The tab to move to the front											*/
/*																																			*/
/*																																			*/
/*																																			*/
/*	Return Value:		None																											*/
/*																																			*/
/* ----------------------------------------------------------------------------------------------------	*/


//	Ensure that a valid tab has been passed, and check to see if the desired tab
//	is already being displayed.

	If ai_index < 1 or ai_index > 5 Then
		Return
	End If

	If ai_index = ii_index Then
		Return
	End If



//	Create reference variable for the individual tab and trigger it's clicked event

	u_top_tab vlu_newtab

	vlu_newtab = uf_getobject(ai_index)
	vlu_newtab.TriggerEvent("ue_clicked")

	Return
end subroutine

public subroutine uf_getindex (ref integer ai_index);// return index
ai_index = ii_index
end subroutine

public subroutine uf_setindex (integer ai_index);CHOOSE CASE ai_index
	CASE 1
		uo_1.TriggerEvent('ue_clicked')
	CASE 2
		uo_2.TriggerEvent('ue_clicked')
	CASE 3
		uo_3.TriggerEvent('ue_clicked')
	CASE 4
		uo_4.TriggerEvent('ue_clicked')
	CASE 5
		uo_5.TriggerEvent('ue_clicked')
END CHOOSE
end subroutine

on constructor;//	set the index instance variable to the first tab

ii_index = 1

//	update the instance variable in all the u_top_tab objects.  This
//	variable will be used as a tag to figure out which tab has been 
//	clicked in the ue_bar_clicked event

uo_1.ii_tab_index = 1
uo_2.ii_tab_index = 2
uo_3.ii_tab_index = 3
uo_4.ii_tab_index = 4
uo_5.ii_tab_index = 5

//	make the first tab visually current

uo_1.p_tab.picturename = "tab_up.bmp"
end on

on u_top_bar.create
this.pb_1=create pb_1
this.uo_5=create uo_5
this.uo_4=create uo_4
this.uo_3=create uo_3
this.uo_2=create uo_2
this.uo_1=create uo_1
this.Control[]={this.pb_1,&
this.uo_5,&
this.uo_4,&
this.uo_3,&
this.uo_2,&
this.uo_1}
end on

on u_top_bar.destroy
destroy(this.pb_1)
destroy(this.uo_5)
destroy(this.uo_4)
destroy(this.uo_3)
destroy(this.uo_2)
destroy(this.uo_1)
end on

type pb_1 from picturebutton within u_top_bar
integer x = 9
integer y = 116
integer width = 2542
integer height = 844
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
boolean originalsize = true
alignment htextalign = left!
end type

type uo_5 from u_top_tab within u_top_bar
integer x = 2021
integer width = 507
integer height = 124
integer taborder = 50
boolean border = false
end type

on uo_5.destroy
call u_top_tab::destroy
end on

type uo_4 from u_top_tab within u_top_bar
integer x = 1522
integer width = 507
integer height = 124
integer taborder = 40
boolean border = false
end type

on uo_4.destroy
call u_top_tab::destroy
end on

type uo_3 from u_top_tab within u_top_bar
integer x = 1024
integer width = 507
integer height = 124
integer taborder = 30
boolean border = false
end type

on uo_3.destroy
call u_top_tab::destroy
end on

type uo_2 from u_top_tab within u_top_bar
integer x = 526
integer width = 507
integer height = 124
integer taborder = 20
boolean border = false
end type

on uo_2.destroy
call u_top_tab::destroy
end on

type uo_1 from u_top_tab within u_top_bar
integer x = 27
integer taborder = 10
boolean border = false
end type

on uo_1.destroy
call u_top_tab::destroy
end on

