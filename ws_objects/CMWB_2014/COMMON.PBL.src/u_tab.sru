$PBExportHeader$u_tab.sru
$PBExportComments$Departmental Layer for
forward
global type u_tab from tab
end type
end forward

global type u_tab from tab
integer width = 896
integer height = 612
integer taborder = 1
integer textsize = -8
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 67108864
boolean raggedright = true
boolean showpicture = false
integer selectedtab = 1
event resize pbm_size
event type integer pfc_getindex ( string as_pagename )
event type any ue_getitem ( string as_pagename,  string as_dwname,  long al_row,  string as_colname )
end type
global u_tab u_tab

type variables
boolean ib_backing_out
boolean ib_lockpage
integer   ii_demandpage
integer   ii_responsepage

Public:
// - Common return value constants:
constant integer 		SUCCESS = 1
constant integer 		FAILURE = -1
constant integer 		NO_ACTION = 0
// - Continue/Prevent return value constants:
constant integer 		CONTINUE_ACTION = 1
constant integer 		PREVENT_ACTION = 0
n_resize	                 inv_resize

Protected:
// Logical Unit of Work -  SelfUpdatingObject - Save Process - (Attributes).
boolean		ib_isupdateable = True
boolean		ib_alwaysvalidate = false // Save process flag to include all objects in validation process.
powerobject	ipo_updaterequestor
powerobject	ipo_pendingupdates[]
powerobject	ipo_updateobjects[]


end variables

forward prototypes
public function integer of_getparentwindow (ref window aw_parent)
public function integer of_setresize (boolean ab_switch)
public function powerobject of_getpage (string as_pagename)
protected function window of_getparentwindow ()
protected function integer of_initpages ()
end prototypes

event resize;//////////////////////////////////////////////////////////////////////////////
//
//	Event:  resize
//
//	Description:
//	Send resize notification to services
//
//////////////////////////////////////////////////////////////////////////////
//	
//	Revision History
//
//	Version
//	5.0   Initial version
//
//////////////////////////////////////////////////////////////////////////////
//
//	Copyright © 1996-1997 Sybase, Inc. and its subsidiaries.  All rights reserved.
//	Any distribution of the PowerBuilder Foundation Classes (PFC)
//	source code by other than Sybase, Inc. and its subsidiaries is prohibited.
//
//////////////////////////////////////////////////////////////////////////////

// Notify the resize service that the object size has changed.
//If IsValid (inv_resize) Then
//	inv_resize.Event pfc_Resize (sizetype, This.Width, This.Height)
//End If
end event

event type integer pfc_getindex(string as_pagename);integer ctr = 1, rtn = -1

do while ctr <= upperbound(this.control[],1)
	if Upper(this.control[ctr].classname()) = Upper(as_pagename) then
		rtn = ctr
	end if
	ctr++
loop

return rtn

end event

event type any ue_getitem(string as_pagename, string as_dwname, long al_row, string as_colname);integer ctr = 1, ctr2 = 1
any la_rtn
setnull(la_rtn)

do while ctr <= upperbound(this.control[],1)
	if upper(this.control[ctr].classname()) = upper(as_pagename) then
		do while ctr2 <= upperbound(this.control[ctr].control[],1)
			if upper(this.control[ctr].control[ctr2].classname()) = upper(as_dwname) then
				if al_row = 0 then al_row = this.control[ctr].control[ctr2].dynamic getrow()

				datawindow ldw_1
				ldw_1 = this.control[ctr].control[ctr2]
				choose case ldw_1. Describe(as_colname + ".Coltype")
				case "string"
					la_rtn = ldw_1.getitemstring(al_row,as_colname)
				case "number", "long", "ulong"
					la_rtn = ldw_1.getitemnumber(al_row,as_colname)
				case "datetime"
					la_rtn = ldw_1.getitemdatetime(al_row,as_colname)
				case "decimal"
					la_rtn = ldw_1.getitemdecimal(al_row,as_colname)
				case "date"
					la_rtn = ldw_1.getitemdate(al_row,as_colname)
				case "time"
					la_rtn = ldw_1.getitemtime(al_row,as_colname)
				end choose
				
				ctr2 = upperbound(this.control[ctr].control[],1)
			end if
			ctr2++
		loop
		ctr = upperbound(this.control[],1)
	end if
	ctr++
loop

return la_rtn
end event

public function integer of_getparentwindow (ref window aw_parent);//////////////////////////////////////////////////////////////////////////////
//
//	Function:  of_GetParentWindow
//
//	Access:  public
//
//	Arguments:
//	aw_parent   The Parent window for this object (passed by reference).
//	   If a parent window is not found, aw_parent is NULL
//
//	Returns:  integer
//	 1 = success
//	-1 = error
//
//	Description:	 Calculates the parent window of a window object
//
//////////////////////////////////////////////////////////////////////////////
//
//	Revision History
//
//	Version
//	5.0   Initial version
//
//////////////////////////////////////////////////////////////////////////////
//
//	Copyright © 1996-1997 Sybase, Inc. and its subsidiaries.  All rights reserved.
//	Any distribution of the PowerBuilder Foundation Classes (PFC)
//	source code by other than Sybase, Inc. and its subsidiaries is prohibited.
//
//////////////////////////////////////////////////////////////////////////////

powerobject	lpo_parent

lpo_parent = this.GetParent()

// Loop getting the parent of the object until it is of type window!
do while IsValid (lpo_parent) 
	if lpo_parent.TypeOf() <> window! then
		lpo_parent = lpo_parent.GetParent()
	else
		exit
	end if
loop

if IsNull(lpo_parent) Or not IsValid (lpo_parent) then
	setnull(aw_parent)	
	return -1
end If

aw_parent = lpo_parent
return 1

end function

public function integer of_setresize (boolean ab_switch);//////////////////////////////////////////////////////////////////////////////
//
//	Function:  of_SetResize
//
//	Access:  public
//
//	Arguments:		
//	ab_switch   starts/stops the window resize service
//
//	Returns:  integer
//	 1 = success
//	 0 = no action necessary
//	-1 = error
//
//	Description:
//	Starts or stops the window resize service
//
//////////////////////////////////////////////////////////////////////////////
//
//	Revision History
//
//	Version
//	5.0   Initial version
//
//////////////////////////////////////////////////////////////////////////////
//
//	Copyright © 1996-1997 Sybase, Inc. and its subsidiaries.  All rights reserved.
//	Any distribution of the PowerBuilder Foundation Classes (PFC)
//	source code by other than Sybase, Inc. and its subsidiaries is prohibited.
//
//////////////////////////////////////////////////////////////////////////////

Integer	li_rc

// Check arguments
If IsNull (ab_switch) Then
	Return -1
End If

If ab_Switch Then
	If IsNull(inv_resize) Or Not IsValid (inv_resize) Then
		inv_resize = Create n_resize
		inv_resize.of_SetOrigSize (This.Width, This.Height)
		li_rc = 1
	End If
Else
	If IsValid (inv_resize) Then
		Destroy inv_resize
		li_rc = 1
	End If
End If

Return li_rc

end function

public function powerobject of_getpage (string as_pagename);integer ctr = 1
powerobject ltp_target
string test

do while ctr <= upperbound(this.control[],1)
	ltp_target = this.control[ctr]
	test = ltp_target.classname()
	if Upper(test) = Upper(as_pagename) then
		return ltp_target	
	end if
	ctr++
loop

setnull(ltp_target)
return ltp_target

end function

protected function window of_getparentwindow ();powerobject	lpo_parent

lpo_parent = this.GetParent()

// Loop getting the parent of the object until it is of type window!
do while IsValid (lpo_parent) 
	if lpo_parent.TypeOf() <> window! then
		lpo_parent = lpo_parent.GetParent()
	else
		exit
	end if
loop

if IsNull(lpo_parent) Or not IsValid (lpo_parent) then
	setnull(lpo_parent)	
end If

return lpo_parent

end function

protected function integer of_initpages ();integer li_ctr = 1

do while li_ctr <= upperbound(this.control[],1)
//	if this.control[li_ctr].event dynamic ue_initpage() < 0 then return -(li_ctr)
	li_ctr++
loop
//
return li_ctr - 1


end function

event constructor;integer li_pagecount
li_pagecount = this.of_initpages()
end event

event selectionchanging;if ib_lockpage then
	return 1
else
	return 0
end if
end event

