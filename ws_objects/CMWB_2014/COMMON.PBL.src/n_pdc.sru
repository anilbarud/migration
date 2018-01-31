$PBExportHeader$n_pdc.sru
forward
global type n_pdc from nonvisualobject
end type
end forward

shared variables
/*************************************************************
Class Description:	
Class Public Interfaces:
**************************************************************/

end variables

global type n_pdc from nonvisualobject
end type
global n_pdc n_pdc

type variables
Private:

Protected:
u_dwa 			idw_dw[]
n_pdc			inv_pdc_child[]

n_pdc			inv_pdc_parent
n_transaction inv_transobj
w_ancestor		iwi_window_parent
boolean			ib_DoCommit

Public:
end variables

forward prototypes
public subroutine nf_set_window_parent (w_ancestor awi_window_parent)
public subroutine nf_set_pdc_parent (n_pdc anv_pdc_parent)
public subroutine nf_set_commit (boolean ab_state)
public function integer nf_save ()
public function integer nf_validate_save ()
public function integer nf_check_mandatory ()
public function integer nf_check_bus_rule ()
public function integer nf_update ()
public function integer nf_log_events ()
public function integer nf_set_unused_fields ()
public function integer nf_insert (long al_row)
public function integer nf_set_defaults ()
public function integer nf_change_item (long al_datawindow)
public function integer nf_retrieve ()
public function long nf_get_next_identifier ()
public function long nf_set_identifiers ()
public subroutine nf_set_datawindow (u_dwa adw_dw[], n_transaction anv_transobj)
public subroutine nf_init (u_dwa adw_dw[], n_transaction anv_transobj, w_ancestor awi_window_parent)
end prototypes

public subroutine nf_set_window_parent (w_ancestor awi_window_parent);/*************************************************************************
	Description:		This function stores a reference to the parent window
							in an instance variable
*************************************************************************/

iwi_window_parent = awi_window_parent

/*************************************************************************
	Revision History:
	Developer:				Lorie Vaughan
	Date:						April 3, 1996
	Comment:					Created	
*************************************************************************/

end subroutine

public subroutine nf_set_pdc_parent (n_pdc anv_pdc_parent);/*************************************************************************
	Description:		This function stores a reference to the parent 
							business object in an instance variable
*************************************************************************/

inv_pdc_parent = anv_pdc_parent

/*************************************************************************
	Revision History:
	Developer:				Lorie Vaughan
	Date:						April 3, 1996
	Comment:					Created	
*************************************************************************/

end subroutine

public subroutine nf_set_commit (boolean ab_state);/*************************************************************************
	Description:		This function changes the value of ib_DoCommit.
							It allows the user to change the behaviour of the 
							save function (ie. It will either commit or not 
							commit).
*************************************************************************/

ib_DoCommit = ab_state

/*************************************************************************
	Revision History:
	Developer:				Lorie Vaughan
	Date:						April 3, 1996
	Comment:					Created	
*************************************************************************/

end subroutine

public function integer nf_save ();/*************************************************************************
	Description:		This function controls the steps required during a
							save
*************************************************************************/
LONG ll_return_value

// it it necessary to return the value of the function called not just -1
// if the lower function returned an error code other than -1 then it would be lost
// if only a -1 was returned from here

ll_return_value = nf_validate_save()
IF  ll_return_value < 0 THEN Return ll_return_value
ll_return_value = nf_set_unused_fields()
IF ll_return_value < 0 THEN Return ll_return_value
ll_return_value = nf_set_identifiers() 
IF ll_return_value < 0 THEN Return ll_return_value
ll_return_value = nf_log_events()
IF ll_return_value < 0 THEN Return ll_return_value
ll_return_value = nf_update()
IF ll_return_value < 0 THEN Return ll_return_value

Return 0

/*************************************************************************
	Revision History:
	Developer:				Lorie Vaughan
	Date:						April 4, 1996
	Comment:					Created	
*************************************************************************/

end function

public function integer nf_validate_save ();/*************************************************************************
	Description:		This function controls the steps required for 
							validating the datawindow controls during a save
*************************************************************************/
LONG ll_return_value

ll_return_value = nf_check_mandatory()
If ll_return_value < 0 Then Return ll_return_value
ll_return_value = nf_check_bus_rule()
If ll_return_value < 0 Then Return ll_return_value

Return 0

/*************************************************************************
	Revision History:
	Developer:				Lorie Vaughan
	Date:						April 3, 1996
	Comment:					Created	
*************************************************************************/

end function

public function integer nf_check_mandatory ();/*************************************************************************
	Description:		This function validates that all mandatory fields
							have been entered for each of the datawindows
							in the array
	Note:					Object specified (ie not coded here)
*************************************************************************/

Return 0

/*************************************************************************
	Revision History:
	Developer:				Lorie Vaughan
	Date:						April 3, 1996
	Comment:					Created	
*************************************************************************/

end function

public function integer nf_check_bus_rule ();/*************************************************************************
	Description:		This function validates the fields in each of the 
							datawindows according to the business rules.
	Note:					Object specified (ie not coded here)
*************************************************************************/



Return 0

/*************************************************************************
	Revision History:
	Developer:				Lorie Vaughan
	Date:						April 3, 1996
	Comment:					Created	
*************************************************************************/



end function

public function integer nf_update ();/*************************************************************************
	Description:		This function updates the datawindows in the array.
*************************************************************************/
LONG ll_error
INT	li_cntr = 1, li_bound

li_bound = UpperBound(idw_dw)
DO WHILE li_cntr <= li_bound
	idw_dw[li_cntr].Update()
   ll_error = inv_transobj.nf_handle_error("n_pdc","nf_update","updateing dw")
   IF ll_error < 0 THEN
      Return ll_error
   END IF
	// Error Handling
	li_cntr ++
LOOP
Return 0

/*************************************************************************
	Revision History:
	Developer:				Lorie Vaughan
	Date:						April 4, 1996
	Comment:					Created	
*************************************************************************/



end function

public function integer nf_log_events ();/*************************************************************************
	Description:		This function checks to see if there are any events
							to be logged.  If so, it will log them.
	Note:					Object specified (ie not coded here)
*************************************************************************/



Return 0

/*************************************************************************
	Revision History:
	Developer:				Lorie Vaughan
	Date:						April 4, 1996
	Comment:					Created	
*************************************************************************/



end function

public function integer nf_set_unused_fields ();/*************************************************************************
	Description:		This function controls the steps required to obtain
							identifiers required during create and set defaults 
							on fields that are not shown by the current screen.
	Note:					Object specific (ie not coded here)
*************************************************************************/


Return 0

/*************************************************************************
	Revision History:
	Developer:				Lorie Vaughan
	Date:						April 3, 1996
	Comment:					Created	
*************************************************************************/

end function

public function integer nf_insert (long al_row);/*************************************************************************
	Description:		This function inserts a row into the datawindow
							then calls the set defaults function

*************************************************************************/
LONG ll_loop

ll_loop = 1
DO WHILE ll_loop <= UpperBound(idw_dw[])
   IF idw_dw[ll_loop].InsertRow(al_row)  < 0 THEN Return -1
   ll_loop = ll_loop + 1
LOOP
IF nf_set_defaults() < 0 THEN Return -1

Return 0

/*************************************************************************
	Revision History:
	Developer:				Wendy Doherty
	Date:						April 10, 1996
	Comment:					Created	
*************************************************************************/



end function

public function integer nf_set_defaults ();/*************************************************************************
	Description:		This function is used to set all the visible default 
							values on the datawindow

	Note:					This function is coded at the object level

*************************************************************************/

Return 0

/*************************************************************************
	Revision History:
	Developer:				Wendy Doherty
	Date:						April 10, 1996
	Comment:					Created	
*************************************************************************/

end function

public function integer nf_change_item (long al_datawindow);/*************************************************************************
	Description:		This function should be called from the item changed event
							of the datawindow.  It is necessary so that all the business 
							rules will be in the object and not located in both the window
							and the object

	Note:					The function will have to be coded at the object level.
							The argument to the function is the number of the data window (array subscript)
							That triggered the event

*************************************************************************/

Return 0

/*************************************************************************
	Revision History:
	Developer:				Wendy Doherty
	Date:						April 10, 1996
	Comment:					Created	
*************************************************************************/

end function

public function integer nf_retrieve ();
Return 0
end function

public function long nf_get_next_identifier ();Return 0
end function

public function long nf_set_identifiers ();/*************************************************************************
	Description:		This function is intended to be used to set the system generated key columns
							of the data window

	Note:					Object specified (ie not coded here)
*************************************************************************/

Return 0

/*************************************************************************
	Revision History:
	Developer:				Wendy Doherty
	Date:						April 10, 1996
	Comment:					Created	
*************************************************************************/

end function

public subroutine nf_set_datawindow (u_dwa adw_dw[], n_transaction anv_transobj);/*************************************************************************
	Description:		This function registers datawindows and transaction
							objects plus, it calls the settransobject for the
							datawindow using the corresponding element in the
							transaction array.
*************************************************************************/


Int	li_cntr = 1

Do While li_cntr 				<=	UpperBound(adw_dw)
	idw_dw[li_cntr] 			=	adw_dw[li_cntr]
	idw_dw[li_cntr].SetTransObject(anv_transobj)
	li_cntr ++
Loop

inv_transobj 	=	anv_transobj


/*************************************************************************
	Revision History:
	Developer:				Lorie Vaughan
	Date:						April 3, 1996
	Comment:					Created	
*************************************************************************/

end subroutine

public subroutine nf_init (u_dwa adw_dw[], n_transaction anv_transobj, w_ancestor awi_window_parent);/*************************************************************************
	Description:		This function controls the initialization of the 
							user object by calling appropriate functions to 
							register the datawindow and the parent window.
*************************************************************************/

nf_set_datawindow(adw_dw[],anv_transobj)
nf_set_window_parent(awi_window_parent)

/*************************************************************************
	Revision History:
	Developer:				Lorie Vaughan
	Date:						April 3, 1996
	Comment:					Created	
*************************************************************************/

end subroutine

on n_pdc.create
call super::create
TriggerEvent( this, "constructor" )
end on

on n_pdc.destroy
TriggerEvent( this, "destructor" )
call super::destroy
end on

