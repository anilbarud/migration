$PBExportHeader$u_tabpg.sru
$PBExportComments$Departmental Layer for
forward
global type u_tabpg from userobject
end type
end forward

global type u_tabpg from userobject
long backcolor = 67108864
event ue_readonly ( )
end type
global u_tabpg u_tabpg

type variables
u_tab               itab_parent
integer              ii_index, ii_masteradd
public:
integer              ii_continue

end variables

forward prototypes
public function tab of_getparenttab ()
protected function window of_getparentwindow ()
end prototypes

event ue_readonly();/*	This function is used to set the screen into READ-ONLY mode. This is done by looping through
	all the objects on the screen (contained in control array) and disabling them. this is true
	for all buttons except close and all datawindows except ones with multiple records.
*/
	WINDOWOBJECT	lw_control[]
	INTEGER			li_loop, li_control_count, li_column_count, li_loop2
	OBJECT			lo_typeis
	COMMANDBUTTON	lc_button
	DATAWINDOW		ldw_datawindow
	RADIOBUTTON		lrb_radiobutton
	EDITMASK			lem_editmask
	USEROBJECT        luo_userobject
	STRING			ls_column_name, ls_result

	lw_control[] = THIS.Control[]

	li_loop = 1
	li_control_count = UpperBound (lw_control)

	DO WHILE li_loop <= li_control_count
		CHOOSE CASE	TypeOf(lw_control[li_loop])
			CASE	CommandButton!
			
/*	Will attempt to leave a 'Close' button enabled. This is so the screen can be closed. Will attempt
	by looking for button text equalling '&Close', 'Close' or button name = 'cb_close'.
*/
				lc_button = lw_control[li_loop]
				IF NOT (lc_button.Text = '&Close' OR lc_button.Text = 'Close' OR lc_button.ClassName() = 'cb_close') THEN
					lc_button.Enabled = FALSE
				END IF
			
			CASE	Datawindow!
			
/*	Check to see if the datawindow has a vertical scroll bar. If so, then do not disable the datawindow
	but instead protect all the columns.
*/
				ldw_datawindow = lw_control[li_loop]
				IF ldw_datawindow.VScrollBar = TRUE THEN
					li_loop2 = 1
					li_column_count = Integer(ldw_datawindow.Object.DataWindow.Column.Count)
					DO WHILE li_loop2 <= li_column_count
						ls_column_name = ldw_datawindow.Describe("#" + String(li_loop2) + ".Name")
						ls_result = ldw_datawindow.Modify(String(ls_column_name) + ".Protect='1'")
						li_loop2 ++
					LOOP
				ELSE
					ldw_datawindow.Enabled = FALSE
				END IF
				
			CASE	RadioButton!
				lrb_radiobutton = lw_control[li_loop]
				lrb_radiobutton.Enabled = FALSE
				
			CASE	EditMAsk!
				lem_editmask = lw_control[li_loop]
				lem_editmask.Enabled = FALSE
				
				
			CASE UserObject!
				
				luo_userobject = lw_control[li_loop]
				
				IF luo_userobject.ClassName() = 'uo_command_buttons' THEN
					luo_userobject.Enabled = FALSE
				END IF
					
		END CHOOSE
		li_loop ++
	LOOP


end event

public function tab of_getparenttab ();powerobject	lpo_parent

lpo_parent = this.GetParent()

// Loop getting the parent of the object until it is of type tab!
do while IsValid (lpo_parent) 
	if lpo_parent.TypeOf() <> tab! then
		lpo_parent = lpo_parent.GetParent()
	else
		exit
	end if
loop

if IsNull(lpo_parent) Or not IsValid (lpo_parent) then setnull(lpo_parent)	

return lpo_parent

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

on u_tabpg.create
end on

on u_tabpg.destroy
end on

event constructor;call super::constructor;itab_parent = this.of_getparenttab()
if isnull(itab_parent) or itab_parent.typeof() <> tab! then
	sqlca.sqlcode = -1
//	f_error_handling(sqlca,"u_tabpage inherited from userobject","","parent tab is invalid",false)
end if
ii_index = itab_parent.event pfc_getindex(this.classname())
if not ii_index > 0 then 
	sqlca.sqlcode = -1
//	f_error_handling(sqlca,"u_tabpage inherited from userobject","","could not obtain tabpage index",false)
end if	
end event

