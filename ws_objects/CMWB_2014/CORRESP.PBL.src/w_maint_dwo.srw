$PBExportHeader$w_maint_dwo.srw
$PBExportComments$Ancestor window to allow general maintenance any datawindow objects. Has functionality for add, delete, insert.
forward
global type w_maint_dwo from window
end type
type dw_detail from u_dw_online within w_maint_dwo
end type
type cb_save from commandbutton within w_maint_dwo
end type
type cb_delete from commandbutton within w_maint_dwo
end type
type cb_add from commandbutton within w_maint_dwo
end type
end forward

global type w_maint_dwo from window
integer y = 680
integer width = 2615
integer height = 1332
boolean titlebar = true
string title = "Maintain"
boolean controlmenu = true
long backcolor = 67108864
event ue_filesave pbm_custom01
event ue_filedelete pbm_custom02
event ue_filenew pbm_custom03
event ue_retrieve_data pbm_custom06
event ue_set_fields pbm_custom07
event ue_postopen pbm_custom08
dw_detail dw_detail
cb_save cb_save
cb_delete cb_delete
cb_add cb_add
end type
global w_maint_dwo w_maint_dwo

type variables
Protected:
boolean		  vib_data_ok
long		  vil_curr_row


end variables

forward prototypes
public function boolean wf_update ()
public subroutine wf_disable_screen (string p_color, boolean p_protect)
end prototypes

on ue_filesave;// if window can be saved and data is validated then try to update.

SetPointer( HourGlass! )
vib_data_ok = false

// if datawindow not enabled then do not try to save
IF NOT dw_detail.enabled THEN Return

IF dw_detail.uf_accept_dw() = -1 then 
	Return
ELSE

	vil_curr_row = dw_detail.GetRow()
	this.TriggerEvent( "ue_set_fields" )

	// update the datawindow
	SetPointer(HourGlass!)
   IF NOT wf_update () THEN	Return
END IF

vib_data_ok = true


end on

on ue_filedelete;/////////////////////////////////////////////////////////////////////////
//
//   User Event:  UE_FILEDELETE
//
//   Description: This user_Event will validate the delete,
//						delete the current row.
//
////////////////////////////////////////////////////////////////////////
integer li_yn, li_delete_count
long ll_row_count, ll_row, ll_delete
vib_data_ok = false
IF not dw_detail.enabled THEN 
	vib_data_ok = true
	Return
end if
/* Place code here that was formerly in User Object function f_deleterow - u_dw_online
*/
/* ----------------------------------------------------------------------------------------------------	*/
/*	Function Name: 	f_deletrows																							*/
/*																																			*/
/*	Purpose:				This function deletes all the selected rows in a datawindow. 	*/
/*							Before starting it asks the user for a confirmation of the delete process. */
/*																																			*/
/*	Arguments:			None																					*/
/*	Return Value:		None																																		*/
/* ----------------------------------------------------------------------------------------------------	*/



li_yn  = MessageBox ( "Delete", "Are you sure you want to delete?", Question!, YesNo! )
If li_yn = 1 then
	SetPointer ( Hourglass! )
	ll_row_count = dw_detail.RowCount() - 1
	For ll_row = ll_row_count to 0 Step - 1
		ll_delete = dw_detail.GetSelectedRow ( ll_row )
		If ll_delete <> 0 then 
			dw_detail.DeleteRow ( ll_delete )
			li_delete_count++
		End If
	Next		
	If li_delete_count = 0 then 			//No selected rows, delete current
		dw_detail.DeleteRow ( dw_detail.GetRow () )
	End If
End If


//dw_detail.uf_deleterows()	// Call process to delete

SetPointer(HourGlass!)
IF NOT wf_update () THEN Return

vib_data_ok = true
end on

event ue_filenew;///////////////////////////////////////////////////////////////////// 
//
//   User Event: UE_FILENEW
//
//   Description: This user event will create a new row/record.
//						If changes have been made to the current row/record
//                then prompt user to save or not. If save then update
//                information and create new row. Otherwise create 
//                new row.
//
//////////////////////////////////////////////////////////////////////
dwitemstatus stat
vib_data_ok = false
choose case dw_detail.uf_datachanged()
	case 0	// Do nothing
	case IS >0
		Choose Case MessageBox( this.title, "Do you Wish to Save Changes?", &
										Question!, YesNoCancel!)
		Case 1
			TriggerEvent( this, "ue_filesave" )
			IF not vib_data_ok THEN Return
		case 2,3 // do nothing
		
			Return
		End Choose
END choose
// Get the row status so you will know what kind of update to expect - INSERT or UPDATE	
stat = dw_detail.GetItemStatus ( 1, 0, primary! )
if isnull(stat) then stat = datamodified!
if stat <> new! then
	Reset    ( dw_detail )
	InsertRow( dw_detail, 0 )
end if
SetFocus ( dw_detail )
vib_data_ok = true
 


end event

on ue_set_fields;//***********************************************************
///
// This user event will be used to programatically fill in
// fields that aren't visible to the user. 
//
//	NOTE - Other fields can be added in the descendent
//			 windows under the same event name. It will
//			 extend the srcipt.
//
///


end on

public function boolean wf_update ();// process update, return false if update failed
int	vli_return_code
string	vls_dw_name

vls_dw_name = dw_detail.DataObject	// Get the name of the datawindow assigned to dw_detail


SQLCA.nf_begin_transaction()

dw_detail.Update()
IF SQLCA.nf_handle_error(vls_dw_name,"w_maintain","wf_update function") <> 0 THEN RETURN FALSE

SQLCA.nf_commit_transaction()
IF SQLCA.nf_handle_error('w_maintain', 'EXECUTE IMMEDIATE COMMIT TXN USING SQLCA', 'wf_update') <> 0 THEN Return False


Return True


		
end function

public subroutine wf_disable_screen (string p_color, boolean p_protect);//******************************************************************************
///
// This routine will change the background color of every
// column, as well as protect them.
//
//Parms: string	p_color		Bacground color
//			boolean	p_protect	True to protect column
//										or False no to protect column
///

int		vli_colcount, vli_col_cntr, vli_protect
string	vls_mod_string

IF p_protect THEN
	vli_protect = 1
ELSE
	vli_protect = 0
END IF

vli_colcount = Integer(dw_detail.Describe("Datawindow.Column.Count"))

FOR vli_col_cntr = 1 TO vli_colcount
	vls_mod_string = "#" + String(vli_col_cntr) + ".Background.Color=" + p_color +&
	" #" + String(vli_col_cntr) + ".Protect=" + String(vli_protect)
	dw_detail.Modify(vls_mod_string)
NEXT
end subroutine

on closequery;//dw_detail.ib_supress_pbmessage = TRUE
CHOOSE CASE dw_detail.uf_datachanged()
	CASE is > 0
		CHOOSE CASE MessageBox(title, &
					"Do you want to save changes!", &
					Question!, YesNoCancel!)
			CASE 1		// Save the data
				This.TriggerEvent( "ue_filesave" )
				IF NOT vib_data_ok THEN Message.ReturnValue = 1
				Return
			CASE 2		// Close without saving (do nothing)
			CASE 3		// Cancel and return to window
				dw_detail.SetFocus()
				Message.ReturnValue = 1
				Return
		END CHOOSE
	CASE 0	// No changes made, nothing to do
END CHOOSE

end on

event open;//////////////////////////////////////////////////////////////////////////////////
////
//Purpose: This is a a class window (Ancestor) used to maintain a detail
//			  screen.
//
//Log:
//
//			Date			Who				What
//-------....--------...------------....------------------------------------------
//		 96/01/09		B.Burton			Initial Version
//
// Open Event
//////////////////////////////////////////////////////////////////////////////////
//	

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


dw_detail.SettransObject(SQLCA)

// Insert a new detail screen
IF dw_detail.InsertRow(0) < 0 THEN 
	MessageBox ( this.title, "Could not insert row.")
   Return
END IF

//*************************************************
// Do other processing if needed. Allows you to
// do lenghty processing after the visual 
// components of window processing are complete.
this.PostEvent( "ue_postopen" )
end event

on w_maint_dwo.create
this.dw_detail=create dw_detail
this.cb_save=create cb_save
this.cb_delete=create cb_delete
this.cb_add=create cb_add
this.Control[]={this.dw_detail,&
this.cb_save,&
this.cb_delete,&
this.cb_add}
end on

on w_maint_dwo.destroy
destroy(this.dw_detail)
destroy(this.cb_save)
destroy(this.cb_delete)
destroy(this.cb_add)
end on

type dw_detail from u_dw_online within w_maint_dwo
integer x = 18
integer y = 20
integer width = 2231
integer height = 1184
integer taborder = 10
end type

type cb_save from commandbutton within w_maint_dwo
integer x = 2277
integer y = 928
integer width = 288
integer height = 96
integer taborder = 40
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Save"
boolean default = true
end type

on clicked;parent.TriggerEvent( "ue_filesave" )


end on

type cb_delete from commandbutton within w_maint_dwo
integer x = 2277
integer y = 640
integer width = 288
integer height = 96
integer taborder = 30
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Delete"
end type

on clicked;
parent.TriggerEvent( "ue_filedelete" )
IF NOT vib_data_ok THEN Return	// Error occured

// Insert a blank record for user 
parent.TriggerEvent( "ue_filenew" )
IF NOT vib_data_ok THEN Return	// Error occured
end on

type cb_add from commandbutton within w_maint_dwo
integer x = 2277
integer y = 516
integer width = 288
integer height = 96
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Add"
end type

on clicked;parent.TriggerEvent( "ue_filenew" )
IF NOT vib_data_ok THEN Return	// Error occured
end on

