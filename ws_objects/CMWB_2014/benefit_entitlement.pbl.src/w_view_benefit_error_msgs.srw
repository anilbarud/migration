$PBExportHeader$w_view_benefit_error_msgs.srw
$PBExportComments$used for user interaction to allow users to view and select error messages
forward
global type w_view_benefit_error_msgs from window
end type
type cb_print from commandbutton within w_view_benefit_error_msgs
end type
type cb_select from commandbutton within w_view_benefit_error_msgs
end type
type cb_close from commandbutton within w_view_benefit_error_msgs
end type
type dw_error_msg from u_dw_online within w_view_benefit_error_msgs
end type
end forward

global type w_view_benefit_error_msgs from window
integer x = 2002
integer y = 1000
integer width = 2880
integer height = 1716
boolean titlebar = true
boolean controlmenu = true
boolean minbox = true
boolean resizable = true
windowtype windowtype = popup!
long backcolor = 67108864
string icon = "AppIcon!"
boolean clientedge = true
cb_print cb_print
cb_select cb_select
cb_close cb_close
dw_error_msg dw_error_msg
end type
global w_view_benefit_error_msgs w_view_benefit_error_msgs

type variables
s_window_message istr_window_message

n_resize inv_resize

ULONG		iul_handle
end variables

forward prototypes
public function integer wf_select_and_return ()
end prototypes

public function integer wf_select_and_return ();INTEGER li_rowcount, li_count, li_row

//grab the rowcount
li_rowcount = dw_error_msg.rowcount() 

//default li_count
li_count = 0

//grab the data from the first selected row and go back to the calling window highlight the row and column
IF  li_rowcount > 0 THEN
	
	//get the selected rowvount
	li_count =  long(dw_error_msg.DESCRIBE("Evaluate('sum(if(isselected(),1,0))',0)"))
	
	IF li_count <= 0 THEN
		MESSAGEBOX('No Selected Rows', 'Please select a row before proceding.')
		RETURN -1
	END IF 
	
	IF li_count > 1 THEN //good
		MESSAGEBOX('Multiple Selected Rows', 'The first selected Error will be be returned.')	
	END IF 

	//grab the row
	li_row	 = dw_error_msg.getitemnumber(dw_error_msg.getrow(), 'row_num')
	
	//just in case
	IF li_row <= 0 THEN
		MESSAGEBOX('Row Error', 'Please re-select a row before proceding.')
		RETURN -1
	END IF 
		
	//set the row on the benefit entitlement window
	istr_window_message.awi_parent_window. DYNAMIC wf_setrow(li_row)

ELSE
	MESSAGEBOX('No Selected Rows', 'Please select a row before proceding.')
	RETURN -1
END IF

RETURN 1
end function

event open;datastore	lds_message
INTEGER		li_counter, li_upperbound

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


/*	grab the passed value into the datastore */
istr_window_message 	= Message.PowerObjectParm
lds_message 				= istr_window_message.apo_powerobjectparm[1]

//check and see if there is anything in the mode - used to determine the header
IF NOT isnull( istr_window_message.as_mode) AND TRIM( istr_window_message.as_mode) <> ''  THEN
	CHOOSE CASE istr_window_message.as_mode
		CASE 'A'
			
			THIS.title = 'Benefit Entitlement - Error - Information'
			//Benefit Entitlement - Error - Information
		CASE 'B'
			
			THIS.title = 'Benefit Entitlement - Warning - Information'
			//Benefit Entitlement - Warning - Information
				
		CASE ELSE

			// Information
			THIS.title = 'Information'
	END CHOOSE
	
END IF 

// make sure we have a rowcount 
IF lds_message.rowcount() < 1 THEN 
	MESSAGEBOX('Error Messages', 'No error messages were retrieved from the calling window')
	RETURN -1
END IF

// for each elect row copy to appropriate datawindow
FOR li_counter = 1 TO lds_message.rowcount() 
	IF lds_message.RowsCopy(li_counter, li_counter, Primary!, dw_error_msg, 99999, Primary!) = -1 THEN
		MESSAGEBOX("Generate Error Message","Error Message Information could not be Copied."+&
	    "~rPlease try again or contact the HelpDesk for assistance")
		RETURN -1 	
	END IF 
NEXT

IF IsNull(inv_resize) Or NOT IsValid (inv_resize) THEN
	inv_resize = CREATE n_resize
	inv_resize.of_SetOrigSize (2825,1596)
END IF

inv_resize.of_register(dw_error_msg,'scaletoright&bottom')
inv_resize.of_register(cb_print,'fixedtoright&bottom')
inv_resize.of_register(cb_select,'fixedtoright&bottom')
inv_resize.of_register(cb_close,'fixedtoright&bottom')

// insert elements into global array to have frame close all windows during its close
li_upperbound = UpperBound(gstr_window_array) + 1

iul_handle = Handle(THIS)

gstr_window_array[li_upperbound].window_element = THIS
gstr_window_array[li_upperbound].handle_element = iul_handle

//select the first row
dw_error_msg.selectrow(0,FALSE)

//NOW MAKE SURE THIS ROW IS SELECTED
dw_error_msg.SelectRow(1, TRUE)

//DOES AS NAMED - selects and returns to calling window
wf_select_and_return()

end event

on w_view_benefit_error_msgs.create
this.cb_print=create cb_print
this.cb_select=create cb_select
this.cb_close=create cb_close
this.dw_error_msg=create dw_error_msg
this.Control[]={this.cb_print,&
this.cb_select,&
this.cb_close,&
this.dw_error_msg}
end on

on w_view_benefit_error_msgs.destroy
destroy(this.cb_print)
destroy(this.cb_select)
destroy(this.cb_close)
destroy(this.dw_error_msg)
end on

event resize;LONG ll_workspacewidth,ll_workspaceheight

// Notify the resize service that the window size has changed.
ll_workspacewidth 	= THIS.WorkSpaceWidth()
ll_workspaceheight 	= THIS.WorkSpaceHeight()

IF IsValid (inv_resize) THEN
	inv_resize.Event pfc_Resize (sizetype, ll_workspacewidth, ll_workspaceheight )
END IF
end event

event close;n_common_annuity 	lnv_common_annuity

lnv_common_annuity = CREATE n_common_annuity

lnv_common_annuity.nf_close_handle_array(iul_handle)

end event

type cb_print from commandbutton within w_view_benefit_error_msgs
integer x = 1961
integer y = 1448
integer width = 283
integer height = 104
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Print"
end type

event clicked;IF dw_error_msg.rowcount() > 0 THEN 
	PRINT(dw_error_msg)
END IF 
end event

type cb_select from commandbutton within w_view_benefit_error_msgs
integer x = 2245
integer y = 1448
integer width = 283
integer height = 104
integer taborder = 30
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Select"
end type

event clicked;//DOES AS NAMED - selects and returns to calling window
wf_select_and_return()

end event

type cb_close from commandbutton within w_view_benefit_error_msgs
integer x = 2528
integer y = 1448
integer width = 283
integer height = 104
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Close"
end type

event clicked;close(parent)


end event

type dw_error_msg from u_dw_online within w_view_benefit_error_msgs
integer y = 4
integer width = 2811
integer height = 1412
integer taborder = 10
string dataobject = "d_benefit_error_messages"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event constructor;call super::constructor;THIS.uf_setselect(1)

end event

event rowfocuschanged;call super::rowfocuschanged;//MAKE SURE IT IS A valid row.
IF currentrow <= 0 THEN RETURN

//remove select from all other rows
THIS.selectrow(0,FALSE)

//NOW MAKE SURE THIS ROW IS SELECTED
This.SelectRow(currentrow, true)

//DOES AS NAMED - selects and returns to calling window
wf_select_and_return()
end event

