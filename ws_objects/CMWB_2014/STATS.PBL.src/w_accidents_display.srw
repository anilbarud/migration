$PBExportHeader$w_accidents_display.srw
forward
global type w_accidents_display from w_a_tool
end type
type dw_stats from u_dw_online within w_accidents_display
end type
end forward

global type w_accidents_display from w_a_tool
integer width = 2674
integer height = 1496
boolean titlebar = true
string title = ""
boolean controlmenu = true
boolean resizable = false
windowtype windowtype = response!
dw_stats dw_stats
end type
global w_accidents_display w_accidents_display

type variables
BOOLEAN     ib_needtosave
W_SHEET    iw_wsheet
LONG           il_claimno
INTEGER      il_rowsindw
INTEGER      il_currentrow

end variables

forward prototypes
public subroutine wf_retrieve_accident ()
end prototypes

public subroutine wf_retrieve_accident ();INTEGER  li_error_status

/*	Retrieve the record from ACCIDENT.
*/
	li_error_status = dw_stats.Retrieve(il_claimno)
	li_error_status = SQLCA.nf_handle_error("w_accidents","dw_stats","wf_retrieve_accident: 'dw_stats.Retrieve(il_claimno)'")
	IF li_error_status < 0 THEN
	
/*	Clear the screen and close window.
*/
		MessageBox('Error', 'An error occured retrieving accident information')
		dw_stats.Reset()
		cb_close.PostEvent(Clicked!)
	END IF


end subroutine

event open;call super::open;string ls_sob

SetPointer(HOURGLASS!)

/*	Get the claim_no of the claim to work on.
*/
	il_claimno = Message.DoubleParm

/*	Initialize the datawindows.
*/
	dw_stats.SetTransObject(SQLCA)
	
/*	Retrieve the information for the claim and open the imaged documents.
*/
	wf_retrieve_accident()
	

end event

on w_accidents_display.create
int iCurrent
call super::create
this.dw_stats=create dw_stats
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_stats
end on

on w_accidents_display.destroy
call super::destroy
destroy(this.dw_stats)
end on

event closequery;call super::closequery;long ll_result

/* close all imaged documents currently being displayed
*/

do
	ll_result = f_close_viewer()
loop while ll_result <> 0
end event

type st_title from w_a_tool`st_title within w_accidents_display
string text = "Display Accident Statistics"
end type

type cb_close from w_a_tool`cb_close within w_accidents_display
integer x = 2057
integer y = 1272
integer width = 329
integer height = 108
integer taborder = 10
end type

type dw_stats from u_dw_online within w_accidents_display
integer x = 23
integer y = 108
integer width = 2615
integer height = 1128
integer taborder = 20
string dataobject = "d_accident_stats_accident_display"
boolean livescroll = false
borderstyle borderstyle = styleraised!
end type

