$PBExportHeader$u_checklist.sru
forward
global type u_checklist from userobject
end type
type p_history from picture within u_checklist
end type
type p_notes from picture within u_checklist
end type
type p_checklist from picture within u_checklist
end type
type tab_checklist from tab within u_checklist
end type
type tabpage_checklist from userobject within tab_checklist
end type
type dw_checklist from u_checklist_datawindow within tabpage_checklist
end type
type uo_checklistbar from u_titlebar within tabpage_checklist
end type
type dw_checklist_master from u_checklist_datawindow within tabpage_checklist
end type
type tabpage_checklist from userobject within tab_checklist
dw_checklist dw_checklist
uo_checklistbar uo_checklistbar
dw_checklist_master dw_checklist_master
end type
type tabpage_checklist_notes from userobject within tab_checklist
end type
type cb_cancel from commandbutton within tabpage_checklist_notes
end type
type dw_checklist_notes_master from u_checklist_datawindow within tabpage_checklist_notes
end type
type dw_checklist_notes from u_checklist_datawindow within tabpage_checklist_notes
end type
type uo_notesbar from u_titlebar within tabpage_checklist_notes
end type
type cb_save from commandbutton within tabpage_checklist_notes
end type
type tab_comment from tab within tabpage_checklist_notes
end type
type tabpage_1 from userobject within tab_comment
end type
type dw_checklist_step_notes_entered_tab from u_checklist_datawindow within tabpage_1
end type
type tabpage_1 from userobject within tab_comment
dw_checklist_step_notes_entered_tab dw_checklist_step_notes_entered_tab
end type
type tabpage_2 from userobject within tab_comment
end type
type dw_checklist_notes_entered_tab from u_checklist_datawindow within tabpage_2
end type
type tabpage_2 from userobject within tab_comment
dw_checklist_notes_entered_tab dw_checklist_notes_entered_tab
end type
type tabpage_4 from userobject within tab_comment
end type
type dw_checklist_cancelled_step_comment_tab from u_checklist_datawindow within tabpage_4
end type
type tabpage_4 from userobject within tab_comment
dw_checklist_cancelled_step_comment_tab dw_checklist_cancelled_step_comment_tab
end type
type tabpage_3 from userobject within tab_comment
end type
type dw_checklist_cancelled_comment_tab from u_checklist_datawindow within tabpage_3
end type
type tabpage_3 from userobject within tab_comment
dw_checklist_cancelled_comment_tab dw_checklist_cancelled_comment_tab
end type
type tab_comment from tab within tabpage_checklist_notes
tabpage_1 tabpage_1
tabpage_2 tabpage_2
tabpage_4 tabpage_4
tabpage_3 tabpage_3
end type
type tabpage_checklist_notes from userobject within tab_checklist
cb_cancel cb_cancel
dw_checklist_notes_master dw_checklist_notes_master
dw_checklist_notes dw_checklist_notes
uo_notesbar uo_notesbar
cb_save cb_save
tab_comment tab_comment
end type
type tabpage_checklist_history from userobject within tab_checklist
end type
type uo_historybar from u_titlebar within tabpage_checklist_history
end type
type dw_checklist_history_master from u_checklist_datawindow within tabpage_checklist_history
end type
type dw_checklist_history_detail from u_checklist_datawindow within tabpage_checklist_history
end type
type tabpage_checklist_history from userobject within tab_checklist
uo_historybar uo_historybar
dw_checklist_history_master dw_checklist_history_master
dw_checklist_history_detail dw_checklist_history_detail
end type
type tab_checklist from tab within u_checklist
tabpage_checklist tabpage_checklist
tabpage_checklist_notes tabpage_checklist_notes
tabpage_checklist_history tabpage_checklist_history
end type
type p_left_arrow from picture within u_checklist
end type
type p_right_arrow from picture within u_checklist
end type
type st_blueline from statictext within u_checklist
end type
end forward

global type u_checklist from userobject
integer width = 2199
integer height = 2240
long backcolor = 16777215
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
event ue_checklist_buttonclicked ( u_checklist_datawindow adw_dw,  long row,  long actionreturncode,  dwobject dwo )
event ue_checklist_itemchanged ( ref u_checklist_datawindow adw_dw,  long row,  dwobject dwo,  string data,  ref long al_return )
event ue_checklist_itemchangeaccepted ( ref u_checklist_datawindow adw_dw,  long al_row,  string as_column_name,  string as_data )
event ue_checklist_buttonclicking ( u_checklist_datawindow adw_dw,  long row,  dwobject dwo,  ref long al_return )
event ue_post_constructor ( )
event ue_checklist_rowfocuschanged ( ref datawindow adw_dw,  integer currentrow )
event ue_checklist_note_save_btn_clicking ( ref long al_return )
event ue_checklist_step_status_changed ( ref u_checklist_datawindow adw_dw,  long row,  dwobject dwo,  string data,  ref string as_status_assigned_method_code,  ref long al_return )
event ue_checklist_master_itemchanged ( ref u_checklist_datawindow adw_dw,  long row,  dwobject dwo,  string data,  ref string as_status_assigned_method_code,  ref long al_return )
event ue_checklist_master_itemchangeaccepted ( long al_row,  string al_column_name,  string al_data )
p_history p_history
p_notes p_notes
p_checklist p_checklist
tab_checklist tab_checklist
p_left_arrow p_left_arrow
p_right_arrow p_right_arrow
st_blueline st_blueline
end type
global u_checklist u_checklist

type variables
Boolean ib_checklist_visible
Boolean	ib_step_order = TRUE, ib_cannot_change_status, ib_maximized

LONG		il_resize_steps = 50 , il_maximized_width = 2194, il_minimized_width = 110
Long il_maximized_x = 3003, il_minimized_x = 5079

n_checklist inv_checklist

u_datawindow   idw_dw[]

n_transaction itr_trans_object

WINDOW		iw_parent

//flags if security is to be checked on window.
boolean   I_Authorized_Access
M_DW_RMB_POPUP im_popup

PRIVATE:
Boolean ib_notes_saved, ib_notes_row_set

Long il_sidetoolbar_width
long il_checklist_no, il_step_no
Long il_workspacewidth, il_workspaceheight

String is_user_id, is_checklist_type_code, is_checklist_type_desc, is_status, is_module_code, is_status_assigned_method_code
string is_cancelled_comment




end variables

forward prototypes
public function string uf_get_checklist_type_desc (string as_checklist_type_code)
public function datetime uf_get_datetime ()
public function integer uf_maximized ()
public function integer uf_minimized ()
public function integer uf_set_checklist_invisible ()
public subroutine uf_set_checklist_type_code (string as_checklist_type_code)
public function integer uf_set_checklist_visible ()
public subroutine uf_set_parent_window (ref window aw_parent)
public subroutine uf_set_ib_notes_row (boolean ab_notes_row_set)
public subroutine uf_get_checklist_nvo (ref n_checklist anv_checklist)
public subroutine uf_set_checklistbar_text (string bartext)
public subroutine uf_set_historybar_text (string bartext)
public subroutine uf_set_notesbar_text (string bartext)
public subroutine uf_menu (datawindow adw_dw)
public function integer uf_prevent_tabpage_change ()
public subroutine uf_set_module_code (string as_module_code)
public subroutine uf_clear_datawindows ()
public function string uf_get_cancelled_comment ()
end prototypes

event ue_checklist_itemchanged(ref u_checklist_datawindow adw_dw, long row, dwobject dwo, string data, ref long al_return);//dwitemstatus ldwis
//
//ldwis = tab_checklist.tabpage_checklist.dw_checklist.GetItemStatus(row,0,Primary!)
//
//
//choose case ldwis
//	case NotModified!
//		messagebox('u_checklist','NotModified!')
//		
//	case DataModified!
//		messagebox('u_checklist','DataModified!')
//		
//	case NewModified!
//		messagebox('u_checklist','NewModified!')
//		
//	case New!
//		messagebox('u_checklist','New!')
//end choose
//


end event

event ue_post_constructor();IF IsValid(iw_parent) THEN
	il_workspacewidth = iw_parent.workspacewidth( )
	il_workspaceheight = iw_parent.workspaceheight( )
	
	THIS.x = il_workspacewidth - THIS.width
	
	il_minimized_x = THIS.x
	il_maximized_x = il_minimized_x - 2076
END IF
end event

event ue_checklist_rowfocuschanged(ref datawindow adw_dw, integer currentrow);//ue_rowfocuschanged triggered from rowfocuschanged.
end event

event ue_checklist_note_save_btn_clicking(ref long al_return);
// called at beginning of tab_checklist.tabpage_checklist_notes.cb_save clicked event
// with the intent of exposing this object to the module that contains, and allowing
// the module to prevent the save of a checklist note
end event

event ue_checklist_step_status_changed(ref u_checklist_datawindow adw_dw, long row, dwobject dwo, string data, ref string as_status_assigned_method_code, ref long al_return);is_status_assigned_method_code = as_status_assigned_method_code
end event

public function string uf_get_checklist_type_desc (string as_checklist_type_code);String ls_checklist_type_desc


Select checklist_type_desc
Into :ls_checklist_type_desc
From Checklist_Type
Where checklist_type_code = :as_checklist_type_code
And active_flag = 'Y'
Using itr_trans_object;

itr_trans_object.nf_handle_error('w_checklist','wf_get_checklist_type_desc','Select checklist_type_desc')


Return (ls_checklist_type_desc)
end function

public function datetime uf_get_datetime ();Datetime ldtm_server_datetime

select top 1 getdate()
  into :ldtm_server_datetime
  from sysobjects;
  
itr_trans_object.nf_handle_error('w_checklist','constructior for tab_checklist','select top 1 getdate()')


Return ldtm_server_datetime
end function

public function integer uf_maximized ();integer		li_step_width
long ll_x, ll_width

li_step_width =( il_maximized_width - il_minimized_width) / il_resize_steps
il_sidetoolbar_width = st_blueline.width + 1


DO WHILE THIS.x > il_maximized_x
		ll_x = THIS.x
		ll_width = THIS.width
		
		If ll_width + li_step_width < il_maximized_width Then
			THIS.x = ll_x - li_step_width
			THIS.width = ll_width + li_step_width
		else
			THIS.x = il_maximized_x
			THIS.width = il_maximized_width
			EXIT
		End if
Loop

ib_maximized = true

return 1
end function

public function integer uf_minimized ();integer		li_step_width
long ll_x, ll_width

li_step_width =( il_maximized_width - il_minimized_width) / il_resize_steps
il_sidetoolbar_width = st_blueline.width + 1

DO WHILE THIS.x < il_minimized_x
	ll_x = THIS.x
	ll_width = THIS.width
	
	If ll_width - li_step_width > il_minimized_width Then
		THIS.x = ll_x + li_step_width
		THIS.width = ll_width - li_step_width
	else
		THIS.x = il_minimized_x
		THIS.width = il_minimized_width
		EXIT
	End if
	
Loop

ib_maximized = false

return 1


end function

public function integer uf_set_checklist_invisible ();//
//tab_checklist.visible = False
//st_blueline.visible = False
//p_checklist.visible = False
//p_notes.visible = False
//p_history.visible = False
this.visible = false


ib_checklist_visible = False

Return 1

end function

public subroutine uf_set_checklist_type_code (string as_checklist_type_code);is_checklist_type_code = as_checklist_type_code
end subroutine

public function integer uf_set_checklist_visible ();
//tab_checklist.visible = True
//st_blueline.visible = True
//p_checklist.visible = True
//p_notes.visible = True
//p_history.visible = True

this.visible = true

ib_checklist_visible = True

Return 1

end function

public subroutine uf_set_parent_window (ref window aw_parent);iw_parent = aw_parent
end subroutine

public subroutine uf_set_ib_notes_row (boolean ab_notes_row_set);ib_notes_row_set = ab_notes_row_set
end subroutine

public subroutine uf_get_checklist_nvo (ref n_checklist anv_checklist);anv_checklist = inv_checklist
end subroutine

public subroutine uf_set_checklistbar_text (string bartext);
this.tab_checklist.tabpage_checklist.uo_checklistbar.st_checklist_name.Text = bartext
end subroutine

public subroutine uf_set_historybar_text (string bartext);
this.tab_checklist.tabpage_checklist_history.uo_historybar.st_checklist_name.Text = bartext
end subroutine

public subroutine uf_set_notesbar_text (string bartext);
this.tab_checklist.tabpage_checklist_notes.uo_notesbar.st_checklist_name.Text = bartext
end subroutine

public subroutine uf_menu (datawindow adw_dw);/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/

	If not isvalid(im_popup) Then

		im_popup = Create m_dw_rmb_popup
		im_popup.mf_set_datawindow(adw_dw)
		im_popup.m_options.m_filterlist.visible = TRUE	
		im_popup.m_options.m_sort.visible = TRUE
		im_popup.m_options.m_tooltips.visible = TRUE	
		im_popup.m_options.m_1.visible = FALSE
		im_popup.m_options.m_2.visible = FALSE
	End if

	im_popup.m_options.PopMenu(adw_dw.PointerX( ), adw_dw.PointerY( ))

end subroutine

public function integer uf_prevent_tabpage_change ();INTEGER li_rtn


/*
idw_dw[8] = tab_checklist.tabpage_checklist_notes.tab_comment.tabpage_1.dw_checklist_step_notes_entered_tab
idw_dw[9] = tab_checklist.tabpage_checklist_notes.tab_comment.tabpage_2.dw_checklist_notes_entered_tab
idw_dw[3] = tab_checklist.tabpage_checklist_notes.tab_comment.tabpage_3.dw_checklist_cancelled_comment_tab
idw_dw[10] = tab_checklist.tabpage_checklist_notes.tab_comment.tabpage_4.dw_checklist_cancelled_step_comment_tab
*/


li_rtn = inv_checklist.nf_check_note_modified_status()
IF li_rtn < 0 THEN
	// prevent tab from changing
	RETURN 1
	
ELSE
	// allow tab to change
	RETURN 0
END IF
end function

public subroutine uf_set_module_code (string as_module_code);is_module_code = as_module_code
end subroutine

public subroutine uf_clear_datawindows ();
tab_checklist.tabpage_checklist.dw_checklist_master.Reset()
tab_checklist.tabpage_checklist.dw_checklist.Reset()
tab_checklist.tabpage_checklist_history.dw_checklist_history_detail.Reset()
tab_checklist.tabpage_checklist_history.dw_checklist_history_master.Reset()
tab_checklist.tabpage_checklist_notes.dw_checklist_notes.Reset()
tab_checklist.tabpage_checklist_notes.dw_checklist_notes_master.Reset()

tab_checklist.tabpage_checklist.uo_checklistbar.st_checklist_name.Text = ''
end subroutine

public function string uf_get_cancelled_comment ();return is_cancelled_comment
end function

on u_checklist.create
this.p_history=create p_history
this.p_notes=create p_notes
this.p_checklist=create p_checklist
this.tab_checklist=create tab_checklist
this.p_left_arrow=create p_left_arrow
this.p_right_arrow=create p_right_arrow
this.st_blueline=create st_blueline
this.Control[]={this.p_history,&
this.p_notes,&
this.p_checklist,&
this.tab_checklist,&
this.p_left_arrow,&
this.p_right_arrow,&
this.st_blueline}
end on

on u_checklist.destroy
destroy(this.p_history)
destroy(this.p_notes)
destroy(this.p_checklist)
destroy(this.tab_checklist)
destroy(this.p_left_arrow)
destroy(this.p_right_arrow)
destroy(this.st_blueline)
end on

event constructor;inv_checklist = Create n_checklist


idw_dw[1] = tab_checklist.tabpage_checklist.dw_checklist
idw_dw[2] = tab_checklist.tabpage_checklist_notes.dw_checklist_notes
idw_dw[3] = tab_checklist.tabpage_checklist_notes.tab_comment.tabpage_3.dw_checklist_cancelled_comment_tab
idw_dw[4] = tab_checklist.tabpage_checklist_history.dw_checklist_history_master
idw_dw[5] = tab_checklist.tabpage_checklist_history.dw_checklist_history_detail
idw_dw[6] = tab_checklist.tabpage_checklist_notes.dw_checklist_notes_master
idw_dw[7] = tab_checklist.tabpage_checklist.dw_checklist_master
idw_dw[8] = tab_checklist.tabpage_checklist_notes.tab_comment.tabpage_1.dw_checklist_step_notes_entered_tab
idw_dw[9] = tab_checklist.tabpage_checklist_notes.tab_comment.tabpage_2.dw_checklist_notes_entered_tab
idw_dw[10] = tab_checklist.tabpage_checklist_notes.tab_comment.tabpage_4.dw_checklist_cancelled_step_comment_tab

IF ib_cannot_change_status = False THEN
	//swap the datawindow
	idw_dw[1].dataobject = 'd_checklist_disabled'
END IF


PostEvent("ue_post_constructor")


/*

Put a computed column in the sql. Initialize it to 1 and give it a 
meaningful name. 
Set the Protect expression to that column. Your DDDW column is now 
protected. 
Code in the Clicked event, if the dwo.name is your DDDW set the protect 
column to 0 and SetColumn on the DDDW column. 
(I'm not going to try to get so fancy that they have to click on the arrow) 
Code in the RowFocusChanging event if the currentrow is the DDDW row set the 
protect column to 0 in currentrow 
Code in the ItemFocusChanged event to set the protect column to 1 if the new 
dwo.name is not your DDDW column. 
Code in the pbm_dwnkey user event to catch the KeyDownArrow! and do whatever 
you want 

You could also leave the expression alone - protecting the DDDW column will 
keep them from tabbing into it and they will have to use the mouse. Then set 
the protect column as above and code in the ItemChanged event if dwo.name = 
DDDW column and protect column = 1 then Beep(1) and return 1. In this case 
you might want to turn off protection if they key something other than down 
arrow. 

*/
end event

type p_history from picture within u_checklist
integer x = 5
integer y = 740
integer width = 110
integer height = 356
string picturename = "History.gif"
boolean border = true
boolean focusrectangle = false
end type

event clicked;long ll_return



IF tab_checklist.tabpage_checklist_history.visible = False OR tab_checklist.SelectedTab <> 3 Then
	tab_checklist.SelectedTab = 3
End If
IF ib_maximized = False Then
	p_right_arrow.visible =True
	p_left_arrow.visible = False
	ll_return = uf_maximized()
END IF
end event

type p_notes from picture within u_checklist
integer x = 5
integer y = 444
integer width = 110
integer height = 300
string picturename = "Notes.gif"
boolean border = true
boolean focusrectangle = false
end type

event clicked;long ll_return



IF tab_checklist.tabpage_checklist_notes.visible = False OR tab_checklist.SelectedTab <> 2 Then
	tab_checklist.SelectedTab = 2
End If
IF ib_maximized = False Then
	p_right_arrow.visible =True
	p_left_arrow.visible = False
	ll_return = uf_maximized()
END IF
end event

type p_checklist from picture within u_checklist
event ue_mousemove pbm_mousemove
integer x = 5
integer y = 92
integer width = 110
integer height = 356
string picturename = "Checklist.gif"
boolean border = true
boolean focusrectangle = false
end type

event clicked;Long ll_return


IF tab_checklist.tabpage_checklist.visible = False OR tab_checklist.SelectedTab <> 1 Then
	tab_checklist.SelectedTab = 1
End If
IF ib_maximized = False Then
	p_right_arrow.visible =True
	p_left_arrow.visible = False
	ll_return = uf_maximized()
END IF	
end event

type tab_checklist from tab within u_checklist
event ue_mousemove pbm_mousemove
event ue_post_constructor ( )
event create ( )
event destroy ( )
integer x = 119
integer width = 2062
integer height = 2224
integer taborder = 10
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
boolean fixedwidth = true
boolean raggedright = true
boolean focusonbuttondown = true
boolean showpicture = false
boolean boldselectedtext = true
tabposition tabposition = tabsonbottom!
integer selectedtab = 1
tabpage_checklist tabpage_checklist
tabpage_checklist_notes tabpage_checklist_notes
tabpage_checklist_history tabpage_checklist_history
end type

event ue_post_constructor();f_user_id(is_user_id)
If is_user_id = "         " THEN
	MessageBox("System Error","Could not determine User Id. Application Terminating. Call Help Desk.",StopSign!)
	Return
End If

end event

on tab_checklist.create
this.tabpage_checklist=create tabpage_checklist
this.tabpage_checklist_notes=create tabpage_checklist_notes
this.tabpage_checklist_history=create tabpage_checklist_history
this.Control[]={this.tabpage_checklist,&
this.tabpage_checklist_notes,&
this.tabpage_checklist_history}
end on

on tab_checklist.destroy
destroy(this.tabpage_checklist)
destroy(this.tabpage_checklist_notes)
destroy(this.tabpage_checklist_history)
end on

event constructor;
this.postevent("ue_post_constructor")
end event

event selectionchanging;LONG      ll_return, ll_checklist_no, ll_checklist_step_no_on_window, ll_row
STRING    ls_string, ls_checklist_type
INTEGER   li_rtn
	
	CHOOSE CASE oldindex
		CASE	1
			IF newindex = 2 THEN
				IF idw_dw[1].RowCount() < 1 THEN
					Return -1
				END IF	
				
				ll_row = 	idw_dw[1].GetSelectedRow(0)
				//select the same row as the checklist row
				idw_dw[2].SelectRow(0, false)
				idw_dw[2].SelectRow(ll_row, true)
				
				ll_checklist_no = idw_dw[1].getitemnumber(ll_row,"checklist_no")
				ll_checklist_step_no_on_window = idw_dw[1].getitemnumber(ll_row,"checklist_step_no")
				
				//Select the corresponding row in the notes tab.
				IF idw_dw[5].RowCount() > 1 THEN
					ll_return = inv_checklist.nf_retrieve_notes_checklist(ll_checklist_no,ll_checklist_step_no_on_window)
				END IF
				
				//Select the corresponding row in the notes tab.
				ll_return = inv_checklist.nf_set_notes_row(ll_checklist_no, ll_checklist_step_no_on_window,ll_row)
				
			END IF
			
		CASE 2
			// if there is an unsaved checklist note or checklist step note, prevent tab page change
			li_rtn = uf_prevent_tabpage_change()
			IF li_rtn = 1 THEN RETURN li_rtn
			
		CASE 3
			IF newindex = 2 THEN
				IF idw_dw[5].RowCount() < 1 THEN
					Return -1
				END IF	
				ll_row = 	idw_dw[5].GetSelectedRow(0)
				IF ll_row = 0 THEN
					ll_row = 1
					idw_dw[5].SelectRow(ll_row, true)
				END IF
			
				ll_checklist_no = idw_dw[5].getitemnumber(ll_row,"checklist_no")
				ll_checklist_step_no_on_window = idw_dw[5].getitemnumber(ll_row,"checklist_step_no")
				//ls_checklist_type = idw_dw[4].getitemstring(idw_dw[4].getrow(),"checklist_type_code")
				
				//Select the corresponding row in the notes tab.
				IF idw_dw[5].RowCount() > 1 THEN
					ll_return = inv_checklist.nf_retrieve_notes_checklist(ll_checklist_no,ll_checklist_step_no_on_window)
				END IF
				ll_return = inv_checklist.nf_set_notes_row(ll_checklist_no, ll_checklist_step_no_on_window,ll_row)

				//Select the same row as the history row
				idw_dw[2].SelectRow(0, false)
				idw_dw[2].SelectRow(ll_row, true)
			
			END IF	
			
			
	END CHOOSE
	
	CHOOSE CASE newindex
		CASE 1
			idw_dw[1].Setfocus()

		CASE 2
			IF idw_dw[2].getitemstring(ll_row,"checklist_step_status_code") <> 'XSM' THEN
				tab_checklist.tabpage_checklist_notes.tab_comment.tabpage_4.enabled = False
				tab_checklist.tabpage_checklist_notes.tab_comment.tabpage_4.visible = False
				tab_checklist.tabpage_checklist_notes.tab_comment.SelectedTab = 1
			ELSEIF  idw_dw[2].getitemstring(ll_row,"checklist_step_status_code") = 'XSM' THEN
				tab_checklist.tabpage_checklist_notes.tab_comment.tabpage_4.enabled = True
				tab_checklist.tabpage_checklist_notes.tab_comment.tabpage_4.visible = True
			END IF			
								

		CASE	3
			IF idw_dw[5].GetSelectedRow(0) = 0 THEN
				idw_dw[5].SelectRow(1, true)
//				idw_dw[5].SetColumn("checklist_step_type_desc")
				idw_dw[5].SetFocus()
			END IF
			
			
	END CHOOSE
	
	
	Return 0
end event

event selectionchanged;	
	IF newindex = 1 THEN
		idw_dw[1].setfocus()
	ELSEIF newindex = 2 THEN
		idw_dw[2].setfocus()
	ELSEIF newindex = 3 THEN
		idw_dw[5].setfocus()
	END IF
end event

type tabpage_checklist from userobject within tab_checklist
event create ( )
event destroy ( )
integer x = 18
integer y = 16
integer width = 2025
integer height = 2108
long backcolor = 16777215
string text = "Checklist"
long tabtextcolor = 33554432
long tabbackcolor = 16777215
long picturemaskcolor = 536870912
dw_checklist dw_checklist
uo_checklistbar uo_checklistbar
dw_checklist_master dw_checklist_master
end type

on tabpage_checklist.create
this.dw_checklist=create dw_checklist
this.uo_checklistbar=create uo_checklistbar
this.dw_checklist_master=create dw_checklist_master
this.Control[]={this.dw_checklist,&
this.uo_checklistbar,&
this.dw_checklist_master}
end on

on tabpage_checklist.destroy
destroy(this.dw_checklist)
destroy(this.uo_checklistbar)
destroy(this.dw_checklist_master)
end on

type dw_checklist from u_checklist_datawindow within tabpage_checklist
event ue_post_constructor ( )
event ue_add_comment ( long al_checklist_step )
integer y = 532
integer width = 2025
integer height = 1564
integer taborder = 30
string title = "none"
string dataobject = "d_checklist"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event ue_post_constructor();String ls_checklist_type_desc
Long ll_row

ll_row =  THIS.GetRow()

IF ll_row > 0 THEN

	is_checklist_type_code = this.getitemstring(ll_row,"checklist_type_code")
	ls_checklist_type_desc = uf_get_checklist_type_desc(is_checklist_type_code)
	
	tab_checklist.tabpage_checklist.uo_checklistbar.st_checklist_name.Text = ls_checklist_type_desc
END IF

end event

event ue_add_comment(long al_checklist_step);

inv_checklist.nf_cancel_checklist_step( il_checklist_no, il_step_no)
end event

event buttonclicking;INTEGER	li_next_checklist_step_no, li_current_checklist_step_no
LONG		ll_checklist_no, ll_return

ll_checklist_no = this.getitemnumber(row,'checklist_no')
li_current_checklist_step_no = THIS.GetItemNumber(row,'checklist_step_no')

li_next_checklist_step_no = inv_checklist.nf_get_next_checklist_step(ll_checklist_no)

IF ib_step_order = true THEN
	IF li_next_checklist_step_no < li_current_checklist_step_no THEN
		THIS.ScrollToRow(li_next_checklist_step_no)
		THIS.SelectRow(li_next_checklist_step_no,TRUE)
		MessageBox('Step Skipped','You cannot open the module for any step other than the "current" step.',Exclamation!)
		RETURN 1
	ELSEIF li_next_checklist_step_no > li_current_checklist_step_no THEN
		THIS.ScrollToRow(li_next_checklist_step_no)
		THIS.SelectRow(li_next_checklist_step_no,TRUE)
		MessageBox('Step Complete','You cannot return to a module for a step that has already been completed.',Exclamation!)
		RETURN 1
	END IF
END IF

TRIGGER EVENT ue_checklist_buttonclicking(THIS,row,dwo,ll_return)

RETURN ll_return
end event

event constructor;
this.PostEvent("ue_post_constructor")
end event

event itemchanged;call super::itemchanged;INTEGER        li_return
LONG			   ll_checklist_no, ll_checklist_step_no_on_window, ll_response, ll_row
LONG	         ll_min_incomplete_step_no, ll_last_checklist_step_no, ll_rtn, ll_step_no
String			ls_checklist_step_type_code, ls_status_assigned_method_code, ls_status_assigned_method_desc, ls_checklist_step_type_desc

S_MESSAGE lstr_message
U_CHECKLIST_DATAWINDOW ldw_dw
Int li_row

ldw_dw = THIS

dwitemstatus ldwis
ldwis = THIS.GetItemStatus(row,0,Primary!)

ll_checklist_no                = THIS.GetItemNumber(row,"checklist_no")
ll_checklist_step_no_on_window = THIS.GetItemNumber(row,"checklist_step_no")
ls_checklist_step_type_code    = THIS.GetItemString(row,'checklist_step_type_code')

IF dwo.Name = "checklist_step_status_code" THEN

	// check that the chosen step status is valid
	IF is_status_assigned_method_code = '' THEN
		TRIGGER EVENT ue_checklist_step_status_changed(ldw_dw,row,dwo,data,ls_status_assigned_method_code,ll_rtn)
	ELSE
		// set through triggerevent on containing module
		ls_status_assigned_method_code = is_status_assigned_method_code
	END IF
	IF ll_rtn <> 0 THEN
		RETURN ll_rtn
	END IF
	
	// ls_status_assigned_method_code is returned to be used in function call below
	
	li_return = inv_checklist.nf_checklist_step_type_status_xref(is_module_code,is_checklist_type_code,ls_checklist_step_type_code,data,ls_status_assigned_method_code,ll_checklist_no)
	IF li_return = 0 THEN
		THIS.SetItemStatus(row,0,Primary!,NotModified!)
		RETURN 1
	END IF
	// reset instance variable
	is_status_assigned_method_code = ''
	

	ll_min_incomplete_step_no = inv_checklist.nf_get_next_checklist_step(ll_checklist_no)
	
	IF ib_step_order = TRUE THEN
		//	A mandatory checklist item must be checked complete before the next item can be selected.
		IF  ll_checklist_step_no_on_window > ll_min_incomplete_step_no THEN
			CHOOSE CASE data
				CASE 'COM', 'COA', 'NRA', 'NRM', 'XSM'
					Messagebox('Error','Each checklist item must be concluded before the next item can be selected.',Exclamation!)				
					inv_checklist.nf_retrieve_checklists( is_checklist_type_code, ll_checklist_no)
					// call the routine to select the current row.
					ll_step_no = inv_checklist.nf_get_next_checklist_step(ll_checklist_no)
					li_row = idw_dw[1].uf_find_row('checklist_step_no',string(ll_step_no))
					Return 1
			END CHOOSE
		END IF
	END IF


	//If the last step is checked complete then save the data to the CHECKLIST table
	Select checklist_step_no
	Into :ll_last_checklist_step_no
	From CHECKLIST_STEP
	Where checklist_no = :ll_checklist_no
	And checklist_step_no = (Select Max(checklist_step_no)
									 From   CHECKLIST_STEP
									 Where checklist_no = :ll_checklist_no)
	Using  itr_trans_object;
	itr_trans_object.nf_handle_error('w_checklist','dw_checklist.clicked','Select From CHECKLIST_STEP')

	IF ll_checklist_step_no_on_window = ll_last_checklist_step_no THEN
		IF data = 'XSM' THEN 
			Messagebox('Error','The last step can not be cancelled.',Exclamation!)
			inv_checklist.nf_retrieve_checklists( is_checklist_type_code, ll_checklist_no)
			Return 1
		END IF

	ELSE
		IF data = 'XSM' THEN 
			ll_response = Messagebox('Question','Are you sure you want to Cancel this checklist step?',Question!,YesNo!,2)
			IF ll_response = 1 THEN
				Messagebox('Notification','You are required to enter a checklist comment when cancelling.',Information!)
				//check for a comment, cancel all remaining steps and then save both datawindows.
				Open(w_cancel_comment)
				//If the user decided to cancel the cancel then put them back on the status.
     		   lstr_message = Message.PowerObjectParm
			   IF lstr_message.as_stringparm[1] = 'cancel' THEN
					inv_checklist.nf_retrieve_checklists( is_checklist_type_code, ll_checklist_no)
				   Return 1
				ELSEIF lstr_message.as_stringparm[1] = 'save' THEN
					ll_row = idw_dw[1].GetRow()
					
					//set the checklist_step to cancelled
					idw_dw[1].SetItem(ll_row,'checklist_step_status_code',data)
					idw_dw[1].SetItem(ll_row,'concluded_by_user_id',is_user_id)
					idw_dw[1].SetItem(ll_row,'concluded_date',uf_get_datetime())
					idw_dw[1].SetItem(ll_row,'cancelled_comment',lstr_message.as_stringparm[2])
					
					idw_dw[1].setitemstatus(ll_row,0,Primary!,Datamodified!)
					
					// opening of window above has triggered uf_trigger_itemchangeaccepted
					// before any change has taken place, so it must be posted again
					post function uf_trigger_itemchangeaccepted(THIS,ll_row,"checklist_step_status_code",data)

			    END IF
	
				tab_checklist.tabpage_checklist_notes.cb_save.enabled = FALSE

			ELSE
				inv_checklist.nf_retrieve_checklists( is_checklist_type_code, ll_checklist_no)
				Return 1
			END IF
		END IF
	END IF
	
END IF

ldwis = THIS.GetItemStatus(row,0,Primary!)

TRIGGER EVENT ue_checklist_itemchanged(ldw_dw,row,dwo,data,ll_rtn)

ldwis = THIS.GetItemStatus(row,0,Primary!)

RETURN ll_rtn
end event

event itemerror;Return 1
end event

event rowfocuschanged;DATETIME    ldtm_checklist_step_concluded_date
LONG        ll_checklist_no, ll_checklist_step_no, ll_checklist_step_no_on_window, ll_return
STRING      ls_step_type_code, ls_step_status_code
U_CHECKLIST_DATAWINDOW	ldw_dw

IF This.RowCount() < 1 OR currentrow < 1 THEN
	Return 0
END IF	

ll_checklist_no = this.getitemnumber(currentRow,"checklist_no")
ll_checklist_step_no_on_window = this.getitemnumber(currentRow,"checklist_step_no")
ls_step_type_code = this.getitemString(currentRow,"checklist_step_type_code")

This.SelectRow(0, false)
This.SelectRow(currentRow, true)

ldtm_checklist_step_concluded_date = THIS.GetItemDateTime(currentrow,'concluded_date')
IF IsNull(ldtm_checklist_step_concluded_date) THEN
	inv_checklist.nf_filter_checklist_step_status(is_module_code, is_checklist_type_code, ls_step_type_code)
END IF

//Select the corresponding row in the notes tab.
//IF ib_notes_row_set = FALSE THEN
//	ib_notes_row_set = TRUE
//	inv_checklist.post nf_set_notes_row(ll_checklist_no, ll_checklist_step_no_on_window,currentRow)
//END IF
//
ldw_dw = this

Trigger Event ue_checklist_rowfocuschanged(ldw_dw,currentrow)

end event

event ue_itemchangeaccepted;call super::ue_itemchangeaccepted;DATETIME   ldtm_server_datetime
STRING     ls_cancelled_comment, ls_checklist_status_code, ls_status_assigned_method_code
LONG       ll_checklist_no, ll_last_checklist_step_no, ll_checklist_step_no_on_window, ll_return
LONG       ll_row,ll_rtn
U_CHECKLIST_DATAWINDOW ldw_dw
DWObject   l_dwo


//messagebox('ancestor','itemchanged accepted - begin')
dwitemstatus ldwis
ldwis = THIS.GetItemStatus(al_row,0,Primary!)

ldw_dw = THIS

ll_checklist_no = this.getitemnumber(al_row,"checklist_no")
ll_checklist_step_no_on_window = this.getitemnumber(al_row,"checklist_step_no")
		
//If the last step is checked complete then save the data to the CHECKLIST table
Select checklist_step_no
Into :ll_last_checklist_step_no
From CHECKLIST_STEP
Where checklist_no = :ll_checklist_no
And checklist_step_no = (Select Max(checklist_step_no)
								 From   CHECKLIST_STEP
								 Where checklist_no = :ll_checklist_no)
Using  itr_trans_object;
itr_trans_object.nf_handle_error('w_checklist','dw_checklist.clicked','Select From CHECKLIST_STEP')

// get current date time
ldtm_server_datetime = uf_get_datetime()

IF data <> 'XCA' THEN
	
	THIS.SetItem(al_row,'concluded_date',ldtm_server_datetime)
	THIS.SetItem(al_row,'concluded_by_user_id',is_user_id)
	THIS.SetItem(al_row,'checklist_step_status_code',data)
	
	IF ll_checklist_step_no_on_window = ll_last_checklist_step_no THEN
		idw_dw[7].SetItem(1,'checklist_status_code','CA')
		idw_dw[7].SetItem(1,'concluded_date',ldtm_server_datetime)
		idw_dw[7].SetItem(1,'concluded_by_user_id',is_user_id)
		
		ll_row = 1
		l_dwo = tab_checklist.tabpage_checklist.dw_checklist_master.Object.checklist_status_code
		ls_checklist_status_code = 'CA'
		ls_status_assigned_method_code = 'A'
		
		TRIGGER event ue_checklist_master_itemchanged(ldw_dw,ll_row,l_dwo,ls_checklist_status_code,ls_status_assigned_method_code,ll_rtn)
				
		idw_dw[7].Update()
		itr_trans_object.nf_handle_error('tab_checklist.tabpage_checklist.dw_checklist','ue_itemchangedaccepted','idw_dw[7].Update')
	
		idw_dw[7].Retrieve(ll_checklist_no)
		itr_trans_object.nf_handle_error('tab_checklist.tabpage_checklist.dw_checklist','ue_itemchangedaccepted','idw_dw[7].Retrieve')
	END IF
	
END IF

THIS.Update()
itr_trans_object.nf_handle_error('tab_checklist.tabpage_checklist.dw_checklist','ue_itemchangedaccepted','idw_dw[1].Update')

IF data = 'INA' Then
	this.setitem(al_row,"step_checked",'N')
ELSE			
	this.setitem(al_row,"step_checked",'Y')
END IF

//messagebox('ancestor','itemchanged accepted - begin')

ldwis = THIS.GetItemStatus(al_row,0,Primary!)

TRIGGER EVENT ue_checklist_itemchangeaccepted(ldw_dw,al_row,as_column_name,data)
 
end event

event buttonclicked;call super::buttonclicked;TRIGGER EVENT ue_checklist_buttonclicked(THIS,row,actionreturncode,dwo)
end event

event rbuttondown;//
end event

type uo_checklistbar from u_titlebar within tabpage_checklist
event destroy ( )
integer width = 2021
integer taborder = 10
end type

on uo_checklistbar.destroy
call u_titlebar::destroy
end on

type dw_checklist_master from u_checklist_datawindow within tabpage_checklist
integer y = 104
integer width = 2025
integer height = 424
integer taborder = 20
string title = "none"
string dataobject = "d_checklist_master"
boolean hscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;INTEGER                  li_find, li_rows, li_rtn
DATETIME                 ldtm_server_datetime
LONG                     ll_checklist_no, ll_rtn
STRING                   ls_data, ls_find, ls_status_assigned_method_code, ls_status_assigned_method_desc
s_message                lstr_message
u_checklist_datawindow   ldw_dw
dwobject                 ldw_dwo
DWItemStatus             ldwis


ll_checklist_no = this.getitemnumber(row,'checklist_no')

IF dwo.Name = 'checklist_status_code' THEN
		
	// check that the chosen status is valid
	TRIGGER EVENT ue_checklist_master_itemchanged(ldw_dw,row,dwo,data,ls_status_assigned_method_code,ll_rtn)
	IF ll_rtn = 1 THEN
		idw_dw[7].Retrieve(ll_checklist_no)
		itr_trans_object.nf_handle_error('u_checklist','dw_checklist_master','itemchanged')
		idw_dw[7].Setcolumn('checklist_status_code')
		idw_dw[7].setitem(row,'checklist_status_code','IA')
		RETURN 1
	END IF
	
	// ls_status_assigned_method_code is returned to be used in function call below
	
	li_rtn = inv_checklist.nf_checklist_type_status_xref(is_module_code,is_checklist_type_code,data,ls_status_assigned_method_code,ll_checklist_no)
	IF li_rtn = 0 THEN
		RETURN 1
	END IF
	
	// Cancelled manually
	IF data = 'XM' THEN
		li_rtn = Messagebox('Question','Are you sure you want to Cancel this checklist?',Question!,YesNo!,2)
		IF li_rtn = 1 THEN
			Messagebox('Notification','You are required to enter a checklist comment when cancelling.',Information!)
			//check for a comment, cancel all remaining steps and then save both datawindows.
			Open(w_cancel_comment)
			
			//If the user decided to cancel the cancel then put them back on the status.
     		lstr_message = Message.PowerObjectParm
			IF lstr_message.as_stringparm[1] = 'cancel' THEN
				itr_trans_object.nf_rollback_transaction()
				
				idw_dw[7].Retrieve(ll_checklist_no)
				itr_trans_object.nf_handle_error('u_checklist','dw_checklist_master','itemchanged')
				idw_dw[7].Setcolumn('checklist_status_code')
				idw_dw[7].setitem(row,'checklist_status_code','IA')
				RETURN 1
			ELSEIF lstr_message.as_stringparm[1] = 'save' THEN
				ldtm_server_datetime = uf_get_datetime()
								
				THIS.SetItem(1,'checklist_status_code','XM')
				THIS.SetItem(1,'cancelled_comment',lstr_message.as_stringparm[2])
				
				is_cancelled_comment = lstr_message.as_stringparm[2]
				
				// opening of window above has triggered uf_trigger_itemchangeaccepted
				// before any change has taken place, so it must be posted again
				trigger function uf_trigger_itemchangeaccepted(THIS,1,"checklist_status_code",data)

				li_rows = idw_dw[1].rowcount()
				
				ls_find = 'checklist_step_status_code = "INA"'
				li_find = idw_dw[1].Find(ls_find,0,li_rows)
				DO WHILE li_find > 0
					idw_dw[1].SetItem(li_find,'checklist_step_status_code','XCA')
					idw_dw[1].SetItem(li_find,'concluded_date',ldtm_server_datetime)
					idw_dw[1].SetItem(li_find,'concluded_by_user_id',is_user_id)
					li_find = idw_dw[1].Find(ls_find,li_find+1,li_rows)
				LOOP
				
				li_rtn = idw_dw[1].Update()
				itr_trans_object.nf_handle_error('u_checklist','itemchanged','dw_checklist.update')
									
				idw_dw[1].setitemstatus(li_rows,0,Primary!,Datamodified!)
			
				// opening of window above has triggered uf_trigger_itemchangeaccepted
				// before any change has taken place, so it must be posted again
				post function uf_trigger_itemchangeaccepted(idw_dw[1],li_rows,"checklist_step_status_code",'XCA')
		
				ldw_dw = idw_dw[1]
				ldwis = idw_dw[1].GetItemStatus(row,0,Primary!)
				ldw_dwo = ldw_dw.Object.checklist_step_status_code
				TRIGGER EVENT ue_checklist_itemchanged(ldw_dw,li_rows,ldw_dwo,'XCA',ll_rtn)
	
				ldwis = idw_dw[1].GetItemStatus(row,0,Primary!)
				
				RETURN ll_rtn
				
			END IF
		ELSE
			// response = 1, chose not to cancel
			idw_dw[7].setitem(row,'checklist_status_code','IA')
			RETURN 1
		END IF

	ELSEIF data = 'XA' THEN  //cancelled automatically
		idw_dw[7].setitem(row,'checklist_status_code','XA')
		idw_dw[7].Update()
		itr_trans_object.nf_handle_error('u_checklist','dw_checklist_master','idw_dw[7].Update')
		
		
		Update CHECKLIST_STEP
		Set    checklist_step_status_code = 'XCA', concluded_by_user_id = :is_user_id, concluded_date = :ldtm_server_datetime
		Where  checklist_no = :ll_checklist_no
		And    checklist_step_status_code = 'INA'
		Using itr_trans_object;
		
		itr_trans_object.nf_handle_error('u_checklist','dw_checklist_master','Update CHECKLIST STEP')
		
		//set the checklist_steps to cancelled
		idw_dw[1].Retrieve(ll_checklist_no)
		itr_trans_object.nf_handle_error('u_checklist','dw_checklist_master','itemchanged')
		idw_dw[7].Retrieve(ll_checklist_no)
		itr_trans_object.nf_handle_error('u_checklist','dw_checklist_master','itemchanged')
		
	ELSE
		idw_dw[7].Retrieve(ll_checklist_no)
		itr_trans_object.nf_handle_error('u_checklist','dw_checklist_master','itemchanged')
		idw_dw[7].Setcolumn('checklist_status_code')
		RETURN 1
	END IF

END IF


// ue_checklist_status_changed
end event

event constructor;call super::constructor;post uf_set_window(iw_parent)
end event

event rbuttondown;//
end event

event ue_itemchangeaccepted;call super::ue_itemchangeaccepted;INTEGER   li_rtn

THIS.SetItem(1,'concluded_date',uf_get_datetime())
THIS.SetItem(1,'concluded_by_user_id',is_user_id)

li_rtn = THIS.Update()
itr_trans_object.nf_handle_error('u_checklist','ue_itemchangeaccepted','dw_checklist_master.update')
IF li_rtn < 0 THEN
	MessageBox('Update Error','The cancellation of the checklist did not complete. Contact the HELPDESK.',Exclamation!)
END IF

TRIGGER EVENT ue_checklist_master_itemchangeaccepted(al_row,as_column_name,data)
 
end event

type tabpage_checklist_notes from userobject within tab_checklist
event create ( )
event destroy ( )
integer x = 18
integer y = 16
integer width = 2025
integer height = 2108
long backcolor = 16777215
string text = "Notes"
long tabtextcolor = 33554432
long tabbackcolor = 16777215
long picturemaskcolor = 536870912
cb_cancel cb_cancel
dw_checklist_notes_master dw_checklist_notes_master
dw_checklist_notes dw_checklist_notes
uo_notesbar uo_notesbar
cb_save cb_save
tab_comment tab_comment
end type

on tabpage_checklist_notes.create
this.cb_cancel=create cb_cancel
this.dw_checklist_notes_master=create dw_checklist_notes_master
this.dw_checklist_notes=create dw_checklist_notes
this.uo_notesbar=create uo_notesbar
this.cb_save=create cb_save
this.tab_comment=create tab_comment
this.Control[]={this.cb_cancel,&
this.dw_checklist_notes_master,&
this.dw_checklist_notes,&
this.uo_notesbar,&
this.cb_save,&
this.tab_comment}
end on

on tabpage_checklist_notes.destroy
destroy(this.cb_cancel)
destroy(this.dw_checklist_notes_master)
destroy(this.dw_checklist_notes)
destroy(this.uo_notesbar)
destroy(this.cb_save)
destroy(this.tab_comment)
end on

type cb_cancel from commandbutton within tabpage_checklist_notes
integer x = 1669
integer y = 2012
integer width = 343
integer height = 92
integer taborder = 60
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean enabled = false
string text = "cancel"
end type

event clicked;/*idw_dw[1] = tab_checklist.tabpage_checklist.dw_checklist
idw_dw[2] = tab_checklist.tabpage_checklist_notes.dw_checklist_notes
idw_dw[3] = tab_checklist.tabpage_checklist_notes.tab_comment.tabpage_3.dw_checklist_cancelled_comment_tab
idw_dw[4] = tab_checklist.tabpage_checklist_history.dw_checklist_history_master
idw_dw[5] = tab_checklist.tabpage_checklist_history.dw_checklist_history_detail
idw_dw[6] = tab_checklist.tabpage_checklist_notes.dw_checklist_notes_master
idw_dw[7] = tab_checklist.tabpage_checklist.dw_checklist_master
idw_dw[8] = tab_checklist.tabpage_checklist_notes.tab_comment.tabpage_1.dw_checklist_step_notes_entered_tab
idw_dw[9] = tab_checklist.tabpage_checklist_notes.tab_comment.tabpage_2.dw_checklist_notes_entered_tab
idw_dw[10] = tab_checklist.tabpage_checklist_notes.tab_comment.tabpage_4.dw_checklist_cancelled_step_comment_tab*/

INTEGER   li_rtn
Long ll_checklist_step_no, ll_original_row
Dwitemstatus  ls_status

li_rtn = idw_dw[8].Accepttext()
li_rtn = idw_dw[9].Accepttext()


ll_original_row = idw_dw[2].getselectedrow(0)
IF ll_original_row > 0 THEN
	ll_checklist_step_no = idw_dw[2].GetItemNumber(ll_original_row,"checklist_step_no")
	il_checklist_no = idw_dw[2].GetItemNumber(ll_original_row,"checklist_no")
	
	//Step Comment
	ls_status =  idw_dw[8].getItemStatus(idw_dw[8].getrow(),'step_comment',Primary!)
	IF idw_dw[8].getItemStatus(idw_dw[8].getrow(),'step_comment',Primary!) = DataModified! THEN
		li_rtn = idw_dw[8].Retrieve(il_checklist_no,ll_checklist_step_no)
		itr_trans_object.nf_handle_error('w_checklist','tab_checkist.tabpage_checklist_notes.cb_cancel','idw_dw[8].Retrieve()')
	END IF
ELSE
	il_checklist_no = idw_dw[1].GetItemNumber(1,"checklist_no")
END IF

//Checklist Comment
ls_status = idw_dw[9].getItemStatus(idw_dw[9].getrow(),'checklist_comment',Primary!)
IF idw_dw[9].getItemStatus(idw_dw[9].getrow(),'checklist_comment',Primary!) = DataModified! THEN
	li_rtn = idw_dw[9].Retrieve(il_checklist_no)
	itr_trans_object.nf_handle_error('w_checklist','tab_checkist.tabpage_checklist_notes.cb_cancel','idw_dw[8].Retrieve()')
END IF

idw_dw[3].SetItemStatus(1,0,Primary!,NotModified!)

idw_dw[10].SetItemStatus(1,0,Primary!,NotModified!)

idw_dw[9].SetItemStatus(1,0,Primary!,NotModified!)

idw_dw[8].SetItemStatus(1,0,Primary!,NotModified!)


this.enabled = FALSE
tab_checklist.tabpage_checklist_notes.cb_save.enabled = FALSE
end event

type dw_checklist_notes_master from u_checklist_datawindow within tabpage_checklist_notes
integer y = 104
integer width = 2025
integer height = 424
integer taborder = 50
string title = "none"
string dataobject = "d_checklist_notes_master"
boolean hscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event clicked;Datetime ldtm_concluded_date, ldtm_not_required_date
Long ll_notes_row, ll_checklist_step_no, ll_checklist_no
String ls_step_status_code

IF row < 1 THEN Return -1

IF dwo.Name = 'p_comment' THEN
// Open the popup and display the checklist comment.
END IF


end event

event rbuttondown;//
end event

type dw_checklist_notes from u_checklist_datawindow within tabpage_checklist_notes
event ue_display_detail pbm_dwnlbuttondblclk
event ue_post_constructor ( )
integer y = 532
integer width = 2025
integer height = 932
integer taborder = 50
string title = "none"
string dataobject = "d_checklist_notes"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event ue_post_constructor();String ls_checklist_type_desc

//IF THIS.GetRow() > 0 THEN
	
//IF idw_dw[DW_CHECKLIST_NOTES].GetRow() > 0 THEN
//	is_checklist_type_code = this.getitemstring(this.getrow(),"checklist_type_code")
//	ls_checklist_type_desc = wf_get_checklist_type_desc(is_checklist_type_code)
//	
//	tab_checklist.tabpage_checklist_notes.uo_notes.st_checklist_name.Text = ls_checklist_type_desc
//END IF
end event

event constructor;this.PostEvent("ue_post_constructor")
end event

event rbuttondown;

uf_menu(this)

end event

event rowfocuschanged;call super::rowfocuschanged;
Long ll_checklist_step_no, ll_checklist_no, ll_return
String ls_step_status_code, ls_comment

IF currentrow < 1 THEN Return -1

ls_step_status_code = THIS.GetItemString(currentrow,'checklist_step_status_code')
ll_checklist_no =  THIS.GetItemNumber(currentrow,'checklist_no')
ll_checklist_step_no = THIS.GetItemNumber(currentrow,'checklist_step_no')


THIS.SelectRow(0,FALSE)
THIS.SelectRow(currentrow,TRUE)

idw_dw[8].Retrieve(ll_checklist_no, ll_checklist_step_no)
itr_trans_object.nf_handle_error('w_checklist','dw_checklist_notes.clicked','dw_checklist_notes_entered.Retrieve')

idw_dw[8].ScrollToRow(currentrow)
idw_dw[8].visible = TRUE

//Checks to see if the status is incompleted
IF ls_step_status_code = 'INA' THEN
	idw_dw[8].Enabled = TRUE
	idw_dw[8].SetFocus()
ELSEIF ls_step_status_code = 'XSM' THEN
	idw_dw[8].Enabled = FALSE
	idw_dw[10].Retrieve(ll_checklist_no, ll_checklist_step_no)
	itr_trans_object.nf_handle_error('w_checklist','dw_checklist_notes.clicked','dw_checklist_notes_entered.Retrieve')

   tab_checklist.tabpage_checklist_notes.tab_comment.tabpage_4.enabled = TRUE
	tab_checklist.tabpage_checklist_notes.tab_comment.tabpage_4.visible = TRUE
	idw_dw[10].ScrollToRow(currentrow)
	idw_dw[10].Visible = TRUE
ELSE	
	idw_dw[8].Enabled = FALSE
END IF

//Check to make sure the cancelled comment isn't displayed when it shouldn't be.
IF ls_step_status_code <> 'XSM' AND tab_checklist.tabpage_checklist_notes.tab_comment.tabpage_4.Visible = TRUE THEN
	tab_checklist.tabpage_checklist_notes.tab_comment.tabpage_4.Enabled = FALSE
	tab_checklist.tabpage_checklist_notes.tab_comment.tabpage_4.Visible = FALSE
	tab_checklist.tabpage_checklist_notes.tab_comment.SelectedTab = 1
END IF

end event

event rowfocuschanging;call super::rowfocuschanging;INTEGER li_rtn

// get status of all checklist notes
li_rtn = inv_checklist.nf_check_note_modified_status()

IF li_rtn < 0 THEN
	// prevent row from changing
	RETURN 1
ELSE
	// allow row to change
	RETURN 0
END IF




end event

type uo_notesbar from u_titlebar within tabpage_checklist_notes
event destroy ( )
integer width = 2021
integer taborder = 10
end type

on uo_notesbar.destroy
call u_titlebar::destroy
end on

type cb_save from commandbutton within tabpage_checklist_notes
integer x = 1298
integer y = 2012
integer width = 343
integer height = 92
integer taborder = 50
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean enabled = false
string text = "save"
boolean default = true
end type

event clicked;/*

idw_dw[1] = tab_checklist.tabpage_checklist.dw_checklist
idw_dw[2] = tab_checklist.tabpage_checklist_notes.dw_checklist_notes
idw_dw[3] = tab_checklist.tabpage_checklist_notes.tab_comment.tabpage_3.dw_checklist_cancelled_comment_tab
idw_dw[4] = tab_checklist.tabpage_checklist_history.dw_checklist_history_master
idw_dw[5] = tab_checklist.tabpage_checklist_history.dw_checklist_history_detail
idw_dw[6] = tab_checklist.tabpage_checklist_notes.dw_checklist_notes_master
idw_dw[7] = tab_checklist.tabpage_checklist.dw_checklist_master
idw_dw[8] = tab_checklist.tabpage_checklist_notes.tab_comment.tabpage_1.dw_checklist_step_notes_entered_tab
idw_dw[9] = tab_checklist.tabpage_checklist_notes.tab_comment.tabpage_2.dw_checklist_notes_entered_tab
idw_dw[10] = tab_checklist.tabpage_checklist_notes.tab_comment.tabpage_4.dw_checklist_cancelled_step_comment_tab

*/

Boolean       lb_cancel_checklist, lb_update = False
INTEGER       li_rowcount, li_find, li_trancount
String        ls_status_code, ls_checklist_type_code, ls_step_status_code, ls_comment, ls_find
Long          ll_checklist_step_no, ll_row, ll_notes_master_original_row, ll_row2, ll_rtn
DATETIME      ldtm_server_datetime
Dwitemstatus  ls_status

// allow module to prevent cancellation of saving of checklist note
TRIGGER EVENT ue_checklist_note_save_btn_clicking(ll_rtn)
IF ll_rtn < 0 THEN
	tab_checklist.tabpage_checklist_notes.cb_cancel.TriggerEvent(Clicked!)
	RETURN
END IF

idw_dw[1].Accepttext()
idw_dw[3].Accepttext()
idw_dw[8].Accepttext()
idw_dw[9].Accepttext()
idw_dw[10].Accepttext()


ll_notes_master_original_row = idw_dw[2].getselectedrow(0)

// STEP comment cannot be saved if a step is not selected
IF ll_notes_master_original_row > 0 THEN
	il_checklist_no = idw_dw[2].GetItemNumber(ll_notes_master_original_row,"checklist_no")
	ll_checklist_step_no = idw_dw[2].GetItemNumber(ll_notes_master_original_row,"checklist_step_no")
	
	//Step Comment
	ls_status =  idw_dw[8].getItemStatus(idw_dw[8].getrow(),'step_comment',Primary!)
	IF idw_dw[8].getItemStatus(idw_dw[8].getrow(),'step_comment',Primary!) = DataModified! THEN
		
		ll_row = idw_dw[8].getrow()
		ls_status_code = idw_dw[1].getitemstring(ll_row,'checklist_status_code')
		ls_comment = idw_dw[8].getitemstring(idw_dw[8].getrow(),'step_comment')
		
		ls_comment = f_clean_string_1(Trim(ls_comment))		
		idw_dw[8].SetItem(idw_dw[8].getrow(),'step_comment',f_clean_string_1(Trim(ls_comment)))

		IF ls_comment = '' OR Len(ls_comment) < 5 OR LEFT(ls_comment,1) = '' THEN
			Messagebox('Error','The comment cannot be blank, must be more than 5 characters and cannot start with a blank. Please revise.',Exclamation!)
			idw_dw[8].setcolumn("step_comment")
			idw_dw[8].setfocus()
			Return -1
		END IF

		itr_trans_object.nf_begin_transaction()
	
		idw_dw[8].Update()
		itr_trans_object.nf_handle_error('w_checklist','tab_checkist.tabpage_checklist_notes.cb_save','idw_dw[8].Update()')
		ls_comment = ''
		lb_update = true
	END IF
ELSE
	il_checklist_no = idw_dw[1].GetItemNumber(1,"checklist_no")
END IF
ls_checklist_type_code = idw_dw[1].getitemstring(idw_dw[1].getrow(),'checklist_type_code')


//Checklist Comment
ls_status = idw_dw[9].getItemStatus(idw_dw[9].getrow(),'checklist_comment',Primary!)
IF idw_dw[9].getItemStatus(idw_dw[9].getrow(),'checklist_comment',Primary!) = DataModified! THEN
	
	ll_row = idw_dw[9].getrow()
	ls_status_code = idw_dw[7].getitemstring(idw_dw[7].getrow(),'checklist_status_code')  // the checklist master dw
	ls_comment = idw_dw[9].getitemstring(idw_dw[9].getrow(),'checklist_comment')
	
	ls_comment = f_clean_string_1(Trim(ls_comment))
	idw_dw[9].SetItem(idw_dw[9].getrow(),'checklist_comment',f_clean_string_1(Trim(ls_comment)))		
		
	IF ls_comment = '' OR Len(ls_comment) < 5 OR LEFT(ls_comment,1) = '' THEN
		Messagebox('Error','The comment can not be blank, must be more than 5 characters and can not start with a blank. Please revise.',Exclamation!)
		idw_dw[9].setfocus()
		Return -1
	END IF

	itr_trans_object.nf_transaction_count(li_trancount,0,'','','',FALSE)
	IF li_trancount = 0 THEN
		itr_trans_object.nf_begin_transaction()
	END IF
	
	idw_dw[9].Update()
	itr_trans_object.nf_handle_error('w_checklist','tab_checkist.tabpage_checklist_notes.cb_save','idw_dw[9].Update()')
	lb_update = true
END IF


IF lb_update = True THEN

	itr_trans_object.nf_commit_transaction()
	
	lb_update = False

	// retrieve master
	idw_dw[6].Retrieve(il_checklist_no)
	itr_trans_object.nf_handle_error('w_checklist','tab_checkist.tabpage_checklist_notes.cb_save','idw_dw[6].Retrieve')	
	
	//inv_checklist.nf_retrieve_checklists(ls_checklist_type_code,il_checklist_no)
	idw_dw[2].Retrieve(il_checklist_no)
	itr_trans_object.nf_handle_error('w_checklist','tab_checkist.tabpage_checklist_notes.cb_save','idw_dw[2].Retrieve')
	
	IF ll_notes_master_original_row > 0 THEN
		idw_dw[2].SelectRow(1, false)
		idw_dw[2].SelectRow(ll_notes_master_original_row, True)
		idw_dw[2].ScrollToRow(ll_notes_master_original_row)
		
		idw_dw[8].Retrieve(il_checklist_no,ll_checklist_step_no)
		itr_trans_object.nf_handle_error('w_checklist','tab_checkist.tabpage_checklist_notes.cb_save','idw_dw[8].Retrieve')
		
		idw_dw[9].Retrieve(il_checklist_no)
		itr_trans_object.nf_handle_error('w_checklist','tab_checkist.tabpage_checklist_notes.cb_save','idw_dw[9].Retrieve')
		
		idw_dw[10].Retrieve(il_checklist_no,ll_checklist_step_no)
		itr_trans_object.nf_handle_error('w_checklist','tab_checkist.tabpage_checklist_notes.cb_save','idw_dw[10].Retrieve')
		
		idw_dw[3].Retrieve(il_checklist_no)
		itr_trans_object.nf_handle_error('w_checklist','tab_checkist.tabpage_checklist_notes.cb_save','idw_dw[3].Retrieve')
	END IF
	
	
	tab_checklist.tabpage_checklist_notes.tab_comment.tabpage_1.enabled = True
	tab_checklist.tabpage_checklist_notes.tab_comment.tabpage_2.enabled =True


	inv_checklist.nf_set_notes_saved_value(true)
	
END IF

inv_checklist.nf_set_notes_saved_value(false)

this.enabled = false
tab_checklist.tabpage_checklist_notes.cb_cancel.enabled = false
end event

type tab_comment from tab within tabpage_checklist_notes
event create ( )
event destroy ( )
integer x = 5
integer y = 1464
integer width = 2021
integer height = 532
integer taborder = 60
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
boolean raggedright = true
boolean focusonbuttondown = true
boolean boldselectedtext = true
integer selectedtab = 1
tabpage_1 tabpage_1
tabpage_2 tabpage_2
tabpage_4 tabpage_4
tabpage_3 tabpage_3
end type

on tab_comment.create
this.tabpage_1=create tabpage_1
this.tabpage_2=create tabpage_2
this.tabpage_4=create tabpage_4
this.tabpage_3=create tabpage_3
this.Control[]={this.tabpage_1,&
this.tabpage_2,&
this.tabpage_4,&
this.tabpage_3}
end on

on tab_comment.destroy
destroy(this.tabpage_1)
destroy(this.tabpage_2)
destroy(this.tabpage_4)
destroy(this.tabpage_3)
end on

event selectionchanging;INTEGER li_rtn

// if there is an unsaved checklist note or checklist step note, prevent tab page change
li_rtn = uf_prevent_tabpage_change()
IF li_rtn = 1 THEN RETURN li_rtn

end event

type tabpage_1 from userobject within tab_comment
event create ( )
event destroy ( )
integer x = 18
integer y = 100
integer width = 1984
integer height = 416
long backcolor = 16777215
string text = "Step Note"
long tabtextcolor = 33554432
long tabbackcolor = 16777215
long picturemaskcolor = 536870912
dw_checklist_step_notes_entered_tab dw_checklist_step_notes_entered_tab
end type

on tabpage_1.create
this.dw_checklist_step_notes_entered_tab=create dw_checklist_step_notes_entered_tab
this.Control[]={this.dw_checklist_step_notes_entered_tab}
end on

on tabpage_1.destroy
destroy(this.dw_checklist_step_notes_entered_tab)
end on

type dw_checklist_step_notes_entered_tab from u_checklist_datawindow within tabpage_1
integer width = 2021
integer height = 424
integer taborder = 10
string title = "none"
string dataobject = "d_checklist_step_notes_entered"
borderstyle borderstyle = stylelowered!
end type

event itemchanged;call super::itemchanged;	

//	IF data = '' OR Len(data) < 5 OR LEFT(data,1) = '' THEN
//		Messagebox('Error','The comment can not be blank, must be more than 5 characters and can not start with a blank. Please revise.',Exclamation!)
//		this.setcolumn("step_comment")
//		this.setfocus()
//	END IF

end event

event rbuttondown;//
end event

event editchanged;call super::editchanged;IF tab_checklist.tabpage_checklist.dw_checklist_master.GetRow() > 0 THEN
	tab_checklist.tabpage_checklist_notes.cb_save.enabled = True
	tab_checklist.tabpage_checklist_notes.cb_cancel.enabled = True
	THIS.SetItemStatus(1, 0, Primary!, DataModified!)
END IF
end event

type tabpage_2 from userobject within tab_comment
event create ( )
event destroy ( )
integer x = 18
integer y = 100
integer width = 1984
integer height = 416
long backcolor = 16777215
string text = "Checklist Note"
long tabtextcolor = 33554432
long tabbackcolor = 16777215
long picturemaskcolor = 536870912
dw_checklist_notes_entered_tab dw_checklist_notes_entered_tab
end type

on tabpage_2.create
this.dw_checklist_notes_entered_tab=create dw_checklist_notes_entered_tab
this.Control[]={this.dw_checklist_notes_entered_tab}
end on

on tabpage_2.destroy
destroy(this.dw_checklist_notes_entered_tab)
end on

type dw_checklist_notes_entered_tab from u_checklist_datawindow within tabpage_2
integer width = 20210
integer height = 416
integer taborder = 10
string title = "none"
string dataobject = "d_checklist_notes_entered"
borderstyle borderstyle = stylelowered!
end type

event rbuttondown;//
end event

event editchanged;call super::editchanged;IF tab_checklist.tabpage_checklist.dw_checklist_master.GetRow() > 0 THEN
	tab_checklist.tabpage_checklist_notes.cb_save.enabled = True
	tab_checklist.tabpage_checklist_notes.cb_cancel.enabled = True
	THIS.SetItemStatus(1, 0, Primary!, DataModified!)
END IF
end event

type tabpage_4 from userobject within tab_comment
event create ( )
event destroy ( )
boolean visible = false
integer x = 18
integer y = 100
integer width = 1984
integer height = 416
long backcolor = 16777215
string text = "Cancelled Step Note"
long tabtextcolor = 33554432
long tabbackcolor = 16777215
long picturemaskcolor = 536870912
dw_checklist_cancelled_step_comment_tab dw_checklist_cancelled_step_comment_tab
end type

on tabpage_4.create
this.dw_checklist_cancelled_step_comment_tab=create dw_checklist_cancelled_step_comment_tab
this.Control[]={this.dw_checklist_cancelled_step_comment_tab}
end on

on tabpage_4.destroy
destroy(this.dw_checklist_cancelled_step_comment_tab)
end on

type dw_checklist_cancelled_step_comment_tab from u_checklist_datawindow within tabpage_4
integer width = 2021
integer height = 428
integer taborder = 10
string title = "none"
string dataobject = "d_checklist_cancelled_step_notes"
borderstyle borderstyle = stylelowered!
end type

event rbuttondown;//
end event

event editchanged;call super::editchanged;IF tab_checklist.tabpage_checklist.dw_checklist_master.GetRow() > 0 THEN
	tab_checklist.tabpage_checklist_notes.cb_save.enabled = True
	tab_checklist.tabpage_checklist_notes.cb_cancel.enabled = True
	THIS.SetItemStatus(1, 0, Primary!, DataModified!)
END IF
end event

type tabpage_3 from userobject within tab_comment
event create ( )
event destroy ( )
boolean visible = false
integer x = 18
integer y = 100
integer width = 1984
integer height = 416
long backcolor = 16777215
string text = "Cancelled Checklist Note"
long tabtextcolor = 33554432
long tabbackcolor = 16777215
long picturemaskcolor = 536870912
dw_checklist_cancelled_comment_tab dw_checklist_cancelled_comment_tab
end type

on tabpage_3.create
this.dw_checklist_cancelled_comment_tab=create dw_checklist_cancelled_comment_tab
this.Control[]={this.dw_checklist_cancelled_comment_tab}
end on

on tabpage_3.destroy
destroy(this.dw_checklist_cancelled_comment_tab)
end on

type dw_checklist_cancelled_comment_tab from u_checklist_datawindow within tabpage_3
integer width = 2021
integer height = 424
integer taborder = 10
string title = "none"
string dataobject = "d_checklist_cancelled_comment"
borderstyle borderstyle = stylelowered!
end type

event losefocus;call super::losefocus;String ls_comment


ls_comment =  this.getitemstring(this.getrow(),"cancelled_comment")

IF ISNULL(ls_comment) OR ls_comment = '' THEN
	Messagebox('Error','A cancelled comment must be entered.',Exclamation!)
	//tab_checklist.tabpage_checklist	_notes.tab_comment.selecttab = 4
	this.setcolumn("cancelled_comment")
	this.setfocus()
	Return
END IF

IF LEN(ls_comment) < 5 OR LEN(ls_comment) > 256 THEN
	Messagebox('Error','The cancelled comment must be more than 5 characters and less than 256.~nPlease revise.',Exclamation!)
	this.setcolumn("cancelled_comment")
	this.setfocus()
	Return
END IF 


end event

event rbuttondown;//
end event

event editchanged;call super::editchanged;IF tab_checklist.tabpage_checklist.dw_checklist_master.GetRow() > 0 THEN
	tab_checklist.tabpage_checklist_notes.cb_save.enabled = True
	tab_checklist.tabpage_checklist_notes.cb_cancel.enabled = True
	THIS.SetItemStatus(1, 0, Primary!, DataModified!)
END IF
end event

type tabpage_checklist_history from userobject within tab_checklist
event create ( )
event destroy ( )
integer x = 18
integer y = 16
integer width = 2025
integer height = 2108
long backcolor = 16777215
string text = "History"
long tabtextcolor = 33554432
long tabbackcolor = 16777215
long picturemaskcolor = 536870912
uo_historybar uo_historybar
dw_checklist_history_master dw_checklist_history_master
dw_checklist_history_detail dw_checklist_history_detail
end type

on tabpage_checklist_history.create
this.uo_historybar=create uo_historybar
this.dw_checklist_history_master=create dw_checklist_history_master
this.dw_checklist_history_detail=create dw_checklist_history_detail
this.Control[]={this.uo_historybar,&
this.dw_checklist_history_master,&
this.dw_checklist_history_detail}
end on

on tabpage_checklist_history.destroy
destroy(this.uo_historybar)
destroy(this.dw_checklist_history_master)
destroy(this.dw_checklist_history_detail)
end on

event constructor;tab_checklist.tabpage_checklist_history.uo_historybar.st_checklist_name.Text = is_checklist_type_desc
end event

type uo_historybar from u_titlebar within tabpage_checklist_history
event destroy ( )
integer width = 2021
integer taborder = 10
end type

on uo_historybar.destroy
call u_titlebar::destroy
end on

type dw_checklist_history_master from u_checklist_datawindow within tabpage_checklist_history
event ue_post_constructor ( )
integer y = 104
integer width = 2025
integer height = 424
integer taborder = 60
string title = "none"
string dataobject = "d_checklist_history_master"
boolean hscrollbar = true
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event ue_post_constructor();String ls_checklist_type_desc

is_checklist_type_code = this.getitemstring(this.getrow(),"checklist_type_code")
ls_checklist_type_desc = uf_get_checklist_type_desc(is_checklist_type_code)

tab_checklist.tabpage_checklist_history.uo_historybar.st_checklist_name.Text = ls_checklist_type_desc

end event

event clicked;
IF This.RowCount() < 1 OR row < 1 THEN
	Return -1
END IF	

This.SelectRow(0, false)
This.SelectRow(row, true)

il_checklist_no = this.getitemnumber(row,"checklist_no")

end event

event doubleclicked;This.SelectRow(0, false)
This.SelectRow(row, true)
end event

event getfocus;call super::getfocus;
This.SelectRow(0, false)
This.SelectRow(1, true)
end event

event rowfocuschanged;call super::rowfocuschanged;
int li_rows
long ll_checklist_no 

IF currentrow = 0 THEN
	Return 
END IF

ll_checklist_no = idw_dw[4].GetItemNumber(currentrow,"checklist_no")

li_rows     = idw_dw[5].Retrieve(ll_checklist_no)
itr_trans_object.nf_handle_error('n_checklist','nf_retreive_checklists','idw_dw[5].Retrieve')

This.SelectRow(0, false)
This.SelectRow(currentrow, true)
end event

event rbuttondown;uf_menu(this)
end event

type dw_checklist_history_detail from u_checklist_datawindow within tabpage_checklist_history
integer y = 532
integer width = 2025
integer height = 1564
integer taborder = 50
string title = "none"
string dataobject = "d_checklist_history_details"
boolean hscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event clicked;Long ll_return, ll_checklist_no, ll_checklist_step_no_on_window


IF This.RowCount() < 1 OR row < 1 THEN
	Return -1
END IF	

//	A mandatory histroy checklist item must be selected.
This.SelectRow(0, false)
This.SelectRow(row, true)

ll_checklist_no = this.getitemnumber(row,"checklist_no")
ll_checklist_step_no_on_window = this.getitemnumber(row,"checklist_step_no")

//Select the corresponding row in the notes tab.

////ll_return = inv_checklist.nf_set_notes_row(ll_checklist_no, ll_checklist_step_no_on_window,row)
end event

event rbuttondown;uf_menu(this)
end event

event rowfocuschanged;call super::rowfocuschanged;This.SelectRow(0, false)
This.SelectRow(currentRow, true)
end event

type p_left_arrow from picture within u_checklist
integer x = 5
integer width = 110
integer height = 96
string picturename = "arrow_left.gif"
boolean border = true
boolean map3dcolors = true
end type

event clicked;Long ll_return

IF tab_checklist.tabpage_checklist.visible = False OR tab_checklist.SelectedTab <> 1 Then
	tab_checklist.SelectedTab = 1
End If
ll_return = uf_maximized()
p_right_arrow.visible =True
p_left_arrow.visible = False

end event

type p_right_arrow from picture within u_checklist
integer x = 5
integer width = 110
integer height = 96
string picturename = "arrow_right.gif"
boolean border = true
boolean map3dcolors = true
end type

event clicked;Long ll_return

ll_return = uf_minimized()

p_right_arrow.visible = False
p_left_arrow.visible = True
end event

type st_blueline from statictext within u_checklist
integer x = 5
integer width = 110
integer height = 2228
integer textsize = -8
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 134217731
boolean focusrectangle = false
end type

