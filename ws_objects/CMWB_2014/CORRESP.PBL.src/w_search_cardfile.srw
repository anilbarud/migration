$PBExportHeader$w_search_cardfile.srw
$PBExportComments$Card file entry search screen
forward
global type w_search_cardfile from window
end type
type cb_1 from commandbutton within w_search_cardfile
end type
type dw_corr_recip_type from u_dw_online within w_search_cardfile
end type
type dw_srch_criteria from u_dw_online within w_search_cardfile
end type
type st_1 from statictext within w_search_cardfile
end type
type dw_result_list from u_dw_online within w_search_cardfile
end type
type cb_search from commandbutton within w_search_cardfile
end type
type cb_cancel from commandbutton within w_search_cardfile
end type
type cb_ok from commandbutton within w_search_cardfile
end type
type gb_1 from groupbox within w_search_cardfile
end type
type gb_search_list from groupbox within w_search_cardfile
end type
end forward

global type w_search_cardfile from window
integer x = 1051
integer y = 444
integer width = 2107
integer height = 1692
boolean titlebar = true
string title = "Search Cardfile Recipients"
windowtype windowtype = response!
long backcolor = 67108864
event ue_postopen pbm_custom01
cb_1 cb_1
dw_corr_recip_type dw_corr_recip_type
dw_srch_criteria dw_srch_criteria
st_1 st_1
dw_result_list dw_result_list
cb_search cb_search
cb_cancel cb_cancel
cb_ok cb_ok
gb_1 gb_1
gb_search_list gb_search_list
end type
global w_search_cardfile w_search_cardfile

type variables
Protected:
string	vis_name, vis_city,vis_recipient_type
s_cardfile_parms	vis_cardfile_parms
long	vil_srch_row_no,vil_recipient_no
m_cmwb_notools	vim_menu
DatawindowChild		viw_recipient_type




end variables

event open;//////////////////////////////////////////////////////////////////////////////////
////
//Purpose: Allows a user to search and select from a table
//
//
//Log:
//
//			Date			Who				What
//-------....--------...------------....------------------------------------------
//		 96/01/04		B.Burton			Initial Version
//
// Open Event
//////////////////////////////////////////////////////////////////////////////////
//	
window 	lw_parent
int		li_parent_x,li_parent_y,li_parent_width,li_parent_height

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


vis_cardfile_parms = MESSAGE.PowerObjectParm
lw_parent = vis_cardfile_parms.parent_window


vis_recipient_type = vis_cardfile_parms.recipient_type
IF vis_recipient_type = ' ' then SETNULL(vis_recipient_type)
/* Try and postion this response window somewhere relative to the previous window
*/
this.setredraw(false)
lw_parent.setredraw(False)
lw_parent.Bringtotop =False
this.BringtoTop= True 

dw_result_list.SetTransObject(SQLCA)
dw_corr_recip_type.GetChild("recipient_type",viw_recipient_type)
dw_corr_recip_type.SetTransObject(SQLCA)

IF dw_corr_recip_type.InsertRow(0) < 0 THEN 
	MessageBox ( this.title, "Could not insert row on datawindow control dw_corr_recip.")
   Return
END IF
dw_srch_criteria.InsertRow(0)



/*****************************************
*/

/* Set default values or  Get value for variable passed from parent window and verify with drop down data window.
*/

/* If recipient type wasn't set properly.
*/
IF NOT MATCH(vis_recipient_type, "^[A-Z]") then
	vil_srch_row_no = 2
	vis_recipient_type = viw_recipient_type.GETItemString(vil_srch_row_no,"correspond_recipient_type_cd")
ELSE
	
	vil_srch_row_no =viw_recipient_type.Find("correspond_recipient_type_cd = '"+vis_recipient_type+"'",1,viw_recipient_type.Rowcount())
	If vil_srch_row_no < 0	 OR vil_srch_row_no = 0  then
			MessageBox(this.title,"Unable to set Default recipient type,"&
			+"~n~r please choose one from the drop down list!",INFORMATION!)
			dw_corr_recip_type.Setfocus()
			Return
	END If			

END IF

dw_corr_recip_type.SetItem(1,"recipient_type",viw_recipient_type.GETItemString(vil_srch_row_no,"correspond_recipient_type_desc"))
dw_corr_recip_type.ScrollToRow(vil_srch_row_no)
dw_corr_recip_type.Setfocus()
this.Setredraw(True)

//*************************************************
// Do other processing if needed. Allows you to
// do lenghty processing after the visual 
// components of window processing are complete.
//li_parent_x = lw_parent.X
//li_parent_Y = lw_parent.Y
//li_parent_width = lw_parent.WorkspaceWidth()
//li_parent_height = lw_parent.WorkspaceHeight()
//
//Move (li_parent_x,li_parent_y)
//Resize (lw_parent.WorkspaceWidth(),lw_parent.WorkspaceHeight())

//li_parent_height = li_parent_height + vim_menu.m_workbench.m_close.Y+( vim_menu.m_workbench.m_close.Height)
//li_parent_height = li_parent_height - mdi_1.MicroHelpHeight)
///* TEST THISTHis one postions at the bottom of the  frame */
//this.move(lw_parent.WorkspaceWidth()/2+this.WorkspaceX()-lw_parent.WorkspaceWidth(),&
//lw_parent.WorkspaceHeight()+this.WorkspaceHeight())
//
/* this one is way at the bottom */
//this.move(lw_parent.WorkspaceWidth()+this.WorkspaceWidth()/2,&
//lw_parent.WorkspaceHeight()+this.WorkSpaceHeight()/2)
//
/* this one is way at the bottom */
//this.move(lw_parent.WorkspaceWidth()-this.WorkspaceWidth(),&
//lw_parent.WorkspaceHeight()-this.WorkSpaceHeight())
//


/* THis one postions to the left of the frame */
//this.move(lw_parent.x+this.WorkspaceWidth()/2,lw_parent.y+this.WorkspaceWidth()/2)





end event

on w_search_cardfile.create
this.cb_1=create cb_1
this.dw_corr_recip_type=create dw_corr_recip_type
this.dw_srch_criteria=create dw_srch_criteria
this.st_1=create st_1
this.dw_result_list=create dw_result_list
this.cb_search=create cb_search
this.cb_cancel=create cb_cancel
this.cb_ok=create cb_ok
this.gb_1=create gb_1
this.gb_search_list=create gb_search_list
this.Control[]={this.cb_1,&
this.dw_corr_recip_type,&
this.dw_srch_criteria,&
this.st_1,&
this.dw_result_list,&
this.cb_search,&
this.cb_cancel,&
this.cb_ok,&
this.gb_1,&
this.gb_search_list}
end on

on w_search_cardfile.destroy
destroy(this.cb_1)
destroy(this.dw_corr_recip_type)
destroy(this.dw_srch_criteria)
destroy(this.st_1)
destroy(this.dw_result_list)
destroy(this.cb_search)
destroy(this.cb_cancel)
destroy(this.cb_ok)
destroy(this.gb_1)
destroy(this.gb_search_list)
end on

on closequery;vis_cardfile_parms.parent_window.bringtotop=True
vis_cardfile_parms.parent_window.Setredraw(True)
end on

type cb_1 from commandbutton within w_search_cardfile
integer x = 1454
integer y = 264
integer width = 297
integer height = 96
integer taborder = 50
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cl&ear"
end type

on clicked;parent.SetRedraw(False)
gb_search_list.Text = "Results"
dw_corr_recip_type.Reset()
dw_srch_criteria.Reset()
dw_result_list.Reset()
dw_srch_criteria.insertrow(0)
cb_search.enabled=True
cb_ok.enabled=false

dw_corr_recip_type.INSERTROW(0)
dw_corr_recip_type.setfocus()
Parent.SetRedraw(True)
end on

type dw_corr_recip_type from u_dw_online within w_search_cardfile
integer x = 329
integer y = 96
integer width = 882
integer height = 96
integer taborder = 20
string dataobject = "d_lookup_correspond_recipient_type"
boolean border = false
end type

event itemchanged;call super::itemchanged;this.uf_set_pbmessage ( True )
vis_recipient_type = Gettext()

vis_cardfile_parms.recipient_type = GETTEXT()

If ISNULL(vis_recipient_type) then
	MESSAGEBOX('Data Entry Error','Please select a Category',Information!)
	Return 1
END IF


end event

type dw_srch_criteria from u_dw_online within w_search_cardfile
integer x = 55
integer y = 192
integer width = 1207
integer height = 264
integer taborder = 30
string dataobject = "d_srch_criteria"
boolean border = false
end type

event itemchanged;call super::itemchanged;string ls_col_value, ls_col_selected

this.uf_set_pbmessage ( True )

// Get the row number and values
ls_col_value = Gettext()
ls_col_selected = GetcolumnName()

Choose Case ls_col_selected
	Case "name"
		LeftTRIM(ls_col_value)
		IF IsNull( ls_col_value ) OR Len( ls_col_value)  <= 2 &
			OR match(ls_col_value,"[%+]") or match(ls_col_value,"[$']")   THEN 
			MessageBOX("Validation Error","Name  must contain at least 3 characters and"&
							+"~n~r cannot begin with a % or blank, nor end with an aprostophe!")
			RETURN 1
		END IF
	
		vis_name = ls_col_value

	Case "city"	
		LeftTRIM(ls_col_value)
		vis_city = ls_col_value
End Choose
end event

on itemerror;call u_dw_online::itemerror;this.uf_set_pbmessage ( True )
end on

type st_1 from statictext within w_search_cardfile
integer x = 55
integer y = 120
integer width = 274
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Category :"
alignment alignment = center!
boolean focusrectangle = false
end type

type dw_result_list from u_dw_online within w_search_cardfile
boolean visible = false
integer x = 55
integer y = 624
integer width = 1961
integer height = 748
integer taborder = 70
string dataobject = "d_cardfile_list"
boolean hscrollbar = true
boolean vscrollbar = true
boolean hsplitscroll = true
end type

on doubleclicked;call u_dw_online::doubleclicked;cb_ok.triggerevent(Clicked!)
end on

type cb_search from commandbutton within w_search_cardfile
integer x = 1454
integer y = 120
integer width = 315
integer height = 96
integer taborder = 40
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Search"
boolean default = true
end type

on clicked;//********************************************************************
///
// This event will get the criteria entered from the datawindow
//	and use it to retrieve the data
//
//

string	vls_value
int		vli_total_rows, vli_return_code,vll_rownum
string	vls_type_desc

this.enabled = true
gb_search_list.SetRedraw(False)
dw_result_list.SetRedraw(False)
gb_search_list.visible=False
dw_result_list.visible = False

IF dw_corr_recip_type.uf_accept_dw() =-1 Then
		 	dw_corr_recip_type.SetFocus()
			MessageBox("SEARCH MESSAGE: 513-024","Unable to accept claimant type, please re-enter.")
		 	Return

End IF	

IF dw_srch_criteria.uf_Accept_dw() = -1  Then 
				dw_srch_criteria.SETFOCUS()
				Return
END IF
/* Get the latest type description after the accept text event
*/

vis_name = UPPER(dw_srch_criteria.GetItemString(1,"name"))
vis_city = Upper(dw_srch_criteria.GetItemString(1, "city"))
// Get name entered, if any
IF IsNull(vis_name) or vis_name = ' ' THEN
	vis_name	= "%%"
ELSE
	vis_name	= "%"+Upper(vis_name)+"%"
END IF

IF vis_name = "%%"    THEN
	MessageBox("SEARCH MESSAGE: 513-024","Name or City must not be blank, please re-enter.")
		dw_srch_criteria.SETFOCUS()
		Return	
	Return
END IF
// Get city entered, if any
IF IsNull(vis_city) THEN
	vis_city	= "%%"
ELSE
	vis_city	= "%"+Upper(vis_city)+"%"
END IF

//IF vis_name = " " OR vis_city = " "   then
//		MessageBox("SEARCH MESSAGE: 513-024","Name or City must not be blank, please re-enter.")
//		dw_srch_criteria.SETFOCUS()
//		Return	
//END IF
vis_name = "%"+vis_name+"%"
vis_city = "%"+vis_city+"%"


vli_total_rows = dw_result_list.Retrieve(vis_recipient_type, vis_name, vis_city)
vli_return_code = SQLCA.nf_handle_error("dw_result_list","w_search_cardfile","on cb_clicked Search")


IF vli_total_rows = 0 THEN
	MessageBox("SEARCH MESSAGE: 513-023","No records were found that match your criteria.")
	Return
ELSE
	IF vli_total_rows > 500 THEN
		MessageBox("SEARCH MESSAGE: 513-024","Your search criteria should be more specific" &
					+"~n~rsince at least 500 matches occurred.")
		Return
	END IF
END IF

// Only allow one selection to made in datawindow

dw_result_list.uf_setselect(1)
gb_search_list.TEXT = "Results ("+String(vli_total_rows)+")"
dw_result_list.visible= True
gb_search_list.visible = true
// Set command buttons
cb_search.enabled = False
cb_ok.enabled = True
cb_cancel.enabled = True
cb_ok.visible = True
cb_cancel.visible=True

//this.enabled = False
dw_result_list.Setredraw(True)
gb_search_list.Setredraw(True)
/*This statement moves cb_cancel next to cb_ok
*/


cb_cancel.Move(cb_ok.X + cb_ok.Width +20 , cb_ok.y)
dw_result_list.Setredraw(True)
gb_search_list.Setredraw(True)
dw_result_list.Show()
dw_result_list.SetFocus()



end on

type cb_cancel from commandbutton within w_search_cardfile
integer x = 1454
integer y = 552
integer width = 315
integer height = 96
integer taborder = 60
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cance&l"
end type

on clicked;
closewithreturn (parent,vil_recipient_no)
end on

type cb_ok from commandbutton within w_search_cardfile
boolean visible = false
integer x = 1289
integer y = 1464
integer width = 315
integer height = 96
integer taborder = 80
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&OK"
boolean default = true
end type

on clicked;parent.enabled=false
message.DoubleParm = dw_result_list.GetItemNumber(dw_result_list.GetRow(), "correspond_recipient_id")
CloseWithReturn(Parent, message.DoubleParm)

end on

type gb_1 from groupbox within w_search_cardfile
integer x = 23
integer y = 24
integer width = 1838
integer height = 504
integer taborder = 10
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Criteria"
borderstyle borderstyle = styleraised!
end type

type gb_search_list from groupbox within w_search_cardfile
boolean visible = false
integer y = 552
integer width = 2030
integer height = 864
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Results"
borderstyle borderstyle = styleraised!
end type

