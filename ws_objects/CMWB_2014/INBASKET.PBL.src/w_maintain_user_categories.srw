$PBExportHeader$w_maintain_user_categories.srw
$PBExportComments$Window to allow user to add/delete/modify lists of categories that will appear in the In-Basket module
forward
global type w_maintain_user_categories from window
end type
type cb_close from commandbutton within w_maintain_user_categories
end type
type cb_save from commandbutton within w_maintain_user_categories
end type
type cb_delete from commandbutton within w_maintain_user_categories
end type
type dw_setnames from u_dw_online within w_maintain_user_categories
end type
type cb_add_categories from commandbutton within w_maintain_user_categories
end type
type dw_user_categories from u_dw_online within w_maintain_user_categories
end type
type dw_categories_by_set from u_dw_online within w_maintain_user_categories
end type
end forward

global type w_maintain_user_categories from window
integer x = 1893
integer y = 48
integer width = 2766
integer height = 2760
boolean titlebar = true
string title = "Maintain User Categories/Buckets"
string menuname = "m_cmwb_notools"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 67108864
cb_close cb_close
cb_save cb_save
cb_delete cb_delete
dw_setnames dw_setnames
cb_add_categories cb_add_categories
dw_user_categories dw_user_categories
dw_categories_by_set dw_categories_by_set
end type
global w_maintain_user_categories w_maintain_user_categories

type variables
DATAWINDOWCHILD	iw_setnames
BOOLEAN		ib_suppress_pbmessage
M_CMWB_NOTOOLS	im_menu
BOOLEAN		I_Authorized_Access

end variables

forward prototypes
public function integer wf_add_buckets (long al_setid)
end prototypes

public function integer wf_add_buckets (long al_setid);LONG ll_result

/* Add the scanning corections bucket.
*/
	ll_result = dw_categories_by_set.InsertRow(0)
	IF ll_result < 0 THEN
		RETURN -1
	END IF
	dw_categories_by_set.SetItem(ll_result,"catid",864)
	dw_categories_by_set.SetItem(ll_result,"setid",6)
	dw_categories_by_set.SetItem(ll_result,"catname","CORRECTIONS")

/* P10277 - Add the CRA Response Notification bucket.
*/
	IF al_setid <> 9 THEN
		ll_result = dw_categories_by_set.InsertRow(0)
		IF ll_result < 0 THEN
			RETURN -1
		END IF
		dw_categories_by_set.SetItem(ll_result,"catid",2803)
		dw_categories_by_set.SetItem(ll_result,"setid",9)
		dw_categories_by_set.SetItem(ll_result,"catname","CRA Response Notification")
	END IF 
	
	RETURN 0

end function

event open;/* APPLICATION SECURITY CODE.
*/
	G_PFSecurity.UOF_Check_Access(THIS)
	THIS.I_Authorized_Access = TRUE			//declared as an instance variable.

	LONG	ll_result, ll_setid

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


	dw_user_categories.SetTransObject(ImageTrans)
	dw_categories_by_set.SetTransObject(ImageTrans)

	dw_user_categories.uf_setselect(3)
	dw_categories_by_set.uf_setselect(3)

	dw_setnames.GetChild("setid",iw_setnames)
	iw_setnames.SetTransObject(ImageTrans)

	ll_result = iw_setnames.Retrieve()
	IF ImageTrans.nf_handle_error("iw_setnames","w_maintain_user_categories","Open") < 0 THEN
		Close(THIS)
		RETURN
	END IF

/*	Figure out what the setid (imaging work area) is for the user.
*/
	SetNull(ll_setid)

	SELECT User_Profile.default_image_working_setid  
	  INTO :ll_setid  
	  FROM User_Profile
	 WHERE User_Profile.user_id = :vgst_user_profile.user_id
	 USING SQLCA;

	IF SQLCA.nf_handle_error("Embedded SQL: Retrieve from User_Profile","w_maintain_user_categories","open event") < 0 THEN
		Close(THIS)
		RETURN
	END IF

/*	Insert a row into the List and default it to the user's default work area.
*/
	ll_result = dw_setnames.InsertRow(0)
	IF NOT IsNull(ll_setid) AND ll_setid > 0 THEN
		ll_result = dw_setnames.SetItem(1,"setid",ll_setid)
	ELSE
		ll_setid = 0
	END IF

/* Get the regions categories.
*/
	dw_categories_by_set.Retrieve(ll_setid)
	IF ImageTrans.nf_handle_error("dw_category_list","w_maintain_user_categories","Open") < 0 THEN
		Close(THIS)
		RETURN
	END IF

/* Display the user's current setup.
*/
	dw_user_categories.Retrieve(vgst_user_profile.user_id)
	IF ImageTrans.nf_handle_error("dw_user_categories","w_maintain_user_categories","Open") < 0 THEN
		Close(THIS)
		RETURN
	END IF

	ll_result = wf_add_buckets(ll_setid)
	IF ll_result < 0 THEN
		MessageBox('Error Inserting Category','There was an error attempting to insert Corrections and/or CRA Response Notification Category', Information!)
		RETURN
	END IF

/* Set up the instance variable for the menu so that we can refer to the frames menu later.
*/
	im_menu = m_cmwb_notools

end event

on closequery;If dw_user_categories.ModifiedCount() > 0 or dw_user_categories.DeletedCount() > 0 then
	If MessageBox("Save Changes?","Do you want to save your changes before exiting?",Question!, YesNo!) = 1 Then
		cb_save.TriggerEvent(Clicked!)
	End If
End If
end on

on w_maintain_user_categories.create
if this.MenuName = "m_cmwb_notools" then this.MenuID = create m_cmwb_notools
this.cb_close=create cb_close
this.cb_save=create cb_save
this.cb_delete=create cb_delete
this.dw_setnames=create dw_setnames
this.cb_add_categories=create cb_add_categories
this.dw_user_categories=create dw_user_categories
this.dw_categories_by_set=create dw_categories_by_set
this.Control[]={this.cb_close,&
this.cb_save,&
this.cb_delete,&
this.dw_setnames,&
this.cb_add_categories,&
this.dw_user_categories,&
this.dw_categories_by_set}
end on

on w_maintain_user_categories.destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_close)
destroy(this.cb_save)
destroy(this.cb_delete)
destroy(this.dw_setnames)
destroy(this.cb_add_categories)
destroy(this.dw_user_categories)
destroy(this.dw_categories_by_set)
end on

type cb_close from commandbutton within w_maintain_user_categories
integer x = 2331
integer y = 2444
integer width = 357
integer height = 96
integer taborder = 60
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Close"
end type

on clicked;Close(PARENT)
end on

type cb_save from commandbutton within w_maintain_user_categories
integer x = 759
integer y = 2328
integer width = 357
integer height = 96
integer taborder = 50
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Save"
end type

event clicked;
ImageTrans.nf_begin_transaction()

dw_user_categories.Update()
IF ImageTrans.nf_handle_error("dw_user_categories","w_maintain_user_categories","clicked for cb_add_categories") < 0 THEN
	MessageBox("Error Saving Changes","Error saving changes to database. Please exit and try again.")
	RETURN
END IF

ImageTrans.nf_commit_transaction()

end event

type cb_delete from commandbutton within w_maintain_user_categories
integer x = 320
integer y = 2328
integer width = 357
integer height = 96
integer taborder = 70
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Delete"
end type

on clicked;LONG	ll_selected_row, ll_result

ll_selected_row = dw_user_categories.GetSelectedRow(0)
IF ll_selected_row = 0 THEN
	MessageBox("Delete Categories","You must select one or more categories first.")
	RETURN
END IF

/* Get the category id for selected rows.
*/
	DO WHILE ll_selected_row > 0
		ll_result = dw_user_categories.DeleteRow(ll_selected_row)
		IF ll_result = -1 THEN
			MessageBox("Error Removing Row","Error removing selected row. Please close and try again.")
			RETURN
		END IF

/* Get the next selected row.
*/
		ll_selected_row = dw_user_categories.GetSelectedRow(0)
	LOOP

end on

type dw_setnames from u_dw_online within w_maintain_user_categories
integer x = 1632
integer y = 64
integer width = 1047
integer height = 204
integer taborder = 10
string dataobject = "d_setnames"
boolean border = false
end type

event itemchanged;LONG	ll_result, ll_selected_set

/* Find the region selected.
*/
	ll_selected_set = Long(GetText())

/* Get the set's categories.
*/
	dw_categories_by_set.Retrieve(ll_selected_set)
	IF ImageTrans.nf_handle_error("dw_categories_by_set","w_maintain_user_categories","Open") < 0 THEN
		MessageBox("Categories","Error retrieving categories for set" + String(ll_selected_set))
		RETURN
	END IF

	ll_result = wf_add_buckets(ll_selected_set)
	IF ll_result < 0 THEN
		MessageBox('Error Inserting Category','There was an error attempting to insert Corrections and/or CRA Response Notification Category', Information!)
		RETURN
	END IF

end event

type cb_add_categories from commandbutton within w_maintain_user_categories
integer x = 1472
integer y = 832
integer width = 151
integer height = 108
integer taborder = 30
integer textsize = -14
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "<<"
end type

on clicked;LONG		ll_selected_row, ll_catid, ll_result
STRING	ls_catname, ls_exists

ls_exists = 'N'

ll_selected_row = dw_categories_by_set.GetSelectedRow(0)
IF ll_selected_row = 0 THEN
	MessageBox("Add Categories","You must select one or more categories first.")
	RETURN
END IF

/* Get the category id for selected rows.
*/
	DO WHILE ll_selected_row > 0
		ll_catid   = dw_categories_by_set.GetItemNumber(ll_selected_row,"catid")
		ls_catname = dw_categories_by_set.GetItemString(ll_selected_row,"catname")

/* Make sure the selected category doesn't already exist in the user's list.
*/
		ll_result = dw_user_categories.Find("user_category_xref_category_id_nmbr = " + string(ll_catid),1, dw_user_categories.RowCount())
		IF ll_result <= 0 THEN

/* It wasn't found so move it over.
*/
			ll_result = dw_user_categories.InsertRow(0)
			dw_user_categories.SetItem(ll_result,"user_category_xref_category_id_nmbr",ll_catid)
			dw_user_categories.SetItem(ll_result,"user_category_xref_inbox_yn","Y")
			dw_user_categories.SetItem(ll_result,"user_category_xref_outbox_yn","Y")
			dw_user_categories.SetItem(ll_result,"user_category_xref_sort_order",9)
			dw_user_categories.SetItem(ll_result,"cat_catname",ls_catname)
			dw_user_categories.SetItem(ll_result,"user_category_xref_user_name_text",vgst_user_profile.user_id)
		ELSE
			IF ls_exists = 'N' THEN
				MessageBox(ls_catname + " Already Exists","Duplicate entries ignored.")
				ls_exists = 'Y'
			END IF
		END IF

/* Get the next selected row.
*/
		ll_selected_row = dw_categories_by_set.GetSelectedRow(ll_selected_row)
	LOOP

end on

type dw_user_categories from u_dw_online within w_maintain_user_categories
integer x = 41
integer y = 360
integer width = 1408
integer height = 1928
integer taborder = 20
string dataobject = "d_user_categories"
boolean vscrollbar = true
borderstyle borderstyle = styleraised!
end type

type dw_categories_by_set from u_dw_online within w_maintain_user_categories
integer x = 1650
integer y = 360
integer width = 1038
integer height = 1928
integer taborder = 40
string dataobject = "d_categories_by_set"
boolean vscrollbar = true
borderstyle borderstyle = styleraised!
end type

