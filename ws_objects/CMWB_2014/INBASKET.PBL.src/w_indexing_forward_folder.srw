$PBExportHeader$w_indexing_forward_folder.srw
$PBExportComments$Window used by indexing module to forward folder to another bucket in scanning set
forward
global type w_indexing_forward_folder from window
end type
type cb_cancel from commandbutton within w_indexing_forward_folder
end type
type cb_ok from commandbutton within w_indexing_forward_folder
end type
type st_1 from statictext within w_indexing_forward_folder
end type
type dw_select_category from u_dw_online within w_indexing_forward_folder
end type
end forward

global type w_indexing_forward_folder from window
integer x = 2199
integer y = 688
integer width = 1440
integer height = 428
boolean titlebar = true
string title = "Forward Folder"
windowtype windowtype = response!
long backcolor = 67108864
cb_cancel cb_cancel
cb_ok cb_ok
st_1 st_1
dw_select_category dw_select_category
end type
global w_indexing_forward_folder w_indexing_forward_folder

type variables
BOOLEAN	I_Authorized_Access
LONG		il_fldid



end variables

event open;/*	APPLICATION SECURITY CODE.
*/
	G_PFSecurity.UOF_Check_Access(THIS)
	THIS.I_Authorized_Access = TRUE              //declared as an instance variable.

	DATAWINDOWCHILD	ldwc_categories

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


	il_fldid = Message.DoubleParm
	IF IsNull(il_fldid) OR il_fldid <= 0 THEN
		MessageBox("Move Folder","Cannot Determine folder to move",Exclamation!)
		Close(THIS)
		RETURN
	END IF

/* Set transaction objects for data windows.
*/
	dw_select_category.SetTransObject(ImageTrans)
	dw_select_category.GetChild("catid",ldwc_categories)
	ldwc_categories.SetTransObject(ImageTrans)

/* Retrieve the category drop down to ensure the user has selected send to categories.
*/
/* Get all the buckets/categories in the scanning set (6).
*/
	ldwc_categories.Retrieve(6)
	IF ImageTrans.nf_handle_error("ldwc_categories","w_document_indexing","open for w_document_indexing") < 0 THEN
		Close(THIS)
		RETURN
	END IF

	IF ldwc_categories.RowCount() > 0 THEN
		dw_select_category.InsertRow(0)
	ELSE
		MessageBox("Move Folder","There are no categories found to move the folder to.")
		Close(THIS)
		RETURN
	END IF

end event

on w_indexing_forward_folder.create
this.cb_cancel=create cb_cancel
this.cb_ok=create cb_ok
this.st_1=create st_1
this.dw_select_category=create dw_select_category
this.Control[]={this.cb_cancel,&
this.cb_ok,&
this.st_1,&
this.dw_select_category}
end on

on w_indexing_forward_folder.destroy
destroy(this.cb_cancel)
destroy(this.cb_ok)
destroy(this.st_1)
destroy(this.dw_select_category)
end on

type cb_cancel from commandbutton within w_indexing_forward_folder
integer x = 750
integer y = 184
integer width = 320
integer height = 100
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cance&l"
end type

on clicked;Close(PARENT)

end on

type cb_ok from commandbutton within w_indexing_forward_folder
integer x = 315
integer y = 184
integer width = 320
integer height = 100
integer taborder = 30
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
end type

event clicked;LONG			ll_catid
DATAWINDOW	ldw_claim_source
W_SHEET		lw_active_sheet

/* Get the category to move the folder to.
*/
	ll_catid = dw_select_category.GetItemNumber(1,"catid")
	IF IsNull(ll_catid) OR ll_catid <= 0 THEN
		MessageBox("Move Folder","Could not determine selected category .")
		RETURN
	END IF
	
	
	ImageTrans.nf_begin_transaction()

/* Move the folder to the new category.
	This update has to only update one record or there is a problem.
*/
	UPDATE FLD
		SET fldcatid = :ll_catid
	  FROM FLD, CAT
	 WHERE fldid = :il_fldid
		AND catid = :ll_catid
	 USING ImageTrans;

	IF ImageTrans.nf_handle_error("Embedded SQL: Update FLD to move folder","w_document_indexing","clicked for cb_move") < 0 THEN
		Close(PARENT)
		RETURN
	END IF

	IF ImageTrans.SQLNRows <> 1 THEN
		ImageTrans.nf_rollback_transaction()
		
		MessageBox("Move Folder","Folder could not be moved. Please Refresh and try again.")
		RETURN
	END IF

/* This update may not update anything if the folder has no documents in it.
*/
	UPDATE REF
		SET doccatid = :ll_catid,
			 docsetid = 6
	  FROM REF, CAT
	 WHERE docfldid = :il_fldid
		AND catid = :ll_catid
	 USING ImageTrans;
	
	IF ImageTrans.nf_handle_error("Embedded SQL: Update REF to move folder","w_document_indexing","clicked for cb_move") < 0 THEN
		Close(PARENT)
		RETURN
	END IF

/* Wipe out the 'Bring Forward' information for the CLAIM_WORKING record for this folder as
	the user receiving this folder doesn't want that information.
*/
	UPDATE CLAIM_WORKING  
   	SET bring_fwd_date = null,   
	       bring_fwd_note = ""
	 WHERE folderid = :il_fldid
	 USING ImageTrans;

	IF ImageTrans.nf_handle_error("Embedded SQL: Update CLAIM_WORKING","w_inbasket","wf_inbasket_forward_claim") < 0 THEN
		Close(PARENT)
		RETURN
	END IF

	ImageTrans.nf_commit_transaction()


	MessageBox("Move Folder","Folder has been moved. Your document list will be refreshed.")
	Close(PARENT)

end event

type st_1 from statictext within w_indexing_forward_folder
integer x = 41
integer y = 52
integer width = 439
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Select Category:"
alignment alignment = center!
boolean focusrectangle = false
end type

type dw_select_category from u_dw_online within w_indexing_forward_folder
integer x = 494
integer y = 40
integer width = 869
integer height = 96
integer taborder = 10
string dataobject = "d_select_category"
boolean border = false
end type

