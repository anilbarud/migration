$PBExportHeader$w_edit.srw
$PBExportComments$Used to modify correspondence comments and content
forward
global type w_edit from window
end type
type dw_doc_type from u_dw_online within w_edit
end type
type cb_ok from commandbutton within w_edit
end type
type cb_cancel from commandbutton within w_edit
end type
type gb_1 from groupbox within w_edit
end type
end forward

global type w_edit from window
integer x = 105
integer y = 716
integer width = 1947
integer height = 684
boolean titlebar = true
string title = "Edit Doc Type"
boolean controlmenu = true
windowtype windowtype = popup!
long backcolor = 67108864
event ue_post_open pbm_custom01
dw_doc_type dw_doc_type
cb_ok cb_ok
cb_cancel cb_cancel
gb_1 gb_1
end type
global w_edit w_edit

type variables
s_correspond_claim   vistr_correspond_claim
int		   vii_return_code, vii_row_nbr
string		   physical_file_path	
long		   doc_id
w_correspond	   viw_correspond
DatawindowChild	   idw_ddw_document_types	
long il_usage

u_word ioo_word
long il_edit_doc_handle
end variables

forward prototypes
public function integer wf_update_imara ()
public subroutine wf_get_handle (string as_file_path)
end prototypes

event ue_post_open;LONG ll_handle
BOOLEAN lb_rtn

ll_handle = Handle(w_edit)
lb_rtn = BringWindowToTop(ll_handle)
IF lb_rtn = FALSE THEN
	MessageBox("Error","Problem with BringWindowToTop")
END IF

end event

public function integer wf_update_imara ();dw_doc_type.SetItem(1,"comment", vistr_correspond_claim.corr.comments) 

dw_doc_type.Update()

ImageTrans.nf_handle_error("dw_doc_type", "w_edit", "on wf_update_imara")


Return 0 
  
end function

public subroutine wf_get_handle (string as_file_path);String ls_reverse_file_name, ls_file_name 

// get handle of window just opened
// 1st: cut name of document out of full path
ls_reverse_file_name = Reverse(as_file_path)
ls_file_name = Reverse(Left(ls_reverse_file_name, (Pos(ls_reverse_file_name,'\') - 1) ))

ls_file_name = ls_file_name + ' - Microsoft Word'

il_edit_doc_handle = FindWindowA('OpusApp',ls_file_name)
end subroutine

event open;// ************************************************************************************************
// AUTHOR:	MW George
// Modified:	94.10.06

// ************************************************************************************************
// SET WINDOW VALUES & INSTANCE VARIABLES

//int		vli_rc
long		ll_row_nbr, vll_rc, ll_handle, ll_rtn
w_edit lw_this
Window lw_frame

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


vistr_correspond_claim = Message.PowerObjectParm
viw_correspond = vistr_correspond_claim.parent_window
this.title			= vistr_correspond_claim.corr.corr_action+"Claim "+" (#"+String(vistr_correspond_claim.claim_no)+")"
physical_file_path = vistr_correspond_claim.corr.document_name

This.Visible = FALSE

IF NOT FileExists(physical_file_path) THEN
	   MessageBox(title,"Unable to locate file  - " + physical_file_path + &
		+ "~n~r~n~rPlease select a different document or Contact Helpdesk for assistance",Information!)
			cb_cancel.TriggerEvent(Clicked!)
	Return
END IF

/* Set database object for drop down datawindow
*/

dw_doc_type.SetTransObject(ImageTrans)
ll_row_nbr = dw_doc_type.InsertRow(0)

/* Get the child datawindw name and Get value for variable from drop down data window.
*/	
dw_doc_type.GetChild("type_code",idw_ddw_document_types)
vistr_correspond_claim.corr.comments =idw_ddw_document_types.GetItemString(idw_ddw_document_types.GetRow(),"type_desc")
dw_doc_type.SetItem(1,"type_code",vistr_correspond_claim.corr.document_type)
dw_doc_type.SetItem(1,"comment",vistr_correspond_claim.corr.comments)
ll_row_nbr = dw_doc_type.Retrieve(vistr_correspond_claim.corr.doc_id)
ImageTrans.nf_handle_error("dw_doc_type","w_edit","on open event")

IF ll_row_nbr = 0 THEN
	MessageBox(this.title,"No rows found for document type in imaging database" &
	+ "~n~r~n~rPLEASE CONTACT YOUR SYSTEM ADMINISTRATOR",StopSign!)
	Return
END IF

IF IsValid(viw_correspond) THEN
	lw_frame = viw_correspond.ParentWindow().ParentWindow()  // w_frame
	THIS.x = lw_frame.x + (lw_frame.width - THIS.Width)/2
	THIS.y = lw_frame.y + (lw_frame.height - THIS.Height)/2
END IF
This.Visible = TRUE

dw_doc_type.SetFocus()
this.Postevent("ue_post_open")
end event

on closequery;// ******************************************************************************************************************************
// REFRESH THE "CORRESPOND" WINDOW AND THE "MAINTAIN" WINDOW
w_frame.enabled = True

If IsValid(viw_correspond) THEN
	viw_correspond.wf_reset_buttons(True, "")
	viw_correspond.cb_edit.SetFocus()
ELSE
	MessageBox (title,"Not able to display focus in window properly",INFORMATION!)
END IF 

end on

on w_edit.create
this.dw_doc_type=create dw_doc_type
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
this.gb_1=create gb_1
this.Control[]={this.dw_doc_type,&
this.cb_ok,&
this.cb_cancel,&
this.gb_1}
end on

on w_edit.destroy
destroy(this.dw_doc_type)
destroy(this.cb_ok)
destroy(this.cb_cancel)
destroy(this.gb_1)
end on

type dw_doc_type from u_dw_online within w_edit
integer x = 82
integer y = 96
integer width = 1728
integer height = 312
integer taborder = 20
string dataobject = "d_doc_type"
boolean border = false
end type

on itemchanged;call u_dw_online::itemchanged;// Declare local variables
string	ls_col_value,ls_col_selected

// Get the row number and values
	vii_row_nbr = idw_ddw_document_types.GetRow()
	ls_col_value = Gettext()
	ls_col_selected = GetcolumnName()
	
	Choose Case ls_col_selected
		Case "type_code"
			vistr_correspond_claim.corr.document_type = Trim(idw_ddw_document_types.GetItemString(vii_row_nbr,"type_code"))
			vistr_correspond_claim.corr.comments =idw_ddw_document_types.GetItemString(idw_ddw_document_types.GetRow(),"type_desc")
			dw_doc_type.SetItem(1,"type_code",vistr_correspond_claim.corr.document_type)
			dw_doc_type.SetItem(1,"comment",vistr_correspond_claim.corr.comments)
	End Choose 

end on

type cb_ok from commandbutton within w_edit
event activatecorrespond pbm_custom01
integer x = 878
integer y = 480
integer width = 384
integer height = 96
integer taborder = 30
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
boolean default = true
end type

event clicked;// ************************************************************************************************
// AUTHOR:	MW George
// Modified:	94.10.28

// ************************************************************************************************
// DECLARATIONS

long		rc
string	vls_msg
int li_rc

// ***************************************************************************************************************
// SET THE DOCUMENT PATHS AND PHYSICAL FILE NAME
// NOTE:
// TO PROPERLY FORMAT THE FILEOPEN COMMAND, THE DOC_PATH BELOW IS ENCLOSED WITHIN
// SINGLE QUOTES TO ALLOW THE STRING ITSELF TO CONTAIN DOUBLE QUOTES WHICH IS WHAT
// WORDBASIC REQUIRES FOR THE FILE NAME.
// Added by Earl Assoon on Feb. 16, 1995 to avoid double clicking of buttons after they've been clicked
cb_ok.enabled = FALSE
cb_cancel.enabled = FALSE
//  End of changes by Earl
/*	Get the latest document type that was chosen by the user 
*/

IF dw_doc_type.Accepttext() = -1 THEN 
	MessageBox (parent.title,"Unable to save document comments"&
	+"~n~r Please contact the help desk", Information!)
	dw_doc_type.SetFocus()
	cb_ok.enabled = TRUE
	cb_cancel.enabled = TRUE
	return
END IF

vistr_correspond_claim.corr.comments = dw_doc_type.GETITEMSTRING(1,"comment")

vls_msg = "Editing " + Trim(vistr_correspond_claim.corr.template_type) + ", Please wait..."

/* Disable main window, so user loses control of the application,
	 when communcation with word is timing out. (user can't quit until
	 the error is displayed after 60 sec.)
*/
w_frame.enabled = False


SQLCA.nf_begin_transaction()

UPDATE	CORRESPONDENCE
SET			user_comments = :vistr_correspond_claim.corr.comments
WHERE		(claim_no		= :vistr_correspond_claim.claim_no) 	AND
			(correspond_no	= :vistr_correspond_claim.corr.corr_no)
USING 		SQLCA;

// expect one record to be updated
SQLCA.nf_handle_error('w_edit','UPDATE CORRESPONDENCE','cb_ok.clicked',1)
	
// begin transaction - IMARA db
ImageTrans.nf_begin_transaction()

rc = wf_update_imara()
if rc = 0 then	
	// commit both transactions
	SQLCA.nf_commit_transaction()	
	ImageTrans.nf_commit_transaction()	
else
	SQLCA.nf_rollback_transaction()
end if


close(parent)
Return






end event

type cb_cancel from commandbutton within w_edit
integer x = 1344
integer y = 480
integer width = 384
integer height = 96
integer taborder = 40
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cance&l"
boolean cancel = true
end type

on clicked;close (parent)

end on

type gb_1 from groupbox within w_edit
integer x = 27
integer y = 24
integer width = 1810
integer height = 432
integer taborder = 10
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Detail"
end type

