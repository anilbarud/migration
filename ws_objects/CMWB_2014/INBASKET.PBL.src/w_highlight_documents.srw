$PBExportHeader$w_highlight_documents.srw
$PBExportComments$Window to allow user to choose document types that they want highlighted.  Applies choices to w_sheet's document list
forward
global type w_highlight_documents from window
end type
type dw_select_document_class from u_dw_online within w_highlight_documents
end type
type dw_select_document_type from u_dw_online within w_highlight_documents
end type
type cb_ok from commandbutton within w_highlight_documents
end type
type cb_close from commandbutton within w_highlight_documents
end type
end forward

global type w_highlight_documents from window
integer x = 2199
integer y = 688
integer width = 2405
integer height = 1488
boolean titlebar = true
string title = "Highlight Documents"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 67108864
dw_select_document_class dw_select_document_class
dw_select_document_type dw_select_document_type
cb_ok cb_ok
cb_close cb_close
end type
global w_highlight_documents w_highlight_documents

type variables
BOOLEAN	I_Authorized_Access
LONG		il_dw_select_document_class_anchor_row
LONG		il_dw_select_document_type_anchor_row

end variables

event open;/* APPLICATION SECURITY CODE.
*/
	G_PFSecurity.UOF_Check_Access(THIS)
	THIS.I_Authorized_Access = TRUE				//declared as an instance variable.

	LONG	ll_function_results_code, ll_rownum

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


	dw_select_document_class.SetTransObject(ImageTrans)
	dw_select_document_type.SetTransObject(ImageTrans)

/* Get all the document classes.
*/
	ll_function_results_code = dw_select_document_class.Retrieve()
	IF ImageTrans.nf_handle_error("dw_select_document_class","w_select_documents","Open") < 0 THEN
		Close(THIS)
		RETURN
	END IF

/* If no rows found then where are the document classes ???
*/
	IF ll_function_results_code <= 0 THEN
	   MessageBox("Document Classes","No documents classes found.",StopSign!,OK!)
   	Close(THIS)
		RETURN
	END IF

/* Make sure none of the classes are selected.
*/
	ll_rownum = 1
	DO UNTIL ll_rownum > ll_function_results_code
		dw_select_document_class.SetItem(ll_rownum,"class_check",'N')
		ll_rownum++
	LOOP

/* Get all the document types.
*/
	ll_function_results_code = dw_select_document_type.Retrieve()
	IF ImageTrans.nf_handle_error("w_select_document_type","w_select_documents","Open") < 0 THEN
		Close(THIS)
		RETURN
	END IF

/* If no rows found then where are the document types ???
*/
	IF ll_function_results_code <= 0 THEN
   	MessageBox("Document Types","No documents types found.",StopSign!,OK!)
	   Close(THIS)
		RETURN
	END IF

/* Make sure none of the types are selected.
*/
	ll_rownum = 1
	DO UNTIL ll_rownum > ll_function_results_code
		dw_select_document_type.SetItem(ll_rownum,"document_check",'N')
		ll_rownum++
	LOOP

/* Hide all the document types until a class is accepted.
*/
	dw_select_document_type.SetFilter("document_check='Y'")
	dw_select_document_type.Filter()
	il_dw_select_document_class_anchor_row = 0
	il_dw_select_document_type_anchor_row = 0

end event

on w_highlight_documents.create
this.dw_select_document_class=create dw_select_document_class
this.dw_select_document_type=create dw_select_document_type
this.cb_ok=create cb_ok
this.cb_close=create cb_close
this.Control[]={this.dw_select_document_class,&
this.dw_select_document_type,&
this.cb_ok,&
this.cb_close}
end on

on w_highlight_documents.destroy
destroy(this.dw_select_document_class)
destroy(this.dw_select_document_type)
destroy(this.cb_ok)
destroy(this.cb_close)
end on

type dw_select_document_class from u_dw_online within w_highlight_documents
integer x = 37
integer y = 36
integer width = 974
integer height = 1128
integer taborder = 20
string dataobject = "d_select_document_class"
boolean vscrollbar = true
borderstyle borderstyle = styleraised!
end type

on itemchanged;LONG		ll_rownum , ll_rowsindw
STRING	ls_document_class, ls_document_check

/* Highlight current row.
*/
	ll_rownum = GetRow()
	IF ll_rownum = 0 THEN
		RETURN
	END IF
	SelectRow(0,FALSE)
	SelectRow(ll_rownum,TRUE)

/* Include the combined forms.
*/
	ls_document_class = "class_code = '" + GetItemString(ll_rownum,"class_code") + "'"
	IF GetItemString(ll_rownum,"class_code") = 'A' THEN
		ls_document_class = ls_document_class + " OR type_code = 'SDC' OR type_code = 'SDD' OR type_code = 'MPC' OR type_code = 'MPD'"
	END IF

	dw_select_document_type.SetFilter("")
	dw_select_document_type.Filter()	
	dw_select_document_type.SetFilter(ls_document_class)
	dw_select_document_type.Filter()	

/* If a document class has just been selected then check all the document types in that class
	or uncheck them all.
*/
	IF GetText() = "Y" THEN
		ls_document_check = "Y"
	ELSE
		ls_document_check = "N"
	END IF

	ll_rownum = 1
	ll_rowsindw = dw_select_document_type.RowCount()
	DO UNTIL ll_rownum > ll_rowsindw
		dw_select_document_type.SetItem(ll_rownum,"document_check",ls_document_check)
		ll_rownum++
	LOOP

end on

type dw_select_document_type from u_dw_online within w_highlight_documents
integer x = 1061
integer y = 36
integer width = 1253
integer height = 1128
integer taborder = 10
string dataobject = "d_select_document_type"
boolean vscrollbar = true
borderstyle borderstyle = styleraised!
end type

type cb_ok from commandbutton within w_highlight_documents
integer x = 599
integer y = 1216
integer width = 334
integer height = 108
integer taborder = 40
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
boolean default = true
end type

on clicked;LONG		ll_function_results_code, ll_selected_row, ll_document_row, ll_rowsindw
STRING	ls_selected_type
W_SHEET	lw_active_sheet

/* Disable button to prevent second clicking.
*/
	Enabled = FALSE
	cb_close.Enabled = FALSE

/* Process last entry in the datawindows.
*/
	IF dw_select_document_class.AcceptText() = -1 THEN
		GOTO Normal_Exit
	END IF

	IF dw_select_document_type.AcceptText() = -1 THEN
		GOTO Normal_Exit
	END IF

/* Get the name of the active work sheet.
*/
	lw_active_sheet = w_frame.GetActiveSheet()

/* If valid sheet name found then continue, else abort process.
*/
	IF IsValid(lw_active_sheet) = TRUE THEN

/* Check to make sure there are documents to select.
*/
		ll_rowsindw = lw_active_sheet.dw_documents.RowCount()
	   IF ll_rowsindw <= 0 THEN
   	   MessageBox("Documents","There are no master documents to highlight.",INFORMATION!,OK!)
      	GOTO Normal_Exit
		END IF
	ELSE

/* Cannot determine active sheet. Abort workbench.
*/
		MessageBox("Worksheet","Cannot determine active sheet. Please try again.")
		GOTO Normal_Exit
	END IF

/* Unhighlight everything to start first.
*/
	lw_active_sheet.dw_documents.SelectRow(0,FALSE)

/* Now go through the list of selected document types and see highlight the documents accordingly.
*/
	dw_select_document_type.SetFilter("document_check='Y'")
	dw_select_document_type.Filter()

	ll_selected_row = 1
	DO WHILE ll_selected_row <= ll_rowsindw
		ll_document_row = 0
		ll_document_row = dw_select_document_type.Find("type_code='" + lw_active_sheet.dw_documents.GetItemString(ll_selected_row,"docindex_type") + "'",1,dw_select_document_type.RowCount())

/* If the document type has been selected to be highlighted then highlight it!
*/
		IF ll_document_row > 0 THEN
			IF dw_select_document_type.GetItemString(ll_document_row,"document_check") = "Y" THEN
				lw_active_sheet.dw_documents.SelectRow(ll_selected_row,True)
			END IF
		END IF
		ll_selected_row++
	LOOP
	
Normal_Exit:

/* Enable button.
*/
   Enabled = TRUE
	cb_close.Enabled = TRUE
	RETURN

end on

type cb_close from commandbutton within w_highlight_documents
integer x = 1230
integer y = 1216
integer width = 334
integer height = 108
integer taborder = 30
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Close"
end type

on clicked;/* Disable button to prevent second clicking.
*/
	THIS.Enabled = FALSE
	Close(PARENT)

end on

