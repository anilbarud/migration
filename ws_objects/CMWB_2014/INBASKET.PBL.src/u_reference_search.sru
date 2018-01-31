$PBExportHeader$u_reference_search.sru
$PBExportComments$Visual User Object that finds and displays a document by searching for the document using the reference_no.
forward
global type u_reference_search from userobject
end type
type uo_image_append from u_image_append within u_reference_search
end type
type sle_referenceno from singlelineedit within u_reference_search
end type
type dw_documents_by_reference_no from u_dw_online within u_reference_search
end type
type dw_document_path from u_dw_document_path within u_reference_search
end type
type cb_view from commandbutton within u_reference_search
end type
type gb_reference from groupbox within u_reference_search
end type
end forward

global type u_reference_search from userobject
integer width = 1230
integer height = 156
long backcolor = 67108864
long tabtextcolor = 33554432
uo_image_append uo_image_append
sle_referenceno sle_referenceno
dw_documents_by_reference_no dw_documents_by_reference_no
dw_document_path dw_document_path
cb_view cb_view
gb_reference gb_reference
end type
global u_reference_search u_reference_search

type variables
u_dw_document_path 	iu_dw_document_path
BOOLEAN		ib_is_search_expanded
LONG                                 il_window_handle

end variables

forward prototypes
public function boolean f_view_document (long al_rowtouse)
public subroutine uf_set_parent (long al_parent)
end prototypes

public function boolean f_view_document (long al_rowtouse);/*	This function is used to view the document contained on the choosen row in 
	dw_documents_by_reference_no.

	Arguments:	al_rowtouse - The number of the row in dw_documents_by_reference_no containing
									  the selected document to view. 
*/
	LONG		ll_docid
	INTEGER	li_rowsreturned
	string ls_doc_type

	ll_docid = dw_documents_by_reference_no.GetItemNumber(al_rowtouse,"docid")
	
	if isvalid(uo_image_append) then
		
		/* check to see if new viewe should be called */
			if uo_image_append.of_init(ll_docid)	<= 0 then
				RETURN false
			end if
				
				
			ls_doc_type =  uo_image_append.of_get_file_type()
				
			
			CHOOSE CASE ls_doc_type
				/*  Imaged document */ 
				CASE 'IMA', 'TIF'
					uo_image_append.of_set_option()
					li_rowsreturned = uo_image_append.of_append_image(ll_docid) 
					if li_rowsreturned < 1 then 
						MessageBox("Imaged Document","The requested imaged document was not found.")	
						return false
					end if
				case else
					/* execel and word */
					li_rowsreturned = iu_dw_document_path.f_manage_document(ll_docid,"V","NORMAL")
			end choose
		
			
			IF li_rowsreturned = 0 THEN
				MessageBox("Imaged Document","The requested imaged document was not found.")
				RETURN FALSE
			END IF
	
	
	end if
	
	
	
	
	
	

	RETURN TRUE
end function

public subroutine uf_set_parent (long al_parent);	il_window_handle = al_parent
	iu_dw_document_path.uf_set_window_handle(il_window_handle)

end subroutine

event constructor;/*	Create an instance of the user object for the view/print function
*/
	iu_dw_document_path = dw_document_path
	iu_dw_document_path.Hide()
/*	Set the transaction object for the retrieve of DOCUMENT_INDEX records. Also set the datawindow
	to select 1 row at a time.
*/
	dw_documents_by_reference_no.SetTransObject(ImageTrans)
	dw_documents_by_reference_no.uf_SetSelect(1)

end event

on u_reference_search.create
this.uo_image_append=create uo_image_append
this.sle_referenceno=create sle_referenceno
this.dw_documents_by_reference_no=create dw_documents_by_reference_no
this.dw_document_path=create dw_document_path
this.cb_view=create cb_view
this.gb_reference=create gb_reference
this.Control[]={this.uo_image_append,&
this.sle_referenceno,&
this.dw_documents_by_reference_no,&
this.dw_document_path,&
this.cb_view,&
this.gb_reference}
end on

on u_reference_search.destroy
destroy(this.uo_image_append)
destroy(this.sle_referenceno)
destroy(this.dw_documents_by_reference_no)
destroy(this.dw_document_path)
destroy(this.cb_view)
destroy(this.gb_reference)
end on

type uo_image_append from u_image_append within u_reference_search
boolean visible = false
integer x = 1161
integer y = 92
integer taborder = 20
end type

on uo_image_append.destroy
call u_image_append::destroy
end on

type sle_referenceno from singlelineedit within u_reference_search
integer x = 183
integer y = 60
integer width = 498
integer height = 72
integer taborder = 10
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
borderstyle borderstyle = stylelowered!
end type

type dw_documents_by_reference_no from u_dw_online within u_reference_search
integer x = 46
integer y = 192
integer width = 1125
integer height = 528
integer taborder = 30
string dataobject = "d_documents_by_reference_no"
boolean hscrollbar = true
boolean vscrollbar = true
end type

on doubleclicked;call u_dw_online::doubleclicked;/* Call function to view the document on the row that was double-clicked.
*/
	IF f_view_document(THIS.GetRow()) THEN
		ib_is_search_expanded = FALSE
		gb_reference.Height = 153
		PARENT.Height = 155
		PARENT.BringToTop = FALSE
	END IF

/*	After viewing the document for the selected row, deselect the row.
*/
	THIS.SelectRow(0,FALSE)
end on

type dw_document_path from u_dw_document_path within u_reference_search
boolean visible = false
integer x = 288
integer y = 228
integer taborder = 0
end type

type cb_view from commandbutton within u_reference_search
integer x = 869
integer y = 44
integer width = 247
integer height = 92
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Vie&w"
end type

event clicked;LONG		ll_referenceno, ll_docid, ll_rowsindw
INTEGER	li_errorstatus
BOOLEAN	lb_result

/*	Check to see if a retrieve should be done, or the document on the selected row in
	dw_documents_by_reference_no should be viewed.
*/
	IF dw_documents_by_reference_no.RowCount() > 0 and dw_documents_by_reference_no.GetSelectedRow(0) > 0 THEN
		dw_documents_by_reference_no.TriggerEvent(DoubleClicked!)
	ELSE

/*	Retrieve all the DOCUMENT_INDEX records for the given reference_no. But check to see if
	a value has been entered and that it is a number.
*/
		IF sle_referenceno.Text = "" THEN
			MessageBox("Invalid Reference Number","A reference number must be entered.")
			RETURN
		END IF

		IF IsNumber(sle_referenceno.Text) THEN
			ll_referenceno = Long(sle_referenceno.Text)
		ELSE
			MessageBox("Invalid Reference Number","The reference number can not contain letters.")
			RETURN
		END IF

/*	The current version of SQL Server and PowerBuilder can only assign numbers too int and long
	datatypes respectively of size (2^31 - 1) to -2^31,( that is -2,147,483,648 to 2,147,483,647 inclusive).
	Therefore need to check to see if the entered number is outside this range. Look at the error code
	returned by the retrieve. A number entered outside the allowable range should return a code of 232.
*/
		ll_rowsindw = dw_documents_by_reference_no.Retrieve(ll_referenceno)
		IF imagetrans.SQLDBCode = 232 THEN
			MessageBox("Invalid Reference No","The entered reference number must be between -2,147,483,648 and 2,147,483,647.")
			imagetrans.SQLDBCode = 0
			imagetrans.SQLErrText = ""
			RETURN
		END IF
		IF ll_rowsindw = 1 THEN
			ib_is_search_expanded = FALSE
		ELSE
			ib_is_search_expanded = TRUE
		END IF


/*	Get the document id for selected row and view the document.
*/
		IF ll_rowsindw > 1 THEN
			PARENT.Height = 745
			gb_reference.Height = 741
			PARENT.BringToTop = TRUE
		ELSE
			IF ll_rowsindw = 0 THEN
				MessageBox("Reference Number Search Result","No document was found containing the entered reference_no.")
			ELSE
				lb_result = f_view_document(1)
			END IF
		END IF
	END IF

/*	Set focus back to the application.
*/
	PARENT.SetFocus()

end event

type gb_reference from groupbox within u_reference_search
integer x = 5
integer width = 1211
integer height = 152
integer taborder = 40
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Ref #/Batch ID Search"
end type

