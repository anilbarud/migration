$PBExportHeader$w_individual_doclist.srw
forward
global type w_individual_doclist from window
end type
type uo_image_append from u_image_append within w_individual_doclist
end type
type dw_document_path from u_dw_document_path within w_individual_doclist
end type
type uo_filter_control from u_dw_filter_control within w_individual_doclist
end type
type st_role_text from statictext within w_individual_doclist
end type
type st_number_text from statictext within w_individual_doclist
end type
type st_name_text from statictext within w_individual_doclist
end type
type st_role from statictext within w_individual_doclist
end type
type st_number from statictext within w_individual_doclist
end type
type st_name from statictext within w_individual_doclist
end type
type cb_close from commandbutton within w_individual_doclist
end type
type dw_doc_list from u_datawindow within w_individual_doclist
end type
end forward

global type w_individual_doclist from window
integer width = 4160
integer height = 2076
boolean titlebar = true
string title = "Individual/Claim Document List"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
uo_image_append uo_image_append
dw_document_path dw_document_path
uo_filter_control uo_filter_control
st_role_text st_role_text
st_number_text st_number_text
st_name_text st_name_text
st_role st_role
st_number st_number
st_name st_name
cb_close cb_close
dw_doc_list dw_doc_list
end type
global w_individual_doclist w_individual_doclist

type variables
LONG							il_document_row_number
STRING						is_minimized_title
ULONG						iul_handle
u_dw_document_path 	iu_dw_document_path
end variables

forward prototypes
public function integer wf_initial_module_filter (string as_filter)
end prototypes

public function integer wf_initial_module_filter (string as_filter);INTEGER li_rtn
STRING	ls_filter_return, ls_rtn, ls_modify

// filter specific doc types for module
ls_filter_return = uo_filter_control.idw_datawindow.inv_filter.of_SetFilter(as_filter)
uo_filter_control.idw_datawindow.inv_filter.ib_filter_on = TRUE

ls_modify = 'st_filter.Text="'+ls_filter_return+ '"'

ls_rtn = dw_doc_list.Modify(ls_modify)


RETURN li_rtn
end function

on w_individual_doclist.create
this.uo_image_append=create uo_image_append
this.dw_document_path=create dw_document_path
this.uo_filter_control=create uo_filter_control
this.st_role_text=create st_role_text
this.st_number_text=create st_number_text
this.st_name_text=create st_name_text
this.st_role=create st_role
this.st_number=create st_number
this.st_name=create st_name
this.cb_close=create cb_close
this.dw_doc_list=create dw_doc_list
this.Control[]={this.uo_image_append,&
this.dw_document_path,&
this.uo_filter_control,&
this.st_role_text,&
this.st_number_text,&
this.st_name_text,&
this.st_role,&
this.st_number,&
this.st_name,&
this.cb_close,&
this.dw_doc_list}
end on

on w_individual_doclist.destroy
destroy(this.uo_image_append)
destroy(this.dw_document_path)
destroy(this.uo_filter_control)
destroy(this.st_role_text)
destroy(this.st_number_text)
destroy(this.st_name_text)
destroy(this.st_role)
destroy(this.st_number)
destroy(this.st_name)
destroy(this.cb_close)
destroy(this.dw_doc_list)
end on

event open;BOOLEAN				lb_rtn
INTEGER					li_rtn, li_upperbound
LONG						ll_claim_no, ll_individual_no, ll_rows
STRING					ls_claim_role_code, ls_module_code
STRING					ls_name, ls_filter, ls_module_desc
STRING					ls_filter_return, ls_rtn, ls_modify
s_window_message	lstr_message

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


lstr_message = Message.PowerObjectParm

ls_claim_role_code = lstr_message.as_stringparm[1]
ls_module_code = lstr_message.as_stringparm[2]
ll_claim_no = lstr_message.al_doubleparm[1]
ll_individual_no = lstr_message.al_doubleparm[2]

iul_handle = Handle(This)

// sets up to close this window if frame is closed
li_upperbound = UpperBound(gstr_window_array) + 1
gstr_window_array[li_upperbound].window_element = THIS
gstr_window_array[li_upperbound].handle_element = iul_handle

dw_doc_list.SetTransObject(SQLCA)

SetRedraw(FALSE)


ll_rows = dw_doc_list.Retrieve(ls_claim_role_code, ll_claim_no, ll_individual_no)
SQLCA.nf_handle_error('w_individual_document_list', 'open', 'dw_doc_list.Retrieve')


SELECT	given_names + ' ' + last_name
INTO		:ls_name
FROM		INDIVIDUAL
WHERE	individual_no = :ll_individual_no
USING SQLCA;
SQLCA.nf_handle_error('w_individual_document_list', 'open', 'SELECT given_names + last_name')

SELECT	module_desc
INTO		:ls_module_desc
FROM		Module
WHERE	module_code = :ls_module_code
USING SQLCA;
SQLCA.nf_handle_error('w_individual_document_list', 'open', 'SELECT module_desc from Module')


st_name_text.Text = ls_name

CHOOSE CASE ls_claim_role_code
	CASE 'C','SS'
		st_role.Visible = TRUE
		st_role_text.Visible = TRUE
				
		IF ls_claim_role_code = 'C' THEN
			st_number.Text = 'Individual:'
			st_number_text.Text = String(ll_individual_no)
			st_role_text.Text = 'Claimant'
		ELSE
			st_number.Text = 'Claim:'
			st_number_text.Text = String(ll_claim_no)
			st_role_text.Text = 'Surviving Spouse'
		END IF
		
	CASE 'X'
		st_number.Text = 'Claim:'
		st_number_text.Text = String(ll_claim_no)		
		
		st_role.Visible = FALSE
		st_role_text.Visible = FALSE
END CHOOSE

is_minimized_title = 'Doc List - '+ ls_module_desc +' - '+ st_role_text.Text +' - '+ ls_name

SetRedraw(TRUE)


dw_doc_list.uf_setfilter(TRUE)
uo_filter_control.uf_set_requestor(dw_doc_list)


// filter specific doc types for module
ls_filter = "module_code = '"+ls_module_code+"'"
ls_filter_return = uo_filter_control.idw_datawindow.inv_filter.of_SetFilter(ls_filter)
uo_filter_control.idw_datawindow.inv_filter.ib_filter_on = TRUE

ls_modify = 'st_filter.Text="'+ls_filter_return+ '"'

ls_rtn = dw_doc_list.Modify(ls_modify)



/*	Create an instance of the user object for the view/print function
*/
iu_dw_document_path = dw_document_path
iu_dw_document_path.Hide()
iu_dw_document_path.uf_set_window_handle(iul_handle)


end event

event close;n_common_annuity	lnv_common_annuity

lnv_common_annuity = Create n_common_annuity

lnv_common_annuity.nf_close_handle_array(iul_handle)


end event

event resize;if This.WindowState = Minimized! then
	this.title = is_minimized_title
else
	this.title = "Individual/Claim Document List"
END IF
end event

type uo_image_append from u_image_append within w_individual_doclist
boolean visible = false
integer x = 1906
integer y = 1796
integer width = 334
integer height = 144
integer taborder = 40
end type

on uo_image_append.destroy
call u_image_append::destroy
end on

type dw_document_path from u_dw_document_path within w_individual_doclist
boolean visible = false
integer x = 2331
integer y = 1836
integer taborder = 30
end type

type uo_filter_control from u_dw_filter_control within w_individual_doclist
integer x = 59
integer y = 1760
integer taborder = 20
end type

on uo_filter_control.destroy
call u_dw_filter_control::destroy
end on

event ue_filter_changed;call super::ue_filter_changed;STRING	ls_rtn, ls_modify

// filter specific doc types for module
ls_modify = 'st_filter.Text="'+ls_new_filter+ '"'

ls_rtn = dw_doc_list.Modify(ls_modify)

end event

type st_role_text from statictext within w_individual_doclist
integer x = 3177
integer y = 40
integer width = 891
integer height = 96
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean focusrectangle = false
end type

type st_number_text from statictext within w_individual_doclist
integer x = 2263
integer y = 40
integer width = 466
integer height = 96
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean focusrectangle = false
end type

type st_name_text from statictext within w_individual_doclist
integer x = 361
integer y = 40
integer width = 1248
integer height = 96
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean focusrectangle = false
end type

type st_role from statictext within w_individual_doclist
integer x = 2953
integer y = 40
integer width = 201
integer height = 96
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Role: "
alignment alignment = right!
boolean focusrectangle = false
end type

type st_number from statictext within w_individual_doclist
integer x = 1851
integer y = 40
integer width = 375
integer height = 96
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Individual:"
alignment alignment = right!
boolean focusrectangle = false
end type

type st_name from statictext within w_individual_doclist
integer x = 64
integer y = 40
integer width = 265
integer height = 96
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Name: "
alignment alignment = right!
boolean focusrectangle = false
end type

type cb_close from commandbutton within w_individual_doclist
integer x = 3666
integer y = 1808
integer width = 402
integer height = 104
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Close"
boolean default = true
end type

event clicked;Close(Parent)
end event

type dw_doc_list from u_datawindow within w_individual_doclist
integer x = 55
integer y = 192
integer width = 4009
integer height = 1564
integer taborder = 10
string dataobject = "d_individual_document_list"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event constructor;call super::constructor;THIS.uf_setselect(1)
THIS.uf_SetSort(True)

post uf_set_window(PARENT)
end event

event rbuttondown;m_dw_rmb_popup lm_popup
m_doclist_popup   lm_doclist_popup
/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/


lm_doclist_popup  = Create m_doclist_popup
lm_doclist_popup.mf_set_datawindow(This)
lm_doclist_popup.PopMenu(PARENT.PointerX( ), PARENT.PointerY( ))

IF lm_doclist_popup.ib_other_sort_requested THEN
	
	
	lm_popup = Create m_dw_rmb_popup
	lm_popup.mf_set_datawindow(This)
	lm_popup.m_options.m_sort.visible = TRUE
	
	// don't display lines
	lm_popup.m_options.m_1.Visible = FALSE
	lm_popup.m_options.m_2.Visible = FALSE
	
	
	lm_popup.m_options.PopMenu(PARENT.PointerX( ), PARENT.PointerY( ))

END IF


end event

event doubleclicked;call super::doubleclicked;LONG    ll_doc_id
string ls_doc_type

/*	Get the number of the row that was selected
	Only continue if a row was selected
*/

	il_document_row_number = row
	IF il_document_row_number <= 0 THEN
		RETURN
	END IF

/*	Get the document id for selected row
	View the document
*/
	ll_doc_id =dw_doc_list.GetItemNumber (il_document_row_number,"docid")
	
	
	/*	Get the document id for selected row
	View the document
*/	
	if uo_image_append.of_init(ll_doc_id)	<= 0 then
		RETURN
	end if
		
		
	ls_doc_type =  uo_image_append.of_get_file_type()
		
	
	CHOOSE CASE ls_doc_type
		/*  Imaged document */ 
		CASE 'IMA', 'TIF'
			if uo_image_append.of_append_image(ll_doc_id) < 0 then	RETURN
		case else
			iu_dw_document_path.f_manage_document(ll_doc_id,"V","NORMAL")
	end choose
			
	
end event

