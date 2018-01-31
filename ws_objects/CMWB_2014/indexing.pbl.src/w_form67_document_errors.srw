$PBExportHeader$w_form67_document_errors.srw
forward
global type w_form67_document_errors from window
end type
type cb_print from commandbutton within w_form67_document_errors
end type
type st_module_source_desc from statictext within w_form67_document_errors
end type
type st_4 from statictext within w_form67_document_errors
end type
type st_docnpgs from statictext within w_form67_document_errors
end type
type st_3 from statictext within w_form67_document_errors
end type
type st_docname from statictext within w_form67_document_errors
end type
type st_2 from statictext within w_form67_document_errors
end type
type st_fldname from statictext within w_form67_document_errors
end type
type st_1 from statictext within w_form67_document_errors
end type
type dw_form67_document_errors from u_dw_online within w_form67_document_errors
end type
type cb_close from commandbutton within w_form67_document_errors
end type
end forward

global type w_form67_document_errors from window
integer width = 2638
integer height = 1648
boolean titlebar = true
string title = "Form 67 Document Errors"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
cb_print cb_print
st_module_source_desc st_module_source_desc
st_4 st_4
st_docnpgs st_docnpgs
st_3 st_3
st_docname st_docname
st_2 st_2
st_fldname st_fldname
st_1 st_1
dw_form67_document_errors dw_form67_document_errors
cb_close cb_close
end type
global w_form67_document_errors w_form67_document_errors

on w_form67_document_errors.create
this.cb_print=create cb_print
this.st_module_source_desc=create st_module_source_desc
this.st_4=create st_4
this.st_docnpgs=create st_docnpgs
this.st_3=create st_3
this.st_docname=create st_docname
this.st_2=create st_2
this.st_fldname=create st_fldname
this.st_1=create st_1
this.dw_form67_document_errors=create dw_form67_document_errors
this.cb_close=create cb_close
this.Control[]={this.cb_print,&
this.st_module_source_desc,&
this.st_4,&
this.st_docnpgs,&
this.st_3,&
this.st_docname,&
this.st_2,&
this.st_fldname,&
this.st_1,&
this.dw_form67_document_errors,&
this.cb_close}
end on

on w_form67_document_errors.destroy
destroy(this.cb_print)
destroy(this.st_module_source_desc)
destroy(this.st_4)
destroy(this.st_docnpgs)
destroy(this.st_3)
destroy(this.st_docname)
destroy(this.st_2)
destroy(this.st_fldname)
destroy(this.st_1)
destroy(this.dw_form67_document_errors)
destroy(this.cb_close)
end on

event open;Long    ll_num_rows, ll_docid, ll_docnpgs
Integer li_rtn
String  ls_fldname, ls_docname, ls_module_source_desc 

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


li_rtn = dw_form67_document_errors.SetTransObject(SQLCA)

ll_docid = Message.DoubleParm 

ll_num_rows = dw_form67_document_errors.Retrieve(ll_docid)
li_rtn = SQLCA.nf_handle_error("w_form67_document_errors", "dw_form67_document_errors", "open - dw_form67_document_errors.Retrieve(ll_docid)")

SELECT MAX(FLD.fldname), MAX(DOC.docname), MAX(DOC.docnpgs), MAX(Module_Source.module_source_desc) 
  INTO :ls_fldname, :ls_docname, :ll_docnpgs, :ls_module_source_desc 
  FROM DOC, 
       REF, 
       FLD, 
       Module_Source 
 WHERE DOC.docid = :ll_docid  
   AND DOC.docid = REF.docid  
   AND REF.docfldid = FLD.fldid 
   AND DOC.module_source_code = Module_Source.module_source_code 
	USING imagetrans ; 

li_rtn = imagetrans.nf_handle_error("w_form67_document_errors", "", "open - SELECT MAX(FLD.fldname) ... FROM DOC, REF, FLD, Module_Source...")

st_docname.Text = ls_docname 
st_docnpgs.Text = String(ll_docnpgs)
st_fldname.Text = ls_fldname
st_module_source_desc.Text = ls_module_source_desc

end event

type cb_print from commandbutton within w_form67_document_errors
integer x = 965
integer y = 1464
integer width = 311
integer height = 92
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Print"
end type

event clicked;//print the datawindow don't really care if there are any rows.
dw_form67_document_errors.print()
end event

type st_module_source_desc from statictext within w_form67_document_errors
integer x = 549
integer y = 344
integer width = 1033
integer height = 68
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean focusrectangle = false
end type

type st_4 from statictext within w_form67_document_errors
integer x = 18
integer y = 344
integer width = 503
integer height = 68
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Document Source:"
boolean focusrectangle = false
end type

type st_docnpgs from statictext within w_form67_document_errors
integer x = 549
integer y = 236
integer width = 1033
integer height = 68
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean focusrectangle = false
end type

type st_3 from statictext within w_form67_document_errors
integer x = 18
integer y = 236
integer width = 471
integer height = 68
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Number of Pages:"
boolean focusrectangle = false
end type

type st_docname from statictext within w_form67_document_errors
integer x = 549
integer y = 124
integer width = 1033
integer height = 68
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean focusrectangle = false
end type

type st_2 from statictext within w_form67_document_errors
integer x = 18
integer y = 124
integer width = 448
integer height = 68
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Document Name:"
boolean focusrectangle = false
end type

type st_fldname from statictext within w_form67_document_errors
integer x = 549
integer y = 16
integer width = 1033
integer height = 68
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean focusrectangle = false
end type

type st_1 from statictext within w_form67_document_errors
integer x = 18
integer y = 16
integer width = 366
integer height = 68
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Folder Name:"
boolean focusrectangle = false
end type

type dw_form67_document_errors from u_dw_online within w_form67_document_errors
integer x = 14
integer y = 456
integer width = 2601
integer height = 984
integer taborder = 10
string title = "none"
string dataobject = "d_form67_document_errors"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

type cb_close from commandbutton within w_form67_document_errors
integer x = 1385
integer y = 1464
integer width = 311
integer height = 92
integer taborder = 10
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Close"
end type

event clicked;Close(Parent)
end event

