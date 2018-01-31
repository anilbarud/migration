$PBExportHeader$w_memotab.srw
$PBExportComments$Response window to show the user any memotabs on a particular document.  Called from Account Payments.
forward
global type w_memotab from window
end type
type dw_memolines from u_dw_online within w_memotab
end type
type dw_document_indexed_date from u_dw_online within w_memotab
end type
type cb_close from commandbutton within w_memotab
end type
type dw_memotab from u_dw_online within w_memotab
end type
end forward

global type w_memotab from window
integer x = 1335
integer y = 688
integer width = 2601
integer height = 1364
boolean titlebar = true
string title = "Memotabs"
windowtype windowtype = response!
long backcolor = 67108864
dw_memolines dw_memolines
dw_document_indexed_date dw_document_indexed_date
cb_close cb_close
dw_memotab dw_memotab
end type
global w_memotab w_memotab

event open;LONG 	 ll_docid, ll_rownum, ll_counter, ll_position, ll_start
STRING ls_select,	ls_line, ls_part

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


	ll_docid = Message.DoubleParm

	dw_memotab.SetTransObject(ImageTrans)
	dw_document_indexed_date.SetTransObject(SQLCA)//ImageTrans

// Populate the datawindows
// If any retrieve no rows THEN display a default message

	dw_document_indexed_date.Retrieve(ll_docid)
	IF SQLCA.nf_handle_error("dw_document_indexed_date","w_memotab","open") < 0 THEN
		Close(This)
		Return
	END IF

	IF dw_document_indexed_date.RowCount() = 0 THEN
		MessageBox("Memotab","No Indexing information found.")
		Close(This)
		Return
	END IF

	IF IsNull(dw_document_indexed_date.GetItemDateTime(1,"doccreated")) THEN
		dw_document_indexed_date.ModIFy("doccreated.Visible=0")
		dw_document_indexed_date.ModIFy("t_note.Visible=1")
	ELSE
		dw_document_indexed_date.ModIFy("doccreated.Visible=1")
		dw_document_indexed_date.ModIFy("t_note.Visible=0")
	END IF

	dw_memotab.Retrieve(ll_docid)
	IF ImageTrans.nf_handle_error("dw_memotab","w_memotab","open event") < 0 THEN
		Close(this)
		Return
	END IF

	IF dw_memotab.RowCount() = 0 THEN
		dw_memotab.InsertRow(0)
		dw_memotab.ModIFy("t_memo.Visible=1")
		dw_memotab.ModIFy("sticky.Visible=0")
		Return
	ELSE
		dw_memotab.ModIFy("t_memo.Visible=0")
		dw_memotab.ModIFy("sticky.Visible=1")
	END IF

// For each memo tab separate the lines by finding the \n

	ll_counter = 1
	ls_part = ""

	DO WHILE ll_counter <= dw_memotab.RowCount()
		ll_start = 1
		ls_select = dw_memotab.GetItemString(ll_counter,"sticky")

		ll_position = Pos(ls_select,"\n",ll_start)

		DO WHILE ll_position <> 0 and ll_start < Len(ls_select)
			ls_line  = ls_part + Mid(ls_select, ll_start, ll_position - ll_start)
			IF Len(ls_line) > 80 THEN
				ls_part = Right(ls_line, Len(ls_line) - 79)
				ls_line = Left(ls_line, 80)
			ELSE
				ls_part = ""
			END IF
			ll_start = ll_position + 2
			ll_rownum = dw_memolines.InsertRow(0)
			dw_memolines.SetItem(ll_rownum,"memo",ls_line)
			ll_position = Pos(ls_select,"\n",ll_start)
		LOOP

		ls_line  = Mid(ls_select, ll_start)
		IF Len(ls_line) > 80 THEN
			ls_part = Right(ls_line, Len(ls_line) - 79)
			ls_line = Left(ls_line, 80)
		ELSE
			ls_part = ""
		END IF		
		ll_rownum = dw_memolines.InsertRow(0)
		dw_memolines.SetItem(ll_rownum,"memo",ls_line)

		ll_rownum = dw_memolines.InsertRow(0)
		dw_memolines.SetItem(ll_rownum,"memo",ls_part)

		ls_part = Right(ls_part, Len(ls_part) - 79)
		ll_counter++
	LOOP
end event

on w_memotab.create
this.dw_memolines=create dw_memolines
this.dw_document_indexed_date=create dw_document_indexed_date
this.cb_close=create cb_close
this.dw_memotab=create dw_memotab
this.Control[]={this.dw_memolines,&
this.dw_document_indexed_date,&
this.cb_close,&
this.dw_memotab}
end on

on w_memotab.destroy
destroy(this.dw_memolines)
destroy(this.dw_document_indexed_date)
destroy(this.cb_close)
destroy(this.dw_memotab)
end on

type dw_memolines from u_dw_online within w_memotab
integer x = 14
integer y = 300
integer width = 2565
integer height = 820
integer taborder = 30
string dataobject = "d_memo_lines"
boolean vscrollbar = true
borderstyle borderstyle = styleraised!
end type

type dw_document_indexed_date from u_dw_online within w_memotab
integer x = 14
integer y = 24
integer width = 2560
integer height = 272
integer taborder = 10
string dataobject = "d_document_indexed_date"
borderstyle borderstyle = styleraised!
end type

type cb_close from commandbutton within w_memotab
integer x = 1088
integer y = 1156
integer width = 411
integer height = 108
integer taborder = 40
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Close"
boolean default = true
end type

on clicked;SetPointer(HourGlass!)
Close(Parent)
end on

type dw_memotab from u_dw_online within w_memotab
boolean visible = false
integer x = 18
integer y = 244
integer width = 2226
integer height = 844
integer taborder = 20
boolean enabled = false
string dataobject = "d_memotab"
boolean vscrollbar = true
borderstyle borderstyle = styleraised!
end type

