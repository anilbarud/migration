$PBExportHeader$w_batch_type_report.srw
forward
global type w_batch_type_report from w_a_report
end type
type dw_batch_type from u_dw_online within w_batch_type_report
end type
type cb_retrieve_list from commandbutton within w_batch_type_report
end type
type dw_group_by from u_dw_online within w_batch_type_report
end type
type st_1 from statictext within w_batch_type_report
end type
type dw_batch_list from u_dw_online within w_batch_type_report
end type
type pb_maximize from cb_dw_maximize within w_batch_type_report
end type
end forward

global type w_batch_type_report from w_a_report
integer y = 49
string title = "Batch Report by Type"
dw_batch_type dw_batch_type
cb_retrieve_list cb_retrieve_list
dw_group_by dw_group_by
st_1 st_1
dw_batch_list dw_batch_list
pb_maximize pb_maximize
end type
global w_batch_type_report w_batch_type_report

type variables
DataWindowChild		idwc_type
DataWindowChild		idwc_region
end variables

forward prototypes
public subroutine wf_modify_report_grouping (string as_column_name)
public function integer wf_retrieve_report ()
end prototypes

public subroutine wf_modify_report_grouping (string as_column_name);STRING		ls_return

this.setredraw(false)

ls_return = dw_report.Modify("c_group1.expression = '" + as_column_name + "'")
IF ls_return <> '' Then
	SignalError(-666,"Error modifying datawindow. " + ls_return)
End if	
dw_report.Sort()
dw_report.GroupCalc()

this.setredraw(True)

end subroutine

public function integer wf_retrieve_report ();INT		li_rtn
LONG		ll_batch_no
STRING	ls_column_name
LONG		ll_row

ll_row = dw_batch_list.GetRow()

IF ll_row > 0 THen
	ll_batch_no = dw_batch_list.GetItemNumber(ll_row,'batch_no')
	li_rtn = dw_report.Retrieve(ll_batch_no)
	SQLCA.nf_handle_error('w_batch_type_report','dw_batch_list.doubleclicked','retrieve')
	If li_rtn <= 0 Then SignalError(-666,'Error retrieving batch report')
	
	pb_maximize.event ue_doubleclicked()
	
	dw_report.Sort()
	dw_report.GroupCalc()
Else
	return 0
end if

return 1
end function

on w_batch_type_report.create
int iCurrent
call super::create
this.dw_batch_type=create dw_batch_type
this.cb_retrieve_list=create cb_retrieve_list
this.dw_group_by=create dw_group_by
this.st_1=create st_1
this.dw_batch_list=create dw_batch_list
this.pb_maximize=create pb_maximize
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_batch_type
this.Control[iCurrent+2]=this.cb_retrieve_list
this.Control[iCurrent+3]=this.dw_group_by
this.Control[iCurrent+4]=this.st_1
this.Control[iCurrent+5]=this.dw_batch_list
this.Control[iCurrent+6]=this.pb_maximize
end on

on w_batch_type_report.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.dw_batch_type)
destroy(this.cb_retrieve_list)
destroy(this.dw_group_by)
destroy(this.st_1)
destroy(this.dw_batch_list)
destroy(this.pb_maximize)
end on

event open;call super::open;INTEGER					li_rtn
LONG						ll_row



dw_batch_type.InsertRow(0)
dw_group_by.InsertRow(0)

li_rtn = dw_batch_type.GetChild('admin_region_code',idwc_region)
IF li_rtn <> 1 Then SignalError(-666,'Error getting region datawindow child')
idwc_region.SetTransObject(SQLCA)

li_rtn = dw_batch_type.GetChild('batch_type_code',idwc_type)
IF li_rtn <> 1 Then SignalError(-666,'Error getting type datawindow child')
idwc_type.SetTransObject(SQLCA)

li_rtn = idwc_region.Retrieve()
IF li_rtn <= 0 Then SignalError(-666,'Error retrieving region datawindow child')
SQLCA.nf_handle_error('w_batch_type_report','OPEN','Retrieve regions')

li_rtn = idwc_type.Retrieve()
IF li_rtn <= 0 Then SignalError(-666,'Error retrieving type datawindow child')
SQLCA.nf_handle_error('w_batch_type_report','OPEN','Retrieve type')


//Insert an "ALL" dummy record so the user can get back all regions
ll_row = idwc_region.InsertRow(0)
idwc_region.SetItem(ll_row,'admin_region_code','ALL')
idwc_region.SetItem(ll_row,'admin_region_desc','ALL')

dw_batch_type.SetItem(1,'top_rowS',50)
dw_group_by.SetItem(1,'group_column','benefit_category_desc')


dw_batch_list.SetTransObject(SQLCA)
dw_report.SetTransObject(SQLCA)

dw_report.object.datawindow.hidegrayline = True

pb_maximize.uf_set_requestor(dw_batch_list)
pb_maximize.uf_set_max_position (0,100,2428,2706)


end event

type dw_report from w_a_report`dw_report within w_batch_type_report
integer x = 0
integer y = 768
integer width = 2706
integer height = 1756
string dataobject = "d_payment_processing_by_batch_type"
boolean hscrollbar = true
borderstyle borderstyle = stylelowered!
end type

type dw_batch_type from u_dw_online within w_batch_type_report
integer width = 2217
integer height = 96
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "d_batch_type_select"
boolean border = false
boolean livescroll = true
end type

event itemchanged;STRING		ls_region_required
LONG			ll_found_row

Choose case dwo.name
	CASE "batch_type_code"		
		
		ll_found_row = idwc_type.Find("batch_type_code = '" + data + "'",1,1000)
		If ll_found_row <= 0 Then SignalError(-666,'Error finding batch type')
		ls_region_required = idwc_type.GetItemString(ll_found_row,'admin_region_required_flag')
		
		IF ls_region_required = "Y" THEN
			dw_batch_type.Modify("admin_region_code.protect = 0")
			dw_batch_type.SetItem(1,'admin_region_code',vgst_user_profile.default_admin_region_code)
		else
			dw_batch_type.Modify("admin_region_code.protect = 1")
			dw_batch_type.SetItem(1,'admin_region_code','ALL')			
		End if
		
End choose		
end event

type cb_retrieve_list from commandbutton within w_batch_type_report
integer x = 2254
integer y = 8
integer width = 407
integer height = 80
integer taborder = 20
boolean bringtotop = true
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Retrieve list"
end type

event clicked;STRING 		ls_batch_type_code
STRING		ls_admin_region_code
LONG			ll_top_rows
LONG			ll_rows

ls_batch_type_code = dw_batch_type.GetItemString(1,'batch_type_code')

If ls_batch_type_code  = '' or IsNull(ls_batch_type_code) Then
	MessageBox('Error','Batch type is required.')
	dw_batch_type.SetFocus()
	dw_batch_type.SetColumn("batch_type_code")
	RETURN
END IF

ls_admin_region_code = dw_batch_type.GetItemString(1,'admin_region_code')
ll_top_rows = dw_batch_type.GetItemNumber(1,'top_rows')

dw_batch_list.Reset()

ll_rows = dw_batch_list.Retrieve(ll_top_rows,ls_batch_type_code,ls_admin_region_code)
IF ll_rows < 0 Then SignalError(-666,'Error retrieving batch list')

IF ll_rows = 0 Then
	MessageBox('No batches','There are no batches that meet the specified criteria.')
	dw_report.ReSet()
	RETURN
END IF



end event

type dw_group_by from u_dw_online within w_batch_type_report
integer x = 251
integer y = 668
integer width = 722
integer height = 88
integer taborder = 11
boolean bringtotop = true
string dataobject = "d_batch_report_grouping_list"
boolean border = false
boolean livescroll = false
end type

event itemchanged;call super::itemchanged;STRING 			ls_return
STRING			ls_expression

If dw_report.RowCount() > 0 Then
	wf_modify_report_grouping(data)
	wf_retrieve_report()
End if
end event

type st_1 from statictext within w_batch_type_report
integer y = 680
integer width = 242
integer height = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Group by:"
boolean focusrectangle = false
end type

type dw_batch_list from u_dw_online within w_batch_type_report
integer y = 100
integer width = 2706
integer height = 532
integer taborder = 11
boolean bringtotop = true
string dataobject = "d_batch_list"
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event constructor;call super::constructor;this.uf_setsort(True)


end event

event rowfocuschanged;call super::rowfocuschanged;
IF this.RowCount() > 0 Then
	this.SelectRow(0,False)
	This.SelectRow(currentrow,True)
	wf_retrieve_report()
END IF
end event

type pb_maximize from cb_dw_maximize within w_batch_type_report
integer x = 2537
integer y = 108
integer taborder = 21
boolean bringtotop = true
end type

