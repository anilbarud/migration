$PBExportHeader$w_eligibility_export_report.srw
$PBExportComments$Retreives a list of related payments (i.e. same PAYMENT_PRESCRIPTION.prescription_set_no) from the PAYMENT_PRESCRIPTION table
forward
global type w_eligibility_export_report from w_a_report
end type
type dw_find_report_dates from u_dw_online within w_eligibility_export_report
end type
type cb_ok from commandbutton within w_eligibility_export_report
end type
end forward

global type w_eligibility_export_report from w_a_report
integer width = 2820
string title = "Eligibility Export Report"
dw_find_report_dates dw_find_report_dates
cb_ok cb_ok
end type
global w_eligibility_export_report w_eligibility_export_report

type variables
//BOOLEAN		ib_suppress_pbmessage
//STRING			is_report_type_code
//STRING			is_admin_region_code
//DATAWINDOWCHILD	iw_region_list


end variables

forward prototypes
public function integer wf_retrieve_report_dates ()
public function integer wf_user_has_module_authorizations (string as_module)
public subroutine wf_retrieve_report ()
end prototypes

public function integer wf_retrieve_report_dates ();/*	Retrieve the report and highlight the first row.
*/
dw_report.Reset()
dw_find_report_dates.Reset()
dw_find_report_dates.Retrieve()

SQLCA.nf_handle_error("w_eligibility_export_report","wf_retrieve_report_dates()","dw_find_report_dates.Retrieve()")	
			
IF dw_find_report_dates.RowCount() > 0 THEN
	dw_find_report_dates.SelectRow(1,true)
	dw_find_report_dates.ScrollToRow(1)
	RETURN 1
ELSE
	RETURN -1
END IF
end function

public function integer wf_user_has_module_authorizations (string as_module);///* grabs the count of how all regions - user must have
//   an entry for each region for "bca"
//*/
//INTEGER li_count
//
//SELECT count(*) INTO :li_count FROM Admin_Region a WHERE NOT EXISTS
//  ( SELECT * FROM Authorizations b WHERE a.admin_region_code = b.admin_region_code 
//    AND b.authorized_by_login_id = :vgst_user_profile.user_id
//    AND b.authorization_type_code = "bca" )
//AND a.active_flag = "Y"
//USING SQLCA;
//
//SQLCA.nf_handle_error("w_eligibility_export_report","","wf_user_has_module_authorizations - SELECT count(*)")
//
//IF li_count > 0 THEN RETURN -1
//
// SELECT count(*) INTO :li_count
//  FROM Admin_Region a ,Authorizations b 
// WHERE a.admin_region_code       = b.admin_region_code
//   AND a.active_flag             = "Y" 
//   AND b.authorized_by_login_id  = :vgst_user_profile.user_id
//   AND b.authorization_type_code = "bca" 
//   AND (convert(smalldatetime,convert(char(10),getdate(),112),112) < b.effective_from_date  
//    OR convert(smalldatetime,convert(char(10),getdate(),112),112) > b.effective_to_date)
// USING SQLCA;
//
//SQLCA.nf_handle_error("w_eligibility_export_report","","wf_user_has_module_authorizations - SELECT count(*)")
//
//IF li_count > 0 THEN RETURN -1
//
RETURN 1
end function

public subroutine wf_retrieve_report ();DATETIME		ldt_report_datetime
LONG			ll_RowNumber, ll_export_no

/*	Retrieve the report date*/
ll_RowNumber = dw_find_report_dates.GetRow()
IF ll_RowNumber <= 0 THEN RETURN


ll_export_no = dw_find_report_dates.GetItemNumber(ll_RowNumber,"export_no")
ldt_report_datetime = dw_find_report_dates.GetItemDateTime(ll_RowNumber,"export_date")	
/*	Retrieve the detail lines.*/
dw_report.retrieve(ll_export_no, ldt_report_datetime)


SQLCA.nf_handle_error('w_eligibility_export_report','wf_retrieve_report','Retrevial Error ')
	
end subroutine

event open;call super::open;/* Setup the transaction objects for the datawindows */
dw_find_report_dates.SetTransObject(SQLCA)
dw_report.SetTransObject(SQLCA)
//dw_composite_report.SetTransObject(SQLCA)
	
IF wf_retrieve_report_dates() = -1 THEN
	MessageBox("Missing Export Dates","Unable to retrieve list of Export Dates. Contact Systems!",Exclamation!)
	RETURN
END IF


	

	

end event

on w_eligibility_export_report.create
int iCurrent
call super::create
this.dw_find_report_dates=create dw_find_report_dates
this.cb_ok=create cb_ok
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_find_report_dates
this.Control[iCurrent+2]=this.cb_ok
end on

on w_eligibility_export_report.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.dw_find_report_dates)
destroy(this.cb_ok)
end on

type dw_report from w_a_report`dw_report within w_eligibility_export_report
integer x = 18
integer y = 467
integer width = 2706
integer height = 2010
integer taborder = 40
string dataobject = "d_abcc_elig_expt_composite"
boolean hscrollbar = true
boolean livescroll = false
end type

type dw_find_report_dates from u_dw_online within w_eligibility_export_report
integer x = 37
integer y = 32
integer width = 878
integer height = 419
integer taborder = 30
string dataobject = "d_eligibility_export_report_dates"
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event clicked;IF row > 0 THEN
	dw_find_report_dates.SelectRow(0, False)
	dw_find_report_dates.SelectRow(row, True)
END IF
	
end event

type cb_ok from commandbutton within w_eligibility_export_report
event clicked pbm_bnclicked
integer x = 2154
integer y = 211
integer width = 388
integer height = 109
integer taborder = 21
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
end type

event clicked;/*	Variables*/
SetPointer(HourGlass!)

dw_find_report_dates.AcceptText()

wf_retrieve_report()
	


end event

