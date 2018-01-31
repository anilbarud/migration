$PBExportHeader$w_non_imaged_files_logged_out_report.srw
forward
global type w_non_imaged_files_logged_out_report from w_a_report
end type
type cb_ok from commandbutton within w_non_imaged_files_logged_out_report
end type
type dw_admin_regions from u_dw_online within w_non_imaged_files_logged_out_report
end type
end forward

global type w_non_imaged_files_logged_out_report from w_a_report
boolean TitleBar=true
string Title="Non Imaged Files Logged Out"
cb_ok cb_ok
dw_admin_regions dw_admin_regions
end type
global w_non_imaged_files_logged_out_report w_non_imaged_files_logged_out_report

on open;call w_a_report::open;/* Set the transactions for the report window.
*/
	dw_report.SetTransObject(SQLCA)
	dw_admin_regions.SetTransObject(SQLCA)

/*	Return all admin region codes.
*/
	dw_admin_regions.InsertRow(0)

end on

on w_non_imaged_files_logged_out_report.create
int iCurrent
call w_a_report::create
this.cb_ok=create cb_ok
this.dw_admin_regions=create dw_admin_regions
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=cb_ok
this.Control[iCurrent+2]=dw_admin_regions
end on

on w_non_imaged_files_logged_out_report.destroy
call w_a_report::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_ok)
destroy(this.dw_admin_regions)
end on

type dw_report from w_a_report`dw_report within w_non_imaged_files_logged_out_report
int X=51
int Y=469
int Height=2065
int TabOrder=30
string DataObject="d_ni_files_logged_out_by_region"
boolean HScrollBar=true
end type

on dw_report::retrieveend;call w_a_report`dw_report::retrieveend;/*	If now data returned, then display message.
*/
	IF THIS.RowCount() = 0 THEN
		MessageBox("Data Warning","No data was found for this run of the report.")
	END IF

end on

type cb_ok from commandbutton within w_non_imaged_files_logged_out_report
int X=2163
int Y=173
int Width=389
int Height=109
int TabOrder=20
boolean BringToTop=true
string Text="&OK"
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

on clicked;INTEGER				li_rowsindw, li_entryfoundat
STRING				ls_byregion
DATAWINDOWCHILD	ldwc_child

/*	Check to see if want to query by a particular region.
*/
	dw_admin_regions.AcceptText()
	ls_byregion = dw_admin_regions.GetItemString(1,'admin_region_code')

	IF ls_byregion <> '' THEN

/*	Query for a region. But first check to see if the value of ls_byregion is valid.
*/
		dw_admin_regions.GetChild('admin_region_code',ldwc_child)
		li_rowsindw = ldwc_child.RowCount()
		li_entryfoundat = ldwc_child.Find("admin_region_code = '" + ls_byregion + "'",1,li_rowsindw)
		IF li_entryfoundat = 0 THEN
			MessageBox('Invaild Region','The region selected to query by is invalid.')
		ELSE

/*	Set force plan on to improve performance.
*/
			EXECUTE IMMEDIATE 'set forceplan on' USING SQLCA;
			IF SQLCA.nf_handle_error('Setting force plan on', 'w_non_imaged_files_logged_out_report', 'cb_ok.Clicked!') < 0 THEN
				MessageBox("Set force plan failure","Unable to set force plan on, therefore the report could take a while.")
			END IF
			dw_report.SetRedraw(FALSE)
			dw_report.DataObject = 'd_ni_files_logged_out_by_region'
			dw_report.SetTransObject(SQLCA)
			dw_report.SetRedraw(TRUE)
			dw_report.Retrieve(ls_byregion)
			IF SQLCA.nf_handle_error('w_non_imaged_files_logged_out_report','cb_ok','dw_report.Retrieve(ls_byregion)') < 0 THEN
				MessageBox("Report Error","An error occured while trying to return data for report.")
			END IF

/*	Set force plan off.
*/
			EXECUTE IMMEDIATE 'set forceplan off' USING SQLCA;
			IF SQLCA.nf_handle_error('Setting force plan off', 'w_non_imaged_files_logged_out_report', 'cb_ok.Clicked!') < 0 THEN
				MessageBox("Set force plan failure","Unable to set force plan off after the running of the report.")
			END IF
		END IF
	ELSE

/*	Set force plan on to improve performance.
*/
		EXECUTE IMMEDIATE 'set forceplan on' USING SQLCA;
		IF SQLCA.nf_handle_error('Setting force plan on', 'w_non_imaged_files_logged_out_report', 'cb_ok.Clicked!') < 0 THEN
			MessageBox("Set force plan failure","Unable to set force plan on, therefore the report could take a while.")
		END IF
		dw_report.SetRedraw(FALSE)
		dw_report.DataObject = 'd_ni_files_logged_out_all'
		dw_report.SetTransObject(SQLCA)
		dw_report.SetRedraw(TRUE)
		dw_report.Retrieve()
		IF SQLCA.nf_handle_error('w_non_imaged_files_logged_out_report','cb_ok','dw_report.Retrieve()') < 0 THEN
			MessageBox("Report Error","An error occured while trying to return data for report.")
		END IF

/*	Set force plan off.
*/
		EXECUTE IMMEDIATE 'set forceplan off' USING SQLCA;
		IF SQLCA.nf_handle_error('Setting force plan off', 'w_non_imaged_files_logged_out_report', 'cb_ok.Clicked!') < 0 THEN
			MessageBox("Set force plan failure","Unable to set force plan off after the running of the report.")
		END IF
	END IF

end on

type dw_admin_regions from u_dw_online within w_non_imaged_files_logged_out_report
int X=33
int Y=29
int Width=1445
int Height=393
int TabOrder=10
string DataObject="d_admin_regions_for_report"
boolean Border=false
end type

