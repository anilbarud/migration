$PBExportHeader$w_unpaid_accounts.srw
forward
global type w_unpaid_accounts from w_a_report
end type
type cb_ok from commandbutton within w_unpaid_accounts
end type
type dw_select_taxing_category from u_dw_online within w_unpaid_accounts
end type
type gb_1 from groupbox within w_unpaid_accounts
end type
end forward

global type w_unpaid_accounts from w_a_report
boolean TitleBar=true
string Title="Unpaid Accounts Report"
cb_ok cb_ok
dw_select_taxing_category dw_select_taxing_category
gb_1 gb_1
end type
global w_unpaid_accounts w_unpaid_accounts

type variables
DataWindowChild		idwc_taxing_categories

end variables

on open;call w_a_report::open;/* Declare Variables                                                                                    
*/
	LONG    ll_result, ll_setid,	ll_nmbr_categories

/* Database Connections                                                                                 
*/
	dw_report.SetTransObject(ImageTrans)


/*	Ensure the user has a default admin region code
*/
	If IsNull(vgst_user_profile.default_admin_region_code) or vgst_user_profile.default_admin_region_code <= "   " Then
		MessageBox("Unpaid Accounts","You must have a default administration region code in the User Profile table " + &
						"~r~nbefore you can access this screen.",StopSign!)
		Close(this)
		Return
	End If

/*	Figure out what the setid is for the users default admin region
*/
	SetNull(ll_setid)

	SELECT Admin_Region_Sets_Xref.set_id  
	INTO :ll_setid  
	FROM Admin_Region_Sets_Xref
	WHERE Admin_Region_Sets_Xref.admin_region_code = :vgst_user_profile.default_admin_region_code using ImageTrans ;

	IF ImageTrans.nf_handle_error("w_unpaid_accounts","Embedded SQL: Retrieve from Admin_Region_Sets_Xref","open event") < 0 THEN
		Close(This)
		Return
	END IF

	If IsNull(ll_setid) Then
		MessageBox("w_unpaid_accounts","Could not find setid in table Admin_Region_Sets_Xref for region '" + &
			vgst_user_profile.default_admin_region_code + "'...Please call the Help Desk",StopSign!)
		Close(this)
		Return
	End If

/*	Obtain a handle to the drop down datawindow and retrieve the list of pc taxing categories for the region
*/
	dw_select_taxing_category.GetChild("catid",idwc_taxing_categories)
	idwc_taxing_categories.SetTransObject(ImageTrans)

	ll_nmbr_categories = idwc_taxing_categories.Retrieve(ll_setid)
	If ImageTrans.nf_handle_error('w_unpaid_accounts','dw_select_taxing_category','open') < 0 then
		Close(this)
		Return
	End If

/*	Set up the external datawindows so the user can enter/select from them
*/
	dw_select_taxing_category.InsertRow(0)

end on

on w_unpaid_accounts.create
int iCurrent
call w_a_report::create
this.cb_ok=create cb_ok
this.dw_select_taxing_category=create dw_select_taxing_category
this.gb_1=create gb_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=cb_ok
this.Control[iCurrent+2]=dw_select_taxing_category
this.Control[iCurrent+3]=gb_1
end on

on w_unpaid_accounts.destroy
call w_a_report::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_ok)
destroy(this.dw_select_taxing_category)
destroy(this.gb_1)
end on

type dw_report from w_a_report`dw_report within w_unpaid_accounts
int Y=429
int Height=2053
int TabOrder=40
string DataObject="d_unpaid_accounts"
boolean HScrollBar=true
end type

type cb_ok from commandbutton within w_unpaid_accounts
int X=2227
int Y=161
int Width=435
int Height=109
int TabOrder=30
string Text="&OK"
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

on clicked;LONG		ll_catid, ll_cntr, ll_rowcount, ll_docid,	ll_nmbr_entries, ll_numrows
INTEGER	li_return_status
STRING	ls_status


/*	Ensure that a valid category has been selected
*/
	ll_catid = dw_select_taxing_category.GetItemNumber(1,"catid")
	IF IsNull(ll_catid) or ll_catid <= 0 THEN
		MessageBox("Unpaid Accounts","You must select a category before you can run the report.",Exclamation!)
		dw_select_taxing_category.SetFocus()
		Return
	END IF

	IF ll_catid = 2 Then
		MessageBox("Unpaid Accounts","You can not select the claimsmaster category for this report.")
		dw_select_taxing_category.SetFocus()
		Return
	END IF

	SetPointer(HourGlass!)

	dw_report.SetFilter("")
	dw_report.Filter()


/*	Retrieve and display the account list
*/
	dw_report.Reset()
	ll_numrows = dw_report.Retrieve(ll_catid)
	ImageTrans.nf_handle_error("w_unpaid_accounts","dw_report","cb_ok")
	IF ll_numrows <= 0 then
		MessageBox("Unpaid Accounts","No data found to satisfy request")
	END IF

/*	For each row in the account list, check to see if there is an entry in the PAYMENT_DOCUMENT table
*/
	ll_cntr = 1
	ll_rowcount = dw_report.RowCount()
	Do While ll_cntr <= ll_rowcount
		ll_docid = dw_report.GetItemNumber(ll_cntr,"document_index_docid")
		IF ll_docid > 0 THEN
			ll_nmbr_entries = 0
			SELECT count(*)
			INTO   :ll_nmbr_entries
			FROM   PAYMENT_DOCUMENT
			WHERE  doc_id = :ll_docid using SQLCA;

			IF SQLCA.nf_handle_error('w_unpaid_accounts','Embedded SQL: select from PAYMENT_DOCUMENT','cb_ok')  < 0 Then
				Close(Parent)
				Return
			END IF

			IF ll_nmbr_entries > 0 THEN
				dw_report.SetItem(ll_cntr,"document_index_docid",0)		// This means that it has entries
			END IF
		End If
		ll_cntr ++
	Loop	

	dw_report.SetFilter("document_index_docid > 0")
	dw_report.Filter()

	IF dw_report.RowCount() = 0 THEN
		MessageBox("Unpaid Accounts","No Data found to Satisfy Request")
	END IF
end on

type dw_select_taxing_category from u_dw_online within w_unpaid_accounts
int X=220
int Y=149
int Width=1093
int Height=89
int TabOrder=10
string DataObject="d_select_taxing_category"
boolean Border=false
end type

type gb_1 from groupbox within w_unpaid_accounts
int X=74
int Y=53
int Width=1537
int Height=269
int TabOrder=20
string Text="Select Category"
BorderStyle BorderStyle=StyleLowered!
long TextColor=33554432
long BackColor=67108864
int TextSize=-10
int Weight=700
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

