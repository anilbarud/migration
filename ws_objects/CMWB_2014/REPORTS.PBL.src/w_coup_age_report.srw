$PBExportHeader$w_coup_age_report.srw
$PBExportComments$Coup aging report main window
forward
global type w_coup_age_report from w_a_report
end type
type dw_select_set from u_dw_online within w_coup_age_report
end type
type gb_2 from groupbox within w_coup_age_report
end type
end forward

global type w_coup_age_report from w_a_report
dw_select_set dw_select_set
gb_2 gb_2
end type
global w_coup_age_report w_coup_age_report

type variables
datawindowchild	idwc_setid
end variables

on open;call w_a_report::open;/* Set up the Drop down data window used to select the work set required.
	This report uses the stored procedure 'Coup_Aging_Report', which resides
	on ImageTrans. 
*/
	dw_select_set.GetChild("set_id",idwc_setid)

	idwc_setid.SetTransObject(ImageTrans)
	dw_report.SetTransObject(ImageTrans)

	idwc_setid.Retrieve()
	ImageTrans.nf_handle_error("w_coup_age_report","dw_select_set","cb_ok")
	dw_select_set.InsertRow(0)
end on

on w_coup_age_report.create
int iCurrent
call w_a_report::create
this.dw_select_set=create dw_select_set
this.gb_2=create gb_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=dw_select_set
this.Control[iCurrent+2]=gb_2
end on

on w_coup_age_report.destroy
call w_a_report::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.dw_select_set)
destroy(this.gb_2)
end on

type dw_report from w_a_report`dw_report within w_coup_age_report
int Y=461
int Height=2021
int TabOrder=30
string DataObject="d_coup_age_report"
boolean HScrollBar=true
end type

type dw_select_set from u_dw_online within w_coup_age_report
event ue_retrieve_report pbm_custom01
int X=97
int Y=181
int Width=1253
int Height=93
int TabOrder=10
string DataObject="d_select_set"
boolean Border=false
end type

on ue_retrieve_report;call u_dw_online::ue_retrieve_report;/* This report uses the stored procedure 'Coup_Agin_Report', which resides
	on the ImageTrans database.
*/
	LONG		al_setid,	ll_currow

	ll_currow = GetRow()

/*	Get the setid and ensure it's valid
*/
	al_setid = GetItemNumber(ll_currow,"set_id")
	IF al_setid <= 0 THEN
		Return
	END IF

/*	Retrieve the report
*/
	dw_report.Retrieve(al_setid)
	ImageTrans.nf_handle_error("w_coup_age_report","dw_report","cb_ok")


end on

on itemchanged;call u_dw_online::itemchanged;Long			vll_currow


//	Check to see if we have a current row (ie, they didn't click on the header or something)

	vll_currow = GetRow()
	If vll_currow <=0 Then
		Return
	End IF

//	Post the event to retrieve the datawindow

	PostEvent("ue_retrieve_report")

end on

type gb_2 from groupbox within w_coup_age_report
int X=42
int Y=29
int Width=1326
int Height=341
int TabOrder=20
string Text="Select Work in Process Set"
BorderStyle BorderStyle=StyleLowered!
long TextColor=33554432
long BackColor=67108864
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

