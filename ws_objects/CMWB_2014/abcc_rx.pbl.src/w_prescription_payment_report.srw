$PBExportHeader$w_prescription_payment_report.srw
$PBExportComments$Retreives a list of related payments (i.e. same PAYMENT_PRESCRIPTION.prescription_set_no) from the PAYMENT_PRESCRIPTION table
forward
global type w_prescription_payment_report from w_a_report
end type
type st_1 from statictext within w_prescription_payment_report
end type
type em_date from editmask within w_prescription_payment_report
end type
type cb_retrieve from commandbutton within w_prescription_payment_report
end type
type cb_zoom from commandbutton within w_prescription_payment_report
end type
end forward

global type w_prescription_payment_report from w_a_report
st_1 st_1
em_date em_date
cb_retrieve cb_retrieve
cb_zoom cb_zoom
end type
global w_prescription_payment_report w_prescription_payment_report

forward prototypes
public function integer wf_user_has_module_authorizations (string as_module)
end prototypes

public function integer wf_user_has_module_authorizations (string as_module);/* grabs the count of how all regions - user must have
   an entry for each region for "bca"
*/
INTEGER li_count

SELECT count(*)
INTO :li_count
FROM Admin_Region a
WHERE NOT EXISTS
  ( SELECT * 
  FROM Authorizations b 
  WHERE a.admin_region_code = b.admin_region_code 
    AND b.authorized_by_login_id = :vgst_user_profile.user_id
    AND b.authorization_type_code = "bca" )
AND a.active_flag = "Y"
USING SQLCA;

SQLCA.nf_handle_error("w_prescription_payment_report","wf_user_has_module_authorizations()","SELECT count(*)")

IF li_count > 0 THEN RETURN -1

 SELECT count(*) INTO :li_count
  FROM Admin_Region a ,Authorizations b 
 WHERE a.admin_region_code       = b.admin_region_code
   AND a.active_flag             = "Y" 
   AND b.authorized_by_login_id  = :vgst_user_profile.user_id
   AND b.authorization_type_code = "bca" 
   AND (convert(smalldatetime,convert(char(10),getdate(),112),112) < b.effective_from_date  
    OR convert(smalldatetime,convert(char(10),getdate(),112),112) > b.effective_to_date)
 USING SQLCA;

SQLCA.nf_handle_error("w_prescription_payment_report","wf_user_has_module_authorizations()","SELECT count(*)")

IF li_count > 0 THEN RETURN -1

RETURN 1
end function

on w_prescription_payment_report.create
int iCurrent
call super::create
this.st_1=create st_1
this.em_date=create em_date
this.cb_retrieve=create cb_retrieve
this.cb_zoom=create cb_zoom
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.em_date
this.Control[iCurrent+3]=this.cb_retrieve
this.Control[iCurrent+4]=this.cb_zoom
end on

on w_prescription_payment_report.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.st_1)
destroy(this.em_date)
destroy(this.cb_retrieve)
destroy(this.cb_zoom)
end on

type dw_report from w_a_report`dw_report within w_prescription_payment_report
integer x = 27
integer y = 172
integer width = 2670
integer height = 2368
string dataobject = "d_prescription_payment_report"
boolean hscrollbar = true
end type

type st_1 from statictext within w_prescription_payment_report
integer x = 27
integer y = 60
integer width = 663
integer height = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Oldest Prescription Date:"
boolean focusrectangle = false
end type

type em_date from editmask within w_prescription_payment_report
integer x = 690
integer y = 44
integer width = 334
integer height = 84
integer taborder = 10
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string text = "none"
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "yyyy-mm-dd"
end type

type cb_retrieve from commandbutton within w_prescription_payment_report
integer x = 2254
integer y = 32
integer width = 402
integer height = 108
integer taborder = 20
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Retrieve"
boolean default = true
end type

event clicked;LONG			ll_numrows, ll_return
STRING		ls_sql
DATE			ldt_date, ldt_today
DATETIME	ldtm_date

dw_report.Reset()

/* Verify that the entered date is reasonable */
ldt_date = Date(em_date.Text)
ldt_today = Today()

IF DaysAfter(ldt_date, ldt_today) < 0 THEN
	MessageBox('Date Problem','The date cannot be in the future')
	RETURN
ELSEIF DaysAfter(ldt_date, 2004-10-01) > 0 THEN
	ldt_date = 1900-01-01
END IF

ldtm_date = DateTime(ldt_date)

/*	Database Connections and initialization.*/
dw_report.SetTransObject (SQLCA)

/*	Retrieve the report.*/
ll_numrows = dw_report.Retrieve(ldtm_date)
SQLCA.nf_handle_error("w_prescription_payment_report","Open - event","dw_report.Retrieve()") 
	
IF ll_numrows = 0 THEN MessageBox("Query Results","No data was found for report.")



end event

type cb_zoom from commandbutton within w_prescription_payment_report
integer x = 1230
integer y = 32
integer width = 402
integer height = 108
integer taborder = 20
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Fit to Window"
end type

event clicked;IF THIS.text = 'Fit to Window' THEN
	dw_report.Object.DataWindow.Zoom = 75
	THIS.text = 'Full Size'
ELSE
	dw_report.Object.DataWindow.Zoom = 100
	THIS.text = 'Fit to Window'
END IF
end event

