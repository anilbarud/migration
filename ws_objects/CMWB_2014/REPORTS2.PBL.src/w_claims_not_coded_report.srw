$PBExportHeader$w_claims_not_coded_report.srw
forward
global type w_claims_not_coded_report from window
end type
type cb_close from commandbutton within w_claims_not_coded_report
end type
type cb_print from commandbutton within w_claims_not_coded_report
end type
type cb_retrieve from commandbutton within w_claims_not_coded_report
end type
type st_1 from statictext within w_claims_not_coded_report
end type
type em_year from editmask within w_claims_not_coded_report
end type
type dw_claims_not_coded from u_dw_online within w_claims_not_coded_report
end type
end forward

global type w_claims_not_coded_report from window
integer x = 1893
integer y = 48
integer width = 2766
integer height = 2684
boolean titlebar = true
string title = "Claims Not Coded Report"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
cb_close cb_close
cb_print cb_print
cb_retrieve cb_retrieve
st_1 st_1
em_year em_year
dw_claims_not_coded dw_claims_not_coded
end type
global w_claims_not_coded_report w_claims_not_coded_report

type variables
DATETIME	idtm_start_date, idtm_end_date

end variables

on w_claims_not_coded_report.create
this.cb_close=create cb_close
this.cb_print=create cb_print
this.cb_retrieve=create cb_retrieve
this.st_1=create st_1
this.em_year=create em_year
this.dw_claims_not_coded=create dw_claims_not_coded
this.Control[]={this.cb_close,&
this.cb_print,&
this.cb_retrieve,&
this.st_1,&
this.em_year,&
this.dw_claims_not_coded}
end on

on w_claims_not_coded_report.destroy
destroy(this.cb_close)
destroy(this.cb_print)
destroy(this.cb_retrieve)
destroy(this.st_1)
destroy(this.em_year)
destroy(this.dw_claims_not_coded)
end on

event open;DATETIME ldtm_server_datetime
INTEGER li_rtn

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


dw_claims_not_coded.SetTransObject(SQLCA)

// Set the taxation year in the edit mask
ldtm_server_datetime = f_server_datetime()

IF Month(Date(ldtm_server_datetime)) <= 3 THEN
	em_year.Text = String(Long(String(ldtm_server_datetime, "yyyy"))-1,"####")
ELSE
	em_year.Text = String(ldtm_server_datetime, "yyyy") 
END IF
em_year.SetFocus()


end event

type cb_close from commandbutton within w_claims_not_coded_report
integer x = 2395
integer y = 44
integer width = 270
integer height = 96
integer taborder = 40
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Close"
end type

event clicked;Close(PARENT)

end event

type cb_print from commandbutton within w_claims_not_coded_report
integer x = 2112
integer y = 44
integer width = 270
integer height = 96
integer taborder = 30
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "&Print"
end type

event clicked;/*	Print the report.
*/
IF dw_claims_not_coded.RowCount() > 0 THEN
	dw_claims_not_coded.Print()
ELSE
	MessageBox('No Records','The report has no information to print.',information!)
END IF

end event

type cb_retrieve from commandbutton within w_claims_not_coded_report
integer x = 1829
integer y = 44
integer width = 270
integer height = 96
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Retrieve"
end type

event clicked;LONG     ll_year
DATETIME ldt_server_date, ldt_server_datetime
DATETIME ldtm_start_date, ldtm_end_date

SetPointer(HourGlass!)

cb_print.Enabled = FALSE

ldt_server_datetime = f_server_datetime()
ldt_server_date = Datetime(Date(ldt_server_datetime), Time("00:00:00"))

// Get Year
ll_year = Long(em_year.Text)

IF ll_year = 0 OR IsNull(ll_year) THEN
	MessageBox('Missing Year','Please enter a Year to retrieve.', Information!)
	RETURN -1
END IF

IF ll_year > Year(DATE(ldt_server_date)) THEN
	MessageBox('Incorrect Year', 'Please enter a year that is less than or equal to the current year', Information!)
	RETURN -1
END IF

ldtm_start_date  = DateTime(Date(String(ll_year) + '/01/01'))
ldtm_end_date    = DateTime(Date(String(ll_year + 1) + '/04/01'))


dw_claims_not_coded.SetTransObject(SQLCA)
IF dw_claims_not_coded.Retrieve(ll_year, ldtm_start_date, ldtm_end_date) < 0 THEN
	MessageBox('Error', 'Error retrieving claim information.  ' + SQLCA.SQLErrText)
	RETURN -1
END IF

dw_claims_not_coded.SetTransObject(SQLCA)

IF dw_claims_not_coded.RowCount() > 0 THEN
	cb_print.Enabled = TRUE
ELSE 
	MessageBox('No Rows','There are no transactions found for the entered year ' + STRING(ll_year))
	cb_print.Enabled = FALSE
END IF


end event

type st_1 from statictext within w_claims_not_coded_report
integer x = 69
integer y = 72
integer width = 215
integer height = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = " Year:"
boolean focusrectangle = false
end type

type em_year from editmask within w_claims_not_coded_report
integer x = 288
integer y = 60
integer width = 297
integer height = 80
integer taborder = 10
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "####"
boolean spin = true
string displaydata = "L("
end type

type dw_claims_not_coded from u_dw_online within w_claims_not_coded_report
integer x = 46
integer y = 180
integer width = 2629
integer height = 2216
integer taborder = 10
string title = "Potential NLT Claims for NWISP Extract"
string dataobject = "d_claims_not_coded"
boolean hscrollbar = true
boolean vscrollbar = true
end type

