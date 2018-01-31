$PBExportHeader$w_annuity_sub_ledger_summary.srw
$PBExportComments$Report - Shows annuity sub-ledger  (Summary)
forward
global type w_annuity_sub_ledger_summary from w_a_report
end type
type cb_ok from commandbutton within w_annuity_sub_ledger_summary
end type
type st_1 from statictext within w_annuity_sub_ledger_summary
end type
type st_2 from statictext within w_annuity_sub_ledger_summary
end type
type em_from from editmask within w_annuity_sub_ledger_summary
end type
type em_to from editmask within w_annuity_sub_ledger_summary
end type
type dw_filter_claims from u_dw_online within w_annuity_sub_ledger_summary
end type
type cb_excel from commandbutton within w_annuity_sub_ledger_summary
end type
type gb_1 from groupbox within w_annuity_sub_ledger_summary
end type
end forward

global type w_annuity_sub_ledger_summary from w_a_report
string title = "Annuity Sub-Ledger Report"
cb_ok cb_ok
st_1 st_1
st_2 st_2
em_from em_from
em_to em_to
dw_filter_claims dw_filter_claims
cb_excel cb_excel
gb_1 gb_1
end type
global w_annuity_sub_ledger_summary w_annuity_sub_ledger_summary

type variables
datawindowchild	iw_region_list
end variables

event open;call super::open;INTEGER li_month
STRING  ls_month

/*	Database Connections */
dw_report.SetTransObject(SQLCA)

dw_filter_claims.InsertRow(0)
dw_filter_claims.SetColumn("to_claim")

dw_filter_claims.setitem(1,'from_claim',0)
dw_filter_claims.setitem(1,'to_claim',99999999)

em_from.text = "199501"

li_month = Month(Today())

IF li_month < 10 THEN ls_month = '0' + String(li_month)

em_to.text = String(Year(Today())) + ls_month

dw_report.uf_SetSelect(3)

end event

on w_annuity_sub_ledger_summary.create
int iCurrent
call super::create
this.cb_ok=create cb_ok
this.st_1=create st_1
this.st_2=create st_2
this.em_from=create em_from
this.em_to=create em_to
this.dw_filter_claims=create dw_filter_claims
this.cb_excel=create cb_excel
this.gb_1=create gb_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_ok
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.em_from
this.Control[iCurrent+5]=this.em_to
this.Control[iCurrent+6]=this.dw_filter_claims
this.Control[iCurrent+7]=this.cb_excel
this.Control[iCurrent+8]=this.gb_1
end on

on w_annuity_sub_ledger_summary.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_ok)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.em_from)
destroy(this.em_to)
destroy(this.dw_filter_claims)
destroy(this.cb_excel)
destroy(this.gb_1)
end on

type dw_report from w_a_report`dw_report within w_annuity_sub_ledger_summary
integer x = 32
integer y = 360
integer width = 2647
integer height = 2112
integer taborder = 40
string dataobject = "d_annuity_sub_ledger_summary"
boolean hscrollbar = true
end type

type cb_ok from commandbutton within w_annuity_sub_ledger_summary
integer x = 1522
integer y = 224
integer width = 389
integer height = 108
integer taborder = 50
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
boolean default = true
end type

event clicked;LONG 			ll_to_claim, ll_from_claim  ,ll_numrows
INTEGER   	li_month,li_year
DATE		   ldt_from,ldt_to
STRING      ls_from, ls_to,ls_month

/* Acquire and validate the dates.
*/	
SetPointer(HourGlass!)

dw_filter_claims.AcceptText()

ll_to_claim 	 = dw_filter_claims.GetItemNumber(1,"to_claim")
ll_from_claim   = dw_filter_claims.GetItemNumber(1,"from_claim")
	
/* need to set the from and to dates - the from date 
   will have a '-01' added and the to date will change depending on the month selected
*/
ls_from = em_from.text + "-01"
	
ls_month = right(em_to.text,2)
IF ls_month <> '12' THEN
	li_month = integer(ls_month) + 1
   ls_to = left(em_to.text,5) + string(li_month) + "-01"
ELSE
	li_year = integer(left(em_to.text,4)) + 1
	ls_to = string(li_year) + "-01-01"
END IF 
	
/* do the basic date validations */
IF isdate(ls_from) THEN 
	ldt_from     = date(ls_from)	
ELSE 
	messagebox("Validation Error","The 'from' date must be valid",Exclamation!)
	RETURN
END IF 
	
IF isdate(ls_to) THEN 
	ldt_to  = date(ls_to) 
ELSE 
	messagebox("Validation Error","The 'to' date must be valid",Exclamation!)
	RETURN
END IF 
	
IF ldt_from  > ldt_to  THEN 
	messagebox("Validation Error","The 'from' date cannot be greater than the 'to' Date",Exclamation!)
	RETURN
END IF

IF IsNull(ll_to_claim) or isNull(ll_from_claim  ) THEN
	MessageBox("Validation Error","claim range must have a value!",Exclamation!)
	RETURN
END IF
	
IF ll_to_claim < 0 OR ll_from_claim < 0 THEN
	MessageBox("Validation Error","The 'from' claim or 'to' claim cannot be negative.",Exclamation!)
	RETURN
END IF

IF ll_to_claim < ll_from_claim THEN
	MessageBox("Validation Error","The 'from' claim cannot be after the 'to' claim",Exclamation!)
	RETURN
END IF

/* Retrieve the report */ 
ll_numrows = dw_report.Retrieve(ll_from_claim,ll_to_claim,ldt_from,ldt_to)
SQLCA.nf_handle_error("w_annuity_subledger_summary","cb_ok - clicked","dw_report.Retrieve") 
				
IF ll_numrows <= 0 THEN
	MessageBox("Annuity Sub Ledger","No data found to satisfy request")
END IF

end event

type st_1 from statictext within w_annuity_sub_ledger_summary
integer x = 69
integer y = 216
integer width = 283
integer height = 76
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "From Date: "
boolean focusrectangle = false
end type

type st_2 from statictext within w_annuity_sub_ledger_summary
integer x = 795
integer y = 216
integer width = 219
integer height = 76
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "To Date:"
boolean focusrectangle = false
end type

type em_from from editmask within w_annuity_sub_ledger_summary
integer x = 370
integer y = 208
integer width = 247
integer height = 80
integer taborder = 20
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
string mask = "####-##"
end type

event modified;DOUBLE    ldbl_em

em_from.GetData(ldbl_em)

CHOOSE CASE ldbl_em
	CASE IS > 207905
		em_from.text = '207905'
		MessageBox("Validation Error","The 'from' date cannot be less than 'May 2079'",Exclamation!)
	CASE IS < 199501
		em_from.text = '199501'
		MessageBox("Validation Error","The 'from' date cannot be less than 'January 1995'",Exclamation!)
END CHOOSE
end event

type em_to from editmask within w_annuity_sub_ledger_summary
integer x = 1019
integer y = 208
integer width = 247
integer height = 80
integer taborder = 30
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
string mask = "####-##"
end type

event modified;DOUBLE    ldbl_em

em_to.GetData(ldbl_em)

CHOOSE CASE ldbl_em
	CASE IS > 207905
		em_to.text = '207905'
		MessageBox("Validation Error","The 'to' date cannot be less than 'May 2079'",Exclamation!)
	CASE IS < 199501
		em_to.text = '199501'
		MessageBox("Validation Error","The 'to' date cannot be less than 'January 1995'",Exclamation!)
END CHOOSE
end event

type dw_filter_claims from u_dw_online within w_annuity_sub_ledger_summary
integer x = 46
integer y = 96
integer width = 1330
integer height = 100
integer taborder = 10
string dataobject = "d_enter_claim_range"
boolean border = false
end type

type cb_excel from commandbutton within w_annuity_sub_ledger_summary
integer x = 1984
integer y = 228
integer width = 443
integer height = 104
integer taborder = 20
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Export to Excel"
end type

event clicked;STRING ls_path, ls_return_filename, ls_record_type, ls_datawindow_object, ls_initial_directory
INT li_rtn
long ll_period
string ls_year_start , ls_year_end
U_DS lds_excel

ls_initial_directory = "C:\temp\"

if NOT DirectoryExists (ls_initial_directory) THEN
	ls_initial_directory = "C:\"
END IF

ls_year_start = em_from.text
ls_year_end = em_to.text

ls_path =  "Annuity_SubLedger_GL_Summary_" +ls_year_start + "-" + ls_year_end+ ".XLS"

li_rtn =	GetFileSaveName("Save To", ls_path, ls_return_filename,"XLS","XLS File (*.xls), *.XLS", ls_initial_directory)
	
IF FileExists(ls_return_filename) THEN
	IF MessageBox('Overwrite File?','The file exists. Do you want to overwrite the file?',Question!,YesNo!,2) = 2 THEN
		RETURN
	END IF
END IF

ls_datawindow_object = 'ds_annuity_subledger_gl_summary_excel'
lds_excel = Create U_DS
lds_excel.DataObject = ls_datawindow_object
lds_excel.SetTransObject(SQLCA)
li_rtn = dw_report.ShareData(lds_excel)

li_rtn = lds_excel.SaveAs(ls_return_filename, HTMLTABLE!,true)

if li_rtn <= 0  THEN
	MESSAGEBOX("Error Saving File", "There was an error saving the file to the specified location." &
	+"~r~nPlease check that the file is not already open and that you have accesss to the selected save-to location.", EXCLAMATION!)
END IF
end event

type gb_1 from groupbox within w_annuity_sub_ledger_summary
integer x = 27
integer y = 28
integer width = 1376
integer height = 324
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Select Criteria"
end type

