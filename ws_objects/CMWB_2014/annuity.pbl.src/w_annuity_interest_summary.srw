$PBExportHeader$w_annuity_interest_summary.srw
$PBExportComments$Report - Shows a summary of the Interest posted for a year/quarter
forward
global type w_annuity_interest_summary from w_a_report
end type
type cb_ok from commandbutton within w_annuity_interest_summary
end type
type gb_1 from groupbox within w_annuity_interest_summary
end type
type em_date from editmask within w_annuity_interest_summary
end type
type em_quarter from editmask within w_annuity_interest_summary
end type
type st_1 from statictext within w_annuity_interest_summary
end type
type st_2 from statictext within w_annuity_interest_summary
end type
type cb_export from commandbutton within w_annuity_interest_summary
end type
end forward

global type w_annuity_interest_summary from w_a_report
integer width = 3237
string title = "Annuity Applied Interest Summary Report"
cb_ok cb_ok
gb_1 gb_1
em_date em_date
em_quarter em_quarter
st_1 st_1
st_2 st_2
cb_export cb_export
end type
global w_annuity_interest_summary w_annuity_interest_summary

type variables
datawindowchild	iw_region_list
end variables

event open;call super::open;/*	Database Connections */
dw_report.SetTransObject(SQLCA)

dw_report.uf_SetSelect(3)
end event

on w_annuity_interest_summary.create
int iCurrent
call super::create
this.cb_ok=create cb_ok
this.gb_1=create gb_1
this.em_date=create em_date
this.em_quarter=create em_quarter
this.st_1=create st_1
this.st_2=create st_2
this.cb_export=create cb_export
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_ok
this.Control[iCurrent+2]=this.gb_1
this.Control[iCurrent+3]=this.em_date
this.Control[iCurrent+4]=this.em_quarter
this.Control[iCurrent+5]=this.st_1
this.Control[iCurrent+6]=this.st_2
this.Control[iCurrent+7]=this.cb_export
end on

on w_annuity_interest_summary.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_ok)
destroy(this.gb_1)
destroy(this.em_date)
destroy(this.em_quarter)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.cb_export)
end on

type dw_report from w_a_report`dw_report within w_annuity_interest_summary
integer x = 27
integer y = 456
integer width = 3095
integer height = 2052
integer taborder = 40
string dataobject = "d_annuity_applied_interest_summary"
boolean hscrollbar = true
end type

type cb_ok from commandbutton within w_annuity_interest_summary
integer x = 1339
integer y = 240
integer width = 315
integer height = 108
integer taborder = 30
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
end type

event clicked;LONG ll_numrows
integer li_quarter_no,li_year
decimal  ldcm_interest_rate  
datetime ldtm_interest_applied_date  
date ldt_quarter_start_date,	ldt_quarter_end_date 

SetPointer(HourGlass!)

dw_report.reset()

/* setup the date range for the quarter being processed */

li_quarter_no = integer(em_quarter.text)
li_year = integer(em_date.text)

CHOOSE CASE li_quarter_no
	CASE 1
		ldt_quarter_start_date = date(li_year,1,1)
		ldt_quarter_end_date = date(li_year,3,31)
	CASE 2
		ldt_quarter_start_date = date(li_year,4,1)
		ldt_quarter_end_date = date(li_year,6,30)
	CASE 3
		ldt_quarter_start_date = date(li_year,7,1)
		ldt_quarter_end_date = date(li_year,9,30)
	CASE 4
		ldt_quarter_start_date = date(li_year,10,1)
		ldt_quarter_end_date = date(li_year,12,31)
END CHOOSE


SELECT annuity_interest_rate, interest_applied_date  
  INTO :ldcm_interest_rate,   :ldtm_interest_applied_date  
  FROM Annuity_Interest_Rate  
 WHERE ( year       = :li_year ) 
   AND ( quarter_no = :li_quarter_no ) 
   AND  (active_flag = 'Y') using SQLCA ;

SQLCA.nf_handle_error("Embedded SQL","Annuity_Interest"," ") 

//The li_year,li_quarter_no,ldcm_interest_rate values are passed into the datawindow and are displayed in the report header, not in the datawindow select.		
if isnull(ldtm_interest_applied_date) OR Date(ldtm_interest_applied_date) =  Date("1900-01-01") then
	MessageBox("Annuity Sub Ledger - Applied Interest Report","Interest has not been applied for year " + string(li_year) + "  and quarter " + string(li_quarter_no))
else /* Retrieve the report */ 
	ll_numrows = dw_report.RETRIEVE(datetime(ldt_quarter_start_date),datetime(ldt_quarter_end_date),li_year,li_quarter_no,ldcm_interest_rate)
		
	IF ll_numrows <= 0 THEN
		MessageBox("Annuity Sub Ledger - Applied Interest Report","No data found to satisfy request")
	END IF
end if









end event

type gb_1 from groupbox within w_annuity_interest_summary
integer x = 37
integer y = 56
integer width = 1042
integer height = 384
integer taborder = 10
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Select Criteria"
end type

type em_date from editmask within w_annuity_interest_summary
integer x = 425
integer y = 160
integer width = 370
integer height = 76
integer taborder = 20
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
string text = "1993"
textcase textcase = upper!
string mask = "####"
boolean spin = true
string displaydata = ""
double increment = 1
string minmax = "1993~~2040"
end type

type em_quarter from editmask within w_annuity_interest_summary
integer x = 430
integer y = 280
integer width = 187
integer height = 76
integer taborder = 21
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
string text = "1"
textcase textcase = upper!
string mask = "#"
boolean spin = true
string displaydata = "1~t1/2~t2/3~t3/4~t4/"
double increment = 1
string minmax = "1~~4"
boolean usecodetable = true
end type

type st_1 from statictext within w_annuity_interest_summary
integer x = 96
integer y = 156
integer width = 247
integer height = 72
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = " Year:"
boolean focusrectangle = false
end type

type st_2 from statictext within w_annuity_interest_summary
integer x = 96
integer y = 280
integer width = 256
integer height = 72
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "  Quarter:"
boolean focusrectangle = false
end type

type cb_export from commandbutton within w_annuity_interest_summary
integer x = 1806
integer y = 244
integer width = 443
integer height = 104
integer taborder = 40
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
string ls_year_quarter
U_DS lds_excel

ls_initial_directory = "C:\temp\"

if NOT DirectoryExists (ls_initial_directory) THEN
	ls_initial_directory = "C:\"
END IF

ls_year_quarter = em_date.text
ls_year_quarter = ls_year_quarter + '-Q' + em_quarter.text

ls_path =  "Applied_Annuity_Interst_Summary_" + ls_year_quarter + ".XLS"

li_rtn =	GetFileSaveName("Save To", ls_path, ls_return_filename,"XLS","XLS File (*.xls), *.XLS", ls_initial_directory)
	
IF FileExists(ls_return_filename) THEN
	IF MessageBox('Overwrite File?','The file exists. Do you want to overwrite the file?',Question!,YesNo!,2) = 2 THEN
		RETURN
	END IF
END IF

ls_datawindow_object = 'ds_annuity_applied_interest_summary_excel'
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

