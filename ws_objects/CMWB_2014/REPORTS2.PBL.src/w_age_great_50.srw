$PBExportHeader$w_age_great_50.srw
$PBExportComments$Window to report on Active Claims With Age > 50.
forward
global type w_age_great_50 from w_a_report
end type
type cb_1 from commandbutton within w_age_great_50
end type
type em_date_begin from editmask within w_age_great_50
end type
type rb_turning from radiobutton within w_age_great_50
end type
type rb_over from radiobutton within w_age_great_50
end type
type em_age from editmask within w_age_great_50
end type
type em_date_end from editmask within w_age_great_50
end type
type st_between from statictext within w_age_great_50
end type
type st_and from statictext within w_age_great_50
end type
type gb_1 from groupbox within w_age_great_50
end type
end forward

global type w_age_great_50 from w_a_report
cb_1 cb_1
em_date_begin em_date_begin
rb_turning rb_turning
rb_over rb_over
em_age em_age
em_date_end em_date_end
st_between st_between
st_and st_and
gb_1 gb_1
end type
global w_age_great_50 w_age_great_50

on w_age_great_50.create
int iCurrent
call super::create
this.cb_1=create cb_1
this.em_date_begin=create em_date_begin
this.rb_turning=create rb_turning
this.rb_over=create rb_over
this.em_age=create em_age
this.em_date_end=create em_date_end
this.st_between=create st_between
this.st_and=create st_and
this.gb_1=create gb_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_1
this.Control[iCurrent+2]=this.em_date_begin
this.Control[iCurrent+3]=this.rb_turning
this.Control[iCurrent+4]=this.rb_over
this.Control[iCurrent+5]=this.em_age
this.Control[iCurrent+6]=this.em_date_end
this.Control[iCurrent+7]=this.st_between
this.Control[iCurrent+8]=this.st_and
this.Control[iCurrent+9]=this.gb_1
end on

on w_age_great_50.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_1)
destroy(this.em_date_begin)
destroy(this.rb_turning)
destroy(this.rb_over)
destroy(this.em_age)
destroy(this.em_date_end)
destroy(this.st_between)
destroy(this.st_and)
destroy(this.gb_1)
end on

event open;call super::open;/*	Database Connections and initialization.
*/
	dw_report.SetTransObject (SQLCA)


end event

type dw_report from w_a_report`dw_report within w_age_great_50
integer x = 27
integer y = 312
integer width = 2656
integer height = 2168
string dataobject = "d_age_great_50"
end type

type cb_1 from commandbutton within w_age_great_50
integer x = 2062
integer y = 100
integer width = 448
integer height = 108
integer taborder = 10
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
end type

event clicked;
/*	Initialization
*/
LONG		ll_numrows
INT       li_age
DATE ldt_date_begin, ldt_date_end, ldt_date

/*	Retrieve the report.
*/

ldt_date_begin = DATE(em_date_begin.text)
ldt_date_end   = DATE(em_date_end.text)
li_age             = INTEGER(em_age.text)

IF li_age < 1 THEN
	messagebox("Wrong Age Format", "Age entered must be greater than 0")
	Return 0
END IF

IF rb_turning.checked THEN
	IF NOT isDate(em_date_begin.text)THEN
		messagebox("Wrong Date Format", "Begin date must be after 1900-01-01 and before 2079-01-01")
		Return 0
	END IF
	
	IF NOT isDate(em_date_end.text)THEN
		messagebox("Wrong Date Format", "End date must be after 1900-01-01 and before 2079-01-01")
		Return 0
	END IF
	
	IF ldt_date_begin < 1900-01-01 or ldt_date_begin > 2079-06-05 THEN
		messagebox("Wrong Date Format", "Begin date must be after 1900-01-01 and before 2079-01-01")
		Return 0
	END IF
	
	IF ldt_date_end < 1900-01-01 or ldt_date_end> 2079-06-05 THEN
		messagebox("Wrong Date Format", "End date must be after 1900-01-01 and before 2079-01-01")
		Return 0
	END IF
	
	IF ldt_date_end < ldt_date_begin THEN
		messagebox("Wrong Date Format", "End date must be on or after begin date")
		Return 0
	END IF
	
	dw_report.object.t_3.text='Turning Age ' + STRING(li_age) + ' Between ' + STRING(ldt_date_begin,'yyyy-mm-dd') + ' and ' + STRING( ldt_date_end,'yyyy-mm-dd')
	// subtract the entered 'age' years from beginning and end dates
	ldt_date_begin = date(year(ldt_date_begin) - li_age, month(ldt_date_begin), day(ldt_date_begin))
	ldt_date_end   = date(year(ldt_date_end) - li_age, month(ldt_date_end), day(ldt_date_end))
ELSE
	dw_report.object.t_3.text='Age ' +  STRING(li_age) + ' or Older'
	ldt_date_end   = date(f_server_datetime())
	// subtract the entered 'age' years from current date
	ldt_date_end   = date(year(ldt_date_end) - li_age, month(ldt_date_end), day(ldt_date_end))
	ldt_date_begin = 1800-01-01
END IF
	
ll_numrows = dw_report.Retrieve(ldt_date_begin,ldt_date_end)
IF SQLCA.nf_handle_error("w_age_great_50","dw_report","OK - Clicked") < 0 Then
	Return -1
END IF

IF ll_numrows = 0 THEN
	MessageBox("Query Results","No data was found for report.")
END IF

dw_report.SetFocus()


end event

type em_date_begin from editmask within w_age_great_50
integer x = 1138
integer y = 124
integer width = 357
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
string displaydata = "~t/"
string minmax = "~~"
end type

event constructor;// display todays date as a default
text = String(DATE(year(today()),month(today()), 1) ,'yyyy-mm-dd')
end event

type rb_turning from radiobutton within w_age_great_50
integer x = 192
integer y = 92
integer width = 393
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
string text = "Turning Age"
boolean checked = true
end type

event clicked;if this.checked then
	st_between.visible = true
	st_and.visible = true
	em_date_begin.visible = true
	em_date_end.visible = true
end if
end event

type rb_over from radiobutton within w_age_great_50
integer x = 192
integer y = 172
integer width = 471
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
string text = "0ver the Age of"
end type

event clicked;if this.checked then
	st_between.visible = false
	st_and.visible = false
	em_date_begin.visible = false
	em_date_end.visible = false
end if
end event

type em_age from editmask within w_age_great_50
integer x = 699
integer y = 124
integer width = 151
integer height = 88
integer taborder = 20
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string text = "50"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "##"
string displaydata = "~t/"
string minmax = "~~"
end type

type em_date_end from editmask within w_age_great_50
integer x = 1641
integer y = 124
integer width = 357
integer height = 84
integer taborder = 20
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
string displaydata = "~t/"
string minmax = "~~"
end type

event constructor;//default the display date, at the end of the month

date ld_next_month, ld_today
integer li_year, li_month

ld_today =  DATE(f_server_datetime())
li_year = year(ld_today)
li_month = month(ld_today)

if li_month = 12  then 
	ld_next_month = DATE(li_year +1, 1 , 1)
else
	ld_next_month = DATE(li_year, li_month + 1 , 1)
End If

//populate the date with next month minus one day
this.text = STRING(RelativeDate(ld_next_month, -1),'yyyy-mm-dd')

end event

type st_between from statictext within w_age_great_50
integer x = 878
integer y = 136
integer width = 238
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
string text = "between"
boolean focusrectangle = false
end type

type st_and from statictext within w_age_great_50
integer x = 1522
integer y = 136
integer width = 119
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
string text = "and"
boolean focusrectangle = false
end type

type gb_1 from groupbox within w_age_great_50
integer x = 27
integer y = 28
integer width = 658
integer height = 244
integer taborder = 10
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Claimant is"
end type

