$PBExportHeader$w_blank_sin_report.srw
$PBExportComments$Report window for validation of T5 data.
forward
global type w_blank_sin_report from w_a_report
end type
type vsb_year from vscrollbar within w_blank_sin_report
end type
type cb_retrieve from commandbutton within w_blank_sin_report
end type
type em_taxation_year from editmask within w_blank_sin_report
end type
type st_transmission_date from statictext within w_blank_sin_report
end type
type cb_print_details from commandbutton within w_blank_sin_report
end type
type cb_close from commandbutton within w_blank_sin_report
end type
end forward

global type w_blank_sin_report from w_a_report
boolean TitleBar=true
string Title="Blank SIN Report"
event ue_open ( )
vsb_year vsb_year
cb_retrieve cb_retrieve
em_taxation_year em_taxation_year
st_transmission_date st_transmission_date
cb_print_details cb_print_details
cb_close cb_close
end type
global w_blank_sin_report w_blank_sin_report

type variables
datetime	idtm_start_date, idtm_end_date


end variables

event ue_open;datetime ldt_server_datetime

SetPointer(HourGlass!)

// Set database information
dw_report.SetTransObject(SQLCA)

// Set the taxation year on the edit mask
ldt_server_datetime = f_server_datetime()
IF Month(Date(ldt_server_datetime)) <= 3 THEN
	em_taxation_year.Text = String(Long(String(ldt_server_datetime, "yyyy"))-1,"####")
ELSE
	em_taxation_year.Text = String(ldt_server_datetime, "yyyy") 
END IF
em_taxation_year.SetFocus()
end event

on w_blank_sin_report.create
int iCurrent
call super::create
this.vsb_year=create vsb_year
this.cb_retrieve=create cb_retrieve
this.em_taxation_year=create em_taxation_year
this.st_transmission_date=create st_transmission_date
this.cb_print_details=create cb_print_details
this.cb_close=create cb_close
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.vsb_year
this.Control[iCurrent+2]=this.cb_retrieve
this.Control[iCurrent+3]=this.em_taxation_year
this.Control[iCurrent+4]=this.st_transmission_date
this.Control[iCurrent+5]=this.cb_print_details
this.Control[iCurrent+6]=this.cb_close
end on

on w_blank_sin_report.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.vsb_year)
destroy(this.cb_retrieve)
destroy(this.em_taxation_year)
destroy(this.st_transmission_date)
destroy(this.cb_print_details)
destroy(this.cb_close)
end on

event open;Long    ll_num_rows, ll_row
Integer li_rtn

SetPointer(HourGlass!)

// Call this event so window opens quicker
PostEvent("ue_open")

end event

type dw_report from w_a_report`dw_report within w_blank_sin_report
int X=0
int Y=164
int Width=2725
int Height=2192
string DataObject="d_blank_sin"
end type

type vsb_year from vscrollbar within w_blank_sin_report
int X=722
int Y=32
int Width=73
int Height=96
boolean Enabled=false
end type

event linedown;datetime ldtm_server_date

ldtm_server_date = f_server_datetime()

IF Long(em_taxation_year.Text) < Year(Date(ldtm_server_date)) THEN
	em_taxation_year.Text = String(Long(em_taxation_year.Text) + 1)
END IF
end event

event lineup;IF Long(em_taxation_year.Text) > 1999 THEN
	em_taxation_year.Text = String(Long(em_taxation_year.Text) - 1)
END IF
end event

type cb_retrieve from commandbutton within w_blank_sin_report
int X=1234
int Y=2412
int Width=434
int Height=108
int TabOrder=50
string Text="&Retrieve "
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

event clicked;Long ll_taxation_year, ll_row, ll_rows, ll_recipient_no
DateTime ldt_server_date, ldt_server_datetime

SetPointer(HourGlass!)

ldt_server_datetime = f_server_datetime()
ldt_server_date = Datetime(Date(ldt_server_datetime), Time("00:00:00"))

// Get Taxation Year
ll_taxation_year = Long(em_taxation_year.Text)
IF ll_taxation_year < 1999 OR ll_taxation_year > Year(Date(ldt_server_date)) THEN
	em_taxation_year.SetFocus()
	MessageBox('Error', 'Taxation year does not pass validation.  It must be >= 1999 and <= the current year.')
	RETURN -1
END IF

if ll_taxation_year > 2000 then
	dw_report.dataobject = 'd_blank_sin_after2000'
	dw_report.settransobject(sqlca)
end if

idtm_start_date = DateTime(Date(String(ll_taxation_year) + '/01/01'))
idtm_end_date = DateTime(Date(String(ll_taxation_year + 1) + '/01/01'))

IF dw_report.Retrieve(idtm_start_date, idtm_end_date) < 0 THEN
	MessageBox('Error', 'Error retrieving trustee information')
	Return -1
END IF

cb_print_details.Enabled = TRUE

end event

type em_taxation_year from editmask within w_blank_sin_report
int X=512
int Y=32
int Width=219
int Height=96
int TabOrder=10
Alignment Alignment=Center!
BorderStyle BorderStyle=StyleLowered!
string Mask="####"
string DisplayData="L("
long BackColor=16777215
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

type st_transmission_date from statictext within w_blank_sin_report
int X=37
int Y=36
int Width=453
int Height=72
boolean Enabled=false
string Text="Taxation Year:"
boolean FocusRectangle=false
long BackColor=67108864
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

type cb_print_details from commandbutton within w_blank_sin_report
int X=1691
int Y=2412
int Width=434
int Height=108
int TabOrder=30
boolean Enabled=false
string Text="&Print Report"
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

event clicked;dw_report.Print()

end event

type cb_close from commandbutton within w_blank_sin_report
int X=2149
int Y=2412
int Width=434
int Height=108
int TabOrder=40
string Text="&Close"
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

event clicked;Close(Parent)
end event

