$PBExportHeader$w_open_awards_unproc_pymts.srw
forward
global type w_open_awards_unproc_pymts from w_a_report
end type
type cb_ok from commandbutton within w_open_awards_unproc_pymts
end type
type em_prov_no from editmask within w_open_awards_unproc_pymts
end type
type st_rcpt_no from statictext within w_open_awards_unproc_pymts
end type
type cb_search from commandbutton within w_open_awards_unproc_pymts
end type
type cb_clear from commandbutton within w_open_awards_unproc_pymts
end type
type dw_provider_type from datawindow within w_open_awards_unproc_pymts
end type
type st_name from statictext within w_open_awards_unproc_pymts
end type
end forward

global type w_open_awards_unproc_pymts from w_a_report
integer y = 49
integer width = 2802
integer height = 2940
boolean minbox = false
cb_ok cb_ok
em_prov_no em_prov_no
st_rcpt_no st_rcpt_no
cb_search cb_search
cb_clear cb_clear
dw_provider_type dw_provider_type
st_name st_name
end type
global w_open_awards_unproc_pymts w_open_awards_unproc_pymts

type variables
String		is_prov_type
end variables

on w_open_awards_unproc_pymts.create
int iCurrent
call super::create
this.cb_ok=create cb_ok
this.em_prov_no=create em_prov_no
this.st_rcpt_no=create st_rcpt_no
this.cb_search=create cb_search
this.cb_clear=create cb_clear
this.dw_provider_type=create dw_provider_type
this.st_name=create st_name
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_ok
this.Control[iCurrent+2]=this.em_prov_no
this.Control[iCurrent+3]=this.st_rcpt_no
this.Control[iCurrent+4]=this.cb_search
this.Control[iCurrent+5]=this.cb_clear
this.Control[iCurrent+6]=this.dw_provider_type
this.Control[iCurrent+7]=this.st_name
end on

on w_open_awards_unproc_pymts.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.cb_ok)
destroy(this.em_prov_no)
destroy(this.st_rcpt_no)
destroy(this.cb_search)
destroy(this.cb_clear)
destroy(this.dw_provider_type)
destroy(this.st_name)
end on

event open;call super::open;DATAWINDOWCHILD			ldwc_child
INTEGER 						li_row

li_row = dw_report.SetTransObject (SQLCA)
li_row = dw_provider_type.SetTransObject(SQLCA)
li_row = dw_provider_type.InsertRow(0)
li_row = dw_provider_type.SetRow(1)
li_row = dw_provider_type.SetColumn("provider_type_code")




end event

type dw_report from w_a_report`dw_report within w_open_awards_unproc_pymts
integer x = 37
integer y = 320
integer width = 2670
integer height = 2240
integer taborder = 0
string dataobject = "d_open_awards_unproc_pymts"
boolean hscrollbar = true
boolean resizable = true
borderstyle borderstyle = stylelowered!
end type

type cb_ok from commandbutton within w_open_awards_unproc_pymts
integer x = 2231
integer y = 160
integer width = 402
integer height = 104
integer taborder = 50
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "OK"
boolean default = true
end type

event clicked;STRING		ls_prov_type
LONG			ll_prov_no, ll_no_rows

dw_provider_type.AcceptText()
ll_prov_no = Long(em_prov_no.Text)
ls_prov_type = dw_provider_type.GetItemString(1,'provider_type_code')
	
ll_no_rows = dw_report.Retrieve(ll_prov_no, ls_prov_type)
SQLCA.nf_handle_error("w_open_awards_unproc_pymts","dw_report","cb_ok")
IF ll_no_rows = 0 and ll_prov_no <= 0 THEN
	MessageBox("Scheduled Payments to Providers/Recipients Report - Invalid Provider Number","The provider number must be greater than 0.",Information!)
ELSEIF ll_no_rows = 0 and ll_prov_no > 0 THEN
	MessageBox("Scheduled Payments to Providers/Recipients Report","No payments were found for this provider number and type.",Information!)
ELSEIF ls_prov_type = '' OR ISNull(ls_prov_type) THEN
	MessageBox("Scheduled Payments to Providers/Recipients Report - Invalid Provider Type","A provider type must be selected from the dropdown list.",Information!)
ELSEIF ll_no_rows < 0 THEN
	MessageBox("Scheduled Payments to Providers/Recipients Report","A problem occurred while trying to retrieve the report.~r~nPlease contact the HelpDesk.",StopSign!)
	RETURN
END IF



	
	
end event

type em_prov_no from editmask within w_open_awards_unproc_pymts
integer x = 549
integer y = 176
integer width = 325
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
string text = "none"
borderstyle borderstyle = stylelowered!
string mask = "#######"
string minmax = "1~~"
end type

event modified;STRING	ls_prov_no, ls_prov_type, ls_prov_name
LONG		ll_result


ls_prov_no = em_prov_no.Text

IF ls_prov_no = '' OR IsNull(ls_prov_no) THEN
	MessageBox("Invalid Provider Number","The provider number must be entered, and it must be greater than 0.",Information!)
	Return
END IF


ls_prov_type = is_prov_type

SELECT	name
    INTO	:ls_prov_name
  FROM	PROVIDER
WHERE	provider_no = :ls_prov_no
     AND	provider_type_code = :ls_prov_type
  USING SQLCA;
  
ll_result = SQLCA.nf_handle_error("Embedded SQL: Select from PROVIDER","w_open_awards_unproc_pymts","em_prov_no modified event")
IF ll_result < 0 THEN
	RETURN -1
END IF

st_name.Text = ls_prov_name

end event

type st_rcpt_no from statictext within w_open_awards_unproc_pymts
integer x = 105
integer y = 192
integer width = 439
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Provider No.:"
boolean focusrectangle = false
end type

type cb_search from commandbutton within w_open_awards_unproc_pymts
integer x = 951
integer y = 160
integer width = 96
integer height = 96
integer taborder = 30
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "?"
end type

event clicked;STRING		ls_prov_type, ls_provno
INT			li_return
LONG			ll_prov_no
S_WINDOW_MESSAGE	lstr_message

ls_prov_type = dw_provider_type.GetItemString(1,'provider_type_code')

lstr_message.al_doubleparm[1] = ll_prov_no
lstr_message.as_stringparm[1] = ls_prov_type

IF ls_prov_type = 'V' OR ls_prov_type =  'M' OR ls_prov_type =  'O' THEN
	OpenWithParm(w_service_provider_search, ls_prov_type)
	lstr_message = Message.PowerObjectParm
ELSE
	MessageBox('Warning', 'No search available for this recipient type.')
END IF

//lstr_message = Message.PowerObjectParm
ll_prov_no	 =	lstr_message.al_doubleparm[1]
ls_prov_type =	lstr_message.as_stringparm[1]

dw_provider_type.SetText(ls_prov_type)
dw_provider_type.SetItem (1,'provider_type_code', ll_prov_no)
em_prov_no.Text = String(ll_prov_no)

cb_ok.Default = TRUE
dw_report.Reset()
//trigger em_prov_no modified event to populate provider name in st_name
em_prov_no.TriggerEvent(Modified!)

end event

type cb_clear from commandbutton within w_open_awards_unproc_pymts
integer x = 2231
integer y = 32
integer width = 402
integer height = 104
integer taborder = 40
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Clear"
end type

event clicked;
em_prov_no.Text = ''
st_name.Text = ''
dw_provider_type.Reset()
dw_provider_type.InsertRow(0)
dw_report.Reset()


end event

type dw_provider_type from datawindow within w_open_awards_unproc_pymts
integer x = 73
integer y = 32
integer width = 1280
integer height = 128
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "d_provider_type"
boolean border = false
boolean livescroll = true
end type

event itemchanged;STRING		ls_type

THIS.AcceptText()
ls_type = THIS.GetItemString(1,'provider_type_code')
	
IF ls_type = '' OR IsNull(ls_type)  THEN
	MessageBox("Scheduled Payments to Providers/Recipients Report - Invalid Provider Type","A provider type must be selected.",Information!)
	RETURN
END IF

is_prov_type = ls_type

end event

type st_name from statictext within w_open_awards_unproc_pymts
integer x = 1097
integer y = 192
integer width = 1061
integer height = 64
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
end type

