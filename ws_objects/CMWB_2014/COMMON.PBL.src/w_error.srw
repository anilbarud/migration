$PBExportHeader$w_error.srw
$PBExportComments$Common abort screen with mail and print functionality
forward
global type w_error from window
end type
type sle_return_data from singlelineedit within w_error
end type
type mle_error_text from multilineedit within w_error
end type
type st_2 from statictext within w_error
end type
type mle_comment from multilineedit within w_error
end type
type st_5 from statictext within w_error
end type
type sle_application from singlelineedit within w_error
end type
type sle_error_datetime from singlelineedit within w_error
end type
type st_3 from statictext within w_error
end type
type st_1 from statictext within w_error
end type
type sle_error_line from singlelineedit within w_error
end type
type sle_error_object_event from singlelineedit within w_error
end type
type sle_error_object from singlelineedit within w_error
end type
type sle_error_window_menu from singlelineedit within w_error
end type
type sle_error_number from singlelineedit within w_error
end type
type st_error_line from statictext within w_error
end type
type st_error_object_event from statictext within w_error
end type
type st_error_object from statictext within w_error
end type
type st_error_window_menu from statictext within w_error
end type
type st_error_text from statictext within w_error
end type
type st_error_number from statictext within w_error
end type
type sle_sql_db_code from singlelineedit within w_error
end type
type st_sql_db_code from statictext within w_error
end type
type cb_ok from commandbutton within w_error
end type
type st_database from statictext within w_error
end type
type sle_database from singlelineedit within w_error
end type
type st_return_data from statictext within w_error
end type
type gb_1 from groupbox within w_error
end type
type gb_general from groupbox within w_error
end type
type gb_error from groupbox within w_error
end type
end forward

global type w_error from window
integer width = 3232
integer height = 1996
boolean titlebar = true
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 67108864
sle_return_data sle_return_data
mle_error_text mle_error_text
st_2 st_2
mle_comment mle_comment
st_5 st_5
sle_application sle_application
sle_error_datetime sle_error_datetime
st_3 st_3
st_1 st_1
sle_error_line sle_error_line
sle_error_object_event sle_error_object_event
sle_error_object sle_error_object
sle_error_window_menu sle_error_window_menu
sle_error_number sle_error_number
st_error_line st_error_line
st_error_object_event st_error_object_event
st_error_object st_error_object
st_error_window_menu st_error_window_menu
st_error_text st_error_text
st_error_number st_error_number
sle_sql_db_code sle_sql_db_code
st_sql_db_code st_sql_db_code
cb_ok cb_ok
st_database st_database
sle_database sle_database
st_return_data st_return_data
gb_1 gb_1
gb_general gb_general
gb_error gb_error
end type
global w_error w_error

event open;//	Load the date, time and application name

	sle_error_datetime.text=string(today(),"mmmm dd, yyyy")+"   -   "+string(now(),"H:MM:SS am/pm")
	If IsValid(G_PFSecurity) Then
		sle_application.text =g_pfsecurity.i_application_name
	End If

// If a database error, fill in database error values. Also,
//	Change the text of the error number and error message fields to be more meaningful.
// Otherwise, hide the database error specific code

	If Error.is_Type = 'D' THEN
	   If Error.il_DBCode <> 0 THEN
		   sle_sql_db_code.text=  string(Error.il_DBCode)
	   End If
	   sle_return_data.text = Error.is_ReturnData
		st_error_number.Text = "SQL Code:"
		st_error_text.Text = "SQL Error Text:"
	   gb_error.text = 'Database Error'
	Else
	   gb_error.text = 'System Error'
	   st_return_data.visible = FALSE
	   st_sql_db_code.visible = FALSE
	End If

// Fill in general info

	mle_error_text.text = Error.Text
	IF Error.Number <> 0 THEN
		sle_error_number.text=string(Error.Number)
	END IF
	sle_database.text           = Error.is_database
	sle_error_object.text       = Error.object
	sle_error_window_menu.text  = Error.windowmenu
	sle_error_object_event.text = Error.objectevent
	IF Error.Line<>0 THEN
		sle_error_line.text=string(Error.Line)
	END IF
	
	cb_ok.enabled = TRUE
	mle_comment.SetFocus()
end event

on w_error.create
this.sle_return_data=create sle_return_data
this.mle_error_text=create mle_error_text
this.st_2=create st_2
this.mle_comment=create mle_comment
this.st_5=create st_5
this.sle_application=create sle_application
this.sle_error_datetime=create sle_error_datetime
this.st_3=create st_3
this.st_1=create st_1
this.sle_error_line=create sle_error_line
this.sle_error_object_event=create sle_error_object_event
this.sle_error_object=create sle_error_object
this.sle_error_window_menu=create sle_error_window_menu
this.sle_error_number=create sle_error_number
this.st_error_line=create st_error_line
this.st_error_object_event=create st_error_object_event
this.st_error_object=create st_error_object
this.st_error_window_menu=create st_error_window_menu
this.st_error_text=create st_error_text
this.st_error_number=create st_error_number
this.sle_sql_db_code=create sle_sql_db_code
this.st_sql_db_code=create st_sql_db_code
this.cb_ok=create cb_ok
this.st_database=create st_database
this.sle_database=create sle_database
this.st_return_data=create st_return_data
this.gb_1=create gb_1
this.gb_general=create gb_general
this.gb_error=create gb_error
this.Control[]={this.sle_return_data,&
this.mle_error_text,&
this.st_2,&
this.mle_comment,&
this.st_5,&
this.sle_application,&
this.sle_error_datetime,&
this.st_3,&
this.st_1,&
this.sle_error_line,&
this.sle_error_object_event,&
this.sle_error_object,&
this.sle_error_window_menu,&
this.sle_error_number,&
this.st_error_line,&
this.st_error_object_event,&
this.st_error_object,&
this.st_error_window_menu,&
this.st_error_text,&
this.st_error_number,&
this.sle_sql_db_code,&
this.st_sql_db_code,&
this.cb_ok,&
this.st_database,&
this.sle_database,&
this.st_return_data,&
this.gb_1,&
this.gb_general,&
this.gb_error}
end on

on w_error.destroy
destroy(this.sle_return_data)
destroy(this.mle_error_text)
destroy(this.st_2)
destroy(this.mle_comment)
destroy(this.st_5)
destroy(this.sle_application)
destroy(this.sle_error_datetime)
destroy(this.st_3)
destroy(this.st_1)
destroy(this.sle_error_line)
destroy(this.sle_error_object_event)
destroy(this.sle_error_object)
destroy(this.sle_error_window_menu)
destroy(this.sle_error_number)
destroy(this.st_error_line)
destroy(this.st_error_object_event)
destroy(this.st_error_object)
destroy(this.st_error_window_menu)
destroy(this.st_error_text)
destroy(this.st_error_number)
destroy(this.sle_sql_db_code)
destroy(this.st_sql_db_code)
destroy(this.cb_ok)
destroy(this.st_database)
destroy(this.sle_database)
destroy(this.st_return_data)
destroy(this.gb_1)
destroy(this.gb_general)
destroy(this.gb_error)
end on

type sle_return_data from singlelineedit within w_error
integer x = 773
integer y = 1204
integer width = 2217
integer height = 76
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean border = false
boolean autohscroll = false
boolean displayonly = true
end type

type mle_error_text from multilineedit within w_error
integer x = 777
integer y = 968
integer width = 2217
integer height = 152
integer taborder = 10
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean border = false
boolean vscrollbar = true
boolean displayonly = true
end type

type st_2 from statictext within w_error
integer x = 206
integer y = 1468
integer width = 1765
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Briefly describe what you were doing at the time the error occurred:"
boolean focusrectangle = false
end type

type mle_comment from multilineedit within w_error
integer x = 786
integer y = 1548
integer width = 2217
integer height = 168
integer taborder = 30
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
boolean vscrollbar = true
boolean autovscroll = true
borderstyle borderstyle = stylelowered!
end type

type st_5 from statictext within w_error
integer x = 206
integer y = 308
integer width = 480
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Application Name:"
boolean focusrectangle = false
end type

type sle_application from singlelineedit within w_error
integer x = 777
integer y = 312
integer width = 2217
integer height = 76
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean border = false
boolean autohscroll = false
boolean displayonly = true
end type

type sle_error_datetime from singlelineedit within w_error
integer x = 96
integer y = 1784
integer width = 1074
integer height = 84
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean border = false
boolean autohscroll = false
boolean displayonly = true
end type

type st_3 from statictext within w_error
integer x = 782
integer width = 1655
integer height = 148
integer textsize = -24
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 255
long backcolor = 67108864
boolean enabled = false
string text = "APPLICATION ERROR"
alignment alignment = center!
boolean focusrectangle = false
end type

type st_1 from statictext within w_error
integer x = 165
integer y = 140
integer width = 2885
integer height = 68
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "An unrecoverable application error has occured.  You Must Exit from Windows and Reboot Your PC"
alignment alignment = center!
boolean focusrectangle = false
end type

type sle_error_line from singlelineedit within w_error
integer x = 777
integer y = 692
integer width = 2217
integer height = 76
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean border = false
boolean autohscroll = false
boolean displayonly = true
end type

type sle_error_object_event from singlelineedit within w_error
integer x = 777
integer y = 616
integer width = 2217
integer height = 76
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean border = false
boolean autohscroll = false
boolean displayonly = true
end type

type sle_error_object from singlelineedit within w_error
integer x = 777
integer y = 540
integer width = 2217
integer height = 76
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean border = false
boolean autohscroll = false
boolean displayonly = true
end type

type sle_error_window_menu from singlelineedit within w_error
integer x = 777
integer y = 464
integer width = 2217
integer height = 76
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean border = false
boolean autohscroll = false
boolean displayonly = true
end type

type sle_error_number from singlelineedit within w_error
integer x = 777
integer y = 892
integer width = 2217
integer height = 76
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean border = false
boolean autohscroll = false
boolean displayonly = true
end type

type st_error_line from statictext within w_error
integer x = 206
integer y = 692
integer width = 448
integer height = 68
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Line:"
boolean focusrectangle = false
end type

type st_error_object_event from statictext within w_error
integer x = 206
integer y = 616
integer width = 448
integer height = 68
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Object Event:"
boolean focusrectangle = false
end type

type st_error_object from statictext within w_error
integer x = 206
integer y = 540
integer width = 448
integer height = 68
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Object:"
boolean focusrectangle = false
end type

type st_error_window_menu from statictext within w_error
integer x = 206
integer y = 464
integer width = 448
integer height = 68
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Window/Menu:"
boolean focusrectangle = false
end type

type st_error_text from statictext within w_error
integer x = 206
integer y = 968
integer width = 503
integer height = 68
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Error Message:"
boolean focusrectangle = false
end type

type st_error_number from statictext within w_error
integer x = 206
integer y = 892
integer width = 475
integer height = 68
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Error Number:"
boolean focusrectangle = false
end type

type sle_sql_db_code from singlelineedit within w_error
integer x = 773
integer y = 1120
integer width = 2217
integer height = 76
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean border = false
boolean autohscroll = false
boolean displayonly = true
end type

type st_sql_db_code from statictext within w_error
integer x = 206
integer y = 1120
integer width = 448
integer height = 68
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "SQL DB Code:"
boolean focusrectangle = false
end type

type cb_ok from commandbutton within w_error
integer x = 2738
integer y = 1760
integer width = 347
integer height = 116
integer taborder = 40
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "OK"
boolean default = true
end type

event clicked;integer			vli_counter, li_upperbound, li_rtn
string			vls_msmail_recipient_name, ls_run
LONG        ll_subject_len, ll_error_len

STRING ls_subject_first, ls_subject, ls_text, ls_recipients[], ls_recipient_list, ls_from

IF IsValid(G_PFSecurity) THEN
	
	ls_from = "default"
	
	// Check to see if there are any mail recipients
	// If there aren't THEN there is no need to continue
	vls_msmail_recipient_name = G_PFSecurity.uof_return_mail_recipient(1)
	If IsNull(vls_msmail_recipient_name) or vls_msmail_recipient_name = "" THEN
		Close(Parent)
		Return
	END IF
	
	// Prepare mail message
	ls_subject_first = "Application Error - " + sle_application.text + " - " + sle_error_window_menu.text
	ll_error_len = 74 - LEN(ls_subject_first)
	ls_subject = ls_subject_first + " - " + LEFT(mle_error_text.text, ll_error_len)
	
	ls_subject = f_replace_ascii(ls_subject,Char(32))
	
	ls_text =&
	"General Error Info"+&
	"~r~n------------------------------~r~n" + &
	"~r~n  Application Name: " + sle_application.text+&
	"~r~n  Database: "+sle_database.text+&
	"~r~n  Window Menu: "+sle_error_window_menu.text+&
	"~r~n  Object: "+sle_error_object.text+&
	"~r~n  Object Event: "+sle_error_object_event.text+&
	"~r~n  Line: "+sle_error_line.text + &
	"~r~n~r~n"
	
	IF Error.is_type = 'D' THEN
		ls_text = ls_text + &
		"DatabaseError Info"+ &
		"~r~n------------------------------~r~n" + &
		"~r~n  SQL DB Code: "+sle_sql_db_code.text + &
		"~r~n  SQL Return Data: "+ sle_return_data.text+&
		"~r~n  SQL Code: "+sle_error_number.text+&
		"~r~n  SQL ErrorText: " + mle_error_text.text
	ELSE
		ls_text = ls_text +&
		"~r~n~r~nSystem Error Info"+&
		"~r~n------------------------------~r~n" + &
		"~r~n  Error Number: "+sle_error_number.text + &
		"~r~n  Error Text: " + mle_error_text.text
	END IF
	
	If mle_comment.Text > "" THEN
		ls_text = ls_text + "~r~n~r~nUser Comment " + &
					"~r~n------------------------------~r~n~r~n" + mle_comment.text
	END IF
	
	// Set up mail recipients
	vli_counter = 1
	li_upperbound = UpperBound(G_PFSecurity.vis_mail_recipients[])
	Do Until vli_counter > li_upperbound
		vls_msmail_recipient_name = G_PFSecurity.uof_return_mail_recipient(vli_counter)
	
		If vls_msmail_recipient_name = "" THEN
			Exit
		Else
			IF vli_counter = li_upperbound THEN
				ls_recipient_list = ls_recipient_list + vls_msmail_recipient_name
			ELSE
				ls_recipient_list = ls_recipient_list + vls_msmail_recipient_name + ','
			END IF
		END IF
	
		vli_counter++
	Loop

	ls_run = gs_email_path + ' "'+ ls_from + '" "' + ls_recipient_list + '" "' + ls_subject + '" "' + ls_text + '" " "'
	clipboard(ls_run)
	
	// Send the mail
	li_rtn = Run(ls_run, Minimized!)	
	
	IF li_rtn < 0 THEN
		MessageBox("Mail Send", "Sending mail message failed - application error was not sent to help desk!")
		Return
	END IF
	
	Close(Parent)
	Return
ELSE
	Close(Parent)
	Return
END IF
end event

type st_database from statictext within w_error
integer x = 206
integer y = 388
integer width = 297
integer height = 68
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Database: "
boolean focusrectangle = false
end type

type sle_database from singlelineedit within w_error
integer x = 777
integer y = 388
integer width = 2217
integer height = 76
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean border = false
boolean autohscroll = false
boolean displayonly = true
end type

type st_return_data from statictext within w_error
integer x = 206
integer y = 1204
integer width = 512
integer height = 68
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "SQL Return Data:"
boolean focusrectangle = false
end type

type gb_1 from groupbox within w_error
integer x = 87
integer y = 1400
integer width = 3040
integer height = 336
integer taborder = 50
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Comment"
end type

type gb_general from groupbox within w_error
integer x = 82
integer y = 232
integer width = 3040
integer height = 560
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "General Info"
end type

type gb_error from groupbox within w_error
integer x = 82
integer y = 820
integer width = 3040
integer height = 560
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "System Error"
end type

