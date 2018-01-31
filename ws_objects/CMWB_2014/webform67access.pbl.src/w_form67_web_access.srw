$PBExportHeader$w_form67_web_access.srw
forward
global type w_form67_web_access from w_ancestor
end type
type uo_image_append from u_image_append within w_form67_web_access
end type
type tab_maintain from tab within w_form67_web_access
end type
type tabpage_employer from userobject within tab_maintain
end type
type st_4 from statictext within tabpage_employer
end type
type em_employer_no from editmask within tabpage_employer
end type
type cb_1 from commandbutton within tabpage_employer
end type
type cb_delete_row from commandbutton within tabpage_employer
end type
type cb_add_row from commandbutton within tabpage_employer
end type
type dw_email_notification from u_dw_online within tabpage_employer
end type
type dw_operation from u_dw_online within tabpage_employer
end type
type cbx_enable_access from checkbox within tabpage_employer
end type
type cb_enable_access from commandbutton within tabpage_employer
end type
type st_3 from statictext within tabpage_employer
end type
type st_2 from statictext within tabpage_employer
end type
type cb_save from commandbutton within tabpage_employer
end type
type cb_cancel from commandbutton within tabpage_employer
end type
type dw_employer from u_dw_online within tabpage_employer
end type
type cb_provider_search from commandbutton within tabpage_employer
end type
type tabpage_employer from userobject within tab_maintain
st_4 st_4
em_employer_no em_employer_no
cb_1 cb_1
cb_delete_row cb_delete_row
cb_add_row cb_add_row
dw_email_notification dw_email_notification
dw_operation dw_operation
cbx_enable_access cbx_enable_access
cb_enable_access cb_enable_access
st_3 st_3
st_2 st_2
cb_save cb_save
cb_cancel cb_cancel
dw_employer dw_employer
cb_provider_search cb_provider_search
end type
type tabpage_67_reports from userobject within tab_maintain
end type
type cb_2 from commandbutton within tabpage_67_reports
end type
type st_1 from statictext within tabpage_67_reports
end type
type em_confirmation from editmask within tabpage_67_reports
end type
type cb_confirmation from commandbutton within tabpage_67_reports
end type
type dw_reports_operation from u_dw_online within tabpage_67_reports
end type
type dw_reports from u_dw_online within tabpage_67_reports
end type
type dw_reports_employer from u_dw_online within tabpage_67_reports
end type
type tabpage_67_reports from userobject within tab_maintain
cb_2 cb_2
st_1 st_1
em_confirmation em_confirmation
cb_confirmation cb_confirmation
dw_reports_operation dw_reports_operation
dw_reports dw_reports
dw_reports_employer dw_reports_employer
end type
type tab_maintain from tab within w_form67_web_access
tabpage_employer tabpage_employer
tabpage_67_reports tabpage_67_reports
end type
end forward

global type w_form67_web_access from w_ancestor
integer y = 48
integer width = 3282
integer height = 2764
string title = "Form 67 Web Access"
string menuname = "m_cmwb_notools"
windowtype windowtype = main!
long backcolor = 67108864
uo_image_append uo_image_append
tab_maintain tab_maintain
end type
global w_form67_web_access w_form67_web_access

type prototypes

end prototypes

type variables
u_dw_document_path 	iu_dw_document_path

n_filter					inv_filter



end variables

forward prototypes
public subroutine wf_reset_dws_for_employer ()
public subroutine wf_retrieve_dws_for_employer (long al_employer_no)
public function boolean wf_get_website_access (long al_employer_no)
public function integer wf_check_email_format (string as_email)
public function long wf_get_employer_no_from_confirmation (long al_confirmation_no)
public function long wf_get_operation_no_from_confirmation (long al_confirmation_no)
end prototypes

public subroutine wf_reset_dws_for_employer ();/* do the retrieve for the users */
tab_maintain.tabpage_employer.dw_employer.reset()
tab_maintain.tabpage_employer.dw_operation.reset()
tab_maintain.tabpage_employer.dw_email_notification.reset()

//reports tab
tab_maintain.tabpage_67_reports.dw_reports.reset()
tab_maintain.tabpage_67_reports.dw_reports_operation.reset()
tab_maintain.tabpage_67_reports.dw_reports_employer.reset()


end subroutine

public subroutine wf_retrieve_dws_for_employer (long al_employer_no);INTEGER li_operation_no, li_operation_rowcount

/* do the retrieve for the users */
/* employer information */
tab_maintain.tabpage_employer.dw_employer.retrieve(al_employer_no)
SQLCA.nf_handle_error("w_form67_web_access","wf_retrieve_dws_for_employer()",".dw_employer.retrieve(al_employer_no)")

/* operation information */
li_operation_rowcount = tab_maintain.tabpage_employer.dw_operation.retrieve(al_employer_no)
SQLCA.nf_handle_error("w_form67_web_access","wf_retrieve_dws_for_employer()","dw_operation.retrieve(al_employer_no)")

/* retrieve the report tab information */
/* employer information */
tab_maintain.tabpage_67_reports.dw_reports_employer.retrieve(al_employer_no)
SQLCA.nf_handle_error("w_form67_web_access","wf_retrieve_dws_for_employer()","dw_reports_employer.retrieve(al_employer_no)")

/* operation information */
li_operation_rowcount = tab_maintain.tabpage_67_reports.dw_reports_operation.retrieve(al_employer_no)
SQLCA.nf_handle_error("w_form67_web_access","wf_retrieve_dws_for_employer()","dw_operation.retrieve(al_employer_no)")

IF li_operation_rowcount > 0  THEN 
	// select the first row - scroll to it grab the operation number and retrieve the web information
	
	 tab_maintain.tabpage_employer.dw_operation.SelectRow(0, false)
	 tab_maintain.tabpage_employer.dw_operation.SelectRow(1, true)

	 li_operation_no =  tab_maintain.tabpage_employer.dw_operation.getitemnumber(1, 'operation_no')

	/* web_email_notification_operation */
	tab_maintain.tabpage_employer.dw_email_notification.retrieve(al_employer_no, li_operation_no)
	SQLCA.nf_handle_error("w_form67_web_access","wf_retrieve_dws_for_employer()","dw_email_notification.retrieve(al_employer_no)")
	
	/* retrieve the report tab information */
	 tab_maintain.tabpage_67_reports.dw_reports_operation.SelectRow(0, false)
	 tab_maintain.tabpage_67_reports.dw_reports_operation.SelectRow(1, true)

	 tab_maintain.tabpage_67_reports.dw_reports_operation.getitemnumber(1, 'operation_no')

	/* web_email_notification_operation */
 	tab_maintain.tabpage_67_reports.dw_reports.retrieve(al_employer_no, li_operation_no)
 	SQLCA.nf_handle_error("w_form67_web_access","wf_retrieve_dws_for_employer()","dw_reports.retrieve(al_employer_no)")

END IF 
end subroutine

public function boolean wf_get_website_access (long al_employer_no);INTEGER			li_access_count
BOOLEAN			lb_access

// find out what the current access is 
SELECT 	COUNT(*) 
INTO 		:li_access_count
FROM     WEB_EMPLOYER_FORM67_ACCESS
WHERE 	employer_no = :al_employer_no
USING 	SQLCA;
SQLCA.nf_handle_error("w_form67_web_access","wf_get_website_access",".dw_employer.retrieve(ll_emplyer_no)")

IF ISNULL(li_access_count) THEN li_access_count = 0

IF li_access_count > 0  THEN 
	lb_access = TRUE
ELSE  
	lb_access = FALSE
END IF 

RETURN lb_access
end function

public function integer wf_check_email_format (string as_email);LONG			ll_len, ll_count_at, ll_count_space
INTEGER		li_x
STRING		ls_string, ls_char


IF NOT IsNull(as_email) and as_email <> '' THEN
	
	IF LEN(as_email) > 255 THEN
		MessageBox('Error Invalid Email','The email address must be valid and not more than 255 characters.',Exclamation!)
		RETURN -1
	END IF		
	
	IF LEN(as_email) < 5  OR Pos(as_email, "@") < 2  OR Pos(as_email, ".") < 3 THEN
		MessageBox('Error Invalid Email','The email address must be in proper format ex. name@nb.ca .',Exclamation!)
		RETURN -1
	END IF
	IF Pos(as_email,'.-') <> 0 OR   Pos(as_email,'-.') <> 0 OR   Pos(as_email,'..') <> 0 THEN
		MessageBox('Error Invalid Email',"The email address must not contain '.- 'or '-.' or '..' .",Exclamation!)
		RETURN -1
	END IF
	IF Pos(as_email,'@@') <> 0 OR Pos(as_email,'--') <> 0 OR  Pos(as_email,'@.') <> 0 THEN
		MessageBox('Error Invalid Email',"The email address must not contain '@@ 'or '--' or '@.' .",Exclamation!)
		RETURN -1
	END IF
	IF Pos(as_email,'.@') <> 0 THEN
		MessageBox('Error Invalid Email',"The email address must not contain '.@ '.",Exclamation!)
		RETURN -1
	END IF
	IF Pos(as_email,' ') <> 0 THEN
		MessageBox('Error Invalid Email',"The email address must not contain a space.",Exclamation!)
		RETURN -1
	END IF	
	IF Pos(as_email, "@") = 1  OR Pos(as_email, ".") = 1 OR  Pos(as_email, " ") = 1 THEN
		MessageBox('Error Invalid Email','The email address must be more than 5 characters.',Exclamation!)
		RETURN -1
	END IF
	
	IF RIGHT(as_email, 1) = ';' THEN
		MessageBox('Error Invalid Email',"The email address must not end with ' ; '",Exclamation!)
		RETURN -1
	END IF
	
	IF RIGHT(as_email, 1) = '.' THEN
		MessageBox('Error Invalid Email',"The email address must not nd with ' . '",Exclamation!)
		RETURN -1
	END IF
	
	// must consist of (one or more alpha numeric Char) + (@) + (one or more char)+ (.)+ (0 or more char)+ (end with english/french alpha or 0-9) 
	ls_char = RIGHT(as_email,1) 
	IF (Asc(ls_char) >= Asc("a") AND Asc(ls_char) <= Asc("z")) OR (Asc(ls_char) >= Asc("A") AND Asc(ls_char) <= Asc("Z")) THEN
		//continue
	ELSE
		
		IF isnumber(ls_char) = FALSE THEN 
			messagebox("Validation Error", "The email address must end with a valid value: A-Z , 0-9")
			RETURN -1
		END IF 
	END IF
	
	
	ll_len = LEN(as_email)
	
	FOR li_x = 1 to ll_len
		ls_string = left(as_email,li_x)
		IF MID(as_email,li_x,1) = '@'  then
			ll_count_at = ll_count_at + 1
		END IF
		IF MID(as_email,li_x,1) = ' ' then
			ll_count_space = ll_count_space + 1
		END IF
	NEXT
	IF ll_count_at > 1 THEN
		MessageBox('Error Invalid Email',"The email address can not have more than one '@' character.",Exclamation!)
		RETURN -1
	END IF
	IF ll_count_space > 0 THEN
		MessageBox('Error Invalid Email',"The email address can not have a space in it.",Exclamation!)
		RETURN -1
	END IF
END IF 
	
RETURN 1
end function

public function long wf_get_employer_no_from_confirmation (long al_confirmation_no);LONG	ll_employer_no 

SELECT  	employer_no 
INTO		:ll_employer_no
FROM 		web_form67
WHERE 	form67_confirmation_no = :al_confirmation_no
USING 	SQLCA;
SQLCA.nf_handle_error("w_form67_web_access", "wf_get_employer_no_from_confirmation()", "SELECT  employer_no ")

IF isnull(ll_employer_no )  THEN ll_employer_no = 0

RETURN ll_employer_no
end function

public function long wf_get_operation_no_from_confirmation (long al_confirmation_no);INTEGER li_operation_no 

SELECT  	operation_no 
INTO		:li_operation_no
FROM 		web_form67
WHERE 	form67_confirmation_no = :al_confirmation_no
USING 	SQLCA;
SQLCA.nf_handle_error("w_form67_web_access", "wf_get_operation_no_from_confirmation()", "SELECT  operation_no ")

IF isnull(li_operation_no )  THEN li_operation_no = 0

RETURN li_operation_no
end function

on w_form67_web_access.create
int iCurrent
call super::create
if IsValid(this.MenuID) then destroy(this.MenuID)
if this.MenuName = "m_cmwb_notools" then this.MenuID = create m_cmwb_notools
this.uo_image_append=create uo_image_append
this.tab_maintain=create tab_maintain
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.uo_image_append
this.Control[iCurrent+2]=this.tab_maintain
end on

on w_form67_web_access.destroy
call super::destroy
if IsValid(MenuID) then destroy(MenuID)
destroy(this.uo_image_append)
destroy(this.tab_maintain)
end on

event open;call super::open;INTEGER			li_row
INT 				li_trancount
BOOLEAN			lb_rtn

/* APPLICATION SECURITY CODE. */
lb_rtn = G_PFSecurity.UOF_Check_Access(This)
IF lb_rtn = FALSE THEN
	Messagebox("Access Denied", "You do not have proper security priveleges to open this window.~r~r" +&
				  "If you need to open this window, Please call the Helpdesk to get the proper security priveleges.", Exclamation!)
	Close(This)
	RETURN -1
END IF

THIS.I_Authorized_Access = TRUE	

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')

tab_maintain.tabpage_employer.dw_employer.settransobject(SQLCA)
tab_maintain.tabpage_employer.dw_operation.settransobject(SQLCA)
tab_maintain.tabpage_employer.dw_email_notification.settransobject(SQLCA)


IF IsNull(inv_resize) OR NOT IsValid (inv_resize) THEN
	inv_resize = CREATE n_resize
	inv_resize.of_SetOrigSize (this.width,this.height)
END IF

//sort & filter
tab_maintain.tabpage_employer.dw_email_notification.uf_setfilter(True)
tab_maintain.tabpage_employer.dw_email_notification.uf_SetSort(True)

// EMPLOYER
THIS.inv_resize.of_register(tab_maintain,0,0,100,100)
THIS.inv_resize.of_register(tab_maintain.tabpage_employer,'scaletoright&bottom')
THIS.inv_resize.of_register(tab_maintain.tabpage_employer.dw_email_notification,'scaletoright&bottom')
THIS.inv_resize.of_register(tab_maintain.tabpage_employer.cb_save,'FixedToBottom')
THIS.inv_resize.of_register(tab_maintain.tabpage_employer.cb_cancel,'FixedToBottom')
THIS.inv_resize.of_register(tab_maintain.tabpage_employer.cb_delete_row,'FixedToBottom')
THIS.inv_resize.of_register(tab_maintain.tabpage_employer.cb_add_row,'FixedToBottom')

// REPORTS
THIS.inv_resize.of_register(tab_maintain.tabpage_67_reports,'scaletoright&bottom')
THIS.inv_resize.of_register(tab_maintain.tabpage_67_reports.dw_reports,'scaletoright&bottom')

/*	Create an instance of the user object for the view function*/
iu_dw_document_path = u_dw_document_path
This.OpenUserObject(iu_dw_document_path)
iu_dw_document_path.Hide()

end event

type uo_image_append from u_image_append within w_form67_web_access
boolean visible = false
integer x = 2135
integer y = 2592
integer width = 1029
integer height = 192
integer taborder = 20
end type

on uo_image_append.destroy
call u_image_append::destroy
end on

type tab_maintain from tab within w_form67_web_access
integer x = 23
integer y = 76
integer width = 3218
integer height = 2504
integer taborder = 10
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long backcolor = 553648127
boolean raggedright = true
boolean focusonbuttondown = true
boolean boldselectedtext = true
integer selectedtab = 1
tabpage_employer tabpage_employer
tabpage_67_reports tabpage_67_reports
end type

on tab_maintain.create
this.tabpage_employer=create tabpage_employer
this.tabpage_67_reports=create tabpage_67_reports
this.Control[]={this.tabpage_employer,&
this.tabpage_67_reports}
end on

on tab_maintain.destroy
destroy(this.tabpage_employer)
destroy(this.tabpage_67_reports)
end on

event selectionchanged;LONG				ll_employer_no
BOOLEAN			lb_access

IF newindex > 0 THEN
 THIS.Control[newindex].TabBackColor = rgb(160,204,231)
END IF 

IF oldindex > 0 THEN
 THIS.Control[oldindex].TabBackColor = 79741120
END IF 

end event

type tabpage_employer from userobject within tab_maintain
integer x = 18
integer y = 100
integer width = 3182
integer height = 2388
long backcolor = 67108864
string text = "Form67 Employer Web Access"
long tabtextcolor = 33554432
long tabbackcolor = 134217752
long picturemaskcolor = 536870912
st_4 st_4
em_employer_no em_employer_no
cb_1 cb_1
cb_delete_row cb_delete_row
cb_add_row cb_add_row
dw_email_notification dw_email_notification
dw_operation dw_operation
cbx_enable_access cbx_enable_access
cb_enable_access cb_enable_access
st_3 st_3
st_2 st_2
cb_save cb_save
cb_cancel cb_cancel
dw_employer dw_employer
cb_provider_search cb_provider_search
end type

on tabpage_employer.create
this.st_4=create st_4
this.em_employer_no=create em_employer_no
this.cb_1=create cb_1
this.cb_delete_row=create cb_delete_row
this.cb_add_row=create cb_add_row
this.dw_email_notification=create dw_email_notification
this.dw_operation=create dw_operation
this.cbx_enable_access=create cbx_enable_access
this.cb_enable_access=create cb_enable_access
this.st_3=create st_3
this.st_2=create st_2
this.cb_save=create cb_save
this.cb_cancel=create cb_cancel
this.dw_employer=create dw_employer
this.cb_provider_search=create cb_provider_search
this.Control[]={this.st_4,&
this.em_employer_no,&
this.cb_1,&
this.cb_delete_row,&
this.cb_add_row,&
this.dw_email_notification,&
this.dw_operation,&
this.cbx_enable_access,&
this.cb_enable_access,&
this.st_3,&
this.st_2,&
this.cb_save,&
this.cb_cancel,&
this.dw_employer,&
this.cb_provider_search}
end on

on tabpage_employer.destroy
destroy(this.st_4)
destroy(this.em_employer_no)
destroy(this.cb_1)
destroy(this.cb_delete_row)
destroy(this.cb_add_row)
destroy(this.dw_email_notification)
destroy(this.dw_operation)
destroy(this.cbx_enable_access)
destroy(this.cb_enable_access)
destroy(this.st_3)
destroy(this.st_2)
destroy(this.cb_save)
destroy(this.cb_cancel)
destroy(this.dw_employer)
destroy(this.cb_provider_search)
end on

type st_4 from statictext within tabpage_employer
integer x = 2606
integer y = 416
integer width = 494
integer height = 52
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "( Employer No. )"
alignment alignment = center!
boolean focusrectangle = false
end type

type em_employer_no from editmask within tabpage_employer
integer x = 2606
integer y = 476
integer width = 494
integer height = 92
integer taborder = 30
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
string text = "none"
alignment alignment = right!
borderstyle borderstyle = stylelowered!
string mask = "##########"
end type

type cb_1 from commandbutton within tabpage_employer
integer x = 2601
integer y = 584
integer width = 494
integer height = 92
integer taborder = 20
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Quick Search"
boolean default = true
end type

event clicked;LONG				ll_employer_no
BOOLEAN			lb_access

ll_employer_no = LONG(em_employer_no.text)

IF ISNULL(ll_employer_no)OR ll_employer_no < 0 THEN RETURN 

wf_reset_dws_for_employer()
wf_retrieve_dws_for_employer(ll_employer_no)
		
lb_access = wf_get_website_access(ll_employer_no)
IF lb_access = TRUE  THEN 
	cbx_enable_access.checked = TRUE
ELSE
	cbx_enable_access.checked = FALSE
END IF 

end event

type cb_delete_row from commandbutton within tabpage_employer
integer x = 389
integer y = 2168
integer width = 402
integer height = 88
integer taborder = 70
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Remove Row"
end type

event clicked;INTEGER 			li_row
dwItemStatus 	l_status

IF dw_operation.rowcount() < 1  THEN RETURN 

/* make sure the employer has access */
IF cbx_enable_access.checked = FALSE  THEN 
	Messagebox('No Web Access', 'Please check the web access on before removing emails', information!)
	RETURN
END IF 

li_row = dw_email_notification.getrow()
IF li_row > 0  THEN 
	
	l_status = dw_email_notification.GetItemStatus(li_row, 0, PRIMARY!)

	dw_email_notification.DELETEROW(li_row)	
	
	// if the row is new or new modified just remove it else remove and click the save button
	IF l_status <> NEW! AND l_status <> NEWMODIFIED! THEN 
		Messagebox('Deleted Row', 'Please remember to save your changes!', information!)
	END IF 	
END IF 
end event

type cb_add_row from commandbutton within tabpage_employer
integer x = 27
integer y = 2168
integer width = 334
integer height = 88
integer taborder = 60
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Add Row"
end type

event clicked;INTEGER 		li_row, li_operation_no
LONG	  		ll_employer_no
BOOLEAN		lb_access


IF dw_operation.rowcount() < 1  THEN RETURN 

li_operation_no = dw_operation.getitemnumber(dw_operation.getrow(), 'operation_no')
ll_employer_no  = dw_operation.getitemnumber(dw_operation.getrow(), 'employer_no')

// find out what the current access is 
lb_access = wf_get_website_access(ll_employer_no)

/* make sure the employer has access */
IF lb_access = FALSE  THEN 
	Messagebox('No Web Access', 'Please check the web access on and click the ‘Confirm’ button before you add the contact information.', information!)
	RETURN
END IF 
 
li_row =  dw_email_notification.insertrow(0)

IF li_operation_no > 0 AND ll_employer_no > 0 THEN 
	/* set a few defaults */
	dw_email_notification.setitem(li_row, 'operation_no', li_operation_no)
	dw_email_notification.setitem(li_row, 'employer_no', ll_employer_no)
END IF 

dw_email_notification.scrolltorow(li_row)
dw_email_notification.SelectRow(0, false)
dw_email_notification.SelectRow(li_row, true)
dw_email_notification.SetColumn(1)
dw_email_notification.setfocus()
end event

type dw_email_notification from u_dw_online within tabpage_employer
integer x = 18
integer y = 1492
integer width = 3136
integer height = 660
integer taborder = 60
string dataobject = "d_email_notification_operation_f67"
boolean hscrollbar = true
boolean vscrollbar = true
end type

event constructor;call super::constructor;this.uf_setselect(1)
end event

type dw_operation from u_dw_online within tabpage_employer
integer x = 18
integer y = 1080
integer width = 3141
integer height = 404
integer taborder = 50
string dataobject = "d_operation_list"
boolean vscrollbar = true
end type

event rowfocuschanged;call super::rowfocuschanged;LONG 		ll_employer_no
INTEGER 	li_operation_no, li_row

li_row = THIS.getrow()

IF li_row > 0 THEN 
	ll_employer_no 	= THIS.getitemnumber(li_row,'employer_no')
	li_operation_no 	= THIS.getitemnumber(li_row,'operation_no')
	
	dw_email_notification.retrieve(ll_employer_no, li_operation_no)
	SQLCA.nf_handle_error("w_form67_web_access","dw_email_notification","retrieve(ll_employer_no, li_operation_no)")
END IF 
end event

event constructor;call super::constructor;this.uf_setselect(1)
end event

type cbx_enable_access from checkbox within tabpage_employer
integer x = 677
integer y = 852
integer width = 1330
integer height = 80
integer textsize = -12
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 32768
long backcolor = 67108864
string text = "WSNB Application Access Enabled"
end type

event clicked;IF dw_employer.rowcount() < 1 OR isnull(dw_employer.rowcount()) THEN 
	THIS.checked = FALSE
	RETURN 
END IF 
end event

type cb_enable_access from commandbutton within tabpage_employer
integer x = 2062
integer y = 840
integer width = 402
integer height = 92
integer taborder = 40
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Confirm"
end type

event clicked;LONG 		ll_employer_no
INTEGER	li_message
BOOLEAN  lb_access = FALSE
STRING	ls_term

IF dw_employer.rowcount() < 1 THEN RETURN 

// grab the employer_no from the employer section 
ll_employer_no = dw_employer.getitemnumber(1,'employer_no')

IF ISNULL(ll_employer_no) THEN ll_employer_no = 0

// find out what the current access is 
lb_access = wf_get_website_access(ll_employer_no)

//if there is a change - if not don`t do anything
IF cbx_enable_access.checked = lb_access OR ll_employer_no <= 0 THEN RETURN 

/*
If the click the access off (remove access) remove them from 
web_employer_form67_access & web_email_notification_operation
*/
IF cbx_enable_access.checked = TRUE THEN 
	ls_term = 'Allow'
ELSE
	ls_term = 'Remove'
END IF 

// confirm - they only get one chance
li_message = messagebox('Warning!!!!!!','Please confirm that you want to ' + ls_term + ' access for this Employer', Question!,YesNo!, 2)
IF li_message = 2  THEN
	//reset the checkbox
	lb_access = wf_get_website_access(ll_employer_no)
	IF lb_access = TRUE  THEN 
		cbx_enable_access.checked = TRUE
	ELSE
		cbx_enable_access.checked = FALSE
	END IF 	

	RETURN 
END IF 

// NOW -- IF THE CHECKED FACTOR IS OPPOSITE FO THE CURRENT STATUS DO THE DELETES
IF cbx_enable_access.checked = TRUE AND lb_access = FALSE THEN  //GRANT ACCESS
	// insert row into WEB_EMPLOYER_FORM67_ACCESS
	
	 SQLCA.nf_begin_transaction()
	 
	 INSERT INTO WEB_EMPLOYER_FORM67_ACCESS  
   	      (employer_no)  
	 VALUES 
   	      (:ll_employer_no)
	 USING SQLCA;
	 SQLCA.nf_handle_error("w_form67_web_access","cb_enable_access - clicked","Embedded SQL:  INSERT INTO WEB_EMPLOYER_FORM67_ACCESS ")
	 
	  SQLCA.nf_commit_transaction()
	
ELSEIF cbx_enable_access.checked = FALSE AND lb_access = TRUE  THEN//REMOVE ACCESS 
	
	// remove row from WEB_EMPLOYER_FORM67_ACCESS and WEB_EMAIL_NOTIFICATION_OPERATION
		 SQLCA.nf_begin_transaction()

		DELETE  	WEB_EMAIL_NOTIFICATION_OPERATION
		WHERE  	employer_no = :ll_employer_no
		USING 	SQLCA ;
		SQLCA.nf_handle_error('w_form67_web_access', 'cb_enable_access - clicked', 'DELETE  FROM WEB_EMAIL_NOTIFICATION_OPERATION')
			
		DELETE  	WEB_EMPLOYER_FORM67_ACCESS
		WHERE  	employer_no = :ll_employer_no
		USING 	SQLCA;
		SQLCA.nf_handle_error('w_form67_web_access', 'cb_enable_access - clicked', 'DELETE  FROM WEB_EMPLOYER_FORM67_ACCESS')

		SQLCA.nf_commit_transaction()			
			
ELSE 
	messagebox('Information!!!!!!','There has been no change to the Employers Access',Information!)
	RETURN
END IF 

//refresh the screen regardless of access change
cb_cancel.triggerevent('clicked')

end event

type st_3 from statictext within tabpage_employer
integer x = 14
integer y = 992
integer width = 3154
integer height = 80
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Email Notification"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_2 from statictext within tabpage_employer
integer x = 14
integer y = 704
integer width = 3154
integer height = 68
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "Web Site Access"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cb_save from commandbutton within tabpage_employer
integer x = 2587
integer y = 2168
integer width = 274
integer height = 88
integer taborder = 50
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Save"
end type

event clicked;INTEGER	li_email_type, li_counter, li_operation_no, li_found_count, li_inner_counter, li_operation_no_to_return, li_insertRow, li_email_type_check
LONG		ll_record_no, ll_employer_no, ll_find, ll_pos, ll_rows, ll_current_row
STRING   ls_email, ls_last_name, ls_first_name, ls_position_desc, ls_telephone_no, ls_cellphone_no, ls_inner_email
STRING   ls_search_string, ls_first_name_check, ls_last_name_check, ls_email_check, ls_position_desc_check, ls_telephone_no_check, ls_cellphone_no_check
Boolean lb_Messaged, lb_update

DWItemStatus l_dw_status
u_ds  lds_contact


/* BR'S
 -- CHECK FORMAT OF EMAIL 
 -- CHECK FORMAT OF PHONE NUMBERS
 -- CHECK FOR MINIMUM CHARACTERS
 -- CHECK TRIM
 -- CHECK EMAIL NUMBER IS NOT A DUPLICATE
 
 idx1 employer_no, operation_no, email_notification_type_code, email_address

*/

dw_email_notification.accepttext()

IF dw_operation.rowcount() < 1  THEN RETURN

// return to the correct operation once the record has saved
li_operation_no_to_return = dw_operation.getrow()

FOR li_counter = 1 TO dw_email_notification.rowcount()

	ll_record_no 			= dw_email_notification.getitemnumber(li_counter, 'record_no')
	ll_employer_no 		= dw_email_notification.getitemnumber(li_counter, 'employer_no')
	li_operation_no 		= dw_email_notification.getitemnumber(li_counter, 'operation_no')
	ls_email					= dw_email_notification.getitemstring(li_counter, 'email_address')
	li_email_type			= dw_email_notification.getitemnumber(li_counter, 'email_notification_type_code')
	ls_last_name 			= dw_email_notification.getitemstring(li_counter, 'last_name')
	ls_first_name 			= dw_email_notification.getitemstring(li_counter, 'first_name')
	ls_position_desc 		= dw_email_notification.getitemstring(li_counter, 'position_desc')
	ls_telephone_no		= dw_email_notification.getitemstring(li_counter, 'telephone_no')
	ls_cellphone_no 		= dw_email_notification.getitemstring(li_counter, 'cellphone_no')
		
	/****************************************************************************************************/
	//trim the data	
	
	IF LEFT(ls_email,1)         = ' ' OR RIGHT(ls_email,1)         = ' ' THEN dw_email_notification.setitem(li_counter,'email_address',Trim(dw_email_notification.GetItemString(li_counter,'email_address')))
	IF LEFT(ls_last_name,1)     = ' ' OR RIGHT(ls_last_name,1)     = ' ' THEN dw_email_notification.setitem(li_counter,'last_name',Trim(dw_email_notification.GetItemString(li_counter,'last_name')))
	IF LEFT(ls_first_name,1)    = ' ' OR RIGHT(ls_first_name,1)    = ' ' THEN dw_email_notification.setitem(li_counter,'first_name',Trim(dw_email_notification.GetItemString(li_counter,'first_name')))
	IF LEFT(ls_position_desc,1) = ' ' OR RIGHT(ls_position_desc,1) = ' ' THEN dw_email_notification.setitem(li_counter,'position_desc',Trim(dw_email_notification.GetItemString(li_counter,'position_desc')))
	IF ISNULL(ls_telephone_no)        OR ls_telephone_no           = ' ' THEN 
		ls_telephone_no = ''
		dw_email_notification.SetItem(li_counter,'telephone_no', ls_telephone_no)
	END IF
	
	IF ISNULL(ls_cellphone_no)        OR ls_cellphone_no           = ' ' THEN
		ls_cellphone_no = ''
		dw_email_notification.SetItem(li_counter,'cellphone_no', ls_cellphone_no)
	END IF
	
	ls_cellphone_no  = TRIM(ls_cellphone_no)
	ls_telephone_no  = TRIM(ls_telephone_no)
	
	/****************************************************************************************************/
	// Mandatory checks
	IF ISNULL(ls_email) OR TRIM(ls_email) = '' THEN 
		MessageBox('Error','The Email Address is mandatory',Exclamation!)
		dw_email_notification.scrolltorow(li_counter)
		RETURN -1	
	END IF 
	
	IF ISNULL(ls_last_name) OR TRIM(ls_last_name) = '' THEN 
		MessageBox('Error','The Last Name is mandatory',Exclamation!)
		dw_email_notification.scrolltorow(li_counter)
		RETURN -1	
	END IF 
	
	IF ISNULL(ls_first_name) OR TRIM(ls_first_name) = '' THEN 
		MessageBox('Error','The First Name is mandatory',Exclamation!)
		dw_email_notification.scrolltorow(li_counter)
		RETURN -1	
	END IF 
	
	IF ISNULL(ls_position_desc) OR TRIM(ls_position_desc) = '' THEN 
		MessageBox('Error','The Position Description is mandatory',Exclamation!)
		dw_email_notification.scrolltorow(li_counter)
		RETURN -1	
	END IF 
	
	IF ISNULL(li_email_type) OR li_email_type < 0 THEN 
		MessageBox('Error','The Email Type is mandatory',Exclamation!)
		dw_email_notification.scrolltorow(li_counter)
		RETURN -1	
	END IF 
	
	IF ISNULL(ll_employer_no) OR ll_employer_no <= 0 THEN 
		MessageBox('Error','The Employer is mandatory',Exclamation!)//should never happen
		dw_email_notification.scrolltorow(li_counter)
		RETURN -1	
	END IF 
	
	IF ISNULL(li_operation_no) OR li_operation_no <= 0 THEN 
		MessageBox('Error','The Operation is mandatory',Exclamation!)//should never happen
		dw_email_notification.scrolltorow(li_counter)
		RETURN -1	
	END IF 
	
	/****************************************************************************************************/
	// check the string columns for minimum chars
	IF len(trim(ls_email)) < 3 AND len(trim(ls_email)) > 0 THEN
		MessageBox('Error','The Email entered for the Operation must be greater then 3 characters',Exclamation!)
		dw_email_notification.scrolltorow(li_counter)
		RETURN -1	
	END IF 

	IF len(trim(ls_last_name)) =1  THEN
		MessageBox('Error','The Last Name entered must be greater then 1 character',Exclamation!)
		dw_email_notification.scrolltorow(li_counter)
		RETURN -1	
	END IF 
			
	IF len(trim(ls_first_name)) = 1 THEN
		MessageBox('Error','The First Name entered must be greater then 1 character',Exclamation!)
		dw_email_notification.scrolltorow(li_counter)
		RETURN -1	
	END IF 
	
	IF len(trim(ls_position_desc)) < 3 AND len(trim(ls_position_desc)) > 0 THEN
		MessageBox('Error','The Position Description must be greater then 2 characters',Exclamation!)
		dw_email_notification.scrolltorow(li_counter)
		RETURN -1	
	END IF 
	
	/****************************************************************************************************/
	// Phone number validations 
	IF ls_cellphone_no <> '' THEN	
		IF MID(ls_cellphone_no,1,1) = '0' OR MID(ls_cellphone_no,1,1) = '1' THEN
			MessageBox('Error Invalid Cellphone No','The cell phone no must be a valid phone no and can not start with 0 or 1.',Exclamation!)
			dw_email_notification.scrolltorow(li_counter)
			RETURN -1
		END IF
		
		IF LEN(ls_cellphone_no) <> 10 AND LEN(ls_cellphone_no) <> 0 THEN
			MessageBox('Error Invalid Cellphone No','The cell phone no must be a valid phone no.',Exclamation!)
			dw_email_notification.scrolltorow(li_counter)
			RETURN -1
		END IF	
		
		ll_pos = Pos(ls_cellphone_no, ' ', 1)			
      IF Len(Trim(ls_cellphone_no)) <> 10 OR ll_pos > 0 THEN
        	MessageBox('Error Invalid Cellphone No','The complete CellPhone number has not been entered. Please correct.')
			dw_email_notification.scrolltorow(li_counter)
         RETURN -1
	   END IF
	END IF	
	
	//Telephone check
	IF TRIM(ls_telephone_no) <> '' THEN	
		IF MID(ls_telephone_no,1,1) = '0' OR MID(ls_telephone_no,1,1) = '1' THEN
			MessageBox('Error Invalid Telephone No','The telephone no must be a valid phone no and can not start with 0 or 1.',Exclamation!)
			dw_email_notification.scrolltorow(li_counter)
			RETURN -1
		END IF
	
		IF LEN(ls_telephone_no) <> 10 AND LEN(ls_telephone_no) <> 0 THEN
			MessageBox('Error Invalid Telephone No','The telephone no must be a valid phone no.',Exclamation!)
			dw_email_notification.scrolltorow(li_counter)
			RETURN -1
		END IF
		
		ll_pos = Pos(ls_telephone_no, ' ', 1)			
      IF Len(Trim(ls_telephone_no)) <> 10 OR ll_pos > 0 THEN
        	MessageBox('Error Invalid Telephone No','The complete Telephone number has not been entered. Please correct.')
			dw_email_notification.scrolltorow(li_counter)
         RETURN -1
	   END IF
	END IF	
	
	/****************************************************************************************************/
	//check all the email rules
	IF wf_check_email_format(ls_email) = -1  THEN RETURN -1
	
	string ls_email_address
	
	//CHECK EMAIL IS UNIQUE
	li_found_count = 0
	FOR li_inner_counter = 1 TO dw_email_notification.rowcount()	
		 ls_email_address = dw_email_notification.getitemstring(li_inner_counter, 'email_address')
		IF lower(ls_email) = lower(ls_email_address) THEN
			li_found_count ++
		END IF 
	NEXT
		
	IF li_found_count > 1 THEN 
		MessageBox('Error','The Email Address must be unique for the Operation',Exclamation!)
		dw_email_notification.scrolltorow(li_inner_counter)
		RETURN -1	
	END IF 
		
NEXT

/*********************** UPDATE SECTION ***********************/
/* Database Updates
This will be used to populate the following database tables:
•	[WEB_EMAIL_NOTIFICATION_OPERATION]
*/

//T016290 - add functionality to have new data or changed data applied to other operations
// first, get row count from the operation datawindow. No point bugging the user for a decision to add data to other operations, if there is only one operation for this employer
IF dw_operation.rowcount() > 1  THEN
	l_dw_status = dw_email_notification.getItemStatus(dw_email_notification.getRow(), 0, PRIMARY!)
	
	IF l_dw_status = NewModified! THEN
		IF MESSAGEBOX("","Would you like to add this contact information to the employer's other operations?", QUESTION!, YESNO!, 2) = 1 THEN
			
			FOR li_counter = 1 TO dw_operation.rowcount()  // 
				
				IF dw_operation.getRow() = li_counter then continue   // skip the operation that has just had a row added, its already in the buffer, and we dont want to add a duplicate for that operation
				
				li_operation_no = dw_operation.getitemnumber(li_counter, 'operation_no')
				
				//there is a chanch that the email, being added to this operation already exists as a contact email in other operations. And if that is the case
				// adding it a second time will cause a constraint error, so we need to check for this
				Select count(*)
				into     :li_found_count
				FROM   WEB_EMAIL_NOTIFICATION_OPERATION
				WHERE email_address = :ls_email
				AND     employer_no = :ll_employer_no
				AND      operation_no = :li_operation_no
				USING SQLCA;
				
				SQLCA.nf_handle_error("w_form67_web_access", "cb_save.clicked", "Select count(*) from WEB_EMAIL_NOTIFICATION_OPERATION")
				
				IF li_found_count > 0 THEN
					MESSAGEBOX("Duplicate Email Address", "Operation No. " + STRING(li_operation_no) + " already has this email address assigned to one of the contacts."&
					                                                      + "~rThis operation will not be updated")
					continue
				END IF
				li_insertRow     = dw_email_notification.insertRow(0)
				dw_email_notification.setitem(li_insertRow, 'record_no', ll_record_no)
				dw_email_notification.setitem(li_insertRow, 'employer_no', ll_employer_no)
				dw_email_notification.setitem(li_insertRow, 'operation_no', li_operation_no)
				dw_email_notification.setitem(li_insertRow, 'email_address', trim(ls_email))
				dw_email_notification.setitem(li_insertRow, 'email_notification_type_code', li_email_type)
				dw_email_notification.setitem(li_insertRow, 'last_name', trim(ls_last_name))
				dw_email_notification.setitem(li_insertRow, 'first_name', trim(ls_first_name))
				dw_email_notification.setitem(li_insertRow, 'position_desc', trim(ls_position_desc))
				dw_email_notification.setitem(li_insertRow, 'telephone_no', trim(ls_telephone_no))
				dw_email_notification.setitem(li_insertRow, 'cellphone_no', trim(ls_cellphone_no))			
			NEXT
		END IF
	ELSEIF l_dw_status = DataModified! THEN   // UPDATE  
			
			lds_contact = create u_ds
			lds_contact.dataobject = 'd_email_notification_operation_f67' 
			lds_contact.settransObject(SQLCA)			
			
			//get the original values for these updateable variables from the datawindow
			ll_current_row  = dw_email_notification.getRow()
			ls_email_check		= dw_email_notification.getitemstring(ll_current_row, 'email_address', PRIMARY!, TRUE)
			ls_last_name_check	= dw_email_notification.getitemstring(ll_current_row, 'last_name', PRIMARY!, TRUE)
			ls_first_name_check 	= dw_email_notification.getitemstring(ll_current_row, 'first_name', PRIMARY!, TRUE)
			ls_position_desc_check 	= dw_email_notification.getitemstring(ll_current_row, 'position_desc', PRIMARY!, TRUE)
			ls_telephone_no_check	= dw_email_notification.getitemstring(ll_current_row, 'telephone_no', PRIMARY!, TRUE)
			ls_cellphone_no_check	= dw_email_notification.getitemstring(ll_current_row, 'cellphone_no', PRIMARY!, TRUE)
			
			// go thru all operations looking for another contact with same name and email address combination and if one is found,  update that record
			ls_search_string = "first_name = '" + ls_first_name_check + "' and last_name = '" + ls_last_name_check + "' and email_address =  '" + ls_email_check + "'"
			
			FOR li_counter = 1 TO dw_operation.rowcount() 
				
				IF dw_operation.getRow() = li_counter then continue  // skip the operation that has just had a row modified, its already in the buffer, and we dont need to set any items there
				
				li_operation_no = dw_operation.getitemnumber(li_counter, 'operation_no')								
				ll_rows = lds_contact.retrieve( ll_employer_no, li_operation_no)				
				
				li_found_count = lds_contact.find(ls_search_string, 1, lds_contact.rowCount())   // can never be more than one contact with the same email address per operation , so we only need to search once per operation
				
				//If we DO find an email contact in another operation that has the same email address AND same first and last names, then 
				// ask the user if they want the updated information applied to the same contact in other operations. Only ask them once, (not each time a conact is found)
				
				IF li_found_count > 0  THEN
					if lb_messaged = FALSE THEN    // only give them the messagebox once. if they say yes, flag that, then proceed with each operation update where applicable, but without the messagebox
						IF MESSAGEBOX("","Would you like to update this contact information for the same contact in another operation(s)?", QUESTION!, YESNO!, 2) = 1   THEN
							lb_update = true
						ELSE
							EXIT
						END IF
						lb_Messaged = true
					END IF
					
					IF lb_update = TRUE THEN
						IF ls_first_name_check     <> ls_first_name     THEN lds_contact.setItem(li_found_count , 'first_name', trim(ls_first_name))
						IF ls_last_name_check     <> ls_last_name      THEN lds_contact.setItem(li_found_count , 'last_name', trim(ls_last_name))
						IF ls_email_check            <> ls_email            THEN lds_contact.setItem(li_found_count , 'email_address', trim(ls_email))
						IF ls_position_desc_check <> ls_position_desc THEN lds_contact.setItem(li_found_count , 'position_desc', trim(ls_position_desc))
						IF ls_telephone_no_check <> ls_telephone_no THEN lds_contact.setItem(li_found_count , 'telephone_no', trim(ls_telephone_no))
						IF ls_cellphone_no_check <> ls_cellphone_no  THEN lds_contact.setItem(li_found_count , 'cellphone_no', trim(ls_cellphone_no))
						
						lds_contact.update()
						
					END IF
				END IF
			NEXT	
	END IF
END IF

SQLCA.nf_begin_transaction()

dw_email_notification.update()
SQLCA.nf_handle_error("w_form67_web_access", "cb_save - clicked", "dw_email_notification.update()")

SQLCA.nf_commit_transaction()

/*********************** FINISH UP ****************************/

cb_cancel.triggerevent('clicked')

// now return to the correct operation
dw_operation.scrolltorow(li_operation_no_to_return)
dw_operation.triggerevent('rowfocuschanged')



end event

type cb_cancel from commandbutton within tabpage_employer
integer x = 2885
integer y = 2168
integer width = 274
integer height = 88
integer taborder = 50
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cancel"
boolean default = true
end type

event clicked;LONG			ll_employer_no
BOOLEAN		lb_access

// grab the current employer , reset the dw's retrieve with the employer
IF dw_employer.rowcount()< 1  THEN RETURN 
	
ll_employer_no = dw_employer.getitemnumber(1, 'employer_no')

wf_reset_dws_for_employer()
wf_retrieve_dws_for_employer(ll_employer_no)

lb_access = wf_get_website_access(ll_employer_no)
IF lb_access = TRUE  THEN 
	cbx_enable_access.checked = TRUE
ELSE
	cbx_enable_access.checked = FALSE
END IF 					

end event

type dw_employer from u_dw_online within tabpage_employer
integer x = 5
integer y = 12
integer width = 2478
integer height = 656
integer taborder = 30
string dataobject = "d_employer_address"
borderstyle borderstyle = styleshadowbox!
end type

event ue_itemchangeaccepted;call super::ue_itemchangeaccepted;
end event

type cb_provider_search from commandbutton within tabpage_employer
integer x = 2688
integer y = 48
integer width = 320
integer height = 92
integer taborder = 20
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Search"
end type

event clicked;STRING			ls_provider_type,  ls_name
LONG				ll_employer_no
INTEGER			li_row
BOOLEAN			lb_access

S_WINDOW_MESSAGE		lstr_message

/*	Pass the search window a X which will enable the searching of all types.
   and disables command buttons on the employer search window.
*/
IF SQLCA.ServiceAvailable() THEN
	
	lstr_message.al_doubleparm[1] = 0
	lstr_message.al_doubleparm[2] = 0
		
	lstr_message.as_stringparm[1] = 'X'

	Openwithparm(w_employer_search,lstr_message)
ELSE
	Beep(2)
END IF
	
lstr_message = Message.PowerObjectParm
IF lstr_message.al_doubleparm[1] > 0 THEN
				
	ll_employer_no	=  lstr_message.al_doubleparm[1]
	ls_name 			=  lstr_message.as_stringparm[1]
		
	wf_reset_dws_for_employer()
	wf_retrieve_dws_for_employer(ll_employer_no)
		
	lb_access = wf_get_website_access(ll_employer_no)
	IF lb_access = TRUE  THEN 
		cbx_enable_access.checked = TRUE
	ELSE
		cbx_enable_access.checked = FALSE
	END IF 					
END IF 





end event

type tabpage_67_reports from userobject within tab_maintain
integer x = 18
integer y = 100
integer width = 3182
integer height = 2388
long backcolor = 67108864
string text = "Form67 Reports"
long tabtextcolor = 33554432
long tabbackcolor = 553648127
long picturemaskcolor = 536870912
cb_2 cb_2
st_1 st_1
em_confirmation em_confirmation
cb_confirmation cb_confirmation
dw_reports_operation dw_reports_operation
dw_reports dw_reports
dw_reports_employer dw_reports_employer
end type

on tabpage_67_reports.create
this.cb_2=create cb_2
this.st_1=create st_1
this.em_confirmation=create em_confirmation
this.cb_confirmation=create cb_confirmation
this.dw_reports_operation=create dw_reports_operation
this.dw_reports=create dw_reports
this.dw_reports_employer=create dw_reports_employer
this.Control[]={this.cb_2,&
this.st_1,&
this.em_confirmation,&
this.cb_confirmation,&
this.dw_reports_operation,&
this.dw_reports,&
this.dw_reports_employer}
end on

on tabpage_67_reports.destroy
destroy(this.cb_2)
destroy(this.st_1)
destroy(this.em_confirmation)
destroy(this.cb_confirmation)
destroy(this.dw_reports_operation)
destroy(this.dw_reports)
destroy(this.dw_reports_employer)
end on

type cb_2 from commandbutton within tabpage_67_reports
integer x = 2597
integer y = 32
integer width = 517
integer height = 92
integer taborder = 20
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Filing Report"
end type

event clicked;If SQLCA.ServiceAvailable() THEN
	OpenSheet(w_electronic_form67_report,w_frame,0,Layered!)
ELSE
	Beep(2)
END IF
end event

type st_1 from statictext within tabpage_67_reports
integer x = 2601
integer y = 416
integer width = 517
integer height = 52
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
long backcolor = 67108864
string text = "( Confirmation No. )"
alignment alignment = center!
boolean focusrectangle = false
end type

type em_confirmation from editmask within tabpage_67_reports
integer x = 2601
integer y = 484
integer width = 517
integer height = 92
integer taborder = 40
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
long textcolor = 33554432
string text = "none"
alignment alignment = right!
borderstyle borderstyle = stylelowered!
string mask = "##-###-###"
end type

type cb_confirmation from commandbutton within tabpage_67_reports
integer x = 2597
integer y = 592
integer width = 517
integer height = 92
integer taborder = 30
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "MS Sans Serif"
string text = "Quick Search"
boolean default = true
end type

event clicked;LONG  		ll_confirmation_no, ll_employer_no
INTEGER		li_row, li_operation, li_pos
BOOLEAN		lb_access
STRING      ls_confirmation

ls_confirmation    = em_confirmation.text

li_pos = Pos(ls_confirmation,'-')
DO WHILE  li_pos > 0 
	ls_confirmation = Replace(ls_confirmation, li_pos, 1, '')
	li_pos = Pos(ls_confirmation,'-',li_pos )
LOOP

ll_confirmation_no = long(ls_confirmation)

IF isnull(ll_confirmation_no) OR ll_confirmation_no <= 1 THEN RETURN 

li_row = dw_reports.find("form67_confirmation_no = " + trim(em_confirmation.text),1 , dw_reports.RowCount())
	
IF li_row > 0  THEN 
	dw_reports.scrolltorow(li_row)
	dw_reports.selectrow(0,false)
	dw_reports.selectrow(li_row, true)
	RETURN 
END IF 

//can't find it for this employer operation so find the employer operation it exists under
ll_employer_no = wf_get_employer_no_from_confirmation(ll_confirmation_no)
li_operation   = wf_get_operation_no_from_confirmation(ll_confirmation_no)


IF ISNULL(ll_employer_no)OR ll_employer_no < 0 OR ISNULL(li_operation) OR li_operation < 0 THEN RETURN 

wf_reset_dws_for_employer()
wf_retrieve_dws_for_employer(ll_employer_no)

IF dw_reports_operation.rowcount() > 0  THEN 
	li_row = dw_reports_operation.find("operation_no = " + string(li_operation),1 , dw_reports_operation.RowCount())
	IF li_row > 0  THEN 
		dw_reports_operation.scrolltorow(li_row)
		dw_reports_operation.selectrow(0,false)
		dw_reports_operation.selectrow(li_row, true)
		dw_reports_operation.TRIGGEREVENT('rowfocuschanged')		
	END IF 
	
	// FIND THE CONFIRMATION 					
	li_row = dw_reports.find("form67_confirmation_no = " + ls_confirmation,1 , dw_reports.RowCount())	
	IF li_row > 0  THEN 
		
		//no find the confirmation number in this list which is only for this operation 
		li_row = dw_reports.find("form67_confirmation_no = " + ls_confirmation,1 , dw_reports.RowCount())
		
		dw_reports.scrolltorow(li_row)
		dw_reports.selectrow(0,false)
		dw_reports.selectrow(li_row, true)
	END IF 				
END IF 	

lb_access = wf_get_website_access(ll_employer_no)
IF lb_access = TRUE  THEN 
	tab_maintain.tabpage_employer.cbx_enable_access.checked = TRUE
ELSE
	tab_maintain.tabpage_employer.cbx_enable_access.checked = FALSE
END IF 
	
end event

type dw_reports_operation from u_dw_online within tabpage_67_reports
integer x = 23
integer y = 756
integer width = 3131
integer height = 404
integer taborder = 11
string dataobject = "d_operation_list"
boolean vscrollbar = true
end type

event constructor;call super::constructor;this.settransobject(sqlca)
this.uf_setselect(1)
end event

event rowfocuschanged;call super::rowfocuschanged;LONG ll_employer_no
INTEGER li_operation_no, li_row

li_row = this.getrow()

IF li_row > 0 THEN 
	ll_employer_no = this.getitemnumber(li_row,'employer_no')
	li_operation_no = this.getitemnumber(li_row,'operation_no')
	
	dw_reports.retrieve(ll_employer_no, li_operation_no)
	SQLCA.nf_handle_error("w_form67_web_access","dw_reports","retrieve(ll_employer_no, li_operation_no)")
END IF 
end event

type dw_reports from u_dw_online within tabpage_67_reports
integer x = 23
integer y = 1176
integer width = 3136
integer height = 1080
integer taborder = 11
string dataobject = "d_web_form67"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = false
end type

event constructor;call super::constructor;this.settransobject(sqlca)
this.uf_setselect(1)
end event

event doubleclicked;call super::doubleclicked;LONG    	ll_doc_id
STRING 	ls_doc_type
INTEGER 	li_document_row_number


/*	Get the number of the row that was selected Only continue if a row was selected
*/

li_document_row_number = row
IF li_document_row_number <= 0 THEN	RETURN

/*	Get the document id for selected rowView the document
*/
ll_doc_id = dw_reports.GetItemNumber(li_document_row_number,"docid")
	
/*	Get the document id for selected row View the document */	
IF uo_image_append.of_init(ll_doc_id)	<= 0 THEN RETURN
		
ls_doc_type =  uo_image_append.of_get_file_type()
		
CHOOSE CASE ls_doc_type
	/*  Imaged document */ 
	CASE 'IMA', 'TIF'
		IF uo_image_append.of_append_image(ll_doc_id) < 0 THEN	RETURN
	CASE ELSE
		iu_dw_document_path.f_manage_document(ll_doc_id,"V","NORMAL")
END CHOOSE
	
IF gb_additional_logging = TRUE THEN	
	N_OBJECTHELPER lnv_object_helper
	// write to the application log
	f_populate_app_log(gs_appname_and_handle ,lnv_object_helper.get_app_component_type(THIS), lnv_object_helper.getpath(THIS), 'doubleclicked - ' + string(  ll_doc_id ) 	+ " docid" )
END IF 
end event

event rbuttondown;M_DW_ONLINE_RMB_POPUP lm_popup

/*	 create the menu*/
lm_popup = Create m_dw_online_rmb_popup
lm_popup.mf_set_datawindow(This)
lm_popup.m_options.m_sort.visible = TRUE
lm_popup.m_options.PopMenu(w_frame.PointerX( ), w_frame.PointerY( ))
Destroy lm_popup
end event

type dw_reports_employer from u_dw_online within tabpage_67_reports
integer x = 5
integer y = 12
integer width = 2478
integer height = 656
integer taborder = 11
string dataobject = "d_employer_address"
borderstyle borderstyle = styleshadowbox!
end type

event constructor;call super::constructor;this.settransobject(sqlca)
end event

