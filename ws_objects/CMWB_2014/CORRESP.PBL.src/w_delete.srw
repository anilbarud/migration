$PBExportHeader$w_delete.srw
$PBExportComments$Deletes a selected correspondence already generated
forward
global type w_delete from window
end type
type st_comments_d from statictext within w_delete
end type
type st_no_d from statictext within w_delete
end type
type st_type_d from statictext within w_delete
end type
type cb_cancel from commandbutton within w_delete
end type
type cb_ok from commandbutton within w_delete
end type
type st_1 from statictext within w_delete
end type
type st_comments from statictext within w_delete
end type
type st_type from statictext within w_delete
end type
type st_no from statictext within w_delete
end type
type gb_identification from groupbox within w_delete
end type
end forward

global type w_delete from window
integer x = 82
integer y = 716
integer width = 1746
integer height = 712
boolean titlebar = true
string title = "DELETE CONFIRMATION"
boolean controlmenu = true
windowtype windowtype = child!
long backcolor = 67108864
string icon = "None!"
st_comments_d st_comments_d
st_no_d st_no_d
st_type_d st_type_d
cb_cancel cb_cancel
cb_ok cb_ok
st_1 st_1
st_comments st_comments
st_type st_type
st_no st_no
gb_identification gb_identification
end type
global w_delete w_delete

type variables
s_correspond_claim   vistr_correspond_claim
int	vii_return_code
w_correspond	viw_correspond
end variables

on closequery;If ISVALID(viw_correspond) THEN 
	viw_correspond.postevent("correspondactivate")
	viw_correspond.wf_reset_buttons(True, "")
	viw_correspond.cb_delete.SetFocus()
ELSE
	MESSAGEBOX(title,"Updates were saved but was unable to display results"&
 					+"~n~r due to invalid window object - W_Correspond"&
					+"~n~r Please inform the helpdesk of this message!",Information!)
END IF

end on

event open;// ************************************************************************************************
// AUTHOR:	MW George
// Modified:	94.10.04

// ************************************************************************************************
long		ll_row_nbr
string	physical_file_path 

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


vistr_correspond_claim	= Message. PowerObjectParm
viw_correspond = vistr_correspond_claim.parent_window


st_no_d.text				= string(vistr_correspond_claim.corr.corr_no)
st_type_d.text				= vistr_correspond_claim.corr.template_type
st_comments_d.text		= vistr_correspond_claim.corr.comments
	
Beep(1)

end event

on w_delete.create
this.st_comments_d=create st_comments_d
this.st_no_d=create st_no_d
this.st_type_d=create st_type_d
this.cb_cancel=create cb_cancel
this.cb_ok=create cb_ok
this.st_1=create st_1
this.st_comments=create st_comments
this.st_type=create st_type
this.st_no=create st_no
this.gb_identification=create gb_identification
this.Control[]={this.st_comments_d,&
this.st_no_d,&
this.st_type_d,&
this.cb_cancel,&
this.cb_ok,&
this.st_1,&
this.st_comments,&
this.st_type,&
this.st_no,&
this.gb_identification}
end on

on w_delete.destroy
destroy(this.st_comments_d)
destroy(this.st_no_d)
destroy(this.st_type_d)
destroy(this.cb_cancel)
destroy(this.cb_ok)
destroy(this.st_1)
destroy(this.st_comments)
destroy(this.st_type)
destroy(this.st_no)
destroy(this.gb_identification)
end on

type st_comments_d from statictext within w_delete
integer x = 384
integer y = 224
integer width = 1262
integer height = 80
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
boolean border = true
boolean focusrectangle = false
end type

type st_no_d from statictext within w_delete
integer x = 1481
integer y = 112
integer width = 165
integer height = 80
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
boolean border = true
boolean focusrectangle = false
end type

type st_type_d from statictext within w_delete
integer x = 384
integer y = 112
integer width = 384
integer height = 80
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
boolean border = true
boolean focusrectangle = false
end type

type cb_cancel from commandbutton within w_delete
integer x = 905
integer y = 504
integer width = 384
integer height = 96
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cance&l"
boolean cancel = true
boolean default = true
end type

on clicked;// ************************************************************************************************
// AUTHOR:	MW George
// Modified:	94.09.26

// ************************************************************************************************
// CLOSE THIS WINDOW WITHOUT DELETING THE CORRESPONDENCE

close (parent)
end on

type cb_ok from commandbutton within w_delete
integer x = 357
integer y = 504
integer width = 384
integer height = 96
integer taborder = 10
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
end type

event clicked;// ************************************************************************************************
// AUTHOR:	MW George
// Modified:	94.10.10

// ************************************************************************************************
// DECLARATIONS

long	rc
date	tdate

tdate = today()
// Next line added by Earl Assoon on FEB 16, 1995 to avoid double clicking of OK button
cb_ok.enabled = FALSE
//End of changes by Earl on FEB 16
// ************************************************************************************************
// DELETE THIS CORRESPONDENCE BY SETTING THE "DELETED_DATE"

SQLCA.nf_begin_transaction()

UPDATE	CORRESPONDENCE
SET			deleted_date		= :tdate,
			deleted_by_user_id	= :vgst_user_profile.user_id,
			correspond_status_code	= 'D'
WHERE		(claim_no		= :vistr_correspond_claim.claim_no)	AND
			(correspond_no	= :vistr_correspond_claim.corr.corr_no)
USING		SQLCA;

// expect one record to be updated
SQLCA.nf_handle_error('w_delete','embedded SQL: UPDATE CORRESPONDENCE','cb_ok.clicked',1)

SQLCA.nf_commit_transaction()

close(parent)
end event

type st_1 from statictext within w_delete
integer x = 137
integer y = 408
integer width = 1467
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "ARE YOU SURE YOU WANT TO DELETE THIS DOCUMENT?"
boolean focusrectangle = false
end type

type st_comments from statictext within w_delete
integer x = 55
integer y = 224
integer width = 311
integer height = 72
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Comments:"
boolean focusrectangle = false
end type

type st_type from statictext within w_delete
integer x = 55
integer y = 112
integer width = 165
integer height = 64
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Type:"
boolean focusrectangle = false
end type

type st_no from statictext within w_delete
integer x = 1262
integer y = 112
integer width = 128
integer height = 64
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "No:"
boolean focusrectangle = false
end type

type gb_identification from groupbox within w_delete
integer x = 27
integer y = 24
integer width = 1673
integer height = 360
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Identification"
end type

