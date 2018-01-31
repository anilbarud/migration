$PBExportHeader$w_reassign_therapist.srw
forward
global type w_reassign_therapist from window
end type
type st_2 from statictext within w_reassign_therapist
end type
type st_1 from statictext within w_reassign_therapist
end type
type dw_license_info from datawindow within w_reassign_therapist
end type
type cb_accept from commandbutton within w_reassign_therapist
end type
type dw_info from datawindow within w_reassign_therapist
end type
type cb_close from commandbutton within w_reassign_therapist
end type
end forward

global type w_reassign_therapist from window
integer x = 2002
integer y = 1000
integer width = 3936
integer height = 1520
boolean titlebar = true
string title = "Re-assign License"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 67108864
string icon = "AppIcon!"
boolean clientedge = true
st_2 st_2
st_1 st_1
dw_license_info dw_license_info
cb_accept cb_accept
dw_info dw_info
cb_close cb_close
end type
global w_reassign_therapist w_reassign_therapist

type variables
s_window_message istr_window_message

ULONG		iul_handle

end variables

event open;STRING			ls_message, ls_license_no, ls_license_prov_state_code, ls_license_type_code, ls_token_id
INT 				li_trancount
LONG				ll_principal_id

/*
INTEGER li_count
	
SELECT count(*)
INTO   :li_count
FROM   THERAPIST_LICENSE          a
JOIN   THERAPIST                  b ON a.therapist_no     = b.therapist_no 
JOIN   v_WIF_CUSTOM_PRINCIPAL_ALL x ON b.wif_principal_id = x.wif_principal_id
WHERE  a.license_no              = :as_license_no
AND    a.license_prov_state_code = :as_license_prov_state_code
AND    a.license_type_code       = :as_license_type_code
AND    NOT EXISTS ( SELECT * 
                    FROM WIF_CUSTOM_PRINCIPAL c WHERE c.wif_principal_id = b.wif_principal_id ) 
USING SQLCA;

*/

s_window_message 	lstr_message 


// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')

/*	grab the passed value into the datastore */
lstr_message= Message.PowerObjectParm

ll_principal_id 				= lstr_message.al_doubleparm[1]
ls_license_no 					= lstr_message.as_stringparm[1]
ls_license_type_code 		= lstr_message.as_stringparm[2]
ls_license_prov_state_code = lstr_message.as_stringparm[3]

/*
lstr_message.al_doubleparm[1] = ll_wif_principal_id
lstr_message.as_stringparm[1] = ls_license_no
lstr_message.as_stringparm[2] = ls_license_type_code
lstr_message.as_stringparm[3] = ls_license_prov_state_code
*/

dw_info.retrieve(ll_principal_id)
SQLCA.nf_handle_error("w_reassign_therapist","open","dw_info.retrieve(ll_principal_id)")

li_trancount = dw_license_info.retrieve(ls_license_no, ls_license_prov_state_code, ls_license_type_code)
SQLCA.nf_handle_error("w_reassign_therapist","open","dw_license_info.retrieve(ll_principal_id)")





end event

on w_reassign_therapist.create
this.st_2=create st_2
this.st_1=create st_1
this.dw_license_info=create dw_license_info
this.cb_accept=create cb_accept
this.dw_info=create dw_info
this.cb_close=create cb_close
this.Control[]={this.st_2,&
this.st_1,&
this.dw_license_info,&
this.cb_accept,&
this.dw_info,&
this.cb_close}
end on

on w_reassign_therapist.destroy
destroy(this.st_2)
destroy(this.st_1)
destroy(this.dw_license_info)
destroy(this.cb_accept)
destroy(this.dw_info)
destroy(this.cb_close)
end on

type st_2 from statictext within w_reassign_therapist
integer x = 27
integer y = 552
integer width = 1170
integer height = 72
integer textsize = -11
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 134217857
long backcolor = 67108864
string text = "Matches found on License number  "
boolean focusrectangle = false
end type

type st_1 from statictext within w_reassign_therapist
integer x = 14
integer y = 16
integer width = 722
integer height = 72
integer textsize = -11
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 134217857
long backcolor = 67108864
string text = "License to Re-assign"
boolean focusrectangle = false
end type

type dw_license_info from datawindow within w_reassign_therapist
integer x = 14
integer y = 640
integer width = 3867
integer height = 616
integer taborder = 20
string title = "none"
string dataobject = "d_therapist_license_reassign"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event constructor;this.settransobject(sqlca)
end event

type cb_accept from commandbutton within w_reassign_therapist
integer x = 1637
integer y = 1304
integer width = 645
integer height = 104
integer taborder = 30
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Accept Re-assignment"
end type

event clicked;/* -*****************************************************************************************
--  - Re-assign Therapist Info. to Physio's New Account
--  ===========================================================
*/
LONG		ll_wif_principal_id, ll_therapist_no
STRING	ls_therapist, ls_license_type_code, ls_license_prov_state_code,  ls_userid, ls_license_no
INTEGER	li_row
DATE		ldt_decision_date

// basic checks
IF dw_license_info.rowcount() < 1  THEN RETURN 

li_row = dw_license_info.getrow()

IF isnull(li_row) OR li_row < 1 THEN RETURN -1

ldt_decision_date = Date(f_server_datetime())
ls_userid         = vgst_user_profile.user_id

ll_wif_principal_id 				= dw_info.getitemnumber(dw_info.getrow(), 'wif_principal_id')
ll_therapist_no 					= dw_license_info.getitemnumber(li_row, 'therapist_no')
ls_license_no 						= 	dw_license_info.getitemstring(li_row ,'license_no')
ls_license_type_code				=	dw_license_info.getitemstring(li_row ,'license_type_code')
ls_license_prov_state_code		=	dw_license_info.getitemstring(li_row ,'license_prov_state_code')

IF isnull(ll_wif_principal_id) OR ll_wif_principal_id < 1 THEN RETURN -1
IF isnull(ll_therapist_no)     OR ll_therapist_no     < 1 THEN RETURN -1

/* updates and commits */
SQLCA.nf_begin_transaction()

// Link  new WIF CUSTOM PRINCIPAL ID  to  THERAPIST record
UPDATE THERAPIST
SET    wif_principal_id =  :ll_wif_principal_id  
WHERE  therapist_no 	   = 	:ll_therapist_no
USING  SQLCA;
SQLCA.nf_handle_error("w_reassign_therapist", "cb_accept - clicked", "UPDATE THERAPIST")


// update the token information as approved
/* WORKFLOW_THERAPIST_APPROVAL - UPDATE  */
/* 4.50		The decision date must be set to the current date if a therapist is approved. */
/* 4.100 	The workflow token must be marked as used if the request for approval has been approved. */

dw_info.setitem(1, 'approval_status_code', '1') 
dw_info.setitem(1, 'decision_date', Date(f_server_datetime()) )
dw_info.setitem(1, 'approver_rejecter_user_id', vgst_user_profile.user_id)
dw_info.update()

SQLCA.nf_handle_error("w_reassign_therapist", "cb_accept - clicked", "dw_info.update()")

SQLCA.nf_commit_transaction()

messagebox('Great Job!', 'Reassignment Complete!')

cb_close.triggerevent('clicked!')


end event

type dw_info from datawindow within w_reassign_therapist
integer x = 14
integer y = 104
integer width = 3877
integer height = 404
integer taborder = 10
string title = "none"
string dataobject = "d_wif_account_list"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
end type

event constructor;this.settransobject(sqlca)
end event

type cb_close from commandbutton within w_reassign_therapist
integer x = 3589
integer y = 1304
integer width = 283
integer height = 84
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Close"
boolean default = true
end type

event clicked;close(parent)



end event

event rowfocuschanged;call super::rowfocuschanged;//MAKE SURE IT IS A valid row.

end event

event rbuttondown;return 1
end event

