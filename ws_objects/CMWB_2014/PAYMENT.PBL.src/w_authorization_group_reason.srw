$PBExportHeader$w_authorization_group_reason.srw
forward
global type w_authorization_group_reason from window
end type
type ddlb_reasons from dropdownlistbox within w_authorization_group_reason
end type
type st_2 from statictext within w_authorization_group_reason
end type
type st_1 from statictext within w_authorization_group_reason
end type
type mle_comment from multilineedit within w_authorization_group_reason
end type
type dw_reasons from datawindow within w_authorization_group_reason
end type
type cb_2 from commandbutton within w_authorization_group_reason
end type
type cb_ok from commandbutton within w_authorization_group_reason
end type
type mle_1 from multilineedit within w_authorization_group_reason
end type
type gb_1 from groupbox within w_authorization_group_reason
end type
end forward

global type w_authorization_group_reason from window
integer width = 1769
integer height = 740
boolean titlebar = true
string title = "Untitled"
windowtype windowtype = response!
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
ddlb_reasons ddlb_reasons
st_2 st_2
st_1 st_1
mle_comment mle_comment
dw_reasons dw_reasons
cb_2 cb_2
cb_ok cb_ok
mle_1 mle_1
gb_1 gb_1
end type
global w_authorization_group_reason w_authorization_group_reason

type variables
STRING is_comment_req, is_reason_code
end variables

forward prototypes
public subroutine wf_get_reason_info (string as_reason_desc, ref string as_reason_code, ref string as_comment_required)
end prototypes

public subroutine wf_get_reason_info (string as_reason_desc, ref string as_reason_code, ref string as_comment_required);SELECT authorization_group_reason_code, comment_required
INTO    :as_reason_code, :as_comment_required
FROM   Authorization_Group_Reason
WHERE authorization_group_reason_desc = :as_reason_desc
USING  SQLCA;

SQLCA.nf_handle_error('w_authorization_group_reason','wf_get_reason_info','SELECT authorization_group_reason_code, comment_required')


end subroutine

on w_authorization_group_reason.create
this.ddlb_reasons=create ddlb_reasons
this.st_2=create st_2
this.st_1=create st_1
this.mle_comment=create mle_comment
this.dw_reasons=create dw_reasons
this.cb_2=create cb_2
this.cb_ok=create cb_ok
this.mle_1=create mle_1
this.gb_1=create gb_1
this.Control[]={this.ddlb_reasons,&
this.st_2,&
this.st_1,&
this.mle_comment,&
this.dw_reasons,&
this.cb_2,&
this.cb_ok,&
this.mle_1,&
this.gb_1}
end on

on w_authorization_group_reason.destroy
destroy(this.ddlb_reasons)
destroy(this.st_2)
destroy(this.st_1)
destroy(this.mle_comment)
destroy(this.dw_reasons)
destroy(this.cb_2)
destroy(this.cb_ok)
destroy(this.mle_1)
destroy(this.gb_1)
end on

event open;LONG ll_reasons, ll_cntr, ll_payment_no
DATAWINDOWCHILD ldwc_reasons

DATASTORE lds_reasons
lds_reasons = CREATE DATASTORE
lds_reasons.DataObject = 'd_authorization_group_reasons_active'

lds_reasons.SetTransObject(SQLCA)

ll_reasons =lds_reasons.Retrieve()

IF ll_reasons < 0 THEN
	MessageBox('No Reasons','No reasons were retrieved. Please call the HelpDesk.', Information!)
ELSE
	FOR ll_cntr = 1 to ll_reasons
		ddlb_reasons.InsertItem(lds_reasons.GetItemString(ll_cntr, 'authorization_group_reason_desc'), ll_cntr)
	NEXT
END IF

ldwc_reasons.SetItem(ldwc_reasons.GetRow(), 'authorization_group_reason_code','')
end event

type ddlb_reasons from dropdownlistbox within w_authorization_group_reason
integer x = 421
integer y = 140
integer width = 1029
integer height = 504
integer taborder = 30
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;STRING ls_reason_desc, ls_reason_comment
 
 ls_reason_desc = ddlb_reasons.Text(index)
 
 wf_get_reason_info(ls_reason_desc, is_reason_code, is_comment_req)
 

end event

type st_2 from statictext within w_authorization_group_reason
integer x = 105
integer y = 256
integer width = 274
integer height = 60
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Comment:"
boolean focusrectangle = false
end type

type st_1 from statictext within w_authorization_group_reason
integer x = 105
integer y = 164
integer width = 274
integer height = 60
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Reason:"
boolean focusrectangle = false
end type

type mle_comment from multilineedit within w_authorization_group_reason
integer x = 421
integer y = 248
integer width = 1024
integer height = 176
integer taborder = 30
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

type dw_reasons from datawindow within w_authorization_group_reason
boolean visible = false
integer x = 384
integer y = 88
integer width = 1106
integer height = 148
integer taborder = 20
string title = "none"
string dataobject = "d_authorization_group_reasons"
boolean border = false
boolean livescroll = true
end type

event itemchanged;DATAWINDOWCHILD ldwc_reasons
STRING s_reason_code

dw_reasons.GetChild('authorization_group_reason_code', ldwc_reasons)

IF ldwc_reasons.RowCount() > 0 THEN
	is_comment_req = ldwc_reasons.GetItemString(ldwc_reasons.GetRow(), 'comment_required')
END IF
end event

type cb_2 from commandbutton within w_authorization_group_reason
integer x = 905
integer y = 464
integer width = 402
integer height = 104
integer taborder = 40
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cancel"
end type

event clicked;STRING ls_null

SetNull(ls_null)

CloseWithReturn(w_authorization_group_reason, ls_null)
end event

type cb_ok from commandbutton within w_authorization_group_reason
integer x = 466
integer y = 464
integer width = 402
integer height = 104
integer taborder = 30
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "OK"
end type

event clicked;STRING  ls_reason_code, ls_comment, ls_reason_text
s_window_message lstr_reason


ls_comment = TRIM(mle_comment.Text)
IF is_comment_req = 'Y' AND ls_comment = '' THEN
	MessageBox('Comment Required','A comment is required for the selected Authorization Grouping Reason.', Information!)
	RETURN
END IF

IF IsNull(is_reason_code) OR is_reason_code = '' THEN
	MessageBox('Reason Required','You must select a reason for creating an Authorization Group.', Information!)
	RETURN
END IF

lstr_reason.as_stringparm[1] = is_reason_code
lstr_reason.as_stringparm[2] = ls_comment

CloseWithReturn(w_authorization_group_reason, lstr_reason)
end event

type mle_1 from multilineedit within w_authorization_group_reason
boolean visible = false
integer x = 530
integer y = 304
integer width = 1010
integer height = 288
integer taborder = 20
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
borderstyle borderstyle = stylelowered!
end type

type gb_1 from groupbox within w_authorization_group_reason
integer x = 37
integer y = 32
integer width = 1687
integer height = 600
integer taborder = 10
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Grouping Reason"
end type

