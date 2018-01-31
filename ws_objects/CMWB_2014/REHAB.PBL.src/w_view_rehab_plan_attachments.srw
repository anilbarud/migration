$PBExportHeader$w_view_rehab_plan_attachments.srw
forward
global type w_view_rehab_plan_attachments from w_ancestor
end type
type cb_ok from commandbutton within w_view_rehab_plan_attachments
end type
type cb_cancel from commandbutton within w_view_rehab_plan_attachments
end type
type dw_rehab_plan_view_attachments from u_dw_online within w_view_rehab_plan_attachments
end type
end forward

global type w_view_rehab_plan_attachments from w_ancestor
int X=993
int Y=541
int Height=1297
WindowType WindowType=response!
boolean TitleBar=true
string Title="View Rehab Plan Attachments"
string MenuName=""
long BackColor=67108864
boolean ControlMenu=false
boolean MinBox=false
boolean MaxBox=false
boolean Resizable=false
cb_ok cb_ok
cb_cancel cb_cancel
dw_rehab_plan_view_attachments dw_rehab_plan_view_attachments
end type
global w_view_rehab_plan_attachments w_view_rehab_plan_attachments

type variables
LONG il_provider_no 
STRING is_provider_type_code
end variables

event open;call super::open;/*	Get the retrieval arguments out of the Message object and then 
	retrieve all the attachments for the task.
*/
	S_WINDOW_MESSAGE	lstr_message
	LONG					ll_claim_no, ll_task_no

	
	dw_rehab_plan_view_attachments.SetTransObject(SQLCA)
	dw_rehab_plan_view_attachments.uf_setselect(1)
	
	lstr_message = Message.PowerObjectParm
	ll_claim_no = lstr_message.al_doubleparm[1]
	ll_task_no = lstr_message.al_doubleparm[2]
	
	dw_rehab_plan_view_attachments.Retrieve(ll_claim_no,ll_task_no)
	
	IF SQLCA.nf_Handle_Error("w_view_rehab_plan_attachments","dw_rehab_plan_view_attachments.Retrieve(ll_claim_no,ll_task_no)","Open Event")	< 0 THEN
		lstr_message.al_doubleparm[3] = -1
		CloseWithReturn(THIS,lstr_message)
	END IF
	
end event

on w_view_rehab_plan_attachments.create
int iCurrent
call w_ancestor::create
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
this.dw_rehab_plan_view_attachments=create dw_rehab_plan_view_attachments
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=cb_ok
this.Control[iCurrent+2]=cb_cancel
this.Control[iCurrent+3]=dw_rehab_plan_view_attachments
end on

on w_view_rehab_plan_attachments.destroy
call w_ancestor::destroy
destroy(this.cb_ok)
destroy(this.cb_cancel)
destroy(this.dw_rehab_plan_view_attachments)
end on

type cb_ok from commandbutton within w_view_rehab_plan_attachments
int X=1075
int Y=1061
int Width=275
int Height=109
int TabOrder=10
string Text="&OK"
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

event clicked;/*	Return the docid for the currently selected row.
*/
	S_WINDOW_MESSAGE	lstr_message
	LONG					ll_current_row, ll_docid

	ll_current_row = dw_rehab_plan_view_attachments.GetRow()
	IF ll_current_row > 0 THEN
		ll_docid = dw_rehab_plan_view_attachments.GetItemNumber(ll_current_row,'docid')
	ELSE
		ll_docid = -1
	END IF
	lstr_message.al_doubleparm[3] = ll_docid
	CloseWithReturn(PARENT,lstr_message)

end event

type cb_cancel from commandbutton within w_view_rehab_plan_attachments
int X=1354
int Y=1061
int Width=275
int Height=109
int TabOrder=20
string Text="Cance&l"
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

event clicked;/*	User chose to cancel, return a 0.
*/
	S_WINDOW_MESSAGE lstr_message

	lstr_message.al_doubleparm[3] = 0
	CloseWithReturn(PARENT,lstr_message)

end event

type dw_rehab_plan_view_attachments from u_dw_online within w_view_rehab_plan_attachments
int X=33
int Y=33
int Width=2707
int Height=965
string DataObject="d_rehab_plan_view_attachments"
boolean VScrollBar=true
end type

event rowfocuschanged;call super::rowfocuschanged;/*	Determine the current row and highlight it.
*/
	LONG	ll_row
	
	ll_row = THIS.GetRow()

	IF ll_row > 0 THEN
		uf_processselect(ll_row,"Mouse")
	END IF
	
end event

