$PBExportHeader$w_adjustments.srw
forward
global type w_adjustments from w_ancestor
end type
type dw_more_info from u_dw_online within w_adjustments
end type
type cb_ok from commandbutton within w_adjustments
end type
end forward

global type w_adjustments from w_ancestor
integer x = 722
integer y = 916
integer width = 1577
integer height = 1172
string title = "Adjustments "
string menuname = ""
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
long backcolor = 67108864
dw_more_info dw_more_info
cb_ok cb_ok
end type
global w_adjustments w_adjustments

on open;call w_ancestor::open;LONG ll_payment_no, ll_rows

ll_payment_no = Message.DoubleParm

dw_more_info.SetTransObject(SQLCA)

ll_rows = dw_more_info.Retrieve(ll_payment_no)
IF ll_rows <= 0 THEN
   MessageBox('Warning', 'No adjustmenst to display.')
	Close(This)
	Return
END IF

end on

on w_adjustments.create
int iCurrent
call super::create
this.dw_more_info=create dw_more_info
this.cb_ok=create cb_ok
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_more_info
this.Control[iCurrent+2]=this.cb_ok
end on

on w_adjustments.destroy
call super::destroy
destroy(this.dw_more_info)
destroy(this.cb_ok)
end on

type dw_more_info from u_dw_online within w_adjustments
integer x = 14
integer y = 8
integer width = 1554
integer height = 932
integer taborder = 10
string dataobject = "d_display_adjustments"
boolean border = false
end type

type cb_ok from commandbutton within w_adjustments
integer x = 1225
integer y = 948
integer width = 247
integer height = 92
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
end type

on clicked;SetPointer(HourGlass!)
Close(Parent)
end on

