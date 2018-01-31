$PBExportHeader$w_select_annuity_payout_type.srw
$PBExportComments$select injured worker or surviving spouse from main menu based on DD criteria
forward
global type w_select_annuity_payout_type from window
end type
type cb_cancel from commandbutton within w_select_annuity_payout_type
end type
type st_3 from statictext within w_select_annuity_payout_type
end type
type st_2 from statictext within w_select_annuity_payout_type
end type
type st_1 from statictext within w_select_annuity_payout_type
end type
type cb_annuity_purchase from commandbutton within w_select_annuity_payout_type
end type
type cb_lump_sum from commandbutton within w_select_annuity_payout_type
end type
type st_4 from statictext within w_select_annuity_payout_type
end type
end forward

global type w_select_annuity_payout_type from window
integer x = 1335
integer y = 688
integer width = 2501
integer height = 1192
boolean titlebar = true
string title = "Select Annuity Payout Type"
windowtype windowtype = response!
long backcolor = 67108864
cb_cancel cb_cancel
st_3 st_3
st_2 st_2
st_1 st_1
cb_annuity_purchase cb_annuity_purchase
cb_lump_sum cb_lump_sum
st_4 st_4
end type
global w_select_annuity_payout_type w_select_annuity_payout_type

type variables
S_WINDOW_MESSAGE istr_message
end variables

on w_select_annuity_payout_type.create
this.cb_cancel=create cb_cancel
this.st_3=create st_3
this.st_2=create st_2
this.st_1=create st_1
this.cb_annuity_purchase=create cb_annuity_purchase
this.cb_lump_sum=create cb_lump_sum
this.st_4=create st_4
this.Control[]={this.cb_cancel,&
this.st_3,&
this.st_2,&
this.st_1,&
this.cb_annuity_purchase,&
this.cb_lump_sum,&
this.st_4}
end on

on w_select_annuity_payout_type.destroy
destroy(this.cb_cancel)
destroy(this.st_3)
destroy(this.st_2)
destroy(this.st_1)
destroy(this.cb_annuity_purchase)
destroy(this.cb_lump_sum)
destroy(this.st_4)
end on

event open;w_prepare_annuity_account lw_paa

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


istr_message = Message.PowerObjectParm

lw_paa = istr_message.apo_powerobjectparm[1]


// center the window
THIS.x = lw_paa.WorkSpaceWidth()/2 - THIS.Width/2
THIS.y = lw_paa.WorkSpaceHeight()/2 - THIS.Height/2

end event

type cb_cancel from commandbutton within w_select_annuity_payout_type
integer x = 1513
integer y = 948
integer width = 402
integer height = 104
integer taborder = 10
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cance&l"
boolean default = true
end type

event clicked;istr_message.as_stringparm[1] = 'CANCEL'

CloseWithReturn(PARENT,istr_message)
end event

type st_3 from statictext within w_select_annuity_payout_type
integer x = 183
integer y = 508
integer width = 2075
integer height = 164
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
string text = "Do you want to issue the annuity benefits in a Lump Sum payment or as an Annuity Purchase?"
boolean focusrectangle = false
end type

type st_2 from statictext within w_select_annuity_payout_type
integer x = 183
integer y = 320
integer width = 2075
integer height = 164
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
string text = "If a Lump Sum payment is required, it must be approved by a Regional Director."
boolean focusrectangle = false
end type

type st_1 from statictext within w_select_annuity_payout_type
integer x = 183
integer y = 192
integer width = 2075
integer height = 84
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
string text = "Annuity benefit amount requires an Annuity Purchase."
boolean focusrectangle = false
end type

type cb_annuity_purchase from commandbutton within w_select_annuity_payout_type
integer x = 992
integer y = 948
integer width = 398
integer height = 104
integer taborder = 70
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Purchase"
end type

event clicked;istr_message.as_stringparm[1] = 'PURCHASE'

CloseWithReturn(PARENT,istr_message)
end event

type cb_lump_sum from commandbutton within w_select_annuity_payout_type
integer x = 489
integer y = 948
integer width = 379
integer height = 104
integer taborder = 60
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Lump &Sum"
end type

event clicked;istr_message.as_stringparm[1] = 'LUMP'

CloseWithReturn(PARENT,istr_message)

end event

type st_4 from statictext within w_select_annuity_payout_type
integer width = 2505
integer height = 896
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
boolean border = true
boolean focusrectangle = false
end type

