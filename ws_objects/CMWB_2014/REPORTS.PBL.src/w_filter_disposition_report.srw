$PBExportHeader$w_filter_disposition_report.srw
forward
global type w_filter_disposition_report from window
end type
type rb_view_full_report from radiobutton within w_filter_disposition_report
end type
type dw_select_region from u_dw_online within w_filter_disposition_report
end type
type dw_select_user from u_dw_online within w_filter_disposition_report
end type
type rb_claim_manager from radiobutton within w_filter_disposition_report
end type
type rb_region from radiobutton within w_filter_disposition_report
end type
type cb_cancel from commandbutton within w_filter_disposition_report
end type
type cb_ok from commandbutton within w_filter_disposition_report
end type
type gb_select_filter from groupbox within w_filter_disposition_report
end type
end forward

global type w_filter_disposition_report from window
integer x = 1335
integer y = 688
integer width = 1678
integer height = 756
boolean titlebar = true
string title = "Filter Transaction List"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 67108864
rb_view_full_report rb_view_full_report
dw_select_region dw_select_region
dw_select_user dw_select_user
rb_claim_manager rb_claim_manager
rb_region rb_region
cb_cancel cb_cancel
cb_ok cb_ok
gb_select_filter gb_select_filter
end type
global w_filter_disposition_report w_filter_disposition_report

type variables

end variables

event open;LONG					ll_result
DATAWINDOWCHILD	lw_region, lw_user

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


/*	Set up the drop down data window for list of users. It is visibled and invisibled as
	required.
*/
	dw_select_user.SetTransObject(SQLCA)
	dw_select_user.InsertRow(0)
	dw_select_user.GetChild("user_id",lw_user)
	lw_user.SetSort("computed_user_full_name A")
	lw_user.Sort()

/*	Set up the drop down data window for list of regions. It is visibled and invisibled as
	required.
*/
	dw_select_region.SetTransObject(SQLCA)
   dw_select_region.InsertRow(0)
	dw_select_region.GetChild("admin_region_code",lw_region)
	lw_region.SetSort("admin_region_code A")
	lw_region.Sort()

end event

on w_filter_disposition_report.create
this.rb_view_full_report=create rb_view_full_report
this.dw_select_region=create dw_select_region
this.dw_select_user=create dw_select_user
this.rb_claim_manager=create rb_claim_manager
this.rb_region=create rb_region
this.cb_cancel=create cb_cancel
this.cb_ok=create cb_ok
this.gb_select_filter=create gb_select_filter
this.Control[]={this.rb_view_full_report,&
this.dw_select_region,&
this.dw_select_user,&
this.rb_claim_manager,&
this.rb_region,&
this.cb_cancel,&
this.cb_ok,&
this.gb_select_filter}
end on

on w_filter_disposition_report.destroy
destroy(this.rb_view_full_report)
destroy(this.dw_select_region)
destroy(this.dw_select_user)
destroy(this.rb_claim_manager)
destroy(this.rb_region)
destroy(this.cb_cancel)
destroy(this.cb_ok)
destroy(this.gb_select_filter)
end on

type rb_view_full_report from radiobutton within w_filter_disposition_report
integer x = 101
integer y = 144
integer width = 613
integer height = 72
integer taborder = 10
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "View Full Report"
end type

on clicked;dw_select_user.Visible = False
dw_select_region.Visible = False

end on

type dw_select_region from u_dw_online within w_filter_disposition_report
boolean visible = false
integer x = 581
integer y = 232
integer width = 914
integer height = 108
integer taborder = 30
string dataobject = "d_select_region"
boolean border = false
end type

type dw_select_user from u_dw_online within w_filter_disposition_report
boolean visible = false
integer x = 581
integer y = 336
integer width = 1006
integer height = 108
integer taborder = 50
string dataobject = "d_select_user"
boolean border = false
end type

type rb_claim_manager from radiobutton within w_filter_disposition_report
integer x = 101
integer y = 352
integer width = 613
integer height = 72
integer taborder = 40
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Claim Manager"
end type

on clicked;dw_select_user.Visible = True
dw_select_region.Visible = False

end on

type rb_region from radiobutton within w_filter_disposition_report
integer x = 101
integer y = 248
integer width = 475
integer height = 72
integer taborder = 20
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Region"
end type

on clicked;dw_select_user.Visible = False
dw_select_region.Visible = True
end on

type cb_cancel from commandbutton within w_filter_disposition_report
integer x = 736
integer y = 520
integer width = 379
integer height = 96
integer taborder = 70
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cance&l"
end type

on clicked;CloseWithReturn(w_filter_disposition_report,"Cancel")
end on

type cb_ok from commandbutton within w_filter_disposition_report
integer x = 315
integer y = 520
integer width = 379
integer height = 96
integer taborder = 60
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&OK"
end type

on clicked;STRING	ls_filter, ls_user_id, ls_region
LONG		ll_row_num

/*	Check the selected option and create the filter
*/
	IF rb_region.checked = True THEN
		IF dw_select_region.AcceptText() < 0 THEN
			Return
		END IF
		ll_row_num = dw_select_region.GetRow()
		ls_region = dw_select_region.GetItemString(ll_row_num,"admin_region_code")
		ls_filter = "(claim_admin_region_code = '" + ls_region + "')"
	ELSEIF rb_claim_manager.checked = True THEN
		IF dw_select_user.AcceptText() < 0 THEN
			Return
		END IF
		ls_user_id = dw_select_user.GetItemString(dw_select_user.GetRow(),"user_id")
		ls_filter = "(claim_claim_manager_user_id = '" +  ls_user_id + "' )"
	ELSE
		ls_filter = ""
	END IF

	CloseWithReturn(w_filter_disposition_report,ls_filter)
end on

type gb_select_filter from groupbox within w_filter_disposition_report
integer x = 64
integer y = 76
integer width = 1550
integer height = 404
integer textsize = -9
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Filter By:"
end type

