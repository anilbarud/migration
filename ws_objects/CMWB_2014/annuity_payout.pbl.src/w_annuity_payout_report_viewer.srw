$PBExportHeader$w_annuity_payout_report_viewer.srw
$PBExportComments$A response window showing the formulary history
forward
global type w_annuity_payout_report_viewer from window
end type
type cb_export from commandbutton within w_annuity_payout_report_viewer
end type
type cb_print from commandbutton within w_annuity_payout_report_viewer
end type
type dw_report from u_dw_online within w_annuity_payout_report_viewer
end type
type cb_1 from commandbutton within w_annuity_payout_report_viewer
end type
end forward

global type w_annuity_payout_report_viewer from window
integer width = 5010
integer height = 2868
boolean titlebar = true
string title = "Annuity Payout Report"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
windowtype windowtype = popup!
long backcolor = 67108864
string icon = "AppIcon!"
boolean center = true
cb_export cb_export
cb_print cb_print
dw_report dw_report
cb_1 cb_1
end type
global w_annuity_payout_report_viewer w_annuity_payout_report_viewer

forward prototypes
public function window wf_get_window_reference ()
end prototypes

public function window wf_get_window_reference ();RETURN THIS
end function

on w_annuity_payout_report_viewer.create
this.cb_export=create cb_export
this.cb_print=create cb_print
this.dw_report=create dw_report
this.cb_1=create cb_1
this.Control[]={this.cb_export,&
this.cb_print,&
this.dw_report,&
this.cb_1}
end on

on w_annuity_payout_report_viewer.destroy
destroy(this.cb_export)
destroy(this.cb_print)
destroy(this.dw_report)
destroy(this.cb_1)
end on

event open;long  ll_individual_no, ll_annuity_payout_no, ll_contract_no, ll_rows
string ls_has_dependants

datawindowchild ldw_payout_summary_child, ldw_annuity_contract_child

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')


S_WINDOW_MESSAGE          lstr_message
lstr_message = MESSAGE.PowerObjectParm

ll_individual_no = lstr_message.al_doubleparm[2]               // il_individual_no
ll_annuity_payout_no = lstr_message.al_doubleparm[3]      // il_annuity_payout_no
ls_has_dependants = lstr_message.as_stringparm[1]         // used to determine which dataobject should be plugged in for the claim txn details portion of the report


IF ls_has_dependants = 'has dependants' THEN
	dw_report.Object.dw_annuity_payout_txn_detail.DataObject = 'd_ap_txn_detail_w_dependants_comp'
ELSE
	dw_report.Object.dw_annuity_payout_txn_detail.DataObject = 'd_annuity_payout_txn_detail_no_dependants'
END IF

dw_report.SetTransObject(SQLCA)

dw_report.Object.DataWindow.Print.preview = "YES"

dw_report.Retrieve(ll_individual_no,ll_annuity_payout_no,vgst_user_profile.default_admin_region_code,'inquiry')
SQLCA.nf_handle_error('w_annuity_payout_report_viewer', 'dw_report', 'open event')

dw_report.getChild('dw_payout_summary', ldw_payout_summary_child)
ldw_payout_summary_child.setFilter('annuity_payout_no = ' + STRING(ll_annuity_payout_no))

ldw_payout_summary_child.filter()
//
//dw_report.getChild('dw_contract_beneficiaries', ldw_annuity_contract_child)
//IF ldw_annuity_contract_child.rowCount() > 0 THEN
//	ll_contract_no = LONG( dw_report.Object.dw_contract_beneficiaries.Object.annuity_contract_no[1])		
//	IF ll_contract_no > 0 THEN
//		dw_report.getChild('dw_contract_beneficiaries_details', ldw_annuity_contract_child)
//		ldw_annuity_contract_child.setTransObject(SQLCA)
//		ldw_annuity_contract_child.retrieve(ll_contract_no)
//		dw_report.Object.dw_contract_beneficiaries_details[1].Object.r_1.visible = 0			
//	END IF
//ELSE
//	//dw_report.Object.dw_page_break_contract_beneficiaries.Object.title_t[1] = 'No Contracts and Benficiaries'
//END IF


dw_report.groupCalc()




end event

event resize;dw_report.width = This.width - 90
dw_report.height = This.height - 280
end event

type cb_export from commandbutton within w_annuity_payout_report_viewer
boolean visible = false
integer x = 544
integer y = 12
integer width = 265
integer height = 84
integer taborder = 30
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Export"
end type

event clicked;string ls_richtext

//ls_richtext = dw_report.CopyRTF(true)
//ll_numchars = rte_report.text = ls_richtext

//messagebox("",ls_richtext)
dw_report.saveas()


//dw_report.saveas("",EXCEL!,TRUE)
//dw_report.saveas("",EXCEL!,FALSE)
//dw_report.saveas("",TEXT!,TRUE)
//dw_report.saveas("",TEXT!,FALSE)

/* none of these work */
end event

type cb_print from commandbutton within w_annuity_payout_report_viewer
integer x = 27
integer y = 12
integer width = 256
integer height = 84
integer taborder = 30
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Print"
end type

event clicked;
dw_report.Object.DataWindow.Print.Orientation = 1
PrintSetup( )
dw_report.Print()

end event

type dw_report from u_dw_online within w_annuity_payout_report_viewer
integer x = 14
integer y = 116
integer width = 4910
integer height = 2628
integer taborder = 10
string dataobject = "d_annuity_summary_print_report_composite"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean border = false
boolean livescroll = false
end type

event rbuttondown;M_DW_RMB_POPUP lm_popup
window		lw_window

IF IsValid(inv_rowselection) Then
	inv_rowselection.event pfc_rbuttondown(xpos,ypos,row,dwo)
End if


/*	
	Create the menu -	Note that this only gives default options.  If 
							you want additional options, you should override
							the ancestor and visible the options you desire.
*/
lm_popup = Create m_dw_rmb_popup
lm_popup.mf_set_datawindow(This)

// don't display lines
lm_popup.m_options.m_1.Visible = FALSE
lm_popup.m_options.m_2.Visible = FALSE

lw_window = wf_get_window_reference()	

lm_popup.m_options.PopMenu(lw_window.PointerX( ), lw_window.PointerY( ))

Destroy lm_popup
end event

type cb_1 from commandbutton within w_annuity_payout_report_viewer
integer x = 288
integer y = 12
integer width = 256
integer height = 84
integer taborder = 20
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "&Close"
end type

event clicked;CLOSE(PARENT)
end event

