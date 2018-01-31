$PBExportHeader$w_formulary_report_viewer.srw
$PBExportComments$A response window showing the formulary history
forward
global type w_formulary_report_viewer from window
end type
type cb_export from commandbutton within w_formulary_report_viewer
end type
type cb_print from commandbutton within w_formulary_report_viewer
end type
type dw_report from u_dw_online within w_formulary_report_viewer
end type
type cb_1 from commandbutton within w_formulary_report_viewer
end type
end forward

global type w_formulary_report_viewer from window
integer width = 3429
integer height = 2436
boolean titlebar = true
string title = "Claim Summary Report"
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
global w_formulary_report_viewer w_formulary_report_viewer

on w_formulary_report_viewer.create
this.cb_export=create cb_export
this.cb_print=create cb_print
this.dw_report=create dw_report
this.cb_1=create cb_1
this.Control[]={this.cb_export,&
this.cb_print,&
this.dw_report,&
this.cb_1}
end on

on w_formulary_report_viewer.destroy
destroy(this.cb_export)
destroy(this.cb_print)
destroy(this.dw_report)
destroy(this.cb_1)
end on

event open;LONG ll_claim_no, ll_row
s_generate_formulary_report lstr_report
datawindow ldw_formulary, ldw_eligibility,ldw_event, ldw_rx, ldw_claim_external, ldw_sa, ldw_formulary_pres_individual
DataWindowChild dwc_formulary, dwc_rx, dwc_eligibility, dwc_event, dwc_basic_claim, dwc_sa, dwc_formulary_pres_individual
Datawindowchild dwc_claim_external
INTEGER li_counter, li_rows
W_SHEET  lw_active_sheet
STRING   ls_given, ls_last
u_ds     lds_claim_name

INT li_trancount

// transaction count must be zero when window opens, trigger application error if non-zero
SQLCA.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// transaction count must be zero when window opens, trigger application error if non-zero
ImageTrans.nf_transaction_count(li_trancount,0,THIS.ClassName(),'open event','Open transaction error',TRUE)

// write to the application log
f_populate_app_log(gs_appname,100,this.ClassName(),'open event')



//populate the local structure
lstr_report = Message.powerobjectparm

//grab the claim number from the calling window
ll_claim_no     = lstr_report.al_claim_no
ldw_formulary   = lstr_report.aw_formulary
ldw_eligibility = lstr_report.aw_eligibility
ldw_event       = lstr_report.aw_event
ldw_rx          = lstr_report.aw_rx
ldw_sa          = lstr_report.aw_sa
ldw_formulary_pres_individual = lstr_report.aw_rx_by_individual

li_rows = ldw_formulary_pres_individual.rowcount()


/* CHECK THAT THE CLAIM NUMBER IS VALID */
IF ISNULL(ll_claim_no) OR ll_claim_no < 1  THEN
	CLOSE(THIS)
	RETURN
END IF

	
/* RETRIEVE THE INFORMATION */
IF dw_report.GetChild("dw_formulary", dwc_formulary) = -1 THEN 
	MESSAGEBOX("Generate Report","Formulary Information could not be accessed."+&
	           "~rPlease try again or contact the HelpDesk for assistance")
	RETURN -1
END IF 

IF dw_report.GetChild("dw_eligibility", dwc_eligibility)  = -1 THEN 
	MESSAGEBOX("Generate Report","Eligibility Information could not be accessed."+&
	           "~rPlease try again or contact the HelpDesk for assistance")
	RETURN -1
END IF

IF dw_report.GetChild("dw_rx", dwc_rx)  = -1 THEN 
	MESSAGEBOX("Generate Report","Prescription Information could not be accessed."+&
	           "~rPlease try again or contact the HelpDesk for assistance")
	RETURN -1
END IF 

IF dw_report.GetChild("dw_formulary_pres_individual", dwc_formulary_pres_individual)  = -1 THEN 
	MESSAGEBOX("Generate Report","Prescription Information by individual could not be accessed."+&
	           "~rPlease try again or contact the HelpDesk for assistance")
	RETURN -1
END IF 

IF dw_report.GetChild("dw_event", dwc_event)  = -1 THEN 
	MESSAGEBOX("Generate Report","Event Information could not be accessed."+&
	           "~rPlease try again or contact the HelpDesk for assistance")
	RETURN -1
END IF 

IF dw_report.GetChild("dw_basic_claim", dwc_basic_claim)  = -1 THEN 
	MESSAGEBOX("Generate Report","Basic Claim Information could not be accessed."+&
	           "~rPlease try again or contact the HelpDesk for assistance")
	RETURN -1
END IF

IF dw_report.GetChild("dw_claim_name_formulary", dwc_claim_external)  = -1 THEN 
	MESSAGEBOX("Generate Report","Basic Claim Name Information could not be accessed."+&
	           "~rPlease try again or contact the HelpDesk for assistance")
	RETURN -1
END IF

IF dw_report.GetChild("dw_rx_special_auth", dwc_sa)  = -1 THEN 
	MESSAGEBOX("Generate Report","Rx Special Auth Information could not be accessed."+&
	           "~rPlease try again or contact the HelpDesk for assistance")
	RETURN -1
END IF

/* claim name information */
lds_claim_name = CREATE u_ds
lds_claim_name.DataObject = 'd_claim_name_formulary'
lds_claim_name.SetTransObject(SQLCA)
lds_claim_name.Retrieve(ll_claim_no)
SQLCA.nf_handle_error('w_formulary_report_viewer','open','lds_claim_name.Retrieve(ll_claim_no)') 
	
FOR li_counter = 1 TO lds_claim_name.rowcount()
	IF lds_claim_name.RowsCopy(li_counter, li_counter, Primary!, dwc_claim_external, 99999, Primary!) = -1 THEN
		MESSAGEBOX("Generate Report","Claim Name Information could not be Copied."+&
	           "~rPlease try again or contact the HelpDesk for assistance")
		RETURN -1
	END IF 	
NEXT
	
// for each elect row copy to appropriate datawindow
FOR li_counter = 1 TO ldw_eligibility.rowcount()
	IF ldw_eligibility.isselected(li_counter) THEN
		IF ldw_eligibility.RowsCopy(li_counter, li_counter, Primary!, dwc_eligibility, 99999, Primary!) = -1 THEN
			MESSAGEBOX("Generate Report","Eligibility Information could not be Copied."+&
	           "~rPlease try again or contact the HelpDesk for assistance")
			RETURN -1
		END IF 	
	END IF 
NEXT

// for each elect row copy to appropriate datawindow
FOR li_counter = 1 TO ldw_formulary.rowcount()
	IF ldw_formulary.isselected(li_counter) THEN
		IF ldw_formulary.RowsCopy(li_counter, li_counter, Primary!, dwc_formulary, 99999, Primary!) = -1 THEN
			MESSAGEBOX("Generate Report","Formulary Information could not be Copied."+&
	           "~rPlease try again or contact the HelpDesk for assistance")
			RETURN -1
		END IF 
	END IF 
NEXT

// for each elect row copy to appropriate datawindow
FOR li_counter = 1 TO ldw_event.rowcount()
	IF ldw_event.isselected(li_counter) THEN
		IF ldw_event.RowsCopy(li_counter, li_counter, Primary!, dwc_event, 99999, Primary!) = -1 THEN
			MESSAGEBOX("Generate Report","Event Information could not be Copied."+&
	           "~rPlease try again or contact the HelpDesk for assistance")
			RETURN -1
		END IF 
	END IF 
NEXT

// for each elect row copy to appropriate datawindow
FOR li_counter = 1 TO ldw_rx.rowcount()
	IF ldw_rx.isselected(li_counter) THEN
		IF ldw_rx.RowsCopy(li_counter, li_counter, Primary!, dwc_rx, 99999, Primary!)	= -1 THEN
			MESSAGEBOX("Generate Report","Prescription Information could not be Copied."+&
	           "~rPlease try again or contact the HelpDesk for assistance")
			RETURN -1
		END IF 
	END IF 
NEXT

// for each elect row copy to appropriate datawindow
FOR li_counter = 1 TO ldw_formulary_pres_individual.rowcount()
	IF ldw_formulary_pres_individual.isselected(li_counter) THEN
		IF ldw_formulary_pres_individual.RowsCopy(li_counter, li_counter, Primary!, dwc_formulary_pres_individual, 99999, Primary!)	= -1 THEN
			MESSAGEBOX("Generate Report","Prescription Information could not be Copied."+&
	           "~rPlease try again or contact the HelpDesk for assistance")
			RETURN -1
		END IF 
	END IF 
NEXT

// for each elect row copy to appropriate datawindow
FOR li_counter = 1 TO ldw_sa.rowcount()
	IF ldw_sa.isselected(li_counter) THEN
		IF ldw_sa.RowsCopy(li_counter, li_counter, Primary!, dwc_sa, 99999, Primary!)	= -1 THEN
			MESSAGEBOX("Generate Report","Special Authorization Information could not be Copied."+&
	           "~rPlease try again or contact the HelpDesk for assistance")
			RETURN -1
		END IF 
	END IF 
NEXT

/* grab the claim number and set it to an instance variable */
lw_active_sheet = w_frame.GetActiveSheet()

FOR li_counter = 1 TO lw_active_sheet.dw_basic_claim.rowcount()
	IF lw_active_sheet.dw_basic_claim.RowsCopy(li_counter, li_counter, Primary!, dwc_basic_claim, 99999, Primary!)	= -1 THEN
		MESSAGEBOX("Generate Report","Basic Claim Information Information could not be Copied."+&
	           "~rPlease try again or contact the HelpDesk for assistance")
		RETURN -1
	END IF 
NEXT

/* if you dont do this eligibility will not show up correctly */
dw_report.Object.DataWindow.Print.preview = "YES"








end event

event resize;dw_report.width = This.width - 90
dw_report.height = This.height - 280
end event

type cb_export from commandbutton within w_formulary_report_viewer
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

type cb_print from commandbutton within w_formulary_report_viewer
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

event clicked;dw_report.print()
end event

type dw_report from u_dw_online within w_formulary_report_viewer
integer x = 14
integer y = 116
integer width = 3337
integer height = 2104
integer taborder = 10
string dataobject = "d_generate_report_formulary"
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean border = false
boolean livescroll = false
end type

type cb_1 from commandbutton within w_formulary_report_viewer
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

