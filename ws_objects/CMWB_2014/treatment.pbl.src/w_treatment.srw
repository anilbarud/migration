$PBExportHeader$w_treatment.srw
forward
global type w_treatment from w_a_tool
end type
type dw_invoice_details from u_dw_online within w_treatment
end type
type dw_treatment_details from u_dw_online within w_treatment
end type
type dw_claim_task from u_dw_online within w_treatment
end type
type dw_pa_score from u_dw_online within w_treatment
end type
type dw_missed_appointments from u_dw_online within w_treatment
end type
type uo_image_append from u_image_append within w_treatment
end type
type dw_foq from u_dwa within w_treatment
end type
end forward

global type w_treatment from w_a_tool
integer width = 3205
integer height = 1912
string title = ""
boolean resizable = false
boolean righttoleft = true
boolean clientedge = true
long il_design_time_height = 1824
long il_design_time_width = 2674
dw_invoice_details dw_invoice_details
dw_treatment_details dw_treatment_details
dw_claim_task dw_claim_task
dw_pa_score dw_pa_score
dw_missed_appointments dw_missed_appointments
uo_image_append uo_image_append
dw_foq dw_foq
end type
global w_treatment w_treatment

type variables
s_window_message		istr_window_message
w_sheet						iw_sheet

n_resize_splitter			inv_resize_splitter
n_filter						inv_filter
n_treatment				inv_treatment

long il_claim_no

U_DWA    idw_dw[]
u_dw_document_path 	iu_dw_document_path

ULONG						iul_handle
end variables

on w_treatment.create
int iCurrent
call super::create
this.dw_invoice_details=create dw_invoice_details
this.dw_treatment_details=create dw_treatment_details
this.dw_claim_task=create dw_claim_task
this.dw_pa_score=create dw_pa_score
this.dw_missed_appointments=create dw_missed_appointments
this.uo_image_append=create uo_image_append
this.dw_foq=create dw_foq
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_invoice_details
this.Control[iCurrent+2]=this.dw_treatment_details
this.Control[iCurrent+3]=this.dw_claim_task
this.Control[iCurrent+4]=this.dw_pa_score
this.Control[iCurrent+5]=this.dw_missed_appointments
this.Control[iCurrent+6]=this.uo_image_append
this.Control[iCurrent+7]=this.dw_foq
end on

on w_treatment.destroy
call super::destroy
destroy(this.dw_invoice_details)
destroy(this.dw_treatment_details)
destroy(this.dw_claim_task)
destroy(this.dw_pa_score)
destroy(this.dw_missed_appointments)
destroy(this.uo_image_append)
destroy(this.dw_foq)
end on

event open;call super::open;LONG	 ll_row, ll_task_no


istr_window_message = Message.PowerObjectParm

/* create the NVO */
inv_treatment 		= Create n_treatment
inv_resize_splitter 	= Create n_resize_splitter
inv_filter 				= Create n_filter

/* grab the claim number and set it to an instance variable */
iw_active_sheet 	= w_frame.GetActiveSheet()
il_claim_no     		= iw_active_sheet.dw_basic_claim.GetItemNumber(1,'claim_no')

/* set up the column select which is a multi select (3) */
dw_claim_task.uf_setselect(3)

/* set up the datawindow array */
idw_dw[1] = dw_claim_task
idw_dw[2] = dw_treatment_details
idw_dw[3] = dw_invoice_details
idw_dw[4] = dw_pa_score
idw_dw[5] = dw_missed_appointments
idw_dw[6] = dw_foq

/* set up the array on the NVO and do the initial retrieves */
inv_treatment.nf_set_datawindow(idw_dw[],SQLCA)
inv_treatment.nf_init()
inv_treatment.nf_set_commit(TRUE)

//starts the resize service in the ancestor object
wf_setresize(true)

inv_resize.of_SetOrigSize (THIS.width, THIS.height)

inv_resize.of_register(dw_claim_task,'scaletoright')
//inv_resize.of_register(dw_treatment_details,'scaletoright')
//inv_resize.of_register(dw_pa_score,'scaletoright')
inv_resize.of_register(dw_missed_appointments,'scaletoright')
inv_resize.of_Register(dw_invoice_details,'scaletoright&bottom')
inv_resize.of_register(dw_foq,'scaletoright')

inv_resize.of_Register(cb_close,'FixedToBottom')


//TREATMENT
idw_dw[1].Retrieve(il_claim_no)
SQLCA.nf_handle_error('w_treatment','open','idw_dw[1].Retrieve') 

ll_row = idw_dw[1].Getrow()

IF ll_row < 1 THEN
	ll_task_no = 0
ELSE
	ll_task_no = idw_dw[1].GetItemNumber(ll_row,"task_no")
END IF

//TREATMENT DETAILS
idw_dw[2].Retrieve(il_claim_no,ll_task_no)
SQLCA.nf_handle_error('w_treatment','open','idw_dw[2].Retrieve') 
//REHAB INVOICE DETAILS
idw_dw[3].Retrieve(il_claim_no, ll_task_no)
SQLCA.nf_handle_error('w_treatment','open','idw_dw[3].Retrieve') 

idw_dw[4].Retrieve(il_claim_no)
SQLCA.nf_handle_error('w_treatment','open','idw_dw[4].Retrieve') 

//missed appointments
idw_dw[5].Retrieve(il_claim_no, ll_task_no)
SQLCA.nf_handle_error('w_treatment','open','idw_dw[5].Retrieve') 





end event

type st_title from w_a_tool`st_title within w_treatment
integer x = 27
integer y = 0
integer width = 878
integer textsize = -11
long textcolor = 16711680
string text = "Physio Treatment By Claim"
alignment alignment = left!
boolean border = false
borderstyle borderstyle = stylebox!
end type

type cb_close from w_a_tool`cb_close within w_treatment
integer x = 2871
integer y = 1756
integer width = 288
integer height = 76
end type

type dw_invoice_details from u_dw_online within w_treatment
integer y = 1220
integer width = 3168
integer height = 528
integer taborder = 11
boolean bringtotop = true
string dataobject = "d_invoice_details"
boolean hscrollbar = true
boolean vscrollbar = true
end type

event clicked;call super::clicked;LONG		ll_docid
STRING		ls_doc_type

CHOOSE CASE dwo.name
	CASE 'p_docid'
	
			ll_docid = THIS.getitemnumber(THIS.GETROW(),'doc_id')
					
			//double check wwe have a valid number
			IF ll_docid > 0 THEN
						
					/*	Get the document id for selected row View the document */	
					IF uo_image_append.of_init(ll_docid)	<= 0 THEN RETURN
			
					ls_doc_type =  uo_image_append.of_get_file_type()
					
					CHOOSE CASE ls_doc_type
						/*  Imaged document */ 
						CASE 'IMA', 'TIF'
							IF uo_image_append.of_append_image(ll_docid) < 0 THEN	RETURN
						CASE ELSE
						
							iw_active_sheet.iu_dw_document_path.f_manage_document(ll_docid,"V","NORMAL")
					END CHOOSE					
			END IF 
	END CHOOSE
end event

event constructor;call super::constructor;THIS.uf_setselect(1)
end event

type dw_treatment_details from u_dw_online within w_treatment
integer x = 5
integer y = 796
integer width = 1883
integer height = 320
integer taborder = 11
boolean bringtotop = true
string dataobject = "d_treatment_details"
boolean border = false
boolean livescroll = false
end type

type dw_claim_task from u_dw_online within w_treatment
integer y = 84
integer width = 3168
integer height = 548
integer taborder = 10
boolean bringtotop = true
string dataobject = "d_claim_task"
boolean vscrollbar = true
end type

event rowfocuschanged;call super::rowfocuschanged;long 		ll_task_no
INTEGER 	li_count

ll_task_no = idw_dw[1].GetItemNumber(currentrow,"task_no")

//REHAB INVOICE DETAILS
idw_dw[2].Retrieve(il_claim_no, ll_task_no)
SQLCA.nf_handle_error('w_treatment','rowfocuschanged','idw_dw[2].Retrieve') 

//REHAB INVOICE DETAILS
idw_dw[3].Retrieve(il_claim_no, ll_task_no)
SQLCA.nf_handle_error('w_treatment','rowfocuschanged','idw_dw[3].Retrieve') 

idw_dw[4].Retrieve(il_claim_no)
SQLCA.nf_handle_error('w_treatment','rowfocuschanged','idw_dw[4].Retrieve') 

//missed appointments
idw_dw[5].Retrieve(il_claim_no, ll_task_no)
SQLCA.nf_handle_error('w_treatment','rowfocuschanged','idw_dw[5].Retrieve') 

//foq
li_count = idw_dw[6].Retrieve(il_claim_no, ll_task_no)
SQLCA.nf_handle_error('w_treatment','rowfocuschanged','idw_dw[5].Retrieve') 




end event

event retrieveend;call super::retrieveend;INTEGER 	li_counter, li_task_no
LONG			ll_claim_no
DATETIME 	ldtm_discharge_date


IF isnull(rowcount) OR rowcount <= 0 THEN RETURN

FOR li_counter = 1 TO rowcount
	
	li_task_no 	= THIS.getitemnumber(li_counter, 'task_no')
	ll_claim_no 	= THIS.getitemnumber(li_counter, 'claim_no')
	
	SELECT 	treatment_discharge_date 
	INTO		:ldtm_discharge_date
	FROM  	REPORT_PHYSIO_MASTER 
	WHERE 	claim_no 						= :ll_claim_no
	AND 		task_no 							= :li_task_no
	AND 		physio_report_type_code 	= 'D'
	USING 	SQLCA;
	SQLCA.nf_handle_error('w_treatment','dw_claim_task','	SELECT 	treatment_discharge_date ')
	
	IF not isnull(ldtm_discharge_date) and  string(ldtm_discharge_date, 'YYYY-MM-DD') <> '1900-01-01' then 
		THIS.setitem(li_counter,'discharge_date',string(ldtm_discharge_date, 'YYYY-MM-DD'))
	end if 	
	
NEXT
end event

type dw_pa_score from u_dw_online within w_treatment
integer x = 5
integer y = 636
integer width = 1883
integer height = 176
integer taborder = 11
boolean bringtotop = true
string dataobject = "d_pa_score"
boolean border = false
end type

type dw_missed_appointments from u_dw_online within w_treatment
integer x = 5
integer y = 1116
integer width = 3163
integer height = 96
integer taborder = 11
boolean bringtotop = true
string dataobject = "d_treatment_missed_appointments"
boolean border = false
end type

type uo_image_append from u_image_append within w_treatment
boolean visible = false
integer x = 1275
integer y = 1756
integer taborder = 21
boolean bringtotop = true
end type

on uo_image_append.destroy
call u_image_append::destroy
end on

type dw_foq from u_dwa within w_treatment
integer x = 1893
integer y = 632
integer width = 1275
integer height = 488
integer taborder = 11
boolean bringtotop = true
string dataobject = "d_treatement_foq_info"
boolean vscrollbar = true
end type

