$PBExportHeader$w_cost_analysis_parameters.srw
$PBExportComments$Enter parameters for Cost Analysis
forward
global type w_cost_analysis_parameters from w_a_tool
end type
type cb_ok from commandbutton within w_cost_analysis_parameters
end type
type cb_cancel from commandbutton within w_cost_analysis_parameters
end type
type dw_opening_type_list from u_dw_online within w_cost_analysis_parameters
end type
type dw_opening_date_parameters from u_dw_online within w_cost_analysis_parameters
end type
type st_claim_accident_date from statictext within w_cost_analysis_parameters
end type
type st_1 from statictext within w_cost_analysis_parameters
end type
end forward

global type w_cost_analysis_parameters from w_a_tool
int X=1586
int Y=1596
int Width=2057
int Height=912
WindowType WindowType=response!
boolean TitleBar=true
string Title=""
boolean ControlMenu=true
boolean Resizable=false
cb_ok cb_ok
cb_cancel cb_cancel
dw_opening_type_list dw_opening_type_list
dw_opening_date_parameters dw_opening_date_parameters
st_claim_accident_date st_claim_accident_date
st_1 st_1
end type
global w_cost_analysis_parameters w_cost_analysis_parameters

type variables
//
// Variables required by this module
//

Date			idt_accident_recurrence_date

s_window_message 	istr_bencalc_parameters

window			iw_calling_window

end variables

on open;call w_a_tool::open;/* ---------------------------------------------------------------------------------------------------- */
/* Declare and Initialize Variables                                                                     */
/* ---------------------------------------------------------------------------------------------------- */

	DATETIME		ldtm_accident_recurrence_date

	LONG			ll_result

	STRING		ls_claim_accident_date

/* Position the response window
*/
	this.x = 1900
	this.y = 600




/* ***********************************************************
	Benefit Calculation Parameters stored in s_window_message

	Claim No								al_doubleparm[1]
	Opening No							al_doubleparm[2]
	Benefit Calculation No			al_doubleparm[3]
	Copy Benefit Calculation No	al_doubleparm[4]

	Transitional Claim Flag			as_stringparm[1]
	Top-up Flag							as_stringparm[2]
	Opening Type Code					as_stringparm[3]

	Accident Recurrence Date		adtm_datetimeparm[1]
	Benefit Start Date				adtm_datetimeparm[2]

	Read/Update Mode					as_mode 

	**********************************************************
*/

	istr_bencalc_parameters 	=	Message.PowerObjectParm
	
	idt_accident_recurrence_date = date(istr_bencalc_parameters.adtm_datetimeparm[1])

/* Set up the Default Parameters on the Window
*/

/* Populate the list of Opening Types.
*/
	dw_opening_type_list.SetTransObject(SQLCA)
	dw_opening_type_list.InsertRow(0)
	SQLCA.nf_handle_error("w_cost_analysis_parameters","dw_opening_type_list","open script")


//	idwc_opening_type_list.SetItem(1,"opening_type_code","RLOE")

/* Set up the dw to accept the date range.
*/
	dw_opening_date_parameters.InsertRow(0)
	dw_opening_date_parameters.SetItem(1, "accident_recurrence_date", idt_accident_recurrence_date)
	dw_opening_date_parameters.SetItem(1, "benefit_start_date", idt_accident_recurrence_date)

/* Display the Claim's Accident Date */
ls_claim_accident_date = string(idt_accident_recurrence_date,'yyyy-mm-dd')
st_claim_accident_date.text = ls_claim_accident_date
 


end on

on w_cost_analysis_parameters.create
int iCurrent
call super::create
this.cb_ok=create cb_ok
this.cb_cancel=create cb_cancel
this.dw_opening_type_list=create dw_opening_type_list
this.dw_opening_date_parameters=create dw_opening_date_parameters
this.st_claim_accident_date=create st_claim_accident_date
this.st_1=create st_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_ok
this.Control[iCurrent+2]=this.cb_cancel
this.Control[iCurrent+3]=this.dw_opening_type_list
this.Control[iCurrent+4]=this.dw_opening_date_parameters
this.Control[iCurrent+5]=this.st_claim_accident_date
this.Control[iCurrent+6]=this.st_1
end on

on w_cost_analysis_parameters.destroy
call super::destroy
destroy(this.cb_ok)
destroy(this.cb_cancel)
destroy(this.dw_opening_type_list)
destroy(this.dw_opening_date_parameters)
destroy(this.st_claim_accident_date)
destroy(this.st_1)
end on

event closequery;call super::closequery;	

	 if isvalid(iw_calling_window) OR isnull(iw_calling_window)then 
		iw_calling_window.PostEvent(Activate!)
	end if

	

end event

type st_title from w_a_tool`st_title within w_cost_analysis_parameters
int Y=4
int Width=2048
string Text="Cost Analysis Parameters"
end type

type cb_close from w_a_tool`cb_close within w_cost_analysis_parameters
int TabOrder=30
end type

type cb_ok from commandbutton within w_cost_analysis_parameters
int X=366
int Y=724
int Width=425
int Height=88
int TabOrder=40
string Text="&OK"
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

event clicked;// Declare Variables

W_SHEET					lwi_active_sheet
DATETIME					ldtm_benefit_start_date, ldtm_accident_recurrence_date
date						ldt_date, ldt_benefit_start_date, ldt_accident_recurrence_date
STRING					ls_opening_type_code

/* ***********************************************************
	Benefit Calculation Parameters stored in s_window_message

	Claim No								al_doubleparm[1]
	Opening No							al_doubleparm[2]
	Benefit Calculation No			al_doubleparm[3]
	Copy Benefit Calculation No	al_doubleparm[4]

	Transitional Claim Flag			as_stringparm[1]
	Top-up Flag							as_stringparm[2]
	Opening Type Code					as_stringparm[3]

	Accident Recurrence Date		adtm_datetimeparm[1]
	Benefit Start Date				adtm_datetimeparm[2]

	Read/Update Mode					as_mode 

	**********************************************************
*/
 

	IF dw_opening_date_parameters.AcceptText() = -1 THEN
		dw_opening_date_parameters.SetFocus()
		Return
	END IF

	IF dw_opening_type_list.AcceptText() = -1 THEN
		dw_opening_type_list.SetFocus()
		Return
	END IF

	IF (dw_opening_type_list.GetItemString(1,"opening_type_code") = " " )        OR  &
		ISNULL (dw_opening_type_list.GetItemString(1,"opening_type_code"))      THEN
		MessageBox("Benefit Calculation Module","An Opening Type must be entered in order to create a Cost Analysis", Exclamation!)
		dw_opening_type_list.SetColumn("opening_type_code")
		Return
   END IF


	IF ISNULL (dw_opening_date_parameters.GetItemDate(1, "accident_recurrence_date") ) THEN
		MessageBox("Benefit Calculation Module","Please enter a value for the Accident/Recurrence Date", Exclamation!)
		dw_opening_date_parameters.SetColumn("accident_recurrence_date")
		Return
   END IF

	IF ISNULL (dw_opening_date_parameters.GetItemDate(1, "benefit_start_date") )   THEN
		MessageBox("Benefit Calculation Module","Please enter a value for the Benefit Start Date", Exclamation!)
		dw_opening_date_parameters.SetColumn("benefit_start_date")
		Return
   END IF

	ldt_benefit_start_date =  dw_opening_date_parameters.GetItemDate(1, "benefit_start_date") 	
	ldt_accident_recurrence_date =  dw_opening_date_parameters.GetItemDate(1, "accident_recurrence_date") 	

 
   ldt_date = idt_accident_recurrence_date

	IF ldt_accident_recurrence_date < idt_accident_recurrence_date THEN
		MessageBox("Benefit Calculation Module","The Accident/Recurrence Date cannot be before the Accident Date of the Claim", Exclamation!)
		dw_opening_date_parameters.SetColumn("accident_recurrence_date")
		Return
   END IF

	IF ldt_benefit_start_date < ldt_accident_recurrence_date  THEN
		MessageBox("Benefit Calculation Module","Benefit Start Date cannot be before the Accident/Recurrence Date of the Opening", Exclamation!)
		dw_opening_date_parameters.SetColumn("benefit_start_date")
		Return
   END IF


/* Get the entered parameters from the window
*/
 	ldt_date = dw_opening_date_parameters.GetItemDate(1, "benefit_start_date") 	
	ldtm_benefit_start_date = datetime(ldt_date)
	ldt_date = dw_opening_date_parameters.GetItemDate(1, "accident_recurrence_date") 	
	ldtm_accident_recurrence_date = datetime(ldt_date)
	ls_opening_type_code = dw_opening_type_list.GetItemString(1, "opening_type_code")

 	istr_bencalc_parameters.adtm_datetimeparm[2] = ldtm_benefit_start_date				// Benefit Start Date of the opening	
	istr_bencalc_parameters.as_stringparm[3]		= ls_opening_type_code					// Opening Type Code
	istr_bencalc_parameters.adtm_datetimeparm[1]	= ldtm_accident_recurrence_date 		// Accident/Recurrence Date	
	

	/* ReSet the default transitional claim flag value depending on the Accident/Recurrence Date entered:
  		Acc/Recurrence Date		TRANSITIONAL
		-------------------		------------		
 		>=  1993-01-01	 			INAPPLICABLE
		< 1993-01-01				Default to NO 
	*/
	IF Date(ldtm_accident_recurrence_date) < Date("1993-01-01") THEN 
		istr_bencalc_parameters.as_stringparm[1] = "N"  
	ELSE
		istr_bencalc_parameters.as_stringparm[1] = "I"  		
	END IF

	lwi_active_sheet = w_frame.GetActiveSheet()
	
	OpenWithParm (lwi_active_sheet.iw_calculation_details,istr_bencalc_parameters,lwi_active_sheet)
	
// Ed Lenarczyk - begin changes  Feb 07, 2000
//						save the calling window to set focus properly
//						iw_calling_window introduced to pass this info
	
	iw_calling_window = lwi_active_sheet.iw_calculation_details

// Ed Lenarczyk - end changes  Feb 07, 2000

 	Close(parent)

end event

type cb_cancel from commandbutton within w_cost_analysis_parameters
int X=1143
int Y=716
int Width=443
int Height=88
int TabOrder=50
string Text="Cance&l"
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

on clicked;/* Cancel Setting Up Cost Analysis Parameters
*/

close (Parent)
 
 


 

end on

type dw_opening_type_list from u_dw_online within w_cost_analysis_parameters
int X=9
int Y=268
int Width=1861
int Height=136
int TabOrder=10
boolean BringToTop=true
string DataObject="d_opening_types"
boolean Border=false
boolean LiveScroll=true
end type

type dw_opening_date_parameters from u_dw_online within w_cost_analysis_parameters
int X=18
int Y=384
int Width=1586
int Height=216
int TabOrder=20
boolean BringToTop=true
string DataObject="d_opening_date_parameters"
boolean Border=false
boolean LiveScroll=true
end type

type st_claim_accident_date from statictext within w_cost_analysis_parameters
int X=562
int Y=120
int Width=475
int Height=72
boolean Enabled=false
boolean BringToTop=true
string Text=" "
boolean FocusRectangle=false
long BackColor=67108864
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

type st_1 from statictext within w_cost_analysis_parameters
int X=9
int Y=120
int Width=549
int Height=72
boolean Enabled=false
boolean BringToTop=true
string Text="Claim Accident Date:"
Alignment Alignment=Center!
boolean FocusRectangle=false
long BackColor=67108864
int TextSize=-9
int Weight=700
string FaceName="Arial"
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

